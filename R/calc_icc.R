#' Calculates Intracluster Correlation Coefficient (ICC), confidence interval, and mean squared error for selected outcome
#'
#' @param y Outcome variable
#' @param data Dataset name (not defined as a survey design object)
#' @param svydata Dataset name (defined as a survey design object)
#' @param strat Stratum ID variable (when using non-survey design data)
#' @param cid Cluster ID variable (when using non-survey design data)
#' @param method ICC calculation method
#' @param median Use median adjustment to ICC calculation or not, default to be FALSE
#' @param ci.type CI calculation method (smith or bootstrap), default to be "bootstrap"
#' @param ci_bootrep Number of bootstrap replications if ci.type is bootstrap, default to be 500
#' @param alpha Alpha for confidence intervals, default to be 0.05
#' @param showmessages Show messages about removed NAs or not, default to be TRUE
#'
#' @return List containing ICC estimate, confidence interval, and mean squared error
#' @rawNamespace import(stats, except = c(filter,lag))
#' @import dplyr
#' @import withr

# calc_icc R version 1.07 - Biostat Global Consulting - 2022-10-06
# *******************************************************************************
# For selected strata and outcomes, calculate ICC and other quantities of interest.
# Based on ICCbin::iccbin
#
# Change log

# Date 			  Version 	Name			      What Changed
# 2020-04-17  1.00      Caitlin Clary   Original: adapt iccbin, keep only ANOVA method
# 2020-05-08  1.01      Caitlin Clary   Add median option (equiv. Stata loneway median)
# 2020-05-21  1.02      Caitlin Clary   Implement bootstrap confidence interval
#                                       Add stratum ID argument to function (for bootstrap)
# 2020-06-01  1.03      Caitlin Clary   Store missing CI values as NA rather than "-"
# 2020-12-08  1.04      Caitlin Clary   When ms_between and ms_within are both zero (p-hat is
#                                       0% or 100%), default ICC to  -1/(n0-1)
# 2022-08-15  1.05      Caitlin Clary   Allow direct input of survey design object
# 2022-09-21  1.06      Caitlin Clary   Make "NAs removed" message display optional
# 2022-10-06  1.07      Mia Yu          Package version and used with_seed function
# *******************************************************************************

calc_icc <- function(
  y,                     # outcome
  data = NULL,           # name of dataset
  svydata = NULL,        #
  strat = NA,            # stratum ID (used when bootstrapping CI)
  cid = NA,              # cluster ID
  method = "aov",        # ICC calculation method (aov only)
  median = FALSE,        # use median adjustment to ICC calculation
  ci.type = "bootstrap", # CI calculation method (smith or bootstrap)
  ci_bootrep = 500,      # no. bootstrap reps if ci.type is bootstrap
  alpha = 0.05,          # alpha for confidence intervals
  showmessages = TRUE){


  # Either data or svydata must be provided
  if(is.null(data) & is.null(svydata)){
    stop("An input dataset is required. Either specify a data frame using the 'data' argument or a survey design object (created using the survey package or the srvyr package) using the 'svydata' argument.")
  }

  # If svydata was provided, check that it's a survey design object
  if(!is.null(svydata)){
    if(!any(class(svydata) %in% c("survey.design", "survey.design2", "tbl_svy"))){
      stop("The svydata object provided is not a survey design object. Use survey::svydesign or srvyr::as_survey_design to create a survey design object.")
    }
  }

  # If data and svydata were provided, warn that the function will use svydata
  if(!is.null(data) & !is.null(svydata)){
    warning("data and svydata were both provided in the function call. The function will use the survey data provided in the svydata object.")
  }

  # Function state: using dataset and design arguments or using a survey design object?
  if(!is.null(data) & is.null(svydata)){
    fstate <- "data"
  } else if(is.null(data) & !is.null(svydata)){
    fstate <- "survey"
  } else if (!is.null(data) & !is.null(svydata)){
    fstate <- "survey"
  }

  # If working from a dataset that isn't a survey design object, set up required variables
  if(fstate == "data"){

    # Working copy of data
    indat <- data

    # Define y and clusterid variables in dataset
    indat$y <- get(y, indat)
    indat$cid <- get(cid, indat)

    # Define stratum variable if provided, otherwise create as single stratum
    if(!is.na(strat)){
      indat$strat <- get(strat, indat)
    } else {
      indat$strat <- 1
    }

    dt <- indat %>%
      select(y, cid, strat) %>%
      data.frame()

    dt <- na.omit(dt)

    # Group dt for weighted average cluster size calculation
    dt_grp <- dt %>%
      group_by(cid) %>%
      summarize(cluster_n = n()) %>%
      mutate(cs = cumsum(cluster_n * (cluster_n/nrow(dt))))

    wtd_avg_clust_size <- (nrow(dt) - last(dt_grp$cs))/(nrow(dt_grp)-1)

    # Extract cluster ID
    cid <- dt$cid

    # Number of clusters
    k <- length(unique(cid))

    if(!is.null(attributes(dt)$na.action)){
      if(showmessages == TRUE){
        message(cat("NAs removed from data rows: \n", paste0(unclass(attributes(dt)$na.action)), "\n"))}
    }

    if(!is.factor(dt$cid)){
      #warning("'cid' has been coerced to a factor")
      dt$cid <- as.factor(dt$cid)
    } else{
      if(length(levels(dt$cid)) > k){
        dt$x <- factor(as.character(dt$cid), levels = unique(dt$cid))
        warning("Missing levels of 'cid' have been removed")
      }
    }

  } else if(fstate == "survey"){

    # Working copy of data
    indat <- svydata

    # Copy of outcome and cluster variables
    indat$variables$y <- get(y, indat$variables)
    indat$variables$cid <- indat$cluster[,1]

    # Define stratum variable if provided, otherwise create as single stratum
    if(indat$has.strata %in% TRUE){
      indat$variables$strat <- indat$strata[,1]
    } else {
      indat$variables$strat <- 1
    }

    dt <- indat$variables %>% as.data.frame() %>%
      select("y", "cid", "strat")

    dt <- na.omit(dt)

    # Group dt for weighted average cluster size calculation
    dt_grp <- dt %>%
      group_by(cid) %>%
      summarize(cluster_n = n()) %>%
      mutate(cs = cumsum(cluster_n * (cluster_n/nrow(dt))))

    wtd_avg_clust_size <- (nrow(dt) - last(dt_grp$cs))/(nrow(dt_grp)-1)

    # Extract cluster ID
    cid <- dt$cid

    # Number of clusters
    k <- length(unique(cid))

    if(!is.factor(dt$cid)){
      #warning("'cid' has been coerced to a factor")
      dt$cid <- as.factor(dt$cid)
    } else{
      if(length(levels(dt$cid)) > k){
        dt$x <- factor(as.character(dt$cid), levels = unique(dt$cid))
        warning("Missing levels of 'cid' have been removed")
      }
    }

    if(!is.null(attributes(dt)$na.action)){
      if(showmessages == TRUE){
        message(cat("NAs removed from data rows: \n", paste0(unclass(attributes(dt)$na.action)), "\n"))}

    }
  }

  zalpha <- qnorm(alpha/2, lower.tail = F)
  square <- function(z){z^2}

  ## Define inputs, preliminary calculations ----

  # Response variable
  y <- dt$y

  # Number of observations in each cluster
  ni <- as.vector(table(cid))

  # Total number of observations
  N <- sum(ni)

  n0 <- (1/(k - 1))*(N - sum((ni^2)/N))
  yi <- aggregate(y, by = list(cid), sum)[ , 2]
  yisq <- yi^2
  msb <- (1/(k - 1))*(sum(yisq/ni) - (1/N)*(sum(yi))^2)
  msw <- (1/(N - k))*(sum(yi) - sum(yisq/ni))

  # ANOVA estimate of ICC, used in CI calculation even if median=TRUE
  rho.aov <- (msb - msw)/(msb + (n0 - 1)*msw)

  if(median == FALSE){
    meth <- "ANOVA Estimate"
    est <- rho.aov
  }

  if(median == TRUE){
    meth <- "ANOVA with Median Adj."
    Fm <- qf(0.5, (k-1), (N-k))
    rho.aov.m <- ((msb/msw)-Fm)/((msb/msw) + (n0-1)*Fm)
    est <- rho.aov.m
  }

  ## Smith CI ----

  # Calculated with standard ICC (rho.aov, not rho.aov.m), regardless of median option selected by the user

  # Do these calculations even if bootstrap is selected, to have Smith CIs as a fallback

  if(is.na(rho.aov)){
    smith_lci <- NA
    smith_uci <- NA
  } else {

    st0 <- 2*square(1 - rho.aov)/square(n0)

    st1 <- square(1 + rho.aov*(n0 - 1))/(N - k)

    st2 <- ((k - 1)*(1 - rho.aov)*(1 + rho.aov*(2*n0 - 1)) +
              square(rho.aov)*(sum(ni^2) - (2/N)*sum(ni^3) + (1/N^2)*square(sum(ni^2))))/square(k - 1)

    var.smith.rho.aov <- st0*(st1 + st2)

    ci.smith.rho.aov <- c(rho.aov - zalpha*sqrt(var.smith.rho.aov), rho.aov + zalpha*sqrt(var.smith.rho.aov))

    smith_lci <- ifelse(ci.smith.rho.aov[1] < 0, 0, ci.smith.rho.aov[1])
    smith_uci <- ifelse(ci.smith.rho.aov[2] > 1, 1, ci.smith.rho.aov[2])
  }

  if(ci.type == "smith"){
    ci.typ <- "Smith"
      lci <- smith_lci
      uci <- smith_uci
        }

  ## Bootstrap CI
  # nclusters should be >1, p != 0, p != 1
  if(ci.type == "bootstrap"){

    # Do not bootstrap if single cluster
    # Do not bootstrap if no variation in outcome
    if(k == 1){
      warning("Bootstrap cannot be performed; only one cluster in data.")
      lci <- smith_lci
      uci <- smith_uci
      ci.typ <- "Smith (bootstrap failed; single cluster)"

    } else if(sum(y)==0 | sum(y)==N){
      ci.typ <- "Cvg is 0 or 100"
      lci <- NA
      uci <- NA
    } else {

      with_seed(8675309)

      # Number of strata
      st <- length(unique(dt$strat))
      # Number of clusters per stratum
      cps <- dt %>%
        group_by(cid) %>% slice(1) %>%
        group_by(strat) %>% summarize(cbys = n())

      mincps <- min(cps$cbys) # should be >1 to proceed with bootstrap

      if(mincps > 1){
     boot_iccs_out <-  replicate(ci_bootrep, {

       clusters_by_stratum <- vector()
       # Sample clusters from each strata, matching original number of clusters in that stratum
       for(q in 1:st){
         dt_stratum <- dt[dt$strat %in% cps$strat[q],]
         dt_stratum$cid <- droplevels(dt_stratum$cid)
         st_clusters <- sample(levels(dt_stratum$cid), cps$cbys[q], replace = TRUE)
         clusters_by_stratum <- c(clusters_by_stratum, st_clusters)
       }

        clusterlist <- data.frame(
          cid = clusters_by_stratum,
          bootcid = seq(1, k, by = 1))

        suppressWarnings(outdt <- dt %>% inner_join(clusterlist, by = "cid"))

        # ICC calculation on bootstrap dataset
        b_cid <- outdt$bootcid
        b_y <- outdt$y
        b_ni <- as.vector(table(b_cid))
        b_N <- sum(b_ni)

        b_n0 <- (1/(k - 1))*(b_N - sum((b_ni^2)/b_N))
        b_yi <- aggregate(b_y, by = list(b_cid), sum)[ , 2]
        b_yisq <- b_yi^2
        b_msb <- (1/(k - 1))*(sum(b_yisq/b_ni) - (1/b_N)*(sum(b_yi))^2)
        b_msw <- (1/(b_N - k))*(sum(b_yi) - sum(b_yisq/b_ni))

        b_Fm <- qf(0.5, (k-1), (b_N-k))

        # Return value: b_rho (gets saved in boot_iccs_out)
        ifelse(median==FALSE,
               (b_msb - b_msw)/(b_msb + (b_n0 - 1)*b_msw),
               ((b_msb/b_msw)-b_Fm)/((b_msb/b_msw) + (b_n0-1)*b_Fm)
        )

      }) # end replicate

      ci.typ <- "Bootstrap percentiles"
      lci <- quantile(boot_iccs_out, alpha/2, na.rm = TRUE)
      uci <- quantile(boot_iccs_out, 1-alpha/2, na.rm = TRUE)

      } else {# end mincps condition

        lci <- smith_lci
        uci <- smith_uci
        ci.typ <- "Smith (bootstrap failed; 1+ stratum has single cluster)"
        }
    } # end else

  } # end if ci.type = bootstrap

  ## Manual overwrite - when p=0 or p=1, ICC=-1/(m-1) and bounds missing

    if(sum(y)==0 | sum(y)==N){
      est <- -1/(wtd_avg_clust_size - 1)
      ci.typ <- "Cvg is 0 or 100"
      lci <- NA
      uci <- NA
    }

    estimates <- data.frame(Methods = meth, ICC = est); row.names(estimates) <- NULL
    ci <- data.frame(Type = ci.typ, LowerCI = lci, UpperCI = uci); row.names(ci) <- NULL
    ms <- data.frame(MSW = msw, MSB = msb); row.names(ms) <- NULL
    list(estimates = estimates, ci = ci, ms = ms)

} # End of function calc_icc
