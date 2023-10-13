#' Create database with svypd results
#'
#' @param variable Outcome variable
#' @param estlabel Label for the estimate variable
#' @param vid Outcome variable ID for filenames
#' @param measureid Analysis indicator ID
#' @param printprogress Progress tracking string to print
#' @param VCP VCQI current program name to be logged, default to be the function name
#' @param ... Other arguments
#'
#' @return A database in VCQI_OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import survey
#' @import haven

# make_svyp_output_database R version 1.05 - Biostat Global Consulting - 2022-10-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-03  1.00      Mia Yu          Original R version
# 2022-08-22  1.01      Mia Yu          Keep only level4 part
# 2022-08-25  1.02      Caitlin Clary   Adapt to use svypd
# 2022-09-12  1.03      Mia Yu          Adapt to allow one cluster dataset
# 2022-09-19  1.04      Mia Yu          Adapt to save variable label
# 2022-10-20  1.05      Mia Yu          Add variable labels
# *******************************************************************************

# NOTE: functional, with some details pending (see TO DO notes)

make_svyp_output_database <- function(
    variable,
    estlabel,
    vid,
    measureid,
    printprogress = NULL,
    VCP = "make_svyp_output_database",
    ...){

  if(!is.null(printprogress)){
    print(printprogress)
  }

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", measureid, "_", ANALYSIS_COUNTER, ".rds"))

  tempvar <- get(variable, dat)
  templabel <- estlabel
  tempvid <- vid
  tempmeasure <- measureid

  # Log the database being made
  vcqi_log_comment(VCP, 3, "Comment",
                   paste0("measureid: ", measureid,
                          " variable: ", variable,
                          " vid: ", vid,
                          " label: ", estlabel))

  # Set survey design based on VCQI_SVYDESIGN_SYNTAX
  # Updated 2022-09-20 to handle single-cluster design better (id = ~1)

  VCQI_SVYDESIGN_SYNTAX <- get("VCQI_SVYDESIGN_SYNTAX", envir = .GlobalEnv)

  if (substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]] == "1"){
    datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                           weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
  } else {
    clusterid <- get(substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]], dat)

    if (length(unique(clusterid)) == 1){
      datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                             weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
    } else {
      datdesign <- svydesign(ids = VCQI_SVYDESIGN_SYNTAX$ids,
                             strata = VCQI_SVYDESIGN_SYNTAX$strata,
                             weights = VCQI_SVYDESIGN_SYNTAX$weights, data = dat)
    }
  }

  go <- NULL

  if (!vcqi_object_exists("VCQI_DATABASES")){VCQI_DATABASES <- NULL}

  vcqi_global(VCQI_DATABASES,
              c(VCQI_DATABASES,paste0(tempmeasure, "_", ANALYSIS_COUNTER, "_", tempvid, "_database.rds")))

  l <- 4
  for (j in 1:nrow(level4_layout)){
    # Pass along the name and id of the sub-stratum
    l4name <- level4_layout$label[j]
    rowtype <- level4_layout$rowtype[j]

    if (rowtype == "DATA_ROW"){
      condition <- level4_layout$condition[j]

      # Count respondents meeting the level4 condition(s)
      count <- subset(dat, eval(rlang::parse_expr(condition)) &
                        tempvar %in% c(0,1)) %>% nrow()

      # Only do the calculation and put out the results if there are
      # respondents in this sub-stratum
      if (count > 0){
        f <- paste0("tempdatdesign <- subset(datdesign, (", condition, ") %in% TRUE)")
        eval(parse_expr(f))

        testresult <- calc_icc(y = variable, svydata = tempdatdesign, ci.type = "smith",
                               showmessages = FALSE)
        icctemp <- testresult$estimates$ICC

        ptest <- svypd(
          svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
          var = variable,
          subset_condition = condition,
          ci_level = 95,
          ci_method = VCQI_CI_METHOD,
          adjust = TRUE,
          truncate = TRUE
        )

        if (is.nan(icctemp)){
          icctemp <- NA
        }

        ptest$icc <- icctemp

        gotemp <- data.frame(
          level = l, level4id = j, level4name = l4name, outcome = variable,
          estimate = ptest$estimate, stderr = ptest$stderr, cilevel = 95,
          cill = ptest$cill, ciul = ptest$ciul,
          lcb = ptest$lcb, ucb = ptest$ucb,
          deff = ptest$deff, icc = icctemp, n = ptest$n, nwtd = ptest$nwtd,
          nclusters = ptest$nclusters, nwtd_est = ptest$nwtd_est, neff = ptest$neff
           )

        go <- rbind(go, gotemp)

      } #end of count
    }

    if (rowtype == "BLANK_ROW"){
      gotemp <- data.frame(level = l, level4id = j, level4name = "BLANK_ROW", outcome = variable, estimate = NA, stderr = NA, cilevel = NA,
                           cill = NA, ciul = NA, lcb = NA, ucb = NA, deff = NA, icc = NA, n = NA, nwtd = NA, nclusters = NA, nwtd_est = NA,
                           neff = NA)
      go <- rbind(go, gotemp)
    }


    if (rowtype == "LABEL_ONLY"){
      gotemp <- data.frame(level = l, level4id = j, level4name = l4name, outcome = variable, estimate = NA, stderr = NA, cilevel = NA,
                           cill = NA, ciul = NA, lcb = NA, ucb = NA, deff = NA, icc = NA, n = NA, nwtd = NA, nclusters = NA, nwtd_est = NA,
                           neff = NA)
      go <- rbind(go, gotemp)
    }

  } #end of j loop

  filename <- paste0(VCQI_OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                     "_", tempvid, "_database.rds")
  saveRDS(go, filename)

  # Now do a little work to put the ids and names of the various stratum
  # levels into the database
  #
  # The database will serve at least two purposes:
  #
  # 1. It can be exported to a flat file or excel file or database and
  #    may be used with mail-merge software to generate reporting forms
  #    in programs like Microsoft Word.  This provides future flexibility.
  #
  # 2. It will serve as the basis of the `measureid'_05TO program that
	#    exports requested records out to Microsoft Excel.

  # We have all the components of the names; make a single name variable that
  # holds what we think would be best to list in a table (but also keep the
  # components)

  dat <- go %>%
    mutate(name = NA,
           level4name = as.character(level4name),
           # Append the name to the front of the level4name if we have a single
           # stratifier; otherwise leave it off.
           name = ifelse(!is.na(level4name), level4name, name),
           name = ifelse(level4name == "BLANK_ROW", NA, name))

  # Label variable name "Survey name for table output"
  dat <- dat %>%
    relocate(c(name, level4id, level4name), .after = level) %>%
    arrange(level, level4id) # Arranging by level4id no longer matches Stata output

  dat$level <- haven::labelled(dat$level, label = "Stratum level") %>% suppressWarnings()
  dat$level4id <- haven::labelled(dat$level4id, label = "Sub-stratum ID") %>% suppressWarnings()
  dat$name <- haven::labelled(dat$name, label = "Stratum name for table output") %>% suppressWarnings()
  dat$outcome <- haven::labelled(dat$outcome, label = "Outcome variable") %>% suppressWarnings()
  dat$estimate <- haven::labelled(dat$estimate, label = templabel) %>% suppressWarnings()
  dat$stderr <- haven::labelled(dat$stderr, label = "Standard error") %>% suppressWarnings()
  dat$cilevel <- haven::labelled(dat$cilevel, label = "Confidence level") %>% suppressWarnings()
  dat$cill <- haven::labelled(dat$cill, label = "2-sided CI lower-bound") %>% suppressWarnings()
  dat$ciul <- haven::labelled(dat$ciul, label = "2-sided CI upper-bound") %>% suppressWarnings()
  dat$lcb <- haven::labelled(dat$lcb, label = "1-sided lower confidence bound") %>% suppressWarnings()
  dat$ucb <- haven::labelled(dat$ucb, label = "1-sided upper confidence bound") %>% suppressWarnings()

  saveRDS(dat, filename)

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
