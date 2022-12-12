#' Calculating bar chart coordinates for VCTC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset with values for a VCTC plot
#'
#' @import tidyselect
#' @import dplyr
#' @import stringr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import survey
#'
# RI_VCTC_01_03DV R version 1.01 - Biostat Global Consulting - 2022-11-04
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-03  1.00      Mia Yu          Original R version
# 2022-11-04  1.01      Mia Yu          Package version
# *******************************************************************************

RI_VCTC_01_03DV <- function(VCP = "RI_VCTC_01_03DV"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_CHECK_INSTEAD_OF_RUN != 1){

    #empty out the tabulation dataset
    if (file.exists(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_TO.rds"))){
      file.remove(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_TO.rds"))
    }

    for (lvl in seq_along(RI_VCTC_01_LEVELS)) {

      dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,".rds"))
      combineddat <- NULL

      idvar <- get(paste0("level",RI_VCTC_01_LEVELS[lvl],"id"), dat)
      llist <- unique(idvar)

      for (l in seq_along(llist)){

        dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,".rds"))
        saveddat <- dat

        f <- paste0("subdat <- subset(dat,level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l],")")
        eval(rlang::parse_expr(f))
        namevar <- get(paste0("level",RI_VCTC_01_LEVELS[lvl],"name"), subdat)
        stratumname <- namevar[1]

        savedsubdat <- subdat

        if(!vcqi_object_exists("RI_VCTC_01_TEMP_DATASETS")){
          RI_VCTC_01_TEMP_DATASETS <- NULL
        }
        vcqi_global(RI_VCTC_01_TEMP_DATASETS,
                    c(RI_VCTC_01_TEMP_DATASETS, paste0("RI_VCTC_01_",ANALYSIS_COUNTER,"_",RI_VCTC_01_LEVELS[lvl],"_",llist[l])))

        #Calculate the maximum number of tiles across all doses
        max_ntiles <- get("TIMELY_N_DTS", envir = .GlobalEnv)

        for (d in seq_along(TIMELY_CD_LIST)){
          tile <- get(paste0("TIMELY_CD_",TIMELY_CD_LIST[d],"_NTILES"), envir = .GlobalEnv)
          tile <- as.numeric(tile)
          if (tile > max_ntiles){
            max_ntiles <- tile
          }
          rm(tile)
        } #end of TIMELY_CD_LIST d loop

        tplot <- as.data.frame(matrix(nrow = TIMELY_N_DOSES, ncol = max_ntiles+2))

        doselist <- str_to_lower(TIMELY_DOSE_ORDER)

        for (d in seq_along(doselist)){
          D <- str_to_upper(doselist[d])

          cardage <- rlang::sym(paste0("age_at_",doselist[d],"_card"))
          subdat <- subdat %>% mutate(tempvar1 = !!cardage)

          if (RI_RECORDS_NOT_SOUGHT != 1){
            regage <- rlang::sym(paste0("age_at_",doselist[d],"_register"))
            subdat <- subdat %>% rowwise() %>% mutate(tempvar1 = min(!!cardage, !!regage,na.rm = TRUE)) %>%
              ungroup() %>% suppressWarnings()
            subdat <- subdat %>% mutate(tempvar1 = ifelse((tempvar1 == Inf | tempvar1 == -Inf) %in% TRUE, NA, tempvar1))
          } # end of if RI_RECORDS_NOT_SOUGHT

          crude <- rlang::sym(paste0("got_crude_",doselist[d],"_to_analyze"))

          subdat <- subdat %>% mutate(tempvar1 = ifelse(is.na(!!crude), NA, tempvar1))
          subdatjoin <- subdat %>% select(respid, tempvar1)

          dat <- left_join(dat, subdatjoin, by = "respid")

          #save this middle step dat too for later use
          saveddat2 <- dat

          if (!(TIMELY_DOSE_ORDER[d] %in% TIMELY_CD_LIST)){

            #use default tiles
            numtile <- as.numeric(TIMELY_N_DTS) - 1

            for (j in 1:numtile){
              dt_ub <- get(paste0("TIMELY_DT_UB_",j), envir = .GlobalEnv)
              minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)
              subdat2 <- subdat %>% mutate(timely_y = ifelse(((tempvar1 < (minage + dt_ub)) & !is.na(!!crude)) %in% TRUE,
                                                             1, 0))

              if((minage == 0 & j == 1) %in% TRUE) {
                subdat2 <- subdat %>% mutate(timely_y = 0)
              }

              subdat2 <- subdat2 %>% select(respid, timely_y)

              #merge the dataset
              dat <- left_join(dat,subdat2, by = "respid")

              #set the survey design
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

              ptest <- svypd(
                svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
                var = "timely_y",
                subset_condition = paste0("level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l]),
                ci_level = 95,
                ci_method = VCQI_CI_METHOD,
                adjust = TRUE,
                truncate = TRUE
              )

              tplot[d,j] <- 100*ptest$estimate

              #set the middle step dat back
              dat <- saveddat2

            } #end of numtile loop

          } else {
            #use user defined tiles
            usertile <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_NTILES"), envir = .GlobalEnv)
            usertile = usertile - 1

            for (j in 1:usertile){
              dt_ub <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_UB_",j), envir = .GlobalEnv)
              minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)
              subdat2 <- subdat %>% mutate(timely_y = ifelse(((tempvar1 < (minage + dt_ub)) & !is.na(!!crude)) %in% TRUE,
                                                             1, 0))

              subdat2 <- subdat2 %>% select(respid, timely_y)

              #merge the dataset
              dat <- left_join(dat,subdat2, by = "respid")

              #set the survey design
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

              ptest <- svypd(
                svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
                var = "timely_y",
                subset_condition = paste0("level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l]),
                ci_level = 95,
                ci_method = VCQI_CI_METHOD,
                adjust = TRUE,
                truncate = TRUE
              )

              tplot[d,j] <- 100*ptest$estimate

              #set the middle step dat back
              dat <- saveddat2

            } #end of usertile loop

          }

          #set the middle step dat back again just in case
          dat <- saveddat2

          ptest2 <- svypd(
            svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
            var = paste0("got_crude_",doselist[d],"_to_analyze"),
            subset_condition = paste0("level",RI_VCTC_01_LEVELS[lvl],"id == ", llist[l]),
            ci_level = 95,
            ci_method = VCQI_CI_METHOD,
            adjust = TRUE,
            truncate = TRUE
          )

          if (!(TIMELY_DOSE_ORDER[d] %in% TIMELY_CD_LIST)){
            #default setting
            colnum <- as.numeric(TIMELY_N_DTS)

            tplot[d, colnum] <- 100*ptest2$estimate
            tplot[d, max_ntiles + 1] <- 100*ptest2$cill
            tplot[d, max_ntiles + 2] <- 100*ptest2$ciul
          } else {
            tile <- get(paste0("TIMELY_CD_",TIMELY_CD_LIST[d],"_NTILES"), envir = .GlobalEnv)
            tile <- as.numeric(tile)

            tplot[d, tile] <- 100*ptest2$estimate
            tplot[d, max_ntiles + 1] <- 100*ptest2$cill
            tplot[d, max_ntiles + 2] <- 100*ptest2$ciul
          }

          subdat <- savedsubdat
        } #end of doselist loop

        if(!vcqi_object_exists("TIMELY_Y_COORDS")){
          ycoords <- c(1:length(TIMELY_DOSE_ORDER))
        } else {
          ycoords <- get("TIMELY_Y_COORDS", envir = .GlobalEnv)
        }

        tplot <- tplot %>% mutate(y = ycoords, dose = doselist)
        saveRDS(tplot,
                file = paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_tplot_",RI_VCTC_01_LEVELS[lvl],"_",llist[l],".rds"))


        tplott <- as.data.frame(t(tplot))
        tplott <- tplott[1:max_ntiles,]

        tempname <- paste0("dose",1:TIMELY_N_DOSES)
        names(tplott) <- tempname

        doselist <- str_to_lower(TIMELY_DOSE_ORDER)

        for (d in seq_along(doselist)){

          dose <- get(paste0("dose",d), tplott)

          tplott <- tplott %>% mutate(label = NA, agespan = NA, height = NA) %>%
            relocate(c(height,agespan,label), .after = paste0("dose",d))

          # populate the first row here; others below
          tplott$height[1] <- dose[1]

          if (!(TIMELY_DOSE_ORDER[d] %in% TIMELY_CD_LIST)){
            #default setting

            numtile <- as.numeric(TIMELY_N_DTS) - 1

            for (j in 1:numtile){

              lebelj <- get(paste0("TIMELY_DT_LABEL_",j), envir = .GlobalEnv)
              tplott$label[j] <- lebelj

              if (j == 1){
                dt_ub <- get(paste0("TIMELY_DT_UB_",j), envir = .GlobalEnv)
                agelimit <- min(VCQI_RI_MAX_AGE_OF_ELIGIBILITY,dt_ub)
                tplott$agespan[1] <- paste0("Age < ",agelimit, " days")
              } #j = 1

              if (j > 1 & vcqi_object_exists(paste0("TIMELY_DT_UB_",j))){
                dt_ub <- get(paste0("TIMELY_DT_UB_",j), envir = .GlobalEnv)
                minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)
                agelimit <- min(VCQI_RI_MAX_AGE_OF_ELIGIBILITY, minage + dt_ub)
                tplott$agespan[j] <- paste0("Age < ",agelimit, " days")
              } #j > 1

              if (j > 1 & !is.na(dose[j])){
                value = as.numeric(dose[j]) - as.numeric(dose[j-1])
                tplott$height[j] <- value
              } #j > 1


            } #end of numtile loop

            j <- TIMELY_N_DTS

            value = as.numeric(dose[j]) - as.numeric(dose[j-1])
            tplott$height[j] <- value
            tplott$agespan[j] <- "Timing unknown"
            tplott$label[j] <- "All ages"

          } else {
            #TO DO: test here
            #use user defined tiles
            usertile <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_NTILES"), envir = .GlobalEnv)
            usertile = usertile - 1

            for (j in 1:usertile){

              lebelj <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_LABEL_",j), envir = .GlobalEnv)
              tplott$label[j] <- lebelj

              if (j == 1){
                dt_ub <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_UB_",j), envir = .GlobalEnv)
                agelimit <- min(VCQI_RI_MAX_AGE_OF_ELIGIBILITY,dt_ub)
                tplott$agespan[1] <- paste0("Age < ",agelimit, " days")
              } #j = 1

              if (j > 1 & vcqi_object_exists(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_UB_",j))){
                dt_ub <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_UB_",j), envir = .GlobalEnv)
                minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)
                agelimit <- min(VCQI_RI_MAX_AGE_OF_ELIGIBILITY, minage + dt_ub)
                tplott$agespan[j] <- paste0("Age < ",agelimit, " days")
              } #j > 1

              if (j > 1 & !is.na(dose[j])){
                value = as.numeric(dose[j]) - as.numeric(dose[j-1])
                tplott$height[j] <- value
              } #j > 1

            } #end of usertile loop

            j <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_NTILES"), envir = .GlobalEnv)

            value = as.numeric(dose[j]) - as.numeric(dose[j-1])
            tplott$height[j] <- value
            tplott$agespan[j] <- "Timing unknown"
            tplott$label[j] <- "All ages"
          }

          names(tplott)[which(names(tplott) == "label")] <- paste0(doselist[d],"_","label")
          names(tplott)[which(names(tplott) == "agespan")] <- paste0(doselist[d],"_","agespan")
          names(tplott)[which(names(tplott) == "height")] <- paste0(doselist[d],"_tile_height")
          names(tplott)[which(names(tplott) == paste0("dose",d))] <- paste0(doselist[d],"_tile_top_pct")

        } #end of doselist loop

        tplott <- tplott %>% mutate(across(ends_with("_tile_top_pct"), ~round(as.numeric(.x), 1)))
        tplott <- tplott %>% mutate(across(ends_with("_tile_height"), ~round(as.numeric(.x), 1)))

        tplott <- tplott %>% mutate(level = RI_VCTC_01_LEVELS[lvl], levelid = llist[l],
                                    stratum_name = stratumname, order = row_number()) %>%
          relocate(c(level,levelid,stratum_name,order))

        combineddat <- rbind(combineddat,tplott)
        combineddat <- combineddat %>% arrange(level, levelid, order)
        saveRDS(combineddat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_TO.rds"))
      } #end of llist loop

    } #end of RI_VCTC_01_LEVELS loop

    if (!vcqi_object_exists("RI_VCTC_01_TEMP_DATASETS")){
      RI_VCTC_01_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_VCTC_01_TEMP_DATASETS,c(RI_VCTC_01_TEMP_DATASETS, paste0("RI_VCTC_01_",ANALYSIS_COUNTER,"_TO.rds")))

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
