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
# RI_VCTC_01_03DV R version 1.03 - Biostat Global Consulting - 2024-01-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-03  1.00      Mia Yu          Original R version
# 2022-11-04  1.01      Mia Yu          Package version
# 2023-07-31  1.02      Mia Yu          Save VCTC category data
# 2024-01-08  1.03      Mia Yu          Correct the lazy logic that cause errors;
#                                       This is an update to match VCQI in Stata
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
        keep <- savedsubdat
        for (d in seq_along(doselist)){
          D <- str_to_upper(doselist[d])

          cardage <- rlang::sym(paste0("age_at_",doselist[d],"_card"))
          subdat <- subdat %>% mutate(tempvar1 = !!cardage)

          # if (RI_RECORDS_NOT_SOUGHT != 1){
          #   regage <- rlang::sym(paste0("age_at_",doselist[d],"_register"))
          #   subdat <- subdat %>% rowwise() %>% mutate(tempvar1 = min(!!cardage, !!regage,na.rm = TRUE)) %>%
          #     ungroup() %>% suppressWarnings()
          #   subdat <- subdat %>% mutate(tempvar1 = ifelse((tempvar1 == Inf | tempvar1 == -Inf) %in% TRUE, NA, tempvar1))
          # } # end of if RI_RECORDS_NOT_SOUGHT

          if (RI_RECORDS_SOUGHT_FOR_ALL == 1){
            regage <- rlang::sym(paste0("age_at_",doselist[d],"_register"))
            subdat <- subdat %>% rowwise() %>% mutate(tempvar1 = min(!!cardage, !!regage,na.rm = TRUE)) %>%
              ungroup() %>% suppressWarnings()
            subdat <- subdat %>% mutate(tempvar1 = ifelse((tempvar1 == Inf | tempvar1 == -Inf) %in% TRUE, NA, tempvar1))
          }

          if (RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
            regage <- rlang::sym(paste0("age_at_",doselist[d],"_register"))
            subdat <- subdat %>% rowwise() %>% mutate(tempvar1 = ifelse(no_card %in% 1,
                                                                        min(!!cardage, !!regage,na.rm = TRUE), tempvar1)) %>%
              ungroup() %>% suppressWarnings()
            subdat <- subdat %>% mutate(tempvar1 = ifelse((tempvar1 == Inf | tempvar1 == -Inf) %in% TRUE, NA, tempvar1))
          }

          crude <- rlang::sym(paste0("got_crude_",doselist[d],"_to_analyze"))

          subdat <- subdat %>% mutate(tempvar1 = ifelse(is.na(!!crude), NA, tempvar1))
          subdatjoin <- subdat %>% select(respid, tempvar1)
          names(subdatjoin)[which(names(subdatjoin) == "tempvar1")] <- paste0("timely_age_at_",doselist[d])
          dat <- left_join(dat, subdatjoin, by = "respid")

          #save this middle step dat too for later use
          saveddat2 <- dat

          if (!(TIMELY_DOSE_ORDER[d] %in% TIMELY_CD_LIST)){

            #use default tiles
            numtile <- as.numeric(TIMELY_N_DTS) - 1
            #tempvar1 <- rlang::sym(paste0("timely_age_at_",doselist[d]))
            for (j in 1:numtile){
              dt_ub <- get(paste0("TIMELY_DT_UB_",j), envir = .GlobalEnv)
              minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)
              subdat2 <- subdat %>% mutate(timely_y = ifelse(((tempvar1 < (minage + dt_ub)) & !is.na(!!crude)) %in% TRUE,
                                                             1, 0))

              if (minage %in% 0 & j %in% 1 & dt_ub %in% 0) {
                subdat2 <- subdat2 %>% mutate(timely_y = 0) # changed made here 2023/07/31
              }

              subdat2 <- subdat2 %>% select(respid,tempvar1, timely_y)

              #merge the dataset
              dat <- left_join(dat,subdat2, by = "respid")

              #set the survey design
              VCQI_SVYDESIGN_SYNTAX <- get("VCQI_SVYDESIGN_SYNTAX", envir = .GlobalEnv)

              if (substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]] == "1"){
                datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                       weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                       fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                       nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                       data = dat)
              } else {

                if (str_count(as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2], pattern = fixed("+")) >= 1){

                  clustervars <- str_split(as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2], pattern = fixed("+"))[[1]]
                  clustervars <- str_trim(clustervars)

                  justone <- TRUE
                  for (i in seq_along(clustervars)){
                    clusterid <- get(clustervars[i], dat)
                    if (!length(unique(clusterid)) %in% 1){
                      justone <- FALSE
                    }
                  } #test if all cluster vars only have 1 unique value

                  if (justone == TRUE){
                    datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  } else {
                    datdesign <- svydesign(ids = VCQI_SVYDESIGN_SYNTAX$ids,
                                           strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  }

                } else {
                  clusterid <- get(substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]], dat)

                  if (length(unique(clusterid)) == 1){
                    datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  } else {
                    datdesign <- svydesign(ids = VCQI_SVYDESIGN_SYNTAX$ids,
                                           strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  }
                } #one stage

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
            #tempvar1 <- rlang::sym(paste0("timely_age_at_",doselist[d]))
            for (j in 1:usertile){
              dt_ub <- get(paste0("TIMELY_CD_",str_to_upper(doselist[d]),"_UB_",j), envir = .GlobalEnv)
              minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)
              subdat2 <- subdat %>% mutate(timely_y = ifelse(((tempvar1 < (minage + dt_ub)) & !is.na(!!crude)) %in% TRUE,
                                                             1, 0))

              if(minage %in% 0 & j %in% 1 & dt_ub %in% 0) {
                subdat2 <- subdat2 %>% mutate(timely_y = 0) # changed made here 2023/07/31
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

                if (str_count(as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2], pattern = fixed("+")) >= 1){

                  clustervars <- str_split(as.character(VCQI_SVYDESIGN_SYNTAX$ids)[2], pattern = fixed("+"))[[1]]
                  clustervars <- str_trim(clustervars)

                  justone <- TRUE
                  for (i in seq_along(clustervars)){
                    clusterid <- get(clustervars[i], dat)
                    if (!length(unique(clusterid)) %in% 1){
                      justone <- FALSE
                    }
                  } #test if all cluster vars only have 1 unique value

                  if (justone == TRUE){
                    datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  } else {
                    datdesign <- svydesign(ids = VCQI_SVYDESIGN_SYNTAX$ids,
                                           strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  }

                } else {
                  clusterid <- get(substring(VCQI_SVYDESIGN_SYNTAX$ids, 1)[[2]], dat)

                  if (length(unique(clusterid)) == 1){
                    datdesign <- svydesign(ids = ~1, strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  } else {
                    datdesign <- svydesign(ids = VCQI_SVYDESIGN_SYNTAX$ids,
                                           strata = VCQI_SVYDESIGN_SYNTAX$strata,
                                           weights = VCQI_SVYDESIGN_SYNTAX$weights,
                                           fpc = VCQI_SVYDESIGN_SYNTAX$fpc,
                                           nest = VCQI_SVYDESIGN_SYNTAX$nest,
                                           data = dat)
                  }
                } #one stage

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
      } #end of llist loop
      if (lvl == 1){
        combineddat2 <- combineddat
      }
      if (lvl > 1){
        combineddat2 <- rbind(combineddat2,combineddat)
      }
    } #end of RI_VCTC_01_LEVELS loop
    saveRDS(combineddat2, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,"_TO.rds"))

    if (!vcqi_object_exists("RI_VCTC_01_TEMP_DATASETS")){
      RI_VCTC_01_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_VCTC_01_TEMP_DATASETS,c(RI_VCTC_01_TEMP_DATASETS, paste0("RI_VCTC_01_",ANALYSIS_COUNTER,"_TO.rds")))

    #notes : "Data are sorted by level and levelid and by bar category order."

    if (!vcqi_object_exists("TIMELY_SAVE_DV_DATASET")){
      TIMELY_SAVE_DV_DATASET <- 1
    }

    # Unless the user specifically asks us not to, we save a dataset with dose-specific VCTC categories
    if (TIMELY_SAVE_DV_DATASET != 0) {
      dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,".rds"))

      doselist <- str_to_lower(TIMELY_DOSE_ORDER)
      for (d in seq_along(doselist)){
        D <- str_to_upper(doselist[d])

        cardage <- rlang::sym(paste0("age_at_",doselist[d],"_card"))
        dat <- dat %>% mutate(tempvar1 = !!cardage)

        if (RI_RECORDS_NOT_SOUGHT != 1){
          regage <- rlang::sym(paste0("age_at_",doselist[d],"_register"))
          dat <- dat %>% rowwise() %>% mutate(tempvar1 = min(!!cardage, !!regage,na.rm = TRUE)) %>%
            ungroup() %>% suppressWarnings()
          dat <- dat %>% mutate(tempvar1 = ifelse((tempvar1 == Inf | tempvar1 == -Inf) %in% TRUE, NA, tempvar1))
        } # end of if RI_RECORDS_NOT_SOUGHT

        if (RI_RECORDS_SOUGHT_FOR_ALL == 1){
          regage <- rlang::sym(paste0("age_at_",doselist[d],"_register"))
          dat <- dat %>% rowwise() %>% mutate(tempvar1 = min(!!cardage, !!regage,na.rm = TRUE)) %>%
            ungroup() %>% suppressWarnings()
          dat <- dat %>% mutate(tempvar1 = ifelse((tempvar1 == Inf | tempvar1 == -Inf) %in% TRUE, NA, tempvar1))
        }

        if (RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
          regage <- rlang::sym(paste0("age_at_",doselist[d],"_register"))
          dat <- dat %>% rowwise() %>% mutate(tempvar1 = ifelse(no_card %in% 1,
                                                                min(!!cardage, !!regage,na.rm = TRUE), tempvar1)) %>%
            ungroup() %>% suppressWarnings()
          dat <- dat %>% mutate(tempvar1 = ifelse((tempvar1 == Inf | tempvar1 == -Inf) %in% TRUE, NA, tempvar1))
        }

        # Make missing if not eligible
        crude <- rlang::sym(paste0("got_crude_",doselist[d],"_to_analyze"))
        dat <- dat %>% mutate(tempvar1 = ifelse(is.na(!!crude), NA, tempvar1))

        dat <- dat %>% mutate(tempvar2 = NA_character_)

        if (!(TIMELY_DOSE_ORDER[d] %in% TIMELY_CD_LIST)){
          numtile <- as.numeric(TIMELY_N_DTS) - 1

          for (j in 1:numtile){
            dt_ub <- get(paste0("TIMELY_DT_UB_",j), envir = .GlobalEnv)
            dt_label <- get(paste0("TIMELY_DT_LABEL_",j), envir = .GlobalEnv)
            minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)

            dat <- dat %>%
              mutate(
                tempvar2 = ifelse(
                  ((tempvar1 < (minage + dt_ub)) %in% TRUE
                   & !is.na(!!crude))
                  & is.na(tempvar2),
                  dt_label, tempvar2))

            # If it's a birth dose, it cannot be early
            if (j %in% 1 & minage %in% 0 & dt_ub %in% 0){
              dat$tempvar2 <- NA_character_
            }
          } # end of numtile j loop

          dt_label <- get(paste0("TIMELY_DT_LABEL_",TIMELY_N_DTS), envir = .GlobalEnv)
          dat <- dat %>% mutate(tempvar2 = ifelse(is.na(tempvar2), dt_label, tempvar2))

          dat$tempvar1 <- haven::labelled(dat$tempvar1, label = paste0("VCTC 01 - Age at", doselist[d]))
          dat$tempvar2 <- haven::labelled(dat$tempvar2, label = paste0("VCTC 01 - category for ", doselist[d]))

          names(dat)[which(names(dat) == "tempvar1")] <- paste0("timely_age_at_",doselist[d])
          names(dat)[which(names(dat) == "tempvar2")] <- paste0("timely_category_",doselist[d])
        } else {

          cdt <- get(paste0("TIMELY_CD_",TIMELY_DOSE_ORDER[d],"_NTILES"), envir = .GlobalEnv)
          numtile <- as.numeric(cdt) - 1

          for (j in 1:numtile){
            dt_ub <- get(paste0("TIMELY_CD_", TIMELY_DOSE_ORDER[d] ,"_UB_", j),
                         envir = .GlobalEnv)

            dt_label <- get(paste0("TIMELY_CD_", TIMELY_DOSE_ORDER[d], "_LABEL_", j),
                            envir = .GlobalEnv)

            minage <- get(paste0(doselist[d],"_min_age_days"), envir = .GlobalEnv)

            dat <- dat %>%
              mutate(tempvar2 = ifelse(
                ((tempvar1 < (minage + dt_ub)) %in% TRUE
                 & !is.na(!!crude))
                & is.na(tempvar2),
                dt_label, tempvar2))

            # If it's a birth dose, it cannot be early
            if (j %in% 1 & minage %in% 0 & dt_ub %in% 0){
              dat$tempvar2 <- NA_character_
            }
          } #end of numtile j loop

          dt_label <- get(paste0("TIMELY_CD_",TIMELY_DOSE_ORDER[d],"_LABEL_",cdt), envir = .GlobalEnv)
          dat <- dat %>% mutate(tempvar2 = ifelse(is.na(tempvar2), dt_label, tempvar2))

          dat$tempvar1 <- haven::labelled(dat$tempvar1, label = paste0("VCTC 01 - Age at", doselist[d]))
          dat$tempvar2 <- haven::labelled(dat$tempvar2, label = paste0("VCTC 01 - category for ", doselist[d]))

          names(dat)[which(names(dat) == "tempvar1")] <- paste0("timely_age_at_",doselist[d])
          names(dat)[which(names(dat) == "tempvar2")] <- paste0("timely_category_",doselist[d])
        }
      } # end of doselist d loop

      saveRDS(dat,file = paste0(VCQI_OUTPUT_FOLDER,"/RI_VCTC_01_",ANALYSIS_COUNTER,".rds"))
    }

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
