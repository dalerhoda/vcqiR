#' Check/create unique ID variables in RI dataset and merge level4 stratifiers
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return RI_with_ids dataset in VCQI_OUTPUT_FOLDER
#'
#' @export
#'
#' @import dplyr
#' @import tidyselect
#'
#' @examples
#' establish_unique_RI_ids()

# establish_unique_RI_ids R version 1.05 - Biostat Global Consulting - 2022-10-07
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-07-17   1.00      Mia Yu         Original R version
# 2022-08-07   1.01      Mia Yu         Fixes the problem that occurs when level 4
#                                       not set
# 2022-08-11   1.02      Caitlin Clary  Updated process to merge level4 variables,
#                                       reformatted code throughout
# 2022-09-23   1.03      Mia Yu         Updated to drop variables after merging
# 2022-10-05   1.04      Mia Yu         Package version
# 2022-10-07   1.05      Mia Yu         Updated line 152 and the usage of vcqi_object_exists
# *******************************************************************************

establish_unique_RI_ids <- function(VCP = "establish_unique_RI_ids"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if(!(VCQI_CHECK_INSTEAD_OF_RUN %in% 1)){

    # Make a little dataset named level2namesforlevel3

    dat <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_CM_DATASET)) %>%
      select(level3id = "HH01",
             level2id = "province_id") %>%
      distinct()

    level2name <- vcqi_read(LEVEL2_NAME_DATASET)

    dat <- left_join(dat, level2name, by = "level2id") %>%
      rename(level2nameforlevel3 = level2name)

    rm(level2name)

    saveRDS(dat, paste0(VCQI_OUTPUT_FOLDER,"/level2namesforlevel3.rds"))

    # Now add ID variables to the RI household interview dataset
    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/",
                            tools::file_path_sans_ext(VCQI_RI_DATASET),"_clean.rds")) %>%
      mutate(RI11 = as.character(RI11))

    # Save initial version of RI_with_ids
    saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))

    vcqi_global(RI_TEMP_DATASETS, c(RI_TEMP_DATASETS, "RI_with_ids.rds"))

    # Prepare RI data for merging
    dat <- dat %>%
      mutate(HH01 = RI01,
             HH03 = RI03,
             dataframe = "dat1")

    # Read CM data
    dat2 <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET)) %>%
      select(HH01, HH03, urban_cluster, psweight_1year, province_id, HH04, HH02) %>%
      mutate(dataframe = "dat2")

    # Join variables from CM
    dat <- full_join(dat, dat2, by = c("HH01", "HH03"))
    rm(dat2)

    # We want to keep clusters that do not appear in the dataset, for purposes
    # of calculating degrees of freedom. Be sure to set their weight to zero so
    # they are included properly in the calculations. Note that all outcomes
    # will be missing for these respondents, so they will not affect point
    # estimates, but their presence will help make the DOF calculation right.

    dat <- dat %>%
      mutate(
        psweight_1year = ifelse((is.na(dataframe.x) & dataframe.y %in% "dat2") %in% TRUE,
                                0, psweight_1year),
        RI01 = ifelse(is.na(dataframe.x) & dataframe.y %in% "dat2", HH01, RI01)
      )

    if(!("RI02" %in% names(dat))){dat <- mutate(dat, RI02 = NA)}

    dat <- dat %>%
      mutate(
        RI02 = ifelse((is.na(dataframe.x) & dataframe.y %in% "dat2") %in% TRUE, HH02, RI02),
        RI03 = ifelse((is.na(dataframe.x) & dataframe.y %in% "dat2") %in% TRUE, HH03, RI03)
      )

    if(!("RI04" %in% names(dat))){dat <- mutate(dat, RI04 = NA)}

    dat <- dat %>% mutate(
      RI04 = ifelse((is.na(dataframe.x) & dataframe.y %in% "dat2") %in% TRUE, HH04, RI04),
      RI11 = ifelse((is.na(dataframe.x) & dataframe.y %in% "dat2") %in% TRUE, "1", RI11),
      RI12 = ifelse((is.na(dataframe.x) & dataframe.y %in% "dat2") %in% TRUE, 1, RI12),
      stratumid = RI01
    ) %>%
      rename(psweight = psweight_1year) %>%
      select(-dataframe.x, -dataframe.y)

    # If RI03 is unique within RI01 then we can simply use RI03
    # as the clusterid; otherwise we want to make a unique clusterid

    dat <- dat %>%
      group_by(RI01, RI03) %>%
      mutate(rowid = row_number()) %>%
      ungroup() %>%
      mutate(dropthis1 = ifelse(rowid == 1, 1, 0)) %>%
      group_by(RI03) %>%
      mutate(dropthis2 = sum(dropthis1)) %>%
      ungroup()

    if(all(dat$dropthis2 == 1)){
      dat <- mutate(dat, clusterid = RI03)
    } else{
      dat <- dat %>% group_by(RI01, RI03) %>% mutate(clusterid = cur_group_id()) %>% ungroup()
    }

    dat <- select(dat, -c(rowid, dropthis1, dropthis2))

    dat <- dat %>% arrange(RI01, RI03, RI11) %>%
      group_by(RI01,RI03,RI11) %>%
      mutate(hhid = cur_group_id()) %>%
      ungroup() %>%
      group_by(RI01,RI03,RI11,RI12) %>%
      mutate(respid = cur_group_id()) %>%
      ungroup() %>%
      mutate(level1id = 1,
             level2id = province_id,
             level3id = RI01)

    # Obtain level1 names from a small dataset for that purpose
    level1name <- vcqi_read(LEVEL1_NAME_DATASET)
    dat <- left_join(dat, level1name, by = "level1id")

    # Obtain province names from a small dataset for that purpose
    level2name <- vcqi_read(LEVEL2_NAME_DATASET)
    dat <- left_join(dat, level2name, by = "level2id")

    # Obtain stratum names from a small dataset for that purpose
    level3name <- vcqi_read(LEVEL3_NAME_DATASET)
    dat <- left_join(dat, level3name, by = "level3id")

    rm(level1name, level2name, level3name)

    # Check for level4 stratifiers and merge them in if necessary

    if(vcqi_object_exists("VCQI_LEVEL4_SET_VARLIST")){
      level4 <- get("VCQI_LEVEL4_SET_VARLIST", envir = .GlobalEnv)
    } else{
      level4 <- NULL
    }

    if(!is.null(level4)){

      for(v in seq_along(level4)){

        l4_in_RI <- FALSE

        if(level4[v] %in% names(dat)){
          print(paste0("The stratifier ", level4[v], " is already part of the RI dataset."))
          l4_in_RI <- TRUE
        }

        if(l4_in_RI == FALSE){

          # 1. Try to merge stratifier from HM dataset
          print(paste0("Variable ", level4[v], " is not in the RI dataset; try to merge from HM"))
          dat <- dat %>% mutate(HM01 = RI01,
                                HM03 = RI03,
                                HM09 = RI11,
                                HM22 = RI12)

          HM <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_HM_DATASET))

          if(level4[v] %in% names(HM)){
            HM <- select(HM, HM01, HM03, HM09, HM22, level4[v])

            dat <- left_join(dat, HM, by = c("HM01", "HM03", "HM09", "HM22")) %>%
              select(-c(HM01, HM03, HM09, HM22))

            print(paste0("Variable ", level4[v], " found in HM dataset"))

            rm(HM)

            l4_in_RI <- TRUE
          }

          # 2. Try to merge stratifier from HH dataset
          if(l4_in_RI == FALSE){

            print(paste0("Trying to merge variable ", level4[v], " from HH"))

            HH <- vcqi_read(paste0(VCQI_DATA_FOLDER,"/",VCQI_HH_DATASET))

            dat <- dat %>% mutate(HH01 = HM01,
                                  HH03 = HM03,
                                  HH14 = HM09)

            if(level4[v] %in% names(HH)){
              HH <- select(HH, c(HH01, HH03, HH14, level4[v]))

              dat <- left_join(dat, HH, by = c("HH01", "HH03", "HH14")) %>%
                select(-c(HH01,HH03,HH14))

              print(paste0("Variable ", level4[v], " found in HH dataset"))

              rm(HH)

              l4_in_RI <- TRUE

            }
          }

          # 3. Try to merge stratifier from CM dataset
          if(l4_in_RI == FALSE){
            print(paste0("Trying to merge variable ", level4[v], " from CM"))

            CM <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET))

            if(level4[v] %in% names(CM)){
              CM <- select(CM, HH01, HH03, level4[v])

              dat <- left_join(dat, CM, by = c("HH01", "HH03")) %>%
                select(-c(HH01, HH03))

              print(paste0("Variable ", level4[v], " found in CM dataset"))

              rm(CM)

              l4_in_RI <- TRUE
            }
          }

          # 4. If not merged from any source, print message
          if(l4_in_RI == FALSE){
            print(paste0("Did not merge ", level4[v], " onto RI dataset"))
          }

        } # End process when stratifier was not already in RI dataset
      } # End of level4 loop

      rm(l4_in_RI)
    } else {
      dat <- select(dat, -c(HH01, HH03))
    }

    vartodrop <- c("HM01", "HM03","HM09","HM22","HH01","HH03","HH14")
    vartodrop <- vartodrop[which(vartodrop %in% names(dat))]

    dat <- dat %>% select(-all_of(vartodrop))

    # Save updated RI_with_ids
    saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds"))
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

