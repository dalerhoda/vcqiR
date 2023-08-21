#' Merge RI indicators calculated by VCQI onto the originally provided RI dataset
#'
#' @param outpath Path where RI indicator datasets are saved (by default the function looks in VCQI_OUTPUT_FOLDER)
#' @param analysiscounter The analysiscounter number, must be an integer bigger than 0
#'
#' @return RI_augmented_dataset and vcqi_ads_logfile datasets in VCQI_OUTPUT_FOLDER
#'
#' @export
#'
#' @importFrom utils type.convert
#' @import dplyr
#' @import tidyselect
#' @import stringr
#'
#' @examples
#' make_RI_augmented_dataset(outpath = NA)

# make_RI_augmented_dataset R version 1.02 - Biostat Global Consulting - 2023-01-06
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-01  1.00      Mia Yu          Original R version
# 2022-11-02  1.01      Mia Yu          Package version
# 2023-01-06  1.02      Mia Yu          Added new indicators
# *******************************************************************************

# General Notes:
# This program uses the VCQI temp datasets.
#
# There are two ways you can run this program:
# 1. In the RI Control Program by calling the program before vcqi_cleanup.
# 2. Separate from a VCQI Control Program. This requires that vcqi_global
#    DELETE_TEMP_VCQI_DATASETS be set to 0 to save the temp datasets.
#
# This program can only be run on RI Indicators that result in a dataset with
# a single line per person. RI_COVG_05 will not be included.

make_RI_augmented_dataset <- function(outpath = NA, analysiscounter = NA){

  if (is.na(outpath)){
    outpath <- VCQI_OUTPUT_FOLDER
  }

  if (is.na(analysiscounter)){
    # If analysiscounter not define for the function, set it to be 10
    analysiscounter <- 10
  } else {
    # Check that analysis counter is numeric and > 0
    if (!is.numeric(type.convert(analysiscounter, as.is = TRUE))){
      stop("analysiscounter must be a number.")}
    if (analysiscounter < 1){
      stop("analysiscounter must be 1 or higher")
    }
  }

  # This program will be used to take the individual indicator datasets to make
  # one large VCQI dataset

  # Note: Be sure to change the vcqi_global DELETE_TEMP_VCQI_DATASETS to 0 so
  # the datasets are saved

  # Set directory to be where the VCQI DATASETS are located

  # NOTE: for now we only have these RI programs
  RILIST <- c("RI_COVG_01","RI_COVG_02","RI_COVG_03","RI_COVG_04","RI_CONT_01",
              "RI_QUAL_01","RI_QUAL_02","RI_QUAL_07B","RI_QUAL_08","RI_QUAL_09")

  # Delete dataset if it already existis
  if(file.exists(paste0(outpath, "/RI_augmented_dataset.rds"))) {
    file.remove(paste0(outpath, "/RI_augmented_dataset.rds"))
  }

  # First step is to check to see which RI VCQI datasets have been created...
  # Note this will NOT capture any DESC or COVG DIFF datasets

  # Begin by making a file list of all applicable datasets
  filelist <- NULL

  for (v in seq_along(RILIST)) {
    # Check to see if there are files with multiple analysis counters
    for (i in 1:analysiscounter) {
      if (file.exists(paste0(VCQI_OUTPUT_FOLDER, "/",RILIST[v],"_",i,".rds"))) {
        filelist <- c(filelist, paste0(RILIST[v],"_",i,".rds"))
      }

    } #end of i loop

  } #end of v loop

  #NOTE: for now we don't need check_multi_row_indicators
  #check_multi_row_indicators, analysiscounter(`analysiscounter') filelist(`filelist')

  # For all datasets, create a local with the varlist and with dataset name to be used later on
  for (i in seq_along(filelist)){

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/",filelist[i]))

    dat <- haven::zap_label(dat)
    dat <- haven::zap_labels(dat)

    dat <- dat %>% arrange(respid,stratumid,clusterid)

    # Save a copy of the dataset file that will be edited with any variable renames
    saveRDS(dat,paste0(outpath, "/VCQI_ADS_",filelist[i]))

  } #end of i loop

  if (vcqi_object_exists("VCQI_RIHC_DATASET")){

    dat <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_RIHC_DATASET))

    dat <- rename(dat, RI01 = RIHC01, RI03 = RIHC03, RI11 = RIHC14, RI12 = RIHC15)

    dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_RI_DATASET))

    # Merge with RI dataset
    dat <- full_join(dat,dat2, by = c("RI01", "RI03", "RI11", "RI12"))

  } else {

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_RI_DATASET))

  }

  saveRDS(dat,paste0(outpath, "/RI_augmented_dataset.rds"))

  keep <- c("level1id","level2id","level3id", VCQI_LEVEL4_SET_VARLIST)

  dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds")) %>% select(all_of(keep),RI01,RI03,RI11,RI12)
  dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
  dupname <- dupname[which(!dupname %in% c("RI01", "RI03", "RI11", "RI12"))]
  dat2 <- dat2 %>% select(-c(all_of(dupname)))

  dat <- full_join(dat, dat2, by = c("RI01", "RI03", "RI11", "RI12"))
  rm(dat2)
  saveRDS(dat,paste0(outpath, "/RI_augmented_dataset.rds"))

  for (i in 1:3){
    dat2 <- vcqi_read(get(paste0("LEVEL",i,"_NAME_DATASET"), envir = .GlobalEnv))
    dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
    dupname <- dupname[which(!dupname %in% paste0("level",i,"id"))]
    dat2 <- dat2 %>% select(-c(all_of(dupname)))

    dat <- full_join(dat, dat2, by = paste0("level",i,"id"))
    dat <- dat %>% relocate(paste0("level",i,"name"),.after = paste0("level",i,"id"))
    saveRDS(dat,paste0(outpath, "/RI_augmented_dataset.rds"))
  }

  # Merge together with the first file in RI_LIST
  ad_1 <- vcqi_read(paste0(outpath, "/VCQI_ADS_",filelist[1]))
  ad_1 <- haven::zap_label(ad_1)
  ad_1 <- haven::zap_labels(ad_1)
  dupname <- names(ad_1)[which(names(ad_1) %in% names(dat))]
  dupname <- dupname[which(!dupname %in% c("RI01", "RI03", "RI11", "RI12"))]
  ad_1 <- ad_1 %>% select(-c(all_of(dupname)))

  ad_1_varlist <- names(ad_1)
  dat <- full_join(dat, ad_1, by = c("RI01", "RI03", "RI11", "RI12"))

  vcqiadsvarlist <- NULL

  for (v in seq_along(ad_1_varlist)){

    tempdf <- data.frame(variable = ad_1_varlist[v], dataset = str_sub(filelist[1], end = -5),
                         analysis_counter = str_sub(filelist[1], start = -5, end = -5),
                         message = "Variable added to varlist from this dataset", new_var_name = "")

    vcqiadsvarlist <- rbind(vcqiadsvarlist, tempdf)

  }
  rm(tempdf)

  dup <- 0
  vartodrop <- NULL

  for (i in seq_along(filelist)){

    if (i > 1){

      ad <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/VCQI_ADS_",filelist[i]))

      ad <- haven::zap_label(ad)
      ad <- haven::zap_labels(ad)

      varlist_ad <- names(ad)

      for (u in seq_along(varlist_ad)){

        if (varlist_ad[u] %in% names(dat)){

          #exclude the id vars for now
          if (!(varlist_ad[u] %in% c("respid", "stratumid", "clusterid"))){

            comparedat <- dat %>% select(respid,clusterid,stratumid, all_of(varlist_ad[u])) %>% arrange(respid)
            comparead <- ad %>% select(respid,clusterid,stratumid, all_of(varlist_ad[u])) %>% arrange(respid)

            comparevar1 <- get(varlist_ad[u], comparedat)
            comparevar2 <- get(varlist_ad[u], comparead)

            if (all.equal(comparevar1, comparevar2, check.attributes = FALSE) %in% TRUE){
              # If the values match, drop the variable from ad and post in the log...
              print("found identical values")

              # if noidenticaldups set to be TRUE, delete the identical variable
              ad <- ad %>% select(-c(all_of(varlist_ad[u])))

              tempdf <- data.frame(
                  variable = varlist_ad[u], dataset = str_sub(filelist[i], end = -5),
                  analysis_counter = str_sub(filelist[i], start = -5, end = -5),
                  message = "Variable existed in previous dataset(s) but had no conflicting values,
                                   thus variable dropped", new_var_name = "")

                vcqiadsvarlist <- rbind(vcqiadsvarlist, tempdf)

            } else {
              # If values differ, then the new variable will need to be renamed and added to the varlist
              print("found some differences.")
              dup = dup + 1

              # However, it might be the same as a previous variable that has been renamed already.

              if (paste0(varlist_ad[u],"_ac",str_sub(filelist[i], start = -5, end = -5)) %in% names(dat)){
                comparedat2 <- dat %>% select(respid,clusterid,stratumid,
                                              all_of(paste0(varlist_ad[u],"_ac",str_sub(filelist[i], start = -5, end = -5)))) %>% arrange(respid)
                comparevar3 <- get(paste0(varlist_ad[u],"_ac",str_sub(filelist[i], start = -5, end = -5)), comparedat2)

                if (all.equal(comparevar3, comparevar2, check.attributes = FALSE) %in% TRUE){
                  # If same as a previous variable that has been renamed already.
                  dup = dup - 1
                  ad <- ad %>% select(-c(all_of(varlist_ad[u])))
                  tempdf <- data.frame(
                    variable = varlist_ad[u], dataset = str_sub(filelist[i], end = -5),
                    analysis_counter = str_sub(filelist[i], start = -5, end = -5),
                    message = "Variable existed in previous dataset(s) but had no conflicting values,
                                   thus variable dropped", new_var_name = "")
                } else {

                  #If the values differ and a renamed variable with same name but different values
                  #TO DO: open to other new names
                  names(ad)[which(names(ad) == varlist_ad[u])] <-
                    paste0(varlist_ad[u],"_ac",str_sub(filelist[i], start = -5, end = -5),"_",str_sub(filelist[i], end = -7))

                  rownum <- which(vcqiadsvarlist$variable == varlist_ad[u]
                                  & vcqiadsvarlist$message == "Variable added to varlist from this dataset")

                  if (length(rownum) > 1){
                    rownum <- rownum[length(rownum)]
                  }

                  ac_before <- as.numeric(vcqiadsvarlist$analysis_counter[rownum])
                  # Create a new variable with new name and add the original name to a list to drop eventually
                  vartodrop <- c(vartodrop, varlist_ad[u])
                  varu <- rlang::sym(varlist_ad[u])
                  dat <- dat %>% mutate(!!paste0(varlist_ad[u],"_ac",ac_before) := !!varu) %>%
                    relocate(paste0(varlist_ad[u],"_ac",ac_before),.after = varlist_ad[u])

                  vcqiadsvarlist$new_var_name[rownum] <- paste0(varlist_ad[u],"_ac",ac_before)

                  tempdf <- data.frame(variable = varlist_ad[u], dataset = str_sub(filelist[i], end = -5),
                                       analysis_counter = str_sub(filelist[i], start = -5, end = -5),
                                       message = "Variable existed in previous dataset(s) with conflicting values",
                                       new_var_name = paste0(varlist_ad[u],"_ac",str_sub(filelist[i], start = -5, end = -5)))

                  vcqiadsvarlist <- rbind(vcqiadsvarlist, tempdf)



                }
              } else {

                #If the values differ and no renamed variable with same name
                names(ad)[which(names(ad) == varlist_ad[u])] <-
                  paste0(varlist_ad[u],"_ac",str_sub(filelist[i], start = -5, end = -5))

                rownum <- which(vcqiadsvarlist$variable == varlist_ad[u]
                                & vcqiadsvarlist$message == "Variable added to varlist from this dataset")

                if (length(rownum) > 1){
                  rownum <- rownum[length(rownum)]
                }

                ac_before <- as.numeric(vcqiadsvarlist$analysis_counter[rownum])
                # Create a new variable with new name and add the original name to a list to drop eventually
                vartodrop <- c(vartodrop, varlist_ad[u])
                varu <- rlang::sym(varlist_ad[u])
                dat <- dat %>% mutate(!!paste0(varlist_ad[u],"_ac",ac_before) := !!varu) %>%
                  relocate(paste0(varlist_ad[u],"_ac",ac_before),.after = varlist_ad[u])

                vcqiadsvarlist$new_var_name[rownum] <- paste0(varlist_ad[u],"_ac",ac_before)

                tempdf <- data.frame(variable = varlist_ad[u], dataset = str_sub(filelist[i], end = -5),
                                     analysis_counter = str_sub(filelist[i], start = -5, end = -5),
                                     message = "Variable existed in previous dataset(s) with conflicting values",
                                     new_var_name = paste0(varlist_ad[u],"_ac",str_sub(filelist[i], start = -5, end = -5)))

                vcqiadsvarlist <- rbind(vcqiadsvarlist, tempdf)
              }

            }

          }

        } else if (!(varlist_ad[u] %in% names(dat))){
          # If the variable does not exist in any previous datasets, add it to the augmented log and post
          print(paste0(filelist[i],": ",varlist_ad[u]," is unique so far."))

          tempdf <- data.frame(variable = varlist_ad[u], dataset = str_sub(filelist[i], end = -5),
                               analysis_counter = str_sub(filelist[i], start = -5, end = -5),
                               message = "Variable added to varlist from this dataset", new_var_name = "")

          vcqiadsvarlist <- rbind(vcqiadsvarlist, tempdf)

          rm(tempdf)
        }

      } #end of varlist_ad loop

      dat <- full_join(dat,ad,by = c("respid", "stratumid", "clusterid"))
      saveRDS(dat,paste0(outpath, "/RI_augmented_dataset.rds"))

    } #end of if i > 1

  } #end of i loop

  dat <- dat %>% select(-c(all_of(vartodrop)))
  saveRDS(dat,paste0(outpath, "/RI_augmented_dataset.rds"))

  saveRDS(vcqiadsvarlist, paste0(outpath, "/vcqi_ads_logfile.rds"))

  for (v in seq_along(filelist)){
    file.remove(paste0(outpath, "/VCQI_ADS_",filelist[v]))
  }

  print(paste0("VCQI Augmented Dataset Program identified ", dup,
               " variables that had conflicting values between the VCQI indicator datasets."))
  print("Please reference the log: vcqi_ads_logfile.dta for details.")
  print("VCQI Augmented Dataset was saved as RI_augmented_dataset.rds")
}
