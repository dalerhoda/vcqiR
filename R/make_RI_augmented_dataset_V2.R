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
#' @importFrom utils glob2rx
#' @import dplyr
#' @import tidyselect
#' @rawNamespace import(rlang, except = c(local_options, with_options))
#' @import stringr
#'
#' @examples
#' make_RI_augmented_dataset_v2(outpath = NA)

# make_RI_augmented_dataset_v2 R version 1.01 - Biostat Global Consulting - 2023-07-29
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-04-03  1.00      Mia Yu          Copied over from make_RI_augmented_dataset_updated
#                                       Changed logic to match the new Stata version
# 2023-07-29  1.01      Mia Yu          Added code to merge in RI_MOV_flags_to_merge;
#                                       Added code to include VCTC timely_age_at* and timely_category_* variables
# 2023-09-29  1.02      Caitlin Clary   Keep psweight in dataset (mirror Stata)
# *******************************************************************************

# This program creates one RI dataset containing the original RI dataset provided in VCQI
# And all RI Indicator datasets found in the OUTPUTPATH.

# ********************************************************************************
# Program Syntax
#
# There are no required inputs for this program to run.
# ********************************************************************************
# Optional Options:
#
# OUTPUTPATH 	-- format: 			string
#				description:	Folder where you would like the Augmented dataset to be saved.
#				default value:	Current Directory
#
# ANALYSISCOUNTER -- format: 	integer
#               description:	The Analysis Counter is a global that is set when you run VCQI.
#								              This program will look in the OUTPUTpath for VCQI datasets with the ANALYSIScounter suffix.
#								              Enter in the highest ANALYSIScounter you used for this VCQI dataset
#								              if you would like it to be part of the augmented dataset.
# 				    default value:	10
#				          note1: 			If the Analysis Counter is greater than 10 this will need to be populated.
#
# *******************************************************************************
# General Notes:
# This program uses the VCQI temp datasets.
#
# There are two ways you can run this program:
#
# 1. During RI Control Program by placing the program sytax before vcqi_cleanup.
# 2. Separate from VCQI Control Program. This requires that vcqi_global DELETE_TEMP_VCQI_DATASETS be set to 0 to save the temp datasets.
#
# This program can only be ran on RI Indicators that result in a dataset with a single line per person.
# RI_COVG_05 will not be included.
#
# *******************************************************************************

make_RI_augmented_dataset_v2 <- function(outpath = NA, analysiscounter = 10){

  # This program will be used to take the individual indicator datasets to make
  # one large VCQI dataset

  # Note: Be sure to change the vcqi_global DELETE_TEMP_VCQI_DATASETS to 0 so
  # the datasets are saved

  # Set directory to be where the VCQI DATASETS are located

  if (!is.na(outpath)){
    setwd(outpath)
  } else {
    setwd(VCQI_OUTPUT_FOLDER)
  }

  #NOTE: this part is different than Stata since Stata can control the type of
  #      of the input value
  # If not default value, check to make sure that AC is an interger >= 1
  if (analysiscounter != 10){
    # Check that analysis counter is numeric and > 0
    if (!is.integer(type.convert(analysiscounter, as.is = TRUE))){
      stop("analysiscounter must be an interger")}
    if (analysiscounter < 1){
      stop("analysiscounter must be 1 or higher")
    }
  }

  # Setup global RILIST to contain all indicators that could have been run...
  # Note: Anytime a new RI indicator is created it will need to be added to this list
  # Note: This program will only work for indicators where the dataset has 1 row per person
  # RI_COVG_05 is an indicator completed at the Cluster level and
  # is not included in the augmented dataset
  # NOTE: We currently only implemented some indicators

  RILIST <- c("RI_COVG_01","RI_COVG_02","RI_COVG_03","RI_COVG_04","RI_CONT_01",
              "RI_CONT_01B","RI_QUAL_01","RI_QUAL_02","RI_QUAL_07B","RI_QUAL_08",
              "RI_QUAL_09","RI_VCTC_01")

  RI_COVG_01 <- "got_crude_*"
  RI_COVG_02 <- c("age_at_*", "got_valid_*")
  RI_COVG_03 <- c("fully_vaccinated_*", "fully_vxd_for_age_crude","fully_vxd_for_age_valid")
  RI_COVG_04 <- c("not_vaccinated_crude", "not_vaccinated_valid", "not_vaccinated_by_age1")
  RI_CONT_01 <- "dropout_*"
  RI_CONT_01B <- "wtd_dropout_*"
  RI_QUAL_01 <- c("had_card", "had_card_with_*", "had_card_or_register")
  if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
    RI_QUAL_01 <- c(RI_QUAL_01,"had_register", "had_register_with_*")
  }
  RI_QUAL_02 <- "ever_had_an_ri_card"
  RI_QUAL_07B <- c("age_at_visit", "num_days_since_*", "got_hypo_*")
  RI_QUAL_08 <- c("total_mov_visits_*", "total_elig_visits_*", "total_movs_*")
  RI_QUAL_09 <- c("doses_with_*", "child_had_*")
  RI_VCTC_01 <- c("timely_age_at_*", "timely_category_*")

  # Delete dataset if it already existis
  if(file.exists("RI_augmented_dataset_v2.rds")) {
    file.remove("RI_augmented_dataset_v2.rds")
  }

  # First step is to check to see which RI VCQI datasets have been created...
  # Note this will NOT capture any DESC or COVG DIFF datasets

  # Begin by making a file list of all applicable datasets
  filelist <- NULL
  filelist_globals <- NULL
  for (v in seq_along(RILIST)) {
    # Check to see if there are files with multiple analysis counters
    for (i in 1:analysiscounter) {
      # Confirm which datasets exist and add them to the filelist
      if (file.exists(paste0(VCQI_OUTPUT_FOLDER, "/",RILIST[v],"_",i,".rds"))) {
        filelist <- c(filelist, paste0(RILIST[v],"_",i,".rds"))
        filelist_globals <- c(filelist_globals,RILIST[v])
      }

    } #end of i loop

  } #end of v loop

  # Now we will check to see if the Indicators that create multiple rows per
  # person were ran.
  # If they were, reshape so there is only 1 row per person

  # NOTE: placeholder here for check_multi_row_indicators program

  # Start the dataset with the original RI dataset provided in VCQI

  # Merge in the RIHC dataset it it exists
  # Open the RIHC dataset and rename Stratum, Cluster, HH and childid for merging purposes

  if (vcqi_object_exists("VCQI_RIHC_DATASET")){

    dat <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_RIHC_DATASET))

    dat <- rename(dat, RI01 = RIHC01, RI03 = RIHC03, RI11 = RIHC14, RI12 = RIHC15)

    dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_RI_DATASET))

    # 2023-04-03: added this part to match the update option
    dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
    dupname <- dupname[which(!dupname %in% c("RI01", "RI03", "RI11", "RI12"))]
    dat <- dat %>% select(-c(all_of(dupname)))

    # Merge with RI dataset
    dat <- full_join(dat,dat2, by = c("RI01", "RI03", "RI11", "RI12"))

  } else {

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_RI_DATASET))

  }

  saveRDS(dat,"RI_augmented_dataset_v2.rds")

  keep <- c("level1id","level2id","level3id", VCQI_LEVEL4_SET_VARLIST)

  dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds")) %>%
    select(all_of(keep), RI01, RI03, RI11, RI12,
           respid, stratumid, clusterid, psweight)

  dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
  dupname <- dupname[which(!dupname %in% c("RI01", "RI03", "RI11", "RI12"))]
  dat2 <- dat2 %>% select(-c(all_of(dupname)))

  dat <- full_join(dat, dat2, by = c("RI01", "RI03", "RI11", "RI12"))
  rm(dat2)
  dat <- dat %>% arrange(respid,stratumid,clusterid)
  saveRDS(dat,"RI_augmented_dataset_v2.rds")

  for (i in 1:3){
    dat2 <- vcqi_read(get(paste0("LEVEL",i,"_NAME_DATASET"), envir = .GlobalEnv))
    dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
    dupname <- dupname[which(!dupname %in% paste0("level",i,"id"))]
    dat2 <- dat2 %>% select(-c(all_of(dupname)))

    dat <- full_join(dat, dat2, by = paste0("level",i,"id"))
    dat <- dat %>% relocate(paste0("level",i,"name"),.after = paste0("level",i,"id"))
    saveRDS(dat,"RI_augmented_dataset_v2.rds")
  }

  orderlist <- names(dat)

  for (i in seq_along(filelist)){
    fileuse <- filelist[i]
    indicator_name <- get(filelist_globals[i])

    ad <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/",fileuse))
    ad <- haven::zap_label(ad)
    ad <- haven::zap_labels(ad)

    ad <- ad %>% arrange(respid,stratumid,clusterid)

    keeplist <- NULL
    for (n in seq_along(indicator_name)){
      ac <- as.numeric(substr(fileuse, nchar(fileuse) - 4,nchar(fileuse) - 4))
      keeplist <- c(keeplist,grep(glob2rx(indicator_name[n]), names(ad), value=TRUE))
    } # end of indicator_name n loop

    ad <- ad %>% select(respid,stratumid,clusterid, all_of(keeplist))

    for (v in seq_along(keeplist)){
      f <- paste0("comment(ad$",keeplist[v],
                  ") <- 'Original Varname -",keeplist[v],
                  "; Analysis Counter - ", ac,
                  "; Indicator - ",filelist_globals[i],"'")
      eval(rlang::parse_expr(f))

      if (grepl("RI_QUAL_09", fileuse, fixed = TRUE) & grepl("child_had", keeplist[v], fixed = TRUE)){
        rename <- gsub("child_had_","had_",keeplist[v],fixed = TRUE)
        f <- paste0("comment(ad$",keeplist[v],") <- Changing varname from ",keeplist[v]," to",rename,"due to character limits")
        names(ad)[names(ad) == keeplist[v]] <- rename
        keeplist[v] <- rename
      }

      if (grepl("RI_QUAL_01", fileuse, fixed = TRUE) & grepl("had_register_with_dates_or_ticks", keeplist[v], fixed = TRUE)){
        comment(ad$had_register_with_dates_or_ticks) <-
          "Changing varname from had_register_with_dates_or_ticks to had_reg_with_dates_or_ticks due to character limits"
        names(ad)[names(ad) == keeplist[v]] <- "had_reg_with_dates_or_ticks"
        keeplist[v] <- "had_reg_with_dates_or_ticks"
      }

      if (grepl("RI_QUAL_01", fileuse, fixed = TRUE) & grepl("had_register_with_flawless_dates", keeplist[v], fixed = TRUE)){
        comment(ad$had_register_with_flawless_dates) <-
          "Changing varname from had_register_with_flawless_dates to had_reg_with_flawless_dates due to character limits"
        names(ad)[names(ad) == keeplist[v]] <- "had_reg_with_flawless_dates"
        keeplist[v] <- "had_reg_with_flawless_dates"
      }

      if (ac < 10){
        ac_new <- paste0("0",ac)
      }
      names(ad)[names(ad) == keeplist[v]] <- paste0(keeplist[v],"_",ac_new)
      orderlist <- c(orderlist, paste0(keeplist[v],"_",ac_new))

    } # end of keeplist v loop


    saveRDS(ad,paste0("VCQI_ADS_",fileuse))

    dat <- vcqi_read("RI_augmented_dataset_v2.rds")
    dat <- full_join(dat, ad, by = c("respid", "stratumid", "clusterid"))
    saveRDS(dat,"RI_augmented_dataset_v2.rds")

  } #end of i loop
  dat <- vcqi_read("RI_augmented_dataset_v2.rds")
  dat <- dat %>% relocate(all_of(orderlist))
  saveRDS(dat,"RI_augmented_dataset_v2.rds")

  for (v in seq_along(filelist)){
    file.remove(paste0("VCQI_ADS_",filelist[v]))
  }

  # We want to merge on the derived variables from calc_mov if the file exists
  if (file.exists("RI_MOV_flags_to_merge.rds")) {
    dat2 <- vcqi_read("RI_MOV_flags_to_merge.rds")
    keeplist2 <- NULL
    flags <- c("days_until_cor_*_crude",
               "days_until_cor_*_valid",
               "flag_cor_mov_*_crude",
               "flag_cor_mov_*_valid",
               "flag_had_mov_*_crude",
               "flag_had_mov_*_valid",
               "flag_uncor_mov_*_crude",
               "flag_uncor_mov_*_valid",
               "total_elig_*_crude",
               "total_elig_*_valid",
               "total_mov_*_crude",
               "total_mov_*_valid")
    for (f in seq_along(flags)){
      keeplist2 <- c(keeplist2,grep(glob2rx(flags[f]), names(dat2), value=TRUE))
    }
    dat2 <- dat2 %>%
      select(c(respid,total_movs_crude,total_movs_valid,total_elig_visits_crude,
               total_elig_visits_valid,total_visit_movs_crude,total_visit_movs_valid,
               all_of(keeplist2)))

    dat <- left_join(dat, dat2, by = "respid")
    saveRDS(dat, "RI_augmented_dataset_v2.rds")
  }
}
