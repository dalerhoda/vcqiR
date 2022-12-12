#' Pre-process dataset for RI_VCTC_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_VCTC_01_<ANALYSIS_COUNTER>)
#'
#' @import tidyselect
#' @import dplyr
#' @import stringr

# RI_VCTC_01_01PP R version 1.01 - Biostat Global Consulting - 2022-11-03
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-03  1.00      Mia Yu          Original R version
# 2022-11-03  1.01      Mia Yu          Package version
# *******************************************************************************

RI_VCTC_01_01PP <- function(VCP = "RI_VCTC_01_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_CHECK_INSTEAD_OF_RUN != 1){
    #Verify RI_COVG_01 & _02 & RI_QUAL_01 ran
    #NOTE: For now only verify RI_COVG_01 & _02 ran
    check_RI_COVG_01_03DV()
    check_RI_COVG_02_03DV()

    dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_", ANALYSIS_COUNTER,".rds"))
    dat <- haven::zap_label(dat)
    dat <- haven::zap_labels(dat)

    dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_01_", ANALYSIS_COUNTER,".rds"))
    dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
    dupname <- dupname[which(!dupname %in% c("respid"))]
    dat2 <- dat2 %>% select(-c(all_of(dupname)))

    dat <- full_join(dat,dat2,by = "respid")

    if (TIMELY_HBR_LINE_PLOT == 1){
      #NOTE: For now we don't implement this
      dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_01_", ANALYSIS_COUNTER,".rds"))
      dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
      dupname <- dupname[which(!dupname %in% c("respid"))]
      dat2 <- dat2 %>% select(-c(all_of(dupname)))

      dat <- full_join(dat,dat2,by = "respid")
    }

    if (TIMELY_FULLY_VXD_LINE_PLOT == 1 | TIMELY_FULLY_VXD_NOTE == 1){
      #NOTE: For now we don't implement this
      dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_03_", ANALYSIS_COUNTER,".rds"))
      dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
      dupname <- dupname[which(!dupname %in% c("respid"))]
      dat2 <- dat2 %>% select(-c(all_of(dupname)))

      dat <- full_join(dat,dat2,by = "respid")
    }

    if (TIMELY_NOT_VXD_LINE_PLOT == 1 | TIMELY_NOT_VXD_NOTE == 1){
      #NOTE: For now we don't implement this
      dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_04_", ANALYSIS_COUNTER,".rds"))
      dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
      dupname <- dupname[which(!dupname %in% c("respid"))]
      dat2 <- dat2 %>% select(-c(all_of(dupname)))

      dat <- full_join(dat,dat2,by = "respid")
    }

    #NOTE: For now we are not facilitating Vx Coverage & Timeliness Charts (VCTCs) for level 4 strata
    dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds")) %>% select(respid,level1name,level2name,level3name)
    dupname <- names(dat2)[which(names(dat2) %in% names(dat))]
    dupname <- dupname[which(!dupname %in% c("respid"))]
    dat2 <- dat2 %>% select(-c(all_of(dupname)))
    dat <- full_join(dat,dat2,by = "respid")

    dlist <- NULL
    doselist <- str_to_lower(TIMELY_DOSE_ORDER)

    for (d in seq_along(doselist)){
      dlist <- c(dlist,
                 paste0("age_at_",doselist[d],"_card"),
                 paste0("got_crude_",doselist[d],"_to_analyze"))

      if (RI_RECORDS_NOT_SOUGHT != 1){
        dlist <- c(dlist, paste0("age_at_",doselist[d],"_register"))
      }
    } #end of doselist loop

    #NOTE: TIMELY_HBR_LINE_VARIABLE, TIMELY_FULLY_VXD_LINE_VARIABLE, TIMELY_NOT_VXD_LINE_VARIABLE not included for now since we don't implement it
    dat <- dat %>% select(level1id,level2id,level3id,level1name,level2name,level3name,
                          stratumid,clusterid,respid,RI01,RI03,RI11,RI12,HH02,HH04,psweight,
                          all_of(dlist))

    saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_VCTC_01_", ANALYSIS_COUNTER, ".rds"))

    if (!vcqi_object_exists("RI_VCTC_01_TEMP_DATASETS")){
      RI_VCTC_01_TEMP_DATASETS <- NULL
    }
    vcqi_global(RI_VCTC_01_TEMP_DATASETS, c(RI_VCTC_01_TEMP_DATASETS,paste0("RI_VCTC_01_",ANALYSIS_COUNTER,".rds")))

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
