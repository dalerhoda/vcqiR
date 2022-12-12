#' Pre-process dataset for RI_COVG_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_COVG_02_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# RI_COVG_02_01PP R version 1.01 - Biostat Global Consulting - 2022-10-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-10  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# *******************************************************************************

RI_COVG_02_01PP <- function(VCP = "RI_COVG_02_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  #Verify RI_COVG_01 ran
  check_RI_COVG_01_03DV()

  filename <- paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds")
  dat <- vcqi_read(filename)

  dlist <- NULL
  type <- c("card","register")

  for (s in seq_along(type)){
    for (d in seq_along(RI_DOSE_LIST)){
      dlist <- c(dlist,paste0(RI_DOSE_LIST[d],"_",type[s],"_date"))
    } #end of dose loop
  } #end of type loop


  if(!vcqi_object_exists("VCQI_LEVEL4_SET_VARLIST")){
    dat <- select(dat, c(level1id, level2id, level3id, stratumid, clusterid, respid,
                         RI01, RI03, RI11, RI12, HH02, HH04, psweight,
                         all_of(dlist),dob_for_valid_dose_calculations,no_card, age_at_interview))
  }

  if(vcqi_object_exists("VCQI_LEVEL4_SET_VARLIST")){
    dat <- select(dat, c(level1id, level2id, level3id, stratumid, clusterid, respid,
                         RI01, RI03, RI11, RI12, HH02, HH04, psweight,
                         all_of(VCQI_LEVEL4_SET_VARLIST),all_of(dlist),
                         dob_for_valid_dose_calculations,no_card, age_at_interview))
  }


  filename <- paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_02_",ANALYSIS_COUNTER,".rds")
  saveRDS(dat, filename)

  if (!vcqi_object_exists("RI_COVG_02_TEMP_DATASETS")){
    RI_COVG_02_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_COVG_02_TEMP_DATASETS,
              c(RI_COVG_02_TEMP_DATASETS,
                # paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_02_", ANALYSIS_COUNTER,".rds")
                paste0("RI_COVG_02_", ANALYSIS_COUNTER, ".rds")
                ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

