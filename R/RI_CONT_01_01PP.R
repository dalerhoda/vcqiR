#' Pre-process dataset for RI_CONT_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_CONT_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @import stringr

# RI_CONT_01_01PP R version 1.01 - Biostat Global Consulting - 2024-03-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-16  1.00      Mia Yu          Original R package version
# 2024-03-20  1.01      Mia Yu          Add VCQI_PASS_THRU_VARLIST to selection list
# *******************************************************************************


RI_CONT_01_01PP <- function(VCP = "RI_CONT_01_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # Verify RI_COVG_01 ran
  check_RI_COVG_01_03DV()

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_COVG_01_",ANALYSIS_COUNTER,".rds"))

  r <- 1 #TO DO: this was never used...
  dlist <- NULL
  dropoutlower <- str_to_lower(RI_CONT_01_DROPOUT_LIST)

  for (d in seq_along(dropoutlower)){
    dlist <- c(dlist, paste0("got_crude_",dropoutlower[d],"_to_analyze"))
  } #end of dropoutlower d loop

  dat <- dat %>% select(level1id,level2id,level3id,stratumid,clusterid,respid,
                        RI01,RI03,RI11,RI12,HH02,HH04,psweight,
                        all_of(VCQI_LEVEL4_SET_VARLIST),all_of(dlist),
                        all_of(VCQI_PASS_THRU_VARLIST))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_CONT_01_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("RI_CONT_01_TEMP_DATASETS")){
    RI_CONT_01_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_CONT_01_TEMP_DATASETS,
              c(RI_CONT_01_TEMP_DATASETS,
                paste0("RI_CONT_01_", ANALYSIS_COUNTER, ".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
