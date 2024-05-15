#' Pre-process dataset for RI_QUAL_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_QUAL_01_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect
#' @import stringr

# RI_QUAL_01_01PP R version 1.02 - Biostat Global Consulting - 2024-03-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-20  1.00      Mia Yu          Original R package version
# 2023-07-18  1.01      Mia Yu          Keep level3name and match Stata version
# 2024-03-20  1.02      Mia Yu          Add VCQI_PASS_THRU_VARLIST to selection list
# *******************************************************************************

RI_QUAL_01_01PP <- function(VCP = "RI_QUAL_01_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_with_ids.rds"))

  dlist <- NULL
  rilower <- str_to_lower(RI_DOSE_LIST)

  for (d in seq_along(rilower)){
    dlist <- c(dlist, paste0(rilower[d],"_card_date"),paste0(rilower[d],"_register_date"),
               paste0(rilower[d],"_card_tick"),paste0(rilower[d],"_register_tick"))
  } #end of d loop

  dat <- dat %>% select(level1id,level2id,level3id,level3name,stratumid,clusterid,respid,
                        RI01,RI03,RI11,RI12,HH02,HH04,psweight,
                        all_of(VCQI_LEVEL4_SET_VARLIST),all_of(dlist),RI27,no_card,no_register,
                        all_of(VCQI_PASS_THRU_VARLIST))

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_01_",ANALYSIS_COUNTER,".rds"))

  if (!vcqi_object_exists("RI_QUAL_01_TEMP_DATASETS")){
    RI_QUAL_01_TEMP_DATASETS <- NULL
  }
  vcqi_global(RI_QUAL_01_TEMP_DATASETS,
              c(RI_QUAL_01_TEMP_DATASETS,
                paste0("RI_QUAL_01_", ANALYSIS_COUNTER, ".rds")
              ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
