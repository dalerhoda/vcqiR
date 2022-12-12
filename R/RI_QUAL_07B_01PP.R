#' Pre-process dataset for RI_QUAL_07B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A dataset (RI_QUAL_07B_<ANALYSIS_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# RI_QUAL_07B_01PP R version 1.02 - Biostat Global Consulting - 2022-10-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-20  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# 2022-10-24  1.02      Mia Yu          Add vcqi_halt_immediately
# *******************************************************************************

RI_QUAL_07B_01PP <- function(VCP = "RI_QUAL_07B_01PP"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL

  # Confirm calculate_MOV_flags has been run and RI_MOV_long_form_data.dta (i.e., Step07) is available
  if(!file.exists(paste0(VCQI_OUTPUT_FOLDER,"/RI_MOV_long_form_data.rds"))){
    errormsgs <- c(errormsgs,
                   "RI_MOV_long_form_data.dta must exist before running RI_QUAL_07B. Be sure to run calculate_MOV_flags before running RI_QUAL_07B.")
    vcqi_log_comment(VCP, 1, "Error",
                     "RI_MOV_long_form_data.dta must exist before running RI_QUAL_07B. Be sure to run calculate_MOV_flags before running RI_QUAL_07B.")
    exitflag <- 1
  }

  if (exitflag == 1){
    vcqi_global(VCQI_ERROR,1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_MOV_long_form_data.rds"))
  dat <- dat %>% select(respid, dob, visitdate)

  dat2 <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER, "/RI_with_ids.rds"))
  dat2 <- dat2 %>% select(
    respid, clusterid, stratumid, psweight, level1id, level1name, level2id,
    level2name, level3id, level3name, all_of(VCQI_LEVEL4_SET_VARLIST))

  # Merge back on variables needed in the _GO program
  dat <- full_join(dat, dat2, by = "respid")

  saveRDS(dat, file = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_",
                             ANALYSIS_COUNTER, ".rds"))

  if (!vcqi_object_exists("RI_QUAL_07B_TEMP_DATASETS")){
    RI_QUAL_07B_TEMP_DATASETS <- NULL
  }

  vcqi_global(RI_QUAL_07B_TEMP_DATASETS,
              c(RI_QUAL_07B_TEMP_DATASETS,
                paste0("RI_QUAL_07B_", ANALYSIS_COUNTER, ".rds")
                ))

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}

