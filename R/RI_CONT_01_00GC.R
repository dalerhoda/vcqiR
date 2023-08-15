#' Check global macros for RI_CONT_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import stringr

# RI_CONT_01_00GC R version 1.00 - Biostat Global Consulting - 2022-12-15
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-15  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_CONT_01_00GC <- function(VCP = "RI_CONT_01_00GC"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  vcqi_log_global(RI_CONT_01_DROPOUT_LIST)
  vcqi_log_global(RI_DOSE_LIST)

  exitflag <- 0
  errormsgs <- NULL

  # Confirm global RI_CONT_01_DROPOUT_LIST is defined

  if (!vcqi_object_exists("RI_CONT_01_DROPOUT_LIST")){
    errormsgs <- c(errormsgs,"You must define global variable RI_CONT_01_DROPOUT_LIST.")
    vcqi_log_comment(VCP, 1, "Error", "You must define global variable RI_CONT_01_DROPOUT_LIST.")
    exitflag <- 1
  }

  #NOTE: this is different from Stata since length of NA is considered to be 1, thus confusing message might occur
  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Confirm dose names in global RI_CONT_01_DROPOUT_LIST are found in global RI_DOSE_LIST
  match = 0
  dropoutlower <- str_to_lower(RI_CONT_01_DROPOUT_LIST)
  rilower <- str_to_lower(RI_DOSE_LIST)

  for (g in seq_along(dropoutlower)){

    for (d in seq_along(rilower)){

      if ((rilower[d] == dropoutlower[g]) %in% TRUE){
        match = match +1
      }

    } #end of rilower d loop

  } #end of dropoutlower g loop

  if (match != length(RI_CONT_01_DROPOUT_LIST)){
    errormsgs <- c(errormsgs,
                   "Some dose names in global RI_CONT_01_DROPOUT_LIST are not included in the RI_DOSE_LIST.")
    vcqi_log_comment(VCP, 1,"Error","Some dose names in global RI_CONT_01_DROPOUT_LIST are not included in the RI_DOSE_LIST.")
    exitflag <- 1
  }

  # Confirm global RI_CONT_01_DROPOUT_LIST is a multiple of 2

  if ((length(RI_CONT_01_DROPOUT_LIST) %% 2) != 0){
    errormsgs <- c(errormsgs,
                   "RI_CONT_01_DROPOUT_LIST does not contain an even number of dose names.")
    vcqi_log_comment(VCP, 1,"Error","RI_CONT_01_DROPOUT_LIST does not contain an even number of dose names.")
    exitflag <- 1
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
