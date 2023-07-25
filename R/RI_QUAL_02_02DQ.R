#' Check data quality for RI_QUAL_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Errors if conditions not met
#'
#' @import dplyr

# RI_QUAL_02_02DQ R version 1.00 - Biostat Global Consulting - 2022-12-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-21  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_QUAL_02_02DQ <- function(VCP = "RI_QUAL_02_02DQ"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  dat <- vcqi_read(paste0(VCQI_OUTPUT_FOLDER,"/RI_QUAL_02_",ANALYSIS_COUNTER,".rds"))

  exitflag <- 0
  errormsgs <- NULL

  var <- get("RI26",dat)
  if(!(all(var %in% c(1, 2, 99, NA)))){
    errormsgs <- c(errormsgs, "RI26 contains values that are not the expected values of 1,2,99,NA")
    exitflag <- 1
    vcqi_log_comment(VCP, 1, "Error", "RI26 contains values that are not the expected values of 1,2,99,NA")

    varlevels <- dat %>% count(RI26)
    print(varlevels)
  }

  if(exitflag == 1){
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
