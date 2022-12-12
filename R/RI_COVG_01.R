#' Calculate crude coverage
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases, tables, and plots
#' @export
#'
#' @examples
#' RI_COVG_01()

# RI_COVG_01 R version 1.01 - Biostat Global Consulting - 2022-10-10
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-03  1.00      Mia Yu          Original R version
# 2022-10-10  1.01      Mia Yu          Package version
# *******************************************************************************

RI_COVG_01 <- function(VCP = "RI_COVG_01"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP, "..."))

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    RI_COVG_01_01PP()
  }

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Checking data quality")
    RI_COVG_01_02DQ()
  }

  if(VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    RI_COVG_01_03DV()
  }

  if(VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    RI_COVG_01_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    RI_COVG_01_05TOST()
  }

  if(MAKE_PLOTS %in% 1){
    print("Making plots")
    RI_COVG_01_06PO()
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
