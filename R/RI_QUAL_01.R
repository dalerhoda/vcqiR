#' Card and register availability
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases, tables, and plots
#' @export
#'
#' @examples
#' RI_QUAL_01()

# RI_QUAL_01 R version 1.00 - Biostat Global Consulting - 2022-12-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-20  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_QUAL_01 <- function(VCP = "RI_QUAL_01"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP, "..."))

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    RI_QUAL_01_01PP()
  }

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Checking data quality")
    RI_QUAL_01_02DQ()
  }

  if(VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    RI_QUAL_01_03DV()
  }

  if(VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    RI_QUAL_01_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    RI_QUAL_01_05TOST()
  }

  if(MAKE_PLOTS %in% 1){
    print("Making plots")
    RI_QUAL_01_06PO()
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
