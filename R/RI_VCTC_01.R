#' RI_VCTC_01: Vaccination Coverage and Timeliness Stacked Bar Charts
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Datasets, tables, and plots
#' @export
#'
#' @examples
#' RI_VCTC_01()

# RI_VCTC_01 R version 1.00 - Biostat Global Consulting - 2022-11-10
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-10  1.00      Mia Yu          Package version
# *******************************************************************************

RI_VCTC_01 <- function(VCP = "RI_VCTC_01"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP, "..."))

  print("Checking global macros")
  RI_VCTC_01_00GC()

  if (VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    RI_VCTC_01_01PP()
  }

  if(VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    RI_VCTC_01_03DV()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    RI_VCTC_01_05TO()
  }

  if (MAKE_PLOTS %in% 1){
    print("Making plots")
    RI_VCTC_01_06PO()
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
