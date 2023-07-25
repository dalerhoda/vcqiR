#' Fully vaccinated
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases, tables, and plots
#' @export
#'
#' @examples
#' RI_COVG_03()

# RI_COVG_03 R version 1.00 - Biostat Global Consulting - 2022-12-14
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-14  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_COVG_03 <- function(VCP = "RI_COVG_03"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP, "..."))

  print("Checking global macros")
  RI_COVG_03_00GC()

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    RI_COVG_03_01PP()
  }

  if(VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    RI_COVG_03_03DV()
  }

  if(VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    RI_COVG_03_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    RI_COVG_03_05TOST()
  }

  if(MAKE_PLOTS %in% 1){
    print("Making plots")
    RI_COVG_03_06PO()
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
