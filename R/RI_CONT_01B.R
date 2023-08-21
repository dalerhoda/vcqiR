#' Dropout between two crude doses
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases, tables, and plots
#' @export
#'
#' @examples
#' RI_CONT_01B()

# RI_CONT_01B R version 1.00 - Biostat Global Consulting - 2023-07-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-07-18  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_CONT_01B <- function(VCP = "RI_CONT_01B"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP, "..."))

  print("Checking global macros")
  RI_CONT_01B_00GC()

  if(VCQI_PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    RI_CONT_01B_01PP()
  }

  if(VCQI_GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    RI_CONT_01B_03DV()
  }

  if(VCQI_GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    RI_CONT_01B_04GO()
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    RI_CONT_01B_05TOST()
  }

  if(MAKE_PLOTS %in% 1){
    print("Making plots")
    RI_CONT_01B_06PO()
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
