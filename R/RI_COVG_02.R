#' Calculate valid coverage
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Derived variables, databases, tables, and plots
#' @export
#'
#' @examples
#' RI_COVG_02()

# RI_COVG_02 R version 1.01 - Biostat Global Consulting - 2022-10-10
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-08-10  1.00      Mia Yu          Original R version
# 2022-10-10  1.01      Mia Yu          Package version
# *******************************************************************************

RI_COVG_02 <- function(VCP = "RI_COVG_02"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (VCQI_NO_DOBS == 1){
    vcqi_log_comment(VCP, 2, "Warning", "User requested RI_COVG_02 but no respondents have full date of birth info, so the indicator will be skipped.")
  } else{
    print(paste0("Calculating ", VCP))

    if (VCQI_PREPROCESS_DATA %in% 1){
      print("Pre-processing dataset")
      RI_COVG_02_01PP()
    }

    if (VCQI_GENERATE_DVS %in% 1){
      print("Calculating derived variables")
      RI_COVG_02_03DV()
    }

    if (VCQI_GENERATE_DATABASES %in% 1){
      print("Generating output databases")
      RI_COVG_02_04GO()
    }

    if(EXPORT_TO_EXCEL %in% 1){
      print("Exporting to Excel")
      RI_COVG_02_05TOST()
    }

    if (MAKE_PLOTS %in% 1){
      print("Making plots")
      RI_COVG_02_06PO()
    }

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
