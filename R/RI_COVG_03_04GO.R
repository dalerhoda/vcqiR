#' Generate output databases for RI_COVG_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER

# RI_COVG_03_04GO R version 1.00 - Biostat Global Consulting - 2022-12-13
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-13  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_COVG_03_04GO <- function(VCP = "RI_COVG_03_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  make_svyp_output_database(variable = "fully_vaccinated_crude",
                            estlabel = "Fully vaccinated - crude",
                            vid = "fvc",
                            measureid = "RI_COVG_03")

  if (VCQI_NO_DOBS != 1){
    make_svyp_output_database(variable = "fully_vaccinated_valid",
                              estlabel = "Fully vaccinated - valid",
                              vid = "fvv",
                              measureid = "RI_COVG_03")

    make_svyp_output_database(variable = "fully_vaccinated_by_age1",
                              estlabel = "Fully vaccinated with valid doses by age 1",
                              vid = "fva1",
                              measureid = "RI_COVG_03")
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
