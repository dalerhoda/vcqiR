#' Generate output databases for RI_COVG_04
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER

# RI_COVG_04_04GO R version 1.00 - Biostat Global Consulting - 2022-12-14
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-14  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_COVG_04_04GO <- function(VCP = "RI_COVG_04_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  make_svyp_output_database(variable = "not_vaccinated_crude",
                            estlabel = "Not vaccinated - crude",
                            vid = "nvc",
                            measureid = "RI_COVG_04")

  if (VCQI_NO_DOBS != 1){
    make_svyp_output_database(variable = "not_vaccinated_valid",
                              estlabel = "Not vaccinated - valid",
                              vid = "nvv",
                              measureid = "RI_COVG_04")

    make_svyp_output_database(variable = "not_vaccinated_by_age1",
                              estlabel = "No valid doses by age 1",
                              vid = "nva1",
                              measureid = "RI_COVG_04")
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
