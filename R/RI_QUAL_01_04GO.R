#' Generate output databases for RI_QUAL_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_01_04GO R version 1.00 - Biostat Global Consulting - 2022-12-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-20  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_QUAL_01_04GO <- function(VCP = "RI_QUAL_01_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  source <- "card"

  if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
    source <- c(source, "register")
  }

  for (s in seq_along(source)){
    # This indicator tabulates weighted results...make that database
    make_svyp_output_database(
      variable = paste0("had_", source[s]),
      estlabel = paste0("RI ", stringr::str_to_title(source[s])," Availability (%)"),
      vid = source[s],
      measureid = "RI_QUAL_01")
    make_svyp_output_database(
      variable = paste0("had_", source[s], "_with_dates"),
      estlabel = paste0("RI ", stringr::str_to_title(source[s])," with Dates (%)"),
      vid = paste0(source[s],"_dates"),
      measureid = "RI_QUAL_01")
    make_svyp_output_database(
      variable = paste0("had_", source[s], "_with_dates_or_ticks"),
      estlabel = paste0("RI ", stringr::str_to_title(source[s])," with Dates or Ticks (%)"),
      vid = paste0(source[s],"_dates_ticks"),
      measureid = "RI_QUAL_01")
    make_svyp_output_database(
      variable = paste0("had_", source[s], "_with_flawless_dates"),
      estlabel = paste0("RI ", stringr::str_to_title(source[s])," with Only Clean Dates (%)"),
      vid = paste0(source[s],"_dates_clean"),
      measureid = "RI_QUAL_01")

  } #end of source s loop

  make_svyp_output_database(
    variable = "had_card_or_register",
    estlabel = "RI Card or Register Availability (%)",
    vid = "card_or_register",
    measureid = "RI_QUAL_01")

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
