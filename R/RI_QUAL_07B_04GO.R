#' Generate output databases for RI_QUAL_07B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_07B_04GO R version 1.01 - Biostat Global Consulting - 2022-10-11
# ******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-28  1.00      Caitlin Clary   Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# ******************************************************************************

RI_QUAL_07B_04GO <- function(VCP = "RI_QUAL_07B_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  for(d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    print(MOV_OUTPUT_DOSE_LIST[d])
    make_svyp_output_database(
      variable = paste0("got_hypo_", MOV_OUTPUT_DOSE_LIST[d]),
      estlabel = paste0("Would have valid ", stringr::str_to_upper(MOV_OUTPUT_DOSE_LIST[d]),
                        " if no MOVs (%)"),
      vid = MOV_OUTPUT_DOSE_LIST[d],
      measureid = "RI_QUAL_07B")
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}

