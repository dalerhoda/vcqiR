#' Generate output databases for RI_CONT_01B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Databases in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_CONT_01B_04GO R version 1.00 - Biostat Global Consulting - 2023-07-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-07-18  1.00      Mia Yu          Original R package version
#                                       Use make_svyp_output_database
# *******************************************************************************


RI_CONT_01B_04GO <- function(VCP = "RI_CONT_01B_04GO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  j = 1
  while (j <= length(RI_CONT_01B_DROPOUT_LIST)) {

    d1 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
    j = j+1
    d2 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
    j = j+1

    print(paste0(d1, " to ", d2))

    make_svyp_output_database(variable = paste0("wtd_dropout_",d1,"_",d2),
                              estlabel = paste0(str_to_upper(d1),"-",str_to_upper(d2)," Dropout (%)"),
                              vid = paste0(d1,"_",d2),
                              measureid = "RI_CONT_01B")

  } #end of while

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
