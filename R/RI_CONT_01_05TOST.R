#' Export datasets to Excel for RI_CONT_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_CONT_01_05TOST R version 1.00 - Biostat Global Consulting - 2023-07-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-07-24  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_CONT_01_05TOST <- function(VCP = "RI_CONT_01_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_CONT_01", "TO_RI_CONT_01_columnlabel",
              "TO_RI_CONT_01_formatnum", "TO_RI_CONT_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  j = 1
  while (j <= length(RI_CONT_01_DROPOUT_LIST)){

    d1 <- str_to_lower(RI_CONT_01_DROPOUT_LIST[j])
    j = j+1
    d2 <- str_to_lower(RI_CONT_01_DROPOUT_LIST[j])
    j = j+1

    print(paste0(d1, " to ", d2))

    make_table_column(
      tablename = "TO_RI_CONT_01",
      dbfilename = paste0("RI_CONT_01_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(str_to_upper(d1),"-",str_to_upper(d2)," Dropout (%)"))

    make_table_column(
      tablename = "TO_RI_CONT_01",
      dbfilename = paste0("RI_CONT_01_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE,
      label = "N")
  } #end of while

  export_table_to_excel(indicator = "RI_CONT_01",brief = FALSE)
  rm(list = c("TO_RI_CONT_01", "TO_RI_CONT_01_columnlabel",
              "TO_RI_CONT_01_formatnum", "TO_RI_CONT_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_CONT_01_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
