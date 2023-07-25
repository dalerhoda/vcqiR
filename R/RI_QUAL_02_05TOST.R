#' Export datasets to Excel for RI_QUAL_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER

# RI_QUAL_02_05TOST R version 1.00 - Biostat Global Consulting - 2022-12-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-21  1.00      Mia Yu          Original R package version
# *******************************************************************************

RI_QUAL_02_05TOST <- function(VCP = "RI_QUAL_02_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_QUAL_02", "TO_RI_QUAL_02_columnlabel",
              "TO_RI_QUAL_02_formatnum", "TO_RI_QUAL_02_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = "Ever Received RI Card (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
  make_table_column(
    tablename = "TO_RI_QUAL_02",
    dbfilename = paste0("RI_QUAL_02_",ANALYSIS_COUNTER,"_1_database.rds"),
    variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

  # Now export to excel
  export_table_to_excel(indicator = "RI_QUAL_02",brief = FALSE)
  rm(list = c("TO_RI_QUAL_02", "TO_RI_QUAL_02_columnlabel",
              "TO_RI_QUAL_02_formatnum", "TO_RI_QUAL_02_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_QUAL_02_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
