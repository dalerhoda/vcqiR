#' Export datasets to Excel for RI_COVG_03
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER

# RI_COVG_03_05TOST R version 1.00 - Biostat Global Consulting - 2022-12-13
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-13  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_COVG_03_05TOST <- function(VCP = "RI_COVG_03_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_COVG_03", "TO_RI_COVG_03_BRIEF", "TO_RI_COVG_03_columnlabel", "TO_RI_COVG_03_BRIEF_columnlabel",
              "TO_RI_COVG_03_formatnum","TO_RI_COVG_03_BRIEF_formatnum",
              "TO_RI_COVG_03_colformat", "TO_RI_COVG_03_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  #CRUDE
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = "Fully vaccinated - crude")
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
  make_table_column(
    tablename = "TO_RI_COVG_03",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

  make_table_column(
    tablename = "TO_RI_COVG_03_BRIEF",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = "Fully vaccinated - crude")
  make_table_column(
    tablename = "TO_RI_COVG_03_BRIEF",
    dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvc_database.rds"),
    variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

  if (VCQI_NO_DOBS != 1){

    # VALID
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "Fully vaccinated - valid")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

    make_table_column(
      tablename = "TO_RI_COVG_03_BRIEF",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "Fully vaccinated - valid")
    make_table_column(
      tablename = "TO_RI_COVG_03_BRIEF",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fvv_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)


    # valid age 1
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "Fully vaccinated with valid doses by age 1")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
    make_table_column(
      tablename = "TO_RI_COVG_03",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

    make_table_column(
      tablename = "TO_RI_COVG_03_BRIEF",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "Fully vaccinated with valid doses by age 1")
    make_table_column(
      tablename = "TO_RI_COVG_03_BRIEF",
      dbfilename = paste0("RI_COVG_03_",ANALYSIS_COUNTER,"_fva1_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

  }

  export_table_to_excel(indicator = "RI_COVG_03")
  rm(list = c("TO_RI_COVG_03", "TO_RI_COVG_03_BRIEF", "TO_RI_COVG_03_columnlabel", "TO_RI_COVG_03_BRIEF_columnlabel",
              "TO_RI_COVG_03_formatnum","TO_RI_COVG_03_BRIEF_formatnum",
              "TO_RI_COVG_03_colformat", "TO_RI_COVG_03_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_COVG_03_CN","TO_RI_COVG_03_BRIEF_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
