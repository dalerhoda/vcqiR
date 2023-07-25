#' Export datasets to Excel for RI_COVG_04
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER

# RI_COVG_04_05TOST R version 1.00 - Biostat Global Consulting - 2022-12-13
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-13  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_COVG_04_05TOST <- function(VCP = "RI_COVG_04_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_COVG_04", "TO_RI_COVG_04_BRIEF", "TO_RI_COVG_04_columnlabel", "TO_RI_COVG_04_BRIEF_columnlabel",
              "TO_RI_COVG_04_formatnum","TO_RI_COVG_04_BRIEF_formatnum",
              "TO_RI_COVG_04_colformat", "TO_RI_COVG_04_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  #CRUDE
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = "Not vaccinated - crude")
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
  make_table_column(
    tablename = "TO_RI_COVG_04",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

  make_table_column(
    tablename = "TO_RI_COVG_04_BRIEF",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = "Not vaccinated - crude")
  make_table_column(
    tablename = "TO_RI_COVG_04_BRIEF",
    dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvc_database.rds"),
    variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

  if (VCQI_NO_DOBS != 1){

    # VALID
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "Not vaccinated - valid")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

    make_table_column(
      tablename = "TO_RI_COVG_04_BRIEF",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "Not vaccinated - valid")
    make_table_column(
      tablename = "TO_RI_COVG_04_BRIEF",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nvv_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)


    # valid age 1
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "No valid doses by age 1")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
    make_table_column(
      tablename = "TO_RI_COVG_04",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

    make_table_column(
      tablename = "TO_RI_COVG_04_BRIEF",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = "No valid doses by age 1")
    make_table_column(
      tablename = "TO_RI_COVG_04_BRIEF",
      dbfilename = paste0("RI_COVG_04_",ANALYSIS_COUNTER,"_nva1_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

  }

  export_table_to_excel(indicator = "RI_COVG_04")
  rm(list = c("TO_RI_COVG_04", "TO_RI_COVG_04_BRIEF", "TO_RI_COVG_04_columnlabel", "TO_RI_COVG_04_BRIEF_columnlabel",
              "TO_RI_COVG_04_formatnum","TO_RI_COVG_04_BRIEF_formatnum",
              "TO_RI_COVG_04_colformat", "TO_RI_COVG_04_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_COVG_04_CN","TO_RI_COVG_04_BRIEF_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
