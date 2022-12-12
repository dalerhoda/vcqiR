#' Export datasets to Excel for RI_COVG_02
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_COVG_02_05TOST R version 1.01 - Biostat Global Consulting - 2022-10-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-06  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# *******************************************************************************

RI_COVG_02_05TOST <- function(VCP = "RI_COVG_02_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_COVG_02", "TO_RI_COVG_02_BRIEF", "TO_RI_COVG_02_columnlabel", "TO_RI_COVG_02_BRIEF_columnlabel",
              "TO_RI_COVG_02_formatnum","TO_RI_COVG_02_BRIEF_formatnum",
              "TO_RI_COVG_02_colformat", "TO_RI_COVG_02_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  for (d in seq_along(RI_DOSE_LIST)){
    print(RI_DOSE_LIST[d])
    du <- str_to_upper(RI_DOSE_LIST[d])

    # output for valid coverage
    # by card
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_c_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(du," valid coverage, by card (%)"))
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_c_database.rds"),
      variable = "ci", noannotate = TRUE, label = NA)

    if ((RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1) %in% TRUE){
      # by register
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_r_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," valid coverage, by register (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_r_database.rds"),
        variable = "ci", noannotate = TRUE, label = NA)

      # by card or register
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_cr_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," valid coverage, by card or register (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_cr_database.rds"),
        variable = "ci", noannotate = TRUE, label = NA)

    }

    # to analyze
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(du," valid coverage (%)"))
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "ci", noannotate = TRUE, label = NA)
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
    make_table_column(
      tablename = "TO_RI_COVG_02",
      dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

    # Valid coverage by age 1
    min_age <- get(paste0(RI_DOSE_LIST[d],"_min_age_days"), envir = .GlobalEnv)

    if (min_age < 365){
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_ca1_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," valid coverage by age 1, by card (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_ca1_database.rds"),
        variable = "ci", noannotate = TRUE, label = NA)

      if ((RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1) %in% TRUE){
        make_table_column(
          tablename = "TO_RI_COVG_02",
          dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_ra1_database.rds"),
          variable = "estimate", replacevar = NA, noannotate = TRUE,
          label = paste0(du," valid coverage by age 1, by register (%)"))
        make_table_column(
          tablename = "TO_RI_COVG_02",
          dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_ra1_database.rds"),
          variable = "ci", noannotate = TRUE, label = NA)

        make_table_column(
          tablename = "TO_RI_COVG_02",
          dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_cra1_database.rds"),
          variable = "estimate", replacevar = NA, noannotate = TRUE,
          label = paste0(du," valid coverage by age 1, by card or register (%)"))
        make_table_column(
          tablename = "TO_RI_COVG_02",
          dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_cra1_database.rds"),
          variable = "ci", noannotate = TRUE, label = NA)
      }

      # to analyze
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," valid coverage by age 1 (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "ci", noannotate = TRUE, label = NA)
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
      make_table_column(
        tablename = "TO_RI_COVG_02",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

      make_table_column(
        tablename = "TO_RI_COVG_02_BRIEF",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," valid coverage by age 1 (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_02_BRIEF",
        dbfilename = paste0("RI_COVG_02_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_aa1_database.rds"),
        variable = "ci", noannotate = TRUE, label = NA)

    } #end of check minage < 365

  } #end of dose loop

  export_table_to_excel(indicator = "RI_COVG_02")
  rm(list = c("TO_RI_COVG_02", "TO_RI_COVG_02_BRIEF", "TO_RI_COVG_02_columnlabel", "TO_RI_COVG_02_BRIEF_columnlabel",
              "TO_RI_COVG_02_formatnum","TO_RI_COVG_02_BRIEF_formatnum",
              "TO_RI_COVG_02_colformat", "TO_RI_COVG_02_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_COVG_02_CN","TO_RI_COVG_02_BRIEF_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
