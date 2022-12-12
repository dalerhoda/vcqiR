#' Export datasets to Excel for RI_COVG_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_COVG_01_05TOST R version 1.01 - Biostat Global Consulting - 2022-10-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-04  1.00      Mia Yu          Original R version
# 2022-10-08  1.01      Mia Yu          Package version
# *******************************************************************************


RI_COVG_01_05TOST <- function(VCP = "RI_COVG_01_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_COVG_01", "TO_RI_COVG_01_BRIEF", "TO_RI_COVG_01_columnlabel", "TO_RI_COVG_01_BRIEF_columnlabel",
              "TO_RI_COVG_01_formatnum","TO_RI_COVG_01_BRIEF_formatnum",
              "TO_RI_COVG_01_colformat", "TO_RI_COVG_01_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  for (d in seq_along(RI_DOSE_LIST)){
    print(RI_DOSE_LIST[d])
    du <- str_to_upper(RI_DOSE_LIST[d])

    # by card
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_c_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(du," crude coverage, by card (%)"))
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_c_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

    # by history
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_h_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(du," crude coverage, by recall (%)"))
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_h_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

    # by scar (for BCG only)
    if (RI_DOSE_LIST[d] == "bcg"){
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_bcg_s_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = "BCG crude coverage, by scar (%)")
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_bcg_s_database.rds"),
        variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)  }

    # Add the words 'or scar' to the ch and chr labels if the dose is BCG
    if (RI_DOSE_LIST[d] == "bcg"){
      ch_label <- "by card or history or scar"
      chr_label <- "by card or history or register or scar"
    } else{
      ch_label <- "by card or history"
      chr_label <- "by card or history or register"
    }

    # by card or history (or scar if BCG)
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_ch_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(du," crude coverage, ", ch_label," (%)"))
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_ch_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

    if ((RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1) %in% TRUE){
      # by register
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_r_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," crude coverage, by register (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_r_database.rds"),
        variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

      # by card or register
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_cr_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," crude coverage, by card or register (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_cr_database.rds"),
        variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

      # by card or history or register (or scar if BCG)
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_chr_database.rds"),
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(du," crude coverage, ", chr_label," (%)"))
      make_table_column(
        tablename = "TO_RI_COVG_01",
        dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_chr_database.rds"),
        variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
    }

    # to analyze
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(du," crude coverage (%)"))
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "ci", noannotate = TRUE, label = NA)
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
    make_table_column(
      tablename = "TO_RI_COVG_01",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

    #TO_RI_COVG_01_BRIEF
    make_table_column(
      tablename = "TO_RI_COVG_01_BRIEF",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(du," crude coverage (%)"))
    make_table_column(
      tablename = "TO_RI_COVG_01_BRIEF",
      dbfilename = paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

    dbsave <- paste0("RI_COVG_01_",ANALYSIS_COUNTER,"_",RI_DOSE_LIST[d],"_a_database.rds")

  } #end of dose loop

  # Add N values to the far right side of the _BRIEF table
  make_table_column(
    tablename = "TO_RI_COVG_01_BRIEF",
    dbfilename = dbsave,
    variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
  make_table_column(
    tablename = "TO_RI_COVG_01_BRIEF",
    dbfilename = dbsave,
    variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

  export_table_to_excel(indicator = "RI_COVG_01")
  rm(list = c("TO_RI_COVG_01", "TO_RI_COVG_01_BRIEF", "TO_RI_COVG_01_columnlabel", "TO_RI_COVG_01_BRIEF_columnlabel",
              "TO_RI_COVG_01_formatnum","TO_RI_COVG_01_BRIEF_formatnum",
              "TO_RI_COVG_01_colformat", "TO_RI_COVG_01_BRIEF_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_COVG_01_CN","TO_RI_COVG_01_BRIEF_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
