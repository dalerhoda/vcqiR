#' Export datasets to Excel for RI_CONT_01B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_CONT_01B_05TOST R version 1.00 - Biostat Global Consulting - 2023-07-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-07-24  1.00      Mia Yu          Original R package version
# *******************************************************************************


RI_CONT_01B_05TOST <- function(VCP = "RI_CONT_01B_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_CONT_01B", "TO_RI_CONT_01B_columnlabel",
              "TO_RI_CONT_01B_formatnum", "TO_RI_CONT_01B_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  j = 1
  while (j <= length(RI_CONT_01B_DROPOUT_LIST)){

    d1 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
    j = j+1
    d2 <- str_to_lower(RI_CONT_01B_DROPOUT_LIST[j])
    j = j+1

    print(paste0(d1, " to ", d2))

    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(d1,"-",d2," Dropout (%)"))
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "ci", noannotate = TRUE, label = NA)
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "stderr", replacevar = NA, noannotate = TRUE, label = "StdErr (%)")
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "lcb", replacevar = NA, noannotate = TRUE, label = "95% LCB (%)")
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "ucb", replacevar = NA, noannotate = TRUE, label = "95% UCB (%)")
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "deff", replacevar = NA, noannotate = TRUE, label = "DEFF")
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "icc", replacevar = NA, noannotate = TRUE, label = "ICC")
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
    make_table_column(
      tablename = "TO_RI_CONT_01B",
      dbfilename = paste0("RI_CONT_01B_",ANALYSIS_COUNTER,"_",d1,"_",d2,"_database.rds"),
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")
  } #end of while

  export_table_to_excel(indicator = "RI_CONT_01B",brief = FALSE)
  rm(list = c("TO_RI_CONT_01B", "TO_RI_CONT_01B_columnlabel",
              "TO_RI_CONT_01B_formatnum", "TO_RI_CONT_01B_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_CONT_01B_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")

}
