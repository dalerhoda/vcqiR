#' Export datasets to Excel for RI_QUAL_01
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_01_05TOST R version 1.01 - Biostat Global Consulting - 2024-05-15
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-12-20  1.00      Mia Yu          Original R package version
# 2024-05-15  1.01      Mia Yu          Added multi lingual strings
# *******************************************************************************

RI_QUAL_01_05TOST <- function(VCP = "RI_QUAL_01_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c("TO_RI_QUAL_01", "TO_RI_QUAL_01_columnlabel",
              "TO_RI_QUAL_01_formatnum", "TO_RI_QUAL_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  # Create local with document source type
  source <- "card"
  if (RI_RECORDS_SOUGHT_FOR_ALL == 1 | RI_RECORDS_SOUGHT_IF_NO_CARD == 1){
    source <- c(source, "register")
  }

  for (s in seq_along(source)){

    if (source[s] == "card"){
      sp <- str_to_title(language_string(language_use = language_use, str = "OS_414"))
    }

    if (source[s] == "register"){
      sp <- str_to_title(language_string(language_use = language_use, str = "OS_415"))
    }

    # Respondent had the source ?
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_416"),
                     " ",
                     sp,
                     " ",
                     language_string(language_use = language_use, str = "OS_417"),
                     " ",
                     language_string(language_use = language_use, str = "OS_1")))
    #label = paste0("RI ",sp," Availability (%)"))
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

    # Had source with dates
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_dates_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_416"),
                     " ",
                     sp,
                     " ",
                     language_string(language_use = language_use, str = "OS_418"),
                     " ",
                     language_string(language_use = language_use, str = "OS_1")))
    #label = paste0("RI ",sp," with Dates (%)"))
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_dates_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

    # Had source with dates or ticks
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_dates_ticks_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_416"),
                     " ",
                     sp,
                     " ",
                     language_string(language_use = language_use, str = "OS_419"),
                     " ",
                     language_string(language_use = language_use, str = "OS_1")))
    #label = paste0("RI ",sp," with Dates or Ticks (%)"))
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_dates_ticks_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

    # Had source with only clean dates
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_dates_clean_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(language_string(language_use = language_use, str = "OS_416"),
                     " ",
                     sp,
                     " ",
                     language_string(language_use = language_use, str = "OS_420"),
                     " ",
                     language_string(language_use = language_use, str = "OS_1")))
    #label = paste0("RI ",sp," with Only Clean Dates (%)"))
    make_table_column(
      tablename = "TO_RI_QUAL_01",
      dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_",source[s],"_dates_clean_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

  } #end of source s loop

  # Card or register availability
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "estimate", replacevar = NA, noannotate = TRUE,
    label = paste0(language_string(language_use = language_use, str = "OS_416"),
                   " ",
                   language_string(language_use = language_use, str = "OS_421"),
                   " ",
                   language_string(language_use = language_use, str = "OS_1"))) #label = "RI Card or Register Availability (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "stderr", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_318")) #"StdErr (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "lcb", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_319")) #"95% LCB (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "ucb", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_320")) #"95% UCB (%)")
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "deff", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_321")) #"DEFF")
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "icc", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_322")) #"ICC")
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_48")) #"N")
  make_table_column(
    tablename = "TO_RI_QUAL_01",
    dbfilename = paste0("RI_QUAL_01_",ANALYSIS_COUNTER,"_card_or_register_database.rds"),
    variable = "nwtd", replacevar = NA, noannotate = TRUE,
    label = language_string(language_use = language_use, str = "OS_323")) #"Weighted N")

  # Now export to excel
  export_table_to_excel(indicator = "RI_QUAL_01",brief = FALSE)
  rm(list = c("TO_RI_QUAL_01", "TO_RI_QUAL_01_columnlabel",
              "TO_RI_QUAL_01_formatnum", "TO_RI_QUAL_01_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  rm(list = c("TO_RI_QUAL_01_CN"), envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
