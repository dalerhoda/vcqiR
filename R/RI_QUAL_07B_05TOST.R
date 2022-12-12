#' Export datasets to Excel for RI_QUAL_07B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_07B_05TOST R version 1.02 - Biostat Global Consulting - 2022-10-11
# ******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-29  1.00      Caitlin Clary   Original R version
# 2022-10-11  1.01      Mia Yu          Add parts that remove temprary global
# 2022-10-11  1.02      Mia YU          Pakcage version
# ******************************************************************************

RI_QUAL_07B_05TOST <- function(VCP = "RI_QUAL_07B_05TOST"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # Set some values
  indicator <- "RI_QUAL_07B"
  tb <- "TO_RI_QUAL_07B"
  ac <- ANALYSIS_COUNTER

  if (file.exists(paste0(VCQI_OUTPUT_FOLDER, "/", tb, ".rds"))){
    file.remove(paste0(VCQI_OUTPUT_FOLDER, "/", tb, ".rds"))
  }

  rm(list = c("TO_RI_QUAL_07B", "TO_RI_QUAL_07B_columnlabel",
               "TO_RI_QUAL_07B_formatnum","TO_RI_QUAL_07B_colformat"), envir = .GlobalEnv) %>% suppressWarnings()

  for(d in seq_along(MOV_OUTPUT_DOSE_LIST)){
    dn <- MOV_OUTPUT_DOSE_LIST[d]
    print(dn)

    make_table_column(
      tablename = tb,
      dbfilename = paste0("RI_COVG_02_", ac, "_", dn, "_a_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0("Had valid ", str_to_upper(dn), " (%)")
    )

    make_table_column(
      tablename = tb,
      dbfilename = paste0(indicator, "_", ac, "_", dn, "_database.rds"),
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0("Would have valid ", str_to_upper(dn), " if no MOVs (%)")
    )

    make_table_column(
      tablename = tb,
      dbfilename = paste0(indicator, "_", ac, "_", dn, "_database.rds"),
      variable = "ci", replacevar = NA, noannotate = TRUE, label = NA)

  }

  # Add N at the far right of the table
  make_table_column(
    tablename = tb,
    dbfilename = paste0(indicator, "_", ac, "_", MOV_OUTPUT_DOSE_LIST[1], "_database.rds"),
    variable = "n", replacevar = NA, noannotate = TRUE, label = "N")
  make_table_column(
    tablename = tb,
    dbfilename = paste0(indicator, "_", ac, "_", MOV_OUTPUT_DOSE_LIST[1], "_database.rds"),
    variable = "nwtd", replacevar = NA, noannotate = TRUE, label = "Weighted N")

  export_table_to_excel(indicator = indicator, brief = FALSE)

  rm(list = c("TO_RI_QUAL_07B", "TO_RI_QUAL_07B_columnlabel",
               "TO_RI_QUAL_07B_formatnum","TO_RI_QUAL_07B_colformat"), envir = .GlobalEnv) %>% suppressWarnings()
  rm(TO_RI_QUAL_07B_CN, envir = .GlobalEnv) %>% suppressWarnings()

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

