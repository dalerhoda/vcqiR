#' Make plots for RI_QUAL_07B
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return Plots in VCQI_OUTPUT_FOLDER
#'
#' @import stringr

# RI_QUAL_07B_06PO R version 1.01 - Biostat Global Consulting - 2022-10-11
# ******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-29  1.00      Caitlin Clary   Original R version
# 2022-10-11  1.01      Mia Yu          Fix problems
# 2022-10-11  1.01      Mia Yu          Package version
# ******************************************************************************

RI_QUAL_07B_06PO <- function(VCP = "RI_QUAL_07B_06PO"){
  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  # Inchworm or barchart plots

  if (VCQI_MAKE_IW_PLOTS == 1){

    print(paste0(IWPLOT_TYPE,"s (2 plots per dose)")) # TO DO update # plots per dose

    newpath <- paste0(VCQI_OUTPUT_FOLDER, "/Plots_IW_UW")
    dir.create(newpath, showWarnings = FALSE)

    if (IWPLOT_TYPE == "Bar chart"){
      plottype <- "brplot"
    } else {
      plottype <- "iwplot"
    }


    for (d in seq_along(MOV_OUTPUT_DOSE_LIST)){
      dn <- MOV_OUTPUT_DOSE_LIST[d]
      print(dn)

      # Single plot

      filestub <- paste0("RI_QUAL_07B_", ANALYSIS_COUNTER, "_", plottype, "_", dn)

      if (VCQI_SAVE_IW_PLOT_DATA == 1){
        savedata <- paste0(newpath, "/", filestub)
      } else{
        savedata <- NA
      }

      vcqi_to_plot(
        database = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_", ANALYSIS_COUNTER,
                          "_", dn, "_database.rds"),
        filename = paste0(newpath, "/RI_QUAL_07B_", ANALYSIS_COUNTER, "_", plottype, "_", dn),
        datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_", ANALYSIS_COUNTER, ".rds"),
        title = paste0("RI - Would have Valid ", str_to_upper(dn), " if no MOVs (%)"),
        name = paste0("RI_QUAL_07B_", ANALYSIS_COUNTER, "_iwplot_", dn),
        savedata = savedata)

      vcqi_log_comment(VCP, 3, "Comment", paste0(IWPLOT_TYPE, " was created and exported."))

      # Double inchworm to show valid coverage with no MOVs versus observed valid coverage (with MOVs)

      # If user is doing what-if analysis and using a value of ANALYSIS_COUNTER
      # for which RI_COVG_02_<ANALYSIS_COUNTER>_<dose>_a_database does not exist,
      # try pointing to file RI_COVG_02_1_<dose_a_database and issue a warning

      # If RI_COVG_02_1_<dose_a_database does not exist, skip this plot

      double_ac <- NULL
      if(file.exists(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_", ANALYSIS_COUNTER, "_", dn, "_a_database.rds"))){
        double_ac <- ANALYSIS_COUNTER
      } else if(file.exists(paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_", 1, "_", dn, "_a_database.rds"))){
        double_ac <- 1
        vcqi_log_comment(
          VCP, 2, "Warning",
          paste0("RI_QUAL_07B made a double inchworm plot using ", "RI_COVG_02_", 1, "_", dn, "_a_database ", "because ", "RI_COVG_02_", ANALYSIS_COUNTER, "_", dn, "_a_database", "did not exist"))
      }

      if(!is.null(double_ac)){

        filestub <- paste0("RI_QUAL_07B_", ANALYSIS_COUNTER, "_", plottype, "_", dn, "_double")

        if (VCQI_SAVE_IW_PLOT_DATA == 1){
          savedata <- paste0(newpath, "/", filestub)
        } else{
          savedata <- NA
        }

        vcqi_to_double_plot(
          database = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_", ANALYSIS_COUNTER,
                            "_", dn, "_database.rds"),
          database2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_", double_ac,
                             "_", dn, "_a_database.rds"),
          filename = paste0(newpath, "/RI_QUAL_07B_", ANALYSIS_COUNTER, "_",
                            plottype, "_", dn, "_double"),
          datafile = paste0(VCQI_OUTPUT_FOLDER, "/RI_QUAL_07B_", ANALYSIS_COUNTER, ".rds"),
          datafile2 = paste0(VCQI_OUTPUT_FOLDER, "/RI_COVG_02_", double_ac, ".rds"),
          title = paste0("RI - Would have Valid ", str_to_upper(dn), " if no MOVs (%)"),
          name = paste0(paste0("RI_QUAL_07B_", ANALYSIS_COUNTER, "_iwplot_", dn, "_double")),
          note = "Gray shape is valid coverage; colored shape is valid coverage if no MOVs",
          savedata = savedata
        )
      }

    } #end of d loop

  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}

