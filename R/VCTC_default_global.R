#' Assign default values for global variables used in RI_VCTC_01
#'
#' @return Values in the global environment
#'
#' @export
#'
#' @examples
#' VCTC_default_global()

# VCTC_default_global R version 1.01 - Biostat Global Consulting - 2022-11-14
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-08  1.00      Mia Yu          Original R version
# 2022-11-14  1.01      Mia Yu          Package version
# *******************************************************************************

# Based on default globals_for_timeliness_plots in Stata VCQI

VCTC_default_global <- function(){
  # DT means 'default tiles'
  # CD means 'customized doses'

  # NOTE: this function contains only default settings

  # Establish parameters for default tiles

  assign("TIMELY_N_DTS", 5, envir = .GlobalEnv)  # 5 tiles per dose

  assign("TIMELY_DT_UB_1", 0, envir = .GlobalEnv)     # given before the target age - so given early
  assign("TIMELY_DT_UB_2", 28, envir = .GlobalEnv)    # given < the target age plus 28 days - so timely (within 28 days)
  assign("TIMELY_DT_UB_3", 56, envir = .GlobalEnv)    # given < the target age plus 56 days - so < 2 months late
  assign("TIMELY_DT_UB_4", 100000, envir = .GlobalEnv)  # given >=  the target age plus 56 days - so 2+ months late
  #note that the fifth tile does not have an upper bound; it represents children whose timing is unknown

  assign("TIMELY_DT_COLOR_1", "#993399", envir = .GlobalEnv)
  assign("TIMELY_DT_COLOR_2", "#339933", envir = .GlobalEnv)
  assign("TIMELY_DT_COLOR_3", "#FF99FF", envir = .GlobalEnv)
  assign("TIMELY_DT_COLOR_4", "#FF5CFF", envir = .GlobalEnv)
  assign("TIMELY_DT_COLOR_5", "#FFEBFF", envir = .GlobalEnv)

  assign("TIMELY_DT_LCOLOR_1", "lightgrey", envir = .GlobalEnv)
  assign("TIMELY_DT_LCOLOR_2", "lightgrey", envir = .GlobalEnv)
  assign("TIMELY_DT_LCOLOR_3", "lightgrey", envir = .GlobalEnv)
  assign("TIMELY_DT_LCOLOR_4", "lightgrey", envir = .GlobalEnv)
  assign("TIMELY_DT_LCOLOR_5", "lightgrey", envir = .GlobalEnv)

  assign("TIMELY_DT_LABEL_1", "Too Early", envir = .GlobalEnv)
  assign("TIMELY_DT_LABEL_2", "Timely (28 days)", envir = .GlobalEnv)
  assign("TIMELY_DT_LABEL_3", "< 2 Months Late", envir = .GlobalEnv)
  assign("TIMELY_DT_LABEL_4", "2+ Months Late", envir = .GlobalEnv)
  assign("TIMELY_DT_LABEL_5", "Timing Unknown", envir = .GlobalEnv)

  assign("TIMELY_DT_LEGEND_LABEL_1", "Too Early", envir = .GlobalEnv)
  assign("TIMELY_DT_LEGEND_LABEL_2", "Timely (28 Days)", envir = .GlobalEnv)
  assign("TIMELY_DT_LEGEND_LABEL_3", "< 2 Months Late", envir = .GlobalEnv)
  assign("TIMELY_DT_LEGEND_LABEL_4", "2+ Months Late", envir = .GlobalEnv)
  assign("TIMELY_DT_LEGEND_LABEL_5", "Timing Unknown", envir = .GlobalEnv)

  # *********************************************

  assign("TIMELY_XLABEL_SIZE", 10, envir = .GlobalEnv)
  assign("TIMELY_XLABEL_COLOR", "black", envir = .GlobalEnv)

  assign("TIMELY_YLABEL_SIZE", 10, envir = .GlobalEnv)
  assign("TIMELY_YLABEL_COLOR", "black", envir = .GlobalEnv)

  assign("TIMELY_BARWIDTH", 0.67, envir = .GlobalEnv)

  assign("TIMELY_CI_LCOLOR", "grey8", envir = .GlobalEnv)
  #This controls the width of the error bar; recommended range is 3-6
  assign("TIMELY_CI_LWIDTH", 4, envir = .GlobalEnv)

  #NOTE: the following VCQI parts are not implemented yet in R
  assign("TIMELY_HBR_LINE_PLOT", 0, envir = .GlobalEnv)
  assign("TIMELY_FULLY_VXD_LINE_PLOT", 0, envir = .GlobalEnv)
  assign("TIMELY_FULLY_VXD_NOTE", 0, envir = .GlobalEnv)
  assign("TIMELY_NOT_VXD_LINE_PLOT", 0, envir = .GlobalEnv)
  assign("TIMELY_NOT_VXD_NOTE", 0, envir = .GlobalEnv)

  #The dimensions of the plot being saved, unit is inch
  assign("TIMELY_PLOT_WIDTH",15, envir = .GlobalEnv)
  assign("TIMELY_PLOT_HEIGHT",8, envir = .GlobalEnv)

  #The order of the legend
  #With default setting only:
  assign("TIMELY_LEGEND_ORDER", c("DT_1","DT_2","DT_3","DT_4","DT_5"), envir = .GlobalEnv)

  # *********************************************
  #
  # Text bar options

  # ORDER is from left to right
  # If you do not wish to annotate the plot with a text bar,
  # simply make the TIMELY_TEXTBAR_ORDER empty.

  assign("TIMELY_TEXTBAR_ORDER", c("COVG", "N", "NEFF", "DEFF", "ICC"), envir = .GlobalEnv)

  assign("TIMELY_TEXTBAR_X_COVG", 104, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_N",    110, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_NEFF", 116, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_DEFF", 122, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_ICC",  128, envir = .GlobalEnv)

  assign("TIMELY_TEXTBAR_LABEL_COVG", "Coverage(%)", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_N",    "N", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_NEFF", "NEFF", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_DEFF", "DEFF", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_ICC",  "ICC", envir = .GlobalEnv)


  assign("TIMELY_XSCALE_MAX", 134, envir = .GlobalEnv)

  assign("TIMELY_TEXTBAR_COLOR_COVG", "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_N",    "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_NEFF", "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_DEFF", "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_ICC",  "black", envir = .GlobalEnv)

  # Number of digits after the decimal in coverage
  assign("TIMELY_TEXTBAR_COVG_DEC_DIGITS", 1, envir = .GlobalEnv)

  # Number of digits after the decimal in ICC
  assign("TIMELY_TEXTBAR_ICC_DEC_DIGITS", 3, envir = .GlobalEnv)

  # The footnote of the plot
  assign("TIMELY_PLOT_NOTE",
         "Abbreviations: NEFF: Effective sample size  DEFF: Design effect  ICC: Intracluster correlation coefficient",
         envir = .GlobalEnv)
}
