#' Assign default values for global variables used in RI_VCTC_01
#'
#' @return Values in the global environment
#'
#' @import stringr
#' @export
#'
#' @examples
#' VCTC_global_same_legend_for_all()

# VCTC_global_same_legend_for_all R version 1.03 - Biostat Global Consulting - 2023-07-18
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-08  1.00      Mia Yu          Original R version
# 2022-11-14  1.01      Mia Yu          Package version
# 2023-01-08  1.02      Mia Yu          Added new globals
# 2023-07-18  1.03      Mia Yu          Added CI to default, change the way the note
#                                       is generated to match Stata
# *******************************************************************************

# Based on globals_for_timeliness_plots - same_legend_for_all_doses in Stata VCQI

VCTC_global_same_legend_for_all <- function(){
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
  assign("TIMELY_DT_COLOR_5", "#e0e0e0", envir = .GlobalEnv)

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

  # Because we are spacing the bars about every y=10 units instead of the default
  # y=1 units apart, specify a bar width that is 10X the default.
  vcqi_global(TIMELY_BARWIDTH, 6.7)

  assign("TIMELY_CI_LCOLOR", "grey8", envir = .GlobalEnv)
  #This controls the width of the error bar; recommended range is 3-6
  assign("TIMELY_CI_LWIDTH", 4, envir = .GlobalEnv)

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

  assign("TIMELY_TEXTBAR_ORDER", c("COVG", "CI", "N", "NEFF", "DEFF", "ICC"), envir = .GlobalEnv)

  assign("TIMELY_TEXTBAR_X_COVG", 108, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_CI",   122, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_N",    135, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_NEFF", 145, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_DEFF", 155, envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_X_ICC",  165, envir = .GlobalEnv)

  assign("TIMELY_TEXTBAR_LABEL_COVG", "Coverage(%)", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_CI",   "95% CI", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_N",    "N", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_NEFF", "NEFF", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_DEFF", "DEFF", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_LABEL_ICC",  "ICC", envir = .GlobalEnv)


  assign("TIMELY_XSCALE_MAX", 170, envir = .GlobalEnv)

  assign("TIMELY_TEXTBAR_COLOR_COVG", "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_N",    "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_NEFF", "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_DEFF", "black", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_COLOR_ICC",  "black", envir = .GlobalEnv)

  # Number of digits after the decimal in coverage
  assign("TIMELY_TEXTBAR_COVG_DEC_DIGITS", 1, envir = .GlobalEnv)

  # Number of digits after the decimal in ICC
  assign("TIMELY_TEXTBAR_ICC_DEC_DIGITS", 3, envir = .GlobalEnv)

  # Define a set of abbreviations that the code can use to build up a footnote
  #
  # It will only include the abbreviations for the textbar elements that the user requests
  #
  # Update here
  assign("TIMELY_TEXTBAR_ABBREV_CI", "CI- Confidence interval", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_ABBREV_ICC", "ICC- Intracluster correlation coefficient", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_ABBREV_NEFF", "NEFF- Effective sample size", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_ABBREV_N", "N- Sample size", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_ABBREV_NHBR", "NHBR- N with HBR", envir = .GlobalEnv)
  assign("TIMELY_TEXTBAR_ABBREV_DEFF", "DEFF- Design effect", envir = .GlobalEnv)

  # *********************************************
  # Specify whether to indicate the % who showed a HBR (or register)
  # If yes, set this to 1, otherwise 0
  assign("TIMELY_HBR_LINE_PLOT", 1, envir = .GlobalEnv)

  # If yes, then which variable from RI_QUAL_01 do you want to use?
  # The default is had_card.  There are several other options, especially
  # if the survey visited health centers to look at register records.
  assign("TIMELY_HBR_LINE_VARIABLE", "had_card", envir = .GlobalEnv)

  #  HBR line properties
  assign("TIMELY_HBR_LINE_WIDTH", 1, envir = .GlobalEnv)
  assign("TIMELY_HBR_LINE_COLOR", "grey8", envir = .GlobalEnv)
  assign("TIMELY_HBR_LINE_PATTERN", "dashed", envir = .GlobalEnv)
  assign("TIMELY_HBR_LINE_LABEL", "<-- Showed HBR", envir = .GlobalEnv)
  #NOTE: this global did not show up in the Stata .do file yet
  assign("TIMELY_HBR_LINE_LABEL_COLOR", "grey2", envir = .GlobalEnv)
  assign("TIMELY_ABBREV_CAPTION_LINE1", "Abbreviations: HBR- home-based record", envir = .GlobalEnv)

  if (RI_RECORDS_NOT_SOUGHT == 0){
    assign("TIMELY_HBR_LINE_VARIABLE", "had_card_or_register", envir = .GlobalEnv)
    assign("TIMELY_HBR_LINE_LABEL", "<-- Showed HBR or FBR", envir = .GlobalEnv)
    assign("TIMELY_ABBREV_CAPTION_LINE1", "Abbreviations: HBR- home-based record; FBR- facility-based record", envir = .GlobalEnv)
  }

  # *********************************************
  # Summarize the % fully vxd and not vxd in a VCTC footnote

  assign("TIMELY_FULLY_VXD_NOTE", 1, envir = .GlobalEnv)
  assign("TIMELY_FULLY_VXD_NOTE_SUPPRESS_CI", 0, envir = .GlobalEnv)
  assign("TIMELY_FULLY_VXD_NOTE_VARIABLE", "fully_vaccinated_crude", envir = .GlobalEnv)
  fully_list <- get(paste0("RI_DOSES_TBFV_AC_",ANALYSIS_COUNTER), envir = .GlobalEnv)
  assign("TIMELY_FULLY_VXD_DOSELIST_TEXT",
         paste0("Fully vaccinated dose list: ", str_flatten(fully_list, collapse = " ")), envir = .GlobalEnv)

  assign("TIMELY_NOT_VXD_NOTE", 1, envir = .GlobalEnv)
  assign("TIMELY_NOT_VXD_NOTE_SUPPRESS_CI", 0, envir = .GlobalEnv)
  assign("TIMELY_NOT_VXD_NOTE_VARIABLE", "not_vaccinated_crude", envir = .GlobalEnv)
  assign("TIMELY_NOT_VXD_DOSELIST_TEXT",
         "Not vaccinated means the child did not receive any of the doses from the fully vaccinated dose list.", envir = .GlobalEnv)

}
