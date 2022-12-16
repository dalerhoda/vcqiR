# globals_for_timeliness_plots R version 1.00 - Biostat Global Consulting - 2022-11-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-11-08  1.00      Mia Yu          Original R version
# *******************************************************************************


# DT means 'default tiles'
# CD means 'customized doses'

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
#
# Specify parameters for customized doses
# Note that some of these parameters over-ride the defaults set above
#
#
# Specify customized tile and legend definitions, but wrap them in a
# comment block so they will not be used by default.
#
# These serve as examples.  Users may update the custom definitions either
# in this .do file or in their control program.

# assign("TIMELY_CD_LIST", c("bcg","hepb"), envir = .GlobalEnv)  # customized definitions for BCG & HEPB
#
# assign("TIMELY_CD_BCG_NTILES", 5, envir = .GlobalEnv)   # BCG still has 5 tiles
#
# assign("TIMELY_CD_BCG_UB_1", 5, envir = .GlobalEnv)     # First tile is for given <= target age (0 days) plus 5 days
# assign("TIMELY_CD_BCG_UB_2", 56, envir = .GlobalEnv)    # Second is for given < 2 months late
# assign("TIMELY_CD_BCG_UB_3", 365, envir = .GlobalEnv)   # Third is for given 2+ months late but within a year
# assign("TIMELY_CD_BCG_UB_4", 100000, envir = .GlobalEnv)  # Fourth is for doses given after age 1 year
#
# assign("TIMELY_CD_BCG_LCOLOR_1", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_2", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_3", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_4", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_5", "lightgrey", envir = .GlobalEnv)
#
# assign("TIMELY_CD_BCG_COLOR_1", "#336633", envir = .GlobalEnv)    #use a dark green for this special BCG timely category
# assign("TIMELY_CD_BCG_COLOR_2", "#FF99FF", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_COLOR_3", "#FF5CFF", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_COLOR_4", "black", envir = .GlobalEnv)      # very late BCG shows in a BLACK bar
# assign("TIMELY_CD_BCG_COLOR_5", "#FFEBFF", envir = .GlobalEnv) # standard color
#
# assign("TIMELY_CD_BCG_LCOLOR_1", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_2", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_3", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_4", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LCOLOR_5", "lightgrey", envir = .GlobalEnv)
#
# assign("TIMELY_CD_BCG_LABEL_1", "BCG by day 5", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LABEL_2", "< 2 Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LABEL_3", "2+ Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LABEL_4", "After 1 Year (BCG only)", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LABEL_5", "Timing Unknown", envir = .GlobalEnv)
#
# assign("TIMELY_CD_BCG_LEGEND_LABEL_1", "BCG by Day 5", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LEGEND_LABEL_2", "< 2 Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LEGEND_LABEL_3", "2+ Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LEGEND_LABEL_4", "BCG After 1 Year", envir = .GlobalEnv)
# assign("TIMELY_CD_BCG_LEGEND_LABEL_5", "Timing Unknown", envir = .GlobalEnv)

# *********************************************

# assign("TIMELY_CD_HEPB_NTILES", 4, envir = .GlobalEnv)
#
# assign("TIMELY_CD_HEPB_UB_1", 2, envir = .GlobalEnv)      # HEPB is timely if given on day 0 or 1
# assign("TIMELY_CD_HEPB_UB_2", 56, envir = .GlobalEnv)     # < 2 months late
# assign("TIMELY_CD_HEPB_UB_3", 1000, envir = .GlobalEnv)   # 2+  months late
#
# assign("TIMELY_CD_HEPB_COLOR_1", "#BCDDBC", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_COLOR_2", "#FF99FF", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_COLOR_3", "#FF5CFF", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_COLOR_4", "#FFEBFF", envir = .GlobalEnv) # standard color
#
# assign("TIMELY_CD_HEPB_LCOLOR_1", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LCOLOR_2", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LCOLOR_3", "lightgrey", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LCOLOR_4", "lightgrey", envir = .GlobalEnv)
#
# assign("TIMELY_CD_HEPB_LABEL_1", "Timely (within 1 day)", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LABEL_2", "< 2 Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LABEL_3", "2+ Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LABEL_4", "Timing Unknown", envir = .GlobalEnv)
#
# assign("TIMELY_CD_HEPB_LEGEND_LABEL_1", "HEPB by Day 1", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LEGEND_LABEL_2", "< 2 Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LEGEND_LABEL_3", "2+ Months Late", envir = .GlobalEnv)
# assign("TIMELY_CD_HEPB_LEGEND_LABEL_4", "Timing Unknown", envir = .GlobalEnv)

# *********************************************

assign("TIMELY_XLABEL_SIZE", 10, envir = .GlobalEnv)
assign("TIMELY_XLABEL_COLOR", "black", envir = .GlobalEnv)

assign("TIMELY_YLABEL_SIZE", 10, envir = .GlobalEnv)
assign("TIMELY_YLABEL_COLOR", "black", envir = .GlobalEnv)

assign("TIMELY_BARWIDTH", 0.67, envir = .GlobalEnv)

assign("TIMELY_CI_LCOLOR", "grey8", envir = .GlobalEnv)
#This controls the width of the error bar; recommended range is 3-6
assign("TIMELY_CI_LWIDTH", 4, envir = .GlobalEnv)

#NOTE: the following VCQI parts are now implemented yet in R
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

#With customized dose too:
#assign("TIMELY_LEGEND_ORDER", c("CD_BCG_1","CD_HEPB_1","DT_1","DT_2","DT_3","DT_4","DT_5","CD_BCG_4"), envir = .GlobalEnv)

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
