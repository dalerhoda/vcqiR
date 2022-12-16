# User's Guide RI Control Program R version 1.00 - Biostat Global Consulting - 2022-12-01
#
# Vaccination Coverage Quality Indicators (VCQI) control program to analyze data
# from a routine immunization survey
#
# Change log
#
# Date          Version Number    Name          What Changed
# 2022-12-01    1.00              BGC           Original R Version
#
# This program is configured to analyze the VCQI District 20 demo datasets. It
# serves as a template that users may copy to use with new datasets from real
# surveys.
#
# After copying the program, make a set of edits in Blocks RI-B and RI-D and
# RI-F below in accordance with guidance in the VCQI User's Guide.
#
# You will find the latest versions of VCQI documentation and information about
# VCQI programs at the VCQI Resources Website:
# http://www.biostatglobal.com/VCQI_RESOURCES.html
#
# Written by Biostat Global Consulting

# *************************************************
# Code Block: RI-A             (Do not change) ----
#
# Load the VCQI package
library(vcqiR, attach.required = TRUE)

# Start with clear memory
cleanup_VCQI_globals()

# *************************************************
# Code Block: RI-B           (User may change) ----

# Specify input/output folders and analysis name

# Where should the programs look for datasets?
VCQI_DATA_FOLDER <- "your path"

# Where should the programs put output?
VCQI_OUTPUT_FOLDER <- "your path"

# Establish analysis name (used in log file name and Excel file name)
VCQI_ANALYSIS_NAME <- "District_20_RI"

# Set VCQI_CHECK_INSTEAD_OF_RUN value to 1 to test all metadata and code that
# makes datasets and calculates derived variables, without running the
# indicators or generating output
# Note: checks are not fully implemented and tested in the R version of VCQI
VCQI_CHECK_INSTEAD_OF_RUN <- 0

# *************************************************
# Code Block: RI-C             (Do not change) ----
#
# ** CD to output folder and open VCQI log

setwd(VCQI_OUTPUT_FOLDER)

# Start with a clean, empty Excel file for tabulated output (TO)
unlink(paste0(VCQI_OUTPUT_FOLDER, "/", VCQI_ANALYSIS_NAME, "_TO.xlsx"), force = TRUE)

# Give the current program a name, for logging purposes
VCP <- "RI_Control_Program"

# Open the VCQI log and put a comment in it
vcqi_log_comment(VCP, 3, "Comment", "Run begins...log opened...")

# Document the global macros that were defined before the log opened
vcqi_log_global(VCQI_DATA_FOLDER)
vcqi_log_global(VCQI_OUTPUT_FOLDER)
vcqi_log_global(VCQI_ANALYSIS_NAME)

# Write an entry in the log file documenting the vcqiR package version
vcqi_log_comment(VCP, 3, "Package",
                 paste0("vcqiR package version ", utils::packageVersion("vcqiR")))

# *************************************************
# Code Block: RI-D           (User may change) ----
#
# Specify dataset names and important metadata
# Dataset names should include file extensions
# Accepted file types: .rds, .dta, .csv

# Name of datasets that hold RI data
vcqi_global(VCQI_RI_DATASET, "VCQI_RI_20.dta")
vcqi_global(VCQI_RIHC_DATASET, NA)

# Name of dataset that holds cluster metadata
vcqi_global(VCQI_CM_DATASET, "VCQI_CM_20.dta")

# If you will describe the dataset using DESC_01 then you need to also specify
# the HH and HM datasets
vcqi_global(VCQI_HH_DATASET, NA)
vcqi_global(VCQI_HM_DATASET, NA)

# .........................................................................
# Parameters to describe RI schedule
# .........................................................................
# These parameters may change from survey to survey

#  See:
#  http://www.who.int/immunization/policy/Immunization_routine_table2.pdf?ua=1
#  http://apps.who.int/immunization_monitoring/globalsummary/schedules
#
#  Single-dose antigens will use a parameter named <dose>_min_age_days (required)
#  Single-dose antigens may  use a parameter named <dose>_max_age_days (optional)
#  Note: If a dose is not considered valid AFTER a certain age, then specify
#        that maximum valid age using the _max_age_days parameter.
#        If the dose is considered late, but still valid, then do not specify
#        a maximum age.

bcg_min_age_days          <- 0    # birth dose

penta1_min_age_days       <- 56   # 8 weeks
opv1_min_age_days         <- 56   # 8 weeks

penta2_min_age_days       <- 84   # 12 weeks
penta2_min_interval_days  <- 28   # 4 weeks
opv2_min_age_days         <- 84   # 12 weeks
opv2_min_interval_days    <- 28   # 4 weeks


penta3_min_age_days       <- 112  # 16 weeks
penta3_min_interval_days  <- 28   # 4 weeks
opv3_min_age_days         <- 112  # 16 weeks
opv3_min_interval_days    <- 28   # 4 weeks

mcv_min_age_days          <- 270  # 9 months

# .........................................................................
# Parameters to describe survey
# .........................................................................
# Specify the earliest and latest possible vaccination date for this survey.

# The software assumes this survey includes birth doses, so the earliest date is
# the first possible birthdate for RI survey respondents and the latest date is
# the last possible vaccination date for this dataset - the latest date might be
# the date of the final survey interview.

vcqi_global(EARLIEST_SVY_VACC_DATE_M, 1)
vcqi_global(EARLIEST_SVY_VACC_DATE_D, 1)
vcqi_global(EARLIEST_SVY_VACC_DATE_Y, 2014)

vcqi_global(LATEST_SVY_VACC_DATE_M, 1)
vcqi_global(LATEST_SVY_VACC_DATE_D, 1)
vcqi_global(LATEST_SVY_VACC_DATE_Y, 2017)

# These parameters indicate the eligible age range for survey respondents
# (age expressed in days)

vcqi_global(VCQI_RI_MIN_AGE_OF_ELIGIBILITY, 365)
vcqi_global(VCQI_RI_MAX_AGE_OF_ELIGIBILITY, 729)

# These following parameters help describe the survey protocol with regard to whether they:
#  a) skipped going to health centers to find RI records (RI_RECORDS_NOT_SOUGHT 1)
#  b) looked for records for all respondents (RI_RECORDS_SOUGHT_FOR_ALL 1)
#  c) looked for records for respondents who didn't present vaccination cards
#     during the household interview (RI_RECORDS_SOUGHT_IF_NO_CARD 1)

# These are mutually exclusive, so only one of them should be set to 1.

vcqi_global(RI_RECORDS_NOT_SOUGHT, 1)
vcqi_global(RI_RECORDS_SOUGHT_FOR_ALL, 0)
vcqi_global(RI_RECORDS_SOUGHT_IF_NO_CARD, 0)

# .........................................................................
# Which doses should be included in the analysis?
# .........................................................................

# Note that these abbreviations must correspond to those used in the names of
# the dose date and dose tick variables *AND* the names used above in the
# schedule globals (<dose>_min_age_days and <dose>_min_interval_days and
# <dose>_max_days.  The variables are named using lower-case acronyms.  The
# globals here may be upper or mixed case; they will be converted to lower case
# in the software.

vcqi_global(RI_SINGLE_DOSE_LIST, c("BCG", "MCV"))
vcqi_global(RI_MULTI_2_DOSE_LIST, c())
vcqi_global(RI_MULTI_3_DOSE_LIST, c("PENTA", "OPV"))

# In this example we do not have any two-dose vaccine series to analyze. The
# RI_MULTI_2_DOSE_LIST is defined as an empty list above - it could also be
# NULL: vcqi_global(RI_MULTI_2_DOSE_LIST, NULL), or the line defining
# RI_MULTI_2_DOSE_LIST could be omitted entirely.

# The R VCQI software can handle dose lists with up to 9 doses (RI_MULTI_9_DOSE_LIST)

# .........................................................................
# Parameters to describe the analysis being requested
# .........................................................................

vcqi_global(LEVEL2_ORDER_DATASET, paste0(VCQI_DATA_FOLDER, "/level2order.dta"))
vcqi_global(LEVEL3_ORDER_DATASET, paste0(VCQI_DATA_FOLDER, "/level3order.dta"))

vcqi_global(LEVEL1_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level1name.dta"))
vcqi_global(LEVEL2_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level2names.dta"))
vcqi_global(LEVEL3_NAME_DATASET, paste0(VCQI_DATA_FOLDER, "/level3names.dta"))

# LEVEL4 parameters

# The LEVEL4 parameters determine the geographic and/or demographic strata for
# which results are displayed in tabular output and plots. To use the R version
# of VCQI, there must be at least one variable listed in
# VCQI_LEVEL4_SET_VARLIST. The user may specify a single stratifier (like
# urban/rural) or a set of several stratifiers (like urban/rural and sex and
# household wealth).
#
# For example, setting vcqi_global(VCQI_LEVEL4_SET_VARLIST, c("level1name",
# "level3name")) will produce output for the level 1 stratum (overall/national)
# and each level 3 stratum (e.g. each state). If VCQI_LEVEL4_SET_VARLIST is
# populated and VCQI_LEVEL4_SET_LAYOUT is not defined, then VCQI will generate a
# default layout for tables and figures. That layout file will be saved in the
# VCQI_OUTPUT_FOLDER.
#
# The user may create their own VCQI_LEVEL4_SET_LAYOUT file defining the
# conditions, preferred order, and row labels for the LEVEL4 strata and point to
# that layout file in the control program, e.g.
# vcqi_global(VCQI_LEVEL4_SET_LAYOUT, "Q:/My_VCQI_Output/my_level4_layout.rds").
# See the VCQI User's Guide for more details on creating a layout file.

vcqi_global(VCQI_LEVEL4_SET_VARLIST, "level3name")
vcqi_global(VCQI_LEVEL4_SET_LAYOUT, NA)

# User specifies survey::svydesign syntax to describe the complex sample
# The data argument in survey::svydesign should *not* be specified here
vcqi_global(VCQI_SVYDESIGN_SYNTAX, list(ids = ~clusterid, weights = ~psweight, strata = ~stratumid))

# User specifies the method for calculating confidence intervals
# Valid choices are "Logit", "Wilson", "Jeffreys" or "Clopper"; our default recommendation is "Wilson"
vcqi_global(VCQI_CI_METHOD, "Wilson")

# Specify whether the code should export to excel, or not (usually 1)
vcqi_global(EXPORT_TO_EXCEL, 1)

# User specifies the number of digits after the decimal place in coverage outcomes
vcqi_global(VCQI_NUM_DECIMAL_DIGITS, 1)

# Specify whether the code should make plots, or not (usually 1)
# MAKE_PLOTS must be 1 for any plots to be made
vcqi_global(MAKE_PLOTS, 1)

# Set PLOT_OUTCOMES_IN_TABLE_ORDER to 1 if you want inchworm and
# unweighted plots to list strata in the same order as the tables;
# otherwise the strata will be sorted by the outcome and shown in
# bottom-to-top order of increasing indicator performance
vcqi_global(PLOT_OUTCOMES_IN_TABLE_ORDER, 1)

# Make inchworm/bar plots? Set to 1 for yes.
vcqi_global(VCQI_MAKE_IW_PLOTS, 1)

# Text at right side of inchworm/bar plots
# 1 1-sided 95% LCB | Point Estimate | 1-sided 95% UCB
# 2 Point Estimate (2-sided 95% Confidence Interval)  [THIS IS THE DEFAULT]
# 3 Point Estimate (2-sided 95% Confidence Interval) (0, 1-sided 95% UCB]
# 4 Point Estimate (2-sided 95% Confidence Interval) [1-sided 95% UCB, 100)
# 5 Point Estimate (2-sided 95% CI) (0, 1-sided 95% UCB] [1-sided 95% LCB, 100)
vcqi_global(VCQI_IWPLOT_CITEXT, 2)

# Text at right side of double inchworm/bar plots
# 1 (default) means show both point estimates
# 2 means show both point estimates and both 2-sided 95% CIs
# 3 means do not show any text
vcqi_global(VCQI_DOUBLE_IWPLOT_CITEXT, 1)

# IWPLOT_SHOWBARS = 0 means show inchworm distributions
# IWPLOT_SHOWBARS = 1 means show horizontal bars instead of inchworms
# For the current version of R VCQI, please always set this to be 1
vcqi_global(IWPLOT_SHOWBARS, 1)

# Make unweighted sample proportion plots? Set to 1 for yes.
vcqi_global(VCQI_MAKE_UW_PLOTS, 1)

#Annotate text in the unweighted plot for small sample sizes? Set 1 for yes.
vcqi_global(UWPLOT_ANNOTATE_LOW_MED, 0)
#Add square brackets around N < UWPLOT_ANNOTATE_LOW_N; default is 25
vcqi_global(UWPLOT_ANNOTATE_LOW_N, NA)
#Add parentheses around N < UWPLOT_ANNOTATE_MED_N; default is 50
vcqi_global(UWPLOT_ANNOTATE_MED_N, NA)

# Make organ pipe plots? Set to 1 for yes.
vcqi_global(VCQI_MAKE_OP_PLOTS, 1)

# Save the data underlying each organ pipe plot?  Set to 1 for yes.
#
# Recall that organ pipe plots do not include many quantitative details
# and do not list the cluster id for any of the bars.
#
# If this option is turned on, (set to 1) then the organ pipe plot program
# will save a dataset in the Plots_OP folder for each plot.  The dataset will
# list the cluster id for each bar in the plot along with its height and width.
# This makes it possible to identify which cluster id goes with which bar in
# the plot and to understand the quantitative details of each bar.
vcqi_global(VCQI_SAVE_OP_PLOT_DATA, 1)

# Save the data underlying inchworm plots/bar plots? Set to 1 for yes.
# If this option is turned on, inchworm and barplot programs will save a dataset
# in the Plots_IW_UW folder that makes it possible to understand the quantitative
# details of each plot component and can be used to recreate the plot.
vcqi_global(VCQI_SAVE_IW_PLOT_DATA, 1)

# Save the data underlying unweighted plots? Set to 1 for yes.
# If this option is turned on, unweighted plot programs will save a dataset
# in the Plots_IW_UW folder that makes it possible to understand the quantitative
# details of each plot component and can be used to recreate the plot.
vcqi_global(VCQI_SAVE_UW_PLOT_DATA, 1)

# Specify whether the code should save VCQI output databases
#
# WARNING!! If this macro is set to 1, VCQI will delete ALL files that
# end in _database.rds in the VCQI_OUTPUT_FOLDER at the end of the run
# If you want to save the databases, change the value to 0.
# (Usually 1)
vcqi_global(DELETE_VCQI_DATABASES_AT_END, 0)

# If you wish to aggregate files that end
# in _database.rds into a single dataset, set the
# DELETE_VCQI_DATABASES_AT_END option to 0 and set the
# AGGREGATE_VCQI_DATABASES option to 1.
vcqi_global(AGGREGATE_VCQI_DATABASES, 1)

# Specify whether the code should delete intermediate datasets
# at the end of the analysis (Usually 1)
# If you wish to keep them for additional analysis or debugging,
# set the option to 0.
vcqi_global(DELETE_TEMP_VCQI_DATASETS, 1)
#
# For RI analysis, there is an optional report on data quality
# Set this global to 1 to generate that report
# It appears in its own separate Excel file.
vcqi_global(VCQI_REPORT_DATA_QUALITY, 0)

# Set this global to 1 if you would like to create an augmented dataset
# that merges survey dataset with derived variables calculated by VCQI.
# Default value is 0 (no)
vcqi_global(VCQI_MAKE_AUGMENTED_DATASET, 1)

# *************************************************
# Code Block: RI-E             (Do not change) ----
# ** Format the VCQI dose list and pre-process survey data

# Construct the global RI_DOSE_LIST from what the user specified above
# VCQI currently handles single-dose vaccines and multi-dose vaccines
# with up to nine doses in the series

# First, list single dose vaccines
if (vcqi_object_exists("RI_SINGLE_DOSE_LIST")){
  RI_DOSE_LIST <- stringr::str_to_lower(RI_SINGLE_DOSE_LIST)
} else{
  RI_DOSE_LIST <- NULL
}

# Second, list doses in multi-dose lists
for(i in 2:9){
  if(vcqi_object_exists(paste0("RI_MULTI_", i, "_DOSE_LIST"))){
    dl <- get(paste0("RI_MULTI_", i, "_DOSE_LIST"))
    if(!is.null(dl) & length(dl > 0)){
      RI_DOSE_LIST <- c(RI_DOSE_LIST, paste0(rep(stringr::str_to_lower(dl), each = i), 1:i))}
  }
}

# Put a copy of the dose list in the log
vcqi_log_global(RI_DOSE_LIST)

# .........................................................................
# Check the user's metadata for completeness and correctness
# .........................................................................

# Starting RI_TEMP_DATASETS as an empty object before starting to record temp dataset list
RI_TEMP_DATASETS <- NULL

check_RI_schedule_metadata()
check_RI_survey_metadata()
check_RI_analysis_metadata()

# Run the program to look at date of birth (from history, card, and register)
# and look at dates of vaccination from cards and register. This program
# evaluates each date and checks to see that it occurred in the period
# allowed for respondents eligible for this survey. It also checks to see
# that doses in a sequence were given in order. If any vaccination date
# seems to be outside the right range or recorded out of sequence, the date
# is stripped off and replaced with a simple yes/no tick mark. This step
# means less date-checking is necessary in subsequent programs.

cleanup_RI_dates_and_ticks()

# The name of the datasets coming out of these cleanup steps are:
#   "{VCQI_OUTPUT_FOLDER}/{VCQI_DATASET}_clean" &
#   "{VCQI_OUTPUT_FOLDER}/{VCQI_RIHC_DATASET}_clean"

# .........................................................................
# Establish unique IDs
# .........................................................................

# The name of the dataset coming out of the ID step is RI_with_ids
establish_unique_RI_ids()

# If the user requests a check instead of a run, then turn off
# flags that result in databases, excel output, and plots

if(VCQI_CHECK_INSTEAD_OF_RUN == 1){
  vcqi_log_comment(VCP, 3, "Comment",
                   "The user has requested a check instead of a run.")
  VCQI_PREPROCESS_DATA <- 0
  VCQI_GENERATE_DVS <- 0
  VCQI_GENERATE_DATABASES <- 0
  EXPORT_TO_EXCEL <- 0
  MAKE_PLOTS <- 0
}

# *************************************************
# Code Block: RI-F           (User may change) ----
#
# Calculate VCQI indicators requested by the user

# This is a counter that is used to name datasets. It is usually set to 1 but
# the user might change it if requesting repeat analyses with differing
# parameters - see the User's Guide

vcqi_global(ANALYSIS_COUNTER, 1)

# Most indicators may be run in any order the user wishes, although there are
# are some restrictions...see the table in the section of Chapter 6 entitled
# Analysis Counter.

# Over-ride RI_DOSE_LIST with the order we want to see in tables
vcqi_global(RI_DOSE_LIST, c("bcg", "opv1", "opv2", "opv3", "penta1", "penta2", "penta3", "mcv"))

# .........................................................................
# Summarize vaccination coverage
# .........................................................................

# Estimate crude dose coverage for all the doses in the RI_DOSE_LIST

vcqi_global(RI_COVG_01_TO_TITLE, "Crude Coverage")
vcqi_global(RI_COVG_01_TO_SUBTITLE, NA)
vcqi_global(RI_COVG_01_TO_FOOTNOTE_1, "Abbreviations: CI=Confidence Interval; LCB=Lower Confidence Bound; UCB=Upper Confidence Bound; DEFF=Design Effect; ICC=Intracluster Correlation Coefficient")
vcqi_global(RI_COVG_01_TO_FOOTNOTE_2, "Note: This measure is a population estimate that incorporates survey weights. The CI, LCB and UCB are calculated with software that take the complex survey design into account.")
vcqi_global(SORT_PLOT_LOW_TO_HIGH, 1) # 1 means show strata w/ low outcomes at bottom and high at top; 0 is the opposite

RI_COVG_01()

# Estimate valid dose coverage

vcqi_global(RI_COVG_02_TO_TITLE, "Valid Coverage")
vcqi_global(RI_COVG_02_TO_SUBTITLE, NA)
vcqi_global(RI_COVG_02_TO_FOOTNOTE_1, "Abbreviations: CI=Confidence Interval; LCB=Lower Confidence Bound; UCB=Upper Confidence Bound; DEFF=Design Effect; ICC=Intracluster Correlation Coefficient")
vcqi_global(RI_COVG_02_TO_FOOTNOTE_2, "Note: This measure is a population estimate that incorporates survey weights.  The CI, LCB and UCB are calculated with software that take the complex survey design into account.")
vcqi_global(SORT_PLOT_LOW_TO_HIGH, 1) # 1 means show strata w/ low outcomes at bottom and high at top; 0 is the opposite

RI_COVG_02()

# .........................................................................
# Missed Opportunities for Simultaneous Vaccination (MOV)
# .........................................................................

# The next three indicators are concerned with Missed Opportunities for Simultaneous Vaccination (MOV)

# Usually the user will want to see MOV output for all the doses in the RI_DOSE_LIST
# but sometimes they may want to omit some doses.  Either specify the list of doses
# clearly here, or simply copy the RI_DOSE_LIST into the global MOV_OUTPUT_DOSE_LIST
#  e.g., to generate MOV output for only the basic eight EPI doses, we might say:
#  vcqi_global(MOV_OUTPUT_DOSE_LIST, c("bcg", "opv1", "opv2", "opv3", "dpt1", "dpt2", "dpt3", "mcv"))

vcqi_global(MOV_OUTPUT_DOSE_LIST, RI_DOSE_LIST)

# Run the program to establish which dates the child was vaccinated on and
# whether they received every dose for which they were age-eligible (or
# interval-eligible). Put the results in a dataset that is ready to be
# merged in later for MOV indicators.

calculate_MOV_flags()

# Estimate what valid coverage would have been if there had been no MOVs

vcqi_global(RI_QUAL_07B_TO_TITLE, "Coverage if no MOVs")
vcqi_global(RI_QUAL_07B_TO_SUBTITLE, NA)
vcqi_global(RI_QUAL_07B_TO_FOOTNOTE_1, "Abbreviations: CI=Confidence Interval")
vcqi_global(RI_QUAL_07B_TO_FOOTNOTE_2, "Note: This measure is a population estimate that incorporates survey weights. The CIs are calculated with software that take the complex survey design into account.")
vcqi_global(SORT_PLOT_LOW_TO_HIGH, 1) # 1 means show strata w/ low outcomes at bottom and high at top; 0 is the opposite

RI_QUAL_07B()

# Estimate the proportion of visits that had MOVs

vcqi_global(RI_QUAL_08_VALID_OR_CRUDE, "CRUDE") # Set to CRUDE or VALID

vcqi_global(RI_QUAL_08_TO_TITLE, "Percent of Visits with MOVs")
vcqi_global(RI_QUAL_08_TO_SUBTITLE, NA)
vcqi_global(RI_QUAL_08_TO_FOOTNOTE_1, "Percent of visits where children were eligible for the dose and did not receive it.")

if(stringr::str_to_upper(RI_QUAL_08_VALID_OR_CRUDE) == "VALID"){
  vcqi_global(RI_QUAL_08_TO_FOOTNOTE_2, "Note: Early doses are ignored in this analysis; the respondent is considered to have not received them.")
}

if(stringr::str_to_upper(RI_QUAL_08_VALID_OR_CRUDE) == "CRUDE"){
  vcqi_global(RI_QUAL_08_TO_FOOTNOTE_2, "Note: Early doses are accepted in this analysis; all doses are considered valid doses.")
}

vcqi_global(RI_QUAL_08_TO_FOOTNOTE_3, "Note: The final measure on this sheet, MOVs per Visit, is not a percent. It is a ratio.")
vcqi_global(SORT_PLOT_LOW_TO_HIGH, 0) # 1 means show strata w/ low outcomes at bottom and high at top; 0 is the opposite

RI_QUAL_08()

# Estimate the proportion of children who experienced 1+ MOVs
vcqi_global(RI_QUAL_09_VALID_OR_CRUDE, "CRUDE") # Set to CRUDE or VALID

vcqi_global(RI_QUAL_09_TO_TITLE, "Percent of Respondents with MOVs")
vcqi_global(RI_QUAL_09_TO_SUBTITLE, NA)
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_1, "Percent of respondents who had date of birth and visit date data who failed to receive a vaccination for which they were eligible on an occasion when they received another vaccination.")
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_2, "An uncorrected MOV means that the respondent had still not received a valid dose at the time of the survey.")
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_3, "A corrected MOV means that the respondent had received a valid dose by the time of the survey.")
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_4, "The denominator for Had MOV (%) is the number of respondents who had visits eligible.")
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_5, "The denominator for MOV uncorrected and corrected (%) is the number of MOVs.")
vcqi_global(RI_QUAL_09_TO_FOOTNOTE_6, "Note that for individual doses, the % MOV uncorrected + % MOV corrected adds up to 100%.")

if(stringr::str_to_upper(RI_QUAL_09_VALID_OR_CRUDE) == "VALID"){
  vcqi_global(RI_QUAL_09_TO_FOOTNOTE_7, "Note: Early doses are ignored in this analysis; the respondent is considered to have not received them.")
}

if(stringr::str_to_upper(RI_QUAL_09_VALID_OR_CRUDE) == "CRUDE"){
  vcqi_global(RI_QUAL_09_TO_FOOTNOTE_7, "Note: Early doses are accepted in this analysis; all doses are considered valid doses.")
}

# This indicator makes plots (1) if any MOV and (2) if corrected. These are sorted in opposite directions, so global SORT_PLOT_LOW_TO_HIGH is set inside RI_QUAL_09_06PO.R rather than here by the user.

RI_QUAL_09()

# .........................................................................
# Make Coverage and Timeliness Charts
# .........................................................................

# Specify 1 or 2 or 3 here to make charts for every level 1, 2 or 3 stratum.
RI_VCTC_01_LEVELS <- 1
# You may also specify a combination like c(1,3)
# RI_VCTC_01_LEVELS <- c(1,3)

#Specify which doses to show in the chart and the order, from bottom to top

TIMELY_DOSE_ORDER <- c("bcg","opv1","opv2","opv3","penta1","penta2","penta3","mcv")

# Specify the y-coordinates for the bars.  If you want them to be spaced evenly, you may omit this global (leave it empty)
# In this example, we use irregular spacing to group the different dose series.
TIMELY_Y_COORDS <- c(10, 23, 30, 37, 50, 57, 64, 77)

# You may customize the parameters in the .R file and define the full
# path to the .R file below as VCTC_globals_path. Or you may re-specify them
# in code after VCTC_default_global() was called

# Include the user-specified parameters, if want to customize any settings
vcqi_global(VCTC_globals_path, NA)

if (vcqi_object_exists("VCTC_globals_path")){
  source(file = VCTC_globals_path)
} else {
  VCTC_default_global()
}

# Over-ride one default parameters:

# Because we are spacing the bars about every y=10 units instead of the
# default Y=1, specify a bar width that is 10X the default.
vcqi_global(TIMELY_BARWIDTH, 6.7)

# Do the calculations and make the charts
RI_VCTC_01()

# *************************************************
# Code Block: RI-G             (Do not change) ----
#
# Exit gracefully

# Make augmented dataset for additional analysis purposes if user requests it.
if(vcqi_object_value("VCQI_MAKE_AUGMENTED_DATASET", 1) &
   !vcqi_object_value("VCQI_CHECK_INSTEAD_OF_RUN", 1)){
  make_RI_augmented_dataset(outpath = NA)
}

# Close the datasets that hold the results of hypothesis tests and put them into
# the output spreadsheet
# Close the log file and put it into the output spreadsheet
# Clean up extra files
# Send a message to the screen if there are warnings or errors in the log
vcqi_cleanup()
