#' Create barplot
#'
#' @param database Database used to create the plot
#' @param filename Path to save the plot
#' @param datafile The file which the database is based on
#' @param title Title of the plot
#' @param name Plot identifier in file name
#' @param subtitle Subtitle of the plot
#' @param note Caption of the plot
#' @param caption Additional caption (not currently implemented in R)
#' @param savedata Path to save data that the plot is based on
#' @param savew Width of the plot file in inches
#' @param saveh Height of the plot file in inches
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @return A plot
#'
#' @import ggplot2
#' @import dplyr
#' @import stringr

# vcqi_to_plot R version 1.11 - Biostat Global Consulting - 2024-05-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-12  1.00      Mia Yu          Original R version
# 2022-10-10  1.01      Mia Yu          Package version
# 2022-10-19  1.02      Caitlin Clary   Update error message handling, add calls
#                                       to vcqi_halt_immediately
# 2022-12-15  1.03      Mia Yu          Add title etc. to the dataset
# 2023-01-11  1.04      Mia Yu          Add parts to allow users customize level4 plots
# 2023-02-03  1.05      Mia Yu          Updated level4 plot customization
# 2023-10-02  1.10      Mia Yu          Add globals values for multi-lingual purposes
# 2024-05-20  1.11      Mia Yu          Add % after OS_327
#                                       Switch to split_text program
# *******************************************************************************

vcqi_to_plot <- function(
    database,
    filename,
    datafile = NULL,
    title = NULL,
    name = NULL,
    subtitle = NULL,
    note = NULL,
    #currently the note is the "caption" as in ggplot2 we use caption = to add note
    caption = NULL,
    savedata = NA,
    savew = 7,
    saveh = 7,
    VCP = "vcqi_to_plot"){

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  #browser()

  if (file.exists(database)){
    dat <- vcqi_read(database)

    if (!is.data.frame(dat)){
      errormsgs <- c(
        "vcqi_to_plot: The VCQI database passed in to this program is not in the right format.",
        paste0("vcqi_to_plot: The database named was ", database, "."))

      vcqi_log_comment(
        VCP, 1, "Error",
        "vcqi_to_plot: The VCQI database passed in to this program is not in the right format.")
      vcqi_log_comment(
        VCP, 1, "Error",
        paste0("vcqi_to_plot: The database named was ", database, "."))

      vcqi_global(VCQI_ERROR, 1)
      vcqi_halt_immediately(
        halt_message = errormsgs
      )
    }

  } else {
    errormsgs <- c(
      "vcqi_to_plot: The VCQI database passed in to this program does not seem to exist.",
      paste0("vcqi_to_plot: The database named was ", database, ".")
    )

    vcqi_log_comment(
      VCP, 1, "Error",
      "vcqi_to_plot: The VCQI database passed in to this program does not seem to exist.")
    vcqi_log_comment(
      VCP, 1, "Error",
      paste0("vcqi_to_plot: The database named was ", database, "."))

    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  } # end of check database exists

  if (IWPLOT_SHOWBARS == 1){
    name <- gsub("iwplot", "brplot", name, fixed = TRUE)
  }

  # Sort proportions based on user request
  # Default is sorting proportions low at bottom of plot to high at top of plot

  if (SORT_PLOT_LOW_TO_HIGH == 0){
    # meaning, sort prop high to low
    dat <- arrange(dat, desc(estimate))
  } else{
    dat <- arrange(dat, estimate)
  }

  # If user wants strata plotted in table order, merge the table order
  # and sort accordingly

  if (PLOT_OUTCOMES_IN_TABLE_ORDER == 1){
    vcqi_log_comment(
      VCP, 3, "Comment",
      "User has requested that outcomes be plotted in table order instead of sorting by indicator outcome.")
    dat <- arrange(dat, desc(level4id))
  }

  dat <- dat %>%
    subset(!is.na(estimate)) %>%
    mutate(rowid = row_number())

  if (!vcqi_object_exists("VCQI_IWPLOT_CITEXT")){
    VCQI_IWPLOT_CITEXT <- 2 # NOTE: don't think this need to be a global
  }

  hundred <- paste0("100.", str_dup("0", VCQI_NUM_DECIMAL_DIGITS))

  if (VCQI_IWPLOT_CITEXT == 1){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), lcb*100), " | ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% | ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), ucb*100))
    )
  }

  if (VCQI_IWPLOT_CITEXT == 2){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100),"% (",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")")
    )
  }

  if (VCQI_IWPLOT_CITEXT == 3){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% (",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")", " (0, ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), ucb*100), "]")
    )
  }

  if (VCQI_IWPLOT_CITEXT == 4){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% (",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")", " [",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), lcb*100), ", 100)")
    )
  }

  if (VCQI_IWPLOT_CITEXT == 5){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% (" ,
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")", " (0, ",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), ucb*100), "]", " [",
        sprintf(paste0("%4.", VCQI_NUM_DECIMAL_DIGITS, "f"), lcb*100), ", 100)")
    )
  }

  #   cistring6 contains 2sided-95%-lower-limit - p - 2sided-95%-upper-limit  N=N
  #   (where N is ESS for ESS and N for DATASET)
  # 	(and the estimates do not have info after decimal place, per Nigeria 2016 MICS/NICS report protocol)
  if (VCQI_IWPLOT_CITEXT == 6){

    dat <- dat %>% mutate(text = paste0(
      sprintf("%2.0f", cill * 100),
      " - ",
      sprintf("%2.0f", estimate * 100),
      " - ",
      sprintf("%2.0f", ciul * 100))) %>%
      mutate(text = ifelse(cill*100 < 9.5, paste0(" ", text),text)) %>%
      mutate(text = ifelse((neff < 1000 & neff > 99) %in% TRUE,
                              paste0(text," ",language_string(language_use = language_use, str = "OS_48"),
                                     "= ",prettyNum(round(neff),big.mark=",")),text)) %>%
      mutate(text = ifelse((neff <= 99) %in% TRUE,
                           paste0(text," ",language_string(language_use = language_use, str = "OS_48"),
                                  "=  ",prettyNum(round(neff),big.mark=",")),text)) %>%
      mutate(text = ifelse((neff >= 1000) %in% TRUE,
                           paste0(text," ",language_string(language_use = language_use, str = "OS_48"),
                                  "=",prettyNum(round(neff),big.mark=",")),text)) %>%
      mutate(text = ifelse((neff < 50 & neff > 25) %in% TRUE, paste0(text," (*)"),text)) %>%
      mutate(text = ifelse((neff < 25) %in% TRUE, paste0(text," (!)"),text))

  }

  dat <- mutate(dat, text = gsub(hundred, "100", text, fixed = TRUE))

  if (is.null(note)){
    if (VCQI_IWPLOT_CITEXT == 1){
      note = language_string(language_use = language_use, str = "OS_328")
      #"Text at right: 1-sided 95% LCB | Point Estimate | 1-sided 95% UCB"
    }
    if (VCQI_IWPLOT_CITEXT == 2){
      note = language_string(language_use = language_use, str = "OS_329")
      #"Text at right: Point Estimate (2-sided 95% Confidence Interval)"
    }
    if (VCQI_IWPLOT_CITEXT == 3){
      note = language_string(language_use = language_use, str = "OS_330")
      #"Text at right: Point Estimate (2-sided 95% Confidence Interval) (0, 1-sided 95% UCB]"
    }
    if (VCQI_IWPLOT_CITEXT == 4){
      note = language_string(language_use = language_use, str = "OS_331")
      #"Text at right: Point Estimate (2-sided 95% Confidence Interval) [1-sided 95% LCB, 100)"
    }
    if (VCQI_IWPLOT_CITEXT == 5){
      note = language_string(language_use = language_use, str = "OS_332")
      #"Text at right: Point Estimate (2-sided 95% CI) (0, 1-sided 95% UCB] [1-sided 95% LCB, 100)"
    }
  } else{
    if (VCQI_IWPLOT_CITEXT == 1){
      note = paste0(language_string(language_use = language_use, str = "OS_328")," \n", note)
      #"Text at right: 1-sided 95% LCB | Point Estimate | 1-sided 95% UCB"
    }
    if (VCQI_IWPLOT_CITEXT == 2){
      note = paste0(language_string(language_use = language_use, str = "OS_329")," \n", note)
      #"Text at right: Point Estimate (2-sided 95% Confidence Interval)"
    }
    if (VCQI_IWPLOT_CITEXT == 3){
      note = paste0(language_string(language_use = language_use, str = "OS_330")," \n", note)
      #"Text at right: Point Estimate (2-sided 95% Confidence Interval) (0, 1-sided 95% UCB]"
    }
    if (VCQI_IWPLOT_CITEXT == 4){
      note = paste0(language_string(language_use = language_use, str = "OS_331")," \n", note)
      #"Text at right: Point Estimate (2-sided 95% Confidence Interval) [1-sided 95% LCB, 100)"
    }
    if (VCQI_IWPLOT_CITEXT == 5){
      note = paste0(language_string(language_use = language_use, str = "OS_332")," \n", note)
      #"Text at right: Point Estimate (2-sided 95% CI) (0, 1-sided 95% UCB] [1-sided 95% LCB, 100)"
    }
  }
  #2024-05-20:
  note <- split_text(text_string = note, text_cutoff = FOOTNOTE_CUTOFF)

  #DEC 15: add title etc. to the dataset
  dat <- dat %>% mutate(graphtitle = NA, graphsubtitle = NA, graphcaption = NA)
  if (!is.null(title)){
    dat <- dat %>% mutate(graphtitle = title)
  }

  if (!is.null(subtitle)){
    dat <- dat %>% mutate(graphsubtitle = subtitle)
  }

  if (!is.null(note)){
    dat <- dat %>% mutate(graphcaption = note)
  }

  if (!is.na(savedata)){
    saveRDS(dat, file = paste0(savedata,".rds"))
  }

  if (IWPLOT_SHOWBARS == 1){
    extraspace <- max(nchar(dat$text))

    if (is.na(dat$graphtitle[1])){
      title <- NULL
    } else {
      title <- dat$graphtitle[1]
    }

    if (is.na(dat$graphsubtitle[1])){
      subtitle <- NULL
    } else {
      subtitle <- dat$graphsubtitle[1]
    }

    if (is.na(dat$graphcaption[1])){
      note <- NULL
    } else {
      note <- dat$graphcaption[1]
    }

    #first bring the columns to the data
    dat <- dat %>% mutate(order = level4id)
    dat <- left_join(dat, level4_layout, by = "order")
    dat <- dat %>% select(-c(order, label, condition, rowtype))

    if ("outlinecolor1_r" %in% names(dat)) {
      dat <- dat %>% mutate(outlinecolor1_r = ifelse((is.na(outlinecolor1_r) | is.null(outlinecolor1_r) | outlinecolor1_r == "") %in% TRUE,
                                                  "#0000ff" ,outlinecolor1_r))

    } else {
      dat <- dat %>% mutate(outlinecolor1_r = "#0000ff")
    }

    if ("bar_fillcolor1_r" %in% names(dat)) {
      dat <- dat %>% mutate(bar_fillcolor1_r = ifelse((is.na(bar_fillcolor1_r) | is.null(bar_fillcolor1_r) | bar_fillcolor1_r == "") %in% TRUE,
                                               "#2b92be" ,bar_fillcolor1_r))
    } else {
      dat <- dat %>% mutate(bar_fillcolor1_r = "#2b92be")
    }

    gap <-  1

    #change the rowid to have extra space if line added
    if ("addline" %in% names(dat) | "shadecolor1_r" %in% names(dat)) {
      dat <- dat %>% mutate(rowid = rowid * 2)
      gap <- 2
    }

    baseplot <- ggplot(dat, aes(x = rowid, y = estimate * 100)) +
      theme_bw(base_family = "sans") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    for (l in 1:nrow(dat)) {
      if ("shadecolor1_r" %in% names(dat)) {
        shade <- "TRUE"
        shadecolor1_r <- dat$shadecolor1_r[l]
        if (shadecolor1_r == "" | is.na(shadecolor1_r) | is.null(shadecolor1_r)) {
          shade <- "FALSE"
        }
      } else {
        shade <- "FALSE"
      }

      if ("addline" %in% names(dat)) {
        addl <- "TRUE"
        addline <- dat$addline[l]
        if (addline == "" | is.na(addline) | is.null(addline)) {
          addl <- "FALSE"
        }

      } else {
        addl <- "FALSE"
      }

      if (shade == "TRUE") {
        xminimum <- dat$rowid[l] - 0.9
        xmaximum <- dat$rowid[l] + 0.9
        yminloc = 0
        ymaxloc = 100

        baseplot <- baseplot +
          geom_rect(aes_string(xmin = xminimum,xmax = xmaximum,ymin = yminloc,ymax = ymaxloc,), fill = shadecolor1_r)
      }

      if (addl == "TRUE") {
        xlocation <- dat$rowid[l]
        yminloc = 0
        ymaxloc = 100

        if (addline == "below") {
          baseplot <-
            baseplot + geom_linerange(aes_string(x = xlocation - 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

        if (addline == "above") {
          baseplot <-
            baseplot + geom_linerange(aes_string(x = xlocation + 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

        if (addline == "both") {
          baseplot <- baseplot +
            geom_linerange(aes_string(x = xlocation + 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey") +
            geom_linerange(aes_string(x = xlocation - 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

      }

    } #end of nrow l loop

    baseplot <- baseplot +
      geom_col(width = 1, fill = dat$bar_fillcolor1_r, color = dat$outlinecolor1_r, size = 0.3) +
      geom_errorbar(aes(ymin = cill * 100, ymax = ciul * 100), width = .2, position = position_dodge(.9)) +
      geom_text(aes( x = rowid, y = 100 + 1.25 * extraspace, label = text), colour = "black", family = "sans") +
      coord_flip() +
      labs(y = paste0(language_string(language_use = language_use, str = "OS_327"), " %"), #"Estimated Coverage %"
           x = "", title = title, subtitle = subtitle, caption = note) +
      scale_x_continuous(breaks = seq(min(dat$rowid), max(dat$rowid), by = gap), labels = dat$name) +
      #Note: could find a better way to check the space we need for text
      scale_y_continuous(limits = c(0, 100 + 2.25 * extraspace),
                         breaks = c(0, 25, 50, 75, 100)) +
      theme(plot.caption = element_text(hjust = 0),
            text = element_text(family = "sans", colour = "black"))

    ggsave(plot = baseplot, paste0(filename, ".png"), width = savew, height = saveh, units = "in")


  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
