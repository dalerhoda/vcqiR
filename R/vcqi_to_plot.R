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

# vcqi_to_plot R version 1.03 - Biostat Global Consulting - 2022-12-15
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-12  1.00      Mia Yu          Original R version
# 2022-10-10  1.01      Mia Yu          Package version
# 2022-10-19  1.02      Caitlin Clary   Update error message handling, add calls
#                                       to vcqi_halt_immediately
# 2022-12-15  1.03      Mia Yu          Add title etc. to the dataset
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
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), lcb*100), " | ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% | ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ucb*100))
    )
  }

  if (VCQI_IWPLOT_CITEXT == 2){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100),"% (",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")")
    )
  }

  if (VCQI_IWPLOT_CITEXT == 3){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% (",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")", " (0, ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ucb*100), "]")
    )
  }

  if (VCQI_IWPLOT_CITEXT == 4){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% (",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")", " [",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), lcb*100), ", 100)")
    )
  }

  if (VCQI_IWPLOT_CITEXT == 5){
    dat <- mutate(
      dat,
      text = paste0(
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100), "% (" ,
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")", " (0, ",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ucb*100), "]", " [",
        sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), lcb*100), ", 100)")
    )
  }

  dat <- mutate(dat, text = gsub(hundred, "100", text, fixed = TRUE))

  if (is.null(note)){
    if (VCQI_IWPLOT_CITEXT == 1){
      note = "Text at right: 1-sided 95% LCB | Point Estimate | 1-sided 95% UCB"
    }
    if (VCQI_IWPLOT_CITEXT == 2){
      note = "Text at right: Point Estimate (2-sided 95% Confidence Interval)"
    }
    if (VCQI_IWPLOT_CITEXT == 3){
      note = "Text at right: Point Estimate (2-sided 95% Confidence Interval) (0, 1-sided 95% UCB]"
    }
    if (VCQI_IWPLOT_CITEXT == 4){
      note = "Text at right: Point Estimate (2-sided 95% Confidence Interval) [1-sided 95% LCB, 100)"
    }
    if (VCQI_IWPLOT_CITEXT == 5){
      note = "Text at right: Point Estimate (2-sided 95% CI) (0, 1-sided 95% UCB] [1-sided 95% LCB, 100)"
    }
  } else{
    if (VCQI_IWPLOT_CITEXT == 1){
      note = paste0("Text at right: 1-sided 95% LCB | Point Estimate | 1-sided 95% UCB \n ", note)
    }
    if (VCQI_IWPLOT_CITEXT == 2){
      note = paste0("Text at right: Point Estimate (2-sided 95% Confidence Interval) \n ", note)
    }
    if (VCQI_IWPLOT_CITEXT == 3){
      note = paste0("Text at right: Point Estimate (2-sided 95% Confidence Interval) (0, 1-sided 95% UCB] \n ", note)
    }
    if (VCQI_IWPLOT_CITEXT == 4){
      note = paste0("Text at right: Point Estimate (2-sided 95% Confidence Interval) [1-sided 95% LCB, 100) \n ", note)
    }
    if (VCQI_IWPLOT_CITEXT == 5){
      note = paste0("Text at right: Point Estimate (2-sided 95% CI) (0, 1-sided 95% UCB] [1-sided 95% LCB, 100) \n ", note)
    }
  }

  #DEC 15: add title etc. to the dataset
  dat <- dat %>% mutate(graphtitle = title, graphsubtitle = subtitle, graphcaption = note)

  if (!is.na(savedata)){
    saveRDS(dat, file = paste0(savedata,".rds"))
  }

  if (IWPLOT_SHOWBARS == 1){
    extraspace <- max(nchar(dat$text))
    title <- dat$graphtitle[1]
    subtitle <- dat$graphsubtitle[1]
    note <- dat$graphcaption[1]

    ggplot(dat, aes(x = as.factor(rowid), y = estimate * 100)) +
      geom_col(fill = "#2b92be") +
      geom_errorbar(aes(ymin = cill * 100, ymax = ciul * 100),
                    width = .2,
                    position = position_dodge(.9)) +
      geom_text(aes(
        x = as.factor(rowid),
        y = 100 + 1.25*extraspace,
        label = text
      )) +
      coord_flip() +
      labs(
        y = "Estimated Coverage %",
        x = "",
        title = title,
        subtitle = subtitle,
        caption = note
      ) +
      scale_x_discrete(labels = dat$name) +
      #Note: could find a better way to check the space we need for text
      scale_y_continuous(limits = c(0, 100 + 2.25*extraspace),
                         breaks = c(0, 25, 50, 75, 100)) +
      theme(plot.caption = element_text(hjust = 0))+
      theme_bw()

    ggsave(paste0(filename, ".png"), width = savew, height = saveh, units = "in")
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
