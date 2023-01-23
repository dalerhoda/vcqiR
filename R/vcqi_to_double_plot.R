#' Create double barplot
#'
#' @param database Database used to create the plot
#' @param database2 Second database used to create the plot
#' @param filename Path to save the plot
#' @param datafile Tile which the database is based on
#' @param datafile2 Tile which the second database is based on
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

# vcqi_to_double_plot R version 1.04 - Biostat Global Consulting - 2022-12-21
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-14  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# 2022-10-19  1.02      Caitlin Clary   Update error message handling, add calls
#                                       to vcqi_halt_immediately
# 2022-12-15  1.03      Mia Yu          Add title etc. to the dataset
# 2022-12-21  1.04      Mia Yu          Add part to update footnote
# *******************************************************************************

vcqi_to_double_plot <- function(
    database,
    database2,
    filename,
    datafile = NULL,
    datafile2 = NULL,
    title = NULL,
    name = NULL,
    subtitle = NULL,
    note = NULL,
    #currently the note is the "caption" as in ggplot2 we use caption = to add note
    caption = NULL,
    savedata = NA,
    savew = 7,
    saveh = 7,
    VCP = "vcqi_to_double_plot"){

  #NOTE: for now we will not need filetag and datafile for bar plots

  vcqi_log_comment(VCP, 5, "Flow", "Starting")

  if (file.exists(database)){
    dat <- vcqi_read(database)

    if (!is.data.frame(dat)){
      errormsgs <- c(
        "vcqi_to_double_plot: The VCQI database passed in to this program is not in the right format.",
        paste0("vcqi_to_double_plot: The database named was ", database, ".")
      )

      vcqi_log_comment(
        VCP, 1, "Error",
        "vcqi_to_double_plot: The VCQI database passed in to this program is not in the right format.")
      vcqi_log_comment(
        VCP, 1, "Error",
        paste0("vcqi_to_double_plot: The database named was ", database, "."))

      vcqi_global(VCQI_ERROR, 1)
      vcqi_halt_immediately(
        halt_message = errormsgs
      )
    }

  } else {

    errormsgs <- c(
      "vcqi_to_double_plot: The VCQI database passed in to this program does not seem to exist.",
      paste0("vcqi_to_double_plot: The database named was ", database, ".")
    )

    vcqi_log_comment(
      VCP, 1, "Error",
      "vcqi_to_double_plot: The VCQI database passed in to this program does not seem to exist.")
    vcqi_log_comment(
      VCP, 1, "Error",
      paste0("vcqi_to_double_plot: The database named was ", database, "."))

    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )

  } #end of check database exists

  if (file.exists(database2)){
    dat2 <- vcqi_read(database2)

    if (!is.data.frame(dat2)){

      errormsgs <- c(
        "vcqi_to_double_plot: The VCQI database2 passed in to this program is not in the right format.",
        paste0("vcqi_to_double_plot: The database2 named was ", database2, ".")
      )

      vcqi_log_comment(
        VCP, 1, "Error",
        "vcqi_to_double_plot: The VCQI database2 passed in to this program is not in the right format.")
      vcqi_log_comment(
        VCP, 1, "Error",
        paste0("vcqi_to_double_plot: The database2 named was ", database2, "."))

      vcqi_global(VCQI_ERROR, 1)
      vcqi_halt_immediately(
        halt_message = errormsgs
      )
    }

  } else {

    errormsgs <- c(
      "vcqi_to_double_plot: The VCQI database2 passed in to this program does not seem to exist.",
      paste0("vcqi_to_double_plot: The database2 named was ", database2, ".")
    )

    vcqi_log_comment(
      VCP, 1, "Error",
      "vcqi_to_double_plot: The VCQI database2 passed in to this program does not seem to exist.")
    vcqi_log_comment(
      VCP, 1, "Error",
      paste0("vcqi_to_double_plot: The database2 named was ", database2, "."))

    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  } # end of check database2 exists

  if (!is.null(datafile)){
    if (!file.exists(datafile)){

      errormsgs <- c(
        "vcqi_to_double_plot: The VCQI datafile passed in to this program does not seem to exist.",
        paste0("vcqi_to_double_plot: The datafile named was ", datafile, ".")
      )

      vcqi_log_comment(
        VCP, 1, "Error",
        "vcqi_to_double_plot: The VCQI datafile passed in to this program does not seem to exist.")
      vcqi_log_comment(
        VCP, 1, "Error",
        paste0("vcqi_to_double_plot: The datafile named was ", datafile, "."))

      vcqi_global(VCQI_ERROR, 1)
      vcqi_halt_immediately(
        halt_message = errormsgs
      )

    } # end of check datafile exists
  }

  if (!is.null(datafile2)){
    if (!file.exists(datafile2)){

      errormsgs <- c(
        "vcqi_to_double_plot: The VCQI datafile2 passed in to this program does not seem to exist.",
        paste0("vcqi_to_double_plot: The datafile2 named was ", datafile2, ".")
      )

      vcqi_log_comment(
        VCP, 1, "Error",
        "vcqi_to_double_plot: The VCQI datafile2 passed in to this program does not seem to exist.")
      vcqi_log_comment(
        VCP, 1, "Error",
        paste0("vcqi_to_double_plot: The datafile2 named was ", datafile2, "."))

      vcqi_global(VCQI_ERROR, 1)
      vcqi_halt_immediately(
        halt_message = errormsgs
      )

    } # end of check datafile2 exists
  }

  if (IWPLOT_SHOWBARS == 1){
    name <- gsub("iwplot","brplot",name,fixed = TRUE)
  }

  dat <- dat %>% mutate(source = "dat")
  dat2 <- dat2 %>% mutate(source = "dat2")
  #save dat2 to tempdata for later use
  tempdata <- dat2

  #First we add the text

  hundred <- paste0("100.", str_dup("0", VCQI_NUM_DECIMAL_DIGITS))

  if (!vcqi_object_exists("VCQI_DOUBLE_IWPLOT_CITEXT")) {VCQI_DOUBLE_IWPLOT_CITEXT = 1}
  if (!(VCQI_DOUBLE_IWPLOT_CITEXT %in% c(1, 2, 3))) {VCQI_DOUBLE_IWPLOT_CITEXT = 1}

  if (VCQI_DOUBLE_IWPLOT_CITEXT == 1){
    dat <- dat %>%
      mutate(
        text1 = paste0(sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"),
                               estimate*100), " | "))

    dat2 <- dat2 %>%
      mutate(
        text2 = sprintf(paste0("%.",VCQI_NUM_DECIMAL_DIGITS,"f"),
                        estimate*100)) %>%
      select(c(level4id, text2))
  }

  if (VCQI_DOUBLE_IWPLOT_CITEXT == 2){
    dat <- dat %>%
      mutate(
        text1 = paste0(
          sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100)," (",
          sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
          sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ") | "))

    dat2 <- dat2 %>%
      mutate(
        text2 = paste0(
          sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), estimate*100)," (" ,
          sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), cill*100), ", ",
          sprintf(paste0("%.", VCQI_NUM_DECIMAL_DIGITS, "f"), ciul*100), ")")) %>%
      select(c(level4id,text2))
  }

  if (VCQI_DOUBLE_IWPLOT_CITEXT == 3){
    dat <- dat %>% mutate(text1 = "")
    dat2 <- dat2 %>% mutate(text2 = "") %>%
      select(c(level4id,text2))
  }

  if (is.null(note)){
    if (VCQI_DOUBLE_IWPLOT_CITEXT == 1){
      note <- "Text at right: Point estimates from colored and from gray bars"
    }
    if (VCQI_DOUBLE_IWPLOT_CITEXT == 2){
      note <- "Text at right: Colored Point Estimate (2-sided 95% CI)  |  Gray Point Estimate (2-sided 95% CI)"
    }
    if (VCQI_DOUBLE_IWPLOT_CITEXT == 3){
      note <- NULL
    }
  } else {
    if (IWPLOT_SHOWBARS == 1){
      note <- gsub("shape is", "bar shows", note, fixed = TRUE)
    }

    if (VCQI_DOUBLE_IWPLOT_CITEXT == 1){
      note <- paste0("Text at right: Point estimates from colored and from gray bars \n ", note)
    }
    if (VCQI_DOUBLE_IWPLOT_CITEXT == 2){
      note <- paste0("Text at right: Colored Point Estimate (2-sided 95% CI)  |  Gray Point Estimate (2-sided 95% CI) \n ", note)
    }
  }

  #Now we add text2 to dat and combine text1 and text2
  dat <- inner_join(dat, dat2, by = "level4id")
  dat <- dat %>% mutate(text = paste0(text1, text2)) %>% select(-c(text1, text2))
  dat <- mutate(dat, text = gsub(hundred, "100", text, fixed = TRUE))

  #Note: right now only works if two datasets have the same cols, which is true for default setting
  dat2 <- tempdata
  dat2 <- dat2 %>% mutate(text = "")

  if(all(names(dat) == names(dat2))){
    combined <- rbind(dat,dat2)
  } else {
    errormsgs <- paste0(database, "and ", database2, "need to have the same stracture. Please set column names to be the same.")
    vcqi_log_comment(VCP, 1, "Error",
                     paste0(database, "and ", database2, "need to have the same stracture. Please set column names to be the same."))
    vcqi_global(VCQI_ERROR, 1)
    vcqi_halt_immediately(
      halt_message = errormsgs
    )
  }

  combined <- combined %>% group_by(level4id) %>%
    mutate(temp = row_number()) %>%
    mutate(name = ifelse(temp == 2, NA, name)) %>%
    select(-temp) %>% ungroup()

  tempdata <- combined

  # Sort proportions based on user request
  # Default is sorting proportions low at bottom of plot to high at top of plot

  if (SORT_PLOT_LOW_TO_HIGH == 0){
    order <- dat %>%
      subset(!is.na(estimate)) %>%
      arrange(desc(estimate)) %>%
      mutate(rowid = row_number()) %>%
      select(rowid,level4id)

    combined <- combined %>% subset(!is.na(estimate))
    combined <- left_join(combined, order, by = "level4id")
    combined <- combined %>% arrange(rowid)
  } else if (SORT_PLOT_LOW_TO_HIGH == 1){
    order <- dat %>%
      subset(!is.na(estimate)) %>%
      arrange(estimate) %>%
      mutate(rowid = row_number()) %>%
      select(rowid,level4id)

    combined <- combined %>% subset(!is.na(estimate))
    combined <- left_join(combined,order, by = "level4id")
    combined <- combined %>% arrange(rowid)
  }

  if (PLOT_OUTCOMES_IN_TABLE_ORDER == 1){
    combined <- tempdata
    # If user wants strata plotted in table order, merge the table order
    # and sort accordingly
    vcqi_log_comment(VCP, 3, "Comment",
                     "User has requested that outcomes be plotted in table order instead of sorting by indicator outcome.")

    order <- dat %>%
      subset(!is.na(estimate)) %>%
      arrange(desc(level4id)) %>%
      mutate(rowid = row_number()) %>%
      select(rowid,level4id)

    combined <- combined %>% subset(!is.na(estimate))
    combined <- left_join(combined,order, by = "level4id")
    combined <- combined %>% arrange(rowid)
  }

  combined <- combined %>%
    mutate(source = factor(source, levels = c("dat2","dat")))

  #DEC 15: add title etc. to the dataset
  combined <- combined %>% mutate(graphtitle = NA, graphsubtitle = NA, graphcaption = NA)
  if (!is.null(title)){
    combined <- combined %>% mutate(graphtitle = title)
  }

  if (!is.null(subtitle)){
    combined <- combined %>% mutate(graphsubtitle = subtitle)
  }

  if (!is.null(note)){
    combined <- combined %>% mutate(graphcaption = note)
  }

  if (!is.na(savedata)){
    saveRDS(combined, file = paste0(savedata, ".rds"))
  }

  if (IWPLOT_SHOWBARS == 1){
    extraspace <- max(nchar(combined$text))
    group.colors <- c(dat = "#2b92be", dat2 = "lightgrey")
    group.outline <- c(dat = "#0000ff", dat2 = "lightgrey")

    if (is.na(combined$graphtitle[1])){
      title <- NULL
    } else {
      title <- combined$graphtitle[1]
    }

    if (is.na(combined$graphsubtitle[1])){
      subtitle <- NULL
    } else {
      subtitle <- combined$graphsubtitle[1]
    }

    if (is.na(combined$graphcaption[1])){
      note <- NULL
    } else {
      note <- combined$graphcaption[1]
    }

    ggplot(combined, mapping = aes(x = as.factor(rowid),y = estimate * 100,fill = source, color = source)) +
      theme_bw(base_family = "sans")+
      scale_fill_manual(name = "", values = group.colors, guide = "none") +
      scale_colour_manual(name = "", values = group.outline, guide = "none")+
      geom_col(position = position_dodge2(width = 0.5, preserve = "single"),size=0.3) +
      geom_linerange(aes(ymin = cill * 100, ymax = ciul * 100),colour = "black",
                     position = position_dodge(.9)) +
      geom_text(aes(x = as.factor(rowid),
        y = 100 + extraspace,
        label = text),
        size = 3.25,colour = "black",family = "sans") +
      coord_flip() +
      labs(y = "Estimated Coverage %",
        x = "",
        title = title,
        subtitle = subtitle,
        caption = note) +
      scale_x_discrete(labels = combined$name[!is.na(combined$name)]) +
      #Note: could find a better way to check the space we need for text
      scale_y_continuous(limits = c(0, 100 + 2*extraspace),
                         breaks = c(0, 25, 50, 75, 100)) +
      theme(plot.caption = element_text(hjust = 0),
            text = element_text(family = "sans", colour = "black"))

    ggsave(paste0(filename,".png"),width = savew, height = saveh, units = "in")
  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
