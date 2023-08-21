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

# vcqi_to_double_plot R version 1.06 - Biostat Global Consulting - 2023-02-03
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2022-09-14  1.00      Mia Yu          Original R version
# 2022-10-11  1.01      Mia Yu          Package version
# 2022-10-19  1.02      Caitlin Clary   Update error message handling, add calls
#                                       to vcqi_halt_immediately
# 2022-12-15  1.03      Mia Yu          Add title etc. to the dataset
# 2022-12-21  1.04      Mia Yu          Add part to update footnote
# 2023-01-12  1.05      Mia Yu          Add parts to allow users customize level4 plots
# 2023-02-03  1.06      Mia Yu          Updated level4 plots customization
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

    #first bring the columns to the data
    combined <- combined %>% mutate(order = level4id)
    combined <- left_join(combined, level4_layout, by = "order")
    combined <- combined %>% select(-c(order, label, condition, rowtype))

    combined <-
      combined %>% mutate(oldid = rowid) %>% mutate(rowid = row_number())

    if ("outlinecolor2_r" %in% names(combined)) {
      combined <-
        combined %>% mutate(outlinecolor2_r = ifelse((is.na(outlinecolor2_r) | is.null(outlinecolor2_r) | outlinecolor2_r == "") %in% TRUE,
                                                     "lightgrey" , outlinecolor2_r))
    } else {
      combined <- combined %>% mutate(outlinecolor2_r = "lightgrey")
    }

    if ("outlinecolor1_r" %in% names(combined)) {
      combined <- combined %>% mutate(outlinecolor = outlinecolor1_r)

      combined <-
        combined %>% mutate(outlinecolor = ifelse((is.na(outlinecolor) | is.null(outlinecolor) | outlinecolor == "") %in% TRUE,
                                                     "#0000ff" , outlinecolor))
      combined <-
        combined %>% mutate(outlinecolor = ifelse(source == "dat2", outlinecolor2_r, outlinecolor))
    } else {
      combined <-
        combined %>% mutate(outlinecolor = "#0000ff") %>% mutate(outlinecolor = ifelse(source == "dat2", outlinecolor2_r, outlinecolor))
    }

    if ("bar_fillcolor2_r" %in% names(combined)) {
      combined <-
        combined %>% mutate(bar_fillcolor2_r = ifelse((is.na(bar_fillcolor2_r) |is.null(bar_fillcolor2_r) | bar_fillcolor2_r == "") %in% TRUE,
                                                      "lightgrey" ,bar_fillcolor2_r))
    } else {
      combined <-
        combined %>% mutate(bar_fillcolor2_r = "lightgrey")
    }

    if ("bar_fillcolor1_r" %in% names(combined)) {
      combined <- combined %>% mutate(fillcolor = bar_fillcolor1_r)

      combined <-
        combined %>% mutate(fillcolor = ifelse((is.na(fillcolor) |is.null(fillcolor) | fillcolor == "") %in% TRUE,
                                                      "#2b92be" ,fillcolor))
      combined <-
        combined %>% mutate(fillcolor = ifelse(source == "dat2", bar_fillcolor2_r, fillcolor))
    } else {
      combined <-
        combined %>% mutate(fillcolor = "#2b92be") %>% mutate(fillcolor = ifelse(source == "dat2", bar_fillcolor2_r, fillcolor))
    }

    gap <-  1

    #change the rowid to have extra space if line added
    if ("addline" %in% names(combined) | "shadecolor2_r" %in% names(combined)) {
      combined <- combined %>% mutate(rowid = rowid + oldid)
      gap <- 3
    }

    rowindex <- which(combined$source == "dat")

    baseplot <-
      ggplot(combined, aes(x = rowid, y = estimate * 100)) +
      theme_bw(base_family = "sans") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

    for (l in rowindex) {
      if ("shadecolor2_r" %in% names(combined)) {
        shade <- "TRUE"
        shadecolor2_r <- combined$shadecolor2_r[l]
        if (shadecolor2_r == "" | is.na(shadecolor2_r) | is.null(shadecolor2_r)) {
          shade <- "FALSE"
        } else if (((shadecolor2_r %in% combined$bar_fillcolor1_r)| (shadecolor2_r %in% combined$bar_fillcolor2_r)) %in% TRUE) {
          shadecolor2_r <- "lightgoldenrodyellow"
          print("For double bar plot, the shade color will be set to gold if it is the same as any bar fill color")
        }
      } else {
        shade <- "FALSE"
      }

      if ("addline" %in% names(combined)) {
        addl <- "TRUE"
        addline <- combined$addline[l]
        if (addline == "" | is.na(addline) | is.null(addline)) {
          addl <- "FALSE"
        }
      } else {
        addl <- "FALSE"
      }

      if (shade == "TRUE") {
        xminimum <- combined$rowid[l] - 0.9
        xmaximum <- combined$rowid[l] + 1.9
        yminloc = 0
        ymaxloc = 100

        baseplot <- baseplot +
          geom_rect(aes_string(xmin = xminimum,xmax = xmaximum,ymin = yminloc,ymax = ymaxloc),fill = shadecolor2_r)

      }

      if (addl == "TRUE") {

        xlocation <- combined$rowid[l]
        yminloc = 0
        ymaxloc = 100

        if (addline == "below") {
          baseplot <-
            baseplot + geom_linerange(aes_string(x = xlocation - 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

        if (addline == "above") {
          baseplot <-
            baseplot + geom_linerange(aes_string(x = xlocation + 2,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

        if (addline == "both") {
          baseplot <- baseplot +
            geom_linerange(aes_string(x = xlocation + 2,ymin = yminloc,ymax = ymaxloc),color = "lightgrey") +
            geom_linerange(aes_string(x = xlocation - 1,ymin = yminloc,ymax = ymaxloc),color = "lightgrey")
        }

      }

    } #end of nrow l loop

    baseplot <- baseplot +
      geom_col(width = 1,fill = combined$fillcolor,color = combined$outlinecolor,size = 0.3) +
      geom_linerange(aes(ymin = cill * 100, ymax = ciul * 100),
                     position = position_dodge(.9)) +
      geom_text(aes(x = rowid,y = 100 + extraspace,label = text),size = 3.25,colour = "black",family = "sans") +
      coord_flip() +
      labs(y = "Estimated Coverage %",x = "",title = title,subtitle = subtitle,caption = note) +
      scale_x_continuous(breaks = combined$rowid[!is.na(combined$name)], labels = combined$name[!is.na(combined$name)]) +
      #Note: could find a better way to check the space we need for text
      scale_y_continuous(limits = c(0, 100 + 2 * extraspace),
                         breaks = c(0, 25, 50, 75, 100)) +
      theme(plot.caption = element_text(hjust = 0),
            text = element_text(family = "sans", colour = "black"))

    ggsave(plot = baseplot,paste0(filename, ".png"),width = savew,height = saveh,units = "in")


  }

  vcqi_log_comment(VCP, 5, "Flow", "Exiting")
}
