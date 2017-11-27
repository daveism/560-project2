#image defaults
theme_base_size <- 9
image_width <- 4
image_xwidth <- 5
image_height <- 3
image_extra_width <- 12
image_extra_height <- 6

#year data
year_ace_named_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980 & !is.na(year_ace$ace))
year_ace_hurr_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$hurr_ace))
year_ace_major_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$major_ace))
year_ace_intense_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$intense_ace))

cols <- c("Named Storms" = "gray", "Hurricanes" = "yellow", "Major Hurricanes" = "darkorange2","Intense Hurricanes" = "firebrick1")
cols2 <- c("Named" = "gray", "Hurricanes" = "yellow", "Major" = "darkorange2","Intense" = "firebrick1")

year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic")
singlecols <- c("All Storms" = "darkorange2")
barcount <- ggBarStormsYear(year_ace_wa, "Total Named Storms", year_ace_wa$year, year_ace_wa$named_count, "Year", "Count", "NOAA - Hurrdat2 data")


chart_image <- paste("total_named_storms", "png", sep=".")
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, barcount, width=image_extra_width, height=image_extra_height)


linecount <- ggplot() +
  geom_line(data=year_ace_named_wa, aes(x=as.factor(year), y=named_count, group = 1, colour="Named")) +
  geom_line(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_count, group = 2, colour="Hurricanes")) +
  geom_line(data=year_ace_major_wa, aes(x=as.factor(year), y=major_hurricane_count, group = 3, colour="Major")) +
  geom_line(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_hurricane_count, group = 4, colour="Intense")) +
   scale_colour_manual( values = cols2) +

   #points
   geom_point(data=year_ace_named_wa, aes(x=as.factor(year), y=named_count, group = 1), color="gray") +
   geom_point(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_count, group = 2), color="yellow") +
   geom_point(data=year_ace_major_wa, aes(x=as.factor(year), y=major_hurricane_count, group = 3), color="darkorange2") +
   geom_point(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_hurricane_count, group = 4), color="firebrick1") +
    labs( color = "Storm" ) +
    scale_x_discrete(breaks = b_breaks,labels = b_labels) +

    #Smoothed Lines
    geom_smooth(data=year_ace_named_wa, aes(x=as.factor(year), y=named_count, group = 1), method = "lm",color="darkgray",se=0) +
    geom_smooth(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_count, group = 2), method = "lm",color="yellow2",se=0) +
    geom_smooth(data=year_ace_major_wa, aes(x=as.factor(year), y=major_hurricane_count, group = 3), method = "lm",color="darkorange3",se=0) +
    geom_smooth(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_hurricane_count, group = 4), method = "lm",color="firebrick",se=0) +

    coord_equal() +
    theme_minimal(base_size=theme_base_size) +

    labs(
      title = "Count of Storms",
         x = "Year",
         y = "Count",
       caption=paste("Source:","NOAA - Hurrdat2 data")) +

    theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
          aspect.ratio = 6/18,
          legend.position = "bottom",
          # legend.key.width = unit(1,"line"),
          # legend.key.height = unit(1,"line"),
          plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size=6),
          # legend.text=element_text(size=6),
          # legend.title=element_text(size=6)
        )

    chart_image <- paste("total_storms_line", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, linecount, width=image_extra_width, height=image_extra_height)


#ACE for all all stroms all years
all_ace <- ggplot() +
   geom_line(data=year_ace_wa, aes(x=year, y=ace, group = 1, colour="All Storms")) +
   geom_point(data=year_ace_wa, aes(x=year, y=ace, group = 1), color="darkorange2", alpha=3/4) +
    labs( color = "Storms" ) +

   geom_smooth(data=year_ace_wa, aes(x=year, y=ace, group = 1), method = "lm",color="darkorange3",se=0) +
    scale_colour_manual( values = singlecols) +

   coord_equal() +
   theme_minimal(base_size=theme_base_size) +
   labs(
     title = "ACE Index Since 1850",
        x = "Year",
        y = "ACE",
      caption=paste("Source:","NOAA - Hurrdat2 data")) +

   theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
         aspect.ratio = 6/18,
         legend.position = "bottom",
         # legend.key.width = unit(1,"line"),
         # legend.key.height = unit(1,"line"),
         plot.subtitle = element_text(color="#666666"),
         plot.caption = element_text(color="#AAAAAA", size=6),
         # legend.text=element_text(size=6),
         # legend.title=element_text(size=6)
       )

chart_image <- paste("all_ace", "png", sep=".")
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, all_ace, width=image_extra_width, height=image_extra_height)

#Yearly ACE
b_breaks = c("1980", "1985", "1990", "1995", "2000",  "2005", "2010", "2015" )
b_labels = c("1980", "1985", "1990", "1995", "2000",  "2005", "2010", "2015" )

year_ace_plot <- ggplot() +
   #lines
   geom_line(data=year_ace_named_wa, aes(x=as.factor(year), y=ace, group = 1, colour="Named")) +
   geom_line(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurr_ace, group = 2, colour="Hurricanes")) +
   geom_line(data=year_ace_major_wa, aes(x=as.factor(year), y=major_ace, group = 3, colour="Major")) +
   geom_line(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_ace, group = 4, colour="Intense")) +
    scale_colour_manual( values = cols2) +

   #points
   geom_point(data=year_ace_named_wa, aes(x=as.factor(year), y=ace, group = 1), color="gray") +
   geom_point(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurr_ace, group = 2), color="yellow") +
   geom_point(data=year_ace_major_wa, aes(x=as.factor(year), y=major_ace, group = 3), color="darkorange2") +
   geom_point(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_ace, group = 4), color="firebrick1") +
    labs( color = "Storm" ) +
    scale_x_discrete(breaks = b_breaks,labels = b_labels) +

   #Smoothed Lines
   geom_smooth(data=year_ace_named_wa, aes(x=as.factor(year), y=ace, group = 1), method = "lm",color="darkgray",se=0) +
   geom_smooth(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurr_ace, group = 2), method = "lm",color="yellow2",se=0) +
   geom_smooth(data=year_ace_major_wa, aes(x=as.factor(year), y=major_ace, group = 3), method = "lm",color="darkorange3",se=0) +
   geom_smooth(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_ace, group = 4), method = "lm",color="firebrick",se=0) +

   coord_equal() +
   theme_minimal(base_size=theme_base_size) +

   labs(
     title = "ACE Index Since 1980",
        x = "Year",
        y = "ACE",
      caption=paste("Source:","NOAA - Hurrdat2 data")) +

   theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
         aspect.ratio = 6/18,
         legend.position = "bottom",
         # legend.key.width = unit(1,"line"),
         # legend.key.height = unit(1,"line"),
         plot.subtitle = element_text(color="#666666"),
         plot.caption = element_text(color="#AAAAAA", size=6),
         # legend.text=element_text(size=6),
         # legend.title=element_text(size=6)
       )

   chart_image <- paste("ace_storm_types_1980", "png", sep=".")
   chart_image <- file.path(charts_dir, chart_image)
   chart_image <- chart_image[1]
   chart_image <- gsub(" ", "_", chart_image)
   ggsave(chart_image, year_ace_plot, width=image_extra_width, height=image_extra_height)


 #Yearly Mean Max Wind Speed
  year_mean_plot <-  ggplot() +

    #lines
    geom_line(data=year_ace_named_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1, colour="Named")) +
    geom_line(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2, colour="Hurricanes")) +
    geom_line(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3, colour="Major")) +
    geom_line(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4, colour="Intense")) +
     scale_colour_manual( values = cols2) +
     guides(fill=guide_legend(ncol=2)) +

    #points
    geom_point(data=year_ace_named_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1), color="gray") +
    geom_point(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2), color="yellow") +
    geom_point(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3), color="darkorange2") +
    geom_point(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4), color="firebrick1") +
     labs(color = "Storm" ) +
     scale_x_discrete(breaks = b_breaks,labels = b_labels) +

    #Smoothed Lines
    geom_smooth(data=year_ace_named_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1), method = "lm",color="darkgray",se=0) +
    geom_smooth(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2), method = "lm",color="yellow2",se=0) +
    geom_smooth(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3), method = "lm",color="darkorange3",se=0) +
    geom_smooth(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4), method = "lm",color="firebrick",se=0) +

    coord_equal() +
    theme_minimal(base_size=theme_base_size) +

    labs(
      title = "Mean Max Wind Speed M/S Since 1980",
         x = "Year",
         y = "Max Wind Speed M/S",
       caption=paste("Source:","NOAA - Hurrdat2 data")) +

    theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
          aspect.ratio = 6/18,
          legend.position = "bottom",
          # legend.key.width = unit(1,"line"),
          # legend.key.height = unit(1,"line"),
          plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size=6),
          # legend.text=element_text(size=6),
          # legend.title=element_text(size=6)
        )

    chart_image <- paste("yearly_mean_maxwind_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, year_mean_plot, width=image_extra_width, height=image_extra_height)


#storm data
hurr_meta_named_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic"  & hurr_meta$year >= 1980 & hurr_meta$named == 1)
hurr_meta_hurricane_wa  <- subset(hurr_meta_named_wa, hurr_meta_named_wa$basin == "Western Atlantic"  & hurr_meta_named_wa$year >= 1980 & hurr_meta_named_wa$hurricane == 1)
hurr_meta_major_wa <- subset(hurr_meta_named_wa, hurr_meta_named_wa$basin == "Western Atlantic"  & hurr_meta_named_wa$year >= 1980 & hurr_meta_named_wa$hurricane_major == 1 )
hurr_meta_intense_wa <- subset(hurr_meta_named_wa, hurr_meta_named_wa$basin == "Western Atlantic"  & hurr_meta_named_wa$year >= 1980 & hurr_meta_named_wa$hurricane_intense == 1)


hurr_meta_intense_wa_all <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$hurricane_intense == 1)
log_max_wind_fit_intense <- lm( log(hurr_meta_intense_wa_all$max_wind_ms) ~ as.factor(hurr_meta_intense_wa_all$year))


    #### INTENSE
    scatter_intense_wind_fit <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.factor(hurr_meta_hurricane_wa$year))

    #intense scatter
    scatter_intense_wind <- ggScatterAutoF(
      hurr_meta_intense_wa,
      as.numeric(hurr_meta_intense_wa$year),
      log(hurr_meta_intense_wa$max_wind_ms),
      "lm",
      "Intense and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data",
      scatter_intense_wind_fit
    )

    chart_image <- paste("scatter_intense_wind_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_intense_wind, width=image_width, height=image_height)


    #LM fit for max wind and year
    scatter_intense_wind_fit <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.factor(hurr_meta_hurricane_wa$year))

    #Yearly mean wind M/S Intense
    scatter_yearly_intense_wind <- ggScatterAutoYearly(
             year_ace_intense_wa,
             as.numeric(year_ace_intense_wa$year),
             log(year_ace_intense_wa$intense_avg_max_ms),
             "lm",
             "Intense Yearly Mean Max Wind M/S",
             "Storm",
             "Mean Max Wind M/S",
             "NOAA - Hurrdat2 data"
           )

           chart_image <- paste("scatter_yearly_intense_wind_1980", "png", sep=".")
           chart_image <- file.path(charts_dir, chart_image)
           chart_image <- chart_image[1]
           chart_image <- gsub(" ", "_", chart_image)
           ggsave(chart_image, scatter_yearly_intense_wind, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_intense_wind_fit <- lm( log(year_ace_intense_wa$intense_avg_max_ms) ~ as.numeric(year_ace_intense_wa$year))

    log_max_wind_fit_intense <- lm( hurr_meta_intense_wa$max_wind_ms ~ as.factor(hurr_meta_intense_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_intense <- log_max_wind_fit_intense$coefficients[2:length(log_max_wind_fit_intense$coefficients)]
    write.csv(log_max_wind_fit_intense, file.path(data_dir,"intense_coef.csv"))
    log_max_wind_fit_intense <- read.csv(file.path(data_dir,"intense_coef.csv"))
    log_max_wind_fit_intense$X <- str_sub(log_max_wind_fit_intense$X , -4,-1)
    log_max_wind_fit_intense <- dplyr::rename(log_max_wind_fit_intense, year = X, coefficient = x)

    #intense scatter coefficients
    scatter_intense_wind_coefficients <- ggScatterAutoCoef(
      log_max_wind_fit_intense,
      log_max_wind_fit_intense$year,
      log_max_wind_fit_intense$coefficient,
      "lm",
      "Intense Coefficients and of Max Wind M/S",
      "Year",
      "Coefficients",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_intense_wind_coefficients_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_intense_wind_coefficients, width=image_width, height=image_height)

    log_max_wind_fit_intense <- lm( hurr_meta_intense_wa$max_wind_ms ~ as.factor(hurr_meta_intense_wa$year))

    #Yearly mean ace Intense
    scatter_yearly_intense_ace <- ggScatterAutoYearly(
             year_ace_intense_wa,
             as.numeric(year_ace_intense_wa$year),
             year_ace_intense_wa$intense_ace,
             "lm",
             "Intense and ACE (Year)",
             "Storm",
             "ACE",
             "NOAA - Hurrdat2 data"
           )

     chart_image <- paste("storm_intense_yearly_ace_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, scatter_yearly_intense_ace, width=image_width, height=image_height)

    #LM fit for ace and year
    scatter_yearly_intense_ace_fit <- lm( year_ace_intense_wa$intense_ace ~ as.numeric(year_ace_intense_wa$year))

    scatter_intense_ace_fit <- lm( hurr_meta_intense_wa$ace ~ as.factor(hurr_meta_intense_wa$year))

    scatter_intense_ace <- ggScatterAutoF(
      hurr_meta_intense_wa,
      as.numeric(hurr_meta_intense_wa$year),
      hurr_meta_intense_wa$ace,
      "lm",
      "Intense and ACE",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data",
      scatter_intense_ace_fit
    )

   chart_image <- paste("scatter_intense_ace_1980", "png", sep=".")
   chart_image <- file.path(charts_dir, chart_image)
   chart_image <- chart_image[1]
   chart_image <- gsub(" ", "_", chart_image)
   ggsave(chart_image, scatter_intense_ace, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_intense_ace_fit <- lm( hurr_meta_intense_wa$ace ~ as.factor(hurr_meta_intense_wa$year))




    #### major

    scatter_major_wind_fit <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.factor(hurr_meta_major_wa$year))

    #major scatter
    scatter_major_wind <- ggScatterAutoF(
      hurr_meta_major_wa,
      as.numeric(hurr_meta_major_wa$year),
      log(hurr_meta_major_wa$max_wind_ms),
      "lm",
      "Major and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data",
      scatter_major_wind_fit
    )


   chart_image <- paste("scatter_major_wind_1980", "png", sep=".")
   chart_image <- file.path(charts_dir, chart_image)
   chart_image <- chart_image[1]
   chart_image <- gsub(" ", "_", chart_image)
   ggsave(chart_image, scatter_major_wind, width=image_width, height=image_height)


    scatter_major_wind_fit <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.factor(hurr_meta_major_wa$year))

    #Yearly mean wind M/S major
    scatter_yearly_major_wind <- ggScatterAutoYearly(
             year_ace_major_wa,
             as.numeric(year_ace_major_wa$year),
             log(year_ace_major_wa$major_avg_max_ms),
             "lm",
             "Major Yearly Mean Max Wind M/S",
             "Storm",
             "Mean Max Wind M/S",
             "NOAA - Hurrdat2 data"
           )

   chart_image <- paste("scatter_yearly_major_wind_1980", "png", sep=".")
   chart_image <- file.path(charts_dir, chart_image)
   chart_image <- chart_image[1]
   chart_image <- gsub(" ", "_", chart_image)
   ggsave(chart_image, scatter_yearly_major_wind, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_major_wind_fit <- lm( log(year_ace_major_wa$major_avg_max_ms) ~ as.numeric(year_ace_major_wa$year))

    log_max_wind_fit_major <- lm( hurr_meta_major_wa$max_wind_ms ~ as.factor(hurr_meta_major_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_major <- log_max_wind_fit_major$coefficients[2:length(log_max_wind_fit_major$coefficients)]
    write.csv(log_max_wind_fit_major, file.path(data_dir,"major_coef.csv"))
    log_max_wind_fit_major <- read.csv(file.path(data_dir,"major_coef.csv"))
    log_max_wind_fit_major$X <- str_sub(log_max_wind_fit_major$X , -4,-1)
    log_max_wind_fit_major <- dplyr::rename(log_max_wind_fit_major, year = X, coefficient = x)

    #major scatter coefficients
    scatter_major_wind_coefficients <- ggScatterAutoCoef(
      log_max_wind_fit_major,
      log_max_wind_fit_major$year,
      log_max_wind_fit_major$coefficient,
      "lm",
      "Major and Coefficients of Max Wind M/S",
      "Year",
      "Coefficients",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_major_wind_coefficients_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_major_wind_coefficients, width=image_width, height=image_height)

    log_max_wind_fit_major <- lm( hurr_meta_major_wa$max_wind_ms ~ as.factor(hurr_meta_major_wa$year))

    #Yearly ACE major
    scatter_yearly_major_ace <- ggScatterAutoYearly(
             year_ace_major_wa,
             as.numeric(year_ace_major_wa$year),
             year_ace_major_wa$major_ace,
             "lm",
             "Major and ACE (Year)",
             "Storm",
             "ACE",
             "NOAA - Hurrdat2 data"
           )

     chart_image <- paste("scatter_yearly_major_ace_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, scatter_yearly_major_ace, width=image_width, height=image_height)


    #LM fit for MEAN max wind and year
    scatter_yearly_major_ace_fit <- lm( year_ace_major_wa$ace ~ as.numeric(year_ace_major_wa$year))

    scatter_major_ace_fit <- lm( hurr_meta_major_wa$ace ~ as.factor(hurr_meta_major_wa$year))

    scatter_major_ace <- ggScatterAutoF(
      hurr_meta_major_wa,
      as.numeric(hurr_meta_major_wa$year),
      hurr_meta_major_wa$ace,
      "lm",
      "Major and ACE",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data",
      scatter_major_ace_fit
    )

    chart_image <- paste("scatter_major_ace_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_major_ace, width=image_width, height=image_height)


    #LM fit for MEAN max wind and year
    scatter_major_ace_fit <- lm( hurr_meta_major_wa$ace ~ as.factor(hurr_meta_major_wa$year))


    #### hurricane

    scatter_hurricane_wind_fit <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.factor(hurr_meta_hurricane_wa$year))

    #hurricane scatter
    scatter_hurricane_wind <- ggScatterAutoF(
      hurr_meta_hurricane_wa,
      as.numeric(hurr_meta_hurricane_wa$year),
      log(hurr_meta_hurricane_wa$max_wind_ms),
      "lm",
      "Hurricanes and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data",
      scatter_hurricane_wind_fit
    )


    chart_image <- paste("scatter_hurricane_wind_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_hurricane_wind, width=image_width, height=image_height)

    scatter_hurricane_wind_fit <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.factor(hurr_meta_hurricane_wa$year))

    #Yearly mean wind M/S major
    scatter_yearly_hurricane_wind <- ggScatterAutoYearly(
             year_ace_hurr_wa,
             as.numeric(year_ace_hurr_wa$year),
             log(year_ace_hurr_wa$hurricane_avg_max_ms),
             "lm",
             "Hurricane Yearly Mean Max Wind M/S",
             "Storm",
             "Mean Max Wind M/S",
             "NOAA - Hurrdat2 data"
           )

       chart_image <- paste("scatter_yearly_hurricane_wind_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, scatter_yearly_hurricane_wind, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_hurricane_wind_fit <- lm( log(year_ace_hurr_wa$hurricane_avg_max_ms) ~ as.numeric(year_ace_hurr_wa$year))

    log_max_wind_fit_hurricane <- lm( hurr_meta_hurricane_wa$max_wind_ms ~ as.factor(hurr_meta_hurricane_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_hurricane <- log_max_wind_fit_hurricane$coefficients[2:length(log_max_wind_fit_hurricane$coefficients)]
    write.csv(log_max_wind_fit_hurricane, file.path(data_dir,"hurricanes_coef.csv"))
    log_max_wind_fit_hurricane <- read.csv(file.path(data_dir,"hurricanes_coef.csv"))
    log_max_wind_fit_hurricane$X <- str_sub(log_max_wind_fit_hurricane$X , -4,-1)
    log_max_wind_fit_hurricane <- dplyr::rename(log_max_wind_fit_hurricane, year = X, coefficient = x)

    #hurricane scatter coefficients
    scatter_hurricane_wind_coefficients <- ggScatterAutoCoef(
      log_max_wind_fit_hurricane,
      log_max_wind_fit_hurricane$year,
      log_max_wind_fit_hurricane$coefficient,
      "lm",
      "Hurricanes and Coefficients of Max Wind M/S",
      "Year",
      "Coefficient ",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_hurricane_wind_coefficients_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_hurricane_wind_coefficients, width=image_width, height=image_height)

    log_max_wind_fit_hurricane <- lm( hurr_meta_hurricane_wa$max_wind_ms ~ as.factor(hurr_meta_hurricane_wa$year))

    #Yearly ace hurricane
    scatter_yearly_hurricane_ace <- ggScatterAutoYearly(
             year_ace_hurr_wa,
             as.numeric(year_ace_hurr_wa$year),
             year_ace_hurr_wa$hurr_ace,
             "lm",
             "Hurricanes and ACE (Year)",
             "Storm",
             "ACE",
             "NOAA - Hurrdat2 data"
           )

     chart_image <- paste("scatter_yearly_hurricane_ace_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, scatter_yearly_hurricane_ace, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_hurricane_ace_fit <- lm( year_ace_hurr_wa$ace ~ as.numeric(year_ace_hurr_wa$year))

    scatter_hurricane_ace_fit <- lm( hurr_meta_hurricane_wa$ace ~ as.factor(hurr_meta_hurricane_wa$year))

    scatter_hurricane_ace <- ggScatterAutoF(
      hurr_meta_hurricane_wa,
      as.numeric(hurr_meta_hurricane_wa$year),
      hurr_meta_hurricane_wa$ace,
      "lm",
      "Hurricanes and ACE",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data",
      scatter_hurricane_ace_fit
    )

    chart_image <- paste("scatter_hurricane_ace_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_hurricane_ace, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_hurricane_ace_fit <- lm( hurr_meta_hurricane_wa$ace ~ as.factor(hurr_meta_hurricane_wa$year))


      #### named
      scatter_named_wind_fit <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.factor(hurr_meta_named_wa$year))

      #named scatter
      scatter_named_wind <- ggScatterAutoF(
        hurr_meta_named_wa,
        as.numeric(hurr_meta_named_wa$year),
        log(hurr_meta_named_wa$max_wind_ms),
        "lm",
        "Nammed and Max Wind M/S",
        "Storm",
        "Max Wind M/S",
        "NOAA - Hurrdat2 data",
        scatter_named_wind_fit
      )

      chart_image <- paste("scatter_named_wind_1980", "png", sep=".")
      chart_image <- file.path(charts_dir, chart_image)
      chart_image <- chart_image[1]
      chart_image <- gsub(" ", "_", chart_image)
      ggsave(chart_image, scatter_named_wind, width=image_width, height=image_height)

      scatter_named_wind_fit <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.factor(hurr_meta_named_wa$year))

      #Yearly mean wind M/S major
      scatter_yearly_named_wind <- ggScatterAutoYearly(
               year_ace_named_wa,
               as.numeric(year_ace_named_wa$year),
               log(year_ace_named_wa$named_avg_max_ms),
               "lm",
               "Named Yearly Mean Max Wind M/S",
               "Storm",
               "Mean Max Wind M/S",
               "NOAA - Hurrdat2 data"
             )

       chart_image <- paste("scatter_yearly_named_wind_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, scatter_yearly_named_wind, width=image_width, height=image_height)

      #LM fit for MEAN max wind and year
      scatter_yearly_named_hurricane_wind_fit <- lm( log(year_ace_named_wa$named_avg_max_ms) ~ as.numeric(year_ace_named_wa$year))

      log_max_wind_fit_named <- lm( hurr_meta_named_wa$max_wind_ms ~ as.factor(hurr_meta_named_wa$year))

      #remove overall coefficent we only want the individual years
      log_max_wind_fit_named <- log_max_wind_fit_named$coefficients[2:length(log_max_wind_fit_named$coefficients)]
      write.csv(log_max_wind_fit_named, file.path(data_dir,"nammed_coef.csv"))
      log_max_wind_fit_named <- read.csv(file.path(data_dir,"nammed_coef.csv"))
      log_max_wind_fit_named$X <- str_sub(log_max_wind_fit_named$X , -4,-1)
      log_max_wind_fit_named <- dplyr::rename(log_max_wind_fit_named, year = X, coefficient = x)

      #hurricane scatter coefficients
      scatter_named_wind_coefficients <- ggScatterAutoCoef(
        log_max_wind_fit_named,
        log_max_wind_fit_named$year,
        log_max_wind_fit_named$coefficient,
        "lm",
        "Named and Coefficients of Max Wind M/S",
        "Year",
        "Coefficient",
        "NOAA - Hurrdat2 data"
      )

      chart_image <- paste("scatter_named_wind_coefficients_1980", "png", sep=".")
      chart_image <- file.path(charts_dir, chart_image)
      chart_image <- chart_image[1]
      chart_image <- gsub(" ", "_", chart_image)
      ggsave(chart_image, scatter_named_wind_coefficients, width=image_width, height=image_height)

      log_max_wind_fit_named <- lm( hurr_meta_named_wa$max_wind_ms ~ as.factor(hurr_meta_named_wa$year))

      #Yearly ace named
      scatter_yearly_named_ace <- ggScatterAutoYearly(
               year_ace_named_wa,
               as.numeric(year_ace_named_wa$year),
               year_ace_named_wa$ace,
               "lm",
               "Named and ACE (Year)",
               "Storm",
               "ACE",
               "NOAA - Hurrdat2 data"
             )

       chart_image <- paste("scatter_yearly_named_ace_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, scatter_yearly_named_ace, width=image_width, height=image_height)

      #LM fit for aced and year
      scatter_yearly_named_ace_fit <- lm( year_ace_named_wa$ace ~ as.numeric(year_ace_named_wa$year))

      scatter_named_ace_fit <- lm( hurr_meta_named_wa$ace ~ as.factor(hurr_meta_named_wa$year))

      scatter_named_ace <- ggScatterAutoF(
        hurr_meta_named_wa,
        as.numeric(hurr_meta_named_wa$year),
        hurr_meta_named_wa$ace,
        "lm",
        "Nammed and ACE",
        "Storm",
        "ACE",
        "NOAA - Hurrdat2 data",
        scatter_named_ace_fit
      )

      chart_image <- paste("scatter_named_ace_1980", "png", sep=".")
      chart_image <- file.path(charts_dir, chart_image)
      chart_image <- chart_image[1]
      chart_image <- gsub(" ", "_", chart_image)
      ggsave(chart_image, scatter_named_ace, width=image_width, height=image_height)

      #LM fit for MEAN max wind and year
      scatter_named_ace_fit <- lm( hurr_meta_named_wa$ace ~ as.factor(hurr_meta_named_wa$year))


      summary(scatter_intense_wind_fit)
      summary(scatter_yearly_intense_wind_fit)
      summary(log_max_wind_fit_intense)
      summary(scatter_yearly_intense_ace_fit)
      summary(scatter_intense_ace_fit)

      summary(scatter_major_wind_fit)
      summary(scatter_yearly_major_wind_fit)
      summary(log_max_wind_fit_major)
      summary(scatter_yearly_major_ace_fit)
      summary(scatter_major_ace_fit)

      summary(scatter_hurricane_wind_fit)
      summary(scatter_yearly_hurricane_wind_fit)
      summary(log_max_wind_fit_hurricane)
      summary(scatter_yearly_hurricane_ace_fit)
      summary(scatter_hurricane_ace_fit)

      summary(scatter_named_wind_fit)
      summary(scatter_yearly_named_wind_fit)
      summary(log_max_wind_fit_named)
      summary(scatter_yearly_named_ace_fit)
      summary(scatter_named_ace_fit)
