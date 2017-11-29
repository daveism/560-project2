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

year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic" )
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
year_ace_wa80 <- subset( year_ace_wa , year_ace_wa$year >= 1980)
all_ace <- ggplot() +
   geom_line(data=year_ace_wa80, aes(x=year, y=ace, group = 1, colour="All Storms")) +
   geom_point(data=year_ace_wa80, aes(x=year, y=ace, group = 1), color="darkorange2", alpha=3/4) +
    labs( color = "Storms" ) +

   geom_smooth(data=year_ace_wa80, aes(x=year, y=ace, group = 1), method = "lm",color="darkorange3",se=0) +
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
   geom_smooth(data=year_ace_named_wa, aes(x=as.factor(year), y=ace, weight = year_ace_named_wa$named_count, group = 1), method = "lm",color="darkgray",se=0) +
   geom_smooth(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurr_ace, weight = year_ace_hurr_wa$hurricane_count, group = 2), method = "lm",color="yellow2",se=0) +
   geom_smooth(data=year_ace_major_wa, aes(x=as.factor(year), y=major_ace,  weight = year_ace_major_wa$major_hurricane_count, group = 3), method = "lm",color="darkorange3",se=0) +
   geom_smooth(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_ace, weight =  year_ace_intense_wa$intense_hurricane_count, group = 4), method = "lm",color="firebrick",se=0) +

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
log_max_wind_fit_intense <- lm( log(hurr_meta_intense_wa_all$max_wind_ms) ~ as.numeric(hurr_meta_intense_wa_all$year))


    #### INTENSE
    scatter_intense_wind_fit <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.numeric(hurr_meta_intense_wa$year))

    #intense scatter
    scatter_intense_wind <- ggScatterAuto(
      hurr_meta_intense_wa,
      as.numeric(hurr_meta_intense_wa$year),
      log(hurr_meta_intense_wa$max_wind_ms),
      "lm",
      "Intense and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_intense_wind_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_intense_wind, width=image_width, height=image_height)


    #LM fit for max wind and year
    scatter_intense_wind_weighted_fit <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.numeric(hurr_meta_intense_wa$year))

    #Yearly mean wind M/S Intense
    scatter_yearly_intense_wind_weighted <- ggScatterAutoYearlyWeight(
             year_ace_intense_wa,
             as.numeric(year_ace_intense_wa$year),
             log(year_ace_intense_wa$intense_avg_max_ms),
             "lm",
             "Intense Yearly Mean Max Wind M/S \nWeighted by count",
             "Storm",
             "Mean Max Wind M/S",
             "NOAA - Hurrdat2 data",
             as.numeric(year_ace_intense_wa$intense_hurricane_count)
           )

           chart_image <- paste("scatter_yearly_intense_wind_weighted_1980", "png", sep=".")
           chart_image <- file.path(charts_dir, chart_image)
           chart_image <- chart_image[1]
           chart_image <- gsub(" ", "_", chart_image)
           ggsave(chart_image, scatter_yearly_intense_wind_weighted, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_intense_wind_weighted_fit <- lm( log(year_ace_intense_wa$intense_avg_max_ms) ~ as.numeric(year_ace_intense_wa$year))




        #Yearly mean wind M/S Intense
        scatter_yearly_intense_wind <- ggScatterAutoYearly(
                 year_ace_intense_wa,
                 as.numeric(year_ace_intense_wa$year),
                 log(year_ace_intense_wa$intense_avg_max_ms),
                 "lm",
                 "Intense Yearly Mean Max Wind M/S",
                 "Storm",
                 "Mean Max Wind M/S",
                 "NOAA - Hurrdat2 data",
                 as.numeric(year_ace_intense_wa$intense_hurricane_count)
               )

               chart_image <- paste("scatter_yearly_intense_wind_1980", "png", sep=".")
               chart_image <- file.path(charts_dir, chart_image)
               chart_image <- chart_image[1]
               chart_image <- gsub(" ", "_", chart_image)
               ggsave(chart_image, scatter_yearly_intense_wind, width=image_width, height=image_height)

        #LM fit for MEAN max wind and year
        scatter_yearly_intense_wind_fit <- lm( log(year_ace_intense_wa$intense_avg_max_ms) ~ as.numeric(year_ace_intense_wa$year))


    log_max_wind_fit_intense <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.factor(hurr_meta_intense_wa$year))
    log_max_wind_fit_intense_stder <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.factor(hurr_meta_intense_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_intense <- log_max_wind_fit_intense$coefficients[2:length(log_max_wind_fit_intense$coefficients)]
    test_stder <- summary(log_max_wind_fit_intense_stder)$coef[,2]

    write.csv(log_max_wind_fit_intense, file.path(data_dir,"intense_coef.csv"))
    write.csv(test_stder, file.path(data_dir,"intense_stder.csv"))

    log_max_wind_fit_intense <- read.csv(file.path(data_dir,"intense_coef.csv"))
    log_max_wind_fit_intense_stder <- read.csv(file.path(data_dir,"intense_stder.csv"))

    log_max_wind_fit_intense_stder = log_max_wind_fit_intense_stder[-1,]

    log_max_wind_fit_intense$X <- str_sub(log_max_wind_fit_intense$X , -4,-1)
    log_max_wind_fit_intense_stder$X <- str_sub(log_max_wind_fit_intense_stder$X , -4,-1)

    log_max_wind_fit_intense <- dplyr::rename(log_max_wind_fit_intense, year = X, coefficient = x)
    log_max_wind_fit_intense_stder <- dplyr::rename(log_max_wind_fit_intense_stder, year = X, stder = x)

    log_max_wind_fit_major <- merge(x = log_max_wind_fit_intense, y=log_max_wind_fit_intense_stder, by=c("year") , all.x = TRUE)

    write.csv(log_max_wind_fit_major, file.path(data_dir,"intense_coef.csv"))
    write.csv(log_max_wind_fit_intense_stder, file.path(data_dir,"intense_stder.csv"))


####

    #intense scatter coefficients
    scatter_intense_wind_coefficients <- ggScatterAutoCoef(
      log_max_wind_fit_intense,
      log_max_wind_fit_intense$year,
      log_max_wind_fit_intense$coefficient,
      "lm",
      "Intense Coefficients of Max Wind M/S",
      "Year",
      "Coefficients",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_intense_wind_coefficients_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_intense_wind_coefficients, width=image_width, height=image_height)

    #intense scatter coefficients loess
    scatter_intense_wind_coefficients_loess <- ggScatterAutoCoef(
      log_max_wind_fit_intense,
      log_max_wind_fit_intense$year,
      log_max_wind_fit_intense$coefficient,
      "loess",
      "Intense Coefficients of Max Wind M/S",
      "Year",
      "Coefficients",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_intense_wind_coefficients_loess_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_intense_wind_coefficients_loess, width=image_width, height=image_height)


    box_yearly_intense_wind  <- ggplot(hurr_meta_intense_wa ,aes(as.factor(hurr_meta_intense_wa$year),log(hurr_meta_intense_wa$max_wind_ms)) )  +
      geom_boxplot() +
      geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +

     coord_equal() +
     theme_minimal(base_size=theme_base_size) +

     labs(
       title = "Intense and Max Wind M/S",
          x = "Year",
          y = "Max Wind Speed M/S",
        caption=paste("Source:","NOAA - Hurrdat2 data")) +

     theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
           aspect.ratio = 6/18,
           legend.position = "bottom",
           plot.subtitle = element_text(color="#666666"),
           plot.caption = element_text(color="#AAAAAA", size=6),
         )

     chart_image <- paste("box_wind_Intense_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, box_yearly_intense_wind, width=image_extra_width, height=image_extra_height)

    log_max_wind_fit_intense <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.numeric(hurr_meta_intense_wa$year))
    log_max_wind_fit_intense_stder <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.numeric(hurr_meta_intense_wa$year))

    #Yearly mean ace Intense
    scatter_yearly_intense_weighted_ace <- ggScatterAutoYearlyWeight(
             year_ace_intense_wa,
             as.numeric(year_ace_intense_wa$year),
             log(year_ace_intense_wa$intense_ace),
             "lm",
             "Intense and ACE - Year \nWeighted by count",
             "Storm",
             "ACE",
             "NOAA - Hurrdat2 data",
             as.numeric(year_ace_intense_wa$intense_hurricane_count)
           )

     chart_image <- paste("scatter_yearly_intense_ace_weighted_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, scatter_yearly_intense_weighted_ace, width=image_width, height=image_height)

     scatter_yearly_intense_ace_weighted_fit <- lm(  log(year_ace_intense_wa$intense_ace) ~ as.numeric(year_ace_intense_wa$year)  , weights = as.numeric(year_ace_intense_wa$intense_hurricane_count))

     #Yearly mean ace Intense
     scatter_yearly_intense_ace <- ggScatterAutoYearly(
              year_ace_intense_wa,
              as.numeric(year_ace_intense_wa$year),
              log(year_ace_intense_wa$intense_ace),
              "lm",
              "Intense and ACE (Year)",
              "Storm",
              "ACE",
              "NOAA - Hurrdat2 data",
              as.numeric(year_ace_intense_wa$intense_hurricane_count)
            )

      chart_image <- paste("scatter_yearly_intense_ace_1980", "png", sep=".")
      chart_image <- file.path(charts_dir, chart_image)
      chart_image <- chart_image[1]
      chart_image <- gsub(" ", "_", chart_image)
      ggsave(chart_image, scatter_yearly_intense_ace, width=image_width, height=image_height)


    #LM fit for ace and year
    scatter_yearly_intense_ace_fit <- lm(  log(year_ace_intense_wa$intense_ace) ~ as.numeric(year_ace_intense_wa$year))

    scatter_intense_ace_fit <- lm( log(hurr_meta_intense_wa$ace) ~ as.numeric(hurr_meta_intense_wa$year))

    scatter_intense_ace <- ggScatterAuto(
      hurr_meta_intense_wa,
      as.numeric(hurr_meta_intense_wa$year),
      log(hurr_meta_intense_wa$ace),
      "lm",
      "Intense and ACE",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data"
    )

   chart_image <- paste("scatter_intense_ace_1980", "png", sep=".")
   chart_image <- file.path(charts_dir, chart_image)
   chart_image <- chart_image[1]
   chart_image <- gsub(" ", "_", chart_image)
   ggsave(chart_image, scatter_intense_ace, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_intense_ace_fit <- lm( log(hurr_meta_intense_wa$ace) ~ as.numeric(hurr_meta_intense_wa$year))




    #### major

    scatter_major_wind_fit <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.numeric(hurr_meta_major_wa$year))

    #major scatter
    scatter_major_wind <- ggScatterAuto(
      hurr_meta_major_wa,
      as.numeric(hurr_meta_major_wa$year),
      log(hurr_meta_major_wa$max_wind_ms),
      "lm",
      "Major and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data"
    )


   chart_image <- paste("scatter_major_wind_1980", "png", sep=".")
   chart_image <- file.path(charts_dir, chart_image)
   chart_image <- chart_image[1]
   chart_image <- gsub(" ", "_", chart_image)
   ggsave(chart_image, scatter_major_wind, width=image_width, height=image_height)


    scatter_major_wind_fit <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.numeric(hurr_meta_major_wa$year))

    #Yearly mean wind M/S major
    scatter_yearly_major_weighted_wind <- ggScatterAutoYearlyWeight(
             year_ace_major_wa,
             as.numeric(year_ace_major_wa$year),
             log(year_ace_major_wa$major_avg_max_ms),
             "lm",
             "Major Yearly Mean Max Wind M/S \nWeighted by count",
             "Storm",
             "Mean Max Wind M/S",
             "NOAA - Hurrdat2 data",
             as.numeric(year_ace_major_wa$major_hurricane_count)
           )

   chart_image <- paste("scatter_yearly_major_wind_weighted_1980", "png", sep=".")
   chart_image <- file.path(charts_dir, chart_image)
   chart_image <- chart_image[1]
   chart_image <- gsub(" ", "_", chart_image)
   ggsave(chart_image, scatter_yearly_major_weighted_wind, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_major_wind_weighted_fit <- lm( log(year_ace_major_wa$major_avg_max_ms) ~ as.numeric(year_ace_major_wa$year), weights = as.numeric(as.numeric(year_ace_major_wa$major_hurricane_count)))


    #Yearly mean wind M/S major
    scatter_yearly_major_wind <- ggScatterAutoYearly(
             year_ace_major_wa,
             as.numeric(year_ace_major_wa$year),
             log(year_ace_major_wa$major_avg_max_ms),
             "lm",
             "Major Yearly Mean Max Wind M/S",
             "Storm",
             "Mean Max Wind M/S",
             "NOAA - Hurrdat2 data",
             as.numeric(year_ace_major_wa$major_hurricane_count)
           )

    chart_image <- paste("scatter_yearly_major_wind_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_yearly_major_wind, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_major_wind_fit <- lm( log(year_ace_major_wa$major_avg_max_ms) ~ as.numeric(year_ace_major_wa$year) )


    log_max_wind_fit_major <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.factor(hurr_meta_major_wa$year))
    log_max_wind_fit_major_stder <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.factor(hurr_meta_major_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_major <- log_max_wind_fit_major$coefficients[2:length(log_max_wind_fit_major$coefficients)]
    test_stder <- summary(log_max_wind_fit_major_stder)$coef[,2]

    write.csv(log_max_wind_fit_major, file.path(data_dir,"major_coef.csv"))
    write.csv(test_stder, file.path(data_dir,"major_stder.csv"))

    log_max_wind_fit_major <- read.csv(file.path(data_dir,"major_coef.csv"))
    log_max_wind_fit_major_stder <- read.csv(file.path(data_dir,"major_stder.csv"))

    log_max_wind_fit_major_stder = log_max_wind_fit_major_stder[-1,]

    log_max_wind_fit_major$X <- str_sub(log_max_wind_fit_major$X , -4,-1)
    log_max_wind_fit_major_stder$X <- str_sub(log_max_wind_fit_major_stder$X , -4,-1)

    log_max_wind_fit_major <- dplyr::rename(log_max_wind_fit_major, year = X, coefficient = x)
    log_max_wind_fit_major_stder <- dplyr::rename(log_max_wind_fit_major_stder, year = X, stder = x)

    log_max_wind_fit_major <- merge(x = log_max_wind_fit_major, y=log_max_wind_fit_major_stder, by=c("year") , all.x = TRUE)

    write.csv(log_max_wind_fit_major, file.path(data_dir,"major_coef.csv"))
    write.csv(log_max_wind_fit_major_stder, file.path(data_dir,"major_stder.csv"))

####

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

    #major scatter coefficients loess
    scatter_major_wind_coefficients_loess <- ggScatterAutoCoef(
      log_max_wind_fit_major,
      log_max_wind_fit_major$year,
      log_max_wind_fit_major$coefficient,
      "loess",
      "Major and Coefficients of Max Wind M/S",
      "Year",
      "Coefficients",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_major_wind_coefficients_loess_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_major_wind_coefficients_loess, width=image_width, height=image_height)

    box_yearly_major_wind <- ggplot(hurr_meta_major_wa ,aes(as.factor(hurr_meta_major_wa$year),log(hurr_meta_major_wa$max_wind_ms)) )  +
      geom_boxplot() +
      geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +

     coord_equal() +
     theme_minimal(base_size=theme_base_size) +

     labs(
       title = "Major and Max Wind M/S",
          x = "Year",
          y = "Max Wind Speed M/S",
        caption=paste("Source:","NOAA - Hurrdat2 data")) +

     theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
           aspect.ratio = 6/18,
           legend.position = "bottom",
           plot.subtitle = element_text(color="#666666"),
           plot.caption = element_text(color="#AAAAAA", size=6),
         )

     chart_image <- paste("box_wind_major_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, box_yearly_major_wind, width=image_extra_width, height=image_extra_height)

    log_max_wind_fit_major <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.numeric(hurr_meta_major_wa$year))
    log_max_wind_fit_major_stder <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.numeric(hurr_meta_major_wa$year))

    #Yearly ACE major
    scatter_yearly_major_weighted_ace <- ggScatterAutoYearlyWeight(
             year_ace_major_wa,
             as.numeric(year_ace_major_wa$year),
             log(year_ace_major_wa$major_ace),
             "lm",
             "Major and ACE - Year \nWeighted by count",
             "Storm",
             "ACE",
             "NOAA - Hurrdat2 data",
             as.numeric(year_ace_major_wa$major_hurricane_count)
           )

     chart_image <- paste("scatter_yearly_major_ace_weighted_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, scatter_yearly_major_weighted_ace, width=image_width, height=image_height)


     scatter_yearly_major_ace_weighted_fit <- lm( log(year_ace_major_wa$ace) ~ as.numeric(year_ace_major_wa$year) , weights = as.numeric(year_ace_major_wa$major_hurricane_count))


         #Yearly ACE major
         scatter_yearly_major_ace <- ggScatterAutoYearly(
                  year_ace_major_wa,
                  as.numeric(year_ace_major_wa$year),
                  log(year_ace_major_wa$major_ace),
                  "lm",
                  "Major and ACE (Year)",
                  "Storm",
                  "ACE",
                  "NOAA - Hurrdat2 data",
                  as.numeric(year_ace_major_wa$major_hurricane_count)
                )

          chart_image <- paste("scatter_yearly_major_ace_1980", "png", sep=".")
          chart_image <- file.path(charts_dir, chart_image)
          chart_image <- chart_image[1]
          chart_image <- gsub(" ", "_", chart_image)
          ggsave(chart_image, scatter_yearly_major_ace, width=image_width, height=image_height)


    #LM fit for MEAN max wind and year
    scatter_yearly_major_ace_fit <- lm( log(year_ace_major_wa$ace) ~ as.numeric(year_ace_major_wa$year))

    scatter_major_ace_fit <- lm( log(hurr_meta_major_wa$ace) ~ as.numeric(hurr_meta_major_wa$year))

    scatter_major_ace <- ggScatterAuto(
      hurr_meta_major_wa,
      as.numeric(hurr_meta_major_wa$year),
      log(hurr_meta_major_wa$ace),
      "lm",
      "Major and ACE",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_major_ace_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_major_ace, width=image_width, height=image_height)


    #LM fit for MEAN max wind and year
    scatter_major_ace_fit <- lm( log(hurr_meta_major_wa$ace) ~ as.numeric(hurr_meta_major_wa$year))


    #### hurricane

    scatter_hurricane_wind_fit <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.numeric(hurr_meta_hurricane_wa$year))

    #hurricane scatter
    scatter_hurricane_wind <- ggScatterAuto(
      hurr_meta_hurricane_wa,
      as.numeric(hurr_meta_hurricane_wa$year),
      log(hurr_meta_hurricane_wa$max_wind_ms),
      "lm",
      "Hurricanes and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_hurricane_wind_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_hurricane_wind, width=image_width, height=image_height)

    scatter_hurricane_wind_fit <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.numeric(hurr_meta_hurricane_wa$year))

    #Yearly mean wind M/S major
    scatter_yearly_hurricane_weighted_wind <- ggScatterAutoYearlyWeight(
             year_ace_hurr_wa,
             as.numeric(year_ace_hurr_wa$year),
             log(year_ace_hurr_wa$hurricane_avg_max_ms),
             "lm",
             "Hurricane Yearly Mean Max Wind M/S \nWeighted by count",
             "Storm",
             "Mean Max Wind M/S",
             "NOAA - Hurrdat2 data",
             as.numeric(year_ace_hurr_wa$hurricane_count)
           )

       chart_image <- paste("scatter_yearly_hurricane_wind_weighted_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, scatter_yearly_hurricane_weighted_wind, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_hurricane_wind_weighted_fit <- lm( log(year_ace_hurr_wa$hurricane_avg_max_ms) ~ as.numeric(year_ace_hurr_wa$year), weights = as.numeric(year_ace_hurr_wa$hurricane_count))




        #Yearly mean wind M/S major
        scatter_yearly_hurricane_wind <- ggScatterAutoYearly(
                 year_ace_hurr_wa,
                 as.numeric(year_ace_hurr_wa$year),
                 log(year_ace_hurr_wa$hurricane_avg_max_ms),
                 "lm",
                 "Hurricane Yearly Mean Max Wind M/S",
                 "Storm",
                 "Mean Max Wind M/S",
                 "NOAA - Hurrdat2 data",
                 as.numeric(year_ace_hurr_wa$hurricane_count)
               )

           chart_image <- paste("scatter_yearly_hurricane_wind_1980", "png", sep=".")
           chart_image <- file.path(charts_dir, chart_image)
           chart_image <- chart_image[1]
           chart_image <- gsub(" ", "_", chart_image)
           ggsave(chart_image, scatter_yearly_hurricane_wind, width=image_width, height=image_height)

        #LM fit for MEAN max wind and year
        scatter_yearly_hurricane_wind_fit <- lm( log(year_ace_hurr_wa$hurricane_avg_max_ms) ~ as.numeric(year_ace_hurr_wa$year))



    log_max_wind_fit_hurricane <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.factor(hurr_meta_hurricane_wa$year))
    log_max_wind_fit_hurricane_stder <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.factor(hurr_meta_hurricane_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_hurricane <- log_max_wind_fit_hurricane$coefficients[2:length(log_max_wind_fit_hurricane$coefficients)]
    test_stder <- summary(log_max_wind_fit_hurricane_stder)$coef[,2]

    write.csv(log_max_wind_fit_hurricane, file.path(data_dir,"hurricanes_coef.csv"))
    write.csv(test_stder, file.path(data_dir,"hurricanes_stder.csv"))

    log_max_wind_fit_hurricane <- read.csv(file.path(data_dir,"hurricanes_coef.csv"))
    log_max_wind_fit_hurricane_stder <- read.csv(file.path(data_dir,"hurricanes_stder.csv"))

    log_max_wind_fit_hurricane_stder = log_max_wind_fit_hurricane_stder[-1,]

    log_max_wind_fit_hurricane$X <- str_sub(log_max_wind_fit_hurricane$X , -4,-1)
    log_max_wind_fit_hurricane_stder$X <- str_sub(log_max_wind_fit_hurricane_stder$X , -4,-1)

    log_max_wind_fit_hurricane <- dplyr::rename(log_max_wind_fit_hurricane, year = X, coefficient = x)
    log_max_wind_fit_hurricane_stder <- dplyr::rename(log_max_wind_fit_hurricane_stder, year = X, stder = x)

    log_max_wind_fit_hurricane <- merge(x = log_max_wind_fit_hurricane, y=log_max_wind_fit_hurricane_stder, by=c("year") , all.x = TRUE)

    write.csv(log_max_wind_fit_hurricane, file.path(data_dir,"hurricanes_coef.csv"))
    write.csv(log_max_wind_fit_hurricane_stder, file.path(data_dir,"hurricanes_stder.csv"))

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

    #hurricane scatter coefficients loess
    scatter_hurricane_wind_coefficients_loess <- ggScatterAutoCoef(
      log_max_wind_fit_hurricane,
      log_max_wind_fit_hurricane$year,
      log_max_wind_fit_hurricane$coefficient,
      "loess",
      "Hurricanes and Coefficients of Max Wind M/S",
      "Year",
      "Coefficient ",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_hurricane_wind_coefficients_loess_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_hurricane_wind_coefficients_loess, width=image_width, height=image_height)


        box_yearly_hurricane_wind  <- ggplot(hurr_meta_hurricane_wa ,aes(as.factor(hurr_meta_hurricane_wa$year),log(hurr_meta_hurricane_wa$max_wind_ms)) )  +
          geom_boxplot() +
          geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +

         coord_equal() +
         theme_minimal(base_size=theme_base_size) +

         labs(
           title = "Hurricanes and Max Wind M/S",
              x = "Year",
              y = "Max Wind Speed M/S",
            caption=paste("Source:","NOAA - Hurrdat2 data")) +

         theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
               aspect.ratio = 6/18,
               legend.position = "bottom",
               plot.subtitle = element_text(color="#666666"),
               plot.caption = element_text(color="#AAAAAA", size=6),
             )

         chart_image <- paste("box_wind_hurricanes_1980", "png", sep=".")
         chart_image <- file.path(charts_dir, chart_image)
         chart_image <- chart_image[1]
         chart_image <- gsub(" ", "_", chart_image)
         ggsave(chart_image, box_yearly_hurricane_wind, width=image_extra_width, height=image_extra_height)


    log_max_wind_fit_hurricane <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.numeric(hurr_meta_hurricane_wa$year))
    log_max_wind_fit_hurricane_stder <- lm( log(hurr_meta_hurricane_wa$max_wind_ms) ~ as.numeric(hurr_meta_hurricane_wa$year))

    #Yearly ace hurricane
    scatter_yearly_hurricane_weighted_ace <- ggScatterAutoYearlyWeight(
             year_ace_hurr_wa,
             as.numeric(year_ace_hurr_wa$year),
             log(year_ace_hurr_wa$hurr_ace),
             "lm",
             "Hurricanes and ACE - Year \nWeighted by count",
             "Storm",
             "ACE",
             "NOAA - Hurrdat2 data",
             as.numeric(year_ace_hurr_wa$hurricane_count)
           )

     chart_image <- paste("scatter_yearly_hurricane_ace_weighted_1980", "png", sep=".")
     chart_image <- file.path(charts_dir, chart_image)
     chart_image <- chart_image[1]
     chart_image <- gsub(" ", "_", chart_image)
     ggsave(chart_image, scatter_yearly_hurricane_weighted_ace, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_yearly_hurricane_ace_weighted_fit <- lm( log(year_ace_hurr_wa$ace) ~ as.numeric(year_ace_hurr_wa$year),  weights = as.numeric(year_ace_hurr_wa$hurricane_count))



        #Yearly ace hurricane
        scatter_yearly_hurricane_ace <- ggScatterAutoYearly(
                 year_ace_hurr_wa,
                 as.numeric(year_ace_hurr_wa$year),
                 log(year_ace_hurr_wa$hurr_ace),
                 "lm",
                 "Hurricanes and ACE (Year)",
                 "Storm",
                 "ACE",
                 "NOAA - Hurrdat2 data",
                 as.numeric(year_ace_hurr_wa$hurricane_count)
               )

         chart_image <- paste("scatter_yearly_hurricane_ace_1980", "png", sep=".")
         chart_image <- file.path(charts_dir, chart_image)
         chart_image <- chart_image[1]
         chart_image <- gsub(" ", "_", chart_image)
         ggsave(chart_image, scatter_yearly_hurricane_ace, width=image_width, height=image_height)

        #LM fit for MEAN max wind and year
        scatter_yearly_hurricane_ace_fit <- lm( log(year_ace_hurr_wa$ace) ~ as.numeric(year_ace_hurr_wa$year))


    scatter_hurricane_ace_fit <- lm( log(hurr_meta_hurricane_wa$ace) ~ as.numeric(hurr_meta_hurricane_wa$year))

    scatter_hurricane_ace <- ggScatterAuto(
      hurr_meta_hurricane_wa,
      as.numeric(hurr_meta_hurricane_wa$year),
      log(hurr_meta_hurricane_wa$ace),
      "lm",
      "Hurricanes and ACE",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data"
    )

    chart_image <- paste("scatter_hurricane_ace_1980", "png", sep=".")
    chart_image <- file.path(charts_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, scatter_hurricane_ace, width=image_width, height=image_height)

    #LM fit for MEAN max wind and year
    scatter_hurricane_ace_fit <- lm( log(hurr_meta_hurricane_wa$ace) ~ as.numeric(hurr_meta_hurricane_wa$year))


      #### named
      scatter_named_wind_fit <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.numeric(hurr_meta_named_wa$year))

      #named scatter
      scatter_named_wind <- ggScatterAuto(
        hurr_meta_named_wa,
        as.numeric(hurr_meta_named_wa$year),
        log(hurr_meta_named_wa$max_wind_ms),
        "lm",
        "Named and Max Wind M/S",
        "Storm",
        "Max Wind M/S",
        "NOAA - Hurrdat2 data"
      )

      chart_image <- paste("scatter_named_wind_1980", "png", sep=".")
      chart_image <- file.path(charts_dir, chart_image)
      chart_image <- chart_image[1]
      chart_image <- gsub(" ", "_", chart_image)
      ggsave(chart_image, scatter_named_wind, width=image_width, height=image_height)

      scatter_named_wind_fit <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.numeric(hurr_meta_named_wa$year))

      #Yearly mean wind M/S major
      scatter_yearly_named_weighted_wind <- ggScatterAutoYearlyWeight(
               year_ace_named_wa,
               as.numeric(year_ace_named_wa$year),
               log(year_ace_named_wa$named_avg_max_ms),
               "lm",
               "Named Yearly Mean Max Wind M/S (weighted)",
               "Storm",
               "Mean Max Wind M/S",
               "NOAA - Hurrdat2 data",
               as.numeric(year_ace_named_wa$named_count)
             )

       chart_image <- paste("scatter_yearly_named_wind_weighted_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, scatter_yearly_named_weighted_wind, width=image_width, height=image_height)

      #LM fit for MEAN max wind and year
      scatter_yearly_named_wind_weighted_fit <- lm( log(year_ace_named_wa$named_avg_max_ms) ~ as.numeric(year_ace_named_wa$year), weights = as.numeric(year_ace_named_wa$named_count))



            #Yearly mean wind M/S major
            scatter_yearly_named_wind <- ggScatterAutoYearly(
                     year_ace_named_wa,
                     as.numeric(year_ace_named_wa$year),
                     log(year_ace_named_wa$named_avg_max_ms),
                     "lm",
                     "Named Yearly Mean Max Wind M/S",
                     "Storm",
                     "Mean Max Wind M/S",
                     "NOAA - Hurrdat2 data",
                     as.numeric(year_ace_named_wa$named_count)
                   )

             chart_image <- paste("scatter_yearly_named_wind_1980", "png", sep=".")
             chart_image <- file.path(charts_dir, chart_image)
             chart_image <- chart_image[1]
             chart_image <- gsub(" ", "_", chart_image)
             ggsave(chart_image, scatter_yearly_named_wind, width=image_width, height=image_height)

            #LM fit for MEAN max wind and year
            scatter_yearly_named_wind_fit <- lm( log(year_ace_named_wa$named_avg_max_ms) ~ as.numeric(year_ace_named_wa$year))


      log_max_wind_fit_named <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.factor(hurr_meta_named_wa$year))
      log_max_wind_fit_named_stder <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.factor(hurr_meta_named_wa$year))

      #remove overall coefficent we only want the individual years
      log_max_wind_fit_named <- log_max_wind_fit_named$coefficients[2:length(log_max_wind_fit_named$coefficients)]
      test_stder <- summary(log_max_wind_fit_named_stder)$coef[,2]

      write.csv(log_max_wind_fit_named, file.path(data_dir,"nammed_coef.csv"))
      write.csv(test_stder, file.path(data_dir,"nammed_stder.csv"))

      log_max_wind_fit_named <- read.csv(file.path(data_dir,"nammed_coef.csv"))
      log_max_wind_fit_named_stder <- read.csv(file.path(data_dir,"nammed_stder.csv"))

      log_max_wind_fit_named_stder = log_max_wind_fit_named_stder[-1,]

      log_max_wind_fit_named$X <- str_sub(log_max_wind_fit_named$X , -4,-1)
      log_max_wind_fit_named_stder$X <- str_sub(log_max_wind_fit_named_stder$X , -4,-1)

      log_max_wind_fit_named <- dplyr::rename(log_max_wind_fit_named, year = X, coefficient = x)
      log_max_wind_fit_named_stder <- dplyr::rename(log_max_wind_fit_named_stder, year = X, stder = x)

      log_max_wind_fit_named <- merge(x = log_max_wind_fit_named, y=log_max_wind_fit_named_stder, by=c("year") , all.x = TRUE)

      write.csv(log_max_wind_fit_named, file.path(data_dir,"nammed_coef.csv"))
      write.csv(log_max_wind_fit_named_stder, file.path(data_dir,"nammed_stder.csv"))

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

      #hurricane scatter coefficients loess
      scatter_named_wind_coefficients_loess <- ggScatterAutoCoef(
        log_max_wind_fit_named,
        log_max_wind_fit_named$year,
        log_max_wind_fit_named$coefficient,
        "loess",
        "Named and Coefficients of Max Wind M/S",
        "Year",
        "Coefficient",
        "NOAA - Hurrdat2 data"
      )

      chart_image <- paste("scatter_named_wind_coefficients_loess_1980", "png", sep=".")
      chart_image <- file.path(charts_dir, chart_image)
      chart_image <- chart_image[1]
      chart_image <- gsub(" ", "_", chart_image)
      ggsave(chart_image, scatter_named_wind_coefficients_loess, width=image_width, height=image_height)

      box_yearly_named_wind  <- ggplot(hurr_meta_named_wa ,aes(as.factor(hurr_meta_named_wa$year),log(hurr_meta_named_wa$max_wind_ms)) )  +
        geom_boxplot() +
        geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +

       coord_equal() +
       theme_minimal(base_size=theme_base_size) +

       labs(
         title = "Named and Max Wind M/S",
            x = "Year",
            y = "Max Wind Speed M/S",
          caption=paste("Source:","NOAA - Hurrdat2 data")) +

       theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
             aspect.ratio = 6/18,
             legend.position = "bottom",
             plot.subtitle = element_text(color="#666666"),
             plot.caption = element_text(color="#AAAAAA", size=6),
           )

       chart_image <- paste("box_wind_named_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, box_yearly_named_wind, width=image_width, height=image_height)



      log_max_wind_fit_named <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.numeric(hurr_meta_named_wa$year))
      log_max_wind_fit_named_stder <- lm( log(hurr_meta_named_wa$max_wind_ms) ~ as.numeric(hurr_meta_named_wa$year))

      #Yearly ace named
      scatter_yearly_named_weighted_ace <- ggScatterAutoYearlyWeight(
               year_ace_named_wa,
               as.numeric(year_ace_named_wa$year),
               log(year_ace_named_wa$ace),
               "lm",
               "Named and ACE - Year \nWeighted by count",
               "Storm",
               "ACE",
               "NOAA - Hurrdat2 data",
               as.numeric(year_ace_named_wa$named_count)
             )

       chart_image <- paste("scatter_yearly_named_ace_weighted_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, scatter_yearly_named_weighted_ace, width=image_width, height=image_height)

      #LM fit for aced and year
      scatter_yearly_named_ace_weighted_fit <- lm( log(year_ace_named_wa$ace) ~ as.numeric(year_ace_named_wa$year), weights = as.numeric(year_ace_named_wa$named_count))


      #Yearly ace named
      scatter_yearly_named_ace <- ggScatterAutoYearly(
               year_ace_named_wa,
               as.numeric(year_ace_named_wa$year),
               log(year_ace_named_wa$ace),
               "lm",
               "Named and ACE (Year)",
               "Storm",
               "ACE",
               "NOAA - Hurrdat2 data",
               as.numeric(year_ace_named_wa$named_count)
             )

       chart_image <- paste("scatter_yearly_named_ace_1980", "png", sep=".")
       chart_image <- file.path(charts_dir, chart_image)
       chart_image <- chart_image[1]
       chart_image <- gsub(" ", "_", chart_image)
       ggsave(chart_image, scatter_yearly_named_ace, width=image_width, height=image_height)

      #LM fit for aced and year
      scatter_yearly_named_ace_fit <- lm( log(year_ace_named_wa$ace) ~ as.numeric(year_ace_named_wa$year))


      scatter_named_ace_fit <- lm( log(hurr_meta_named_wa$ace) ~ as.numeric(hurr_meta_named_wa$year))

      scatter_named_ace <- ggScatterAuto(
        hurr_meta_named_wa,
        as.numeric(hurr_meta_named_wa$year),
        log(hurr_meta_named_wa$ace),
        "lm",
        "Named and ACE",
        "Storm",
        "ACE",
        "NOAA - Hurrdat2 data"
      )

      chart_image <- paste("scatter_named_ace_1980", "png", sep=".")
      chart_image <- file.path(charts_dir, chart_image)
      chart_image <- chart_image[1]
      chart_image <- gsub(" ", "_", chart_image)
      ggsave(chart_image, scatter_named_ace, width=image_width, height=image_height)

      #LM fit for MEAN max wind and year
      scatter_named_ace_fit <- lm( log(hurr_meta_named_wa$ace) ~ as.numeric(hurr_meta_named_wa$year))



### 3

hurr_meta_cat3 <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic"  & hurr_meta$year >= 1980 & hurr_meta$max_category == 3 )
#named scatter hurr_meta_cat3_wind <-
hurr_meta_cat3_wind <- ggScatterAuto(
  hurr_meta_cat3,
  as.numeric(hurr_meta_cat3$year),
  log(hurr_meta_cat3$max_wind_ms),
  "lm",
  "Category 3 and Max Wind M/S",
  "Storm",
  "Max Wind M/S",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("scatter_cat3_wind_1980", "png", sep=".")
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, hurr_meta_cat3_wind, width=image_width, height=image_height)
#
#
hurr_meta_cat4 <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic"  & hurr_meta$year >= 1980 & hurr_meta$max_category == 4 )

 hurr_meta_cat4_wind <- ggScatterAuto(
  hurr_meta_cat4,
  as.numeric(hurr_meta_cat4$year),
  log(hurr_meta_cat4$max_wind_ms),
  "lm",
  "Category 4 and Max Wind M/S",
  "Storm",
  "Max Wind M/S",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("scatter_cat4_wind_1980", "png", sep=".")
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, hurr_meta_cat4_wind, width=image_width, height=image_height)


###
      summary(scatter_intense_wind_fit)
      summary(scatter_yearly_intense_wind_fit)
      summary(scatter_yearly_intense_wind_weighted_fit)
      summary(log_max_wind_fit_intense)
      summary(scatter_yearly_intense_ace_fit)
      summary(scatter_yearly_intense_ace_weighted_fit)
      summary(scatter_intense_ace_fit)

      #
      summary(gls( log(max_wind_ms) ~ as.numeric(year) , data=hurr_meta_intense_wa))
      summary(gls( log(ace) ~ as.numeric(year) , data=hurr_meta_intense_wa))
      summary(gls( ace ~ as.numeric(year) , data=hurr_meta_intense_wa))
      summary(gls(log(intense_ace) ~ (year),  weights = ~intense_hurricane_count, data=year_ace_intense_wa))
      summary(gls(log(major_ace) ~ (year),  weights = ~major_hurricane_count, data= year_ace_major_wa))

      summary(scatter_major_wind_fit)
      summary(scatter_yearly_major_wind_fit)
      summary(scatter_yearly_major_wind_weighted_fit)
      summary(log_max_wind_fit_major)
      summary(scatter_yearly_major_ace_fit)
      summary(scatter_yearly_major_ace_weighted_fit)
      summary(scatter_major_ace_fit)

      summary(scatter_hurricane_wind_fit)
      summary(scatter_yearly_hurricane_wind_fit)
      summary(scatter_yearly_hurricane_wind_weighted_fit)
      summary(log_max_wind_fit_hurricane)
      summary(scatter_yearly_hurricane_ace_fit)
      summary(scatter_yearly_hurricane_ace_weighted_fit)
      summary(scatter_hurricane_ace_fit)

      summary(scatter_named_wind_fit)
      summary(scatter_yearly_named_wind_fit)
      summary(scatter_yearly_named_wind_weighted_fit)
      summary(log_max_wind_fit_named)
      summary(scatter_yearly_named_ace_fit)
      summary(scatter_yearly_named_ace_weighted_fit)
      summary(scatter_named_ace_fit)
