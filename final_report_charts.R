#year data
year_ace_named_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980 & !is.na(year_ace$ace))
year_ace_hurr_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$hurr_ace))
year_ace_major_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$major_ace))
year_ace_intense_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$intense_ace))

cols <- c("Named Storms" = "gray", "Hurricanes" = "yellow", "Major Hurricanes" = "darkorange2","Intense Hurricanes" = "firebrick1")
cols2 <- c("Named" = "gray", "Hurricanes" = "yellow", "Major" = "darkorange2","Intense" = "firebrick1")

year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic")
singlecols <- c("All Storms" = "darkorange2")

#ACE for all all stroms all years
ggplot() +
   geom_line(data=year_ace_wa, aes(x=year, y=ace, group = 1, colour="All Storms")) +
   geom_point(data=year_ace_wa, aes(x=year, y=ace, group = 1, size=ace), color="darkorange2", alpha=3/4) +
    labs(size = "ACE", color = "Storms" ) +

   geom_smooth(data=year_ace_wa, aes(x=year, y=ace, group = 1), method = "lm",color="darkorange3",se=0) +
    scale_colour_manual( values = singlecols) +

   coord_equal() +
   theme_minimal(base_size=theme_base_size) +
   labs(
     title = "ACE Score's since 1850",
        x = "Year",
        y = "ACE",
      caption=paste("Source:","NOAA - Hurrdat2 data")) +

   theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
         aspect.ratio = 8/12,
         legend.position = "bottom",
         legend.key.width = unit(1,"line"),
         legend.key.height = unit(1,"line"),
         plot.subtitle = element_text(color="#666666"),
         plot.caption = element_text(color="#AAAAAA", size=6),
         legend.text=element_text(size=6),
         legend.title=element_text(size=6)
       )

#Yearly ACE
b_breaks = c("1980", "1985", "1990", "1995", "2000",  "2005", "2010", "2015" )
b_labels = c("1980", "1985", "1990", "1995", "2000",  "2005", "2010", "2015" )


ggplot() +
   #lines
   geom_line(data=year_ace_named_wa, aes(x=as.factor(year), y=ace, group = 1, colour="Named")) +
   geom_line(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurr_ace, group = 2, colour="Hurricanes")) +
   geom_line(data=year_ace_major_wa, aes(x=as.factor(year), y=major_ace, group = 3, colour="Major")) +
   geom_line(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_ace, group = 4, colour="Intense")) +
    scale_colour_manual( values = cols2) +

   #points
   geom_point(data=year_ace_named_wa, aes(x=as.factor(year), y=ace, group = 1, size=ace), color="gray") +
   geom_point(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurr_ace, group = 2, size=ace), color="yellow") +
   geom_point(data=year_ace_major_wa, aes(x=as.factor(year), y=major_ace, group = 3, size=ace), color="darkorange2") +
   geom_point(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_ace, group = 4, size=ace), color="firebrick1") +
    labs(size = "ACE", color = "Storm" ) +
    scale_x_discrete(breaks = b_breaks,labels = b_labels) +

   #Smoothed Lines
   geom_smooth(data=year_ace_named_wa, aes(x=as.factor(year), y=ace, group = 1), method = "lm",color="darkgray",se=0) +
   geom_smooth(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurr_ace, group = 2), method = "lm",color="yellow2",se=0) +
   geom_smooth(data=year_ace_major_wa, aes(x=as.factor(year), y=major_ace, group = 3), method = "lm",color="darkorange3",se=0) +
   geom_smooth(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_ace, group = 4), method = "lm",color="firebrick",se=0) +

   coord_equal() +
   theme_minimal(base_size=theme_base_size) +

   labs(
     title = "ACE Score's since 1980",
        x = "Year",
        y = "ACE",
      caption=paste("Source:","NOAA - Hurrdat2 data")) +

   theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
         aspect.ratio = 8/12,
         legend.position = "bottom",
         legend.key.width = unit(1,"line"),
         legend.key.height = unit(1,"line"),
         plot.subtitle = element_text(color="#666666"),
         plot.caption = element_text(color="#AAAAAA", size=6),
         legend.text=element_text(size=6),
         legend.title=element_text(size=6)
       )



 #Yearly Mean Max Wind Speed
 ggplot() +

    #lines
    geom_line(data=year_ace_named_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1, colour="Named")) +
    geom_line(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2, colour="Hurricanes")) +
    geom_line(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3, colour="Major")) +
    geom_line(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4, colour="Intense")) +
     scale_colour_manual( values = cols2) +
     guides(fill=guide_legend(ncol=2)) +

    #points
    geom_point(data=year_ace_named_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1, size=named_avg_max_ms), color="gray") +
    geom_point(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2, size=hurricane_avg_max_ms), color="yellow") +
    geom_point(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3, size=major_avg_max_ms), color="darkorange2") +
    geom_point(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4, size=intense_avg_max_ms), color="firebrick1") +
     labs(size = "M/S", color = "Storm" ) +
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
          aspect.ratio = 8/12,
          legend.position = "bottom",
          legend.key.width = unit(1,"line"),
          legend.key.height = unit(1,"line"),
          plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size=6),
          legend.text=element_text(size=6),
          legend.title=element_text(size=6)
        )

#storm data
hurr_meta_named_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic"  & hurr_meta$year >= 1980 & hurr_meta$named == 1)
hurr_meta_hurricane_wa  <- subset(hurr_meta_wa, hurr_meta_wa$basin == "Western Atlantic"  & hurr_meta_wa$year >= 1980 & hurr_meta_wa$hurricane == 1)
hurr_meta_major_wa <- subset(hurr_meta_wa, hurr_meta_wa$basin == "Western Atlantic"  & hurr_meta_wa$year >= 1980 & hurr_meta_wa$hurricane_major == 1 )
hurr_meta_intense_wa <- subset(hurr_meta_wa, hurr_meta_wa$basin == "Western Atlantic"  & hurr_meta_wa$year >= 1980 & hurr_meta_wa$hurricane_intense == 1)


hurr_meta_intense_wa_all <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$hurricane_intense == 1)
log_max_wind_fit_intense <- lm( hurr_meta_intense_wa_all$max_wind_ms ~ as.factor(hurr_meta_intense_wa_all$year))

#remove overall coefficent we only want the individual years
log_max_wind_fit_intense <- log_max_wind_fit_intense$coefficients[2:length(log_max_wind_fit_intense$coefficients)]
write.csv(log_max_wind_fit_intense, file.path(data_dir,"coef.csv"))
log_max_wind_fit_intense <- read.csv(file.path(data_dir,"coef.csv"))
log_max_wind_fit_intense$X <- str_sub(log_max_wind_fit_intense$X , -4,-1)
log_max_wind_fit_intense <- dplyr::rename(log_max_wind_fit_intense, year = X, coefficient = x)

#intense scatter coefficients
scatter_intense_wind_coefficients <- ggScatterAutoCoef(
  log_max_wind_fit_intense,
  log_max_wind_fit_intense$year,
  log_max_wind_fit_intense$coefficient,
  "lm",
  "ALL Intense Storms and Max Wind M/S",
  "Year",
  "Coefficient ",
  "NOAA - Hurrdat2 data"
)

    #### INTENSE
    #intense scatter
    scatter_intense_wind <- ggScatterAuto(
      hurr_meta_intense_wa,
      as.numeric(hurr_meta_intense_wa$year),
      log(hurr_meta_intense_wa$max_wind_ms),
      "lm",
      "Intense Hurricanes and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data"
    )

    log_max_wind_fit_intense <- lm( hurr_meta_intense_wa$max_wind_ms ~ as.factor(hurr_meta_intense_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_intense <- log_max_wind_fit_intense$coefficients[2:length(log_max_wind_fit_intense$coefficients)]
    write.csv(log_max_wind_fit_intense, file.path(data_dir,"coef.csv"))
    log_max_wind_fit_intense <- read.csv(file.path(data_dir,"coef.csv"))
    log_max_wind_fit_intense$X <- str_sub(log_max_wind_fit_intense$X , -4,-1)
    log_max_wind_fit_intense <- dplyr::rename(log_max_wind_fit_intense, year = X, coefficient = x)

    #intense scatter coefficients
    scatter_intense_wind_coefficients <- ggScatterAutoCoef(
      log_max_wind_fit_intense,
      log_max_wind_fit_intense$year,
      log_max_wind_fit_intense$coefficient,
      "lm",
      "Intense Hurricanes and Max Wind M/S",
      "Year",
      "Coefficient ",
      "NOAA - Hurrdat2 data"
    )

    scatter_intense_ace <- ggScatterAuto(
      hurr_meta_intense_wa,
      hurr_meta_intense_wa$year,
      hurr_meta_intense_wa$ace,
      "lm",
      "Intense Hurricanes and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data"
    )


    #### major
    #major scatter
    scatter_major_wind <- ggScatterAuto(
      hurr_meta_major_wa,
      hurr_meta_major_wa$year,
      log(hurr_meta_major_wa$max_wind_ms),
      "lm",
      "Major Hurricanes and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data"
    )

    log_max_wind_fit_major <- lm( hurr_meta_major_wa$max_wind_ms ~ as.factor(hurr_meta_major_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_major <- log_max_wind_fit_major$coefficients[2:length(log_max_wind_fit_major$coefficients)]
    write.csv(log_max_wind_fit_major, file.path(data_dir,"coef.csv"))
    log_max_wind_fit_major <- read.csv(file.path(data_dir,"coef.csv"))
    log_max_wind_fit_major$X <- str_sub(log_max_wind_fit_major$X , -4,-1)
    log_max_wind_fit_major <- dplyr::rename(log_max_wind_fit_major, year = X, coefficient = x)

    #major scatter coefficients
    scatter_major_wind_coefficients <- ggScatterAutoCoef(
      log_max_wind_fit_major,
      log_max_wind_fit_major$year,
      log_max_wind_fit_major$coefficient,
      "lm",
      "Major Hurricanes and Max Wind M/S",
      "Year",
      "Coefficient ",
      "NOAA - Hurrdat2 data"
    )

    scatter_major_ace_y <- ggScatterAuto(
      hurr_meta_major_wa,
      hurr_meta_major_wa$year,
      hurr_meta_major_wa$ace,
      "lm",
      "Major Hurricanes and ACE Score",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data"
    )

    scatter_major_ace <- ggScatterAutoNum(
      hurr_meta_major_wa,
      hurr_meta_major_wa$num_id,
      hurr_meta_major_wa$ace,
      "lm",
      "Major Hurricanes and ACE Score",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data"
    )


    #### hurricane
    #hurricane scatter
    scatter_hurricane_wind <- ggScatterAuto(
      hurr_meta_hurricane_wa,
      hurr_meta_hurricane_wa$year,
      log(hurr_meta_hurricane_wa$max_wind_ms),
      "lm",
      "Hurricanes and Max Wind M/S",
      "Storm",
      "Max Wind M/S",
      "NOAA - Hurrdat2 data"
    )

    log_max_wind_fit_hurricane <- lm( hurr_meta_hurricane_wa$max_wind_ms ~ as.factor(hurr_meta_hurricane_wa$year))

    #remove overall coefficent we only want the individual years
    log_max_wind_fit_hurricane <- log_max_wind_fit_hurricane$coefficients[2:length(log_max_wind_fit_hurricane$coefficients)]
    write.csv(log_max_wind_fit_hurricane, file.path(data_dir,"coef.csv"))
    log_max_wind_fit_hurricane <- read.csv(file.path(data_dir,"coef.csv"))
    log_max_wind_fit_hurricane$X <- str_sub(log_max_wind_fit_hurricane$X , -4,-1)
    log_max_wind_fit_hurricane <- dplyr::rename(log_max_wind_fit_hurricane, year = X, coefficient = x)

    #hurricane scatter coefficients
    scatter_hurricane_wind_coefficients <- ggScatterAutoCoef(
      log_max_wind_fit_hurricane,
      log_max_wind_fit_hurricane$year,
      log_max_wind_fit_hurricane$coefficient,
      "lm",
      "Hurricanes and Max Wind M/S",
      "Year",
      "Coefficient ",
      "NOAA - Hurrdat2 data"
    )

    scatter_hurricane_ace <- ggScatterAuto(
      hurr_meta_hurricane_wa,
      hurr_meta_hurricane_wa$year,
      hurr_meta_hurricane_wa$ace,
      "lm",
      "Hurricanes and ACE Score",
      "Storm",
      "ACE",
      "NOAA - Hurrdat2 data"
    )


      #### named
      #named scatter
      scatter_named_wind <- ggScatterAuto(
        hurr_meta_named_wa,
        as.numeric(hurr_meta_named_wa$year),
        log(hurr_meta_named_wa$max_wind_ms),
        "lm",
        "Nammed Storms and Max Wind M/S",
        "Storm",
        "Max Wind M/S",
        "NOAA - Hurrdat2 data"
      )

      log_max_wind_fit_named <- lm( hurr_meta_named_wa$max_wind_ms ~ as.factor(hurr_meta_named_wa$year))

      #remove overall coefficent we only want the individual years
      log_max_wind_fit_named <- log_max_wind_fit_named$coefficients[2:length(log_max_wind_fit_named$coefficients)]
      write.csv(log_max_wind_fit_named, file.path(data_dir,"coef.csv"))
      log_max_wind_fit_named <- read.csv(file.path(data_dir,"coef.csv"))
      log_max_wind_fit_named$X <- str_sub(log_max_wind_fit_named$X , -4,-1)
      log_max_wind_fit_named <- dplyr::rename(log_max_wind_fit_named, year = X, coefficient = x)

      #hurricane scatter coefficients
      scatter_named_wind_coefficients <- ggScatterAutoCoef(
        log_max_wind_fit_named,
        log_max_wind_fit_named$year,
        log_max_wind_fit_named$coefficient,
        "lm",
        "Nammed Storms and Max Wind M/S",
        "Year",
        "Coefficient ",
        "NOAA - Hurrdat2 data"
      )

      scatter_named_ace <- ggScatterAutoCoef(
        hurr_meta_named_wa,
        hurr_meta_named_wa$year,
        hurr_meta_named_wa$ace,
        "lm",
        "Nammed Storms and ACE Score",
        "Storm",
        "ACE",
        "NOAA - Hurrdat2 data"
      )
