#############################
###Intense Hurrucicanes
#############################

major_meta_hurr <- subset(hurr_meta, hurr_meta$max_category >= 3)
major_meta_hurr_sort <- arrange(major_meta_hurr,num_id)

major_obs_hurr <- subset(hurr_obs, hurr_obs$max_category >= 3)
major_obs_sort <- arrange(major_obs_hurr,num_id)

#pressure vs wind
chartWindvsPressure <- ggScatterAuto(major_obs_sort,
  major_obs_sort$pressure,
  major_obs_sort$wind_mph,
  "lm",
  "Major Hurricanes Pressure and Wind",
  "Pressure",
  "Wind MPH",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("all", paste("scatter_major_pressure_wind", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, chartWindvsPressure, width=image_width, height=image_height)


#storm vs wind
allyearwindmajor <- ggScatterAutoNoR(major_meta_hurr_sort,
  major_meta_hurr_sort$num_id,
  major_meta_hurr_sort$max_wind_mph,
  "lm",
  "Major Hurricanes and Max Wind",
  "Storm",
  "Max Wind MPH",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("all", paste("scatter_major_strom_wind", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, allyearwindmajor, width=image_xwidth, height=image_height)

#storm vs wind
allyearwindZoommajor  <- ggScatterAutoNoRLimMajor(major_meta_hurr_sort,
  major_meta_hurr_sort$num_id,
  major_meta_hurr_sort$max_wind_mph,
  "lm",
  "Major Hurricanes and Max Wind",
  "Storm",
  "Max Wind MPH",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("all", paste("scatter_major_strom_wind_zoom", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, allyearwindZoommajor , width=image_xwidth, height=image_height)


BarYearWindmajor <- ggBarMaxAll(
  major_meta_hurr_sort,
  paste("Major Hurricanes Max Wind by Storm", ""),
  major_meta_hurr_sort$num_id,
  major_meta_hurr_sort$max_wind_mph,
  "Storm",
  "Max wind (MPH)",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("All", paste("bar_major_storm_max_wind", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, BarYearWindmajor, width=image_width, height=image_height)

rm(major_meta_hurr)
rm(major_meta_hurr_sort)
rm(major_obs_hurr)
rm(major_obs_sort)

rm(chartWindvsPressure)
rm(allyearwindmajor)
rm(allyearwindZoommajor)
rm(BarYearWindmajor)
