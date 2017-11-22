#############################
###Hurricane Charts all basins
#############################
hurr_meta_sort <- arrange(hurr_meta,num_id)
hurr_obs_sort <- arrange(hurr_obs,num_id)

#pressure vs wind
chartWindvsPressure <- ggScatterAuto(hurr_obs_sort,
  hurr_obs_sort$pressure,
  hurr_obs_sort$wind_mph,
  "lm",
  "Pressure and Wind",
  "Pressure",
  "Wind MPH",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("all", paste("scatter_pressure_and_wind", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, chartWindvsPressure, width=image_width, height=image_height)

#storm vs wind
allyearwind <- ggScatterAutoNoR(hurr_meta_sort,
  hurr_meta_sort$num_id,
  hurr_meta_sort$max_wind_mph,
  "lm",
  "Storm and Max Wind",
  "Storm",
  "Max Wind MPH",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("all", paste("scatter_strom_wind", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, allyearwind, width=image_xwidth, height=image_height)

#storm vs wind
allyearwindZoom <- ggScatterAutoNoRLim(hurr_meta_sort,
  hurr_meta_sort$num_id,
  hurr_meta_sort$max_wind_mph,
  "lm",
  "Storm and Max Wind",
  "Storm",
  "Max Wind MPH",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("all", paste("scatter_strom_wind_zoom", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, allyearwindZoom, width=image_xwidth, height=image_height)

BarYearWind <- ggBarMaxAll(
  hurr_meta_sort,
  paste("Max Wind by Storm", ""),
  hurr_meta_sort$num_id,
  hurr_meta_sort$max_wind_mph,
  "Storm",
  "Max wind (MPH)",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("All", paste("bar_storm_maxwind", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, BarYearWind, width=image_width, height=image_height)


rm(hurr_meta_sort)
rm(hurr_obs_sort)
rm(chartWindvsPressure)
rm(allyearwind)
rm(allyearwindZoom)
