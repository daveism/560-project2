#############################
###Intense Hurricane Charts by individual basins
#############################

major_meta_hurr <- subset(hurr_meta, hurr_meta$max_category >= 3)
major_meta_hurr_sort <- arrange(major_meta_hurr,num_id)

major_obs_hurr <- subset(hurr_obs, hurr_obs$max_category >= 3)
major_obs_sort <- arrange(major_obs_hurr,num_id)

basins <- unique(major_meta_hurr_sort$basin)

for (thebasin in basins){
  basin_meta_hurr <- subset(major_meta_hurr_sort, major_meta_hurr_sort$basin == thebasin )
  basin_obs_hurr <- subset(major_obs_sort, major_obs_sort$basin == thebasin )

  basin_meta_hurr <- arrange(basin_meta_hurr,num_id)
  basin_obs_hurr <- arrange(basin_obs_hurr,num_id)

  #year wind
  Chart_ScatterYearWindmajor <- ggScatterAutoNoR(basin_meta_hurr,
    basin_meta_hurr$num_id,
    basin_meta_hurr$max_wind_mph,
    "lm",
    paste("Major Hurricanes and Max Wind\n", thebasin, sep = " - "),
    "Storm",
    "Max Wind MPH",
    "NOAA - Hurrdat2 data"
  )

  chart_image <- paste(thebasin, paste("scatter_major_storm_wind", "png", sep="."), sep="_" )
  chart_image <- file.path(charts_dir, chart_image)
  chart_image <- chart_image[1]
  chart_image <- gsub(" ", "_", chart_image)
  ggsave(chart_image, Chart_ScatterYearWindmajor, width=image_width, height=image_height)

  #year wind
  Chart_ScatterYearWindmajor_zoom <- ggScatterAutoNoRLimMajor(basin_meta_hurr,
    basin_meta_hurr$num_id,
    basin_meta_hurr$max_wind_mph,
    "lm",
    paste("Major Hurricanes and Max Wind\n", thebasin, sep = " - "),
    "Storm",
    "Max Wind MPH",
    "NOAA - Hurrdat2 data"
  )

  chart_image <- paste(thebasin, paste("scatter_major_storm_wind_zoom", "png", sep="."), sep="_" )
  chart_image <- file.path(charts_dir, chart_image)
  chart_image <- chart_image[1]
  chart_image <- gsub(" ", "_", chart_image)
  ggsave(chart_image, Chart_ScatterYearWindmajor_zoom, width=image_width, height=image_height)


  Chart_BarYearWindmajor <- ggBarMaxAll(
    basin_meta_hurr,
    paste("Major Hurricanes Max Wind by Storm\n", thebasin, sep = " - "),
    basin_meta_hurr$num_id,
    basin_meta_hurr$max_wind_mph,
    "Storm",
    "Max wind (MPH)",
    "NOAA - Hurrdat2 data"
  )
  chart_image <- paste(thebasin, paste("bar_major_storm_maxwind", "png", sep="."), sep="_" )
  chart_image <- file.path(charts_dir, chart_image)
  chart_image <- chart_image[1]
  chart_image <- gsub(" ", "_", chart_image)
  ggsave(chart_image, Chart_BarYearWindmajor, width=image_width, height=image_height)


  #pressure vs wind
  chartWindvsPressuremajor <- ggScatterAuto(basin_obs_hurr,
    basin_obs_hurr$pressure,
    basin_obs_hurr$wind_mph,
    "lm",
    paste("Pressure and Wind\n", thebasin, sep = " - "),
    "Pressure",
    "Wind MPH",
    "NOAA - Hurrdat2 data"
  )

  chart_image <- paste(thebasin, paste("scatter_major_pressure_wind", "png", sep="."), sep="_" )
  chart_image <- file.path(charts_dir, chart_image)
  chart_image <- chart_image[1]
  chart_image <- gsub(" ", "_", chart_image)
  ggsave(chart_image, chartWindvsPressuremajor, width=image_width, height=image_height)

}


rm(major_meta_hurr)
rm(major_meta_hurr_sort)
rm(major_obs_hurr)
rm(major_obs_sort)
rm(basins)
rm(basin_meta_hurr)
rm(basin_obs_hurr)
rm(Chart_ScatterYearWindmajor)
rm(Chart_ScatterYearWindmajor_zoom)
rm(Chart_BarYearWindmajor)
rm(chartWindvsPressuremajor)
