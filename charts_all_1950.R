#############################
###Hurricane Charts all basins after 1950
#############################
hurr_meta_sort <- arrange(hurr_meta,num_id)
hurr_obs_sort <- arrange(hurr_obs,num_id)

meta_hurr_1950 <- subset(hurr_meta_sort, hurr_meta_sort$year >= 1950)

#storm vs wind
allyearwind_1950 <- ggScatterAutoNoR(meta_hurr_1950,
  meta_hurr_1950$num_id,
  meta_hurr_1950$max_wind_mph,
  "lm",
  "Storm and Max Wind \n Since 1950",
  "Storm",
  "Max Wind MPH",
  "NOAA - Hurrdat2 data"
)

chart_image <- paste("all", paste("scatter_strom_wind_1950", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, allyearwind_1950, width=image_xwidth, height=image_height)

Chart_BarYearWind_1950 <- ggBarMaxAll(
  meta_hurr_1950,
  paste("Hurricanes Max Wind by Storm Since 1950 \n", sep = " - "),
  meta_hurr_1950$num_id,
  meta_hurr_1950$max_wind_mph,
  "Storm",
  "Max wind (MPH)",
  "NOAA - Hurrdat2 data"
)
chart_image <- paste("All", paste("bar_storm_maxwind_1950", "png", sep="."), sep="_" )
chart_image <- file.path(charts_dir, chart_image)
chart_image <- chart_image[1]
chart_image <- gsub(" ", "_", chart_image)
ggsave(chart_image, Chart_BarYearWind_1950, width=image_xwidth, height=image_height)

rm(hurr_meta_sort)
rm(hurr_obs_sort)
rm(meta_hurr_1950)
rm(Chart_BarYearWind_1950)
rm(allyearwind_1950)
