#############################
###Hurricane winds by storm
#############################

basin_meta_hurr <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$year >= 2000)
basin_obs_hurr <- subset(hurr_obs, hurr_obs$basin == "Western Atlantic" & hurr_obs$year >= 2000)

basin_meta_hurr <- arrange(basin_meta_hurr,desc(num_id))
basin_obs_hurr <- arrange(basin_obs_hurr,desc(num_id))

for (huricane in basin_obs_hurr$storm_id){
  # print(huricane)
  hur <- subset(hurr_obs, hurr_obs$storm_id == huricane )

  bar_time_winds <- ggBarTime(
    hur,
    paste( paste("Winds speeds for", hur$storm_name)),
    hur$date_time,
    format(hur$wind_mph, digits = 0),
    "Date Time",
    "Wind (MPH)",
    "NOAA - Hurrdat2 data"
  )


    chart_image <- paste( hur$basin, paste(hur$storm_name, hur$storm_id, paste("wind", "png", sep="."), sep="_" ), sep="_" )
    chart_image <- file.path(charts_storm_dir, chart_image)
    chart_image <- chart_image[1]
    chart_image <- gsub(" ", "_", chart_image)
    ggsave(chart_image, bar_time_winds, width=image_width, height=image_height, type = "cairo-png")
    rm(bar_time_winds)

}

rm(hur)
rm(basin_meta_hurr)
rm(basin_obs_hurr)
