#############################
###Hurricane tracks by storm
#############################

basin_meta_hurr <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$year >= 2000)
basin_obs_hurr <- subset(hurr_obs, hurr_obs$basin == "Western Atlantic" & hurr_obs$year >= 2000)

basin_meta_hurr <- arrange(basin_meta_hurr,desc(num_id))
basin_obs_hurr <- arrange(basin_obs_hurr,desc(num_id))

for (huricane in basin_meta_hurr$storm_id){
  # print(huricane)
  hur <- subset(basin_obs_hurr, basin_obs_hurr$storm_id == huricane )

  title <- paste(hur$storm_name[1])
  map_track <- make_hurricane_track_maps(hur, title, "NOAA - Hurrdat2 data")

  # basinname <- gsub(" ", "_", basin)
  storm_name_id <- paste( hur$storm_name, hur$storm_id, sep = "_")
  # image_name_1 <- paste(basinname , storm_name_id, sep = "_")
  image_name <- paste(storm_name_id , "track_map", sep = "_")

  map_image <- paste(image_name, "png", sep=".")
  map_image <- file.path(maps_storm_dir, map_image)
  map_image <- map_image[1]
  chart_image <- gsub(" ", "_", map_image)
  ggsave(map_image, map_track, width=image_width, height=image_height)
  rm(map_image)

}

rm(basin_meta_hurr)
rm(basin_obs_hurr)
