# all the documentation on the hurdat data is at
# http://www.aoml.noaa.gov/hrd/hurdat/newhurdat-format.pdf
# data is in very crazy hard to read format so can't just use read.csv
# Here we work through some really great ways to convert the data
# borrowed from  https://geanders.github.io/RProgrammingForResearch/entering-and-cleaning-data-3.html

create_meta_data <- function(hurr_tracks, basin, basinid){

  # split data into arrays by commas
  hurr_tracks <- lapply(hurr_tracks, str_split, pattern = ",", simplify = TRUE)

  # get lengths of all the fields
  hurr_lengths <- sapply(hurr_tracks, length)

  # there two longths in the file 4 - which is the id, storm name, and number of obsevatiosn (which reall helps deal with the crazy format)
  hurr_meta <- hurr_tracks[hurr_lengths == 4]

  # prepare the hurrican meta data from conversion to data frame
  hurr_meta <- lapply(hurr_meta, tibble::as_tibble)

  # make the data a dataframe so we can do all the R stuff
  hurr_meta <- bind_rows(hurr_meta)

  # we have extrax fields so delete it
  hurr_meta <- dplyr::select(hurr_meta,-V4)

  # rename the fields to something meaningfull
  hurr_meta <- dplyr::rename(hurr_meta, storm_id = V1, storm_name = V2, n_obs = V3)

  # get rid of spaces (trailing and leading in storm name make observatons numeric)
  hurr_meta <- dplyr::mutate(hurr_meta, storm_name = str_trim(storm_name), n_obs = as.numeric(n_obs))
  hurr_meta$basin <- basin
  hurr_meta$basinid <- basinid
  hurr_meta$year <- str_sub(hurr_meta$storm_id, -4, -1)

  hurr_meta$num_id <- as.numeric(
      paste(
          paste(str_sub(hurr_meta$storm_id, -4, -1),
          str_sub(hurr_meta$storm_id, -6, -5), sep = ""),
        hurr_meta$basinid,  sep=".")
  )

  # get the number of observatons and repeat the storm id that number of observatons times
  # this will allow us to add the storm_id to the storm observatons line and this add
  # storm name
  storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)
  storm_name <- rep(hurr_meta$storm_name, times = hurr_meta$n_obs)

  # hurr_meta <- arrange(hurr_meta,desc(storm_id))

  return(hurr_meta)

}

create_obs_data <- function(hurr_tracks, hurr_meta, basin, basinid){

  # split data into arrays by commas
  hurr_tracks <- lapply(hurr_tracks, str_split, pattern = ",", simplify = TRUE)

  # get lengths of all the fields
  hurr_lengths <- sapply(hurr_tracks, length)

  # there two longths in the file 4 - which is the id, storm name, and number of obsevatiosn (which reall helps deal with the crazy format)
  hurr_obs <- hurr_tracks[hurr_lengths == 21]

  # get the number of observatons and repeat the storm id that number of observatons times
  # this will allow us to add the storm_id to the storm observatons line and this add
  # storm name
  storm_id <- rep(hurr_meta$storm_id, times = hurr_meta$n_obs)
  storm_name <- rep(hurr_meta$storm_name, times = hurr_meta$n_obs)
  storm_name <- rep(hurr_meta$storm_name, times = hurr_meta$n_obs)

  # prepare the hurrican obervation data from conversion to data frames
  hurr_obs <- lapply(hurr_obs, tibble::as_tibble)
  hurr_obs <- dplyr::bind_rows(hurr_obs) %>%
  dplyr::mutate(storm_id = storm_id,storm_name = storm_name)

  #add the storm id
  # hurr_obs <- dplyr::mutate(hurr_obs,storm_id = storm_id)

  #add storm name
  # hurr_obs <- dplyr::mutate(hurr_obs,storm_name = storm_name)

  hurr_obs$basin <- basin
  hurr_obs$basinid <- basinid
  hurr_obs$num_id <- as.numeric(
      paste(
          paste(str_sub(hurr_obs$storm_id, -4, -1),
          str_sub(hurr_obs$storm_id, -6, -5), sep = ""),
        hurr_obs$basinid,  sep=".")
  )


  #rename fields to something meaningfull
  hurr_obs <- dplyr::rename(hurr_obs, date = V1, record_indentifer = V3, time = V2, status = V4, latitude = V5,longitude = V6, wind_knts = V7, pressure = V8)
  hurr_obs <- dplyr::rename(hurr_obs, radii_34_ne  = V9, radii_34_se  = V10, radii_34_sw  = V11, radii_34_nw  = V12)
  hurr_obs <- dplyr::rename(hurr_obs, radii_50_ne  = V13, radii_50_se  = V14, radii_50_sw  = V15, radii_50_nw  = V16)
  hurr_obs <- dplyr::rename(hurr_obs, radii_64_ne  = V17, radii_64_se  = V18, radii_64_sw  = V19, radii_64_nw  = V20)

  #get rid of blank fields
  hurr_obs <- dplyr::select(hurr_obs, -V21)

  #add new fields for coded fields
  hurr_obs$status_code <- hurr_obs$status
  hurr_obs$record_indentifer_code <- hurr_obs$record_indentifer

  #prepare coded fields to add a new field description
  storm_levels <- c("TD",
                    "TS",
                    "HU",
                    "EX",
                    "SD",
                    "SS",
                    "LO",
                    "WV",
                    "DB")
  storm_labels <- c("Tropical depression",
                    "Tropical storm",
                    "Hurricane",
                    "Extratropical cyclone",
                    "Subtropical depression",
                    "Subtropical storm",
                    "Other low",
                    "Tropical wave",
                    "Disturbance")

  ri_ids <- c("L",
              "W",
              "P",
              "I",
              "C",
              "S",
              "G",
              "T")
  ri_labels <- c("Landfall (center of system crossing a coastline)",
                  "Maximum sustained wind speed",
                  "Minimum in central pressure",
                  "An intensity peak in terms of both pressure and wind",
                  "Closest approach to a coast, not followed by a landfall",
                  "Change of status of the system",
                  "Genesis",
                  "Provides additional detail on the track (position) of the cyclone")


  #replace with real values and fill nulls with NA
  hurr_obs <- dplyr::mutate(hurr_obs, status = factor(str_trim(status), levels = storm_levels,labels = storm_labels))
  hurr_obs <- dplyr::mutate(hurr_obs, record_indentifer = factor(str_trim(record_indentifer),levels = ri_ids,labels = ri_labels))

  #fix lat long to numbers and make directions sepreate field
  hurr_obs <- dplyr::mutate(hurr_obs,lat_dir = str_extract(latitude, "[A-Z]"),
      latitude = as.numeric(str_extract(latitude, "[^A-Z]+")),
      lon_dir = str_extract(longitude, "[A-Z]"),
      longitude = as.numeric(str_extract(longitude, "[^A-Z]+")))

  #make wind numeric
  hurr_obs <-  dplyr::mutate(hurr_obs, wind_knts = ifelse(wind_knts == " -99", NA, as.numeric(wind_knts)))


  #bar chart of wind_knts by year
  #ggplot(hurr_obs, aes(x = wind_knts)) + geom_histogram(binwidth = 10)

  #fix pressure
  hurr_obs  <- dplyr::mutate(hurr_obs, pressure = ifelse(pressure == " -999", NA, as.numeric(pressure)))

  #fix radii's 34
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_34_ne = ifelse(radii_34_ne == " -999", NA, as.numeric(radii_34_ne)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_34_se = ifelse(radii_34_se == " -999", NA, as.numeric(radii_34_se)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_34_sw = ifelse(radii_34_sw == " -999", NA, as.numeric(radii_34_sw)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_34_nw = ifelse(radii_34_nw == " -999", NA, as.numeric(radii_34_nw)))

  #fix radii's 50
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_50_ne = ifelse(radii_50_ne == " -999", NA, as.numeric(radii_50_ne)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_50_se = ifelse(radii_50_se == " -999", NA, as.numeric(radii_50_se)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_50_sw = ifelse(radii_50_sw == " -999", NA, as.numeric(radii_50_sw)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_50_nw = ifelse(radii_50_nw == " -999", NA, as.numeric(radii_50_nw)))

  #fix radii's 64
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_64_ne = ifelse(radii_64_ne == " -999", NA, as.numeric(radii_64_ne)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_64_se = ifelse(radii_64_se == " -999", NA, as.numeric(radii_64_se)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_64_sw = ifelse(radii_64_sw == " -999", NA, as.numeric(radii_64_sw)))
  hurr_obs  <- dplyr::mutate(hurr_obs, radii_64_nw = ifelse(radii_64_nw == " -999", NA, as.numeric(radii_64_nw)))

  #add data time field make data formated date field
  hurr_obs$date_time <- ymd_hm(paste(hurr_obs$date, hurr_obs$time))
  hurr_obs$date <- ymd(hurr_obs$date)




  # add decade
  hurr_obs$decade <- paste0(substring(year(hurr_obs$date_time), 1, 3), "0s")

  hurr_obs$year <- year(hurr_obs$date_time)

  #add saffir simpson scale
  hurr_obs$category <- ifelse(hurr_obs$wind_knts >= 137, "5",
                       ifelse(hurr_obs$wind_knts >= 113 & hurr_obs$wind_knts <= 136, "4",
                       ifelse(hurr_obs$wind_knts >= 96 & hurr_obs$wind_knts <= 112, "3",
                       ifelse(hurr_obs$wind_knts >= 83 & hurr_obs$wind_knts <= 95, "2",
                       ifelse(hurr_obs$wind_knts >= 64 & hurr_obs$wind_knts <= 82, "1", NA)))))

  hurr_obs$category <- as.numeric(hurr_obs$category)

  #add mph
  hurr_obs$wind_mph <- as.numeric(format(as.numeric((hurr_obs$wind_knts * 6076)/5280), digits = 0))

  #add ms or meters per second
  hurr_obs$meters_per_second <- as.numeric(as.numeric(hurr_obs$wind_knts) * 0.5144)

  return(hurr_obs)
}


append_meta_data <- function(hurr_obs, hurr_meta){

  storm_max_wind <- aggregate(x=hurr_obs$wind_mph, by=list(hurr_obs$storm_id),FUN=max, na.rm=TRUE, na.action=NULL)
  storm_max_wind_ms <- aggregate(x=hurr_obs$meters_per_second, by=list(hurr_obs$storm_id),FUN=max, na.rm=TRUE, na.action=NULL)
  storm_min_pressure <- aggregate(x=hurr_obs$pressure, by=list(hurr_obs$storm_id),FUN=min, na.rm=TRUE, na.action=NULL)
  storm_max_category <- aggregate(x=hurr_obs$category, by=list(hurr_obs$storm_id),FUN=max, na.rm=TRUE, na.action=NULL)

  storm_max_wind[mapply(is.infinite, storm_max_wind)] <- NA
  storm_max_wind_ms[mapply(is.infinite, storm_max_wind_ms)] <- NA
  storm_min_pressure[mapply(is.infinite, storm_min_pressure)] <- NA
  storm_max_category[mapply(is.infinite, storm_max_category)] <- NA

  storm_max_wind <- dplyr::rename(storm_max_wind,  storm_id = Group.1, max_wind_mph = x)
  storm_max_wind_ms <- dplyr::rename(storm_max_wind_ms,  storm_id = Group.1, max_wind_ms = x)
  storm_min_pressure <- dplyr::rename(storm_min_pressure,  storm_id = Group.1, min_pressure = x)
  storm_max_category <- dplyr::rename(storm_max_category,  storm_id = Group.1, max_category = x)

  storm_max_category$max_category <- as.numeric(storm_max_category$max_category)

  hurr_meta <- merge(x = hurr_meta, y=storm_max_wind, by=c("storm_id") , all.x = TRUE)
  hurr_meta <- merge(x = hurr_meta, y=storm_max_wind_ms, by=c("storm_id") , all.x = TRUE)
  hurr_meta <- merge(x = hurr_meta, y=storm_min_pressure, by=c("storm_id") , all.x = TRUE)
  hurr_meta <- merge(x = hurr_meta, y=storm_max_category, by=c("storm_id") , all.x = TRUE)

  hurr_obs <- merge(x = hurr_obs, y=storm_max_category, by=c("storm_id") , all.x = TRUE)

 return(hurr_meta)

}


append_obs_data <- function(hurr_obs, hurr_meta){

  storm_max_wind <- aggregate(x=hurr_obs$wind_mph, by=list(hurr_obs$storm_id),FUN=max, na.rm=TRUE, na.action=NULL)
  storm_max_wind_ms <- aggregate(x=hurr_obs$meters_per_second, by=list(hurr_obs$storm_id),FUN=max, na.rm=TRUE, na.action=NULL)
  storm_min_pressure <- aggregate(x=hurr_obs$pressure, by=list(hurr_obs$storm_id),FUN=min, na.rm=TRUE, na.action=NULL)
  storm_max_category <- aggregate(x=hurr_obs$category, by=list(hurr_obs$storm_id),FUN=max, na.rm=TRUE, na.action=NULL)

  storm_max_wind[mapply(is.infinite, storm_max_wind)] <- NA
  storm_max_wind_ms[mapply(is.infinite, storm_max_wind_ms)] <- NA
  storm_min_pressure[mapply(is.infinite, storm_min_pressure)] <- NA
  storm_max_category[mapply(is.infinite, storm_max_category)] <- NA

  storm_max_wind <- dplyr::rename(storm_max_wind,  storm_id = Group.1, max_wind_mph = x)
  storm_max_wind_ms <- dplyr::rename(storm_max_wind_ms,  storm_id = Group.1, max_wind_ms = x)
  storm_min_pressure <- dplyr::rename(storm_min_pressure,  storm_id = Group.1, min_pressure = x)
  storm_max_category <- dplyr::rename(storm_max_category,  storm_id = Group.1, max_category = x)

  storm_max_category$max_category <- as.numeric(storm_max_category$max_category)

  hurr_obs <- merge(x = hurr_obs, y=storm_max_wind, by=c("storm_id") , all.x = TRUE)
  hurr_obs <- merge(x = hurr_obs, y=storm_max_wind_ms, by=c("storm_id") , all.x = TRUE)
  hurr_obs <- merge(x = hurr_obs, y=storm_min_pressure, by=c("storm_id") , all.x = TRUE)
  hurr_obs <- merge(x = hurr_obs, y=storm_max_category, by=c("storm_id") , all.x = TRUE)

 return(hurr_obs)

}
