make_hurricane_track_maps <- function(hur, tittle, source){

  storm_max_wind <- hur$max_wind_mph
  storm_min_pressure <- hur$min_pressure
  storm_max_category <- hur$max_category

  storm_min_date <- aggregate(x=hur$date, by=list(hur$storm_id),FUN=min)
  storm_max_date <- aggregate(x=hur$date, by=list(hur$storm_id),FUN=max)

  storm_min_date <- dplyr::rename(storm_min_date,  storm_id = Group.1, min_date = x)
  storm_max_date <- dplyr::rename(storm_max_date,  storm_id = Group.1, max_date = x)

  storm_min_date$min_date <- format(as.Date(storm_min_date$min_date), format='%b %d, %Y')
  storm_max_date$max_date <- format(as.Date(storm_max_date$max_date), format='%b %d, %Y')

  storm_max_category[mapply(is.infinite, storm_max_category)] <- "N/A"

  storm_max_windst <- paste("Max Wind: ", format(storm_max_wind, digits = 3))
  storm_min_pressurest <- paste("Min pressure: ", format(storm_min_pressure, digits = 3))
  storm_max_categoryt <- paste("Max category: ", storm_max_category )
  storm_datet <- paste("Date: ", paste(storm_min_date$min_date, storm_max_date$max_date , sep=" - ") )

  the_subtitle <- paste(storm_datet, storm_max_windst,  storm_min_pressurest, storm_max_categoryt, sep="\n")

  storm_max_lat <- aggregate(x=hur$latitude, by=list(hur$storm_id),FUN=max)
  storm_max_lat <- as.numeric(storm_max_lat$x)

  storm_min_lat <- aggregate(x=hur$latitude, by=list(hur$storm_id),FUN=min)
  storm_min_lat <- as.numeric(storm_min_lat$x)

  storm_max_long <- aggregate(x=hur$longitude, by=list(hur$storm_id),FUN=max)
  storm_max_long <- as.numeric(storm_max_long$x)

  storm_min_long <- aggregate(x=hur$longitude, by=list(hur$storm_id),FUN=min)
  storm_min_long <- as.numeric(storm_min_long$x)


  xmin <- -storm_max_long
  xmax <- -storm_min_long
  ymin <- storm_min_lat
  ymax <- storm_max_lat
  buff <- 15

  worldmap = map_data ("world")

  hur$map_status <- ifelse(is.na(hur$category), as.character(hur$status), paste(hur$status, "category", hur$category ))

  storm_color <- c(
        # "" = "#367b7f",
        "Other low" = "#60999e",
        "Tropical depression" = "#4B8A8E",
        "Tropical storm" = "#367b7f",
        "Subtropical storm" = alpha("#367b7f", .8),
        "Extratropical cyclone" = alpha("#367b7f", .8),
        "Extratropical cyclone category 1" = alpha("#f29d69", .8),
        "Hurricane category 1" = "#f29d69",
        "Hurricane category 2" = "#e0713e",
        "Hurricane category 3" = "#D86136",
        "Hurricane category 4" = "#D0512E",
        "Hurricane category 5" = "#c94227"
      )

  storm_breaks <- c(
        # "" = "#367b7f",
        "Other low",
        "Tropical depression",
        "Tropical storm",
        "Subtropical storm",
        "Extratropical cyclone",
        "Extratropical cyclone category 1",
        "Hurricane category 1",
        "Hurricane category 2",
        "Hurricane category 3",
        "Hurricane category 4",
        "Hurricane category 5"
      )

  ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  )

  wrld <- c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "gray50", fill="cornsilk", alpha=0.8, data=worldmap))
  track_map <- ggplot() +
    coord_cartesian(xlim = c((xmin-buff),(xmax+buff)), ylim = c((ymin-buff),(ymax+buff))) +
    wrld  +
    theme(panel.background = element_rect(fill='lightblue')) +
    geom_path(data = hur,
              aes(x = -longitude, y = latitude,
                  group = num_id,
                  color = map_status),
                  size = 1,
            linejoin = "mitre", lineend = "round") +
     scale_color_manual(breaks = storm_breaks, values = storm_color) +
     labs(color="Storm Level")  +
   theme_minimal(base_size=theme_base_size) +
   ditch_the_axes +
   theme(panel.background = element_rect(fill='lightblue')) +
   labs(x = NULL,
         y = NULL,
     title = tittle,
      subtitle=the_subtitle,
      caption=paste("Source:",source))+
      theme(legend.title=element_text(size=4),
            legend.text=element_text(size=3),
            legend.key.width=unit(.25,"line"),
            legend.position="right",
            legend.box="horizontal",
            legend.direction="vertical",
            plot.subtitle = element_text(size=5,color="#666666"),
            plot.caption = element_text(color="#AAAAAA", size=5),
            panel.border = element_rect(colour = "grey60", fill=NA, size=1))

  rm(hur)
  return(track_map)

}
