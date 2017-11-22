is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
}

#insall if not installed
if (!is.installed("stringr")){ install.packages("stringr") }
if (!is.installed("dplyr")){ install.packages("dplyr") }
if (!is.installed("ggplot2")){ install.packages("ggplot2") }
if (!is.installed("tidyr")){ install.packages("tidyr") }
if (!is.installed("lubridate")){ install.packages("lubridate") }
if (!is.installed("ggmap")){ install.packages("ggmap") }
if (!is.installed("maptools")){ install.packages("maptools") }
if (!is.installed("maps")){ install.packages("maps") }
if (!is.installed("maptools")){ install.packages("maptools") }
if (!is.installed("sp")){ install.packages("sp") }
if (!is.installed("leaflet")){ install.packages("leaflet") }

#add refrence to library
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggmap)
library(maptools)
library(maps)
library(sp)
library(leaflet)
options(scipen = 999)
