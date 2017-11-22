repo_loc <- "/Users/daveism/GitHub/560-project2"

maps_dir <- paste(repo_loc,"maps", sep = "/")
maps_storm_dir <- paste(repo_loc,"maps","storm", sep = "/")
charts_dir <- paste(repo_loc,"charts", sep = "/")
charts_storm_dir <- paste(repo_loc,"charts","storm", sep = "/")
data_dir <- paste(repo_loc,"data", sep = "/")

#image defaults
theme_base_size <- 9
image_width <- 4
image_xwidth <- 5
image_height <- 3

#install packages and ref libraries
source(paste(repo_loc,"install.R", sep = "/"))

#source the functions
source(paste(repo_loc,"clean_data_functions.R", sep = "/"))

#get datasets
source(paste(repo_loc,"get_data.R", sep = "/"))

#add map functions
source(paste(repo_loc,"map_functions.R", sep = "/"))

#add chart functions
source(paste(repo_loc,"chart_functions.R", sep = "/"))
source(paste(repo_loc,"make_storm_datasets.R", sep = "/"))

#write csvs
source(paste(repo_loc,"write_csv.R", sep = "/"))

#charts
source(paste(repo_loc,"charts_all.R", sep = "/"))
source(paste(repo_loc,"charts_basins_all.R", sep = "/"))
source(paste(repo_loc,"charts_all_1950.R", sep = "/"))
source(paste(repo_loc,"charts_basins_all_1950.R", sep = "/"))
source(paste(repo_loc,"charts_intense_all.R", sep = "/"))
source(paste(repo_loc,"charts_intense_all_basins.R", sep = "/"))
