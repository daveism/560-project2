# 560-project2
Final Project for stats 560

## Getting Started
First, you will have to [clone](https://github.com/daveism/560-prez) or [download](https://github.com/daveism/560-prez/archive/master.zip) this repository.

Open R studio and do the following:

### Update Hurdat2
If the Hurdat2 data has changed, and the scripts fail, you may need to update the locations of the two files in [get_data.R](get_data.R#L7-L8).  


### Update repository
You will also need to update the location of the downloaded or cloned repository here [run_hurr.R](run_hurr.R#L1).  

Once updated, copy and paste the first 14 lines so the interactive pieces on this guide work properly [run_hurr.R](https://github.com/daveism/560-project2/blob/master/run_hurr.R#L1:L14).

```r
repo_loc <- "/Update to location of the repository on your computer/560-prez"

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

```

### Run the R scripts
In R studio run

Get the hurdat2 data from the NOAA website, clean it up, and reformat it for analysis
```r
source(paste(repo_loc,"run_hurr.R", sep = "/"))
```

Run all final paper data and charts
```r
source(paste(repo_loc,"final_report_charts.R", sep = "/"))
```


***NOTE when NOAA updates the hurdat2 data after this year the hurdat2 file names WILL CHANGE and this will need to be updated - [get_data.R](get_data.R)***
