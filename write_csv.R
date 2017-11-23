write.csv(hurr_meta, file.path(data_dir,"hurricanes_summary.csv"))
write.csv(hurr_obs, file.path(data_dir,"hurricanes_detail.csv"))
write.csv(year_ace, file.path(data_dir,"hurricanes_yearly_ace.csv"))
