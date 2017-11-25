
year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic")
g <- ggplot(year_ace_wa, aes(year))
g + geom_bar()

hurr_meta_intense_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$max_category >= 4 & hurr_meta$year >= 1975)

#over all
#ace chart
ggplot(hurr_meta_intense_wa, aes(year, ace)) +
  geom_smooth(aes(x=year, y=ace, group = 1) , method = "lm",color="#008fd5",se=0) +
  geom_point(aes(size=ace, color=as.character(max_category)), alpha=3/4) +
  scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
  ggtitle("Intense Hurricanes by ACE score ") +
  coord_equal() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)

ggplot(hurr_meta_intense_wa, aes(year, max_wind_mph)) +
  geom_smooth(aes(x=year, y=max_wind_mph, group = 1) , method = "lm",color="#008fd5",se=0) +
  geom_point(aes(size=max_wind_mph, color=as.character(max_category)), alpha=3/4) +
  scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
  labs(size = "ACE Score", color = "Category" ) +

  ggtitle("Intense Hurricanes by Max Wind in Mph") +
  coord_equal() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)


  ggplot(hurr_meta_intense_wa, aes(year, log(max_wind_ms))) +
    geom_smooth(aes(x=year, y=log(max_wind_ms), group = 1) , method = "lm",color="#008fd5",se=0) +
    geom_point(aes(size=log(max_wind_ms), color=as.character(max_category)), alpha=3/4) +
    scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
    labs(size = "ACE Score", color = "Category" ) +
    ggtitle("Intense Hurricanes by Max Wind in M/S") +
    coord_equal() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)



  ggplot(hurr_meta_intense_wa, aes(year, max_wind_ms)) +
    geom_point(aes(size=max_wind_ms), color="blue", alpha=3/4) +
    ggtitle("Intense Hurricanes by wind M/S") +
    coord_equal() +
    theme_bw()

year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic" )

ggplot(data=year_ace_wa, aes(x=year, y=ace, group = 1)) +
   geom_line() +
   geom_smooth(method = "lm",color="#008fd5",se=0) +
   geom_point()

   ggplot(year_ace_wa, aes(year, ace)) +
     geom_smooth(aes(x=year, y=ace, group = 1) , method = "lm",color="#008fd5",se=0) +
     geom_point(aes(size=ace, color=as.character(max_category)), alpha=3/4) +
     # scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
     labs(size = "ACE Score", color = "Category" ) +
     ggtitle("Intense Hurricanes by ACE score ") +
     coord_equal() +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)

     year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic" & max_category >= 4 & year > 1975)

     ggplot(year_ace_wa, aes(year, intense_ace)) +
       geom_smooth(aes(x=year, y=intense_ace, group = 1) , method = "lm",color="#008fd5",se=0) +
       geom_point(aes(size=intense_ace, color=as.character(max_category)), alpha=3/4) +
       # scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
       labs(size = "ACE Score", color = "Category" ) +
       ggtitle("Intense Hurricanes by ACE score ") +
       coord_equal() +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)

       ggplot(year_ace_wa, aes(year, max_wind_ms)) +
         geom_smooth(aes(x=year, y=max_wind_ms, group = 1) , method = "lm",color="#008fd5",se=0) +
         geom_point(aes(size=max_wind_ms, color=as.character(max_category)), alpha=3/4) +
         # scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
         ggtitle("Intense Hurricanes by max wind M/S") +
         coord_equal() +
         theme_bw() +
         theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)



     year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic" & max_category >= 3 & year > 1975)

     ggplot(year_ace_wa, aes(year, intense_ace)) +
       geom_smooth(aes(x=year, y=intense_ace, group = 1) , method = "lm",color="#008fd5",se=0) +
       geom_point(aes(size=intense_ace, color=as.character(max_category)), alpha=3/4) +
       # scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
       ggtitle("Intense Hurricanes by ACE score ") +
       coord_equal() +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)

       ggplot(year_ace_wa, aes(year, log(max_wind_ms))) +
         geom_smooth(aes(x=year, y=log(max_wind_ms), group = 1) , method = "lm",color="#008fd5",se=0) +
         geom_point(aes(size=log(max_wind_ms), color=as.character(max_category)), alpha=3/4) +
         # scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
         ggtitle("Intense Hurricanes by max wind M/S ") +
         coord_equal() +
         theme_bw() +
         theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)




   ggplot(hurr_

   ggplot(data=year_ace_wa, aes(x=year, y=log(intense_avg_max_ms), group = 1)) +
      geom_line() +
      geom_smooth(method = "lm",color="#008fd5",se=0) +
      geom_point()

  ggplot(data=year_ace_wa, aes(x=year, y=max_wind_ms, group = 1)) +
     geom_line() +
     geom_smooth(method = "lm",color="#008fd5",se=0) +
     geom_point()

   ggplot(data=year_ace_wa, aes(x=year, y=min_pressure, group = 1)) +
      geom_line() +
      geom_smooth(method = "lm",color="#008fd5",se=0) +
      geom_point()

   ggplot(data=year_ace_wa, aes(x=year, y=max_category, group = 1)) +
      geom_line() +
      geom_smooth(method = "lm",color="#008fd5",se=0) +
      geom_point()

  ggplot(data=year_ace_wa, aes(x=year, y=max_ace, group = 1)) +
     geom_line() +
     geom_smooth(method = "lm",color="#008fd5",se=0) +
     geom_point()

recent_ace <- subset(year_ace, year_ace$basin == "Western Atlantic" & year_ace$year >= 1975)

ggplot(data=recent_ace, aes(x=year, y=ace, group = 1)) +
   geom_line() +
   geom_smooth(method = "lm",color="#008fd5",se=0) +
   geom_point()

   ggplot(data=recent_ace, aes(x=year, y=log(intense_avg_max_ms), group = 1)) +
      geom_line() +
      geom_smooth(method = "lm",color="#008fd5",se=0) +
      geom_point()

  ggplot(data=recent_ace, aes(x=year, y=max_wind_ms, group = 1)) +
     geom_line() +
     geom_smooth(method = "lm",color="#008fd5",se=0) +
     geom_point()

   ggplot(data=recent_ace, aes(x=year, y=min_pressure, group = 1)) +
      geom_line() +
      geom_smooth(method = "lm",color="#008fd5",se=0) +
      geom_point()

   ggplot(data=recent_ace, aes(x=year, y=max_category, group = 1)) +
      geom_line() +
      geom_smooth(method = "lm",color="#008fd5",se=0) +
      geom_point()

  ggplot(data=recent_ace, aes(x=year, y=max_ace, group = 1)) +
     geom_line() +
     geom_smooth(method = "lm",color="#008fd5",se=0) +
     geom_point()


yearly_ace_fit <- lm( recent_ace$ace ~ recent_ace$year)
summary(yearly_ace_fit)

#do for  ms and mph too
aceyearscatter <-  ggScatterAuto(recent_ace,
              as.numeric(recent_ace$year),
              recent_ace$ace,
                 "lm",
                 "Major Hurricanes and ACE",
                 "Year",
                 "ACE",
                 "NOAA - Hurrdat2 data"
)

aceyearscatter

yearly_ace_fit <- lm( recent_ace$ ~ recent_ace$year)
summary(yearly_ace_fit)

#do for  ms and mph too
aceyearscatter <-  ggScatterAuto(recent_ace,
              as.numeric(recent_ace$year),
              recent_ace$ace,
                 "lm",
                 "Major Hurricanes and ACE",
                 "Year",
                 "ACE",
                 "NOAA - Hurrdat2 data"
)

aceyearscatter


#how to find our more about how year time really effects really intense storms hae to do more than one record per year factor year and log wind to see if really intense storms are chaninge
#all hurricanes in western atlantic from 1960
hurr_meta_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" ) #& hurr_meta$year >= 1960)

log_max_wind_fit <- lm( hurr_meta_wa$max_wind_ms ~ as.factor(hurr_meta_wa$year))

#remove overall coefficent we only want the individual years
coef_log_max_wind_fit <- log_max_wind_fit$coefficients[2:length(log_max_wind_fit$coefficients)]
write.csv(coef_log_max_wind_fit, file.path(data_dir,"coef.csv"))
coef_log_max_wind_fit <- read.csv(file.path(data_dir,"coef.csv"))
coef_log_max_wind_fit$X <- str_sub(coef_log_max_wind_fit$X , -4,-1)
coef_log_max_wind_fit <- dplyr::rename(coef_log_max_wind_fit, year = X, coefficient = x)

#plot(coef_log_max_wind_fit$coefficient)

ggplot(data=coef_log_max_wind_fit, aes(x=year, y=coefficient, group = 1)) +
   geom_line() +
   geom_smooth(method = "lm",color="#008fd5",se=0) +
   geom_point()






       hurr_meta_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$year >= 1950)

       log_max_wind_fit <- lm( hurr_meta_wa$max_wind_ms ~ as.factor(hurr_meta_wa$year))

       #remove overall coefficent we only want the individual years
       coef_log_max_wind_fit <- log_max_wind_fit$coefficients[2:length(log_max_wind_fit$coefficients)]
       write.csv(coef_log_max_wind_fit, file.path(data_dir,"coef.csv"))
       coef_log_max_wind_fit <- read.csv(file.path(data_dir,"coef.csv"))
       coef_log_max_wind_fit$X <- str_sub(coef_log_max_wind_fit$X , -4,-1)
       coef_log_max_wind_fit <- dplyr::rename(coef_log_max_wind_fit, year = X, coefficient = x)

       #plot(coef_log_max_wind_fit$coefficient)

       ggplot(data=coef_log_max_wind_fit, aes(x=year, y=coefficient, group = 1)) +
          geom_line() +
          geom_smooth(method = "lm",color="#008fd5",se=0) +
          geom_point() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))




#intense
#intense hurricanes (cat 4-5) in western atlantic from 1960
hurr_meta_intense_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$max_category >=4) # & hurr_meta$year >= 1980)


log_max_wind_intense_fit <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.factor(hurr_meta_intense_wa$year))
coef_log_max_wind_intense_fit<- log_max_wind_intense_fit$coefficients[2:length(log_max_wind_intense_fit$coefficients)]
write.csv(coef_log_max_wind_intense_fit, file.path(data_dir,"coef.csv"))
coef_log_max_wind_intense_fit <- read.csv(file.path(data_dir,"coef.csv"))
coef_log_max_wind_intense_fit$X <- str_sub(coef_log_max_wind_intense_fit$X , -4,-1)
coef_log_max_wind_intense_fit <- dplyr::rename(coef_log_max_wind_intense_fit, year = X, coefficient = x)

#plot(coef_log_max_wind_intense_fit$coefficient)

ggplot(data=coef_log_max_wind_intense_fit, aes(x=year, y=coefficient, group = 1)) +
   geom_line() +
   geom_smooth(method = "lm",color="#008fd5", se=TRUE, level = 0.95) +
   geom_point()


#redo for after 1980 for dovark methodolgy
#all hurricanes in western atlantic from 1975
hurr_meta_intense_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$year >= 1980)

log_max_wind_fit <- lm( hurr_meta_wa$max_wind_ms ~ as.factor(hurr_meta_wa$year))

log_max_wind_intense_fit <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.factor(hurr_meta_intense_wa$year))
coef_log_max_wind_intense_fit<- log_max_wind_intense_fit$coefficients[2:length(log_max_wind_intense_fit$coefficients)]
write.csv(coef_log_max_wind_intense_fit, file.path(data_dir,"coef.csv"))
coef_log_max_wind_intense_fit <- read.csv(file.path(data_dir,"coef.csv"))
coef_log_max_wind_intense_fit$X <- str_sub(coef_log_max_wind_intense_fit$X , -4,-1)
coef_log_max_wind_intense_fit <- dplyr::rename(coef_log_max_wind_intense_fit, year = X, coefficient = x)

#plot(coef_log_max_wind_intense_fit$coefficient)

ggplot(data=coef_log_max_wind_intense_fit, aes(x=year, y=coefficient, group = 1)) +
   geom_line() +
   geom_smooth(method = "lm",color="#008fd5", se=TRUE, level = 0.95) +
   geom_point()




   #redo for after 1980 for dovark methodolgy
   #all hurricanes in western atlantic from 1975
   hurr_meta_intense_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$max_category >= 4)

   log_max_wind_fit <- lm( hurr_meta_wa$max_wind_ms ~ as.factor(hurr_meta_wa$year))

   log_max_wind_intense_fit <- lm( log(hurr_meta_intense_wa$max_wind_ms) ~ as.factor(hurr_meta_intense_wa$year))
   coef_log_max_wind_intense_fit<- log_max_wind_intense_fit$coefficients[2:length(log_max_wind_intense_fit$coefficients)]
   write.csv(coef_log_max_wind_intense_fit, file.path(data_dir,"coef.csv"))
   coef_log_max_wind_intense_fit <- read.csv(file.path(data_dir,"coef.csv"))
   coef_log_max_wind_intense_fit$X <- str_sub(coef_log_max_wind_intense_fit$X , -4,-1)
   coef_log_max_wind_intense_fit <- dplyr::rename(coef_log_max_wind_intense_fit, year = X, coefficient = x)

   #plot(coef_log_max_wind_intense_fit$coefficient)

   ggplot(data=coef_log_max_wind_intense_fit, aes(x=year, y=coefficient, group = 1)) +
      geom_line() +
      geom_smooth(method = "lm",color="#008fd5", se=TRUE, level = 0.95) +
      geom_point()



#ACE all hurricanes in western atlantic from 1960
ace_fit <- lm( log(hurr_meta_wa$ace) ~ as.factor(hurr_meta_wa$year))

#remove overall coefficent we only want the individual years
coef_ace_fit <- ace_fit$coefficients[2:length(ace_fit$coefficients)]

plot(ace_fit$coefficients[2:length(ace_fit$coefficients)])


#ACE intense hurricanes in western atlantic from 19
hurr_meta_intense_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$max_category >= 4 & hurr_meta$year >= 1980)

ace_intense_fit <- lm( log(hurr_meta_intense_wa$ace) ~ as.factor(hurr_meta_intense_wa$year))
coef_ace_intense_fit  <- ace_intense_fit$coefficients[2:length(ace_intense_fit$coefficients)]

#plot(coef_ace_intense_fit)

write.csv(coef_ace_intense_fit, file.path(data_dir,"coef.csv"))
coef_ace_intense_fit <- read.csv(file.path(data_dir,"coef.csv"))
coef_ace_intense_fit$X <- str_sub(coef_ace_intense_fit$X , -4,-1)
coef_ace_intense_fit <- dplyr::rename(coef_ace_intense_fit, year = X, coefficient = x)

#plot(coef_log_max_wind_intense_fit$coefficient)

ggplot(data=coef_ace_intense_fit, aes(x=year, y=coefficient, group = 1)) +
   geom_line() +
   geom_smooth(method = "lm",color="#008fd5", se=TRUE, level = 0.95) +
   geom_point()


#ACE intense hurricanes in western atlantic from 19
hurr_meta_intense_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$max_category >= 4 & hurr_meta$year >= 1980)

ace_intense_fit <- lm( log(hurr_meta_intense_wa$ace) ~ as.factor(hurr_meta_intense_wa$year))
coef_ace_intense_fit  <- ace_intense_fit$coefficients[2:length(ace_intense_fit$coefficients)]

#plot(coef_ace_intense_fit)

write.csv(coef_ace_intense_fit, file.path(data_dir,"coef.csv"))
coef_ace_intense_fit <- read.csv(file.path(data_dir,"coef.csv"))
coef_ace_intense_fit$X <- str_sub(coef_ace_intense_fit$X , -4,-1)
coef_ace_intense_fit <- dplyr::rename(coef_ace_intense_fit, year = X, coefficient = x)

#plot(coef_log_max_wind_intense_fit$coefficient)

ggplot(data=coef_ace_intense_fit, aes(x=year, y=coefficient, group = 1)) +
   geom_line() +
   geom_smooth(method = "lm",color="#008fd5", se=TRUE, level = 0.95) +
   geom_point()


   boxplot(log(hurr_meta$max_wind_ms) ~ as.factor(hurr_meta$year))


   year_allyears_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & !is.na(year_ace$ace))
   year_allyears_ace_hurr_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & !is.na(year_ace$hurr_ace))
   year_allyears_ace_major_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"    & !is.na(year_ace$major_ace))
   year_allyears_ace_intense_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"    & !is.na(year_ace$intense_ace))

   ggplot() +
      geom_line(data=year_allyears_ace_wa, aes(x=year, y=named_count, group = 1, colour="Named Storms")) +
      geom_line(data=year_allyears_ace_hurr_wa, aes(x=year, y=hurricane_count, group = 2, colour="Hurricanes")) +
      geom_line(data=year_allyears_ace_major_wa, aes(x=year, y=major_hurricane_count, group = 3, colour="Major Hurricanes")) +
      geom_line(data=year_allyears_ace_intense_wa, aes(x=year, y=intense_hurricane_count, group = 4, colour="Intense Hurricanes"))

      ggplot() +
         geom_line(data=year_ace_named_wa, aes(x=year, y=named_count, group = 1, colour="Named Storms")) +
         geom_line(data=year_ace_hurr_wa, aes(x=year, y=hurricane_count, group = 2, colour="Hurricanes")) +
         geom_line(data=year_ace_major_wa, aes(x=year, y=major_hurricane_count, group = 3, colour="Major Hurricanes")) +
         geom_line(data=year_ace_intense_wa, aes(x=year, y=intense_hurricane_count, group = 4, colour="Intense Hurricanes"))


### report?
year_ace_named_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980 & !is.na(year_ace$ace))
year_ace_hurr_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$hurr_ace))
year_ace_major_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$major_ace))
year_ace_intense_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= 1980   & !is.na(year_ace$intense_ace))


cols <- c("Named Storms" = "gray", "Hurricanes" = "khaki1", "Major Hurricanes" = "darkorange2","Intense Hurricanes" = "firebrick1")

ggplot() +
   geom_line(data=year_ace_wa, aes(x=year, y=ace, group = 1, colour="Named Storms")) +
   geom_line(data=year_ace_hurr_wa, aes(x=year, y=hurr_ace, group = 2, colour="Hurricanes")) +
   geom_line(data=year_ace_major_wa, aes(x=year, y=major_ace, group = 3, colour="Major Hurricanes")) +
   geom_line(data=year_ace_intense_wa, aes(x=year, y=intense_ace, group = 4, colour="Intense Hurricanes")) +
    scale_colour_manual( values = cols) +

   geom_smooth(data=year_ace_wa, aes(x=year, y=ace, group = 1), method = "lm",color="darkgray",se=0) +
   geom_point(data=year_ace_wa, aes(x=year, y=ace, group = 1), color="gray") +

   # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
   geom_smooth(data=year_ace_hurr_wa, aes(x=year, y=hurr_ace, group = 2), method = "lm",color="khaki3",se=0) +
   geom_point(data=year_ace_hurr_wa, aes(x=year, y=hurr_ace, group = 2), color="khaki1") +

   # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
   geom_smooth(data=year_ace_major_wa, aes(x=year, y=major_ace, group = 3), method = "lm",color="darkorange3",se=0) +
   geom_point(data=year_ace_major_wa, aes(x=year, y=major_ace, group = 3), color="darkorange2") +

   geom_smooth(data=year_ace_intense_wa, aes(x=year, y=intense_ace, group = 4), method = "lm",color="firebrick",se=0) +
   geom_point(data=year_ace_intense_wa, aes(x=year, y=intense_ace, group = 4), color="firebrick1") +

   ggtitle("Yearly Total ACE Score by Storm Intensities since 1980") +
   coord_equal() +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12, legend.position="right")















ggplot() +
   geom_line(data=year_ace_wa, aes(x=as.factor(year), y=max_wind_ms, group = 1, size=2,colour="Named Storms")) +
   geom_line(data=year_ace_hurr_wa, aes(x=as.factor(year), y=max_wind_ms, group = 2, colour="Hurricanes")) +
   geom_line(data=year_ace_major_wa, aes(x=as.factor(year), y=max_wind_ms, group = 3, colour="Major Hurricanes")) +
   geom_line(data=year_ace_intense_wa, aes(x=as.factor(year), y=max_wind_ms, group = 4, colour="Intense Hurricanes")) +
    scale_colour_manual( values = cols) +

   geom_smooth(data=year_ace_wa, aes(x=as.factor(year), y=max_wind_ms, group = 1), method = "lm",color="darkgray",se=0) +
   geom_point(data=year_ace_wa, aes(x=as.factor(year), y=max_wind_ms, group = 1), color="gray") +

   # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
   geom_smooth(data=year_ace_hurr_wa, aes(x=as.factor(year), y=max_wind_ms, group = 2), method = "lm",color="khaki3",se=0) +
   geom_point(data=year_ace_hurr_wa, aes(x=as.factor(year), y=max_wind_ms, group = 2), color="khaki1") +

   # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
   geom_smooth(data=year_ace_major_wa, aes(x=as.factor(year), y=max_wind_ms, group = 3), method = "lm",color="darkorange3",se=0) +
   geom_point(data=year_ace_major_wa, aes(x=as.factor(year), y=max_wind_ms, group = 3), color="darkorange2") +

   geom_smooth(data=year_ace_intense_wa, aes(x=as.factor(year), y=max_wind_ms, group = 4), method = "lm",color="firebrick",se=0) +
   geom_point(data=year_ace_intense_wa, aes(x=as.factor(year), y=max_wind_ms, group = 4), color="firebrick1") +

   ggtitle("Mean Max Wind Speed M/S Since 1980") +
   coord_equal() +
   theme_bw() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1) ,
         aspect.ratio=6/12,
         legend.position="right",
         legend.key.width=unit(.25,"line"),
         legend.key.height=unit(.5,"line"),
       )


   ggplot() +
      geom_line(data=year_ace_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1, colour="Named Storms")) +
      geom_line(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2, colour="Hurricanes")) +
      geom_line(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3, colour="Major Hurricanes")) +
      geom_line(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4, colour="Intense Hurricanes")) +
       scale_colour_manual( values = cols) +

      geom_smooth(data=year_ace_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1), method = "lm",color="darkgray",se=0) +
      geom_point(data=year_ace_wa, aes(x=as.factor(year), y=named_avg_max_ms, group = 1), color="gray") +

      # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
      geom_smooth(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2), method = "lm",color="khaki3",se=0) +
      geom_point(data=year_ace_hurr_wa, aes(x=as.factor(year), y=hurricane_avg_max_ms, group = 2), color="khaki1") +

      # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
      geom_smooth(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3), method = "lm",color="darkorange3",se=0) +
      geom_point(data=year_ace_major_wa, aes(x=as.factor(year), y=major_avg_max_ms, group = 3), color="darkorange2") +

      geom_smooth(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4), method = "lm",color="firebrick",se=0) +
      geom_point(data=year_ace_intense_wa, aes(x=as.factor(year), y=intense_avg_max_ms, group = 4), color="firebrick1") +

      ggtitle("Mean Max Wind Speed M/S Since 1980") +
      coord_equal() +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12, legend.position="right")





   hurr_meta$xa <- seq.int(nrow(hurr_meta))
   hurr_meta_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic"  & hurr_meta$year >= 1980 & hurr_meta$named == 1)
   hurr_meta_wa$xa <- seq.int(nrow(hurr_meta_wa))

   hurr_meta_hurr_wa  <- subset(hurr_meta_wa, hurr_meta_wa$basin == "Western Atlantic"  & hurr_meta_wa$year >= 1980 & hurr_meta_wa$hurricane == 1)
   hurr_meta_major_wa <- subset(hurr_meta_wa, hurr_meta_wa$basin == "Western Atlantic"  & hurr_meta_wa$year >= 1980 & hurr_meta_wa$hurricane_major == 1 )
   hurr_meta_intense_wa <- subset(hurr_meta_wa, hurr_meta_wa$basin == "Western Atlantic"  & hurr_meta_wa$year >= 1980 & hurr_meta_wa$hurricane_intense == 1)


   log_max_wind_fit <- lm( log(hurr_meta_major_wa$max_wind_ms) ~ as.factor(hurr_meta_major_wa$year))
   log_max_wind_fit <- lm(  hurr_meta_intense_wa$max_wind_ms ~ as.factor(hurr_meta_intense_wa$year))

   #remove overall coefficent we only want the individual years
   coef_log_max_wind_fit <- log_max_wind_fit$coefficients[2:length(log_max_wind_fit$coefficients)]
   write.csv(coef_log_max_wind_fit, file.path(data_dir,"coef.csv"))
   coef_log_max_wind_fit <- read.csv(file.path(data_dir,"coef.csv"))
   coef_log_max_wind_fit$X <- str_sub(coef_log_max_wind_fit$X , -4,-1)
   coef_log_max_wind_fit <- dplyr::rename(coef_log_max_wind_fit, year = X, coefficient = x)

   #plot(coef_log_max_wind_fit$coefficient)

   ggplot(data=coef_log_max_wind_fit, aes(x=year, y=coefficient, group = 1)) +
     geom_line() +
     geom_smooth(method = "lm",color="#008fd5",se=FALSE) +
     geom_point() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1))


   # boxplot(hurr_meta_wa$ace ~ as.factor(hurr_meta_wa$year))
   # boxplot(log(hurr_meta_wa$max_wind_ms) ~ as.factor(hurr_meta_wa$year))

  cols <- c("Named Storms" = "gray", "Hurricanes" = "khaki1", "Major Hurricanes" = "darkorange2","Intense Hurricanes" = "firebrick1")

  ggplot() +
     # geom_line(data=hurr_meta_wa, aes(x=as.factor(year), y=ace, group = 1, colour="Named Storms")) +
     # geom_line(data=hurr_meta_hurr_wa, aes(x=as.factor(year), y=ace, group = 2, colour="Hurricanes")) +
     # geom_line(data=hurr_meta_major_wa, aes(x=as.factor(year), y=ace, group = 3, colour="Major Hurricanes")) +
     # geom_line(data=hurr_meta_intense_wa, aes(x=as.factor(year), y=ace, group = 4, colour="Intense Hurricanes")) +
     #  scale_colour_manual( values = cols) +

     geom_smooth(data=hurr_meta_wa, aes(x=as.factor(year), y=ace, group = 1), method = "lm",color="darkgray",se=0) +
     geom_point(data=hurr_meta_wa, aes(x=as.factor(year), y=ace, group = 1), color="gray") +

     # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
     geom_smooth(data=hurr_meta_hurr_wa, aes(x=as.factor(year), y=ace, group = 2), method = "lm",color="khaki3",se=0) +
     geom_point(data=hurr_meta_hurr_wa, aes(x=as.factor(year), y=ace, group = 2), color="khaki1") +

     # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
     geom_smooth(data=hurr_meta_major_wa, aes(x=as.factor(year), y=ace, group = 3), method = "lm",color="darkorange3",se=0) +
     geom_point(data=hurr_meta_major_wa, aes(x=as.factor(year), y=ace, group = 3), color="darkorange2") +

     geom_smooth(data=hurr_meta_intense_wa, aes(x=as.factor(year), y=ace, group = 4), method = "lm",color="firebrick",se=0) +
     geom_point(data=hurr_meta_intense_wa, aes(x=as.factor(year), y=ace, group = 4), color="firebrick1") +

     ggtitle("ACE Score by Storm Intensities since 1980") +
     coord_equal() +
     theme_bw() +
     theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12, legend.position="right")




       ggplot() +
          geom_line(data=hurr_meta_wa, aes(x=as.factor(year), y=max_wind_mph, group = 1, colour="Named Storms")) +
          geom_line(data=hurr_meta_hurr_wa, aes(x=as.factor(year), y=max_wind_mph, group = 2, colour="Hurricanes")) +
          geom_line(data=hurr_meta_major_wa, aes(x=as.factor(year), y=max_wind_mph, group = 3, colour="Major Hurricanes")) +
          geom_line(data=hurr_meta_intense_wa, aes(x=as.factor(year), y=max_wind_mph, group = 4, colour="Intense Hurricanes")) +
           scale_colour_manual( values = cols) +

          geom_smooth(data=hurr_meta_wa, aes(x=as.factor(year), y=max_wind_mph, group = 1), method = "lm",color="darkgray",se=0) +
          geom_point(data=hurr_meta_wa, aes(x=as.factor(year), y=max_wind_mph, group = 1), color="gray") +

          # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
          geom_smooth(data=hurr_meta_hurr_wa, aes(x=as.factor(year), y=max_wind_mph, group = 2), method = "lm",color="khaki3",se=0) +
          geom_point(data=hurr_meta_hurr_wa, aes(x=as.factor(year), y=max_wind_mph, group = 2), color="khaki1") +

          # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
          geom_smooth(data=hurr_meta_major_wa, aes(x=as.factor(year), y=max_wind_mph, group = 3), method = "lm",color="darkorange3",se=0) +
          geom_point(data=hurr_meta_major_wa, aes(x=as.factor(year), y=max_wind_mph, group = 3), color="darkorange2") +

          geom_smooth(data=hurr_meta_intense_wa, aes(x=as.factor(year), y=max_wind_mph, group = 4), method = "lm",color="firebrick",se=0) +
          geom_point(data=hurr_meta_intense_wa, aes(x=as.factor(year), y=max_wind_mph, group = 4), color="firebrick1") +

          ggtitle("Max wind M/S for individual storms since 1980") +
          coord_equal() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12, legend.position="right")



                 ggplot() +
                    geom_line(data=hurr_meta_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 1, colour="Named Storms")) +
                    geom_line(data=hurr_meta_hurr_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 2, colour="Hurricanes")) +
                    geom_line(data=hurr_meta_major_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 3, colour="Major Hurricanes")) +
                    geom_line(data=hurr_meta_intense_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 4, colour="Intense Hurricanes")) +
                     scale_colour_manual( values = cols) +

                    geom_smooth(data=hurr_meta_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 1), method = "lm",color="darkgray",se=0) +
                    geom_point(data=hurr_meta_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 1), color="gray") +

                    # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
                    geom_smooth(data=hurr_meta_hurr_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 2), method = "lm",color="khaki3",se=0) +
                    geom_point(data=hurr_meta_hurr_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 2), color="khaki1") +

                    # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
                    geom_smooth(data=hurr_meta_major_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 3), method = "lm",color="darkorange3",se=0) +
                    geom_point(data=hurr_meta_major_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 3), color="darkorange2") +

                    geom_smooth(data=hurr_meta_intense_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 4), method = "lm",color="firebrick",se=0) +
                    geom_point(data=hurr_meta_intense_wa, aes(x=as.factor(xa), y=max_wind_mph, group = 4), color="firebrick1") +

                    ggtitle("Max wind M/S for individual storms since 1980") +
                    coord_equal() +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12, legend.position="right")



       ggplot() +
          geom_line(data=hurr_meta_wa, aes(x=xa, y=ace, group = 1, colour="Named Storms")) +
          geom_line(data=hurr_meta_hurr_wa, aes(x=xa, y=ace, group = 2, colour="Hurricanes")) +
          geom_line(data=hurr_meta_major_wa, aes(x=xa, y=ace, group = 3, colour="Major Hurricanes")) +
          geom_line(data=hurr_meta_intense_wa, aes(x=xa, y=ace, group = 4, colour="Intense Hurricanes")) +
           scale_colour_manual( values = cols) +
          #
          geom_smooth(data=hurr_meta_wa, aes(x=xa, y=ace, group = 1), method = "lm",color="darkgray",se=0) +
          # geom_point(data=hurr_meta_wa, aes(x=xa, y=ace, group = 1), color="gray") +
          #
          # # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
          geom_smooth(data=hurr_meta_hurr_wa, aes(x=xa, y=ace, group = 2), method = "lm",color="khaki3",se=0) +
          # geom_point(data=hurr_meta_hurr_wa, aes(x=xa, y=ace, group = 2), color="khaki1") +
          #
          # # scale_colour_manual(values=c("firebrick1","orange","goldenrod1","gray")) +
          geom_smooth(data=hurr_meta_major_wa, aes(x=xa, y=ace, group = 3), method = "lm",color="darkorange3",se=0) +
          # geom_point(data=hurr_meta_major_wa, aes(x=xa, y=ace, group = 3), color="darkorange2") +
          #
          geom_smooth(data=hurr_meta_intense_wa, aes(x=xa, y=ace, group = 4), method = "lm",color="firebrick",se=0) +
          # geom_point(data=hurr_meta_intense_wa, aes(x=xa, y=ace, group = 4), color="firebrick1") +

          ggtitle("ACE Score for individual Storms Since 1980") +
          coord_equal() +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=3/18, legend.position="right")
