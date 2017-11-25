
year_ace_wa <- subset(year_ace, year_ace$basin == "Western Atlantic")
g <- ggplot(year_ace_wa, aes(year))
g + geom_bar()

hurr_meta_intense_wa <- subset(hurr_meta, hurr_meta$basin == "Western Atlantic" & hurr_meta$max_category >= 4 & hurr_meta$year >= 1960)

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
  ggtitle("Intense Hurricanes by Max Wind in Mph") +
  coord_equal() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1) , aspect.ratio=6/12)


  ggplot(hurr_meta_intense_wa, aes(year, max_wind_ms)) +
    geom_smooth(aes(x=year, y=max_wind_ms, group = 1) , method = "lm",color="#008fd5",se=0) +
    geom_point(aes(size=max_wind_ms, color=as.character(max_category)), alpha=3/4) +
    scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
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
     scale_color_manual(breaks = c("4", "5"),values=c("orange","red")) +
     ggtitle("Intense Hurricanes by ACE score ") +
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
