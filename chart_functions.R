#############################
#graphing functions
#############################

ggScatterAuto <-  function(data, xField, yField, method, title,
                       xLabel, yLabel, source){


  m <- lm(yField ~ xField, data);
  r2 <- format(summary(m)$r.squared, digits = 3)
  pearsons <- format(cor(xField, yField, use = "complete.obs"), digits = 3)

  ggplot(data, aes(x = xField, y = yField)) +
  geom_smooth(method = method,color="#008fd5",se=0) +
  geom_point(color="#b2ddf2", alpha=.7, size=3) +
  geom_point(shape = 1, colour="#008fd5", alpha=.5, size=3) +
  theme_minimal(base_size=theme_base_size) +
  labs(title= paste(title),
    subtitle=paste("R-squared = ",r2),
     x=xLabel,
     y=yLabel,
     caption=paste("Source:",source)) +
     theme(plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size=6))
}

ggScatterAutoNoR <-  function(data, xField, yField, method, title,
                       xLabel, yLabel, source){

   m <- lm(yField ~ xField, data);
   r2 <- format(summary(m)$r.squared, digits = 3)

  ggplot(data, aes(x = xField, y = yField)) +
  geom_smooth(method = method,color="#008fd5",se=0) +
  geom_point(color="#b2ddf2", alpha=.7, size=3) +
  geom_point(shape = 1, colour="#008fd5", alpha=.5, size=3) +
  theme_minimal(base_size=theme_base_size) +
  labs(title= paste(title),
     subtitle=paste("R-squared = ",r2),
     x=xLabel,
     y=yLabel,
     caption=paste("Source:",source)) +
     theme(plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size=6))
}

ggScatterAutoNoRLim <-  function(data, xField, yField, method, title,
                       xLabel, yLabel, source){

   m <- lm(yField ~ xField, data);
   r2 <- format(summary(m)$r.squared, digits = 3)

  ggplot(data, aes(x = xField, y = yField)) +
  geom_smooth(method = method,color="#008fd5",se=0) +
  geom_point(color="#b2ddf2", alpha=.7, size=3) +
  geom_point(shape = 1, colour="#008fd5", alpha=.5, size=3) +
  coord_cartesian(ylim = c(75, 150)) +
  theme_minimal(base_size=theme_base_size) +
  labs(title= paste(title),
    subtitle=paste("R-squared = ",r2),
     x=xLabel,
     y=yLabel,
     caption=paste("Source:",source)) +
     theme(plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size=6))
}

ggScatterAutoNoRLimMajor <-  function(data, xField, yField, method, title,
                       xLabel, yLabel, source){

   m <- lm(yField ~ xField, data);
   r2 <- format(summary(m)$r.squared, digits = 3)

  ggplot(data, aes(x = xField, y = yField)) +
  geom_smooth(method = method,color="#008fd5",se=0) +
  geom_point(color="#b2ddf2", alpha=.7, size=3) +
  geom_point(shape = 1, colour="#008fd5", alpha=.5, size=3) +
  coord_cartesian(ylim = c(100, 210)) +
  theme_minimal(base_size=theme_base_size) +
  labs(title= paste(title),
    subtitle=paste("R-squared = ",r2),
     x=xLabel,
     y=yLabel,
     caption=paste("Source:",source)) +
     theme(plot.subtitle = element_text(color="#666666"),
          plot.caption = element_text(color="#AAAAAA", size=6))
}

ggBarTime<- function(data, title, xfield, yfield, xlabel, ylabel, source){

  storm_min_date <- aggregate(x=data$date, by=list(data$storm_id),FUN=min)
  storm_max_date <- aggregate(x=data$date, by=list(data$storm_id),FUN=max)

  storm_min_date <- dplyr::rename(storm_min_date,  storm_id = Group.1, min_date = x)
  storm_max_date <- dplyr::rename(storm_max_date,  storm_id = Group.1, max_date = x)

  storm_min_date$min_date <- format(as.Date(storm_min_date$min_date), format='%b %d, %Y')
  storm_max_date$max_date <- format(as.Date(storm_max_date$max_date), format='%b %d, %Y')

  storm_datet <- paste("Date: ", paste(storm_min_date$min_date, storm_max_date$max_date , sep=" - ") )


  ggplot(data) +
  geom_bar(aes(as.factor(xfield), as.numeric(yfield)),
            position = "dodge", stat = "summary",
            fun.y = "max", fill="#b2ddf2", color="#b2ddf2",
            width=.75) +
  coord_cartesian(ylim = c(0, 210)) +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 200, by = 25)) +
  theme_minimal(base_size=theme_base_size) +
   labs(title= paste(title),
        subtitle=storm_datet,
        x=xlabel,
        y=ylabel,
        caption=paste("Source:",source)) +
        theme(plot.subtitle = element_text(color="#666666", size=5),
                 plot.caption = element_text(color="#AAAAAA", size=5),
                 axis.text.x=element_text(angle=90, size=3),
                 axis.ticks.x=element_blank(),
                 panel.grid.major = element_line(colour = "#F0F0F0", size=.1),
               panel.grid.minor = element_blank(),
           panel.background = element_blank(),
         panel.border = element_rect(colour = "#F0F0F0", fill=NA, size=.25))
}

ggBarMaxAll<- function(data, title, xfield, yfield, xlabel, ylabel, source){

  m <- lm(yfield ~ xfield, data);
  r2 <- format(summary(m)$r.squared, digits = 3)

  b_breaks = c(185101.1, 190001.1, 193001.1, 195001.1, 196001.1, 197001.1, 198001.1,  199001.1, 200001.1, 201001.1)
  b_labels = c("1850",  "1900",   "1930",   "1950",  "1960",   "1970",  "1980",     "1990",    "2000",  "2010")

  #adjust per basin
  if (str_sub(xfield[1], -1, -1) == 2){
    b_breaks = c(185101.2, 190001.2, 193001.2, 195001.2, 196001.2, 197001.2, 198001.2,  199001.2, 200001.2, 201001.2)
    b_labels = c("1850",  "1900",   "1930",   "1950",   "1960",   "1970",   "1980",     "1990",   "2000",  "2010")
  }

  ggplot(data,aes(as.factor(xfield),  yfield)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "max",
           fill="#b2ddf2", color="#b2ddf2", size = .5) +
           geom_smooth(method = "lm",color="#008fd5",se=0) +

  scale_x_discrete(
                  breaks = b_breaks,
                  labels = b_labels) +

  theme_minimal(base_size=theme_base_size) +
   labs(title= paste(title),
        subtitle="",
        x=xlabel,
        y=ylabel,
        caption=paste("Source:",source)) +
        theme(plot.subtitle = element_text(color="#666666"),
                 plot.caption = element_text(color="#AAAAAA", size=6),
               panel.grid.minor = element_blank(),
           panel.background = element_blank())

}

ggBarYear<- function(data, title, xfield, yfield, xlabel, ylabel, source){

  m <- lm(yfield ~ xfield, data);
  r2 <- format(summary(m)$r.squared, digits = 3)

  b_breaks = c(1850, 1900, 1930, 1950, 1960, 1970, 1980,  1990, 2000, 2010)
  b_labels = c("1850", "1900", "1930", "1950", "1960", "1970", "1980", "1990", "2000", "2010")

  #adjust per basin
  if (str_sub(xfield[1], -1, -1) == 2){
    b_breaks = c(1850, 1900, 1930, 1950, 1960, 1970, 1980,  1990, 2000, 2010)
    b_labels = c("1850", "1900", "1930", "1950", "1960", "1970", "1980", "1990", "2000", "2010")
  }

  ggplot(data,aes(as.factor(xfield),  yfield)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "max",
           fill="#b2ddf2", color="#b2ddf2", size = .5) +
  scale_x_discrete(
                  breaks = b_breaks,
                  labels = b_labels) +
  theme_minimal(base_size=theme_base_size) +
   labs(title= paste(title),
        subtitle="",
        x=xlabel,
        y=ylabel,
        caption=paste("Source:",source)) +
        theme(plot.subtitle = element_text(color="#666666"),
                 plot.caption = element_text(color="#AAAAAA", size=6),
               panel.grid.minor = element_blank(),
           panel.background = element_blank())

}
