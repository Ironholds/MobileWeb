library(WMUtils)
library(rgeoip)
dir.create(file.path(getwd(),"Datasets","Rhythms"), showWarnings = FALSE)
dir.create(file.path(getwd(),"Graphs","Rhythms"), showWarnings = FALSE)

reader_retrieve <- function(x){
  
  data <- dep_hive_query("select dt,webrequest_source,ip,x_forwarded_for
                        FROM wmf_raw.webrequest TABLESAMPLE(100 ROWS)
                        WHERE year = 2014
                        AND month = 12
                        AND webrequest_source IN('text','mobile')
                        AND content_type LIKE('text/html%')
                        AND uri_path LIKE('/wiki/%')
                        LIMIT 10000000;")
  
  data$x_forwarded_for <- unlist(lapply(strsplit(data$x_forwarded_for,","),function(x){return(x[1])}))
  data$ip[!data$x_forwarded_for == "-"] <- data$x_forwarded_for[!data$x_forwarded_for == "-"]
  data$webrequest_source[data$webrequest_source == "text"] <- "desktop"
  
  data$tz <- geolookup(data$ip, "/usr/local/share/GeoIP/GeoIP2-City.mmdb","timezone")
  data <- data[!data$tz == "Unknown",]
  data <- data[,j={
    
    #Copy .SD
    subset_copy <- copy(.SD)
    
    #Grab timestamp
    localised_stamps <- with_tz(log_strptime(dt), tz)

    #Extract and include weekday and hour
    subset_copy$hour <- hour(localised_stamps)
    subset_copy$day <- as.character(x = wday(x = localised_stamps, label = TRUE))
    
    #Return
    subset_copy
    
  }, by = "tz"]
  
  #Sample and rename
  data <- data[,c("x_forwarded_for","ip","tz","dt") := NULL]
  data$day[data$day %in% c("Sat","Sun")] <- "Weekend"
  data$day[!data$day == "Weekend"] <- "Weekday"
  setnames(data,1,"type")
  return(data)
}


overall <- data[,j={(.SD[sample(1:.N,100000),])},by = "type"]
overall <- overall[,.N,by = c("hour","type")]
write.table(overall, file = file.path(getwd(),"Datasets","Rhythms","reader_circadian_overall.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(overall, aes(x = hour, y = N, type = type, colour = type)) + 
  geom_line(size=1.5) + scale_x_continuous(breaks = seq(0,24,1)) +
  labs(title = "Proportion of mobile and desktop read actions, by hour (localised)",
       x = "Hour",
       y = "Count"),
  file = file.path(getwd(),"Graphs","Rhythms","read_actions_by_hour.png"))

weekend <- data[data$day == "Weekend",j={(.SD[sample(1:.N,50000),])},by = "type"]
weekend <- weekend[,.N,by = c("hour","type")]
write.table(weekend, file = file.path(getwd(),"Datasets","Rhythms","reader_circadian_weekends.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(weekend, aes(x = hour, y = N, type = type, colour = type)) + 
         geom_line(size=1.5) + scale_x_continuous(breaks = seq(0,24,1)) +
         labs(title = "Proportion of mobile and desktop read actions, by hour (localised)",
              x = "Hour",
              y = "Count"),
       file = file.path(getwd(),"Graphs","Rhythms","read_actions_by_hour_weekend.png"))

weekday <- data[data$day == "Weekday",j={(.SD[sample(1:.N,50000),])},by = "type"]
weekday <- weekday[,.N,by = c("hour","type")]
write.table(weekday, file = file.path(getwd(),"Datasets","Rhythms","reader_circadian_weekdays.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(weekday, aes(x = hour, y = N, type = type, colour = type)) + 
         geom_line(size=1.5) + scale_x_continuous(breaks = seq(0,24,1)) +
         labs(title = "Proportion of mobile and desktop read actions, by hour (localised)",
              x = "Hour",
              y = "Count"),
       file = file.path(getwd(),"Graphs","Rhythms","read_actions_by_hour_weekday.png"))