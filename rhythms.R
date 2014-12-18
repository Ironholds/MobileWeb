library(WMUtils)
library(rgeoip)

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
  data <-  data[,j={
    
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
}