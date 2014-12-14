library(reconstructr)
library(WMUtils)

reader <- function(){
  
  data <- mysql_query("SELECT event_experimentId AS userid,
                       timestamp AS timestamp,
                       event_mobileMode AS type
                       FROM ModuleStorage_6978194 WHERE LEFT(timestamp,8) BETWEEN 20140107 AND 20140122",
                      "log")
  
  #Divide into mobile/desktop
  is_mobile <- !is.na(data$type)
  data$type[is_mobile] <- "mobile"
  data$type[!is_mobile] <- "desktop"
  
  #Handle timestamps
  data$timestamp <- as.numeric(mw_strptime(data$timestamp))
}

data <- reader()

session_gen <- function(x){
  
  to_df <- function(x,type,metric){
    return(data.table(x,type,metric))
  }
  
  type <- unique(x$type)
  sessions <- lapply(split(x$timestamp, x$userid),function(x){sessionise(list(x))})
  
  sessions_by_user <- to_df(unlist(lapply(sessions,length)),type,"Sessions per user")
  sessions <- unlist(sessions, recursive = FALSE)
  events_per_session <- to_df(session_events(sessions),type,"Pageviews per session")
  length_of_sessions <- to_df(session_length(sessions, padding_value = 0),type,"Session length")
  
  return(list(sessions_by_user,events_per_session,length_of_sessions))
}

results <- list(session_gen(data[data$type == "mobile",]),
                session_gen(data[data$type == "desktop"]))