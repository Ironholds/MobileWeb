library(reconstructr)
library(WMUtils)
library(ggplot2)
library(diptest)
library(perm)

options(scipen=500)
dir.create(file.path(getwd(),"Datasets","Sessions"), showWarnings = FALSE)
dir.create(file.path(getwd(),"Graphs","Sessions"), showWarnings = FALSE)

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
  
  x <- x[x$userid %in% sample(unique(x$userid),100000),]
  to_df <- function(x,type,metric){
    return(data.table(x = x[x >= 0],type,metric))
  }
  
  type <- unique(x$type)
  sessions <- lapply(split(x$timestamp, x$userid),function(x){sessionise(list(x))})
  
  sessions_by_user <- to_df(unlist(lapply(sessions,length)),type,"Sessions per user")
  sessions <- unlist(sessions, recursive = FALSE)
  events_per_session <- to_df(session_events(sessions),type,"Pageviews per session")
  length_of_sessions <- to_df(session_length(sessions, padding_value = 0),type,"Session length")
  return(list(sessions_by_user,events_per_session,length_of_sessions))
}

results <- mapply(rbind,session_gen(data[data$type == "mobile",]),session_gen(data[data$type == "desktop"]),
               SIMPLIFY = FALSE)

permTS(results[[1]]$x[results[[1]]$type == "mobile"],results[[1]]$x[results[[1]]$type == "desktop"],"greater")
write.table(results[[1]], file = file.path(getwd(),"Datasets","Sessions","sessions_by_user.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(data = results[[1]],
                     aes(x = type, y = x)) + geom_boxplot() + 
         scale_y_log10() + 
         labs(title = "Number of sessions per user",
              x = "Access method",
              y = "Sessions"),
       filename = file.path(getwd(),"Graphs","Sessions",paste0("sessions_by_user.png")))

permTS(results[[2]]$x[results[[2]]$type == "desktop"],results[[2]]$x[results[[2]]$type == "mobile"],"greater")
write.table(results[[2]], file = file.path(getwd(),"Datasets","Sessions","pageviews_per_session.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(data = results[[2]],
                     aes(x = type, y = x)) + geom_boxplot() + 
         scale_y_log10() + 
         labs(title = "Pageviews per session",
              x = "Access method",
              y = "Pageviews"),
       filename = file.path(getwd(),"Graphs","Sessions",paste0("pageviews_per_session.png")))
permTS(results[[3]]$x[results[[3]]$type == "desktop"],results[[3]]$x[results[[3]]$type == "mobile"],"greater")
write.table(results[[3]], file = file.path(getwd(),"Datasets","Sessions","session_length.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(data = results[[3]][results[[3]]$x > 0,],
                     aes(x = type, y = x)) + geom_boxplot() + 
         scale_y_log10() + 
         labs(title = "Session length",
              x = "Access method",
              y = "Session length (seconds)"),
       filename = file.path(getwd(),"Graphs","Sessions",paste0("session_length.png")))

sampled_data <- data[data$userid %in% sample(unique(data$userid[data$type == "mobile"]),100000) | data$userid %in% sample(unique(data$userid[data$type == "desktop"]),100000)]
sampled_data <- sampled_data[,j = .N, by = c("userid","type")]
permTS(sampled_data$N[sampled_data$type == "mobile"],sampled_data$N[sampled_data$type == "desktop"],"less")
write.table(sampled_data[,c("userid") := NULL], file = file.path(getwd(),"Datasets","Sessions","events_per_user.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(data = sampled_data,
                     aes(x = type, y = N)) + geom_boxplot() + 
         scale_y_log10() +
         labs(title = "Events per user",
              x = "Access method",
              y = "Events per user"),
       filename = file.path(getwd(),"Graphs","Sessions",paste0("events_per_user.png")))

#Bounce rate
bounce <- function(x){
  return(length(x[x == 1])/length(x))
}
c(bounce(results[[2]]$x[results[[2]]$type == "mobile"]),bounce(results[[2]]$x[results[[2]]$type == "desktop"]))