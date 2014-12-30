library(reconstructr)
library(WMUtils)
library(ggplot2)
library(lubridate)
library(rgeoip)
library(MWUtils)
library(perm)


options(scipen=500)
dir.create(file.path(getwd(),"Datasets","Edits"), showWarnings = FALSE)
dir.create(file.path(getwd(),"Graphs","Edits"), showWarnings = FALSE)

data <- mysql_query("SELECT rev_timestamp, ts_tags
                     FROM revision LEFT JOIN tag_summary ON rev_id = ts_rev_id
                     WHERE rev_timestamp >= '20130501000000'","enwiki")

timestamps <- as.Date(mw_strptime(data$rev_timestamp))
day(timestamps) <- 01
data$rev_timestamp <- timestamps
is_mobile <- grepl(x = data$ts_tags, pattern = "mobile edit", fixed = TRUE)
data$ts_tags[is_mobile] <- "mobile"
data$ts_tags[!is_mobile] <- "desktop"
data <- data[,j = list(edits = .N), by = c("rev_timestamp","ts_tags")]
data <- data[!data$rev_timestamp == "2014-12-01",]
setnames(data,1:2,c("date","type"))

write.table(data, file = file.path(getwd(),"Datasets","Edits","overall_trend.tsv"),
            quote = T, sep = "\t", row.names = F)
ggsave(plot = ggplot(data, aes(date, edits, type = type, group = type, colour = type)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(breaks = seq(0,5000000,500000)) +
  labs(title = "Edits on the English-language Wikipedia, by access method",
       x = "Month",
       y = "Edits"),
  filename = file.path(getwd(),"Graphs","Edits","overall_trend.png"))

#If we look at the last 30 days in detail...
data <- mysql_query(paste0("SELECT cuc_ip AS ip_address,
                     rc_timestamp AS timestamp,
                     rc_old_len AS old,
                     rc_new_len AS new,
                     ts_tags AS tags,
                     rev_page AS page,
                     rev_sha1,
                     rev_user_text AS user,
                     cuc_agent AS user_agent,
                     user_registration AS reg_date
                     FROM cu_changes INNER JOIN recentchanges
                     ON cuc_this_oldid = rc_this_oldid
                     INNER JOIN revision
                     ON cuc_this_oldid = rev_id
                     INNER JOIN user
                     ON rev_user = user_id
                     INNER JOIN tag_summary
                     ON rev_id = ts_rev_id
                     WHERE rc_bot = 0
                     AND rev_timestamp >='",to_mw(Sys.time() - 2592000),"'"),
                    "enwiki")

#Localise
data$tz <- geolookup(data$ip, "/usr/local/share/GeoIP/GeoIP2-City.mmdb","timezone")
data <- data[,j={
  
  #Copy .SD
  subset_copy <- copy(.SD)
  
  #Grab timestamp
  localised_stamps <- with_tz(mw_strptime(timestamp), tz)
  
  #Extract and include weekday and hour
  subset_copy$hour <- hour(localised_stamps)
  subset_copy$day <- as.character(x = wday(x = localised_stamps, label = TRUE))
  
  #Return
  subset_copy
  
}, by = "tz"]

#Detect reverts
data <- detect_reverts(data,"timestamp","rev_sha1","page")

#Remove automata
bot_usernames <- c("OctraBot","ZiadBot","Happy05dzBot","1999franbot","AlphamaBot","AlphamaBot2","ShitiBot",
                   "EmausBot","Hoangdat bot","AlphamaBot4","XLinkBot","BryanBot","HangsnaBot2","AVdiscuBOT","JBot",
                   "StubCreationBot","Yjs5497 bot","IanraBot","MerlBot","RotlinkBot","Dinobot-br","Jembot","DvtBot",
                   "Fikarumsrobot","H2Bot","BanwolBot","ThitxongkhoiAWB","Rotlink")
bot_agents <- "((Py(thon)?)?wiki(pedia)?bot|MediaWiki|Wiki\\.java|libcurl|(Synch|Abbott|Wartungslisten|Octra)bot|libwww-perl)"
data <- data[!grepl(x = data$user_agent, pattern = bot_agents, perl = TRUE, useBytes = TRUE),]
data <- data[!data$user %in% bot_usernames,]

#Classify edits
data$type[grepl(x = data$tags, pattern = "mobile edit", fixed = TRUE)] <- "mobile"
data$type[is.na(data$type)] <- "desktop"

edit_sample <- data[,j={.SD[sample(1:.N,50000),]},by = "type"]

circ_aggs <- edit_sample[,j = list(edits = .N), by = c("type","hour")]
ggsave(plot = 
ggplot(circ_aggs, aes(x = hour, y = edits, group = type, colour = type)) + 
  geom_line(size = 1.5) +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(title = "Circadian distribution of randomly-sampled mobile and desktop edits"),
file = file.path(getwd(),"Graphs","Edits","circadian.png"))
data <- data[!is.na(data$reg_date),]
editor_sample <- data[as.Date(mw_strptime(data$timestamp)) - 30 >= as.Date(mw_strptime(data$reg_date)),]

mobile <- editor_sample[editor_sample$type == "mobile",]
mob_sessions <- sessionise(split(as.numeric(mw_strptime(mobile$timestamp)),mobile$user))
desktop <- editor_sample[editor_sample$type == "desktop",]
desk_sessions <- sessionise(split(as.numeric(mw_strptime(desktop$timestamp)),desktop$user))

mob_length <- session_length(mob_sessions, padding_value=0)
mob_length <- mob_length[mob_length > 0]
mob_length <- data.frame(length = mob_length, type = "mobile", stringsAsFactors=FALSE)
names(mob_length) <- c("length","type")
desk_length <- session_length(desk_sessions, padding_value=0)
desk_length <- desk_length[desk_length > 0]
desk_length <- data.frame(length = desk_length, type = "desktop", stringsAsFactors=FALSE)
names(desk_length) <- c("length","type")
lengths <- rbind(mob_length,desk_length)
lengths$length <- as.numeric(lengths$length)
ggsave(plot = 
         ggplot(lengths, aes(type,length)) + geom_boxplot() + scale_y_log10() +
         labs(title = "Session length for new editors, mobile versus desktop",
              y = "length (seconds)",
              x = "type"),
       file = file.path(getwd(),"Graphs","Edits","edit_length.png"))

mob_events <- session_events(mob_sessions)
mob_events <- mob_events[mob_events > 0]
mob_events <- data.frame(events = mob_events, type = "mobile", stringsAsFactors=FALSE)
names(mob_events) <- c("events","type")
desk_events <- session_events(desk_sessions)
desk_events <- desk_events[desk_events > 0]
desk_events <- data.frame(events = desk_events, type = "desktop", stringsAsFactors=FALSE)
names(desk_events) <- c("events","type")
events <- rbind(mob_events,desk_events)
events$events <- as.numeric(events$events)
ggsave(plot = 
         ggplot(events, aes(type,events)) + geom_boxplot() + scale_y_log10() +
         labs(title = "Edits per session for new editors, mobile versus desktop",
              y = "edits per session",
              x = "type"),
       file = file.path(getwd(),"Graphs","Edits","edits_per_session.png"))

editor_sample$diff <- editor_sample$new - editor_sample$old
diff_size <- data.frame(size = abs(editor_sample$diff), type = editor_sample$type, stringsAsFactors=F)

ggsave(plot = 
  ggplot(diff_size, aes(type,size)) + geom_boxplot() +
    labs(title = "Diff size per edit for new editors, mobile versus desktop",
         y = "diff size (bytes)",
         x = "type") + scale_y_log10(),
  file = file.path(getwd(),"Graphs","Edits","diff_size.png"))