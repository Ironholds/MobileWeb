library(reconstructr)
library(WMUtils)
library(ggplot2)
library(lubridate)
library(rgeoip)

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
                     rev_page AS page,
                     rev_sha1
                     FROM cu_changes INNER JOIN recentchanges
                     ON cuc_this_oldid = rc_this_oldid
                     INNER JOIN revision
                     ON cuc_this_oldid = rev_id
                     WHERE rc_bot = 0
                     AND rev_timestamp >='",to_mw(Sys.time() - 2592000),"'"),
                    "enwiki")