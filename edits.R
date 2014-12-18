library(reconstructr)
library(WMUtils)
library(ggplot2)
library(lubridate)
options(scipen=500)
dir.create(file.path(getwd(),"Datasets","Edits"), showWarnings = FALSE)
dir.create(file.path(getwd(),"Graphs","Edits"), showWarnings = FALSE)

ts_tags AS type,
user_editcount AS editcount
FROM cu_changes LEFT JOIN tag_summary ON cuc_this_oldid = ts_rev_id

data <- mysql_query("SELECT rev_timestamp, ts_tags
                     FROM revision LEFT JOIN tag_summary ON rev_id = ts_rev_id
                     WHERE rev_timestamp >= '20130501000000'","enwiki")

timestamps <- as.Date(mw_strptime(data$timestamp))
day(timestamps) <- 01

data <- mysql_query(paste("SELECT cuc_ip AS ip_address,
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
                     AND rev_timestamp >=",to_mw(Sys.time() - 2592000)),
                    "enwiki")