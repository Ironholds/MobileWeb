#Basic analysis of trends around pageviews data
library(WMUtils)
library(ggplot2)
library(reshape2)
library(scales)
dir.create("Datasets/Basic", showWarnings = FALSE)
dir.create("Graphs/Basic", showWarnings = FALSE)
options(scipen=500)

reader <- function(){
  data <- mysql_query("SELECT LEFT(timestamp,10) AS date,
                      countries.country_name AS country,
                      project,
                      access_method,
                      SUM(pageviews) as pv_count
                      FROM pageviews INNER JOIN countries
                      ON pageviews.country = countries.country_code
                      WHERE is_automata = 0
                      AND is_spider = 0
                      GROUP BY LEFT(timestamp,10),country,project,access_method",
                      "staging")
  
  data$ym <- as.Date(paste0(substr(data$date,0,7),"-01"))
  data$date <- as.Date(data$date)
  data$access_method[!data$access_method == "Desktop"] <- "Mobile"
  un_yms <- unique(data$ym)
  data <- data[! data$ym %in% c(un_yms[1],un_yms[length(un_yms)]),]
  return(data)
}

data <- reader()

#Graph overall trends
overall <- rbind(data[,j = list(pageviews = sum(pv_count), access_method = "Total"), by = c("ym")],
                 data[,j = list(pageviews = sum(pv_count)), by = c("ym","access_method")])

ggsave(filename = "Graphs/Basic/overall_trend.png", plot = 
ggplot(overall, aes(ym,pageviews, group = access_method, colour = access_method)) + 
  geom_point() + theme_bw() + scale_x_date(breaks = "month") + 
  stat_smooth(method = "lm", se = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Wikimedia pageviews, by month",
       x = "Year and month",
       y = "Pageviews"))

#R2 values
r2 <- function(x){
  x <- x[order(x$ym),]
  intermediary <- x[,j = list(pageviews = sum(pv_count)), by = c("access_method","ym")]
  if(!nrow(intermediary[intermediary$access_method == "Mobile"]) == nrow(intermediary[intermediary$access_method == "Desktop"])){
    return(list(0,0,0))
  }
  results <- summary(lm(intermediary$pageviews[intermediary$access_method == "Mobile"] ~ intermediary$pageviews[intermediary$access_method == "Desktop"]))
  
  output <- list(results$r.squared,results$adj.r.squared,sum(intermediary$pageviews[intermediary$ym == max(intermediary$ym)]))
  return(output)
}
by_country_r2 <- data[!data$country == "Invalid",j = r2(.SD), by = c("country")]
setnames(by_country_r2,2:4,c("R2","Adjusted_R2","Last_Pageviews"))
top_country_r2 <- by_country_r2[by_country_r2$country %in% by_country_r2$country[order(by_country_r2$Last_Pageviews, decreasing = TRUE)][1:20]]
ggsave(filename = "Graphs/Basic/top_country_R2.png",
       plot = ggplot(data = melt(top_country_r2, id.vars = 1, measure.vars = 2:3),
                     aes(reorder(country,value),value,type =variable, colour = variable)) + 
                geom_point() + coord_flip() + scale_y_continuous() +
                labs(title = "Linear correlation between mobile and desktop pageview counts, by country",
                     x = "Country", y = "Value"))
by_project_r2 <- data[!data$country == "Invalid",j = r2(.SD), by = c("project")]
setnames(by_project_r2,2:4,c("R2","Adjusted_R2","Last_Pageviews"))
top_project_r2 <- by_project_r2[by_project_r2$project %in% by_project_r2$project[order(by_project_r2$Last_Pageviews, decreasing = TRUE)][1:20]]
ggsave(filename = "Graphs/Basic/top_project_R2.png",
       plot = ggplot(data = melt(top_project_r2, id.vars = 1, measure.vars = 2:3),
                     aes(reorder(project,value),value,type =variable, colour = variable)) + 
                geom_point() + coord_flip() + scale_y_continuous() +
                labs(title = "Linear correlation between mobile and desktop pageview counts, by project",
                     x = "Project", y = "Value"))

#Switchover
switchover <- function(x){
  perc <- (sum(x$pv_count[x$access_method == "Mobile"])/sum(x$pv_count))
  list(perc,sum(x$pv_count))
}
country_singularity_subset <- data[data$ym == max(data$ym) & !data$country == "Invalid",
                                   j = switchover(.SD), by = "country"]
setnames(country_singularity_subset,2:3,c("Percentage_mobile","Total_pageviews"))
top_country_subset <- country_singularity_subset[country_singularity_subset$country %in% country_singularity_subset$country[order(country_singularity_subset$Total_pageviews, decreasing = TRUE)][1:20]]
ggsave(filename = "Graphs/Basic/switchover_by_country.png",
       plot = ggplot(data = top_country_subset,
                     aes(reorder(country,Percentage_mobile),Percentage_mobile)) + 
         geom_point(colour = "red") + coord_flip() + scale_y_continuous(labels = percent, limits = c(0,1)) +
         labs(title = "Percentage of pageviews from Mobile, per country",
              x = "Country", y = "Percentage") + geom_hline(xintercept = 5, yintercept = 0.5))
project_singularity_subset <- data[data$ym == max(data$ym) & !data$country == "Invalid",
                                   j = switchover(.SD), by = "project"]
setnames(project_singularity_subset,2:3,c("Percentage_mobile","Total_pageviews"))
top_project_subset <- project_singularity_subset[project_singularity_subset$project %in% project_singularity_subset$project[order(project_singularity_subset$Total_pageviews, decreasing = TRUE)][1:20]]
ggsave(filename = "Graphs/Basic/switchover_by_project.png",
       plot = ggplot(data = top_project_subset,
                     aes(reorder(project,Percentage_mobile),Percentage_mobile)) + 
         geom_point(colour = "red") + coord_flip() + scale_y_continuous(labels = percent, limits = c(0,1)) +
         labs(title = "Percentage of pageviews from Mobile, per project",
              x = "Country", y = "Percentage") + geom_hline(xintercept = 5, yintercept = 0.5))

write.table(overall, file = file.path(getwd(),"Datasets","Basic","overall_pageviews.tsv"),
            row.names = FALSE, sep = "\t", quote = TRUE)
write.table(by_country_r2[by_country_r2$Last_Pageviews > 20000], file = file.path(getwd(),"Datasets","Basic","by_country_R2.tsv"),
            row.names = FALSE, sep = "\t", quote = TRUE)
write.table(by_project_r2[by_project_r2$Last_Pageviews > 20000], file = file.path(getwd(),"Datasets","Basic","by_project_R2.tsv"),
            row.names = FALSE, sep = "\t", quote = TRUE)
write.table(country_singularity_subset[country_singularity_subset$Total_pageviews > 20000], file = file.path(getwd(),"Datasets","Basic","by_country_singularity.tsv"),
            row.names = FALSE, sep = "\t", quote = TRUE)
write.table(project_singularity_subset[project_singularity_subset$Total_pageviews > 20000], file = file.path(getwd(),"Datasets","Basic","by_project_singularity.tsv"),
            row.names = FALSE, sep = "\t", quote = TRUE)