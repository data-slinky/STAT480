setwd("~/container-data/RDataScience/project")

library(plotly)
library(dplyr)
library(streamgraph)
library(treemap)

# Data Setup step

tot_occ <- read.csv(text=paste0(head(readLines("tot_occ.csv"), -2)), header = TRUE)

time_carr <- read.csv(text=paste0(head(readLines("time_carr.csv"), -2)), header = TRUE)
time_carr1 <- read.csv(text=paste0(head(readLines("time_carr1.csv"), -2)), header = TRUE)
delay_carr <- read.csv(text=paste0(head(readLines("delay_carr.csv"), -2)), header = TRUE)
delay_carr1 <- read.csv(text=paste0(head(readLines("delay_carr1.csv"), -2)), header = TRUE)

arr_carr_97 <- read.csv(text=paste0(head(readLines("arr_carr_97.csv"), -2)), header = TRUE)
dep_carr_97 <- read.csv(text=paste0(head(readLines("dep_carr_97.csv"), -2)), header = TRUE)
arr_carr_98 <- read.csv(text=paste0(head(readLines("arr_carr_98.csv"), -2)), header = TRUE)
dep_carr_98 <- read.csv(text=paste0(head(readLines("dep_carr_98.csv"), -2)), header = TRUE)




names(tot_occ) <- c("description", "count")
tot_occ$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(tot_occ$description))

names(time_carr) <- c("description", "year", "month", "day", "count")
time_carr$month <- formatC(time_carr$month,width=2,format="d",flag="0")
time_carr$day <- formatC(time_carr$day,width=2,format="d",flag="0")
time_carr$date <- paste(time_carr$year, "/", time_carr$month, "/", time_carr$day)
time_carr[, "date"] <- time_carr$date
time_carr$date <- gsub(" ","",time_carr$date) 
time_carr$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(time_carr$description))
time_carr <- subset(time_carr, select = -c(year,month,day) )

names(time_carr1) <- c("description", "year", "month", "count")
time_carr1$month <- formatC(time_carr1$month,width=2,format="d",flag="0")
time_carr1$date <- paste(time_carr1$year, "/", time_carr1$month, "/01")
time_carr1[, "date"] <- time_carr1$date
time_carr1$date <- gsub(" ","",time_carr1$date) 
time_carr1$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(time_carr1$description))
time_carr1 <- subset(time_carr1, select = -c(year,month) )





names(delay_carr) <- c("description", "depdelay", "arrdelay", "year", "month", "day")
delay_carr$month <- formatC(delay_carr$month,width=2,format="d",flag="0")
delay_carr$day <- formatC(delay_carr$day,width=2,format="d",flag="0")
delay_carr$date <- paste(delay_carr$year, "/", delay_carr$month,"/", delay_carr$day)
delay_carr[, "date"] <- delay_carr$date
delay_carr$date <- gsub(" ","",delay_carr$date) 
delay_carr$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(delay_carr$description))
delay_carr <- subset(delay_carr, select = -c(year,month,day) )

names(delay_carr1) <- c("description", "depdelay", "arrdelay", "year", "month")
delay_carr1$month <- formatC(delay_carr1$month,width=2,format="d",flag="0")
delay_carr1$date <- paste(delay_carr1$year, "/", delay_carr1$month,"/01")
delay_carr1[, "date"] <- delay_carr1$date
delay_carr1$date <- gsub(" ","",delay_carr1$date) 
delay_carr1$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(delay_carr1$description))
delay_carr1 <- subset(delay_carr1, select = -c(year,month) )





names(dep_carr_97) <- c("description", "count", "totcount")
names(arr_carr_97) <- c("description", "count", "totcount")
names(dep_carr_98) <- c("description", "count", "totcount")
names(arr_carr_98) <- c("description", "count", "totcount")


dep_carr_97$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(dep_carr_97$description))
arr_carr_97$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(arr_carr_97$description))
dep_carr_98$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(dep_carr_98$description))
arr_carr_98$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(arr_carr_98$description))


dep_carr_97$percentage <- 100*dep_carr_97$count/dep_carr_97$totcount
dep_carr_97[, "percentage"] <- dep_carr_97$percentage
dep_carr_97$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(dep_carr_97$description))
dep_carr_97 <- subset(dep_carr_97, select = -c(count,totcount) )

arr_carr_97$percentage <- 100*arr_carr_97$count/arr_carr_97$totcount
arr_carr_97[, "percentage"] <- arr_carr_97$percentage
arr_carr_97$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(arr_carr_97$description))
arr_carr_97 <- subset(arr_carr_97, select = -c(count,totcount) )

dep_carr_98$percentage <- 100*dep_carr_98$count/dep_carr_98$totcount
dep_carr_98[, "percentage"] <- dep_carr_98$percentage
dep_carr_98$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(dep_carr_98$description))
dep_carr_98 <- subset(dep_carr_98, select = -c(count,totcount) )

arr_carr_98$percentage <- 100*arr_carr_98$count/arr_carr_98$totcount
arr_carr_98[, "percentage"] <- arr_carr_98$percentage
arr_carr_98$description <- gsub("\\s*\\([^\\)]+\\)","",as.character(arr_carr_98$description))
arr_carr_98 <- subset(arr_carr_98, select = -c(count,totcount) )



d97 <- plot_ly(data=arr_carr_97, x = description, y = percentage, type = "bar", name = "arrival delay in 1997")                     #, session="knitr"
d97 <- add_trace(d97, data=dep_carr_97, x = description, y = percentage, type = "bar", name = "departure delay in 1997") %>% 
  layout(title = "Delay percentage v. Carrier, in 1997", showlegend=TRUE, xaxis = list(title = ""), margin = list(l = 100, r = 50, b = 100, t = 50))
d97


d98 <- plot_ly(data=arr_carr_98, x = description, y = percentage, type = "bar", name = "arrival delay in 1998")
d98 <- add_trace(d98, data=dep_carr_98, x = description, y = percentage, type = "bar", name = "departure delay in 1998") %>% 
  layout(title = "Delay percentage v. Carrier, in 1998", showlegend=TRUE, xaxis = list(title = ""), margin = list(l = 100, r = 50, b = 100, t = 50))
d98







st1 <- streamgraph(time_carr1, "description", "count", "date", offset="zero") %>%
  sg_fill_brewer("Spectral") %>% 
  sg_axis_x(tick_units = "date", tick_interval = 1, tick_format = "%Y/%m/%d") %>%
  sg_legend(TRUE, "Carrier: ")



st2 <- streamgraph(delay_carr, "description", "depdelay", "date", offset="zero") %>% 
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = "date", tick_interval = 1, tick_format = "%Y/%m/%d") %>%
  sg_legend(TRUE, "Carrier: ")







st3 <- streamgraph(delay_carr1, "description", "depdelay", "date", offset="zero") %>%
  sg_fill_brewer("Spectral") %>%  
  sg_axis_x(tick_units = "date", tick_interval = 1, tick_format = "%Y/%m/%d") %>%
  sg_legend(TRUE, "Carrier: ")





st4 <- streamgraph(delay_carr, "description", "arrdelay", "date", offset="zero", left = 100) %>% 
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = "date", tick_interval = 1, tick_format = "%Y/%m/%d") %>%
  sg_legend(TRUE, "Carrier: ")





st5 <- streamgraph(delay_carr1, "description", "arrdelay", "date", offset="zero", left =150) %>%
  sg_fill_brewer("Spectral") %>%  
  sg_axis_x(tick_units = "date", tick_interval = 1, tick_format = "%Y/%m/%d") %>%
  sg_legend(TRUE, "Carrier: ")

