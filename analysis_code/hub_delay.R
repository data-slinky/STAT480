# Load the necessary packages
library(maps)
library(mapdata)
library(ggplot2)
library(plotly)

# Set the working directory
setwd("~/container-data/RDataScience/GroupProject/")

# Load airport data file
airports <- read.csv("airports.csv", header=TRUE)

# Load the files from Hive | Last two rows contains some warnings so we skip it during the reading
depdelay97 <- read.csv(text=paste0(head(readLines("depdelay_97.csv"), -2)), header=TRUE)
arrdelay97 <- read.csv(text=paste0(head(readLines("arrdelay_97.csv"), -2)), header=TRUE)
depdelay98 <- read.csv(text=paste0(head(readLines("depdelay_98.csv"), -2)), header=TRUE)
arrdelay98 <- read.csv(text=paste0(head(readLines("arrdelay_98.csv"), -2)), header=TRUE)

# Renaming the DataFrame columns to make things clearer
names(depdelay97) <- gsub("depdelay_97.", "", names(depdelay97))
names(arrdelay97) <- gsub("arrdelay_97.", "", names(arrdelay97))
names(depdelay98) <- gsub("depdelay_98.", "", names(depdelay98))
names(arrdelay98) <- gsub("arrdelay_98.", "", names(arrdelay98))

# Obtain the lat and long from the airports dataset
delay_hubs97 <-merge(depdelay97, arrdelay97, by.y = "dest", by.x = "origin")
delay_hubs98 <-merge(depdelay98, arrdelay98, by.y = "dest", by.x = "origin")

hubs_data97 <-merge(airports, delay_hubs97, by.y = "origin", by.x = "iata")
hubs_data98 <-merge(airports, delay_hubs98, by.y = "origin", by.x = "iata")


# Credit: We follow the example in the plotly documentation: https://cran.r-project.org/web/packages/plotly/plotly.pdf

# Get the quantiles for 1997
hubs_data97$hover <- paste(hubs_data97$airport, " | ", hubs_data97$dep_intensity, " dep. delays | ", hubs_data97$arr_intensity, " arr. delays")
hubs_data97$total_intensity <- hubs_data97$dep_intensity + hubs_data97$arr_intensity

hubs_data97$quant <- with(hubs_data97, cut(total_intensity, quantile(total_intensity)))
levels(hubs_data97$quant) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
hubs_data97$quant <- as.ordered(hubs_data97$quant) # There might be some NA if the data is too small
hubs_data97[is.na(hubs_data97)] <- "1st Quantile"

# Get the quantiles for 1998
hubs_data98$hover <- paste(hubs_data98$airport, " | ", hubs_data98$dep_intensity, " dep. delays | ", hubs_data98$arr_intensity, " arr. delays")
hubs_data98$total_intensity <- hubs_data98$dep_intensity + hubs_data98$arr_intensity

hubs_data98$quant <- with(hubs_data98, cut(total_intensity, quantile(total_intensity)))
levels(hubs_data98$quant) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
hubs_data98$quant <- as.ordered(hubs_data98$quant) # There might be some NA if the data is too small
hubs_data98[is.na(hubs_data98)] <- "1st Quantile"

# Plotting the data

theme1 <- list(scope = 'usa', projection = list(type = 'albers usa'), showland = TRUE, landcolor = toRGB("gray15"),
              subunitwidth = 1, countrywidth = 1, subunitcolor = toRGB("white"), countrycolor = toRGB("white"))

theme2 <- list(scope = 'usa', projection = list(type = 'albers usa'), showland = TRUE, landcolor = toRGB("gray50"),
               subunitwidth = 1, countrywidth = 1, subunitcolor = toRGB("white"), countrycolor = toRGB("white"))

plot97 <- plot_ly(hubs_data97, lon = long, lat = lat, text = hover,
        marker = list(size = sqrt(total_intensity/250) + 1),
        color = quant, type = 'scattergeo', locationmode = 'USA-states') %>%
        layout(geo = theme1, title="Flight Hubs Delay - 1997 <br> Hover for Detail or Click Legend to Subset",
        titlefont = list(family = "Arial, sans-serif", size = 16, color = "gray"))

plot97

plot98 <- plot_ly(hubs_data98, lon = long, lat = lat, text = hover,
        marker = list(size = sqrt(total_intensity/250) + 1),
        color = quant, type = 'scattergeo', locationmode = 'USA-states') %>%
        layout(geo = theme2, title="Flight Hubs Delay - 1998 <br> Hover for Detail or Click Legend to Subset",
        titlefont = list(family = "Arial, sans-serif", size = 16, color = "gray"))

plot98