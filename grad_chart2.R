library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggmap)

grads <- read.csv("pgy1_grads.csv", colClasses = "character")

# grads.city <- grads %>%
#     filter(First.Region != "") %>%
#     group_by(First.Region) %>%
#     summarize(Count = n()) 

grads.jobs <- grads %>%
    filter(First.Job != "") %>%
    mutate(PGY2 = as.logical(PGY2)) %>%
    mutate_geocode(First.Job, source="google") %>%
    group_by(First.Job, lon, lat, PGY2) %>%
    summarize(Count = n())

# lonlat <- lapply(grads.city$First.Region, geocode, source="google")
# lonlat <- lapply(grads.jobs$First.Job, geocode, source="google")
# lonlatdf <- do.call(rbind, lonlat)
# grads.jobs <- cbind(grads.jobs, lonlatdf)
# 

map <- get_map(location = "united states", zoom=4, source="google")

map1 <- ggmap(map) + 
    geom_jitter(aes(x=lon, y=lat, color=PGY2), data=grads.jobs, size=4, alpha=0.6) + 
    geom_jitter(aes(x=lon, y=lat), data=grads.jobs, size=4, color="black", shape=1) +
    scale_color_manual(values=c("red","blue")) +
    theme_nothing()
map1

map.hou <- get_map(location = "houston", source="google")
map.hou.osm <- get_map(location="houston", source="osm")

map2 <- ggmap(map.hou) + 
    geom_jitter(aes(x=lon, y=lat, color=PGY2), data=grads.jobs, size=4, alpha=0.5) +
    geom_jitter(aes(x=lon, y=lat), data=grads.jobs, size=4, color="black", shape=1) +
    scale_color_manual(values=c("red","blue")) +
    theme_nothing()
map2

