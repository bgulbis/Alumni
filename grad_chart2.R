library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggmap)

map <- get_map(location = "united states", zoom=4, source="google")
# ggmap(map)

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
map1 <- ggmap(map) + 
    geom_point(aes(x=lon, y=lat, shape=PGY2, color=PGY2), data=grads.jobs, alpha=0.8) 
    # geom_text(aes(x=lon, y=lat, label=First.Region), data=grads.city)
map1

map.hou <- get_map(location = "houston", source="google")
map2 <- ggmap(map.hou) + 
    geom_jitter(aes(x=lon, y=lat, shape=PGY2, color=PGY2), data=grads.jobs, alpha=0.8) +
    scale_shape_manual(values=c(3, 16)) +
    scale_color_manual(values=c('#999999','#E69F00')) 
    # scale_size_manual(values=c(2,3,4))
    
# geom_text(aes(x=lon, y=lat, label=First.Region), data=grads.city)
map2

