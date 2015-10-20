library(dplyr)
library(tidyr)
library(maps)
library(stringr)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

## read in grads data
grads <- read.csv("pgy1_grads.csv", colClasses = "character")

grads.jobs <- grads %>%
    filter(First.Job != "") %>%
    mutate(PGY2 = as.logical(PGY2)) %>%
    mutate_geocode(First.Job, source="google") %>%
    group_by(First.Job, lon, lat, PGY2) %>%
    summarize(Count = n())

## get state borders
all_state <- map_data("state")

cols <- brewer.pal(5, "Blues")

## make map with exact points
graph1 <- ggplot() +
    geom_polygon(data=all_state, aes(x=long, y=lat, group=group), fill=cols[1], colour="black") +
    geom_jitter(aes(x=lon, y=lat, fill=PGY2), data=grads.jobs, shape=21, size=4, alpha=0.5) +
    # geom_jitter(aes(x=lon, y=lat), data=grads.jobs, size=4, color="black", shape=1) +
    scale_fill_manual(values=c("dark blue","dark green")) +
    theme_nothing() +
    coord_map()

# png(filename="grads.png", bg="transparent")
# print(graph1)
# dev.off()

tiff(filename="grads_points.tiff")
print(graph1)
dev.off()

grads.state <- grads %>%
    filter(First.State != "") %>%
    group_by(First.State) %>%
    summarize(Count = n())

states <- state.abb
dc <- data.frame(abb = "DC", region = "district of columbia")
states.dc <- data.frame(abb = state.abb, region = str_to_lower(state.name[match(states, state.abb)])) %>%
    bind_rows(dc)

state.grad <- grads.state %>%
    mutate(region = str_to_lower(First.State)) %>%
    select(-First.State) %>%
    right_join(all_state, by="region") %>%
    mutate(Count = ifelse(is.na(Count), 0, ifelse(Count <= 2, 3, ifelse(Count > 2, 5, Count))))

## make map of states with grads
graph2 <- ggplot() +
    geom_polygon(data=state.grad, aes(x=long, y=lat, group=group, fill=Count), colour="black") +
    scale_fill_continuous(low=cols[1], high=cols[5], guide=FALSE) +
    theme_nothing() +
#     labs(x="", y="", fill="Jobs") +
#     scale_x_continuous(breaks = NULL) +
#     scale_y_continuous(breaks = NULL) +
#     theme_bw() +
#     theme(panel.border = element_rect(fill=NULL, color="white")) +
    coord_map()

# png(filename="grads.png", bg="transparent")
# print(graph1)
# dev.off()
# 
tiff(filename="grads_state.tiff")
print(graph2)
dev.off()

setEPS()
postscript("grads.state.eps")
print(graph2)
dev.off()
