library(dplyr)
library(tidyr)
# library(googleVis)
# 
grads <- read.csv("pgy1_grads.csv", colClasses = "character")

grads.jobs <- grads %>%
    filter(First.Job != "") %>%
    mutate(PGY2 = as.logical(PGY2)) %>%
    mutate_geocode(First.Job, source="google") %>%
    group_by(First.Job, lon, lat, PGY2) %>%
    summarize(Count = n())


# 
# grads.city <- grads %>%
#     filter(First.Region != "") %>%
#     group_by(First.Region) %>%
#     summarize(Count = n(),
#               Val = 1)
# 
# g1 <- gvisGeoMap(grads.city, locationvar = "First.Region", numvar = "Count", 
#                  options=list(region="US", dataMode="markers"))
# plot(g1)
# 
grads.state <- grads %>%
    filter(First.State != "") %>%
    group_by(First.State) %>%
    summarize(Count = n())
# 
# g1a <- gvisGeoMap(grads.state, locationvar = "First.State", numvar = "Count", 
#                   options=list(region="US", dataMode="regions"))
# plot(g1a)
# 
# g1b <- gvisGeoMap(grads.city, locationvar = "First.Region", numvar = "Count", 
#                  options=list(region="US-TX", dataMode="markers"))
# plot(g1b)
# 
# grads.city.curr <- grads %>%
#     filter(Current.Region != "") %>%
#     group_by(Current.Region) %>%
#     summarize(Count = n())
# 
# g2 <- gvisGeoMap(grads.city.curr, locationvar = "Current.Region", numvar = "Count", 
#                  options=list(region="US", dataMode="markers"))
# plot(g2)
# 
# grads.pgy2 <- grads %>%
#     filter(PGY2 != "", Grad.Year >= 2009) %>%
#     group_by(PGY2) %>%
#     summarize(Count = n())
# 
# grads2 <- grads %>%
#     gather("type", "region", 10:11)
# 
# grads2.sum <- grads2 %>%
#     group_by(type, region) %>%
#     filter(region != "") %>%
#     summarize(count = n(),
#               val = 1)
# 
# g3 <- gvisGeoMap(grads2.sum, locationvar = "region", numvar = "count", 
#                  options=list(region="US", dataMode="markers"))
# plot(g3)

library(maps)
library(stringr)
library(ggplot2)
library(RColorBrewer)

all_state <- map_data("state")
states <- state.abb
dc <- data.frame(abb = "DC", region = "district of columbia")
states.dc <- data.frame(abb = state.abb, region = str_to_lower(state.name[match(states, state.abb)])) %>%
    bind_rows(dc)

state.grad <- grads.state %>%
    mutate(region = str_to_lower(First.State)) %>%
    select(-First.State) %>%
    right_join(all_state, by="region") %>%
    mutate(Count = ifelse(is.na(Count), 0, ifelse(Count > 2, 3, Count)))

cols <- brewer.pal(3, "Blues")
graph1 <- ggplot() +
    geom_polygon(data=state.grad, aes(x=long, y=lat, group=group, fill=Count), colour="black") +
    scale_fill_continuous(low=cols[1], high=cols[3], guide=FALSE) +
    labs(x="", y="", fill="Jobs") +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    theme_bw() +
    theme(panel.border = element_rect(fill=NULL, color="white")) +
    coord_map()

png(filename="grads.png", bg="transparent")
print(graph1)
dev.off()

tiff(filename="grads.tiff")
print(graph1)
dev.off()

graph2 <- ggplot() +
    geom_polygon(data=all_state, aes(x=long, y=lat, group=group), fill=cols[1], colour="black") +
    geom_jitter(aes(x=lon, y=lat, fill=PGY2), data=grads.jobs, shape=21, size=4, alpha=0.5) +
    # geom_jitter(aes(x=lon, y=lat), data=grads.jobs, size=4, color="black", shape=1) +
    scale_fill_manual(values=c("dark blue","dark green")) +
    theme_nothing() +
    coord_map()

graph2