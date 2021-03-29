rm (list = ls())

# Load packages

library(tidyverse)
library(readr)
library(ggthemes)
library(knitr)
library(maps)
library(dplyr)
library(readxl)

setwd("C:/Users/tanus/Dropbox/Geo-PKO working material for v 2.1/Dataset")
GeoPKO <- read_xlsx("Geo_PKO_v.2.1.xlsx")

### NOTE: all paths will need to be redirected

## prep for mapping

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)
library(viridis)
library(ggpubr)

world <- ne_countries(scale = 50, returnclass = "sf")

# converting from factor to numeric
GeoPKO$no.troops <- as.numeric(GeoPKO$no.troops)
GeoPKO$longitude <- as.numeric(GeoPKO$longitude)
GeoPKO$latitude <- as.numeric(GeoPKO$latitude)



### WORLD MAP - map for maximum deployment size of all time


# dataframe

alltimemax1 <- GeoPKO %>%
    select(year, mission, month, location, latitude, longitude, no.troops, country, hq)


alltimemax2 <- alltimemax1 %>%
    group_by(location, latitude, longitude, mission, country,hq) %>%
    dplyr::summarise(max = max(no.troops)) %>%
    ungroup()


# visualisation

worldpkomax <- ggplot(data=world) +

    geom_sf() +

    coord_sf(xlim = c(-90, 125), ylim = c(-35, 55)) +

    geom_point(data=alltimemax2,
               aes(x=longitude, y=latitude, size=max, color=max, alpha=max, stroke=1))+

    #   labs(title="UN Global Peacekeeping Deployments, 1994-2020")+
    labs(caption="Source: Geo-PKO v2.0")+

    scale_size(name="Largest Troop Deployment", range = c(0.5, 6), breaks = c(10, 100, 500, 1000, 3000, 4500, 6000, 7500))+

    scale_color_viridis(option="viridis", name="Largest Troop Deployment", breaks=c(10, 100, 500, 1000, 3000, 4500, 6000, 7500))+

    scale_alpha_continuous(name="Largest Troop Deployment", range=c(0.3,1), breaks=c(10, 100, 500, 1000, 3000, 4500, 6000, 7500))+

    theme(
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
        axis.title=element_blank(),
        axis.ticks=element_line(),
        axis.text=element_text(),
        panel.background = element_rect(fill = 'white'),
        plot.caption = element_text(hjust = 0.5),
        legend.direction = "vertical",
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.key=element_blank(),
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(0, 10, 10, 0)),
        legend.text = element_text(
            margin = margin(r = 10, unit = "pt"))
    ) +

    guides(colour=guide_legend(ncol=2),
           size=guide_legend(ncol=2)) +

    xlab("longitude") +
    ylab("latitude")

worldpkomax

ggsave("worldpkomax.png", path = "C:/Users/tanus/Dropbox/Geo-PKO working material for v 2.0/Dataset v2.1 Working Folder/PCR Page Visualisations")



### AFRICA MAP

# dataframes (created for both africa and middle east for viz purposes)

africadf <- GeoPKO %>%
    filter(country%in%c("Algeria", "Central African Republic", "DRC", "Egypt", "Israel", "Mali", "Lebanon", "Rwanda", "South Sudan", "Sudan", "Syria", "Uganda", "Western Sahara"), year==2020) %>%
    select(mission, month, location, latitude, longitude, no.troops, country, hq)

africadf1 <- africadf %>%
    group_by(location, latitude, longitude, mission, country,hq) %>%
    dplyr::summarize(ave = mean(no.troops, na.rm=TRUE)) %>%
    ungroup()

withoutmelabs <- africadf <- GeoPKO %>%
    filter(country%in%c("Algeria", "Central African Republic", "DRC", "Egypt", "Mali", "Rwanda", "South Sudan", "Sudan", "Uganda", "Western Sahara"), year==2020) %>%
    select(mission, month, location, latitude, longitude, no.troops, country, hq)

withoutmelabs1 <- withoutmelabs %>%
    group_by(location, latitude, longitude, mission, country,hq) %>%
    dplyr::summarize(ave = mean(no.troops, na.rm=TRUE)) %>%
    ungroup()


# visualisation

africapko <- ggplot(data=world) +

    geom_sf() +

    coord_sf(xlim = c(-20, 55), ylim = c(-40, 40)) +

    geom_point(data=africadf1,
               aes(x=longitude, y=latitude, size=ave, color=mission, stroke=1.5), alpha=.4)+

    geom_point(
        data=withoutmelabs1 %>%
            filter(hq==3),
        aes(x=longitude, y=latitude), color="black", shape=16, size=3
    ) +

    geom_label_repel(
        data=withoutmelabs1 %>%
            filter(hq==3),
        min.segment.length = 0.2,
        label.size = 0.5,
        box.padding = 2,
        size = 3,
        aes(x=longitude, y=latitude, label=mission)
    ) +

#    labs(title="UN Peacekeeping Deployment and Mission HQs - Africa and the Middle East, 2020")+

    labs(size="Average Troop Deployment",col="Mission")+

    labs(caption="Source: Geo-PKO v2.0")+

    scale_size(range = c(1, 16), breaks = c(10, 100, 500,1000,2000,4000))+

    theme(
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
        axis.title=element_blank(),
        axis.ticks=element_line(),
        axis.text=element_text(),
        panel.background = element_rect(fill = 'white'),
        legend.position="right",
        legend.direction="vertical",
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.key=element_blank(),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(10, 10, 10, 10)),
        legend.text = element_text(
            margin = margin(r = 10, unit = "pt"))
    ) +

    xlab("longitude") +
    ylab("latitude")

africapko

ggsave("africapko.png", path = "C:/Users/tanus/Dropbox/Geo-PKO working material for v 2.0/Dataset v2.1 Working Folder/PCR Page Visualisations")



### MIDDLE EAST MAP


# dataframe

medf <- GeoPKO %>%
    filter(country%in%c("Egypt", "Israel", "Lebanon", "Syria"), year==2020) %>%
    select(mission, month, location, latitude, longitude, no.troops, country, hq)

medf1 <- medf %>%
    group_by(location, latitude, longitude, mission, country,hq) %>%
    dplyr::summarize(ave = mean(no.troops, na.rm=TRUE)) %>%
    ungroup()


# visualisation

mepko <- ggplot(data=world) +

    geom_sf() +

    coord_sf(xlim = c(32, 37), ylim = c(30, 34)) +

    geom_point(data=medf1,
               aes(x=longitude, y=latitude, size=ave, color=mission, stroke=1.5), alpha=.3)+

    geom_point(
        data=medf1 %>%
            filter(hq==3),
        aes(x=longitude, y=latitude), color="black", shape=16, size=2
    ) +

    geom_label_repel(
        data=medf1 %>%
            filter(hq==3),
        min.segment.length = 0.2,
        label.size = 0.5,
        box.padding = 2,
        size = 4,
        aes(x=longitude, y=latitude, label=mission)
    ) +

    labs(title="UN Peacekeeping Deployment and Mission HQs - Middle East, 2020")+

    labs(size="Average Troop Deployment",col="Mission")+

    labs(caption="Source: Geo-PKO v2.0")+

    scale_size(range = c(2, 10), breaks = c(10, 100, 500,1000,2000, 4000))+

    theme(
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
        axis.title=element_blank(),
        axis.ticks=element_line(),
        axis.text=element_text(),
        panel.background = element_rect(fill = 'white'),
        legend.position="right",
        legend.direction="vertical",
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.key=element_blank(),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(10, 10, 10, 10)),
        legend.text = element_text(
            margin = margin(r = 10, unit = "pt"))
    ) +

    xlab("longitude") +
    ylab("latitude")

mepko

ggsave("mepko.png", path = "C:/Users/tanus/Dropbox/Geo-PKO working material for v 2.0/Dataset v2.1 Working Folder/PCR Page Visualisations")



### EUROPE MAP


# dataframe

europedf <- GeoPKO %>%
    filter(country%in%c("Kosovo", "Cyprus"), year==2020) %>%
    select(mission, month, location, latitude, longitude, no.troops, country, hq)

europedf1 <- europedf %>%
    group_by(location, latitude, longitude, mission, country,hq) %>%
    dplyr::summarize(ave = mean(no.troops, na.rm=TRUE)) %>%
    ungroup()

# visualisation

europepko <- ggplot(data=world) +

    geom_sf() +

    coord_sf(xlim = c(18, 38), ylim = c(34, 44)) +

    geom_point(data=europedf1,
               aes(x=longitude, y=latitude, size=ave, color=mission, stroke=1.5), alpha=.4)+

    geom_point(
        data=europedf1 %>%
            filter(hq==3),
        aes(x=longitude, y=latitude), color="black", shape=16, size=2
    ) +

    geom_label_repel(
        data=europedf1 %>%
            filter(hq==3),
        min.segment.length = 0.2,
        label.size = 0.5,
        box.padding = 2,
        size = 4,
        aes(x=longitude, y=latitude, label=mission)
    ) +

    #    labs(title="UN Peacekeeping Deployment and Mission HQs - Europe, 2020")+

    labs(size="Average Troop Deployment",col="Mission")+

    labs(caption="Source: Geo-PKO v2.0")+

    scale_size(range = c(2, 8), breaks = c(10, 50, 100, 200))+

    theme(
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
        axis.title=element_blank(),
        axis.ticks=element_line(),
        axis.text=element_text(),
        panel.background = element_rect(fill = 'white'),
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(0, 10, 10, 0)),
        legend.key=element_blank(),
        legend.text = element_text(
            margin = margin(r = 10, unit = "pt"))
    ) +

    xlab("longitude") +
    ylab("latitude")

europepko

ggsave("europepko.png", path = "C:/Users/tanus/Dropbox/Geo-PKO working material for v 2.0/Dataset v2.1 Working Folder/PCR Page Visualisations")



### CENTRAL AMERICA MAP (no deployments from 2020)

# dataframe

cadf <- GeoPKO %>%
    filter(country%in%c("Haiti"), year==2019) %>%
    select(mission, month, location, latitude, longitude, no.troops, country, hq)

cadf1 <- cadf %>%
    group_by(location, latitude, longitude, mission, country,hq) %>%
    dplyr::summarize(ave = mean(no.troops, na.rm=TRUE)) %>%
    ungroup()


# visualisation

capko <- ggplot(data=world) +

    geom_sf() +

    coord_sf(xlim = c(-85,-70), ylim = c(10, 25)) +

    geom_point(data=cadf1,
               aes(x=longitude, y=latitude, color=mission, stroke=1.5), alpha=.7)+

    geom_point(
        data=cadf1 %>%
            filter(hq==3),
        aes(x=longitude, y=latitude), color="black", shape=16, size=6
    ) +

    geom_label_repel(
        data=cadf1 %>%
            filter(hq==3),
        min.segment.length = 0.2,
        label.size = 0.5,
        box.padding = 2,
        size = 4,
        aes(x=longitude, y=latitude, label=mission)
    ) +

    #    labs(title="UN Peacekeeping Deployment and Mission HQs - Central America, 2020")+
    labs(caption="Source: Geo-PKO v2.0")+

    labs(col="Mission")+

    theme(
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
        axis.title=element_blank(),
        axis.ticks=element_line(),
        axis.text=element_text(),
        panel.background = element_rect(fill = 'white'),
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(0, 10, 10, 0)),
        legend.key=element_blank(),
        legend.text = element_text(
            margin = margin(r = 10, unit = "pt"))
    ) +

    xlab("longitude") +
    ylab("latitude")

capko

ggsave("capko.png", path = "C:/Users/tanus/Dropbox/Geo-PKO working material for v 2.0/Dataset v2.1 Working Folder/PCR Page Visualisations")




### ADDITIONAL MAPS (not currently on PCR page)



## WORLD MAP for an all-time worldaverage


# dataframe

alltimemap1 <- GeoPKO %>%
  select(year, mission, month, location, latitude, longitude, no.troops, country, hq)


alltimemap2 <- alltimemap1 %>%
  group_by(location, latitude, longitude, mission, country,hq) %>%
  dplyr::summarize(ave = mean(no.troops, na.rm=TRUE)) %>%
  ungroup()


# visualisation

worldpko <- ggplot(data=world) +

  geom_sf() +

  coord_sf(xlim = c(-90, 125), ylim = c(-35, 55)) +

  geom_point(data=alltimemap2,
             aes(x=longitude, y=latitude, size=ave, color=ave, stroke=1.5), alpha=.3)+

  #   labs(title="UN Global Peacekeeping Deployments, 1994-2020")+

  labs(size="Average Troop Deployment",col="Average Troop Deployment")+

  scale_size(range = c(1, 10), breaks = c(1, 10, 100, 500, 1000, 2000, 4000, 5000))+

  guides(col = guide_legend(ncol=1),
         size = guide_legend(order = 1)) +

  theme(
    panel.grid=element_blank(),
    panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
    axis.title=element_blank(),
    axis.ticks=element_line(),
    axis.text=element_text(),
    panel.background = element_rect(fill = 'white'),
    legend.direction = "vertical",
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key=element_blank(),
    legend.title = element_text(),
    legend.spacing = unit(0.4, "cm"),
    legend.box.spacing = unit(1, "cm"),
    legend.margin = margin(c(0, 10, 10, 0)),
    legend.text = element_text(
      margin = margin(r = 10, unit = "pt"))
  ) +

  xlab("longitude") +
  ylab("latitude")


worldpko

ggsave("worldpko.png")



## WORLD MAP - YEARLY


## creating the dataframe

# filter for year and variables

mapdf <- GeoPKO %>%
  filter(year==2020) %>%
  select(mission, month, location, latitude, longitude, no.troops, country, hq)

# average the troop numbers over months

mapdf1 <- mapdf %>%
  group_by(location, latitude, longitude, mission, country,hq) %>%
  dplyr::dplyr::summarize(ave = mean(no.troops, na.rm=TRUE)) %>%
  ungroup()


## visualisation

worldpko <- ggplot(data=world) +

  geom_sf() +

  coord_sf(xlim = c(-90, 125), ylim = c(-35, 55)) +

  geom_point(data=mapdf1,
             aes(x=longitude, y=latitude, size=ave, color=country), alpha=.4)+

  geom_point(
    data=mapdf1 %>%
      filter(hq==3),
    aes(x=longitude, y=latitude), color="black", shape=16, size=2
  ) +

  geom_label_repel(
    data=mapdf1 %>%
      filter(hq==3),
    min.segment.length = 0.2,
    label.size = 0.5,
    box.padding = 2,
    size = 3,
    aes(x=longitude, y=latitude, label=mission)
  ) +

  labs(title="UN Global Peacekeeping Deployment and Mission HQs, 2020")+

  labs(size="Average Troop Deployment",col="Country")+

  labs(caption="Source: Geo-PKO v2.0")+

  scale_size(range = c(1, 10), breaks = c(10, 100, 500, 1000, 2000,4000))+

  guides(col = guide_legend(ncol=3),
         size = guide_legend(order = 1)) +

  theme(
    panel.grid=element_blank(),
    panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
    axis.title=element_blank(),
    axis.ticks=element_line(),
    axis.text=element_text(),
    panel.background = element_rect(fill = 'white'),
    legend.direction = "vertical",
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(),
    legend.key=element_blank(),
    legend.spacing = unit(0.4, "cm"),
    legend.box.spacing = unit(1, "cm"),
    legend.margin = margin(c(0, 10, 10, 0)),
    legend.text = element_text(
      margin = margin(r = 10, unit = "pt"))
  ) +

  xlab("longitude") +
  ylab("latitude")

worldpko

ggsave("worldpko.png", path = "C:/Users/tanus/Dropbox/Geo-PKO working material for v 2.0/Dataset v2.1 Working Folder/PCR Page Visualisations")

