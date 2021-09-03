# Xander Crawford INFO250 Final Project code

library(tidyverse)
library(ggradar)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(dplyr)

library(maps)
library(mapdata)

# Data prep
data <- read.csv('dataset.csv', as.is <- T)
data <- data[data$yr > 1929 & data$yr < 2021,]

# bubble plot
ggplot(data = data) +
  geom_point(aes(x = yr,
                 y = production_million_bushels,
                 size = harvested_million_acres,
                 color = crop
                 ),
             alpha=.6) +
  scale_x_continuous("Year",
               limits=c(1930, 2020),
               breaks = c(1930, 1945, 1960, 1975, 1990, 2005, 2020)) +
  scale_y_continuous("Production (Million bushels)",
                     breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000)) +
  scale_colour_brewer('Crop',
                      palette = "Set2"
                    ) +
  #facet_wrap(~crop) +
  theme_clean() +
  theme(legend.position = "top") +
  theme(plot.caption = element_text(vjust = .5)) + 
  theme(plot.title.position = "panel",
        plot.title=element_text(face="bold"),
        plot.background = element_rect(fill = "white",
                                       colour = "white")) +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  labs(title = "United States Feed Grain Supply (1930-2020)",
       subtitle = "A timeline of United States feed grain harvest from 1930-2020. The height of the bubble \nvisualizes the amount of crop harvested and the size of the point is relative to the land \narea harvested.",
       caption = "Note: Production = the mesured amount of the crop harvested in bushels\nSource: USDA Economic Research Service",
       size = "Area Harvested (Million Acres)")

# save bubble plot
ggsave("harvest_bubble.svg",
       width = 10, height = 6, unit = "in",
       dpi = 400)

# Map data
usa <- map_data('usa')
state <- map_data("state")

# harvest area data
abc <- data[data$yr == 2020,]$harvested_million_acres

# Calculate size percentages
million_acres_us = 1945
million_acres_pa = 28806/1000
sum(abc)/million_acres_us
million_acres_pa/million_acres_us

# Calculate corn share of production in 2020
cba <- data[data$yr == 2020,]$production_million_bushels
cba[1]/sum(cba)

acerage_to_geo <- function(million_acres, long, lat, gr){
  # Convert acreage to square miles than compute a square using geo points
  square_miles = million_acres*1000000/640
  miles = sqrt(square_miles)
  one_geo_point = 69
  side_length = miles/one_geo_point
  
  area = data.frame(
    g = rep(gr, 1),
    x = long,
    x1 = long + side_length,
    y = lat,
    y1 = lat + side_length,
    acres = million_acres
  )
  return(area)
}

# Calculate areas
x1 = -113
y1 = 36
area_barley <- acerage_to_geo(abc[2], x1, y1, "Barley")
area_corn <- acerage_to_geo(abc[1], area_barley$x1 + 7, y1, "Corn")
area_oat <- acerage_to_geo(abc[3], area_corn$x1 + 7, y1, "Oats")
area_soghrum <- acerage_to_geo(abc[4], area_oat$x1 + 7, y1, "Sorghum")
areas = rbind(area_barley, area_corn, area_oat, area_soghrum)

# Plot map with 2020 land areas
ggplot() +
  geom_polygon(data=state, 
               aes(x=long, 
                   y=lat, 
                   #fill=region, 
                   group=group),
               show.legend = NA,
               color="white", 
               fill = "grey80",
               alpha=.5) +
  coord_fixed(1.3) +
  geom_rect(data = areas,
            aes(
              xmin = x,
              xmax = x1,
              ymin = y,
              ymax = y1,
              fill = g),
            alpha=.7) +
  scale_fill_brewer('Crop',
                      palette = "Set2") +
  geom_text(data = areas, 
            aes(
              family="sans",
              fontface="italic",
              x=x1,
              y=y,
              label=paste(acres, "Million Acres", sep = " "),
              hjust=-0.01, 
              vjust=-0.3
            )) +
  theme_map(base_size = 12, base_family = "sans") +
  theme(plot.title.position = "panel",
        plot.title=element_text(face="bold", size=20),
        plot.background = element_rect(fill = "white",
                                       colour = "white")) +
  theme(legend.title=element_text(face="bold"),
        legend.box.background = element_rect(colour = "grey40", size=1.5)) +
  labs(title = "Feed Grain Harvest Land Area Comparison (2020)",
      subtitle = "The colored squares are the combined area of harvest for feed grain crops relative to the size of the United States in 2020.",
      caption = "Note: Area projection is approximate to the size of the United States\nSource: USDA Economic Research Service")

ggsave("land_area.svg",
       width = 16, height = 9, unit = "in",
       dpi = 400)

# radar plots

# Get data and prep
usage <- read.csv('radar_vals.csv', as.is=T)
usage$crop <- as.factor(usage$crop)
usage$yr <- as.factor(usage$yr)

str(usage)

a <- usage %>% filter(crop == "Barley")
rownames(a) <- a$yr
a<- subset(a , select = -c(crop, yr) )

df <- a  %>% rownames_to_column("group")
# Start calculating jump in consumption
con_jump = df$Consumption[3] - df$Consumption[2]


# Barley plot
ggradar(
  df, 
  values.radar = c("%0", "%37.5", "%75"),
  grid.min = 0, grid.mid = .375, grid.max = .75,
  # Polygons
  group.line.width = .7, 
  group.point.size = 2,
  group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Background and grid lines
  gridline.min.linetype = "solid",
  gridline.mid.linetype = "dotted",
  gridline.max.linetype = "solid",
  gridline.max.colour = "white",
  axis.labels = c("Food/Alcohol/Industrial",	"Seed",	"Feed",	"Exports",	"End Stock"),
  legend.position = "top",
  legend.title = "Year",
  background.circle.colour = "#66C2A5",
  plot.title = "Barley Disappearance (1975 - 2020)"
) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 7)) +
  labs(caption = "Source: USDA Economic Research Service")

# save barley plot
ggsave("barley.svg",
       width = 10, height = 6, unit = "in",
       dpi = 400)

# Corn plot
a <- usage %>% filter(crop == "Corn")
rownames(a) <- a$yr
a<- subset(a , select = -c(crop, yr) )

df <- a  %>% rownames_to_column("group")
rbind(con_jump, df$Consumption[3] - df$Consumption[2])
ggradar(
  df, 
  values.radar = c("%0", "%37.5", "%75"),
  grid.min = 0, grid.mid = .375, grid.max = .75,
  # Polygons
  group.line.width = .7, 
  group.point.size = 2,
  group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Background and grid lines
  gridline.min.linetype = "solid",
  gridline.mid.linetype = "dotted",
  gridline.max.linetype = "solid",
  gridline.max.colour = "white",
  axis.labels = c("Food/Alcohol/Industrial",	"Seed",	"Feed",	"Exports",	"End Stock"),
  background.circle.colour = "#FC8D62",
  legend.position = "top",
  legend.title = "Year",
  plot.title = "Corn Disappearance (1975 - 2020)"
) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 7)) + 
  labs(caption = "Source: USDA Economic Research Service")

# save corn plot
ggsave("corn.svg",
       width = 10, height = 6, unit = "in",
       dpi = 400)

# Oats plot
a <- usage %>% filter(crop == "Oats")
rownames(a) <- a$yr
a<- subset(a , select = -c(crop, yr) )

df <- a  %>% rownames_to_column("group")
#calculating jump in consumption
rbind(con_jump, df$Consumption[3] - df$Consumption[2])
ggradar(
  df, 
  values.radar = c("%0", "%37.5", "%75"),
  grid.min = 0, grid.mid = .375, grid.max = .75,
  # Polygons
  group.line.width = .7, 
  group.point.size = 2,
  group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Background and grid lines
  gridline.min.linetype = "solid",
  gridline.mid.linetype = "dotted",
  gridline.max.linetype = "solid",
  gridline.max.colour = "white",
  axis.labels = c("Food/Alcohol/Industrial",	"Seed",	"Feed",	"Exports",	"End Stock"),
  background.circle.colour = "#8DA0CB",
  legend.position = "top",
  legend.title = "Year",
  plot.title = "Oat Disappearance (1975 - 2020)"
) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 7)) +
  labs(caption = "Source: USDA Economic Research Service")

# save oats plot
ggsave("oats.svg",
       width = 10, height = 6, unit = "in",
       dpi = 400)

a <- usage %>% filter(crop == "Sorghum")
rownames(a) <- a$yr
a<- subset(a , select = -c(crop, yr) )

df <- a  %>% rownames_to_column("group")
rbind(con_jump, df$Consumption[3] - df$Consumption[2])
ggradar(
  df, 
  values.radar = c("%0", "%37.5", "%75"),
  grid.min = 0, grid.mid = .375, grid.max = .75,
  # Polygons
  group.line.width = .7, 
  group.point.size = 2,
  group.colours = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Background and grid lines
  gridline.min.linetype = "solid",
  gridline.mid.linetype = "dotted",
  gridline.max.linetype = "solid",
  gridline.max.colour = "white",
  axis.labels = c("Food/Alcohol/Industrial",	"Seed",	"Feed",	"Exports",	"End Stock"),
  background.circle.colour = "#E78AC3",
  legend.position = "top",
  legend.title = "Year",
  plot.title = "Sorghum Disappearance (1975 - 2020)"
) + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 7)) +
  labs(caption = "Source: USDA Economic Research Service")

# save bubble plot
ggsave("sorghum.svg",
       width = 10, height = 6, unit = "in",
       dpi = 400)

# calculate the consumption jump
sum(con_jump)/4
con_jump
