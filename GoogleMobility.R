#############################################################
## Heatmaps - Google Mobility Data
## Graeme Meiklejohn
## April 2020
#############################################################

##############################
# Load librairies ----
##############################
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyr)
## Make sure you load rgeos before maptools - otherwise there's a permission thing that stops maptools working properly
library(rgeos)
library(maptools)
library(rgdal)
library(readxl)
library(grid)
library(gridExtra)
library(data.table)
library(lubridate)

##############################
#  Functions ----
##############################





##############################
#  Code ----
##############################

#Load in data
raw_data_perc_fall <- read_excel("data/profiles.xlsx",sheet=2) %>% as_tibble() 
raw_data_perc_fall_sc <- raw_data_perc_fall %>% filter(REGION_NM == "Scotland")
raw_data_int_perc_fall <- read_excel("data/profiles.xlsx",sheet=3) %>%
  as_tibble() %>% 
  filter(!(Place %in% c("China","Russia","Egypt")))%>% 
  gather(sector, value,
         `Retail & recreation`,
         `Grocery & pharmacy`,
         Parks  ,
         `Transit stations`,
         Workplace, 
         Residential)

  


# Visualisations

#Compare All
ggplot() +
  geom_col(data=raw_data_int_perc_fall, aes(x=sector, y=value, group=sector, fill = sector)) +
  facet_wrap(~Place) +
  theme_classic() +
  labs(x = "",
       y = "Percentage difference to Baseline %",
       title="Mobility Trends using Google Analytics Data",
       subtitle = "16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())


# Comparing Specific Sectors
unique(raw_data_int_perc_fall$sector)

retail_int <- ggplot() +
  geom_col(data=raw_data_int_perc_fall %>% filter(sector=="Retail & recreation"), 
           aes(x=reorder(Place, -value), y=value, fill = value)) +
  theme_classic() +
  theme(axis.text.x = element_text(face="italic", 
                                     size=12, angle=45, hjust = 1),
          axis.text.y = element_text(face="italic", 
                                     size=12, angle=45))+
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Retail & Recreation") +
  labs(x = "",
       y = "Percentage difference to Baseline %",
       title="Mobility Trends using Google Analytics Data",
       subtitle = "16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") 


food_int <- ggplot() +
  geom_col(data=raw_data_int_perc_fall %>% filter(sector=="Grocery & pharmacy"), 
           aes(x=reorder(Place, -value), y=value, fill = value)) +
  theme_classic() +
  theme(axis.text.x = element_text(face="italic", 
                                   size=12, angle=45, hjust = 1),
        axis.text.y = element_text(face="italic", 
                                   size=12, angle=45))+
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Food Retailers & Pharmacies") +
  labs(x = "",
       y = "Percentage difference to Baseline %",
       title="Mobility Trends using Google Analytics Data",
       subtitle = "16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") 

parks_int <- ggplot() +
  geom_col(data=raw_data_int_perc_fall %>% filter(sector=="Parks"), 
           aes(x=reorder(Place, -value), y=value, fill = value)) +
  theme_classic() +
  theme(axis.text.x = element_text(face="italic", 
                                   size=12, angle=45, hjust = 1),
        axis.text.y = element_text(face="italic", 
                                   size=12, angle=45))+
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Parks") +
  labs(x = "",
       y = "Percentage difference to Baseline %",
       title="Mobility Trends using Google Analytics Data",
       subtitle = "16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") 

transit_int <- ggplot() +
  geom_col(data=raw_data_int_perc_fall %>% filter(sector=="Transit stations"), 
           aes(x=reorder(Place, -value), y=value, fill = value)) +
  theme_classic() +
  theme(axis.text.x = element_text(face="italic", 
                                   size=12, angle=45, hjust = 1),
        axis.text.y = element_text(face="italic", 
                                   size=12, angle=45))+
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Transit stations") +
  labs(x = "",
       y = "Percentage difference to Baseline %",
       title="Mobility Trends using Google Analytics Data",
       subtitle = "16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") 

workplace_int <- ggplot() +
  geom_col(data=raw_data_int_perc_fall %>% filter(sector=="Workplace"), 
           aes(x=reorder(Place, -value), y=value, fill = value)) +
  theme_classic() +
  theme(axis.text.x = element_text(face="italic", 
                                   size=12, angle=45, hjust = 1),
        axis.text.y = element_text(face="italic", 
                                   size=12, angle=45))+
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Workplaces") +
  labs(x = "",
       y = "Percentage difference to Baseline %",
       title="Mobility Trends using Google Analytics Data",
       subtitle = "16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") 

residential_int <- ggplot() +
  geom_col(data=raw_data_int_perc_fall %>% filter(sector=="Residential"), 
           aes(x=reorder(Place, -value), y=value, fill = value)) +
  theme_classic() +
  theme(axis.text.x = element_text(face="italic", 
                                   size=12, angle=45, hjust = 1),
        axis.text.y = element_text(face="italic", 
                                   size=12, angle=45))+
  scale_fill_gradient(low = "#AAD9A7", high = "#1E7019",na.value = "grey40",
                      name = "% Change in Mobility to Residential") +
  labs(x = "",
       y = "Percentage difference to Baseline %",
       title="Mobility Trends using Google Analytics Data",
       subtitle = "16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") 

residential_int
workplace_int
transit_int
retail_int
food_int
parks_int

grid.arrange(parks_int, food_int, retail_int, transit_int, workplace_int, residential_int, ncol=3, nrow=2)




##############################
#  Heatmaps ----
##############################


# Code below creates the maps. These have been saved 
# and can be loaded - hence commenting out for now.  



#Load in shapefile and convert to ggplot compatable dataframe
raw_map<-readOGR("./maps_data/LA/","Local_Authority_Districts_December_2016_Full_Extent_Boundaries_in_the_UK")
dataframe_map<- fortify(raw_map,region = "lad16nm")



#Append data to map, dropping non-matched regions. Use left join to keep non-matched
map_data_sc <-dataframe_map  %>%
  right_join(raw_data_perc_fall_sc,
             by = c("id" = "GSS_NM")) %>%
  droplevels()

# Checking to make sure they're all there!
unique(map_data_sc$id)

save.image("mobility_map_sc.rdata")


# Load the Data - can just do this step if happy with the year the map data is currently set on. 
load("mobility_map_sc.rdata")


#Map Creator

#Create png of image
png('map.png')

workplace <- ggplot() + geom_map(data = map_data_sc,
                    map = map_data_sc,
                    mapping=aes(map_id=id,
                                 fill=WORKPLACE),
                    size=0.15,
                    colour="black" ) +
  expand_limits(x = map_data_sc$long, y = map_data_sc$lat)+
  coord_fixed() +
  #check out colorbrewer2.org for map colour
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Workplaces") +
  labs(title="Mobility Trends in Scotland using Google Analytics Data",
       subtitle = "Comparing change from 16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") +
  #Now remove all "graph" elements to leave map only
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 


dev.off()



residential <- ggplot() + geom_map(data = map_data_sc,
                                 map = map_data_sc,
                                 mapping=aes(map_id=id,
                                             fill=RESIDENTIAL),
                                 size=0.15,
                                 colour="black" ) +
  expand_limits(x = map_data_sc$long, y = map_data_sc$lat)+
  coord_fixed() +
  #check out colorbrewer2.org for map colour
  scale_fill_gradient(low = "#AAD9A7", high = "#1E7019",na.value = "grey40",
                      name = "% Change in Mobility to Residential") +
  labs(title="Mobility Trends in Scotland using Google Analytics Data",
       subtitle = "Comparing change from 16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") +
  #Now remove all "graph" elements to leave map only
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 

retail <- ggplot() + geom_map(data = map_data_sc,
                                   map = map_data_sc,
                                   mapping=aes(map_id=id,
                                               fill=RETAIL_AND_RECREATION),
                                   size=0.15,
                                   colour="black" ) +
  expand_limits(x = map_data_sc$long, y = map_data_sc$lat)+
  coord_fixed() +
  #check out colorbrewer2.org for map colour
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Retail & Recreational") +
  labs(title="Mobility Trends in Scotland using Google Analytics Data",
       subtitle = "Comparing change from 16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") +
  #Now remove all "graph" elements to leave map only
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 



food_pharm <- ggplot() + geom_map(data = map_data_sc,
                              map = map_data_sc,
                              mapping=aes(map_id=id,
                                          fill=GROCERY_AND_PHARMACY),
                              size=0.15,
                              colour="black" ) +
  expand_limits(x = map_data_sc$long, y = map_data_sc$lat)+
  coord_fixed() +
  #check out colorbrewer2.org for map colour
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Grocery & Pharmacy") +
  labs(title="Mobility Trends in Scotland using Google Analytics Data",
       subtitle = "Comparing change from 16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") +
  #Now remove all "graph" elements to leave map only
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 


parks <- ggplot() + geom_map(data = map_data_sc,
                                  map = map_data_sc,
                                  mapping=aes(map_id=id,
                                              fill=PARKS),
                                  size=0.15,
                                  colour="black" ) +
  expand_limits(x = map_data_sc$long, y = map_data_sc$lat)+
  coord_fixed() +
  #check out colorbrewer2.org for map colour
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Parks") +
  labs(title="Mobility Trends in Scotland using Google Analytics Data",
       subtitle = "Comparing change from 16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") +
  #Now remove all "graph" elements to leave map only
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 


transit <- ggplot() + geom_map(data = map_data_sc,
                             map = map_data_sc,
                             mapping=aes(map_id=id,
                                         fill=TRANSIT_STATIONS),
                             size=0.15,
                             colour="black" ) +
  expand_limits(x = map_data_sc$long, y = map_data_sc$lat)+
  coord_fixed() +
  #check out colorbrewer2.org for map colour
  scale_fill_gradient(low = "#AE3806", high = "#EED7CD",na.value = "grey40",
                      name = "% Change in Mobility to Transit & Stations") +
  labs(title="Mobility Trends in Scotland using Google Analytics Data",
       subtitle = "Comparing change from 16 Feb 2019 to 29 March 2020",
       caption = "Source: Google analytics") +
  #Now remove all "graph" elements to leave map only
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),#legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) 


grid.arrange(transit, workplace, parks, food_pharm, retail, residential, ncol=3, nrow=2)





