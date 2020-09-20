# Meagan Cusack
# Assignment 1 
 
# Load Libraries

library(dplyr)
library(tidyverse)
library(tidycensus) 
library(sf)
library(kableExtra)
library(tmap)
library(maptools)
library(plotly)

options(scipen=999)
options(tigris_class = "sf")

##### Load Styling options #####

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

?theme

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions 

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}


# Load hexadecimal color palette

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")


# Load census API key 

census_api_key("e90c5bc9f1d1e33d8cdfd56b8df26814d394cda9", overwrite = TRUE) 


#### DATA - (1) Show your data wrangling work.

## CENSUS DATA

# Portland census tracts: https://www2.census.gov/geo/maps/dc10map/tract/st41_or/c41051_multnomah/DC10CT_C41051_001.pdf
# Portland coordinate system: https://epsg.io/6855

# 2009 Census data: https://api.census.gov/data/2009/acs/acs5/groups.html
# 2017 Census data: https://api.census.gov/data/2017/acs/acs5/groups.html

# Variables:
# Total population = B01003_001E
# Median rent = B25058_001E
# Median household income = B19013_001E
# Poverty status = B17017_001E
# Nonfamily household = B11010_001E

###NEED TO FIGURE OUT HOW TO FILTER OUT THESE TRACTS: 41051003103,41051003102,41051005202,41051005203
#   filter(GEOID != c(41051003103,41051003102,41051005202,41051005203)) %>% 
#   Make the entire thing and THEN make subset?
#   tracts09_subset <- subset(tracts09, GEOID != 41051003103 & GEOID != 41051003102 & GEOID != 41051005202 & GEOID != 41051005203,
#                          select=c(GEOID, geometry, NAME, variable, estimate, moe)) 
#   I think the numbers I'm using simply don't align with the data, but I can't find a map that shows the correct numbers then. 


tracts09 <-  
  get_acs(geography = "tract", variables = c("B01003_001E","B25058_001E","B19013_001E",
                                             "B17017_001E","B11010_001E"), 
          year=2009, state=41, county=051, geometry=T) %>% 
  st_transform('EPSG:6855') 

tracts09 <- 
  tracts09 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B01003_001,              
         MedRent = B25058_001,
         MedHHInco = B19013_001, 
         Poverty = B17017_001,
         Nonfamily = B11010_001)

tracts09 <- 
  tracts09 %>%
  mutate(pctPoverty = ifelse(TotalPop > 0, Poverty / TotalPop, 0),
         pctNonfamily = ifelse(TotalPop > 0, Nonfamily / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Poverty,-Nonfamily)

tracts17 <- 
  get_acs(geography = "tract", variables = c("B01003_001E","B25058_001E","B19013_001E",
                                             "B17017_001E","B11010_001E"), 
          year=2017, state=41, county=051, geometry=T) %>%
  st_transform('EPSG:6855') 

tracts17 <- 
  tracts17 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B01003_001,              
         MedRent = B25058_001,
         MedHHInco = B19013_001, 
         Poverty = B17017_001,
         Nonfamily = B11010_001)

tracts17 <- 
  tracts17 %>%
  mutate(pctPoverty = ifelse(TotalPop > 0, Poverty / TotalPop, 0),
         pctNonfamily = ifelse(TotalPop > 0, Nonfamily / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Poverty,-Nonfamily)

allTracts <- rbind(tracts09,tracts17)


## LIGHTRAIL STATIONS  **WOULD LIKE TO ADD SPECIFIC LINES IN COLOR**

PDXlightrail <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/f9c61dec4ea142a6b031e0fb2119f1e7_53.geojson")) %>%
  st_transform(st_crs(tracts09))  

# Map

ggplot() + 
  geom_sf(data=st_union(tracts09)) +
  geom_sf(data=PDXlightrail, size= 2) +
  labs(title="Lighrail Stations", 
       subtitle="Portland, OR", 
       caption="Figure 1") +
  mapTheme()

# Relating lightrail and census tracts (buffers)

PDXBuffers <- 
  rbind(
    st_buffer(PDXlightrail, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(PDXlightrail, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

ggplot() +
  geom_sf(data=PDXBuffers) +
  geom_sf(data=PDXlightrail, show.legend = "point") +
  facet_wrap(~Legend) +                                              
  labs(title="Lighrail Stations in relation to Census Tracts", 
              subtitle="Portland, OR", 
              caption = "Figure 2") +
  mapTheme()

## CENSUS TRACT BUFFERS

# Select option to map census tracts to buffer
# [Consult the text to understand the difference between these three types of joins and discuss which is likely appropriate for this analysis]

# Create an sf object with ONLY the unioned buffer

buffer <- filter(PDXBuffers, Legend=="Unioned Buffer")

# Clip ... cookie cutter

clip <- 
  st_intersection(buffer, tracts09) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip Selection")

# Spatial Selection ... tracts that intersect or touch the buffer

selection <- 
  tracts09[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

# Select by Centroid ... all tract centroids that intersect the buffer

selectCentroids <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Centroid Selection")

# Map buffers

Spatial <- 
  rbind(
    clip %>%
      mutate(Legend = "Clip Selection") %>%
      dplyr::select(Legend),
    selection %>%
      mutate(Legend = "Spatial Selection") %>%
      dplyr::select(Legend),
    selectCentroids %>%
      mutate(Legend = "Centroid Selection") %>%
      dplyr::select(Legend))

ggplot() +
  geom_sf(data=Spatial) +
  geom_sf(data=PDXlightrail, show.legend = "point") +
  facet_wrap(~Legend) +                                             
  labs(title="Mapping Census Tracts to Buffers", 
       subtitle="Portland, OR", 
       caption = "Figure 3") +
  mapTheme()
 

#### INDICATOR MAPS - (2) Four small-multiple (2000 & 2017+) visualizations comparing four selected Census variables across time and space (TOD vs. non-TOD).

# We do our centroid joins as above, and then do a "disjoin" to get the ones that *don't*
# join, and add them all together.
# Do this operation and then examine it.
# What represents the joins/doesn't join dichotomy?
# Note that this contains a correct 2009-2017 inflation calculation

allTracts.group <- 
  rbind(                                                                  #2009 AND 2017 data for relevant TOD areas
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent))    #relevant inflation adjustment

# I guess this shows tOD? It's hard to see...   ### I DON'T THINK WE NEED TO INCLUDE THIS ONE!

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Median Rent                                   ### I SPENT AN HOUR TRYING TO FIGURE OUT HOW TO FORMAT THE LEGEND TO HAVE COMMAS AND DECIMALS WITHOUT LUCK

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year) +
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Median Household Income                       ### SAME HERE, WANT TO FORMAT THE LEGEND

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedHHInco))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedHHInco"),
                    name = "Median Household Income\n(Quintile Breaks)") +
  labs(title = "Median Household Income 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Poverty Rate

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(pctPoverty))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctPoverty"),
                    name = "Poverty rate\n(Quintile Breaks") +              ### SAME HERE, WE AT LEAST NEED 2 DECIMAL PLACES
  labs(title = "Poverty Rate 2009-2017") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# Nonfamily Households 

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(pctNonfamily))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctNonfamily"),          ### AND HERE, WE AT LEAST NEED 2 DECIMAL PLACES
                    name = "Nonfamily households\n(Quintile Breaks)") +
  labs(title = "Nonfamily Households 2009-2017") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


#### INDICATOR PLOTS - (3) One grouped bar plot making these same comparisons.

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%                                      
  group_by(year, TOD) %>%
  summarize("Median Rent" = mean(MedRent, na.rm = T),
            "Median Household Income" = mean(MedHHInco, na.rm = T),
            "Poverty (%)" = mean(pctPoverty, na.rm = T),
            "Nonfamily Household (%)" = mean(pctNonfamily, na.rm = T))

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%                          ## NEED THE TITLES TO WRAP!! AND ADD A FIGURE NOTE
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom") 


#### INDICATOR TABLES - (4) One table making these same comparisons.

#kable(allTracts.Summary) %>%                                     ## I DON'T THINK WE NEED THIS 
 # kable_styling() %>%
#  footnote(general_title = "\n")

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 1")

#### GRADUATED SYMBOL MAPS - (5) Create two graduated symbol maps of population and rent within 0.5 mile of each transit station. 
# [A graduate symbol map represents quantities for each transit station proportionally.]

#Map 1 - Population 

tmap_mode("view")

Population09 <- 
  st_intersection(PDXBuffers, tracts09) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")
  
tm_shape(Population09)+
  tm_bubbles(col = "#bae4bc",
             size = "TotalPop",
             border.col = "black",
             border.lwd = 1)

Population17 <- 
  st_intersection(PDXBuffers, tracts17) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip") 
  
tm_shape(Population17)+
  tm_bubbles(col = "#43a2ca",
             size = "TotalPop",
             border.col = "black",
             border.lwd = 1)

#Map 2 - rent

Rent09 <- 
  st_intersection(PDXBuffers, tracts09) %>%
  dplyr::select(MedRent) %>%
  mutate(Selection_Type = "Clip")

tm_shape(Rent09)+
  tm_bubbles(col = "#bae4bc",
             size = "MedRent",
             border.col = "black",
             border.lwd = 1)

Rent17 <- 
  st_intersection(PDXBuffers, tracts17) %>%
  dplyr::select(MedRent) %>%
  mutate(Selection_Type = "Clip") 

tm_shape(Rent17)+
  tm_bubbles(col = "#43a2ca",
             size = "MedRent",
             border.col = "black",
             border.lwd = 1)

#### MULTIPLE RING BUFFER - (6) Create a geom_line plot that shows mean rent as a function of distance to subway stations (Figure x.x.x). 
# [To do this you will need to use the multipleRingBuffer function in the ppaR package.]

# Multiple ring function: 

multipleRingBuffer <- function(inputPolygon, maxDistance, interval)
  
{
  distances <- seq(0, maxDistance, interval)
  distancesCounter <- 2
  numberOfRings <- floor(maxDistance / interval)
  numberOfRingsCounter <- 1
  
  allRings <- data.frame()
  while (numberOfRingsCounter <= numberOfRings)
  
  {
    
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
      
    {
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      buffer1_ <- st_difference(inputPolygon, buffer1)
      thisRing <- st_cast(buffer1_, "POLYGON")
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      thisRing$distance <- distances[distancesCounter]
    }
    
    else if(distances[distancesCounter] < 0 & distancesCounter > 2)
      
    {
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      thisRing <- st_difference(buffer2,buffer1)
      thisRing <- st_cast(thisRing, "POLYGON")
      thisRing <- as.data.frame(thisRing$geometry)
      thisRing$distance <- distances[distancesCounter]
    }
    
    else
      
    {
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      thisRing <- st_difference(buffer1,buffer1_)
      thisRing <- st_cast(thisRing, "POLYGON")
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      thisRing$distance <- distances[distancesCounter]
    }  
    
  allRings <- rbind(allRings, thisRing)
  distancesCounter <- distancesCounter + 1
  numberOfRingsCounter <- numberOfRingsCounter + 1
  
  }
  
  allRings <- st_as_sf(allRings)
  
}

ringedbuffer <- multipleRingBuffer(PDXlightrail, 10, 1)


allTracts_buffer <- st_intersection(allTracts, ringedbuffer)


lineplot <- ggplot (data=allTracts_buffer, aes(x=distance, y=MedRent, group=year, shape=year)) +
  geom_line() +
  geom_point()

fig <- ggplotly(lineplot)

fig

#------- HERE IS WHERE WE NEED TO CREATE NEW DATA FRAMES (I THINK) !!!!!
# I THINK THE PROBLEM MAY BE THAT WE HAVE "LONG" DATA? (I.E., EACH CENSUS TRACT IS LISTED MULTIPLE TIMES IN THE DATAFRAME)
# I TRIED TO MAKE THE DATA LONG, BUT IT DIDN'T WORK. 
# I DID MAKE NEW DATAFRAMES FOR EACH RING AND YEAR; HOWEVER, THEY ARE IN LONG FORM, SO WE NEED TO RERUN IT.
# THEN, I WANTED TO TAKE THE MEAN OF THE MEDIAN RENTS FOR EACH RING AND PUT THEM IN A NEW DATAFRAME, WHICH IS WHERE
# I THINK WE COULD PULL THE DATA FOR THE LINE GRAPH....

 
# Making data wide

allTracts_buffer_wide <- spread(allTracts_buffer, GEOID, GEOID)
allTracts_buffer_wide

# Data for each ring

ring1_09 <- subset(allTracts_buffer, distance == 1 & year == 2009) 
ring1_17 <- subset(allTracts_buffer, distance == 1 & year == 2017) 
ring2_09 <- subset(allTracts_buffer, distance == 2 & year == 2009) 
ring2_17 <- subset(allTracts_buffer, distance == 2 & year == 2017) 
ring3_09 <- subset(allTracts_buffer, distance == 3 & year == 2009) 
ring3_17 <- subset(allTracts_buffer, distance == 3 & year == 2017) 
ring4_09 <- subset(allTracts_buffer, distance == 4 & year == 2009) 
ring4_17 <- subset(allTracts_buffer, distance == 4 & year == 2017) 
ring5_09 <- subset(allTracts_buffer, distance == 5 & year == 2009) 
ring5_17 <- subset(allTracts_buffer, distance == 5 & year == 2017) 
ring6_09 <- subset(allTracts_buffer, distance == 6 & year == 2009) 
ring6_17 <- subset(allTracts_buffer, distance == 6 & year == 2017) 
ring7_09 <- subset(allTracts_buffer, distance == 7 & year == 2009) 
ring7_17 <- subset(allTracts_buffer, distance == 7 & year == 2017) 
ring8_09 <- subset(allTracts_buffer, distance == 8 & year == 2009) 
ring8_17 <- subset(allTracts_buffer, distance == 8 & year == 2017) 
ring9_09 <- subset(allTracts_buffer, distance == 9 & year == 2009) 
ring9_17 <- subset(allTracts_buffer, distance == 9 & year == 2017) 
ring10_09 <- subset(allTracts_buffer, distance == 10 & year == 2009) 
ring10_17 <- subset(allTracts_buffer, distance == 10 & year == 2017) 

mean(ring1_09$MedRent) 
mean(ring1_17$MedRent) 
mean(ring2_09$MedRent) 
mean(ring2_17$MedRent) 
mean(ring3_09$MedRent) 
mean(ring3_17$MedRent) 
mean(ring4_09$MedRent) 
mean(ring4_17$MedRent) 
mean(ring5_09$MedRent) 
mean(ring5_17$MedRent) 
mean(ring6_09$MedRent) 
mean(ring6_17$MedRent) 
mean(ring7_09$MedRent) 
mean(ring7_17$MedRent) 
mean(ring8_09$MedRent) 
mean(ring8_17$MedRent) 
mean(ring9_09$MedRent) 
mean(ring9_17$MedRent) 
mean(ring10_09$MedRent) 
mean(ring10_17$MedRent) 
 

#### CRIME DATA - (7) Download and wrangle point-level crime data (pick a crime type). 
# [What is the relationship between crime, transit access and rents?]










#-------- PRIOR WORK / NOTES


kable(allTracts.Summary) %>%                                      #styling language for tables
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")


#Some ideas

library(ggplot2)
# Basic line plot with points
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line()+
  geom_point()
# Change the line type
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()
# Change the color
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line(color="red")+
  geom_point()


# --- TOD Indicator Tables ----



kable(allTracts.Summary) %>%                                      #styling language for tables
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.2")

# Let's make some comparisons and speculate about the willingness to pay
# and demographics in these areas 2009-2017 (see the 2000 data in the text too)

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

# --- TOD Indicator Plots ------

# Let's create small multiple plots
# We use the "gather" command (look this one up please)
# To go from wide to long
# Why do we do this??
# Notice we can "pipe" a ggplot call right into this operation!

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom") 
