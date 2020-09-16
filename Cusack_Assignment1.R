# Meagan Cusack
# Assignment 1 

# Note that 2000 decennial census API endpoints are down at the moment,
# This code replaces 2000 API data with 2009 so that the demo runs smoothly
# In the event the API is not up by class time

# Please consult the original Bookdown for the relevant content to contextualize
# This code - https://urbanspatial.github.io/PublicPolicyAnalytics/


##### Set Up #####

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

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

census_api_key("e90c5bc9f1d1e33d8cdfd56b8df26814d394cda9", overwrite = TRUE, install = TRUE) 


##### Year 2009 tracts #####

# We run our year 2000 code using 2009 ACS (and ACS variables from our 2017 list)
# Notice this returns "long" data - let's examine it

?load_variables #allows you to view variables - go to help on side menu to see how it's done

?get_acs

# Requesting ACS data for 9 variables from 2009 in Portland, OR at the census tract level
# %>% is an operator known as a pipe that chains functions together; creates dataframe with geometry attached to it
# transform is for spatial projection

tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
                year=2009, state="Oregon", county="Multnomah", geometry=T) %>% 
  st_transform('EPSG:6855')  # Portland, OR coordinate system - https://epsg.io/6855

# shown in "long data" meaning each variable has its own row - allows for small, multiple plots
# in "wide data" each variable has its own column

# Wide data vs long data (and spread vs gather)
# https://www.garrickadenbuie.com/project/tidyexplain/images/tidyr-spread-gather.gif

# Referencing the data by matrix notation, and learning about the data...
# Let's examine each variable and the elements of an sf object

tracts09[1:3,]

# Look at a table of variables

table(tracts09$variable)

# We create a new data frame consisting only of population

totalPop09 <-     # arrow creates new dataframe
  tracts09 %>%
  filter(variable == "B25026_001")   #select a feature of interest

# Let's examine it

nrow(totalPop09)    #number of rows

names(totalPop09)   #names of variables

head(totalPop09)   #first 6 rows

glimpse(totalPop09)     #provides all the info at once

# Use the base R plotting function to examine it visually

plot(totalPop09)

plot(totalPop09[,5])

# ---- Using ggplot to visualize census data with sf -----

# Each plot adds more and more nuance and information

# Consult the text to understand the symbology schemes

A <- 
  ggplot() +                                                   #create plot
  geom_sf(data = totalPop09, aes(fill = estimate)) +           #make a geometry out of totalpop, using aesthetic estimate
  theme(plot.title = element_text(size=22))                    #adding graphic styling

A

B <- 
  ggplot() +                                                       
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +           
  theme(plot.title = element_text(size=22))                        

B

C <-
  ggplot() +
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(totalPop09, "estimate"),
                    name = "Total\nPopluation\n(Quintile Breaks)") +
  theme(plot.title = element_text(size=22))

C

D <- 
  ggplot() +                                                             #create plot
  geom_sf(data = totalPop09, aes(fill = q5(estimate))) +                 #make a geometry out of totalpop, using estimate in quintile breaks
  scale_fill_manual(values = palette5,                                   #scale should use pallete5 for values
                    labels = qBr(totalPop09, "estimate"),                #label quintile breaks
                    name = "Popluation\n(Quintile Breaks)") +            #label should read as "[NAME]" 
  labs(title = "Total Population", subtitle = "Portland, OR; 2009") +    #using these titles
  mapTheme() + theme(plot.title = element_text(size=22))                 #recipe added from styling options at top of code (giant function)

D

# Let's "spread" the data into wide form

tracts09 <- 
  tracts09 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001,              #renaming variables to be more intelligible
         Whites = B02001_002,
         FemaleBachelors = B15001_050, 
         MaleBachelors = B15001_009,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalPoverty = B06012_002)

st_drop_geometry(tracts09)[1:3,]

# Let's create new rate variables using mutate

tracts09 <- 
  tracts09 %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),    #mutate makes a new column
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)    #retains everything with the exception of the ones with a "-"

# Tracts 2009 is now complete. Let's grab 2017 tracts and do a congruent
# set of operations


##### Year 2017 tracts #####

# Notice that we are getting "wide" data here in the first place
# This saves us the trouble of using "spread"

tracts17 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2017, state="Oregon", county="Multnomah", geometry=T, output="wide") %>%
  st_transform('EPSG:6855') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 


##### Combining 2009 and 2017 data ######

allTracts <- rbind(tracts09,tracts17)    #appending them one on top of the other; not "joined" by a specific variable


##### Transit Data #####

septaStops <-   # NEED TO FIGURE OUT HOW TO SHOW DIFF LINES IN PDX!!!
  rbind(
    st_read("https://opendata.arcgis.com/datasets/8c6e2575c8ad46eb887e6bb35825e1a6_0.geojson") %>% 
      mutate(Line = "El") %>%
      select(Station, Line),
    st_read("https://opendata.arcgis.com/datasets/2e9037fd5bef406488ffe5bb67d21312_0.geojson") %>%
      mutate(Line ="Broad_St") %>%
      select(Station, Line)) %>%
  st_transform(st_crs(tracts09))  


PDXlightrail <- 
  rbind(
    st_read("https://opendata.arcgis.com/datasets/f9c61dec4ea142a6b031e0fb2119f1e7_53.geojson")) %>%
  st_transform(st_crs(tracts09))  

View(PDXlightrail)


# Let's visualize it

ggplot() + 
  geom_sf(data=st_union(tracts09)) +
  geom_sf(data=PDXlightrail, size= 2) +
  labs(title="Lighrail Stations", 
       subtitle="Portland, OR", 
       caption="Figure 1") +
  mapTheme()

# Relating lightrail and census tracts

# Create buffers (in feet - note the CRS) around rail stops -         #using sf package, projected in "feet" ; half mile is 2640 feet
# Both a buffer for each stop, and a union of the buffers...          #sf object contains buffered geometries
# and bind these objects together

PDXBuffers <- 
  rbind(
    st_buffer(PDXlightrail, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(PDXlightrail, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each
 
ggplot() +
  geom_sf(data=PDXBuffers) +
  geom_sf(data=PDXlightrail, show.legend = "point") +
  facet_wrap(~Legend) +                                              #how we make a separate plot for each point (ie, small, multiple plots)
  labs(caption = "Figure 2.6") +
  mapTheme()

# Spatial operations

# Consult the text to understand the difference between these three types of joins
# and discuss which is likely appropriate for this analysis

# Create an sf object with ONLY the unioned buffer

buffer <- filter(PDXBuffers, Legend=="Unioned Buffer")

# Clip the 2009 tracts ... by seeing which tracts intersect (st_intersection)
# with the buffer and clipping out only those areas

clip <- 
  st_intersection(buffer, tracts09) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")

# Do a spatial selection to see which tracts touch the buffer

selection <- 
  tracts09[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")

# Do a centroid-in-polygon join to see which tracts have their centroid in the buffer
# Note the st_centroid call creating centroids for each feature

selectCentroids <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

# Indicator Maps

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




# Exercise - Can you create a small multiple map of the three types of operations?
# Consult the text for some operations you can try 
# This is to be done in breakout groups - code will up uploaded to piazza


#Using AllTracts.group

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


#How to ask good questions!!  https://piazza.com/class/kdae6bghn3qk6?cid=8

# Can you try to create the maps seen in the text?
# The solutions are contained in "map_exercise.R"







# --- TOD Indicator Tables ----

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

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

# Examining three submarkets   # NEED TO CREATE MARKETS FOR PDX!!!

centerCity <-
  st_intersection(
    st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union(),
    st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union()) %>%
  st_sf() %>%
  mutate(Submarket = "Center City")

el <-
  st_buffer(filter(septaStops, Line == "El"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "El")

broad.st <-
  st_buffer(filter(septaStops, Line == "Broad_St"), 2640) %>% st_union() %>%
  st_sf() %>%
  st_difference(centerCity) %>%
  mutate(Submarket = "Broad Street")

threeMarkets <- rbind(el, broad.st, centerCity)

# You can then bind these buffers to tracts and map them or make small multiple plots

allTracts.threeMarkets <- 
  st_join(st_centroid(allTracts), threeMarkets) %>%
  st_drop_geometry() %>%
  left_join(allTracts) %>%
  mutate(Submarket = replace_na(Submarket, "Non-TOD")) %>%
  st_sf() 
