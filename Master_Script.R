#### Pre-defined Parameters ####
#### +Library####
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(grid)
library(gridExtra)
library(rgdal)
library(NLP)
library(dplyr)
library(lubridate)
library(scales)
library(blscrapeR)
library(forcats)
library(reshape)
setwd('/Users/zxuanxu/Box/MUSA_Practicum/Data')
##### +Functions####
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

##### +Themes####
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=1),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )
}
plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=1),
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

#### +Palettes ####
palette5 <- c("#FFC300","#FF5733","#C70039","#900C3F","#581845")

#### Data Imports ####
#### +Census Data ####
#Using census API
key <- "af6358372baac057e0f7a7a0dd2e0be3f6fd0694"
census_api_key(key, overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

#### ++ACS 5-year Estimates ####
tract_2010 <- get_acs(state = "KY", county = "Jefferson", geography = "tract",
                      year = 2010, geometry = TRUE,
                      variables = c(pop = "B01003_001",
                                    medInc = "B06011_001",
                                    medHomePrice = 'B25077_001',
                                    medGrossRent = 'B25064_001',
                                    rentBurden_total = 'B25070_001',
                                    rentBurden_30_35 = 'B25070_007',
                                    rentBurden_35_40 = 'B25070_008',
                                    rentBurden_40_50 = 'B25070_009',
                                    rentBurden_50 = 'B25070_010',
                                    pov_total = 'B17001_001',
                                    pov_below = "B17001_002",
                                    HU_total = 'B25002_001',
                                    HU_occu = 'B25002_002',
                                    HU_vacant = 'B25002_003',
                                    tenure_total = 'B25003_001',
                                    tenure_owner = 'B25003_002',
                                    tenure_renter = 'B25003_003',
                                    race_total = 'B02001_001',
                                    race_white = 'B02001_002',
                                    race_black = 'B02001_003'))%>%
  select(-moe) %>%
  spread(variable, estimate) %>%
  mutate(pct_owner = tenure_owner / tenure_total,
         pct_renter = tenure_renter / tenure_total,
         pct_white = race_white / race_total,
         pct_black = race_black / race_total,
         pct_vacant = HU_vacant / HU_total,
         pct_occupied = HU_occu / HU_total,
         pct_rentBurden = (rentBurden_30_35 + rentBurden_35_40 + rentBurden_40_50 + rentBurden_50) / rentBurden_total,
         pct_belowPov = pov_below / pov_total,
         medInc = medInc * 1.1521,
         medHomePrice = medHomePrice * 1.1521,
         medGrossRent = medGrossRent * 1.1521)
#2018
tract_2018 <- get_acs(state = "KY", county = "Jefferson", geography = "tract",
                      year = 2018, geometry = TRUE,
                      variables = c(pop = "B01003_001",
                                    medInc = "B06011_001",
                                    medHomePrice = 'B25077_001',
                                    medGrossRent = 'B25064_001',
                                    rentBurden_total = 'B25070_001',
                                    rentBurden_30_35 = 'B25070_007',
                                    rentBurden_35_40 = 'B25070_008',
                                    rentBurden_40_50 = 'B25070_009',
                                    rentBurden_50 = 'B25070_010',
                                    pov_total = 'B17001_001',
                                    pov_below = "B17001_002",
                                    HU_total = 'B25002_001',
                                    HU_occu = 'B25002_002',
                                    HU_vacant = 'B25002_003',
                                    tenure_total = 'B25003_001',
                                    tenure_owner = 'B25003_002',
                                    tenure_renter = 'B25003_003',
                                    race_total = 'B02001_001',
                                    race_white = 'B02001_002',
                                    race_black = 'B02001_003'))%>%
  select(-moe) %>%
  spread(variable, estimate) %>%
  mutate(pct_owner = tenure_owner / tenure_total,
         pct_renter = tenure_renter / tenure_total,
         pct_white = race_white / race_total,
         pct_black = race_black / race_total,
         pct_vacant = HU_vacant / HU_total,
         pct_occupied = HU_occu / HU_total,
         pct_rentBurden = (rentBurden_30_35 + rentBurden_35_40 + rentBurden_40_50 + rentBurden_50) / rentBurden_total,
         pct_belowPov = pov_below / pov_total)

#### ++Decenial Census ####
#2000
tract_2000 <- read_csv('./ACS_JeffersonCounty/census_2000.csv') 
tract_2000_geo <- left_join(tract_2018[, c('GEOID','geometry')] %>%
                              mutate(GEOID = as.numeric(GEOID)),tract_2000, by = c('GEOID' = 'Geo_FIPS' ))

#1990
tract_1990 <- read_csv('./ACS_JeffersonCounty/census_1990.csv')
tract_1990_geo <- left_join(tract_2018[, c('GEOID','geometry')] %>%
                              mutate(GEOID = as.numeric(GEOID)), tract_1990, by = c('GEOID' = 'Geo_FIPS'))
#### +Eviction ####
eviction_tracts <- st_read('Eviction_2000_2016/tracts.geojson')
eviction_counties <- st_read('Eviction_2000_2016/counties.geojson')

#### +Foreclosures ####
foreclosures <- read.csv("./Foreclosure_2011_2019/2011-2019_Foreclosures.csv")

#### +Historic Permits ####
permits <- st_read("./HistoricPermit_2003/Permit_History.csv", options=c("Longitude=y","Latitude=x"))

#### +PVA Home Sale ####
sales <- sf::st_read(dsn = "./pvadata.gdb",layer="remf_master")

#### +Zoning ####
zoning <- sf::st_read("https://opendata.arcgis.com/datasets/901f9c3321494238ac38a01d7e96d932_18.geojson")%>%
  st_transform(crs = 2246) 

#### +Parcel ####
parcel <- sf::st_read("./Parcel/Parcel.shp")%>%  
  st_transform(crs = 2246) 

#### +Neighborhoods ####
nhoods <- st_read("https://data.louisvilleky.gov/sites/default/files/urbanneighborhood.geojson")

#### Data Cleansing ####
#### +Create Panel Data for Census & Evictions & Foreclosures ####
#### ++Census####
#Join census data into one panel
panel_tract1 <- merge(tract_1990, tract_2000, by = 'Geo_FIPS', suffixes = c('.1990', '.2000'))
panel_tract2 <- merge(tract_2010 %>%
                        select(GEOID, pop, pct_white, pct_black, medInc, pct_owner, pct_renter, pct_vacant, pct_belowPov, geometry), 
                      tract_2018 %>%
                        st_set_geometry(NULL) %>%
                        select(GEOID, pop, pct_white, pct_black, medInc, pct_owner, pct_renter, pct_vacant, pct_belowPov), 
                      by = 'GEOID', suffixes = c('.2010', '.2018'))
panel_tract3 <- left_join(panel_tract2 %>%
                           mutate(GEOID = as.numeric(GEOID)), panel_tract1, by = c('GEOID' = 'Geo_FIPS'))

#### ++Evictions ####
#Join eviction data into the panel
panel_tract4 <- left_join(panel_tract3, eviction_tracts %>%
                           st_set_geometry(NULL) %>%
                           select(GEOID, starts_with('e.')) %>%
                           mutate(GEOID = as.numeric(as.character(GEOID))), by = 'GEOID')
#### ++Foreclosure ####
#Convert datatype from factor to numeric
foreclosures <- 
  foreclosures %>%
  mutate(x = as.numeric(as.character(Latitude)),
         y  = as.numeric(as.character(Longitude)))

#Convert to sf
foreclosures.sf <-
  foreclosures %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326, agr = "constant") %>%
  st_transform(crs = 2246) 

#Join foreclosures with census tracts
foreclosures.sf <- st_join(foreclosures.sf,
                           panel_tract%>%
                             st_transform(crs=2246),
                           join=st_within,
                           left = TRUE)

#Count foreclosures within census tracts
foreclosure_count <- foreclosures.sf %>%
  st_set_geometry(NULL) %>%
  group_by(GEOID, YEAR) %>%
  summarise(Foreclosures.count = n()) %>%
  spread(YEAR, Foreclosures.count)

foreclosure_count[is.na(foreclosure_count)] <- 0

colnames(foreclosure_count) <- c('GEOID', 'f.2011', 'f.2012','f.2013','f.2014','f.2015','f.2016','f.2017','f.2018','f.2019')

#Join to panel data 
panel_tract5 <- left_join(panel_tract4, foreclosure_count, by = 'GEOID')

#### ++Permits ####
#Convert to sf
permits.sf <-
  permits %>% 
  mutate(Latitude = as.numeric(as.character(Latitude)),
         Longitude = as.numeric(as.character(Longitude))) %>%
  filter(Latitude > 0) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

#Join foreclosures with census tracts
permits.sf <- st_join(permits.sf,
                      panel_tract5 %>% st_transform(crs = 4326),
                      join=st_within,
                      left = TRUE)

#Count permits within census tracts
permits_count <- permits.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4)),
         ifNew = ifelse(PERMITTYPEKEY == 'BUILDING', 'NewConstruction', 'NotNewConstruction')) %>%
  group_by(GEOID, YEAR, ifNew) %>%
  summarise(permits.count = n()) %>%
  cast(GEOID ~ YEAR + ifNew)
permits_count[is.na(permits_count)] <- 0
  
#Join to panel data 
panel_tract6 <- left_join(panel_tract5, permits_count, by = 'GEOID') %>%
  select(-NA_NewConstruction, -NA_NotNewConstruction)



#### +Join Permits and Home Sale Data ####
#Join Sales to Permits
Permit_n_Sales <- left_join(permits, sales, by = c("ADDRESS" = "PROP_ADDRESS"))

#Remove NA - These are addresses that only have either a permit or a sales
Permit_n_Sales <- Permit_n_Sales[!is.na(Permit_n_Sales$CUR_ZIP), ]

#### Exploratory Analysis ####
#### +Panel Data - Correlation ####
# Eviction vs. Renter-occupied Housing Units
grid.arrange(
  panel_tract6 %>%
    ggplot()+
    geom_point(aes(x= pct_renter.2000, y = e.00), alpha = 0.4)+
    geom_smooth(aes(x= pct_renter.2000, y = e.00), method = "lm", se=TRUE, colour = "blue") +
    labs(title='Eviction count as function of percentage of renter-occupied housing units. Based on 2010 census tracts.', 
         subtitle = "2000",
         x="Percentage of renter-occupied housing units", 
         y="Eviction count")+
    plotTheme(),
  
  panel_tract6 %>%
    ggplot()+
    geom_point(aes(x= pct_renter.2010, y = e.10), alpha = 0.4)+
    geom_smooth(aes(x= pct_renter.2010, y = e.10), method = "lm", se=TRUE, colour = "blue") +
    labs(title = ' ',
         subtitle = "2010",
         x="Percentage of renter-occupied housing units", 
         y="Eviction count")+
    plotTheme(),
  
  ncol = 2)

#Eviction vs. Population below Poverty Level
grid.arrange(
  panel_tract6 %>%
    ggplot()+
    geom_point(aes(x= pct_belowPov.2000, y = e.00), alpha = 0.4)+
    geom_smooth(aes(x= pct_belowPov.2000, y = e.00), method = "lm", se=TRUE, colour = "blue") +
    labs(title='Eviction count as function of percentage of population below poverty level. Based on 2010 census tracts.', 
         subtitle = "2000",
         x="Percentage of population below poverty level", 
         y="Eviction count")+
    plotTheme(),
  
  panel_tract6 %>%
    ggplot()+
    geom_point(aes(x= pct_belowPov.2010, y = e.10), alpha = 0.4)+
    geom_smooth(aes(x= pct_belowPov.2010, y = e.10), method = "lm", se=TRUE, colour = "blue") +
    labs(title = ' ',
         subtitle = "2010",
         x="Percentage of population below poverty level", 
         y="Eviction count")+
    plotTheme(),
  
  ncol = 2)

#Foreclosures vs. Ower-occupied Housing Units
panel_tract6 %>%
  ggplot()+
  geom_point(aes(x= pct_owner.2018, y = f.2018), alpha = 0.4)+
  geom_smooth(aes(x= pct_owner.2018, y = f.2018), method = "lm", se=TRUE, colour = "blue") +
  labs(title='Foreclosure count as function of percentage of owner-occupied housing units.', 
       subtitle = "2018. Based on 2010 census tracts.",
       x="Percentage of owner-occupied housing units", 
       y="Foreclosure counts")+
  plotTheme()

#Foreclosures vs. Median Household Income
panel_tract6 %>%
  ggplot()+
  geom_point(aes(x= medInc.2018, y = f.2018), alpha = 0.4)+
  geom_smooth(aes(x= medInc.2018, y = f.2018), method = "lm", se=TRUE, colour = "blue") +
  labs(title='Foreclosure count as function of median household income.', 
       subtitle = "2018.Based on 2010 census tracts.",
       x="Median household income", 
       y="Foreclosure counts")+
  plotTheme()

#Foreclosures vs. Percentage below Poverty Level
panel_tract6 %>%
  ggplot()+
  geom_point(aes(x= pct_belowPov.2018, y = f.2018), alpha = 0.4)+
  geom_smooth(aes(x= pct_belowPov.2018, y = f.2018), method = "lm", se=TRUE, colour = "blue") +
  labs(title='Foreclosure count as function of percentage of population below poverty level.', 
       subtitle = "2018. Based on 2010 census tracts.",
       x="Percentage of population below poverty level", 
       y="Foreclosure counts")+
  plotTheme()

#Evictions vs. Foreclosures
panel_tract6 %>%
  ggplot()+
  geom_point(aes(x= e.16, y = f.2016), alpha = 0.4)+
  geom_smooth(aes(x= e.16, y = f.2016), method = "lm", se=TRUE, colour = "blue") +
  labs(title='Foreclosure count as function of eviction count.', 
       subtitle = "2016. Based on 2010 census tracts.",
       x="Eviction counts", 
       y="Foreclosure counts")+
  plotTheme()

#### +Panel Data - Time Trend ####
#Permits
permits.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4)),
         ifNew = ifelse(PERMITTYPEKEY == 'BUILDING', 'NewConstruction', 'NotNewConstruction')) %>%
  filter(YEAR > 2003 & YEAR < 2019) %>%
  group_by(YEAR, ifNew) %>%
  summarise(permits.count = n()) %>%
  ggplot(aes(x= as.factor(YEAR), y = permits.count, color = ifNew)) +
  geom_line(aes(group = ifNew))+
  labs(title="Permit Acitivities, Louisville, 2004 - 2018", 
       x="Year", 
       y="Permit Counts")+
  plotTheme()

#Evictions
eviction_tracts %>%
  st_set_geometry(NULL) %>%
  select(GEOID, starts_with('e.')) %>%
  gather(-GEOID, key = 'Year', value = 'Count') %>%
  mutate( Year = paste('20', substr(Year, 3, 4), sep = '')) %>%
  group_by(Year) %>%
  summarise(eSum = sum(Count)) %>%
  ggplot(aes(x= as.factor(Year), y = eSum, group = 1)) +
  geom_line(size = 1) +
  ylim(0, 8500) +
  labs(title="Eviction Counts, Louisville, 2000 - 2016", 
       x="Year", 
       y="Eviction Counts")+
  plotTheme()

#Foreclosures
foreclosures.sf %>%
  st_set_geometry(NULL) %>%
  group_by(YEAR) %>%
  summarise(count = n()) %>%
  ggplot(aes(x= as.factor(YEAR), y = count, group = 1)) +
  geom_line(size = 1) +
  ylim(0, 4500) +
  labs(title="Foreclosure Acitivities, Louisville, 2004 - 2019", 
       x="Year", 
       y="Foreclosure Counts")+
  plotTheme()

  
#### +Map ####
#### ++Census ####
#Base map

#Change map


#### ++Eviction Ratio ####
panel_tract6 <- left_join(panel_tract6, eviction_tracts %>%
                            st_set_geometry(NULL) %>%
                            select(GEOID, starts_with('er.')) %>%
                            mutate(GEOID = as.numeric(as.character(GEOID))), by = 'GEOID')
#Base map
panel_tract6 %>%
  mutate(er.00 = ifelse(er.00 == 0, NA, er.00)) %>%
  ggplot(aes(fill = er.00, color = er.00)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1, name = 'Rate(%)') +
  scale_color_viridis(option = 'magma', direction = -1, name = 'Rate(%)') +
  labs(title = "Eviction Rate, Louisville, 2000",
       subtitle = "Rate of eviction judgements to renter-occupied households. Only count a single address per year") +
  mapTheme()
#Change map
panel_tract6 %>%
  mutate(pctChange = 100 * (er.16 - er.00)/er.00) %>%
  ggplot(aes(fill = pctChange, color = pctChange)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1) +
  scale_color_viridis(option = 'magma', direction = -1) +
  labs(title = "Percent Change of Eviction Rate, Louisville, 2000 - 2016",
       subtitle = "Rate of eviction judgements to renter-occupied households. Only count a single address per year")+
  mapTheme()
#### ++Foreclosure Ratio ####
#Base map
panel_tract6 %>%
  mutate(f.2011 = ifelse(f.2011 == 0, NA, f.2011)) %>%
  ggplot(aes(fill = f.2011, color = f.2011)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1, name = 'Counts') +
  scale_color_viridis(option = 'magma', direction = -1, name = 'Counts') +
  labs(title = "Foreclosure Counts, Louisville, 2011") +
  mapTheme()

#Change map
panel_tract6 %>%
  mutate(pctChange = 100 * (f.2019 - f.2011)/f.2011) %>%
  ggplot(aes(fill = pctChange, color = pctChange)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1, name = 'Counts') +
  scale_color_viridis(option = 'magma', direction = -1, name = 'Counts') +
  labs(title = "Percent Change of Foreclosure Counts, Louisville, 2011 - 2019") +
  mapTheme()

#### ++Permits ####
#Base map
#New construction
panel_tract6 %>%
  mutate(`2004_NewConstruction` = ifelse(`2004_NewConstruction` == 0, NA, `2004_NewConstruction`)) %>%
  ggplot(aes(fill = `2004_NewConstruction`, color = `2004_NewConstruction`)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1, name = 'Counts') +
  scale_color_viridis(option = 'magma', direction = -1, name = 'Counts') +
  labs(title = "Permit Counts, Louisville, 2004",
       subtitle = 'Only for new construction.') +
  mapTheme()
#Renovation
panel_tract6 %>%
  mutate(`2004_NotNewConstruction` = ifelse(`2004_NotNewConstruction` == 0, NA, `2004_NotNewConstruction`)) %>%
  ggplot(aes(fill = `2004_NotNewConstruction`, color = `2004_NotNewConstruction`)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1, name = 'Counts') +
  scale_color_viridis(option = 'magma', direction = -1, name = 'Counts') +
  labs(title = "Permit Counts, Louisville, 2004",
       subtitle = 'Only for renovation.') +
  mapTheme()

#Change map
#New construction
panel_tract6 %>%
  mutate(pctChange = 100 * (`2018_NewConstruction` - `2004_NewConstruction`)/`2004_NewConstruction`) %>%
  ggplot(aes(fill = pctChange, color = pctChange)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1, name = 'Percent') +
  scale_color_viridis(option = 'magma', direction = -1, name = 'Percent') +
  labs(title = "Percent Change of Permit Counts, Louisville, 2004",
       subtitle = 'Only for new construction.') +
  mapTheme()
#Renovation
panel_tract6 %>%
  mutate(pctChange = 100 * (`2018_NotNewConstruction` - `2004_NotNewConstruction`)/`2004_NotNewConstruction`) %>%
  ggplot(aes(fill = pctChange, color = pctChange)) +
  geom_sf() +
  scale_fill_viridis(option = 'magma', direction = -1, name = 'Percent') +
  scale_color_viridis(option = 'magma', direction = -1, name = 'Percent') +
  labs(title = "Permit Counts, Louisville, 2004",
       subtitle = 'Only for renovation.') +
  mapTheme()

  


#### Model Build ####


#### Model Validation ####



