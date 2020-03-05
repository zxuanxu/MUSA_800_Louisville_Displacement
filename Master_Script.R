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
library(blscrapeR)
library(spdep)
library(lwgeom)
library(geosphere)
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
palette5 <- c("#2D3047","#419D78","#CEFF1E6","#E0A458","#F17F29")

#### Data Imports ####
#### +Census Data ####
#Using census API
key <- "af6358372baac057e0f7a7a0dd2e0be3f6fd0694"
census_api_key(key, overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

#### ++ACS 5-year Estimates ####
options(tigris_use_cache = TRUE, tigris_class = "sf")
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
  mutate(GEOID = as.numeric(GEOID),
        pct_owner = tenure_owner / tenure_total,
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
  mutate(GEOID = as.numeric(GEOID),
    pct_owner = tenure_owner / tenure_total,
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

#### +Roads ####
roads <- st_read('./KY_Roads/SS.shp')%>%
  st_transform(crs = 4326)
interstates <- roads %>%
  filter(D_STHWYSYS == 'State Primary (Interstate)')

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

#### ++Sales ####
#Convert to sf
permits.sf <-
  permits %>% 
  mutate(Latitude = as.numeric(as.character(Latitude)),
         Longitude = as.numeric(as.character(Longitude))) %>%
  filter(Latitude > 0) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

#Join permits with census tracts
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

#### +Inflation Adjustment for Home Sales ####
#Inflation CPI Data
cpi <- bls_api("CUSR0000SA0")
cpi <- inflation_adjust(2019)
#Change datatype from FACTOR to CHARACTER
sales$PROP_ADDRESS <- as.character(sales$PROP_ADDRESS)

cpi <- cpi%>%
  #Convert year from character to numeric
  mutate(year = as.numeric(year))%>%
  #Add Inflation % Column 
  mutate(Inflation_rate = (255.65658/avg_cpi)*100)

sales_long <- sales %>%
  #Convert data from wide to long format where each row represents an individual sales record - Each address is now listed in 3 seperate records instead of 1
  reshape(direction = "long",
          varying = list(c("TRANSFER_DATE1", "TRANSFER_DATE2", "TRANSFER_DATE3"), c("CONSIDERATION1", "CONSIDERATION2", "CONSIDERATION3")),
          v.names = c("SaleDate","Price"))%>%
  #Remove all records where there is no sales 
  filter(SaleDate != "1900-01-01")%>%
  #Create Year column
  mutate(Year = year(SaleDate),
         #Create MonthYear column
         MonthYear = format(SaleDate, "%Y/%m"))%>%
  #Remove records sold before 1947 since CPI API data is availabile starting in 1947
  filter(Year > 1947)%>%
  #Remove $0 sales
  filter(Price != 0)

#Join Sales and CPI   
sales_long <- sales_long%>% 
  left_join(cpi, by = c("Year" = "year"))%>%
  #calculate 2019 sales prices using inflation rate
  mutate(Price_2019 = Price * Inflation_rate)%>%
  dplyr::select(-avg_cpi,-adj_value,-base_year,-pct_increase)

#Merge sales_long with Parcels
Sales_long_n_Parcels <- left_join(sales_long, parcel, by = c("PARCELID" = "PARCELI"))

##Remove NA - These are addresses that only have either a permit or a sales
Sales_long_n_Parcels <- Sales_long_n_Parcels[!is.na(Sales_long_n_Parcels$Shap_Ar), ]

Sales_long_n_Parcels.sf <- st_as_sf(Sales_long_n_Parcels)%>%
  st_transform(crs = 2246) 

Sales_long_n_Parcels.sf <- st_make_valid(Sales_long_n_Parcels.sf)

#Create centroid from polygon
Sales_long_n_Parcels.sf$geometry <- st_centroid(Sales_long_n_Parcels.sf$geometry)

Sales_long_n_Parcels.sf <- Sales_long_n_Parcels.sf %>%
  as.data.frame() %>%
  st_as_sf()

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
#Permits - New construction vs. Not new construction
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

#Permits - All permit types
permits.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4))) %>%
  filter(YEAR > 2003 & YEAR < 2019) %>%
  group_by(YEAR, PERMITTYPEKEY) %>%
  summarise(permits.count = n()) %>%
  ggplot(aes(x= as.factor(YEAR), y = permits.count, color = PERMITTYPEKEY)) +
  geom_line(aes(group = PERMITTYPEKEY))+
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

#### +Permit Count & Spatial Lag####
bg_2018 <- get_acs(state = "KY", county = "Jefferson", geography = "block group",
                   year = 2018, geometry = TRUE,
                   variables = c(pop = "B01003_001"))%>%
  select(-moe) %>%
  spread(variable, estimate)
#Building Permit only
permits_building <- permits.sf %>%
  filter(PERMITTYPE == 'Building Permit') %>%
  filter(WORKTYPE == 'New' | WORKTYPE == 'Renovation - Alter - Repair' |  WORKTYPE == 'Addition') %>%
  #Create indicator showing new construction or rehabs
  mutate(ifNewPermit = ifelse(WORKTYPE == 'New', 'New', 'Rehabs'))

#### ++ Spatial Lag - KNN first ####
coords = st_coordinates(permits_building)
# Five neighbors
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
#Spatial lag of permit counts
permits_building$lagCosts_5NB <- lag.listw(spatialWeights, permits_building$PROJECTCOSTS)

#### ++ Spatial Join - KNN first ####
permits_building.sf <- st_join(permits_building,
                               bg_2018 %>% st_transform(crs = 4326),
                               join=st_within,
                               left = TRUE,
)
#Count permits within block groups
permits_count <- permits_building.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4))) %>%
  filter(YEAR == 2018 | YEAR == 2010) %>%
  group_by(GEOID, YEAR, ifNewPermit) %>%
  summarise(counts = n()) %>%
  cast(GEOID ~ YEAR + ifNewPermit) %>%
  mutate(`2010_New` = ifelse(is.na(`2010_New`), 0, `2010_New`),
         `2010_Rehabs` = ifelse(is.na(`2010_Rehabs`), 0, `2010_Rehabs`),
         `2018_New` = ifelse(is.na(`2018_New`), 0, `2018_New`),
         `2018_Rehabs` = ifelse(is.na(`2018_Rehabs`), 0, `2018_Rehabs`)) 
#Average project cost in permit data within block groups
permits_cost <- permits_building.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4))) %>%
  filter(YEAR == 2018 | YEAR == 2010) %>%
  group_by(GEOID, YEAR, ifNewPermit) %>%
  summarise(avg_cost = mean(as.numeric(as.character(PROJECTCOSTS)))) %>%
  cast(GEOID ~ YEAR + ifNewPermit) %>%
  mutate(`2010_New_Cost` = ifelse(is.na(`2010_New`), 0, `2010_New`),
         `2010_Rehabs_Cost` = ifelse(is.na(`2010_Rehabs`), 0, `2010_Rehabs`),
         `2018_New_Cost` = ifelse(is.na(`2018_New`), 0, `2018_New`),
         `2018_Rehabs_Cost` = ifelse(is.na(`2018_Rehabs`), 0, `2018_Rehabs`)) 
#Average project cost in permit data within block groups
permits_lag_cost <- permits_building.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4))) %>%
  filter(YEAR == 2018 | YEAR == 2010) %>%
  group_by(GEOID, YEAR, ifNewPermit) %>%
  summarise(avg_lagCost = mean(lagCosts_5NB)) %>%
  cast(GEOID ~ YEAR + ifNewPermit) %>%
  mutate(`2010_NewCostLag` = ifelse(is.na(`2010_New`), 0, `2010_New`),
         `2010_RehabsCostLag` = ifelse(is.na(`2010_Rehabs`), 0, `2010_Rehabs`),
         `2018_NewCostLag` = ifelse(is.na(`2018_New`), 0, `2018_New`),
         `2018_RehabsCostLag` = ifelse(is.na(`2018_Rehabs`), 0, `2018_Rehabs`)) 

#Join back to block groups
permits_bg <- bg_2018 %>%
  left_join(permits_count, by = 'GEOID') %>%
  mutate(`2010_New` = ifelse(is.na(`2010_New`), 0, `2010_New`),
         `2010_Rehabs` = ifelse(is.na(`2010_Rehabs`), 0, `2010_Rehabs`),
         `2018_New` = ifelse(is.na(`2018_New`), 0, `2018_New`),
         `2018_Rehabs` = ifelse(is.na(`2018_Rehabs`), 0, `2018_Rehabs`)) %>%
  left_join(permits_cost%>%
              select(`2010_New_Cost`, `2010_Rehabs_Cost`, `2018_New_Cost`, `2018_Rehabs_Cost`, GEOID), by = 'GEOID') %>%
  mutate(`2010_New_Cost` = ifelse(is.na(`2010_New_Cost`), 0, `2010_New_Cost`),
         `2010_Rehabs_Cost` = ifelse(is.na(`2010_Rehabs_Cost`), 0, `2010_Rehabs_Cost`),
         `2018_New_Cost` = ifelse(is.na(`2018_New_Cost`), 0, `2018_New_Cost`),
         `2018_Rehabs_Cost` = ifelse(is.na(`2018_Rehabs_Cost`), 0, `2018_Rehabs_Cost`)) %>%
  left_join(permits_lag_cost%>%
              select(`2010_NewCostLag`, `2010_RehabsCostLag`, `2018_NewCostLag`, `2018_RehabsCostLag`, GEOID), by = 'GEOID') %>%
  mutate(`2010_NewCostLag` = ifelse(is.na(`2010_NewCostLag`), 0, `2010_NewCostLag`),
         `2010_RehabsCostLag` = ifelse(is.na(`2010_RehabsCostLag`), 0, `2010_RehabsCostLag`),
         `2018_NewCostLag` = ifelse(is.na(`2018_NewCostLag`), 0, `2018_NewCostLag`),
         `2018_RehabsCostLag` = ifelse(is.na(`2018_RehabsCostLag`), 0, `2018_RehabsCostLag`))

#Lag count
coords = st_centroid(permits_bg) %>%
  st_coordinates()
# Five neighbors
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
#Spatial lag of permit counts
permits_bg$lagCounts2010New_5NB <- lag.listw(spatialWeights, permits_bg$`2010_New`)
permits_bg$lagCounts2010Rehabs_5NB <- lag.listw(spatialWeights, permits_bg$`2010_Rehabs`)
permits_bg$lagCounts2018New_5NB <- lag.listw(spatialWeights, permits_bg$`2018_New`)
permits_bg$lagCounts2018Rehabs_5NB <- lag.listw(spatialWeights, permits_bg$`2018_Rehabs`)

#### ++ Spatial Join - Aggregate first####
permits_building2.sf <- st_join(permits_building,
                                bg_2018 %>% st_transform(crs = 4326),
                                join=st_within,
                                left = TRUE,
)
#Count permits within block groups
permits_count2 <- permits_building2.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4))) %>%
  filter(YEAR == 2018 | YEAR == 2010) %>%
  group_by(GEOID, YEAR, ifNewPermit) %>%
  summarise(counts = n()) %>%
  cast(GEOID ~ YEAR + ifNewPermit) %>%
  mutate(`2010_New` = ifelse(is.na(`2010_New`), 0, `2010_New`),
         `2010_Rehabs` = ifelse(is.na(`2010_Rehabs`), 0, `2010_Rehabs`),
         `2018_New` = ifelse(is.na(`2018_New`), 0, `2018_New`),
         `2018_Rehabs` = ifelse(is.na(`2018_Rehabs`), 0, `2018_Rehabs`)) 
#Average project cost in permit data within block groups
permits_cost2 <- permits_building2.sf %>%
  st_set_geometry(NULL) %>%
  mutate(ISSUEDATE = as.character(ISSUEDATE),
         YEAR = as.numeric(substr(ISSUEDATE, 1, 4))) %>%
  filter(YEAR == 2018 | YEAR == 2010) %>%
  group_by(GEOID, YEAR, ifNewPermit) %>%
  summarise(avg_cost = mean(as.numeric(as.character(PROJECTCOSTS)))) %>%
  cast(GEOID ~ YEAR + ifNewPermit) %>%
  mutate(`2010_New_Cost` = ifelse(is.na(`2010_New`), 0, `2010_New`),
         `2010_Rehabs_Cost` = ifelse(is.na(`2010_Rehabs`), 0, `2010_Rehabs`),
         `2018_New_Cost` = ifelse(is.na(`2018_New`), 0, `2018_New`),
         `2018_Rehabs_Cost` = ifelse(is.na(`2018_Rehabs`), 0, `2018_Rehabs`)) 

#Join back to block groups
permits_bg2 <- bg_2018 %>%
  left_join(permits_count2, by = 'GEOID') %>%
  mutate(`2010_New` = ifelse(is.na(`2010_New`), 0, `2010_New`),
         `2010_Rehabs` = ifelse(is.na(`2010_Rehabs`), 0, `2010_Rehabs`),
         `2018_New` = ifelse(is.na(`2018_New`), 0, `2018_New`),
         `2018_Rehabs` = ifelse(is.na(`2018_Rehabs`), 0, `2018_Rehabs`)) %>%
  left_join(permits_cost2%>%
              select(`2010_New_Cost`, `2010_Rehabs_Cost`, `2018_New_Cost`, `2018_Rehabs_Cost`, GEOID), by = 'GEOID') %>%
  mutate(`2010_New_Cost` = ifelse(is.na(`2010_New_Cost`), 0, `2010_New_Cost`),
         `2010_Rehabs_Cost` = ifelse(is.na(`2010_Rehabs_Cost`), 0, `2010_Rehabs_Cost`),
         `2018_New_Cost` = ifelse(is.na(`2018_New_Cost`), 0, `2018_New_Cost`),
         `2018_Rehabs_Cost` = ifelse(is.na(`2018_Rehabs_Cost`), 0, `2018_Rehabs_Cost`))

#### ++ Spatial Lag - Aggregate first####
coords = st_centroid(permits_bg2) %>%
  st_coordinates()
# Five neighbors
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
#Spatial lag of permit counts
permits_bg2$lagCounts2010New_5NB <- lag.listw(spatialWeights, permits_bg2$`2010_New`)
permits_bg2$lagCounts2010Rehabs_5NB <- lag.listw(spatialWeights, permits_bg2$`2010_Rehabs`)
permits_bg2$lagCounts2018New_5NB <- lag.listw(spatialWeights, permits_bg2$`2018_New`)
permits_bg2$lagCounts2018Rehabs_5NB <- lag.listw(spatialWeights, permits_bg2$`2018_Rehabs`)
#Spatial lag of permit costs
permits_bg2$lagCosts2010New_5NB <- lag.listw(spatialWeights, permits_bg2$`2010_New_Cost`)
permits_bg2$lagCosts2010Rehabs_5NB <- lag.listw(spatialWeights, permits_bg2$`2010_Rehabs_Cost`)
permits_bg2$lagCosts2018New_5NB <- lag.listw(spatialWeights, permits_bg2$`2018_New_Cost`)
permits_bg2$lagCosts2018Rehabs_5NB <- lag.listw(spatialWeights, permits_bg2$`2018_Rehabs_Cost`)

#### +Home Sale Count & Spatial Lag####
#### ++Join Sales to Blocks Groups####
Sales_Blocks <- st_join(Sales_long_n_Parcels.sf,
                        bg_2018%>%
                          st_transform(crs=2246),
                        join=st_within,
                        left = TRUE)

#Exclude Sales Price < $10,000 and > $500,000
Sales_Blocks <- Sales_Blocks %>%
  filter(Price_2019 >10000 & Price_2019 <= 500000)

#Calculate average sales price and percent change per block group for years 2010 and 2018 
Sales_Block_Change <-Sales_Blocks %>%
  st_set_geometry(NULL) %>%
  dplyr::filter(Year == 2018 | Year == 2010)%>%
  dplyr::group_by(GEOID, Year) %>%
  dplyr::summarise(AvgPrice = mean(Price_2019))%>%
  spread("Year", AvgPrice)%>%
  mutate(P_change = ((`2018`-`2010`)/`2010`)*100,
         Tot_change = (`2018`-`2010`))

#Calculate averge housing projects per block group  
Sales_Block_Change <-Sales_Blocks %>%
  st_set_geometry(NULL) %>%
  dplyr::filter(Year == 2018 | Year == 2010)%>%
  dplyr::group_by(GEOID, Year) %>%
  dplyr::summarise(count = n())%>%
  spread("Year", count)%>%
  mutate(n_2018 = `2018`,
         n_2010 = `2010`)%>%
  select(GEOID, n_2018, n_2010)%>%
  right_join(Sales_Block_Change, by = c("GEOID" = "GEOID"))

bg_2018 <- bg_2018 %>%
  dplyr::select(GEOID, geometry)

Sales_Block_Change <- Sales_Block_Change %>%
  left_join(bg_2018, by = c("GEOID" = "GEOID"))%>%
  filter(!is.na(P_change))%>%
  st_as_sf()

coords <- st_centroid(Sales_Block_Change) %>%
  st_coordinates()

#### ++Spatial Lag####
# Five neighbors
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")

neighborList_10 <- knn2nb(knearneigh(coords, 10))
spatialWeights_10 <- nb2listw(neighborList_10, style="W")

#Spatial lag of sales prices
Sales_Block_Change$lagPrice2010_5NB <- lag.listw(spatialWeights, Sales_Block_Change$`2010`)
Sales_Block_Change$lagPrice2018_5NB <- lag.listw(spatialWeights, Sales_Block_Change$`2018`)
Sales_Block_Change$lagPrice2010_10NB <- lag.listw(spatialWeights_10, Sales_Block_Change$`2010`)
Sales_Block_Change$lagPrice2018_10NB <- lag.listw(spatialWeights_10, Sales_Block_Change$`2018`)
#Spatial lag of sales counts
Sales_Block_Change$lagCount2010_5NB <- lag.listw(spatialWeights, Sales_Block_Change$`n_2010`)
Sales_Block_Change$lagCount2018_5NB <- lag.listw(spatialWeights, Sales_Block_Change$`n_2018`)

Sales_Block_Change <- read_csv('Block_Groups_Sales_Dist_Nearest_Neighbor.csv')
#### +Merge Sale and Permit ####
sale_permit_lag <- merge(Sales_Block_Change, 
                         permits_bg %>%
                           st_set_geometry(NULL) %>%
                           select(-NAME, -pop), 
                         by.x = 'GEOID',
                         by.y = 'GEOID')
write_csv(sale_permit_lag, 'sale_permit_lag.csv')

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
#### +Feature Engineering####
#### ++Census####
#Change of homeownership rate from 2000 to 2010, 2010 to 2018
home_ownership_table <- tract_2010 %>% 
  select(GEOID, pct_owner) %>%
  left_join(tract_2000 %>% 
              select(Geo_FIPS, pct_owner),
            by = c('GEOID'='Geo_FIPS'),
            suffix = c('2010','2000')) %>%
  left_join(tract_2018 %>% 
              select(GEOID, pct_owner) %>% 
              st_set_geometry(NULL), 
            by = 'GEOID') %>%
    mutate(pct_owner2018 = pct_owner) %>%
    select(-pct_owner) %>%
  mutate(change_pct_owner_00_10 = pct_owner2010 - pct_owner2000,
         change_pct_owner_10_18 = pct_owner2018 - pct_owner2010,
         ifOwnershipDecline_00_10 = ifelse(change_pct_owner_00_10 < 0, 1, 0),
         ifOwnershipDecline_10_18 = ifelse(change_pct_owner_10_18 < 0, 1, 0))
#Join to block groups
bg_2018 <- get_acs(state = "KY", county = "Jefferson", geography = "block group",
                   year = 2018, geometry = TRUE,
                   variables = c(pop = "B01003_001"))%>%
  select(-moe, -NAME) %>%
  spread(variable, estimate)

bg_variables<- bg_2018 %>%
  select(-pop) %>%
  mutate(GEOID_tract = substr(GEOID, 1, 11),
         GEOID_tract = as.numeric(GEOID_tract)) %>%
  left_join(home_ownership_table %>%
              st_set_geometry(NULL),
            by = c('GEOID_tract' = 'GEOID'))
#### ++Highways####
#Distance to the nearest interstate highway
dist <- dist2Line(p = st_coordinates(st_centroid(bg_variables)), line = st_coordinates(interstates)[,1:2])
bg_variables$dist2interstate <- dist
summary(bg_variables$dist2interstate)
write_csv(bg_variables, 'Highway_Homeownership_variables.csv')
#### Model Validation ####



