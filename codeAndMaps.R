library(tidyverse)
library(tidycensus)

setwd("~/Documents/GitHub/ACS_Data_ElPaso/")

load("edgeListWithDistancesTexas.RData")
zipsAndRegions<-read_csv("ZipRegionsElPaso.csv")

TheseZips<-zipsAndRegions %>% filter(Name != "FORT BLISS") %>%
  rename_at("Zip",~"GEOID") %>% mutate(GEOID=as.character(GEOID))
edgeListWithDistancesTexas

#I need to get mobility for NM and the zip codes they are including here
edgeListWithDistancesTexas %>% filter(origin %in% TheseZips$Zip) %>%
  filter(destin %in% TheseZips$Zip)

population_zcta_TX <- get_acs(
  geography = "zcta",
  variables = "B01003_001",
  year = 2019,
  state="TX",
  geometry = T
)

population_zcta_NM <- get_acs(
  geography = "zcta",
  variables = "B01003_001",
  year = 2019,
  state="NM",
  geometry = T
)

popuElPaso<-population_zcta_TX %>% filter(GEOID %in% TheseZips$GEOID) %>%
  left_join(TheseZips %>% select(-State))

popuNM<-population_zcta_NM %>% filter(GEOID %in% TheseZips$GEOID) %>%
  left_join(TheseZips %>% select(-State))

juntos<-rbind(popuElPaso,popuNM)

ggplot()+ theme_void() +
  geom_sf(data=juntos,aes(geometry=geometry,fill=Name))
  
#Extraction of data from ACS to get race and income in each zip code 
## By race
#B03002_007 - Native Hawaiian and Other Pacific Islander Alone
race_vars <- c(
  White = "B03002_003",
  Black = "B03002_004",
  Native = "B03002_005",
  Asian = "B03002_006",
  HIPI = "B03002_007",
  Hispanic = "B03002_012"
)

#Extracting race in each zip code in TX
tx_race <- get_acs(
  geography = "zcta",
  state = "TX",
  variables = race_vars,
  summary_var = "B03002_001",
  year = 2019
) 

#Merging race with WWTP data, calculating percent of race in each zip code
dataWithWWTPAndRaceZip<-mapWithWWDataAndMenbership %>% 
  left_join(tx_race %>% mutate(percPopu=100*(estimate/summary_est)) %>%
              select(GEOID,Race=variable,percPopu)) 

racePopu<-ggplot() + theme_void() +
  geom_sf(data=mapWithWWDataAndMenbership,aes(geometry=geometry),fill="white")+
  geom_sf(data=dataWithWWTPAndRaceZip %>% filter(totalCov>=0,!Race %in% c("HIPI","Native")),aes(geometry=geometry,fill=percPopu)) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA)+
  facet_wrap(~Race)

#Extracting income in each zip code in TX
tx_income <- get_acs(
  geography = "zcta",
  table = "B19001",
  state = "TX",
  year = 2019
)

#Managing data to extract bands of income and to calculate percents
tx_income_recode <- tx_income %>%
  filter(variable != "B19001_001") %>%
  mutate(incgroup = case_when(
    variable < "B19001_008" ~ "below35k", 
    variable < "B19001_013" ~ "bw35kand75k", 
    TRUE ~ "above75k"
  )) %>% left_join(tx_income %>% 
                     filter(variable == "B19001_001") %>% select(GEOID,totalInVar=estimate))

tx_group_sums <- tx_income_recode %>%
  select(-NAME,-variable,-moe) %>%
  group_by(GEOID, incgroup,totalInVar) %>%
  summarize_each(sum) %>% rename_at("estimate",~"population") %>%
  mutate(PercPopu=100*(population/totalInVar))
