# Sept Troll Update, 2023

# Justin Priest
# Kristin Courtney
# September 2023
# justin.priest@alaska.gov
# kristin.courtney@alaska.gov

# This script updates the wild abundance and harvest thru September.
#  These data are useful in case of a closure/extension justification. 

# 2023 Notes: K Courtney went through and updated data sources through SW 37 on Sep 13, 2023. 
# Current harvest SW 37: 1.62M
# Projected total harvest: 1.82M
# Current hatchery troll harvest: 254K 
# Current Wild Harvest: 1.11M
# Projected Wild Harvest: 1.24 M
# Projected troll coho harvest: 946K
# Wild coho troll harvest: 693K
# Wild coho abundance: 3.24M

# wild abundance is down slightly from August estimate of 3.91M, but no conservation concerns. 


source("code/1_dataimport.R")
library(adfgcolors) # remotes::install_github("justinpriest/adfgcolors")



#####################
##    SECTION 1    ##
## UPDATED HARVEST ##

##  This is the same as in 1_dataimport.R but I've kept separate here

allgearharv2023_sept <- read_csv(here::here("data/SEAK_Coho_FishTickets_AllGearHarv1985-2023_15September.csv")) %>%  
  rename("Year" = `DOL Year`, 
         "Total" = `Number Of Animals (sum)`,
         "StatWeek" = `Stat Week`,
         "Gear" = `Gear Name`,
         "Species" = `Species Name`) %>%
  select(Year:Total) 
# On OceanAK, only keep harvest codes 11, 12, 13, 17, and 18



eland_inprog_sept <- read_csv(here::here("data/SEAK_eLandingsCohoHarvUnbatched-2023-15September.csv")) %>%
  rename("Year" = `DFE Year`, 
         "Total" = `Number Of Animals (sum)`,
         "StatWeek" = `DFE Stat Week`,
         "Gear" = `Gear Name`,
         "Species" = `Species Name`) %>%
  select(Year, Gear, StatWeek, Total)


allgearharv2023_sept %>%
  full_join(eland_inprog_sept) %>%
  filter(StatWeek <= 37) %>%
  group_by(Year) %>%
  summarise(totalharvest = sum(Total)) %>% tail(20)
# 2023: thru week 37, 1.36M coho harvested (wild & hatchery)


runtiming_avg <- runtiming %>%
  group_by(StatWeek) %>%
  summarise(meanrunperc = mean(runperc)) 
runtiming_avg

# thru week 37, we're usually 86% of the way through harvest
# 2023: 89% run timing


1.36 / 0.86 # 2023: projects out to about 1.53 (89%) M & 1.58 (896%) total coho harvested in 2023



######################
##    SECTION 2     ##
## UPDATED HATCHERY ##
hatcherycont_sept <- read_csv(here::here("data/SEAK_Coho_MTA_HatcheryTagRecoveryinTroll_1985-2023_15September.csv"), 
                         skip = 26, guess_max = 20000) %>%
  rename("StatWeek" = `Stat Week (CWT)`,
         "Facility_Location" = `Location (Facility or Wild Stock)`) %>%
  mutate(Year = as.integer(Year)) %>%
  filter(StatWeek %>% between(27, 37), Year >= 1990) %>%
  group_by(Year, StatWeek) %>%
  summarise(totalcont = sum(Contribution, na.rm = TRUE)) 

# all gear harvest of hatchery coho
read_csv(here::here("data/SEAK_Coho_MTA_HatcheryTagRecoveryAllGear_2023_15September.csv"), 
         skip = 26, guess_max = 20000) %>%
  rename("StatWeek" = `Stat Week (CWT)`,
         "Facility_Location" = `Location (Facility or Wild Stock)`,
         "gear" = `Gear Class`) %>%
  mutate(Year = as.integer(Year)) %>% 
  group_by(gear) %>%
  summarise(totalcont = sum(Contribution, na.rm = TRUE)) 

4973+35469+254994

# 295436 hatchery coho


hatcherycont_sept


hatcherycont_sept %>% filter(Year == 2023) %>% tail(20)

hatcherycont_sept %>% 
  group_by(Year) %>%
  summarise(annhatchery = sum(totalcont)) %>% tail()
# 254994K hatchery coho harvested in 2023 troll fishery (est)

# total hatchery proportion
295436 / 1364991 # 22% of harvest is hatchery coho

# current wild coho in all gear harvest
1364991 - 295436 # 1,069,555 wild coho harvested

# total wild coho in all gear harvest
(1069555) / .89 # Projects out to 124366M (86%) and 120174M (89%) wild coho harvested


########################
##      SECTION 3     ##
## UPDATED WILD ABUND ##

# Post-season, wild abundance is calculated as the troll harvest rate of indicator stocks,
#  divided by the total wild troll harvest. This gives an index of wild abundance. 
# This late in the season, it's not worth modeling but rather estimating the troll wild harvest
#  and then using a 5-year average of the troll harvest rate. 



allgearharv2023_sept %>%
  filter(Year == 2023, Gear == "Power gurdy troll") %>%
  summarise(trollharvest2023 = sum(Total))
# 910534 coho harvested in troll fishery


# Through SW37, we're estimated to be 89% thru the run. 


# Total troll harv of coho = troll harvest current/run timing
910534/ 0.89 #1023072M coho estimated to be harvested in the 2023 troll fishery


# Total troll harv of wild coho = (total coho-hatchery coho)/run timing
(910534- 254994) / 0.89 # 736562 wild coho in troll
(910543- 254994) # 655549 wild coho in troll current

# Troll harvest rate of wild coho =  wild troll coho/total troll coho
(655549/910543) # 71%

# Estimated end-of-season wild abundance
# wild abund = troll wild harv / troll wild harv rate
910543 / 0.29
# 3139803 M wild coho




#####################################

allgearhatchery <- read_csv(here::here("data/SEAK_Coho_MTA_HatcheryTagRecoveryAllGear_2010-2022_13September.csv"), 
                            skip = 26, guess_max = 20000) %>%
  rename("StatWeek" = `Stat Week (CWT)`,
         "Facility_Location" = `Location (Facility or Wild Stock)`,
         "gear" = `Gear Class`) %>%
  mutate(Year = as.integer(Year)) %>% 
  group_by(Year) %>%
  summarise(totalcont = sum(Contribution, na.rm = TRUE)) 
allgearhatchery

allgearhatchery %>%
  filter(Year >= 2010) %>%
  summarise(mean(totalcont)) # 2010-2022 mean hatch coho harv = 506K



wildhatchharv <- allgearharv2023_sept %>%
  filter(Year >= 2010) %>%
  group_by(Year) %>%
  summarise(totalharvest = sum(Total)) %>% 
  left_join(allgearhatchery) %>%
  mutate(estwildharv = totalharvest - totalcont)

wildhatchharv %>%
  summarise(mean(estwildharv, na.rm = TRUE))
# 2010-2022 wild harv is 1.55M

# Compare this to the 916K projection, 79%
1236/1552

