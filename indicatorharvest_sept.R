library(tidyverse)
library(lubridate)
library(adfgcolors)
library(JTPfunc)

curryear = 2023

# Note in 2022 that the MTA lab changed Source "COM PROP" to "COM CP" 
mta_hsl <- read_csv(here::here("data/SEAK_coho_tagrecovery_hughsmith_2018-2023_14sept2023.csv"), skip = 26) %>%
  rename(year = Year, date = `Date (CWT)`, 
         statweek = `Stat Week (CWT)`, gear = `Gear Class`,
         sample_code = `Sampling Type Code`, river = `Location (Facility or Wild Stock)`) %>% 
  filter(Source == "COM CP" | Source == "SPORT", sample_code == "R") %>%
  dplyr::select(-Sample)

mta_bern <- read_csv(here::here("data/SEAK_coho_tagrecovery_berners_2018-2023_14sept2023.csv"), skip = 26) %>%
  rename(year = Year, date = `Date (CWT)`, 
         statweek = `Stat Week (CWT)`, gear = `Gear Class`,
         sample_code = `Sampling Type Code`, river = `Location (Facility or Wild Stock)`) %>% 
  filter(Source == "COM CP" | Source == "SPORT", sample_code == "R") %>%
  dplyr::select(-Sample)

mta_auke <- read_csv(here::here("data/SEAK_coho_tagrecovery_auke_2018-2023_14sept2023.csv"), skip = 26) %>%
  rename(year = Year, date = `Date (CWT)`, 
         statweek = `Stat Week (CWT)`, gear = `Gear Class`,
         sample_code = `Sampling Type Code`, river = `Location (Facility or Wild Stock)`) %>% 
  filter(Source == "COM CP" | Source == "SPORT", sample_code == "R") %>%
  dplyr::select(-Sample)



# Combine and summarize
mta_all <- mta_hsl %>% full_join(mta_auke) %>% full_join(mta_bern) %>%
  mutate(river = recode(river, "(W) AUKE CR" = "Auke Creek", 
                        "(W) AUKE LK 111-50" = "Auke Creek",
                        "(W) BERNERS R 115-20" = "Berners River",
                        "(W) HUGH SMITH LK 101-30" = "Hugh Smith Lake")) %>%
  group_by(river, year, statweek) %>%
  summarise(totalcont = sum(Contribution, na.rm = TRUE)) %>%
  full_join(crossing(river = c("Auke Creek", "Berners River", "Hugh Smith Lake"), 
           year = 2017:2022, statweek = 28:34)) %>% 
  #prev two lines expand out every combo b/c some statweeks had no samples 
  arrange(river, year, statweek) %>%
  replace(is.na(.), 0) %>% # now replace those NAs with zeros
  mutate(cumtotal = cumsum(totalcont))
mta_all

# 5 year mean:
mta_all %>%
  filter(year != curryear) %>%
  group_by(river, statweek) %>% 
  summarise(annmean = mean(totalcont, na.rm = TRUE)) %>%
  mutate(tot = cumsum(annmean)) %>% View()


indicharv23 <- mta_all %>% 
  ggplot(aes(x=statweek, y = cumtotal, group = year)) +
  geom_line(data = . %>% filter(year != curryear), color = "gray60", size = 1.25) +
  geom_line(data = . %>% filter(year == curryear), color = "cornflowerblue", size = 2.5) +
  scale_color_adfg(palette = "alpenglow", discrete = FALSE) +
  facet_grid(~river) +
  labs(x = "Stat Week", y = "Estimated Coho Harvest") +
  #theme_bw()
  theme_crisp() 
indicharv23

ggsave(indicharv23, filename = "output/indicatorharvest2022_sept.png", dpi = 500, width = 6, height = 4, units = "in")

