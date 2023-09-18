
# SEPTEMBER UPDATE

library(tidyverse)
library(lubridate)
library(patchwork)
library(JTPfunc)


termareas <- c("10140", "10140", "10195", "10735", 
               "11222", "11333", "11335", "11338") 
# These are terminal harv areas and should be excluded as per G. Hagerman

area1 <- c(116, 156, 157, 181, 182, 183, 185, 186, 189, 191, 192)
area2 <- c(113, 154)
area3 <- c(103, 104, 152)
area4 <- c(111, 112, 114, 115)
area5 <- c(105, 106, 107, 108, 109, 110)
area6 <- c(101, 102, 150)

curryear <- 2023
endstatweek = 37
statweek(now())


fishtix <- read_csv(here::here("data/SEAK_coho_harvest_1985-2023_14sept2023.csv")) %>%
  rename(year = `DOL Year`, date_landing = `Date of Landing`, 
         statweek = `Stat Week`, gear = `Gear Name`, 
         harvest = `Harvest Name`, species = `Species Code`, 
         district = District, 
         count = `Number Of Animals (sum)`, weight = `Landed Weight (sum)`) %>%
  dplyr::select(year:weight, -Region) %>%  
  mutate(date_landing = as.Date(date_landing, format = "%m/%d/%Y"),
         statarea = if_else(Subdistrict == 0, paste0(district, Subdistrict, "0"),
                            paste0(district, Subdistrict))) %>%
  filter(harvest == "State managed fishery" | harvest == "Spring Troll Fishery", gear != "Longline (hook and line)",
         !is.na(gear), !statarea %in% termareas)


elandings <- read_csv(here::here("data/SEAK_coho_harvestpending_2023_14sept2023.csv")) %>%
  rename(year = Year, date_landing = `Date of Landing`, 
         statweek = `DFE Stat Week`, gear = Gear, 
         harvest = `Harvest Name`, statarea = `Stat Area`,
         district = `Salmon Mgmt District`,
         species = `Species Code`, 
         count = `Number Of Animals (sum)`, weight = `Landed Weight (sum)`) %>%
  dplyr::select(year:count, -Region) %>%
  mutate(date_landing = as.Date(date_landing, format = "%m/%d/%Y")) %>%
  filter(harvest == "State managed fishery" | harvest == "Spring Troll Fishery",
         gear != "Fish ladder/race way", !is.na(gear),
         !statarea %in% termareas)

fishtix_summary <- fishtix %>% group_by(year, statweek, gear, district) %>%
  summarise(totalcount = sum(count, na.rm = TRUE))

elandings_summary <- elandings %>% group_by(year, statweek, gear, district) %>%
  summarise(totalcount = sum(count, na.rm = TRUE))




cohoharvest <- fishtix_summary %>% 
  full_join(elandings_summary) %>% 
  group_by(year, statweek, gear, district) %>%
  summarise(totalcount = sum(totalcount, na.rm = TRUE)) %>%
  mutate(area = ifelse(district %in% area1, 1, 
                       ifelse(district %in% area2, 2, 
                              ifelse(district %in% area3, 3, 
                                     ifelse(district %in% area4, 4, 
                                            ifelse(district %in% area5, 5, 
                                                   ifelse(district %in% area6, 6, NA)))))),
         areaname = if_else(area == 1, "Area1-\nYakutat", 
                            if_else(area == 2, "Area2-\nSitka", 
                                    if_else(area == 3, "Area3-\nCraig", 
                                            if_else(area == 4, "Area4-\nJuneau/Haines", 
                                                    if_else(area == 5, "Area5-\nPetersburg", 
                                                            if_else(area == 6, "Area6-\nKetchikan", "ERROR")))))))
# the "\n" is a new line character to help with plotting later


cohoharvest %>% filter(year != curryear, statweek <= endstatweek) %>%
  group_by(year,  gear) %>%
  summarise(count = sum(totalcount, na.rm = TRUE)) %>%
  group_by(gear) %>%
  summarise(annualmean = mean(count, na.rm = TRUE))


harv2023 <- cohoharvest %>% filter(year == curryear, statweek <= endstatweek) %>%
  group_by(gear, statweek, area, areaname) %>%
  summarise(count2023 = sum(totalcount, na.rm = TRUE))

harv2023



harv17_22 <- cohoharvest %>% filter(between(year, 2017, curryear), statweek <= endstatweek) %>%
  group_by(year, gear, statweek, area, areaname) %>%
  summarise(totalcount = sum(totalcount, na.rm = TRUE)) 
harv17_22

harv17_22_summ <- harv17_22 %>%
  group_by(gear, statweek, areaname) %>%
  summarise(annualmean = mean(totalcount, na.rm = TRUE))
harv17_22_summ


harv2023 %>% left_join(harv17_22_summ) %>%
  filter(between(statweek, 29, endstatweek),
         gear != "Hand troll", gear != "Set gillnet", gear != "Longline (hook and line")




harv17_22 %>%
  filter(between(statweek, 29, endstatweek),  # CHECK THIS DATE RANGE
         gear != "Hand troll", gear != "Set gillnet") %>%
  ggplot(aes(x=as.factor(statweek), y = totalcount)) +
  geom_boxplot(aes(fill = gear)) +
  geom_point(data = harv2023 %>%
               filter(between(statweek, 29, endstatweek),
                      gear != "Hand troll", gear != "Set gillnet", gear != "Longline (hook and line)"),
             aes(x=as.factor(statweek), y = count2023), color = "deepskyblue3") +
  scale_fill_manual(values = c("aliceblue", "lavenderblush1", "moccasin","ivory1", "bisque")) +
  facet_grid(areaname~gear, scales = "free_y")+
  theme_bw()

"aliceblue"
"ivory1"
"bisque"
"lavenderblush1"
"mistyrose"
"moccasin"






########################
# FINAL FIGURE
driftharv23 <- harv17_22 %>%
  filter(between(statweek, 29, endstatweek),
         gear == "Drift gillnet", area !=3, area != 2) %>% # exclude these two areas
  ggplot(aes(x=as.factor(statweek), y = totalcount)) +
  geom_boxplot(fill = "#ffeccc") + # Used to be moccasin w 0.6 alpha but lightened points
  geom_point(data = harv2023 %>%
               filter(between(statweek, 29, endstatweek),
                      gear == "Drift gillnet", area != 3, area != 2),
             aes(x=as.factor(statweek), y = count2023), 
             color = "deepskyblue3") +
  scale_x_discrete(breaks = seq(from = 29, to = endstatweek, by = 2)) +
  facet_grid(rows = vars(areaname)) +
  labs(title = "Drift gillnet", x = "Stat Week", y = "Coho Harvest (count)") + 
  theme_crisp()
driftharv23
driftharv23fig <- plot_spacer() / driftharv23 # If title added here, overwritten when combined
ggsave(driftharv23fig, filename = "output/driftharv22_sept.png", dpi = 600, width = 3.5, height = 7, units = "in")



trollseineharv23 <- harv17_22 %>%
  filter(between(statweek, 29, endstatweek),
         gear == "Power gurdy troll" | gear == "Purse seine") %>%
  ggplot(aes(x=as.factor(statweek), y = totalcount)) +
  geom_boxplot(aes(fill = gear)) +
  geom_point(data = harv2023 %>%
               filter(between(statweek, 29, endstatweek),
                      gear == "Power gurdy troll" | gear == "Purse seine"),
             aes(x=as.factor(statweek), y = count2023), color = "deepskyblue3") +
  scale_fill_manual(values = c("aliceblue", "lavenderblush1")) +
  scale_x_discrete(breaks = seq(from = 29, to = endstatweek, by = 2)) +
  facet_grid(areaname~gear, scales = "free_y") +
  labs(x = "Stat Week", y = "Coho Harvest (count)") +
  theme_crisp() +
  theme(legend.position = "none",
        panel.spacing.x = unit(1.5, "lines"))
trollseineharv23
ggsave(trollseineharv23, filename = "output/trollseineharv22_sept.png", dpi = 600, width = 7, height = 7, units = "in")



allharv_aug <- trollseineharv23 + driftharv23fig + plot_layout(widths = c(2.25, 1))
allharv_aug
ggsave(allharv_aug, filename = "output/allharv22_sept.png", dpi = 600, width = 7, height = 7, units = "in")





