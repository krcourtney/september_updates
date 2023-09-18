library(tidyverse)
library(lubridate)
library(adfgcolors)
library(JTPfunc)
library(scales)

curryear <- 2023


hs_escapement <- read_csv(here::here("data/SEAK_coho_HSL_escapement_1982-2023_15Sept2023.csv")) %>%
  rename(weir_name = `Weir Name`, year = Year, obs_date = `Obs Day (mm/dd)`, 
         count_type = `Weir Count Type`, sp_name = Species, lifestage = Maturity, count = Count) %>% 
  separate(obs_date, c("month", "dayofmonth"), "/") %>% # split the date so that we can make sense of it
  mutate(dayofmonth = as.numeric(dayofmonth),
         month = as.numeric(month),
         samp_date = ymd(paste0(year, "-", month, "-", dayofmonth)),
         std_date = ymd(paste0(curryear, "-", month, "-", dayofmonth))) %>%
  dplyr::select(weir_name, year, samp_date, std_date, everything()) %>%
  dplyr::select(-dayofmonth, -month, -`Obs Date`) %>%
  filter(count_type == "Counted live" | count_type == "Estimated live", 
         lifestage == "Adult",
         year >= 1982)






hs_escapement


hsinseasonplot <- hs_escapement %>% group_by(year, std_date) %>% 
  summarise(groupcount = sum(count)) %>% # sum up live, sacrificed, count types etc
  arrange(year) %>% mutate(cumsumm = cumsum(groupcount)) %>%
  mutate(yeargroup = case_when(between(year, curryear - 5, curryear) ~ as.character(year),
                               .default = "otheryears")) %>% 
  # above two lines are needed to have legend grouping show only most recent 5 years
  ggplot(aes(x=std_date, y = cumsumm, color = yeargroup, group = year)) + 
  geom_line(size = 1.25, alpha = 0.5) +
  geom_line(data = . %>% filter(between(year, curryear - 5, curryear - 1)), size = 1.5) + 
  geom_line(data = . %>% filter(year == curryear), size = 2.5) + 
  #geom_line(data = . %>% filter(year == curryear), color = "deepskyblue3", size = 2.5) +
  scale_x_date(limits = c(as.Date(paste0(curryear, "-08-01")), as.Date(paste0(curryear, "-11-01")))) +
  scale_y_continuous(labels=comma, limits = c(0, 3000)) +
  scale_color_manual(values = c("#FCA369", "#F48275", "#ED6281", "#99497A", 
                                "#463075", "deepskyblue3", "gray50"),
                     breaks = as.character(seq(from = curryear - 5, to = curryear))) + 
  # manually set colors (urgh), and set breaks to exclude "otheryears" from legend
  scale_x_date(limits = c(as.Date(paste0(curryear, "-08-01")), as.Date(paste0(curryear, "-11-01")))) +
  scale_y_continuous(labels=comma, limits = c(0, 3000)) +
  geom_abline(intercept = 1600, slope = 0) +
  geom_abline(intercept = 500, slope = 0) +
  labs(x="", y = "Cumulative Count", title = "Hugh Smith Coho Run Timing") +
  theme_crisp() +
  theme(legend.title = element_blank())
hsinseasonplot
ggsave(hsinseasonplot, filename = "hughsmithinseason_2023sept.png", dpi = 300, width = 6, height = 4, units = "in")  


hs_escapement %>% group_by(year, std_date) %>% 
  summarise(groupcount = sum(count)) %>%
  arrange(year) %>% mutate(cumsumm = cumsum(groupcount)) %>% 
  filter(std_date == "2023-09-13") %>% View()





# Old code. Simpler but doesn't have current year in the legend
# hs_escapement %>% group_by(year, std_date) %>% 
#   summarise(groupcount = sum(count)) %>% # sum up live, sacrificed, count types etc
#   arrange(year) %>% mutate(cumsumm = cumsum(groupcount)) %>%
#   ggplot(aes(x=std_date, y = cumsumm, group = as.factor(year))) + 
#   geom_line(size = 1.25, alpha = 0.5, color = "gray50") +
#   geom_line(data = . %>% filter(between(year, curryear -5, curryear - 1)), 
#             aes(color = as.factor(year)), size = 1.5) + 
#   geom_line(data = . %>% filter(year == curryear), color = "deepskyblue3", size = 2.5) +
#   scale_x_date(limits = c(as.Date(paste0(curryear, "-08-01")), as.Date(paste0(curryear, "-11-01")))) +
#   scale_y_continuous(labels=comma, limits = c(0, 3000)) +
#   scale_color_adfg(palette = "alpenglow", discrete = TRUE) +
#   geom_abline(intercept = 1600, slope = 0) +
#   geom_abline(intercept = 500, slope = 0) +
#   labs(x="", y = "Cumulative Count", title = "Hugh Smith Coho Run Timing") +
#   theme_crisp() +
#   theme(legend.title = element_blank())


