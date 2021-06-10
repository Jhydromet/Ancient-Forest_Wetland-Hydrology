
library(tidyverse)
library(lubridate)
library(plotly)
library(kableExtra)

# This script plots level logger data, and also summarises monthly water elvels and annual differences


daily.data <- read_csv("Level-Logger-Data-Compiling/gw-waterlevel-dat.csv") %>% 
  mutate(month = month(datetime,label = T,abbr = T),
         date = date(datetime),
         year = year(datetime),
         trans = substr(site, 1,1),
         yday = yday(date)) %>% 
  filter(year != "NA") %>% 
  filter(month %in% c("Apr","May","Jun","Jul","Aug","Sep","Oct"))

# data length plot

p <- daily.data %>%
  filter(date >= ymd("2019-03-28")) %>%
  filter(site !="B3" & site != "A3") %>%
  drop_na(wl) %>% 
  ggplot()+
  geom_line(aes(x = yday, y = site, colour = site), size = 3)+
  scale_x_continuous(breaks = c(92,122,153,183,214,245,275,306),
                     labels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"))+
  labs(x = "Date", y = "Site")+
  theme(legend.position = "none")+
  facet_grid(rows = vars(year))

ggsave(plot = p, filename = "wl-length.png", device = "png", width = 6.5,units = "in")

# corrected data plot

# daily.data %>%
#   filter(date >= ymd("2019-03-28")) %>%
#   filter(site!="ECRK" & site != "WCRK" & site !="B3" & site != "A3") %>%
#   ggplot()+
#   geom_line(aes(x = date, y = wl, colour = site))+
#   # geom_point(aes(x = date, y = gtw_cm_man, colour = site))+
#   geom_abline(slope = 0, intercept = 0, linetype = "dashed")+
#   facet_grid(rows = "trans", scales = "free")+
#   labs(x = "Date", y = "Water Level (cm)")+
#   scale_colour_discrete(name = "Site")

monthly.data <- daily.data %>% 
  filter(!(trans %in% c("E","W"))) %>% 
  group_by(year,month,site,trans) %>% 
  summarise(mn.wl = mean(wl,na.rm = T)) %>% 
  rename(Piezometer = "site")

difference <- monthly.data %>% 
  pivot_wider(names_from = year, values_from = mn.wl) %>% 
  mutate(diff = `2020`-`2019`) %>% 
  drop_na(diff)

p <- difference %>% 
  ggplot()+
  geom_col(aes(month,diff, fill = Piezometer), position = "dodge")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x = "Month", y = "Difference in Water Level (cm)")+
  facet_grid(trans~., scales = "free")+
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "horizontal",
        legend.title = element_blank())

p
# ggsave(p, filename = "MonthlyWaterdiff.png",device = "png", width = 6.5, units = "in")



a <- monthly.data %>% 
  filter(trans == "A") %>% 
  ggplot()+
  geom_col(aes(month,mn.wl,fill = Piezometer),position = "dodge")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x = "Month", y = NULL)+
  facet_grid(trans+year ~.)
a

b <- monthly.data %>% 
  filter(trans == "B") %>% 
  filter(Piezometer %in% c("B1","B4")) %>% 
  ggplot()+
  geom_col(aes(month,mn.wl,fill = Piezometer),position = "dodge")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x = "Month", y = NULL)+
  facet_grid(trans+year ~.)
b

c <- monthly.data %>% 
  filter(trans == "C") %>%
  filter(Piezometer %in% c("C1","C4")) %>% 
  ggplot()+
  geom_col(aes(month,mn.wl,fill = Piezometer),position = "dodge")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x = "Month", y = NULL)+
  facet_grid(trans+year ~.)
c

p <- grid.arrange(a,b,c)

# ggsave(p, filename = "MonthlyWaterLevel.png",device = "png", width = 6.5, units = "in")
