library(tidyverse)
library(lubridate)
library(gridExtra)
library(plotly)
library(roll)


iso <- read_csv("Isotope-Comparisons/Tidy_Iso.csv")

iso %>% 
  filter(region !="global") %>% 
  filter(type !="Rain") %>% 
  ggplot()+
  geom_point(aes(d18O,d2H, colour = region))+
  geom_abline(slope = 8, intercept = 10)+
  scale_colour_discrete(name = "Region")+
  scale_x_continuous(name = expression(paste(delta^18,"O", "(\u2030)")))+
  scale_y_continuous(name = expression(paste(delta^2,"H", "(\u2030)")))+
  theme_bw()+
  theme(legend.position = c(.9,.2))
  