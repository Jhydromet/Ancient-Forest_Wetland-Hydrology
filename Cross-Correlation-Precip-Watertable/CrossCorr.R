library(tidyverse)
library(lubridate)
library(plotly)
library(gridExtra)
library(roll)
library(plotly)
library(kableExtra)
library(tseries)
library(tidymodels)

rain <- read_csv("Cross-Correlation-Precip-Watertable/AF_Station.csv") %>%
  filter(Parameter == "Rain_Tot") %>% 
  pivot_wider(names_from = "Parameter", values_from = "Value") %>% 
  dplyr::select(-Site) %>% 
  dplyr::rename(timestamp = "Timestamp", rain_mm_tot = "Rain_Tot") 

roll_rain <- rain %>% 
  dplyr::mutate(mn.rain = roll_mean(rain_mm_tot, width = 24)) %>% 
  drop_na()

dat <- read_csv("Level-Logger-Data-Compiling/gw-waterlevel-dat.csv") %>% 
  rename(timestamp = "datetime")

sites <- dat %>% 
  dplyr::select(site) %>% 
  filter(!(site %in% c("ECRK","WCRK","B5"))) %>% 
  distinct()

sites <- sites$site


correlator <- function(sites) {
  wldat <- dat %>% 
    filter(site == sites) %>% 
    mutate(wl = roll_mean(wl, width = 24)) %>% 
    drop_na(wl)
  
  cordat <- full_join(roll_rain, wldat) %>% 
    drop_na() %>%
    distinct() %>%
    filter(date(timestamp) >= "2019-06-01" & date(timestamp) <= "2019-11-01")
  
  bccf <-  ccf( cordat$wl,cordat$mn.rain,plot = TRUE, lag = 1000, ylab = paste0("ccf ", sites))
  
  bacfdf <- with(bccf, data.frame(lag, acf))%>%
    rename(crcor = "acf", lagg = "lag") %>%
    mutate(site = sites)
}


data <- sites %>%
  map(correlator) %>%
  reduce(rbind)

data <- data  %>% 
  mutate(valid = case_when(
    site %in% c("A1","A4","B4")~"poor",
    TRUE ~ "good"
  ))

conf.level <- 0.95
ciline <- qnorm((1 + conf.level)/2)/sqrt(14781) # 14781 is the number of data pts in each timeseries

p <- data %>%
  ggplot()+
  # geom_line(data = filter(data, valid == "poor"),aes(lagg,crcor, group = site), colour = "grey", size = .9)+
  geom_line(aes(lagg,crcor,colour = site), size = .9)+
  # geom_line(aes(lagg,crcor,colour=site))+
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0))+
  geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') +
  geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue')+
  # geom_segment(mapping = aes(xend = lagg, yend = 0))+
  labs(x = "Lag (15 Min)", y = "Correlation Coefficient")+
  scale_colour_discrete(name = "Site")+
  theme(legend.position = "none")+
  facet_grid(rows = "site",
             scales = "free")


# , title = paste(" 6 hr rollmean Rain to Water Level Correlation 01 Jun - 01 Nov 2019")

p

ggsave(p, filename = "Crosscorr.png",device = "png",width = 6.5, height = 6, units = "in")



lag.dat <- data %>%
  dplyr::filter(valid == "good") %>% 
  dplyr::group_by(site) %>% 
  dplyr::filter(crcor >= ciline & crcor == max(crcor)) %>%
  dplyr::mutate(lag.hrs = lagg/4,
                p.norm = 
                p.val = 2 * (1 - pnorm(abs(crcor), mean = 0, sd = 1/sqrt(14781)))) %>% 
  dplyr::select(site,lagg,lag.hrs,crcor, p.val)

kable(lag.dat) %>% 
  kable_material()

table <- kbl(lag.dat,digits = 2,
              col.names = c("Site","Lag (15 min)","Lag (hours)","Correlation Coeff."))%>% 
  kable_styling()


  as_image(table, file = "CrossTable.png")







lag.summary <- lag.dat %>%
  dplyr::ungroup() %>% 
  dplyr::summarise(mn.lag.hrs = mean(lag.hrs),
                   sd.lag.hrs = sd(lag.hrs))

kable(lag.summary) %>% 
  kable_material()
