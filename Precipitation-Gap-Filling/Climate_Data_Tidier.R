library(tidyverse)
library(lubridate)
library(plotly)
library(RcppRoll)

setwd("E:/Jeremy's MSc Research/Hydrometric and GIS/Climate Data")

# it will be best to avg on daily timescale before bonding with tipping bucket data.


# Read in & Tidy Regional Data --------------------------------------------

# Read in the regional precip dat (Longworth & Dome Mtn) This is cumulative precip data (from a pluvio?), join together, fix col headers, add Rain_Tot data col (not cumulative)

longprecip <- read_csv("E:/Jeremy's MSc Research/Hydrometric and GIS/Climate Data/LongworthCumPrecip.csv", skip = 2) %>% 
  mutate(Site = "Longworth")

domeprecip <- read_csv("E:/Jeremy's MSc Research/Hydrometric and GIS/Climate Data/DomeMtnCumPrecip.csv", skip = 2) %>% 
  mutate(Site = "DomeMtn")

precipdat <- full_join(longprecip,domeprecip)

colnames(precipdat) <- c("Timestamp", "CuPrecip", "Grade", "Approval","Interpolation", "event", "Site")

precipdat <- precipdat%>%
  mutate(date = date(Timestamp)) %>% 
  group_by(Site, date) %>% 
  summarise(CuPrecip = mean(CuPrecip, na.rm = T)) %>% 
  mutate(Rain_Tot = CuPrecip - lag(CuPrecip, 1))

precipdat$Rain_Tot[precipdat$Rain_Tot < 0] = 0

precipdat <- precipdat %>% 
  pivot_longer(cols = c(Rain_Tot,CuPrecip), names_to = "Parameter")


# Read in the regional Temperature dat (Longworth & Dome Mtn) Units are celsius, join together, fix col headers.

longairt <- read_csv("E:/Jeremy's MSc Research/Hydrometric and GIS/Climate Data/LongworthAirT.csv", skip = 2) %>% 
  mutate(Site = "Longworth",
         `Timestamp (UTC-07:00)` = mdy_hm(`Timestamp (UTC-07:00)`)) 

domeairt <- read_csv("E:/Jeremy's MSc Research/Hydrometric and GIS/Climate Data/DomeMtnAirT.csv", skip = 2) %>% 
  mutate(Site = "DomeMtn")

tempdat <- full_join(longairt,domeairt)

colnames(tempdat) <- c("Timestamp", "AirTC", "Grade", "Approval","Interpolation", "event", "Site")

tempdat <- tempdat %>% 
  mutate(date = date(Timestamp),
         AirTC = as.numeric(gsub(x = AirTC, pattern = ",", replacement = "."))) %>%
  group_by(Site, date) %>% 
  summarise(AirTC = mean(AirTC, na.rm = T)) %>% 
  pivot_longer(cols = AirTC, names_to = "Parameter")

regdat <- full_join(precipdat,tempdat)

# I'm filtering this data between april and nov. 

regdat  %>%
  filter(month(date)>= 4 & month(date)<=10) %>% 
  ggplot(aes(x = date, y = value))+
  geom_line(data = filter(.data = regdat, Parameter == "AirTC"), aes(colour = Site))+
  geom_col(data = filter(.data = regdat, Parameter == "Rain_Tot"), aes(fill = Site))+
  geom_line(data = filter(.data = regdat, Parameter == "CuPrecip"), aes(colour = Site))+
  facet_grid(rows = vars(Parameter),
             scales = "free")

# Read in AF and BVR data -------------------------------------------------

afraw <- read_csv("E:/Jeremy's MSc Research/Hydrometric and GIS/Climate Data/AF_Station.csv") %>% 
  mutate(value = Value) %>% 
  select(-Value)

aftemp <- afraw %>% 
  filter(Parameter == "AirTC") %>% 
  filter(Timestamp <= ymd_hms("2020-10-15 19:00:00") | Timestamp >= ymd_hms("2020-10-17 04:00:00")) %>% 
  filter(Timestamp <= ymd_hms("2020-10-20 18:00:00") | Timestamp >= ymd_hms("2020-10-22 00:00:00"))

p <- aftemp %>% 
  filter(Parameter == "AirTC") %>% 
  ggplot(aes(x = Timestamp, y = value))+
  geom_line()
ggplotly(p)

afnoprecip <- afraw %>% 
  filter(Parameter != "AirTC")

afdat <- bind_rows(afnoprecip, aftemp)
  

bvdat <- read_csv("E:/Jeremy's MSc Research/Hydrometric and GIS/Climate Data/BVR_Precip_AirT.csv", skip = 1)

colnames(bvdat) <- c("record","Timestamp","Rain_Tot","AirTC","RH")

bvdat <- bvdat %>% 
  mutate(Timestamp = round_date(mdy_hms(Timestamp),unit = "minute"),
         Site = "BeaverLake") %>% 
  select(-record) %>% 
  filter(Rain_Tot >= 0) %>% 
  pivot_longer(cols = c(Rain_Tot,AirTC,RH), names_to = "Parameter")

localdat <- full_join(afdat,bvdat) %>%
  mutate(date = date(Timestamp)) %>% 
  filter(Parameter == "AirTC" |
         Parameter == "Rain_Tot") %>% 
  pivot_wider(names_from = Parameter, values_from = value) %>% 
  group_by(Site,date) %>% 
  summarise(AirTC = mean(AirTC, na.rm = T),
            Rain_Tot = sum(Rain_Tot, na.rm = T),
            ) %>% 
  pivot_longer(cols = c(AirTC, Rain_Tot), names_to = "Parameter")

# Join all data together and plot for duration of beaver lake data

dat <- full_join(localdat,regdat) %>% 
  filter(date >= "2020-07-01" & date <= "2021-01-14")


dat$par.labs <- factor(dat$Parameter,
                           levels = c("AirTC","CuPrecip","Rain_Tot"),
                           labels = c(expression(paste("Temp  ","(\u00B0C)")),
                                      expression("CuRain~(mm)"),
                                      expression("Rain~(mm)")))

p <- dat %>% 
  filter(Parameter != "CuPrecip") %>% 
  ggplot(aes(x = date, y = value))+
  theme(strip.text.y = element_text( size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"))+
  labs(y = NULL,x = "Date")+
  scale_fill_discrete(name = "Station")+
  scale_colour_discrete(name = "Station")+
  geom_line(data = filter(.data = dat, Parameter == "AirTC"), aes(colour = Site))+
  
  geom_abline(slope = 0, intercept = 0,linetype = "dashed")+
  geom_col(data = filter(.data = dat, Parameter == "Rain_Tot"), aes(fill = Site))+
  # geom_line(data = filter(.data = dat, Parameter == "CuPrecip"), aes(colour = Site))+
  facet_grid(rows = vars(par.labs),
             scales = "free",
             switch = "both",
             labeller = label_parsed)

ggplotly(p)
  


# Gap Fill AF Precip Data -------------------------------------------------

# DATE RANGE OF NO AF DATA: raindat$AncientForest[raindat$date>= "2020-08-12" & raindat$date>= "2020-10-04"] = NA
# OVERLAPPING WITH ALL STATions: raindat$date>= "2020-10-04" & raindat$date>= "2020-11-05"]
# LONGEST OVERLAP WITH LONGWORTH & DOMEMTN:  date >= ymd("2020-05-01") & date <= ymd("2020-08-12")

raindat <- dat %>% 
  filter(Parameter == "Rain_Tot") %>%
  filter(date >= ymd("2020-05-01") & date <= ymd("2020-10-12")
) %>% 
  select(-Parameter, -par.labs) %>%
  mutate(value = RcppRoll::roll_sum(value, n = 3L, align = "right", na.rm = FALSE, fill = numeric(3))) %>% 
  pivot_wider(names_from = Site, values_from = value) %>% 
  pivot_longer(cols = c(-date, -AncientForest, Longworth, BeaverLake,DomeMtn), names_to = "Site")

raindat$AncientForest[raindat$date>= ymd("2020-08-12") & raindat$date>= ymd("2020-10-04")] = NA

rainwide <- dat %>% 
  filter(Parameter == "Rain_Tot") %>%
  filter(date >= ymd("2020-05-01") & date <= ymd("2020-10-12")
  ) %>% 
  select(-Parameter, -par.labs) %>%
  mutate(value = RcppRoll::roll_sum(value, n = 3L, align = "right", na.rm = FALSE, fill = numeric(3))) %>% 
  pivot_wider(names_from = Site, values_from = value) 

rainwide$AncientForest[rainwide$date>= ymd("2020-08-12") & rainwide$date>= ymd("2020-10-04")] = NA


lm1 <- lm(data = rainwide, formula = AncientForest ~ .)
summary(lm1)



rainstat <- raindat %>% 
  nest(-Site) %>% 
  mutate(fit = map(data, ~ lm(AncientForest ~ value, data = .)),
         results = map(fit, augment)) %>% 
  unnest(results)
  
rainstat$data[raindat$Site == "DomeMtn"]
  
rainstat  %>% 
  ggplot(aes(x = AncientForest, y = .fitted)) +
  geom_abline(intercept = 0, slope = 1, alpha = .2) +  # Line of perfect fit
  geom_point() +
  facet_grid(Site ~ .) +
  theme_bw()
 








p <- raindat %>% 
  ggplot(aes(x = AncientForest, y = value))+
  geom_point(aes(colour = Site))+
  geom_smooth(method = "lm")+
  facet_grid(~Site)

ggplotly(p)



