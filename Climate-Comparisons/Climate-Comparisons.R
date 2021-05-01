library(tidyverse)
library(lubridate)
library(gridExtra)
library(plotly)
library(roll)


# Prince George Airport Plot ----------------------------------------------

# Comparing 2019, 2020 with 1989-2010 climate normals

pgdat <- paste0("Climate-Comparisons/pg_daily_data/", dir(path = "Climate-Comparisons/pg_daily_data/"))

pgdat <- pgdat %>% 
  map(read_csv) %>% 
  reduce(full_join)

pgthin <- pgdat %>%
  dplyr::rename(date = `Date/Time`, temp = `Mean Temp (°C)`, precip = `Total Precip (mm)`) %>%
  dplyr::select(date,temp,precip)

pgmonthly <- pgthin %>% 
  mutate(year = as.factor(year(date)),
         month = month(date,label = T, abbr = T)) %>% 
  group_by(year,month) %>% 
  summarise(temp = mean(temp,na.rm=T),
            precip = sum(precip, na.rm = T))

pgmonthly %>% 
  ggplot()+
  geom_line(aes(month,precip,group = year, colour = year))

pgnorm <- read_csv("Climate-Comparisons/pgnorm.csv") %>% 
  dplyr::select(-snowd,-rain) %>% 
  filter(month != "Annual") %>% 
  mutate(year = "Historical Normal")


pgnorm$month <- as.ordered(factor(pgnorm$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun" ,"Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))


pgclimate <- full_join(pgnorm,pgmonthly)

p1 <- pgclimate %>% 
  ggplot()+
  geom_col(aes(month,precip,group = year, fill = year),position = "dodge")+
  labs(x = "Month", y = "Precipitation (mm)")+
  scale_fill_discrete(name = "Year")+
  theme(legend.position = c(.15,.75))

p2 <- pgclimate %>% 
  ggplot()+
  geom_line(aes(month,temp,group =year, colour = year), size = .75)+
  geom_abline(slope = 0,intercept = 0, linetype = "dashed")+
  labs(x = "Month", y = "Air Temperature (°C)")+
  scale_colour_discrete(name = "Year")+
  theme(legend.position = c(.15,.75))

grid.arrange(p1,p2)



###########################################################################
# DOME MOUNTAIN DATA PLOTTING ---------------------------------------------

# SWE DATA WRANGLING----------------------------------------------------------------



domesw <- read_csv("Climate-Comparisons/other climate data/DataSetExport-SW.Telemetry@1A19P-20210412182208.csv",skip = 2) %>% 
  rename(datetime = `Timestamp (UTC)`, swe.mm = `Value (Millimetres)`) %>% 
  dplyr::select(datetime,swe.mm) %>% 
    mutate(year = year(datetime),
         yday = yday(datetime),
         date = date(datetime)) %>% 
  filter(swe.mm>=0 & datetime >= ymd_hms("2006-10-20 07:00:00")) %>% 
  group_by(date) %>% 
  mutate(daily.swe.mm = mean(swe.mm)) %>% 
  ungroup() %>% 
  group_by(yday) %>% 
  mutate(mean.swe.plt = mean(swe.mm)) %>% 
  filter(date >= ymd("2018-10-01")) %>% 
  ungroup()


domestudy <- domesw %>% 
  filter(date >= ymd("2018-10-01") & date <= ymd("2020-09-30")) %>% 
  mutate(wateryear = case_when(
    date >= ymd("2018-10-01") & date <= ymd("2019-09-30") ~ "2018/2019",
    date >= ymd("2019-10-01")  ~ "2019/2020")) %>% 
  mutate(wyday = case_when(
    yday >= 0 & yday < 274 ~ yday + 91,
    yday >=274 ~ yday-274))

domeswavg <- domestudy %>% 
  select(wyday, wateryear, mean.swe.plt) %>% 
  rename(swe = "mean.swe.plt") %>% 
  mutate(wateryear = "16 Year Mean")

domeswdat <- domestudy %>% 
  select(wyday, wateryear, daily.swe.mm) %>% 
  rename(swe = "daily.swe.mm")

domeswe <- bind_rows(domeswavg,domeswdat)

domeswe$wateryear = factor(domeswe$wateryear, levels = c("2018/2019","2019/2020","16 Year Mean"))


# SWE PLOT ----------------------------------------------------------------



p1 <- domeswe %>%
  filter(wyday <= 340) %>% 
  ggplot()+
  geom_line(aes(wyday, swe, colour = wateryear), size =1)+
  scale_x_continuous(breaks = c(0,31,61,92,123,151,182,213,242,273,304,334),
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul", "Aug","Sep"),
                    minor_breaks = NULL)+
  scale_colour_manual(name = "Water Year",values=c("#F8766D", "#00BFC4", "#999999"))+
  labs(x = "Month", y = "Snow Water Equivalent (mm)")+
  theme(legend.position = "none")



###############################################################################
# MONTHLY AIR TEMPERATURE LINE PLOT ---------------------------------------


dometa <- read_csv("Climate-Comparisons/other climate data/DataSetExport-TA.Working@1A19P-20210414221824.csv", skip = 2) %>% 
  rename(datetime = `Timestamp (UTC)`, temp = `Value (Celsius)`) %>%
  select(datetime, temp) %>% 
  mutate(date = date(datetime),
         yday = yday(date),
         month = month(date, label = T)) %>% 
  group_by(month) %>% 
  mutate(mn.temp = mean(temp,na.rm = T)) %>% 
  ungroup() %>% 
  filter(date >= ymd("2018-10-01")) %>% 
  mutate(wateryear = case_when(
           date >= ymd("2018-10-01") & date <= ymd("2019-09-30") ~ "2018/2019",
           date >= ymd("2019-10-01")  ~ "2019/2020",
           TRUE ~ "NA")) %>% 
  group_by(wateryear,month) %>% 
  mutate(mn.monthly.temp = mean(temp,na.rm=T))

dometa$month = factor(dometa$month, levels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May", "Jun","Jul", "Aug","Sep"))
  
dometavg <- dometa %>% 
  select(wateryear, mn.temp) %>% 
  rename(temp = "mn.temp") %>% 
  mutate(wateryear = "16 Year Mean")

dometdat <- dometa %>% 
  select(wateryear, mn.monthly.temp) %>% 
  rename(temp = "mn.monthly.temp")

dometemp <- bind_rows(dometavg,dometdat)

dometemp$wateryear = factor(dometemp$wateryear, levels = c("2018/2019","2019/2020","16 Year Mean"))



# TEMPERATURE PLOTTING ----------------------------------------------------

p2 <- dometemp %>% 
  ggplot()+
  geom_line(aes(x = month, y = temp, colour = wateryear, group = wateryear), size =1)+
  geom_abline(slope = 0,intercept = 0, linetype = "dashed")+
  scale_colour_manual(name = "Water Year", values=c("#F8766D", "#00BFC4", "#999999"))+
  labs(x = "Month", y = "Air Temperature (°C)")+
  theme(legend.position = "none")

###########################################################################
# Precipitation Data Wrangle ----------------------------------------------


domepc <- read_csv("Climate-Comparisons/other climate data/DataSetExport-PC.Working@1A19P-20210414154037.csv", skip = 2) %>% 
  rename(datetime = `Timestamp (UTC)`, rain = `Value (Millimetres)`) %>%
  select(datetime,rain) %>% 
  mutate(date = date(datetime),
         yday = yday(date),
         month = month(date, label = T),
         year = year(date)) %>% 
  filter(year >= "2014" | year <= "2011") %>% 
  group_by(date) %>% 
  summarise(mn.daily.val = mean(rain,na.rm = T),
            date = date(datetime),
            yday = yday(date),
            month = month(date, label = T),
            year = year(date)) %>% 
  ungroup() %>% 
  mutate(val = mn.daily.val - lag(mn.daily.val, 1),
         val = case_when(
           val < 0 ~ 0,
           TRUE ~ val
         )) %>% 
  ungroup() %>% 
  group_by(month,year) %>% 
  mutate(sm.month = sum(val, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(mn.month = mean(sm.month, na.rm = T)) %>% 
  select(date,month,sm.month,mn.month) %>% 
  filter(date >= ymd("2018-10-01")) %>% 
  mutate(wateryear = case_when(
    date >= ymd("2018-10-01") & date <= ymd("2019-09-30") ~ "2018/2019",
    date >= ymd("2019-10-01")  ~ "2019/2020",
    TRUE ~ "NA"))

domeavg <- domepc %>% 
  select(month, mn.month) %>% 
  rename(mm.precip = "mn.month") %>% 
  mutate(wateryear = "16 Year Mean")

domeyrs <- domepc %>% 
  select(-date,-mn.month) %>% 
  rename(mm.precip = "sm.month")

domepc <- bind_rows(domeavg,domeyrs)


  
domepc$month = factor(domepc$month, levels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May", "Jun","Jul", "Aug","Sep"))

domepc$wateryear = factor(domepc$wateryear, levels = c("2018/2019","2019/2020","16 Year Mean"))



# Precipitation Column Plot -----------------------------------------------



p3 <- domepc %>% 
  ggplot()+
  geom_col(aes(month, mm.precip, fill = wateryear), position = "dodge")+
  scale_fill_manual(values=c("#F8766D", "#00BFC4", "#999999"), name = "Water Year")+
  labs(x = "Month", y = "Precipitation (mm)")+
  theme(legend.position = c(.95,.8))



# PLOT THE GRID -----------------------------------------------------------



grid.arrange(p1,p3,p2)


# 
# 
# 
# domeswe <- domesw %>% 
#   select(-mean.swe.plt) %>% 
#   rename(value = "swe.mm") %>% 
#   mutate(name = "swe")
# 
# domedat <- bind_rows(domeswe,domepc,dometa) %>% 
# filter(date >= "2019-01-01")
# 
# 
# p <- domedat %>% 
#   ggplot(aes(x = date, y = value))+
#   geom_col(data = filter(.data = domedat, name == "rain"))+
#   geom_line(data = filter(.data = domedat, name == "swe"))+
#   geom_line(data = filter(.data = domedat, name == "temp"))+
#   facet_grid(rows = vars(name), scales = "free")
# ggplotly(p)
# 
# domefinal <- domedat %>% 
#   rename(Parameter = "name") %>% 
#   select(date,value,Parameter) %>% 
#   mutate(Site = "Dome")
# 
# write_csv(domefinal,"Climate-Comparisons/DomeMtn.csv" )
