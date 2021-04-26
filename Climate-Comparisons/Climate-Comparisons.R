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


# snow pillows ------------------------------------------------------------

domesw <- read_csv("Climate-Comparisons/other climate data/DataSetExport-SW.Telemetry@1A19P-20210412182208.csv",skip = 2) %>% 
  rename(datetime = `Timestamp (UTC)`, swe.mm = `Value (Millimetres)`) %>% 
  dplyr::select(datetime,swe.mm) %>% 
    mutate(year = year(datetime),
         yday = yday(datetime),
         date = date(datetime),
         month = month(datetime)) %>% 
  filter(swe.mm>=0 & datetime >= ymd_hms("2006-10-20 07:00:00")) %>% 
  group_by(yday) %>% 
  mutate(mean.swe.plt = mean(swe.mm)) %>% 
  ungroup()



# p <- domesw %>%
#   ggplot()+
#   geom_line(aes(datetime,swe.mm))+
#   geom_line(aes(datetime,mean.swe.plt), colour = "red")
# ggplotly(p)


domestudy <- domesw %>% 
  filter(date >= ymd("2018-10-01") & date <= ymd("2020-09-30")) %>% 
  mutate(wateryear = case_when(
    date(datetime) >= ymd("2018-10-01") & date(datetime) <= ymd("2019-09-30") ~ "2018/2019",
    date(datetime) >= ymd("2019-10-01")  ~ "2019/2020")) %>% 
  mutate(wyday = case_when(
    yday >= 0 & yday < 274 ~ yday + 91,
    yday >=274 ~ yday-274))



p <- domestudy %>% 
  ggplot()+
  geom_line(aes(wyday, swe.mm, colour = wateryear), size =1)+
  geom_line(aes(wyday, mean.swe.plt), colour = "darkgrey", linetype = "dashed", size = 1)+
  scale_x_continuous(breaks = c(0,61,123,182,243,304),
                     labels = c("Oct","Dec","Feb","Apr","Jun","Aug"))+
  scale_colour_discrete(name = "Water Year")+
  labs(x = "Month", y = "Snow Water Equivalent (mm)")+
  theme(legend.position = c(.9,.85))

ggplotly(p)

# dome air temp

dometa <- read_csv("Climate-Comparisons/other climate data/DataSetExport-TA.Working@1A19P-20210414221824.csv", skip = 2) %>% 
  rename(datetime = `Timestamp (UTC)`, temp = `Value (Celsius)`) %>%
  select(datetime, temp) %>% 
  mutate(date = date(datetime),
         year = as.factor(year(date)),
         week = week(date),
         month = month(date)) %>% 
  group_by(month, year) %>% 
  mutate(value = mean(temp, na.rm =T),
         name = "temp",
         yday = yday(date)) %>% 
  ungroup() %>% 
  group_by(month) %>% 
  mutate(mn.val = mean(value,na.rm = T))
  

dometa %>% 
  filter(year == "2019" | year == "2020") %>% 
  ggplot()+
    geom_line(aes(x = month, y = value, colour = year))+
    geom_line(aes(x = month, y =mn.val), linetype = "dashed", size = 1)+
  geom_abline(slope = 0,intercept = 0, linetype = "dashed")+
  labs(x = "Month", y = "Air Temperature (°C)")+
  theme(legend.position = c(.9,.9))

# Precip Comparison

domepc <- read_csv("Climate-Comparisons/other climate data/DataSetExport-PC.Working@1A19P-20210414154037.csv", skip = 2) %>% 
  rename(datetime = `Timestamp (UTC)`, rain = `Value (Millimetres)`) %>%
  select(datetime,rain) %>% 
  mutate(date = date(datetime)) %>% 
  group_by(date) %>%
  mutate(rain = mean(rain)) %>% 
  ungroup() %>% 
  mutate(value = rain - lag(rain, 1),
         name = "rain") %>% 
  filter(date >= "2019-05-10") %>% 
  filter(date <="2019-09-29" | date >= "2020-05-12") %>% 
  filter(date <="2020-10-15")
  
  


domepc$value[domepc$value < 0] = 0

p <- domepc %>% 
  ggplot()+
  geom_line(aes(date, value))

ggplotly(p)

domeswe <- domesw %>% 
  select(-mean.swe.plt) %>% 
  rename(value = "swe.mm") %>% 
  mutate(name = "swe")

domedat <- bind_rows(domeswe,domepc,dometa) %>% 
filter(date >= "2019-01-01")


p <- domedat %>% 
  ggplot(aes(x = date, y = value))+
  geom_col(data = filter(.data = domedat, name == "rain"))+
  geom_line(data = filter(.data = domedat, name == "swe"))+
  geom_line(data = filter(.data = domedat, name == "temp"))+
  facet_grid(rows = vars(name), scales = "free")
ggplotly(p)

domefinal <- domedat %>% 
  rename(Parameter = "name") %>% 
  select(date,value,Parameter) %>% 
  mutate(Site = "Dome")

write_csv(domefinal,"Climate-Comparisons/DomeMtn.csv" )
