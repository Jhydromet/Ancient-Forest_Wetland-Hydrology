library(tidyverse)
library(lubridate)
library(plotly)
library(sf)
library(sp)

# Read in the data


files <- dir(path = "Level-Logger-Data-Compiling/AFW_RAW_CSV", pattern = "*.CSV")

# files <- "AFGW-WCRK_002.CSV"


TidyDat <- function(files) {
  
  site <- separate(tibble(files), col = files, sep = "_", into = c("site", NA))
  
  datsite <-  read_csv(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/", files), n_max = 1, col_names = c("lab", "site"))
  
  datsite2 <-  read_delim(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/", files), delim =":", n_max = 1, col_names = c("lab", "site"), trim_ws = T)
  
  log.sn <-  read_csv(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/", files), skip = 3, n_max = 1, col_names = c("lab", "log.sn"))
  
  log.sn2 <-  read_delim(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/", files), delim =":", skip = 3, n_max = 1, col_names = c("lab", "log.sn"), trim_ws = T)
  
  dat <- read_csv(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/", files), skip = 12, col_names = c("record","date","time","raw","value")) %>% 
    mutate(file = files,
           site = site$site,
           datsite = datsite$site,
           datsite = replace_na(data = datsite, datsite2$site),
           datsite = str_trim(datsite),
           log.sn = log.sn$log.sn,
           log.sn = replace_na(data = log.sn, log.sn2$log.sn),
           log.sn = str_trim(log.sn),
           time = replace_na(time, "00:00:00"),
           datetime = paste(date,time),
           datetime = dmy_hms(datetime),
           date = dmy(date),
           raw = as.numeric(raw)) %>% 
    select(file,log.sn,site,datsite,datetime,date, raw)
}

rawdat <- files %>% 
  map(TidyDat) %>% 
  reduce(rbind)

rawdat <- rawdat %>% 
  group_by(site) %>% 
  distinct(datetime, .keep_all = T)%>%
  filter(date >= "2019-04-05" & date <= "2020-12-31") %>%
  mutate(log.sn = as.double(log.sn))


# Check that file names match
# chk <- rawdat %>%
#   mutate() %>%
#   filter(site != datsite) %>%
#   distinct(datsite, .keep_all = T)
# #   
# rawdat %>% 
# distinct(log.sn)


## KEEP CHECKING THAT LOGGER SERIAL, SITE NUMBER AND FILE NAME ARE CONSISTENT!!!!!!

# rawdat %>% 
#   filter(site != "AFGW-B2b") %>% 
# ggplot()+
#   geom_line(aes(datetime,site, colour = site), size = 2) + 
#   labs(x = "Date", y = "Site", title = paste0(Sys.Date()," Tidied WL Logger Data"))+
#   scale_colour_discrete(guide = FALSE)


# Add in Calibration Data & Geometry -------------------------------------------------




cal <- read_csv("Level-Logger-Data-Compiling/Calibration Values.csv") %>% 
  select(-type,-site) %>% 
  rename(site = "site1")


geo <- read_sf("Level-Logger-Data-Compiling/piezos.gpkg") %>% 
  select(site)

cal.geo <- left_join(geo,cal, by = "site") %>% 
  select(-site) %>% 
  rename(site = "altsite") %>% 
  filter(site != "NA")


rawdat %>% select(site) %>% distinct()
cal.geo %>% select(site)

wldat <- full_join(rawdat,cal.geo, by = c("site","log.sn")) %>% 
  mutate(site = sub(".*AFGW-","",site)) %>% 
  select(-file, -toctg_cm_2021)

manual <- read_csv("Level-Logger-Data-Compiling/Manual_Water_Measurements.csv") %>% 
  mutate(date = mdy(date)) %>% 
  rename(log.sn = "serial_gw", gtw_cm_man = "gtw_cm") %>% 
  select(-toctg_cm)

# p <- manual %>% 
#   ggplot()+
#   geom_point(aes(date,toctw_cm,colour = site))+
#   geom_abline(slope = 0,intercept = 0)
# ggplotly(p)

wldat <- left_join(wldat,manual, by = c("date", "site", "log.sn"))



# Clean up the data -----------------------------------------------------

# This well drops so much after pumping that it seems like pumping affected water level for at least two weeks after

# And calculate water levels for each site.

A1G <- wldat %>% 
  filter(site == "A1") %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw + 58,
         head = wl+elevation_m) %>% 
  filter(datetime <= ymd_hms("2019-09-10 14:15:00") | datetime >= ymd_hms("2019-09-19 00:00:00")) %>% 
  filter(date <= "2021-02-03")



# ----------------------------

# A2G was flipped "2019-05-25 23:45:00", removed low vals

A2G <- wldat %>% 
  filter(site == "A2") 

preflip <- A2G %>% 
  filter(datetime <= ymd_hms("2019-05-25 23:45:00")) %>% 
  mutate(raw = raw - 244)

A2G <- A2G%>% 
  filter(raw >= 2600) %>% 
  filter(datetime>= "2019-05-27 00:00:00")

A2G <- bind_rows(preflip, A2G) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw +30,
         head = wl+elevation_m)




rm(preflip)

# -----------------------------

# This site did not successfully record data in 2020/2021


A3G <- wldat %>% 
  filter(site == "A3") %>% 
  filter(datetime <= ymd_hms("2019-10-31 12:30:00")) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+21,
         head = wl+elevation_m)


# -----------------------------

# A4G was flipped on 17 March 2020

A4G <- wldat %>% 
  filter(site == "A4") %>% 
  filter(raw >= 2850)

preflip <- A4G %>% 
  filter(datetime <= ymd_hms("2020-03-17 16:00:00"))

A4G <- A4G %>% 
  filter(datetime >= ymd_hms("2020-03-18 01:00:00")) %>% 
  mutate(raw = raw+29)

A4G <- bind_rows(preflip,A4G) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+15,
         head = wl+elevation_m)


# -----------------------------
# -----------------------------
# Battery death over xmas. logger flip and another sharp drop.


B1G <- wldat %>% 
  filter(site == "B1")

preflip <- B1G %>% 
  filter(datetime <= ymd_hms("2020-08-07 12:15:00"))

B1G <- B1G %>% 
  filter(datetime >= ymd_hms("2020-08-07 16:15:00")) %>% 
  mutate(raw = raw + 20)

B1G <- bind_rows(preflip,B1G)

prewinter <- B1G %>% 
  filter(datetime <= ymd_hms("2019-11-13 15:00:00"))

B1G <- B1G %>% 
  filter(datetime >= ymd_hms("2020-03-31 16:00:00")) %>% 
  mutate(raw = raw + 250) # I am really guessing this correction :(

B1G <- bind_rows(prewinter,B1G) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+ 45,
         head = wl+elevation_m)




# -----------------------------

B2G <- wldat %>% 
  filter(site == "B2") %>% 
  filter(raw >= 2850)

prepump <- B2G %>% 
  filter(datetime <= ymd_hms("2019-05-26 13:45:00")) %>% 
  mutate(raw = raw - 86)

B2G <- B2G %>% 
  filter(datetime >= ymd_hms("2019-05-27 02:30:00"))

B2G <- bind_rows(prepump,B2G)

prepump <- B2G %>% 
  filter(datetime <= ymd_hms("2019-07-05 12:30:00")) %>% 
  mutate(raw = raw - 28)

B2G <- B2G %>% 
  filter(datetime >= ymd_hms("2019-07-05 22:00:00"))

B2G <- bind_rows(prepump,B2G)

prepump <- B2G %>% 
  filter(datetime <= ymd_hms("2020-04-07 13:15:00")) %>% 
  mutate(raw = raw - 502)

B2G <- B2G %>% 
  filter(datetime >= ymd_hms("2020-04-08 06:15:00"))

B2G <- bind_rows(prepump,B2G)

prepump <- B2G %>% 
  filter(datetime <= ymd_hms("2019-11-13 14:15:00")) %>% 
  mutate(raw = raw + 500)

B2G <- B2G %>% 
  filter(datetime >= ymd_hms("2020-03-17 14:15:00"))

B2G <- bind_rows(prepump,B2G) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+20,
         head = wl+elevation_m)





# -----------------------------

# sensor is really noisy, no summer 2020 data.

B3G <- wldat %>% 
  filter(site == "B3")  %>% 
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  filter(abs(raw - lag(raw, 1)) <= 5) %>%
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw,
         head = wl+elevation_m)


# -----------------------------

# beautiful dataset. Rising limb following first deployment finished by ~ 01 May 2019

B4G <- wldat %>% 
  filter(site == "B4") %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+30,
         head = wl+elevation_m)



# -----------------------------

# removed spike during field visit, rising limb till 2020-04-25

B5G <- wldat %>% 
  filter(site == "B5") %>% 
  filter(raw >= 2000) 

prepump <- B5G %>% 
  filter(datetime <= ymd_hms("2020-08-18 12:30:00")) %>% 
  mutate(raw = raw-120)

B5G <- B5G %>% 
  filter(datetime >= ymd_hms("2020-08-18 14:15:00"))

B5G <- bind_rows(prepump,B5G) %>% 
  mutate(toc.tw = (raw*m*0.1+b)/10,
         wl = toctg_cm - toc.tw+30,
         head = wl+elevation_m)



# -----------------------------

# data looks good aside from winter 2020 gap. spiky but not crazy.

C1G <- wldat %>% 
  filter(site == "C1") %>% 
  filter(raw >= 2250) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+55,
         head = wl+elevation_m)




# -----------------------------

# pumping data to be removed.Problems with midnight data points?

C2G <- wldat %>% 
  filter(site == "C2") %>% 
  filter(datetime >= ymd_hms("2019-04-28 15:30:00") | datetime <= ymd_hms("2019-04-28 12:45:00")) %>% 
  filter(datetime >= ymd_hms("2019-05-26 12:45:00") | datetime <= ymd_hms("2019-05-26 11:30:00")) %>%
  filter(datetime >= ymd_hms("2019-07-05 13:45:00") | datetime <= ymd_hms("2019-07-05 10:30:00")) %>% 
  filter(datetime >= ymd_hms("2019-09-10 13:30:00") | datetime <= ymd_hms("2019-09-10 10:30:00")) %>% 
  filter(datetime >= ymd_hms("2019-11-13 13:45:00") | datetime <= ymd_hms("2019-11-13 11:45:00")) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw,
         head = wl+elevation_m)


# -----------------------------

# Only 2019 data. Gap in July from spikey data. generally spikey, either geology or welll..

C3G <- wldat %>% 
  filter(site == "C3") %>% 
  filter(raw >= 2250) %>% 
  filter(datetime >= ymd_hms("2019-07-02 20:30:00") | datetime <= ymd_hms("2019-07-02 10:45:00")) %>% 
  filter(datetime >= ymd_hms("2019-07-23 15:00:00") | datetime <= ymd_hms("2019-07-02 10:45:00")) %>% 
  filter(datetime >= ymd_hms("2019-07-30 21:45:00") | datetime <= ymd_hms("2019-07-30 17:00:00")) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+20,
         head = wl+elevation_m)




# -----------------------------


C4G <- wldat %>% 
  filter(site == "C4") %>% 
  filter(datetime >= ymd_hms("2020-04-09 12:00:00") | datetime <= ymd_hms("2020-04-07 11:45:00")) %>% 
  filter(datetime >= ymd_hms("2020-03-18 21:00:00") | datetime <= ymd_hms("2020-03-17 10:15:00")) %>% 
  filter(datetime != ymd_hms("2020-04-07 12:00:00")) %>% 
  filter(datetime != ymd_hms("2020-04-07 00:00:00")) %>% 
  filter(datetime != ymd_hms("2019-04-05 15:30:00")) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+5,
         head = wl+elevation_m)



# -----------------------------
# -----------------------------

# Spikey and short (Just late 2020). Need to thin out more spike values (see filter with lag(raw,1))

ECRKG <- wldat %>% 
  filter(site == "ECRK") %>% 
  filter(raw >= 2510) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw,
         head = wl+elevation_m)



# I never verified this data with a manual measurement.. FUCK.

# -----------------------------
# looks a little funky, fall 2020 has two sharp increases that seem suspicious.. removing early data.

WCRKG <- wldat %>% 
  filter(site == "WCRK") %>% 
  filter(datetime >= ymd_hms("2020-04-01 19:45:00")) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw+100,
         head = wl+elevation_m)

# I never verified this data with a manual measurement.. FUCK.


# -----------------------------

# Pretty good other than July lot of weird dropped spikes.. maybe whitewater causing an issue?

SLIMS <-  wldat %>% 
  filter(site == "SLIM") %>% 
  filter(raw >= 2332) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  filter(abs(raw - lag(raw, 1)) <= 10) %>% 
  mutate(toc.tw = (raw*m+b)/10,
         wl = toctg_cm - toc.tw,
         head = wl+elevation_m)


# I also need to determine if this is calibrated top down or bot up.

p <- SLIMS %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = wl))+
  geom_point(aes(x = datetime, y = gtw_cm_man), colour = "red")
ggplotly(p)

# -----------------------------

# Looks good aside from the depressingly short time period.

WCRKS <- wldat %>% 
  filter(site == "AFS-D-WCRK") %>% 
  mutate(toc.tw = raw*m+b,
         wl = toctg_cm - toc.tw,
         head = wl+elevation_m)

# -----------------------------

# Pretty good, though I'm alittle concerned it could be wet sediments.

ECRKS <- wldat %>% 
  filter(site == "AFS-D-ECRK") %>% 
  mutate(toc.tw = raw*m+b,
         wl = toctg_cm - toc.tw,
         head = wl+elevation_m)

# p <- ECRKS %>%
#   ggplot()+
#   geom_point(aes(x = datetime, y = raw, colour = site))
# ggplotly(p)

# --------------------------------------------------------------------------------------------------------------------
# BIND ALL CORRECTED DATA TOGETHER ----------------------------------------


dat.cor <- bind_rows(A1G, A2G, A3G, A4G, B1G, B2G, B3G, B4G, B5G, C1G, C2G, C3G, C4G, ECRKG, ECRKS, SLIMS, WCRKG, WCRKS)


p <- dat.cor %>% 
  ggplot()+
  geom_line(aes(x = datetime, y = raw, colour = site))
ggplotly(p)

# Next step is to bind the well height dataset and correct for elevation...

