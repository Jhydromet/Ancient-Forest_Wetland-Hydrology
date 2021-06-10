# Libraries ---------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(plotly)
library(sf)
library(sp)
library(mapview)

# Read in the data -------------------------------------------------------------------------

files <- dir(path = "Level-Logger-Data-Compiling/AFW_RAW_CSV/surface-dat", pattern = "*.CSV")

# files <- "SLIM_001.CSV" 



TidyDat <- function(files) {
  
  site <- separate(tibble(files), col = files, sep = "_", into = c("site", NA))
  
  datsite <-  read_csv(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/surface-dat/", files), n_max = 1, col_names = c("lab", "site"))
  
  datsite2 <-  read_delim(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/surface-dat/", files), delim =":", n_max = 1, col_names = c("lab", "site"), trim_ws = T)
  
  log.sn <-  read_csv(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/surface-dat/", files), skip = 3, n_max = 1, col_names = c("lab", "log.sn"))
  
  log.sn2 <-  read_delim(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/surface-dat/", files), delim =":", skip = 3, n_max = 1, col_names = c("lab", "log.sn"), trim_ws = T)
  
  dat <- read_csv(paste0("Level-Logger-Data-Compiling/AFW_RAW_CSV/surface-dat/", files), skip = 12, col_names = c("record","date","time","raw","value")) %>% 
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
           raw = as.numeric(raw)) %>% 
    dplyr::select(file,log.sn,site,datsite,datetime,date, raw) %>% 
    filter(between(datetime,ymd_hms("2019-03-25 00:00:00"), ymd_hms("2020-12-31 00:00:00"))) %>% 
    mutate(date = date(datetime))
}

rawdat <- files %>% 
  map(TidyDat) %>% 
  reduce(rbind)

rawdat <- rawdat %>% 
  group_by(site) %>% 
  distinct(datetime, .keep_all = T)%>%
  mutate(log.sn = as.double(log.sn))

rawdat.cor <- rawdat %>% 
  mutate(rsite = case_when(
    datsite == "AFS-B6" ~ "AFS-B2",
    datsite == "AFS-B7" ~ "AFS-B3",
    datsite == "AFS-B8" ~ "AFS-B4",
    datsite == "AFS-C10" ~ "AFS-C2",
    datsite == "AFS-A" ~ substr(site,1,6),
    datsite == "AFS-C9" ~ "AFS-C1",
    TRUE ~ datsite
  ))

rawdat.cor <- rawdat.cor %>% 
  mutate(rsite = case_when(
    rsite == "AFS-A"  ~ "AFS-A3",
    TRUE  ~ rsite
    ))

p <- rawdat.cor %>% 
  ggplot()+
  geom_line(aes(datetime,raw,colour = rsite))

ggplotly(p)