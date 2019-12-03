# Read experiments
library(tidyverse)
library(daymetr)
library(XML)
library(lubridate)

# is.leapyear=function(year){
#   #http://en.wikipedia.org/wiki/Leap_year
#   return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
# }
# 
# experiments <- readxl::read_xlsx("rye_experiments.xlsx",1) %>% 
#   separate(loc_citystate,c("site","state"), ", ") %>%
#   mutate(plant_date = as.character(as.Date(plant_date)),
#          plant_date = as.Date(ifelse(is.na(plant_date),
#                               as.character(as.Date(paste(plant_y,plant_mo,plant_mday,sep = "-"))),
#                                            plant_date)),
#          term_date = as.character(as.Date(term_date)),
#          term_date = as.Date(ifelse(is.na(term_date),
#                                     as.character(as.Date(paste(term_y,term_mo,term_mday,sep = "-"))),
#                                     term_date)),
#          ExpID = paste0(gsub(" ","",gsub("\\.","",site)),
#                         "_",year(term_date)),
#          StationID = paste0(gsub(" ","",gsub("\\.","",site)),
#                             "_",state),
#          SoilID = site) %>%
#   group_by(site,year(term_date)) %>%
#   mutate(ExpID = paste0(ExpID,"_",1:n()))
# 
# sites <- experiments %>%
#   transmute(site,lat,long,StationID) %>%
#   unique()
# 
# 
# # Experimetns
# planting <- experiments %>%
#   transmute(ExpID,
#             Title = paste0(pub_reference,"_",ExpID),
#             RelPath= "Y",
#             SDOY= yday(plant_date) - 30,
#             SYear= year(plant_date),
#             NYrs = 1,
#             SoilID = SoilID,
#             Soilfp = "rye_experiments.sdb.xml",
#             StationID = StationID,
#             Cropfp="crops.cdb.xml",
#             Weatherfp= "rye_experiments.wdb.xml",
#             DOY = yday(plant_date),
#             Ppop = "300", CropMod="S", CultivarID= "", 
#             RowSpc="20", SDepth="1", SpeciesID= "RY") %>%
#   data.frame() %>%
#   split(.$ExpID)
# 
# 
# harvest <- experiments %>%
#   transmute(ExpID,
#             Year= year(term_date),
#             DOY= yday(term_date),
#             DAP="", HStg="", HCom="H", HSiz="A", 
#             HBPc = "99",
#             HBmin="0", HPc="99", HKnDnPc="0") %>%
#   data.frame() %>%
#   split(.$ExpID)
# 
# 
# # get wx 
# # daymet <- sites %>%
# #   group_by(StationID) %>%
# #   do(met = try(download_daymet(site = .$StationID, lat = .$lat, lon = .$long,
# #                                start = 1980,end = 2018, simplify = TRUE)))
# # 
# # saveRDS(daymet,"data/daymet_download.rds")
# 
# daymet <- readRDS("data/daymet_download.rds")
# WDB <- newXMLNode("WDB")
# 
# for(i in 1:length(daymet$StationID)){
#   
#   Stations <- newXMLNode("Stations", 
#                          attrs =  c(Elev = unique(daymet$met[[i]]$altitude),
#                                     Lat = unique(daymet$met[[i]]$latitude),
#                                     Long = unique(daymet$met[[i]]$latitude),
#                                     StationID = unique(daymet$met[[i]]$site)))
#   #print(daymet$met[[i]]$site)
#   
#   Weather <- newXMLNode("Weather", attrs =  c(Columns="Year,DOY,SRAD,Tmax,Tmin,Rain,DewP,Wind,PAR"))
#   
#   xmlValue(Weather) <- 
#     # Fix leap year
#     daymet$met[[i]] %>%
#     filter(is.leapyear(year), yday == 365) %>%
#     mutate(yday == 366) %>%
#     bind_rows(daymet$met[[i]]) %>%
#     arrange(year, yday) %>%
#     # recalculate variables
#     spread(measurement,value) %>%
#     transmute(Year = year,
#               DOY = yday,
#               SRAD = round(srad..W.m.2.*dayl..s./1000000, 2), # to MJ/m2/day,
#               Tmax = tmax..deg.c.,
#               Tmin = tmin..deg.c.,
#               Rain = prcp..mm.day.,
#               DewP = "", Wind = "", PAR = "") %>%
#     # make into a string
#     format_csv(col_names = F)
#   
#   WDB %>% addChildren(Stations %>%  addChildren(Weather))
#   
# }
# 
# WDB %>% saveXML("sim/rye_experiments.wdb.xml")