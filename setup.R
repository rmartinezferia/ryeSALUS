library(maps)
library(mapdata)

wdbs <- list.files("D:\\NLDAS\\Weather") %>%
  as_tibble() %>%
  mutate(filename = value,
         value = gsub(".wdb.xml","",value)) %>%
  separate(value, c("lat","long"),sep = "_") %>%
  mutate(lat = as.numeric(gsub("N","",lat)),
         long = -as.numeric(gsub("W","",long)))

state <- map_data("state") %>% filter(region %in% c("north dakota","south dakota","nebraska","kansas","minnesota","iowa","missouri","wisconsin","illinois","indiana","michigan","ohio"))

experiments <- readxl::read_xlsx("assets/rye_experiments.xlsx",1) %>%
  filter(site != "BelleMina")

site <- experiments %>%
  group_by(state,site,lat,long) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(wdb = NA)

for(i in 1:length(site$wdb)){
  site[i,]$wdb <- wdbs %>%
    mutate(a = lat - site[i,]$lat,
           b = long - site[i,]$long,
           c = sqrt(a^2 + b^2)) %>%
    filter(c == min(c)) %>%
    select(filename) %>%
    unlist()
}

file.copy(paste0("D:\\NLDAS\\Weather\\",site$wdb),
          paste0("sim/",site$wdb))

wdbs %>%
  ggplot(aes(long,lat)) +
  geom_tile(fill = "gray90") +
  geom_polygon(data = state, 
               aes(x=long, y = lat, group = group), 
               fill = alpha("white",0.1),
               colour = "black") + 
  geom_point(data = site,
             aes(size = n, fill = n),
             shape = 21) + 
  scale_fill_viridis_c() +
  ggthemes::theme_few() +
  coord_fixed(1.2) + 
  labs(y="",x="", fill = "Site-years:", size = "Site-years")

ggsave("figures/calibration_sites.jpeg",width = 6, height = 4, dpi = 300)

site_soils <-readxl::read_xlsx("assets/rye_experiments.xlsx",2) %>%
  filter(site != "Belle Mina",
         aoi > 10) %>%
  left_join(site)

sdbs <- list.files("D:\\SSURGO\\SSURGO_SALUS",pattern = ".xml")

soils <- c()

for(i in 1:length(sdbs)){
  
  print(sdbs[i])
  soils <- xmlParse(paste0("D:\\SSURGO\\SSURGO_SALUS\\",sdbs[i])) %>%
    xpathApply("//Soil",function(x) xmlAttrs(x)[[1]]) %>%
    unlist() %>%
    c(soils)
}

saveRDS(soils,"data/SoilIDs.rds")

sims <- site_soils %>%
  mutate(StationID = gsub(".wdb.xml","",wdb),
         sdb = paste0(state,".sdb.xml"),
         SoilID = paste0(state,mukey),
         sdb = ifelse(SoilID %in% soils, sdb, "extras.sdb.xml")) %>%
  select(site,wdb,StationID,sdb,SoilID,musym,aoi) %>%
  unique() %>%
  right_join(experiments) %>%
  group_by(site) %>%
  mutate(aoi = aoi/sum(aoi),
         ExpID = paste0(ExpID,"_",musym),
         Title = paste0(pub_reference,"_",ExpID))

file.copy(paste0("D:\\SSURGO\\SSURGO_SALUS\\",unique(sims$sdb)),
          paste0("sim/",unique(sims$sdb)))

saveRDS(sims,"data/calibration_sims.rds")

sims <- sims %>%
  transmute(ExpID,
            Title,
            RelPath= "Y",
            SDOY= 1,#yday(as.Date(paste(plant_y,plant_mo,plant_mday,sep = "-"))) - 10 #1,
            SYear= plant_y,
            NYrs = 2,
            SoilID = SoilID,
            Soilfp = sdb,
            StationID = StationID,
            Weatherfp= wdb,
            Cropfp="crops.cdb.xml",
            DOY = yday(as.Date(paste(plant_y,plant_mo,plant_mday,sep = "-"))),
            Year = SYear+1,
            TDOY = yday(as.Date(paste(term_y,term_mo,term_mday,sep = "-")))) %>%
  data.frame() %>%
  split(.$ExpID)

XDB.node <- newXMLNode("XDB")

for(i in names(sims)){
  
  print(i)

  experiment <- sims[[i]]
  
  Experiment.node <- newXMLNode("Experiment", attrs =  unlist(experiment[,2:12]))
  Rotation_Components.node <- newXMLNode("Rotation_Components")
  
  Experiment.node %>%
    addChildren(newXMLNode("Mgt_InitialCond",attrs =  c(DOY = "1",Year = experiment[,"SYear"])) %>%
                  addChildren(data.frame(Layer = 1:4,
                                         DLayrI = c("30","120","200","230"),
                                         INinorg = c("3","2","1","0"),
                                         SWInit = c("0.2","0.2","0.2","0.2"),stringsAsFactors = F) %>%  
                                split(.$Layer) %>% lapply(function(x){newXMLNode(name = "Layer", attrs = x[-1])})))
  
  
  Component.node.maize <- newXMLNode("Component", attrs = c(OrderNum="1",RcID = paste0(experiment$ExpID,"_corn"),
                                                            Title=paste0(experiment$ExpID,"_corn"),IPltI="r",IIrrI="r",IferI="r",IResI="r",ITilI="r",IMS_HarI="R",IHarI="r",IEnvI="n"))

  Component.node.maize %>%  addChildren(newXMLNode("Mgt_Planting",
                                                   attrs =  c(DOY = "125",Year = experiment$SYear, Ppop = "9", CropMod="S", CultivarID= "", RowSpc="76", SDepth="2", SpeciesID= "MZ")))

  Component.node.maize %>%  addChildren(newXMLNode("Mgt_Fertilizer_App",
                                                   attrs =  c(DOY = "125",Year = experiment$SYear, DOY="125", ANFer= "200", AKFer="0.0", APFer="0.0", DFert = "5", IFType= "FE010")))

  Component.node.maize %>%  addChildren(newXMLNode("Mgt_Harvest_App",
                                                   attrs =  c(DOY = experiment$DOY - 10,Year = experiment$SYear,DAP="", HStg="", HCom="H", HSiz="A",
                                                              HBPc = "0",HBmin="0", HPc="99", HKnDnPc="50")))
  Rotation_Components.node %>% addChildren(Component.node.maize)
  
  Component.node.rye <- newXMLNode("Component", attrs = c(OrderNum="2",RcID=paste0(experiment$ExpID,"_rye"),
                                                          Title=paste0(experiment$ExpID,"_rye"),IPltI="r",IIrrI="r",IferI="r",IResI="r",ITilI="r",IMS_HarI="R",IHarI="r",IEnvI="n"))

  Component.node.rye %>%  addChildren(newXMLNode("Mgt_Planting",
                                                 attrs =  unlist(c(Year = experiment$SYear, DOY = experiment$DOY, Ppop = "300", CropMod="S", CultivarID= "", RowSpc="20", SDepth="1", SpeciesID= "RY"))))

  Component.node.rye %>%  addChildren(newXMLNode("Mgt_Harvest_App",
                                                 attrs =  unlist(c(Year = experiment$Year, DOY = experiment$TDOY, DAP="", HStg="", HCom="H", HSiz="A",HBPc = "99",HBmin="0", HPc="99", HKnDnPc="0"))))

  Rotation_Components.node %>% addChildren(Component.node.rye)

  XDB.node %>% addChildren(Experiment.node %>% addChildren(Rotation_Components.node))
  
  }

XDB.node %>% saveXML("sim/rye_experiments.xdb.xml")
