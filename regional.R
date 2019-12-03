library(foreach)
library(doParallel)
source("library/salus_simpleParms.R")
# Inputs
states <- c("IN","OH","MI",
            "ND","NE","KS",
            "IA","MN","WI",
            "SD","IL","MO")
ds <- c(20) # days ofset from Oct 7 (DOY 280)
years <- 1989:2018

numCores <- detectCores()
registerDoParallel(numCores)

for(state in states){
  
  path <- file.path("regionalSim",state)
  
  for(d in ds){
    
    cat(state,d,"\n")
    
    simdomain <- read.csv(paste0("assets/Raster_Counts/",state,"_combined_corn_county.csv")) %>%
      separate(key, c("SoilID","Weatherfp","Weatherfp1","FIPS"), sep = "_") %>%
      mutate(Weatherfp = paste0(Weatherfp,"_",Weatherfp1)) %>%
      select(-Weatherfp1,SoilID) %>%
      filter(SoilID %in% readRDS("data/SoilIDs.rds")) %>%
      group_by(Weatherfp,SoilID) %>%
      summarise(area = sum(Count)*30) %>%
      arrange(Weatherfp, area) %>%
      mutate(parea = area/sum(area),
             cparea = cumsum(parea)) %>%
      filter(parea == max(parea))
    
    if(!dir.exists(path)) {
      dir.create(path)
      file.copy(paste0("D:\\NLDAS\\Weather\\",simdomain$Weatherfp),
                file.path(path,simdomain$Weatherfp))
      file.copy(paste0("D:\\SSURGO\\SSURGO_SALUS\\",paste0(state,".sdb.xml")),
                file.path(path,paste0(state,".sdb.xml")))
      file.copy("sim/salus.gdb.xml",file.path(path,"salus.gdb.xml"))
      file.copy("sim/crops.cdb.xml",file.path(path,"crops.cdb.xml"))
    } 
    
    sims <- simdomain %>%
      mutate(value = gsub(".wdb.xml","",Weatherfp)) %>%
      separate(value, c("lat","long"),sep = "_",remove = F) %>%
      mutate(StationID = gsub(".wdb.xml","",Weatherfp),
             lat = as.numeric(gsub("N","",lat)),
             long = -as.numeric(gsub("W","",long))) %>%
      transmute(StationID, SoilID, lat, long, ExpID = paste0(gsub("","",value))) %>%
      data.frame() #%>%  split(.$StationID)
    
    for(i in 1:length(sims$ExpID)){

      cat("\r Writing xdb files... ",round(i/length(sims$ExpID),2)*100,"%")

      XDB.node <- newXMLNode("XDB")

      for(y in years){


        Experiment.node <- newXMLNode("Experiment", attrs =  c(ExpID = paste0(sims$ExpID[i],"_",as.character(y),"_",as.character(280+d)),
                                                               Title = "",
                                                               RelPath= "Y",
                                                               SDOY= as.character(280 + d - 10),#yday(as.Date(paste(plant_y,plant_mo,plant_mday,sep = "-"))) - 10 #1,
                                                               SYear= as.character(y-1),
                                                               NYrs = "1",
                                                               SoilID = sims$SoilID[i],
                                                               Soilfp = paste0(state,".sdb.xml"),
                                                               StationID = sims$StationID[i],
                                                               Weatherfp= sims$Weatherfp[i],
                                                               Cropfp="crops.cdb.xml"))

        Rotation_Components.node <- newXMLNode("Rotation_Components")

        Experiment.node %>%
          addChildren(newXMLNode("Mgt_InitialCond",attrs =  c(DOY = as.character(280 + d - 10),Year = as.character(y - 1))) %>%
                        addChildren(data.frame(Layer = 1:4,
                                               DLayrI = c("30","120","200","230"),
                                               INinorg = c("3","2","1","0"),
                                               SWInit = c("0.2","0.2","0.2","0.2"),stringsAsFactors = F) %>%
                                      split(.$Layer) %>% lapply(function(x){newXMLNode(name = "Layer", attrs = x[-1])})))


        Component.node.rye <- newXMLNode("Component", attrs = c(OrderNum="1",RcID="1",
                                                                Title="1",IPltI="r",IIrrI="r",IferI="r",IResI="r",ITilI="r",IMS_HarI="R",IHarI="r",IEnvI="n"))

        Component.node.rye %>%  addChildren(newXMLNode("Mgt_Planting",
                                                       attrs =  unlist(c(Year = as.character(y-1), DOY = as.character(280 + d), Ppop = "300", CropMod="S", CultivarID= "", RowSpc="20", SDepth="1", SpeciesID= "RY"))))

        Rotation_Components.node %>% addChildren(Component.node.rye)

        XDB.node %>% addChildren(Experiment.node %>% addChildren(Rotation_Components.node))

      }

      XDB.node %>% saveXML(paste0(path,"/",sims$ExpID[i],"_",as.character(280+d),".xdb.xml"))

      flush.console()

    }

    xdbs <- list.files(path,paste0(as.character(280 + d),".xdb"))
    
    # test <- salus(vars1 = "CWAD",
    #       file1 = gsub("xdb.xml","csv",xdbs[1]),
    #       cdb= "crops.cdb.xml",
    #       xdb = xdbs[1],
    #       wrkdir = path,
    #       options = " -wn")
    
    cat("\nrunning SALUS...\n")
    Sys.time()
    out <- foreach(i=1:length(xdbs), .combine=rbind) %dopar% {
      library(XML)
      library(dplyr)
      library(tidyr)
      try(salus(vars1 = "CWAD",
                file1 = gsub("xdb.xml","csv",xdbs[i]),
                cdb= "crops.cdb.xml",
                xdb = xdbs[i],
                wrkdir = path,
                verbose = F,
                options = " -wn"))
    }
    Sys.time()
    
    saveRDS(out,paste0("regionalSim/",state,"_",as.character(280 + d),"_raw.rds"))
    
  }
  
}
