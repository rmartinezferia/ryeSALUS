###
salus_optim_run <- function(parms_grid, species = "RY"){
  
  source("library/salus_simpleParms.R")
  obs <- readRDS("data/calibration_sims.rds")
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(XML)
  
  #cat(parms_grid," ")
  
  parms <- read.csv("assets/crop_parm_table.csv", stringsAsFactors = F) %>% 
    #filter(SpeciesID == species) %>%
    select(SpeciesID,Parm,Value) %>%
    spread(Parm,Value) %>%
    split(.$SpeciesID)# %>%
    #map(unlist)
  
  for(parm in names(parms_grid)) {

    parms[[1]][[parm]] <- parms_grid[[parm]]

  }
  # 
    salus_simpleParms(parms = parms,
                      vars1 =  "CWAD",
                      cdb= "crops.cdb.xml",
                      xdb = "rye_experiments.xdb.xml",
                      wrkdir = "sim",
                      options = " -wn",#-NFacPow4",
                      outfile = "data/rye.Rdata")%>%
    mutate(biomass = as.numeric(as.character(CWAD))) %>%
    filter(RcID == 2, RID == 1) %>%
    group_by(ExpID) %>%
    summarise(sim = max(biomass, na.rm = T)) %>%
    left_join(obs)  %>%
    group_by(site,trt_code,term_y) %>%
    summarise(obs = mean(bio_kgha),
              sim = mean(sim)) %>%
    ungroup() %>%
    summarise(NSE = NSE(obs,sim)) %>%
    unlist() %>% 
    unname()
}

#salus_optim_run(c(LAImax = 3))
