load("data/measuredData.RData")
file.copy("assets/calibration_all_Ndep.xdb.xml","calibration/calibration_all.xdb.xml",overwrite = T)

roots_season <- roots %>% 
  ungroup() %>%
  transmute(ExpID, Year = year(date), DOY = yday(date), obs = rootbm, n_treatment = "N50") %>% 
  filter(Year != 2012,
         !is.na(obs))

season_lai2 <- filter(season_lai, DOY < 200) %>% 
  mutate(n_treatment = "N50")

biomass_n <- biomass %>% 
  transmute(n_treatment, obs = nuptake_mean) %>% 
  filter(!is.na(obs))

yield_obs_calibration <- biomass %>% 
  transmute(n_treatment, date = date_mean, obs = yield_mean) %>%
  bind_rows(yield %>% transmute(Year = year, n_treatment = "N50", date, obs = CWAD/1000)) %>%
  bind_rows(MLEyields %>% 
              separate(ExpID, c("ExpID","Rep")) %>%
              transmute(ExpID, Year = year(date), date = as.Date(date), n_treatment = n_trt, obs = yield)) %>%
  group_by(ExpID,n_treatment,Year) %>%
  summarise(obs = mean(obs),
            date = min(date)) %>%
  group_by(ExpID) %>%
  #mutate(age = Year - min(Year) + 1) %>%
  filter(#age > 0,
         !is.na(obs))

n_rate_trial2 <- filter(n_rate_trial, treatment_n_rate %in% c(0,50,150)) 
  
###
salus_optim_run <- function(parms_grid){
  
  cat(parms_grid,"\n")
  
  parms <- readRDS("data/calibratedParms.rds")
  
  for(parm in names(parms_grid)) parms$SU[[parm]] <- parms_grid[[parm]]
  
  salus <- salus_simpleParms(parms = parms,
                             vars1 =  "CWAD,RWAD,LAI,N_Harvst",
                             xdb = "calibration_all.xdb.xml",
                             overwrite_cdb = FALSE,
                             options = " -wn -npoolseparate",# -NFacPow4",
                             outfile = tempfile()) %>%
    separate(ExpID, c("ExpID","n_treatment")) %>%
    mutate(date = as.Date(paste(Year,DOY),format = "%Y %j"),
           treatment_n_rate = as.numeric(gsub("NR","",n_treatment)),
           year = Year)
  
  # Yield from Intensive and MLE sites
  salus %>%
    group_by(ExpID,Year) %>%
    summarise(pred = max(CWAD,na.rm = T)/1000) %>%
    left_join(yield_obs_calibration, by = c("ExpID", "Year")) %>%
    ungroup() %>%
    filter(complete.cases(.)) %>%
    summarise(NSE = 0.2*NSE(obs,pred)) %>%
    # Roots from KBS
    bind_rows(salus %>%
                transmute(ExpID,Year,DOY,pred = RWAD/1000) %>%
                left_join(roots_season, by = c("ExpID", "Year", "DOY")) %>%
                ungroup() %>%
                filter(complete.cases(.)) %>%
                summarise(NSE = 0.2*NSE(obs,pred))) %>%
  #   # LAI from KBS
    bind_rows(salus %>%
                group_by(ExpID) %>%
                transmute(Year,DOY,pred = LAI) %>%
                left_join(season_lai2, by = c("ExpID", "Year", "DOY")) %>%
                ungroup() %>%
                filter(complete.cases(.)) %>%
                summarise(NSE = 0.2*NSE(obs,pred))) %>%
  # # Uptake from Intensive and MLE sites
    # bind_rows(salus %>%
    #             group_by(n_treatment,ExpID,Year) %>%
    #             mutate(shoot = cumsum(c(0,diff(N_Harvst)))) %>%
    #             group_by(n_treatment,ExpID,Year) %>%
    #             summarise(pred = max(shoot,na.rm = T)) %>%
    #             left_join(biomass_n, by = c("n_treatment", "ExpID", "Year")) %>%
    #             ungroup() %>%
    #             filter(complete.cases(.)) %>%
    #             summarise(NSE = 0.2*NSE(obs,pred))) %>%
  # # Yield from N rate study
    bind_rows(salus %>%
                mutate(treatment_n_rate = as.numeric(gsub("NR","",n_treatment)),
                       year = Year) %>%
                group_by(ExpID,treatment_n_rate, year) %>%
                summarise(pred = max(CWAD/1000, na.rm = T)) %>%
                left_join(n_rate_trial2 %>% select(treatment_n_rate, year,yield),
                          by = c("treatment_n_rate", "year")) %>%
                ungroup() %>%
                filter(complete.cases(.)) %>%
                summarise(NSE = 0.2*NSE(yield,pred))) %>%
  #   # Biomass concentrations from N rate
    bind_rows(salus %>%
                mutate(ExpID,treatment_n_rate = as.numeric(gsub("NR","",n_treatment)),
                       year = Year) %>%
                group_by(treatment_n_rate, year) %>%
                summarise(pred =  max(cumsum(c(0,diff(N_Harvst))))/max(CWAD/1000, na.rm = T)/10) %>%
                left_join(n_rate_trial2 %>% select(treatment_n_rate, year,percent_nitrogen),
                          by = c("treatment_n_rate", "year")) %>%
                ungroup() %>%
                filter(complete.cases(.)) %>%
                summarise(NSE = 0.2*NSE(percent_nitrogen,pred))) %>%
    summarise(objFun = 1 - sum(NSE)) %>%
    unname() %>%
    unlist() %>%
    suppressMessages()
  
}

#salus_optim_run(c(RUEmax = 3.18,
#                  RootPartFac = 2.4,
#                  RootSenFac  = 0.25, 
#                  PlntN_Em=0.018,
#                  PlntN_Hf=0.001,
#                  PlntN_Mt=0.008))