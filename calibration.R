source("init.R")
#library(rvest)
#source("setup/measuredData2.R")
#source("setup/experiments.R")
#source("setup/soils.R")
source("library/salus_simpleParms.R")
#obs <- readRDS("data/measuredData.RDS")
#library(ggpubr)

theme_set(ggthemes::theme_few() + theme(panel.background = element_rect(fill = "white"),
                                        legend.background = element_blank(),
                                        legend.key = element_blank()))

myVars <- "CWAD"#,LAI,RWAD,DrghtFac,ColdFac,NitroFac,RelatvTT,layer:9:SW,layer:9:ST,layer:9:NIADL,ETAD,EOAD,N_Plants,N_RapFOMBl,NIAD,NOAD,C_ActOrgBl,N_Harvst,C_Net,C_In,C_Out,C_CO2"

obs <- readRDS("data/calibration_sims.rds")

calib_parms <- read.csv("assets/crop_parm_table.csv", stringsAsFactors = F) %>% 
  #filter(SpeciesID == species) %>%
  select(SpeciesID,Parm,Value) %>%
  spread(Parm,Value) %>%
  split(.$SpeciesID)# %>%,
#list(RY = readRDS("data/optim_rye.rds")$par)
  
salus <- 
  salus_simpleParms(parms = calib_parms,
                    vars1 =  myVars,
                    cdb= "crops.cdb.xml",
                    xdb = "rye_experiments.xdb.xml",
                    wrkdir = "sim",
                    options = " -wn",#-NFacPow4",
                    outfile = "data/rye.Rdata")%>%
  mutate(date = as.Date(paste(Year,DOY),format = "%Y %j"),
         biomass = as.numeric(as.character(CWAD))) %>%
  filter(RcID == 1, RID == 1) 

salus %>%
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

fit <- salus %>%
  group_by(ExpID) %>%
  summarise(sim = max(biomass, na.rm = T)) %>%
  left_join(obs)  %>%
  #filter(site == "Kelley") %>%
  group_by(pub_reference,dataType,site,trt_code,term_y) %>%
  summarise(obs = mean(bio_kgha)/1000,
            pred = mean(sim)/1000) %>% 
  group_by(dataType)

fit %>%
  ggplot(aes(pred,obs)) +
  geom_abline() +
  #geom_abline(intercept = 1.1) + 
  #geom_abline(intercept = -1.1) + 
  geom_point(aes(fill = pub_reference), shape = 21, size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = F, colour = "red") + 
  geom_text(data = fit  %>% fitSummary(units = "Mg/ha"), aes(x=0.3,y=7,label = label),
            hjust = 0,size = 3) + 
  coord_equal(xlim = c(0,8), ylim = c(0,8), expand = F) +
  facet_wrap(~dataType) +
  #annotate("text",x = 0.3, y = 7, label = fit  %>% fitSummary(units = "Mg/ha"),
  #         hjust = 0, size = 3) + 
  labs(x = expression("Simulated rye biomass (Mg"~ha^-1~")"),
       y = expression("Observed rye biomass (Mg"~ha^-1~")"),
       fill = "Training:                             Testing:                         ") +
  theme(legend.position = "top",
        legend.key.height = unit(2,"mm"),
        strip.text = element_blank()) + 
  guides(fill = guide_legend(ncol = 2,
                             title.position = "top",
                             title.hjust = 0.5))

ggsave("figures/calibration_fit.jpeg",width = 5, height = 5, dpi = 600)


salus %>%
  separate(ExpID, c("site","year","trt","soil")) %>%
  left_join(obs %>%
              mutate(date = as.Date(paste(term_y,term_mo,term_mday,sep = "-"))) %>%
              select(site, date, bio_kgha)) %>% 
  filter(site %in% c("Waseca","Rosemont"),
         yday(date) > 60,
         yday(date) < 200) %>%
  ggplot(aes(yday(date),biomass)) +
  geom_line(lwd = 1 , aes(colour = "Simulated")) + 
  geom_point(aes(y = bio_kgha, fill = "Measured"), size = 3, shape = 21) +
  facet_wrap(site~year, ncol = 4) + 
  scale_fill_manual(values = "red") + 
  scale_colour_manual(values = "blue") + 
  labs(y = expression("Rye biomass (Mg"~ha^-1*")"), x = "Day of year",
       fill = "", colour = "") + 
  theme(legend.position = "top")

ggsave("figures/calibration_example.jpeg",width = 8, height = 3, dpi = 300)
