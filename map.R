source("init.R")
library(maps)
library(mapdata)

# Read simulation output
rawdata <- list.files("regionalSim","raw")
term <- data.frame()

for(i in 1:length(rawdata)){
  
  cat("\r",round(i/length(rawdata),2)*100,"%")
  
  term <- readRDS(file.path("regionalSim",rawdata[i])) %>%
    as_tibble() %>%
    filter(CWAD >= 5000) %>%
    group_by(ExpID) %>%
    summarise(DOY = min(DOY)) %>%
    separate(ExpID,c("lat","long","year","dop"),sep = "_") %>%
    group_by(lat,long,dop) %>%
    summarise(DOY = list(quantile(DOY,probs = c(0.2,0.5,0.8)))) %>%
    unnest() %>%
    mutate(prob = c("20%","50%","80%")) %>%
    ungroup() %>%
    mutate(state = substr(rawdata[i],1,2),
           lat = as.numeric(gsub("N","",lat)),
           long = -as.numeric(gsub("W","",long))) %>%
    bind_rows(term)
  
}

# Domain for simulations
simdomain <-
  data.frame(state = c("IN","OH","MI",
                      "ND","NE","KS",
                      "IA","MN","WI",
                      "SD","IL","MO")) %>%
  group_by(state) %>%
  do(data = read.csv(paste0("assets/Raster_Counts/",.$state,"_combined_corn_county.csv"))) %>%
  unnest() %>%
  separate(key, c("SoilID","lat","long","fips"), sep = "_") %>%
  mutate(Weatherfp = paste0(lat,"_",long)) %>%
  filter(SoilID %in% readRDS("data/SoilIDs.rds")) %>%
  group_by(lat,long,fips) %>%
  summarise(area = sum(Count)*0.09) %>%
  ungroup() %>%
  mutate(lat = as.numeric(gsub("N","",lat)),
         long = -as.numeric(gsub("W","",gsub(".wdb.xml","",long))),
         fips = as.numeric(fips)) %>%
  left_join(county.fips %>%
              separate(polyname,c("region","subregion"), sep = ","))

# Summarize by county
term_county <- term %>%
  left_join(simdomain) %>%
  group_by(region, subregion,prob,dop,fips) %>%
  summarise(area = sum(area),
            DOY = mean(DOY)
            )%>%
  filter(area > 30000) %>%
  ungroup() %>%
  mutate(dop = factor(dop, labels = c("Sep-15","Oct-7","Nov-1")))

# Save table with state averages
term_county %>%
  mutate(dop = factor(dop, labels = c("Sep15","Oct7","Nov1"))) %>%
  group_by(region,dop,prob) %>%
  summarise(DOY = as.character(as.Date(paste(2019,mean(DOY)),"%Y %j"),"%b-%d")) %>%
  ungroup() %>%
  transmute(region= toupper(region),
            dop = paste0(dop,"_",prob),
            DOY) %>%
  spread(dop,DOY) %>%
  write.csv("figures/mapTable.csv", row.names = F)

state_poly <- map_data("state") %>% 
  filter(region %in% c("north dakota","south dakota","nebraska","kansas",
                       "minnesota","iowa","wisconsin","missouri","illinois","indiana","michigan","ohio"))

# Plot all the data
map_data("county")%>%
  left_join(term_county) %>%
  filter(!is.na(DOY)) %>%
  ggplot(aes(long,lat, fill=DOY)) +
  geom_polygon(aes(x=long, y = lat, group = group, fill = pmin(180,pmax(DOY,100))),colour = "gray10",lwd = 0.3) + 
  geom_polygon(data = state_poly, 
              aes(x=long, y = lat, group = group), 
              fill = alpha("white",0.1),
              colour = "black", lwd = 0.5) + 
  facet_grid(prob~dop) + 
  scale_fill_viridis_c(direction = -1, breaks = c(100,120,140,160,180), labels = c("<Apr-10","May-1","May-20","Jun-10",">Jun-30")) + 
  coord_equal(ratio = 1.3,
              expand = T) + 
  labs(y = "", x = "", fill = "Earliest termination date with rye biomass > 5.0 Mg/ha") +
  ggthemes::theme_few() + 
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               barheight = 0.8))   

ggsave("figures/map.jpg",width = 8, height = 8)

# Plot only one map
map_data("county")%>%
  left_join(term_county) %>%
  filter(!is.na(DOY), dop == "Oct-7", prob == "80%") %>%
  ggplot(aes(long,lat, fill=DOY)) +
  geom_polygon(aes(x=long, y = lat, group = group, fill = pmin(180,pmax(DOY,100))),colour = "gray10",lwd = 0.3) + 
  geom_polygon(data = state_poly, 
               aes(x=long, y = lat, group = group), 
               fill = alpha("white",0.1),
               colour = "black", lwd = 0.5) + 
  #facet_grid(prob~dop) + 
  scale_fill_viridis_c(direction = -1, breaks = c(100,120,140,160,180), labels = c("<Apr-10","May-1","May-20","Jun-10",">Jun-30")) + 
  coord_equal(ratio = 1.3,
              expand = T) + 
  labs(y = "Latitude", x = "Longitude", fill = "Earliest termination date with rye biomass > 5.0 Mg/ha") +
  ggthemes::theme_few() + 
  theme(legend.position = "top",
        panel.background = element_rect(fill = "white")) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 20,
                               barheight = 0.8))   

ggsave("figures/map_Oct7P80.jpg",width = 5, height = 5)
