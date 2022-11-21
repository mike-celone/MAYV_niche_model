library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(cowplot)
library(readxl)
library(raster)
countries <- c("brazil", "peru", "argentina", "chile", "uruguay", "paraguay",
               "ecuador", "colombia", "panama", "suriname", "guyana", 
               "trinidad and tobago", "mexico", "belize", "costa rica", 
               "el salvador", "guatemala", "honduras", "nicaragua", "haiti",
               "bolivia", "france", "venezuela", "dominican republic", "puerto rico", 
               "jamaica", "cuba")
# World map
country_map <- ne_countries(country = countries, scale = "medium", returnclass = "sf")

# Brazil map
brazil <- ne_states(country = "brazil", returnclass = "sf")

# Bring in the occurrence data
occ_pts <- read_excel("All_Hosts_occ_75km_19Nov22.xlsx") 

# Bring in consensus scores
cons_country <- read.csv("Consensus_score_16NOV22.csv", fileEncoding="UTF-8-BOM")
cons_brazil <- read.csv("Brazil_consensus_score_16NOV22.csv", fileEncoding="UTF-8-BOM")

# Bring in the model mean and SD tif layers
model_mean <- raster("BRT_all_host_layers_5OCT22/mean_allhost_5OCT22.tif")
model_sd <- raster("BRT_all_host_layers_5OCT22/sd_allhost_5OCT22.tif")

# Bring in raster files 
# These have been processed in ArcGIS to ensure matching extent and resolution
setwd("C:/Users/Mike/Dropbox/MAYV_risk_modeling/covs_22Sep")
rast_files <- list.files(pattern ='.asc$', all.files=TRUE) 

# Create a raster stack
predictors <- stack(rast_files)

# Fix up the layer names
names <- c("Evergreen", 
           "EVI", 
           "LST_Day",
           "LST_Night", 
           "Rainfall", 
           "TCB", 
           "TCW", 
           "Urban",
           "Elevation",  
           "Slope")

names(predictors) <- names

#----Fig 1----#
# Covariate maps

# Convert raster to dataframe
list <- list()
for (i in names){
  
  ras_df <- as.data.frame(predictors[[i]], xy = TRUE) %>% 
    na.omit()
  
  list[[i]] <- ras_df 
  
}

# Climatic variables
lst_day <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = LST_Day), data = list$LST_Day)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "RdBu", direction = -1) + 
  theme_void() 

lst_night <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = LST_Night), data = list$LST_Night)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "RdBu", direction = -1) + 
  theme_void() 

rain <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Rainfall), data = list$Rainfall)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "YlGnBu", direction = +1) + 
  theme_void() 

fig1a <- plot_grid(lst_day, lst_night, rain, nrow = 1, labels = c('A', 'B', 'C'))

ggsave("Fig1a.jpg", fig1a, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, 
       width = 35, 
       height = 12,
       units = "cm"
)

# Land cover/vegetation
evi <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = EVI), data = list$EVI)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "Greens", direction = +1) + 
  theme_void() 

evergreen <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Evergreen), data = list$Evergreen)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "PRGn", direction = +1) + 
  theme_void() 

urban <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Urban), data = list$Urban)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", direction = -1) + 
  theme_void() 

lc <- plot_grid(evi, evergreen, urban, nrow = 1, labels = c('D','E','F'))

tcb <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = TCB), data = list$TCB)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "RdGy", direction = -1) + 
  theme_void() 

tcw <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = TCW), data = list$TCW)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "RdYlBu", direction = +1) + 
  theme_void() 

tc <- plot_grid(tcb, tcw, nrow = 1, labels = c('G','H'))

fig1b <- plot_grid(lc, tc, nrow = 2)

ggsave("Fig1b.jpg", fig1b, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, 
       width = 28, 
       height = 17,
       units = "cm"
)
  
# Topography
elevation <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Elevation), data = list$Elevation)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "BrBG", direction = +1) + 
  theme_void() 

slope <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Slope), data = list$Slope)+
  coord_sf(xlim = c(-120, -32), ylim = c(-50, 33), expand = FALSE) +
  scale_fill_distiller(palette = "Oranges", direction = +1) + 
  theme_void() 

fig1c <- plot_grid(elevation, slope, nrow = 1, align = "hv", 
                  labels = c('I','J'))

ggsave("Fig1c.jpg", fig1c, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, 
       width = 35, 
       height = 12,
       units = "cm"
       )

#----Fig 2----#
# Categorize the scores
cons <- cons_country %>%
  mutate(
    Category = case_when(
      Score <= 3 ~ "Very Low",
      Score >3 & Score <=7 ~ "Low",
      Score >7 & Score <= 11 ~ "Moderate",
      Score >11 & Score <=15 ~ "High",
      Score >15 ~ "Very High"))

# Join with the world shapefile
join <- inner_join(country_map, cons, by=c("admin"="Country"))

# Convert the score cat to factor
join$Category <- factor(join$Category, 
                        levels = c("Very High", "High", "Moderate", "Low", "Very Low"))

# Create the fig
fig2a <- ggplot() + 
  geom_sf(aes(fill = Category), data = join) +
  scale_fill_brewer(palette = "Spectral") + 
  geom_sf(fill = 'transparent', data = country_map)+
  coord_sf(xlim = c(-110, -32), ylim = c(-50, 23), expand = FALSE) +
  labs(x = "longitude", y = "latitude") +
  annotation_scale(location = "br", width_hint = 0.2, style = "tick") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         #pad_x = unit(1.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(.1, .4),
    legend.key.size = unit(0.9, 'cm'), 
    legend.title = element_text(size=14), 
    legend.text = element_text(size=10))

#----Fig 2b----#
# Categorize the scores
cons_br <- cons_brazil %>%
  mutate(
    Category = case_when(
      Score <= 3 ~ "Very Low",
      Score >3 & Score <=7 ~ "Low",
      Score >7 & Score <= 11 ~ "Moderate",
      Score >11 & Score <=15 ~ "High",
      Score >15 ~ "Very High"))

# Join with the brazil ADM1 shapefile
join_br <- inner_join(brazil, cons_br, by=c("name"="State"))

# Convert the score cat to factor
join_br$Category <- factor(join_br$Category, 
                           levels = c("Very High", "High", "Moderate", "Low", "Very Low"))

# Create the fig
fig2b <- ggplot() + 
  geom_sf(aes(fill = Category), data = join_br) +
  scale_fill_brewer(palette = "Spectral") + 
  geom_sf(fill = 'transparent', data = country_map)+
  coord_sf(xlim = c(-75, -32), ylim = c(-36, 8), expand = FALSE) +
  labs(x = "longitude", y = "latitude") +
  annotation_scale(location = "br", width_hint = 0.2, style = "tick") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         #pad_x = unit(1.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none",
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(),  
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

fig2_final <- plot_grid(fig2a, fig2b, align = "h", axis = "b", 
                        rel_widths = c(1, 0.7), labels = c('A', 'B'), 
                        label_size = 18)

ggsave("Fig2.jpg", fig2_final, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, 
       width = 30, 
       height = 20,
       units = "cm"
       )

#----Fig 3----#

# Occurrence locations
occ_pts_fig <- ggplot(data = country_map) +
  geom_sf() +
  coord_sf(xlim = c(-110, -32), ylim = c(-33, 25), expand = FALSE) +
  geom_jitter(data = occ_pts, aes(x = X_Coord, y = Y_Coord, fill = Host),
              #alpha = 0.7, 
              pch=21, size = 4) +
  scale_fill_brewer(palette="RdYlBu") +
  labs(x = "longitude", y = "latitude") +
  annotation_scale(location = "br", width_hint = 0.2, style = "tick") +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         #pad_x = unit(1.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  #theme(legend.position = "none") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# MAVY cases by year
# Some studies did not report a precise year so take the average of the range of years
pts_new <- occ_pts %>% 
  mutate(
    Year = case_when(
      Year_MAYV_Start != Year_MAYV_End ~ ((Year_MAYV_Start + Year_MAYV_End)/2),
      TRUE ~ Year_MAYV_Start)) %>%
  mutate(
    Year = round(Year, digits=0)) %>%
  select(Host, Year) %>%
  count(Host, Year)

# Turn host type into a factor
pts_new$Host <- factor(pts_new$Host, 
                       levels = c("Animal", "Arthropod", "Human"))

# Create a stacked bar chart
occ_pts_year <- ggplot(pts_new, aes(fill = Host, y = n, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_fill_brewer(palette="RdYlBu") +
  #xlab("Year") + ylab("MAYV Occurrences") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(colour="black"))

# Plot the map with bar chart as an inset
fig3_final <- occ_pts_fig + annotation_custom(ggplotGrob(occ_pts_year), 
                                              xmin = -108, xmax = -80, 
                                              ymin = -30, ymax = -10)

ggsave("Fig3.jpg", fig3_final, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, width = 33, height = 15, units = "cm")

#---Fig 4---#
# Average model predictions
# Convert raster to dataframe
meanras_df <- as.data.frame(model_mean, xy = TRUE) %>% 
  rename(Suitability = mean_allhost_5OCT22) %>%
  na.omit()

main <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Suitability), data = meanras_df)+
  geom_sf(fill = 'transparent', data = country_map)+
  coord_sf(xlim = c(-120, -32), ylim = c(-40, 33), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", breaks = c(0.1, 0.5, 0.9), labels = c("Low", "Med", "High")) +
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(.3, .4),
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_text(size=14), 
    legend.text = element_text(size=10)) +   
  geom_rect(aes(xmin = -63, ymin = 9, xmax = -60, ymax = 11.5),
            fill = NA, 
            colour = "black",
            size = 0.8)   

fig4a <- ggdraw(main) +
  draw_plot(
    {
      main + 
        coord_sf(
          xlim = c(-63, -60),
          ylim = c(9, 11.5),
          expand = FALSE) +
        theme(legend.position = "none")
    },
    x = 0.6, 
    y = 0.8,
    width = 0.2, 
    height = 0.2)

# Model uncertainty
# Convert raster to dataframe
sdras_df <- as.data.frame(model_sd, xy = TRUE) %>% 
  rename(Uncertainty = sd_allhost_5OCT22) %>%
  na.omit()

fig4b <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Uncertainty), data = sdras_df)+
  geom_sf(fill = 'transparent', data = country_map)+
  coord_sf(xlim = c(-120, -32), ylim = c(-40, 33), expand = FALSE) +
  scale_fill_distiller(palette = "GnBu", direction = +1, breaks = c(0.03, 0.3), labels = c("Low", "High")) +
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(.3, .4),
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_text(size=14), 
    legend.text = element_text(size=10)) 

fig4_final <- plot_grid(fig4a, fig4b)

ggsave("Fig4.jpg", fig4_final, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, width = 33, height = 15, units = "cm")

#---Fig 5---#

# Loop through and extract the data from the model list
effect.plot <- function(var, lis_no){
  
  list <- list()
  
  p <- data_all %>% filter(id==1) %>% select(var)
  
  for(i in 1:length(model_list)) {
    
    list[[i]] <- model_list[[i]]$effects[[lis_no]][2]
    
    colnames(list[[i]]) <- paste("y_", i, sep="")
    
    df <- do.call("cbind", list)
    
    finaldf <- df %>% 
      rowwise() %>% 
      mutate(
        mean = mean(c_across(1:ncol(df))),
        sd = sd(c_across(1:ncol(df))))
  }
  
  x <- model_list[[i]]$effects[[lis_no]][1]
  
  final <- cbind(x, finaldf)
  
  plot <- ggplot(final, aes(x=final[,1], y=mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = mean - sd,
                    ymax = mean + sd), alpha=0.2) + 
    geom_rug(data = p,
             inherit.aes = FALSE, 
             aes(unlist(p[1])), 
             sides = "t", 
             color = "#4C4C4C") +
    ylab("Marginal effect") + 
    xlab(var)
}

evergreen_effect <- effect.plot("Evergreen", lis_no=1)
evi_effect <- effect.plot("EVI", lis_no=2)
lstDay_effect <- effect.plot("LST_Day", lis_no=3)
lstNight_effect <- effect.plot("LST_Night", lis_no=4)
rain_effect <- effect.plot("Rainfall", lis_no=5)
tcb_effect <- effect.plot("TCB", lis_no=6)
tcw_effect <- effect.plot("TCW", lis_no=7)
urban_effect <- effect.plot("Urban", lis_no=8)
elev_effect <- effect.plot("Elevation", lis_no=9)
slope_effect <- effect.plot("Slope", lis_no=10)

# Plot the most important variables
fig5_final <- cowplot::plot_grid(lstNight_effect, rain_effect, evi_effect,
                                 nrow=2)

# Plot all PDPs
cowplot::plot_grid(evi_effect, evergreen_effect, lstDay_effect, lstNight_effect,
                   rain_effect, tcb_effect, elev_effect, slope_effect, tcw_effect,
                   urban_effect, nrow = 2)

#---Additional figs---#
# Supplementary Fig1
all_host <- raster("C:/Users/Mike/Dropbox/MAYV_risk_modeling/BRT_all_host_layers_5OCT22/mean_allhost_5OCT22.tif")
human_only <- raster("C:/Users/Mike/Dropbox/MAYV_risk_modeling/BRT_human_only_layers_17NOV22/mean_human_only_20NOV22.tif")

# Convert raster to dataframe
meanras_df <- as.data.frame(human_only, xy = TRUE) %>% 
  rename(Suitability = mean_human_only_20NOV22) %>%
  na.omit()

main <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Suitability), data = meanras_df)+
  geom_sf(fill = 'transparent', data = country_map)+
  coord_sf(xlim = c(-120, -32), ylim = c(-40, 33), expand = FALSE) +
  scale_fill_distiller(palette = "Spectral", breaks = c(0.1, 0.5, 0.9), labels = c("Low", "Med", "High")) +
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(.3, .4),
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_text(size=14), 
    legend.text = element_text(size=10)) +   
  geom_rect(aes(xmin = -63, ymin = 9, xmax = -60, ymax = 11.5),
            fill = NA, 
            colour = "black",
            size = 0.8)   

human_only_map <- ggdraw(main) +
  draw_plot(
    {
      main + 
        coord_sf(
          xlim = c(-63, -60),
          ylim = c(9, 11.5),
          expand = FALSE) +
        theme(legend.position = "none")
    },
    x = 0.6, 
    y = 0.8,
    width = 0.2, 
    height = 0.2)

# Calulate the difference between the two models
diff <- all_host - human_only

# Convert raster to dataframe
diff_df <- as.data.frame(diff, xy = TRUE) %>% 
  rename(Difference=layer) %>%
  na.omit()

plot_dif <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Difference), data = diff_df) +
  geom_col() +
  scale_fill_gradient2(low = "black",
                       mid = "white",
                       high = "red",
                       midpoint = 0) +
  geom_sf(fill = 'transparent', data = country_map) +
  coord_sf(xlim = c(-120, -32), ylim = c(-40, 33), expand = FALSE) +
  theme_void()

ggsave("Suppl1.jpg", human_only_map, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, width = 20, height = 15, units = "cm")
ggsave("Suppl2.jpg", plot_dif, path = "C:/Users/Mike/Dropbox/MAYV_risk_modeling/Manuscript",
       dpi = 300, width = 20, height = 15, units = "cm")
