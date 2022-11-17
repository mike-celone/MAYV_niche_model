library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(cowplot)
library(readxl)
countries <- c("brazil", "peru", "argentina", "chile", "uruguay", "paraguay",
               "ecuador", "colombia", "panama", "suriname", "guyana", 
               "trinidad and tobago", "mexico", "belize", "costa rica", 
               "el salvador", "guatemala", "honduras", "nicaragua", "haiti",
               "bolivia", "france", "venezuela", "dominican republic")
# World map
country_map <- ne_countries(country = countries, scale = "medium", returnclass = "sf")

# Brazil map
brazil <- ne_states(country = "brazil", returnclass = "sf")

# Bring in the occurrence data
occ_pts <- read_excel("All_Hosts_occ_75km_9SEP22.xlsx") 

# Bring in consensus scores
cons_country <- read.csv("Consensus_score_16NOV22.csv", fileEncoding="UTF-8-BOM")
cons_brazil <- read.csv("Brazil_consensus_score_16NOV22.csv", fileEncoding="UTF-8-BOM")

#----Fig 1----#
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
fig1a <- ggplot() + 
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

#----Fig 1b----#
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
fig1b <- ggplot() + 
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

fig1_final <- plot_grid(fig1a, fig1b, align = "h", axis = "b", 
                        rel_widths = c(1, 0.7), labels = c('A', 'B'), 
                        label_size = 18)

#----Fig 2----#

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
fig2_final <- occ_pts_fig + annotation_custom(ggplotGrob(occ_pts_year), 
                                              xmin = -108, xmax = -80, 
                                              ymin = -30, ymax = -10)

#---Fig 3---#

# Convert raster to dataframe
meanras_df <- as.data.frame(model_mean, xy = TRUE) %>% 
  rename(Suitability = layer) %>%
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

fig3_final <- ggdraw(main) +
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

#---Fig 4---#

# Convert raster to dataframe
sdras_df <- as.data.frame(model_sd, xy = TRUE) %>% 
  rename(Uncertainty = layer) %>%
  na.omit()

Fig4_final <- ggplot()+
  geom_raster(aes(x = x, y = y, fill = Uncertainty), data = sdras_df)+
  geom_sf(fill = 'transparent', data = country_map)+
  coord_sf(xlim = c(-120, -32), ylim = c(-40, 33), expand = FALSE) +
  scale_fill_distiller(palette = "GnBu", trans = "reverse", breaks = c(0.1, 0.5), labels = c("Low", "High")) +
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(.3, .4),
    legend.key.size = unit(1, 'cm'), 
    legend.title = element_text(size=14), 
    legend.text = element_text(size=10))    

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
