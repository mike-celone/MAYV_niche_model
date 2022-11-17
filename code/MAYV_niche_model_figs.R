library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(RColorBrewer)
library(gridExtra)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Bring in the occurrence data
# Data includes records from all host types where uncertainty is <75km
occ_pts <- read_excel("All_Hosts_occ_75km_9SEP22.xlsx") 

# Plot of occurrence locations
occ_pts_fig <- ggplot(data = world) +
                      geom_sf() +
                      coord_sf(xlim = c(-110, -32), ylim = c(-33, 25), expand = FALSE) +
                      geom_jitter(data = occ_pts, aes(x = X_Coord, y = Y_Coord, fill = Host),
                                  alpha = 0.7, pch=21, size = 4) +
                      scale_fill_brewer(palette="RdYlBu") +
                      labs(x = "longitude", y = "latitude") +
                      annotation_scale(location = "br", width_hint = 0.2, style = "tick") +
                      annotation_north_arrow(location = "tr", which_north = "true", 
                                             #pad_x = unit(1.5, "in"), pad_y = unit(0.5, "in"),
                                             style = north_arrow_fancy_orienteering) +
                      #theme(legend.position = "none") +
                      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Plot of cases by year
pts_new <- occ_pts %>% 
  mutate(
    Year = case_when(
      Year_MAYV_Start != Year_MAYV_End ~ ((Year_MAYV_Start + Year_MAYV_End)/2),
      TRUE ~ Year_MAYV_Start)) %>%
  mutate(
    Year = round(Year, digits=0)) %>%
  select(Host, Year) %>%
  count(Host, Year)

pts_new$Host <- factor(pts_new$Host, 
                          levels = c("Animal", "Arthropod", "Human"))

occ_pts_year <- ggplot(pts_new, aes(fill = Host, y = n, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  scale_fill_brewer(palette="RdYlBu") +
  #xlab("Year") + ylab("MAYV Occurrences") +
  theme(legend.position = "none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(colour="black"))

fig1_final <- occ_pts_fig + annotation_custom(ggplotGrob(occ_pts_year), 
                                              xmin = -108, xmax = -80, 
                                              ymin = -30, ymax = -10)


#--Partial dependence with rug plots--#

# Loop through and extract the data from the model list
effect.plot <- function(var, lis_no){
  
  list <- list()
  
  p <- data_all %>% 
    filter(id==1) %>% 
    select(var)
  
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

# Plot all PDPs
cowplot::plot_grid(evi_effect, 
                   evergreen_effect, 
                   lstDay_effect,
                   lstNight_effect,
                   rain_effect,
                   tcb_effect,
                   elev_effect,
                   slope_effect,
                   tcw_effect,
                   urban_effect)
