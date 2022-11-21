
# Load required packages
library(SDMtune)
library(ggplot2)
library(raster)
library(dplyr)
library(dismo)
library(spThin)
library(sf)
library(seegSDM)

# Set today's date
today <- as.character(Sys.Date())

#----------------------Import data------------------------------#

# Bring in the occurrence data
# Data includes records from all host types where uncertainty is <75km
occ_pts <- read_excel("All_Hosts_occ_75km_9SEP22.xlsx")

# Bring in population density layer
popdens <- raster("Sep22_popdens.asc")

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

# Use spThin to remove occurrence points within the same 5km radius
occ_pts$species <- "MAYV"

occ_pts_thin <- thin(occ_pts, 
                     lat.col = "X_Coord", 
                     long.col = "Y_Coord",
                     spec.col = "species",
                     thin.par = 5,
                     reps = 1,
                     locs.thinned.list.return = T,
                     write.files = F)

# Turn the spThin list into a dataframe
occ_pts_final <- as.data.frame(occ_pts_thin) %>%
  dplyr::rename(x = Latitude, y = Longitude) %>%
  dplyr::select(x, y)

#-------------------Background data-----------------------#

# Randomly sample 10000 background points using 2-degree method 
# Sample is weighted by the population density
bg_pts <- bgSample(popdens,
                   n = 10000,
                   prob = TRUE,
                   replace = FALSE,
                   spatial = TRUE)

bg_pts <- as.data.frame(bg_pts)

# Plot presence/absence points to make sure everything looks alright
ggplot(map_data("world"), aes(long, lat)) +
    geom_polygon(aes(group = group), fill = "grey95", color = "gray40", size = 0.2) +
    geom_jitter(data = bg_pts, aes(x = x, y = y),
                color = "blue", alpha = 0.5, size = 0.8) +
    geom_jitter(data = occ_pts_final, aes(x = x, y = y), color = "red",
                alpha = 0.8, size = 1.5) +
    labs(x = "longitude", y = "latitude") +
    theme_minimal() +
    scale_x_continuous(limits = c(-110, -32)) +
    scale_y_continuous(limits = c(-60, 30))
  
#-----------------Assess variable correlation-------------------#

# Prepare a data object using the SDMTune package
bg_swd <- prepareSWD(species = "Bgs", 
                     a = bg_pts, 
                     env = predictors)

# Explore the Spearman correlation between variables
plotCor(bg_swd, 
        method = "spearman", 
        cor_th = 0.8)

corVar(bg_swd, 
       method = "spearman", 
       cor_th = 0.8)

# Train a simple model using regular 5-fold CV
swd_object <- prepareSWD(species = "MAYV",
                         p = occ_pts_final,
                         a = bg_pts,
                         env = predictors)

folds <- randomFolds(swd_object, 
                     k = 5, 
                     only_presence = TRUE)

cv_model_brt <- train(method = "BRT", 
                      data = swd_object, 
                      folds = folds)

# Remove correlated variables based on AUC
remove_corr_model_brt <- varSel(cv_model_brt, 
                                metric = "auc", 
                                bg4cor = bg_swd, 
                                method = "spearman", 
                                cor_th = 0.8, 
                                permut = 10)

#Drop the correlated variable
#predictors_final <- dropLayer(predictors, c(""))

#----Set up dataframe for modeling----#

# Add a column for presence/absence
bg_pts$id <- 0
occ_pts_final$id <- 1 

# Bind the presence and absence points
data <- rbind(bg_pts, occ_pts_final)

# Extract the covariate values at presence/absence points 
data_extract <- extract(predictors, data[,1:2])

# Bind the coordinates with the data
data_all <- cbind(data, data_extract)

# Remove NA values
data_all <- na.omit(data_all)
