######################################
#Mayaro Virus niche model development#
#Boosted regression tree model       #
#Created August 30, 2022             #
#Michael Celone                      #
######################################

library(SDMtune)
library(ggplot2)
library(raster)
library(readxl)
library(dplyr)
library(dismo)
library(spThin)
library(sf)
library(seegSDM)
setwd("C:/Users/Mike/OneDrive - usuhs.edu/MAYV/Prediction project")

# clear workspace
#rm(list = ls())

#----------------------Import data------------------------------#
#Bring in the occurrence data
#Data includes records from all host types where uncertainty is <75km
occ_pts <- read_excel("All_Hosts_occ_75km_9SEP22.xlsx")

#Bring in population density layer
popdens <- raster("Sep22_popdens.asc")

#Bring in raster files 
#These have been processed in ArcGIS to ensure matching extent and resolution
setwd("C:/Users/Mike/OneDrive - usuhs.edu/MAYV/Prediction project/covs_22Sep")
rast_files <- list.files(pattern ='.asc$', 
                         all.files=TRUE) 

#Create a raster stack
predictors <- stack(rast_files)

#Fix up the layer names
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

#Use spThin to remove occurrence points within the same 5km radius
occ_pts$species <- "MAYV"

occ_pts_thin <- thin(occ_pts, 
                     lat.col = "X_Coord", 
                     long.col = "Y_Coord",
                     spec.col = "species",
                     thin.par = 5,
                     reps = 1,
                     locs.thinned.list.return = T,
                     write.files = F)

#Turn the spThin list into a dataframe
occ_pts_final <- as.data.frame(occ_pts_thin) %>%
  rename(x=Latitude, y=Longitude) %>%
  dplyr::select(x, y)

#-------------------Background data-----------------------#

#Randomly sample 10000 background points using 2-degree method
#Sample is weighted by the population density
bg_pts <- bgSample(popdens,
                   n = 10000,
                   prob = TRUE,
                   replace = FALSE,
                   spatial = TRUE)

bg_pts <- as.data.frame(bg_pts)

#Plot presence/absence points to make sure everything looks alright
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
library(SDMtune)

bg_swd <- prepareSWD(species = "Bgs", 
                     a = bg_pts, 
                     env = predictors)

#Explore the Spearman correlation between variables
plotCor(bg_swd, 
        method = "spearman", 
        cor_th = 0.8)

corVar(bg_swd, 
       method = "spearman", 
       cor_th = 0.8)

#Train a simple model using regular 5-fold CV
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

#Remove correlated variables based on AUC
remove_corr_model_brt <- varSel(cv_model_brt, 
                                metric = "auc", 
                                bg4cor = bg_swd, 
                                method = "spearman", 
                                cor_th = 0.8, 
                                permut = 10)

#Drop the correlated variable
#predictors_final <- dropLayer(predictors, c(""))

#Add a column for presence/absence
bg_pts$id <- 0
occ_pts_final$id <- 1 

#Bind the presence and absence points
data <- rbind(bg_pts, occ_pts_final)

#Extract the covariate values at presence/absence points 
data_extract <- extract(predictors, data[,1:2])

#Bind the coordinates with the data
data_all <- cbind(data, data_extract)

#Remove NA values
#presence <- data_all %>% filter(id==1)
#new_DF <- presence[rowSums(is.na(presence)) > 0,]
data_all <- na.omit(data_all)

#Number of bootstraps
n_boot <- 100

#Take N bootstrap samples of the dataset w/ minimum of 25 presence and 25 absence
data_list <- replicate(n_boot,
                       subsample(data_all,
                                 nrow(data_all),
                                 minimum=c(25,25),
                                 prescol = 3,
                                 replace = TRUE,
                                 max_tries = 20),
                       simplify = FALSE)

#-------BRT model implementation------#
model_list <-   lapply(data_list,
                       runBRT,
                       gbm.x = 4:13,
                       gbm.y = 3,
                       pred.raster = predictors,
                       gbm.coords = 1:2,
                       wt = function(id) ifelse(id == 1, 1, sum(id)/sum(1-id)),
                       verbose = TRUE,
                       #Default parameter settings
                       #tree.complexity = 4,
                       #learning.rate = 0.01,
                       #bag.fraction = 0.75,
                       #n.trees = 10,
                       n.folds = 10,
                       max.trees = 10000,
                       step.size = 10)

#Cross-validation stats
stat_lis <- lapply(model_list, 
                   seegSDM::getStats,
                   cv = TRUE)

#Stack the prediction maps
preds <- raster::stack(lapply(model_list, '[[', 4))

#Mean, median and uncertainty across the models
preds_sry <- seegSDM::combinePreds(preds, 
                                   quantiles = c(0.025, 0.975),
                                   parallel = FALSE, ncore = NULL)

# calculate uncertainty as the difference between the quantiles
preds_sry$uncertainty <- preds_sry[[4]] - preds_sry[[3]]

#convert the CV stats list into a matrix using the do.call function
stats <- do.call("rbind", stat_lis)

#generate the relative influence plot
relinf <- seegSDM::getRelInf(model_list, plot = TRUE)

#generate the effect plots
#effect <- seegSDM::getEffectPlots(model_list, plot = TRUE)

#generate custom PDP with rug plots

#Loop through and extract the data from the model list
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
  
  x <- model_list[[lis_no]]$effects[[lis_no]][1]
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

evi_effect <- effect.plot("EVI", lis_no=2)

lstDay_effect <- effect.plot("LST_Day", lis_no=3)
lstNight_effect <- effect.plot("LST_Night", lis_no=4)
rain_effect <- effect.plot("Rainfall", lis_no=5)
tcb_effect <- effect.plot("TCB", lis_no=6)
elev_effect <- effect.plot("Elevation", lis_no=7)
slope_effect <- effect.plot("Slope", lis_no=8)
vector_effect <- effect.plot("Vector", lis_no=9)

cowplot::plot_grid(evi_effect, 
                   primate_effect, 
                   lstDay_effect,
                   lstNight_effect,
                   rain_effect,
                   tcb_effect,
                   elev_effect,
                   slope_effect,
                   vector_effect)

writeRaster(mean, "mean.tif", format = "GTiff")
writeRaster(sd, "sd.tif", format = "GTiff")

#-------MaxEnt model implementation------------#
dismo::maxent()

data_list <- replicate(n_boot,
                       subsample(data,
                                 nrow(data),
                                 minimum=c(25,25),
                                 prescol = 3,
                                 replace = TRUE,
                                 max_tries = 20),
                       simplify = FALSE)

preds <- list(); auc <- list(); varimp <- list(); response <- list(); models <- list()

#Loop through the bootstrapped datasets and fit models 
for (i in 1:5){
  
  #set up an object with presence and background points
  p <- data_list[[i]] %>% filter(id==1) %>% select(-id)
  a <- data_list[[i]] %>% filter(id==0) %>% select(-id)
  swd <- prepareSWD(species = "MAYV", p = p, a = a, env = predictors_final)
  
  #Each model is fit using 10-fold cv
  folds <- randomFolds(swd, k = 10, only_presence = TRUE)
  models[[i]] <- train(method = "Maxent", data = swd, folds = folds)
  
  #Store the AUC, prediction maps, and variable importance in seperate lists
  auc[[i]] <- auc(models[[i]], test = TRUE)
  preds[[i]] <- predict(models[[i]], data = predictors_final, type = "cloglog")
  varimp[[i]] <- maxentVarImp(models[[i]])

}

#Function to get the data for the effect plots
effect.func <- function(var){
  
  #Create empty list
  list <- list()
  
  #Apply the SDMtune "plotResponse" function to the 100 models
  response <- lapply(models, plotResponse, var=var, type="cloglog")
  
  #Loop through and extract the data from the model list
  for(i in 1:length(models)) {
    list[[i]] <- response[[i]]$data[2:11]
    }
  
  x <- response[[1]]$data[1]
  
  #Bind the lists together into a dataframe
  df <- do.call("cbind", list)
  final <- cbind(x, df)

}

#Loop through each variable and get the data for the effects plots
vars <- names(predictors_final)

for (i in vars){ 
  #Run through the function above for each variable
  data <- effect.func(i)
  #Change the column names to get rid of duplicates
  colnames(data) <- c(1:ncol(data))
  #Calculate the row means and sd across the models
  data_final <- data %>% 
    rowwise() %>% 
    mutate(
      mean = mean(c_across(2:ncol(data))),
      sd = sd(c_across(2:ncol(data))))
  
  assign(paste(i, sep=""), data_final, envir = .GlobalEnv)
  }
