library(seegSDM)

today <- as.character(Sys.Date())

# Number of bootstraps
n_boot <- 100

# Take N bootstrap samples of the dataset w/ minimum of 25 presence and 25 absence
data_list <- replicate(n_boot,
                       subsample(data_all,
                                 nrow(data_all),
                                 minimum=c(25,25),
                                 prescol = 3,
                                 replace = TRUE,
                                 max_tries = 20),
                       simplify = FALSE)

#-------BRT model implementation------#
# List to store model results
model_list <- list()

# List to store cross-validation results
cv_list <- list()

# set up folder for model output
dir.create(paste("BRT_model_results_", today, sep=""))

for (i in 1:n_boot){
  
  model_list[[i]] <- seegSDM::runBRT(data_list[[i]],
                                     gbm.x = 4:13,
                                     gbm.y = 3,
                                     pred.raster = predictors,
                                     gbm.coords = 1:2,
                                     wt = function(id) ifelse(id == 1, 1, sum(id)/sum(1-id)),
                                     verbose = TRUE,
                                     #Default parameter settings
                                     tree.complexity = 4,
                                     learning.rate = 0.01,
                                     bag.fraction = 0.75,
                                     n.trees = 10,
                                     n.folds = 10,
                                     max.trees = 10000,
                                     step.size = 10)
  
  # Cross-validation statistics
  cv_list[[i]] <- seegSDM::getStats(model_list[[i]],
                                    cv = TRUE)
  
  # Save each prediction raster as TIF files
  writeRaster(model_list[[i]][[4]], 
              paste("BRT_model_", i, "_all_host_", today, ".tif", sep=""), 
              format = "GTiff")
  
}

 # Stack the prediction maps
preds <- raster::stack(lapply(model_list, '[[', 4))

# Calculate the mean across the 100 sub-models
model_mean <- raster::calc(preds, fun = mean)

# calculate uncertainty as the SD across the 100 sub-models
model_sd <- raster::calc(preds, fun = sd)

# convert the CV stats list into a matrix using the do.call function
stats <- do.call("rbind", cv_list)

# write the cv stats to CSV
write.csv(stats, paste("cv_stats_", today, ".csv", sep = ""))

# generate the relative influence plot
relinf <- seegSDM::getRelInf(model_list, plot = TRUE)

writeRaster(model_mean, "mean.tif", format = "GTiff")
writeRaster(model_sd, "sd.tif", format = "GTiff")
