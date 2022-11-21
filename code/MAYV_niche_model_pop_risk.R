library(raster)
library(maptools)

#---Estimate population at risk using the GPW raster---#

# Bring in the population raster 
pop_count <- raster("gpw_v4_population_count_rev11_2020_2pt5_min.tif")

# Bring in the model mean tif layer
model_mean <- raster("BRT_all_host_layers_5OCT22/mean_allhost_5OCT22.tif")

# Extract prediction values at each occurrence point
extract_occ_values <- extract(model_mean, occ_pts_final, df = T)

# Determine the value that encompasses 90% of occurrence points
threshold <- quantile(extract_occ_values[,2], probs = .1)

# convert prediction map to binary
reclass <- function(x) {
  ifelse(x <=  0.488, 0,
         ifelse(x >  0.488, 1, NA)) }

binary_map <- calc(model_mean, fun = reclass)

# Crop population map to our study area
pop_crop <- crop(pop_count, binary_map) 
pop_mask <- mask(pop_crop, binary_map)

# Multiply binary map and population raster
count <- binary_map*pop_mask

# Sum the population pixel values across each country
# Load the country polygons
data("wrld_simpl")

# Disable scientific notation
options(scipen = 999)

# Extract the population counts within each suitable pixel
pop_sum <- extract(count, 
                   SpatialPolygons(wrld_simpl@polygons))

df <- data.frame(ISO3=wrld_simpl$ISO3, 
                 SUM=unlist(lapply(pop_sum, sum, na.rm=T)))
