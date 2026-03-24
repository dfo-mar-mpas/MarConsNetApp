library(terra)
library(dplyr)

tar_load(MPAs)
mpa_vect <- vect(mpas)
url <- "https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/5b86e2d2-cec1-4956-a9d5-12d487aca11b/attachments/NorthwestAtlantic_VesselDensity_2023_AIS.zip"
temp_zip <- tempfile(fileext = ".zip")
download.file(url, temp_zip, mode = "wb")
temp_dir <- tempdir()
unzip(temp_zip, exdir = temp_dir)
list.files(temp_dir)

mpas <- MPAs[-45,]

# list files
tif_file <- file.path(temp_dir, "All_VesselsPerDay_2023_AIS.tif")
r <- rast(tif_file)
r_ll <- project(r, "EPSG:4326")  # reproject to lon/lat

## CREATING PALETTE
vals_sample <- values(r_ll, mat = FALSE)        # extract raster values
vals_sample <- vals_sample[is.finite(vals_sample)]  # remove NA and Inf

plot(r_ll)
plot(mpa_vect, add = TRUE, border = "lightgray", lwd = 2)

## ANALYSIS
r_mpas <- mask(r_ll, mpa_vect)

# Quick plot
plot(r_mpas, main="Vessel Density within MPAs")
plot(mpa_vect, add=TRUE, border="blue", lwd=2)

# ANALYSIS
## Step 1 — Extract raster values inside each MPA
mpa_values <- terra::extract(r_ll, mpa_vect, ID=TRUE)

## Step 2 — Summarize per MPA

mpa_rasters <- vector("list", length(mpa_vect))

# Loop over each MPA to mask the raster
for(i in seq_along(mpa_vect)){
  # mask raster to the single polygon
  mpa_rasters[[i]] <- mask(r_ll, mpa_vect[i])
}

# Extract statistics (mean, max, sum) from the raster inside each MPA
mpa_summary <- data.frame(
  NAME_E = mpas$NAME_E,
  mean_vessels = sapply(mpa_rasters, function(x) mean(values(x), na.rm=TRUE)),
  max_vessels  = sapply(mpa_rasters, function(x) max(values(x), na.rm=TRUE)),
  sum_vessels  = sapply(mpa_rasters, function(x) sum(values(x), na.rm=TRUE)),
  raster = I(mpa_rasters)  # store the SpatRaster in a list-column
)

final <- mpa_summary[,c("NAME_E", 'mean_vessels', 'raster')]
return(final)


