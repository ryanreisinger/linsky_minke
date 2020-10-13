library(raster)
library(raadtools)

# Set an extent (in WGS84) based on the tracking data
ext_wgs84 <- extent(c(-64.2 - 0.5, -62.492 + 0.5, -65.176 - 0.5, -64.722 + 0.5))

# Get bathymetry for that area
# Resolution isn't great
b <- readtopo(topo = "gebco_19",
              xylim = ext_wgs84)

writeRaster(b, "minke_bath.grd", format = "raster")

# So, let's get the REMA elevantion data (land only)

# First, project the WGS1984 extent into the CRS that REMA uses
ext_stereo <- projectExtent(b, crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Then get the REMA data
e <- readtopo(topo = "rema_100m", xylim = ext_stereo)
plot(e)

# Next, project the REMA data into WGS84
# Be careful, this step takes a while...
e_wgs <- projectRaster(e, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
plot(e_wgs)

# Save the raster
writeRaster(e_wgs, "minke_rema.grd", format = "raster")
