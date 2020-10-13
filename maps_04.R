# Minke maps
# Ryan Reisinger for Jake Linsky
# 2020-04-10

## Set the working directory
setwd("C:/Users/Ryan Reisinger/Documents/Academic/UCSC/Work/People/Jake/Minke Ice Data/")

## Libraries
library(ggplot2) # For plotting
library(raster) # For the mapping data
library(sf) # for shape files

## Make a list of the individuals
ids <- list.files(recursive = F)[grepl(x = list.files(recursive = F), pattern = "bb")] # Get the folder names
ids <- gsub(" .*$", "", ids) # Get only the individual name

## Read in and combine all the data we're intereste in,
## while adding the individual ids

## Get the right file names
ice.files <- list.files(recursive = T)[grepl(x = list.files(recursive = T), pattern = "IceObs.csv")]
loc.files <- list.files(recursive = T)[grepl(x = list.files(recursive = T), pattern = "degtrack.csv")]

## Let's loop through the individuals, read in those files,
## and combine them all into one dataframe that we can subset later
## as neccessary

locs <- data.frame() # Empty dataframe to hold locations
ice <- data.frame() # Empty dataframe to hold ice

for (i in ids) {
  
  print(i) # Print the individual so we can diagnose any errors
  
  ## The locations
  this.loc <- read.csv(loc.files[grepl(x = loc.files, pattern = i)],
                       header = F,
                       stringsAsFactors = F,
                       numerals = "no.loss",
                       na.strings = "NaN")
  names(this.loc) <- c("lon", "lat") # Name the variables
  this.loc$id <- i # Add the animal ID
  locs <- rbind(locs, this.loc) # Add the data to the holder
  
  ## The ice data
  this.ice <- read.csv(ice.files[grepl(x = ice.files, pattern = i)],
                       header = F,
                       stringsAsFactors = F,
                       numerals = "no.loss",
                       na.strings = "NaN")
  names(this.ice) <- c("lon", "lat", "ice") # Name the variables
  this.ice$id <- i # Add the animal ID
  ice <- rbind(ice, this.ice) # Add the data to the holder
  
}

## Now locs and ice contain the lonlat locations and ice data, respectively,
## for all the individuals

locs <- locs[complete.cases(locs), ] # Drop NaNs
ice <- ice[!(is.na(ice$lon)), ] # Drop NaNs
ice <- ice[!(is.na(ice$ice)), ] # Drop NaNs


## Create a factor from the ice data
ice$ice <- as.factor(ice$ice)

## Make sure lat lon are numeric
locs$lat <- as.numeric(locs$lat)
locs$lon <- as.numeric(locs$lon)

ice$lat <- as.numeric(ice$lat)
ice$lon <- as.numeric(ice$lon)

#-------------------------------------------------
## Maps

# ## Look at the geographic extent
# min(locs$lon)
# max(locs$lon)
# min(locs$lat)
# max(locs$lat)

## Load up the raster data
b <- raster("./ryan/minke_bath.grd")
b[b>0] <- 0

## Load up the shapefiles
coast <- st_read("./background maps/background maps/Coastline_high_res_polygon.shp")

## Project it into WGS84 to match the tracking data
coast.p <- st_transform(x = coast, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")



for (i in 1:length(ids)) {
  
  ## Let's map a given individual
  # this.id <- "bb180125-30"
  this.id <- ids[i]
  
  print(this.id) # Keep track
  
  ## Get the data
  this.loc <- locs[locs$id == this.id, ]
  this.ice <- ice[ice$id == this.id, ]
  
  ## Calculate the current limits and ranges
  minlon <- min(this.loc$lon)
  maxlon <- max(this.loc$lon)
  
  minlat <- min(this.loc$lat)
  maxlat <- max(this.loc$lat)
  
  latrange <- max(this.loc$lat) - min(this.loc$lat)
  lonrange <- max(this.loc$lon) - min(this.loc$lon)
  
  asp <- 3 # lon/lat aspect ratio
  
  if (lonrange/asp < latrange){
    # expand lonrange
    minlon <- minlon - (latrange*asp)/2
    maxlon <- maxlon + (latrange*asp)/2
  } else {
    # # expand latrange
    # minlat <- minlat - (lonrange/asp)*0.5
    # maxlat <- maxlat + (lonrange/asp)*0.5
  }
    
  
  ## Crop the raster according to this individual
  # this.ext <- extent(minlon,
  #                    maxlon,
  #                    minlat,
  #                    maxlat)
  
  # Slight buffer at the edge
  this.ext <- extent(minlon - 0.02,
                     maxlon + 0.02,
                     minlat - 0.005,
                     maxlat + 0.005)
  
  this.b <- crop(b, this.ext)
  
  ## Crop the coastline using the raster
  this.coast <- st_crop(st_buffer(coast.p, dist = 0), this.b)
  
  ## Check quickly as raster
  plot(this.b)
  points(this.loc$lon, this.loc$lat, pch = 16, cex = 0.1)
  
  ## Plot in ggplot
  
  ## Creat a dataframe from the raster
  p <- data.frame(rasterToPoints(this.b))
  names(p) <- c("x", "y", "z")
  
  ## 'Fortify' the shape file
  cst <- fortify(this.coast)
  
  ## Colours
  colrs <- c(rgb(0, 0, 0),
             rgb(1, 0, 0),
             rgb(1, .62, 0),
             rgb(1, 1, 0),
             rgb(1, 1, .62),
             rgb(1, 1, 1))
  
  tiff(paste0("./ryan/maps/", this.id, ".tiff"), res = 600, height = 5, width = 8, units = "in") 
  plt <- ggplot(data = p, aes(x = x, y = y)) +
    # geom_tile() +
    # scale_fill_gradient(low = "slategray1", high = "slategray1") +
    coord_quickmap() +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_sf(data = this.coast, inherit.aes = F, fill = "gray40", colour = "black") +
    geom_path(data = this.loc, aes(x = lon, y = lat), inherit.aes = F,
              na.rm = T) +
    geom_point(data = this.ice, aes(x = lon, y = lat, color = ice),
               size = 2,
               inherit.aes = F,
               na.rm = T) +
    scale_color_manual(values = colrs,
                       labels = c("0%", "1-20%", "21-40%", "41-60%", "61-80%", "81-100%")) +
    labs(title = this.id,
         x = "",
         y = "",
         color = "Ice\nconcentration") + # Name the colour legend
    guides(fill = FALSE,
           color = guide_legend(reverse = TRUE)) + # Switch off the fill legend and reverse the colours
    theme_bw() +
    theme(axis.text = element_text(colour = "black"),
          panel.background = element_rect(fill = "gray80",
                                          colour = "gray80"),
          legend.key = element_rect(fill = "gray80",
                                    colour = "gray80"),
          panel.grid = element_blank())
  
  print(plt)
  
  dev.off()
  
}
