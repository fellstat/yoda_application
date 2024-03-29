#library(raster)
library(sf)

#wpt <- wp %>% projectRaster(crs = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

world_pop_count <- function(da_locations, wp, d=10){
  units(d) <- "km"
  
  dist_1 <- rep(0, nrow(da_locations))
  for(i in 1:nrow(da_locations)){
    if((i %% 10) == 1)
      cat(".")
    if((i %% 100) == 1)
      cat("\n")
    pt <- da_locations[i,c("longitude","latitude")] %>% as.numeric() %>% st_point()
    #projection(pt) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
    #buf <- pt %>% st_buffer(dist=50000)
    
    buf <- st_set_crs(st_sfc(pt), st_crs("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))  %>% 
      st_transform(3488) %>% 
      st_buffer(dist=d) %>%
      st_transform("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
    tryCatch({
    wp_crop <- raster::crop(wp, raster::extent(st_bbox(buf)[c(1,3,2,4)]))
    ras <- raster::mask(wp_crop,st_as_sf(buf))
    dist_1[i] <- raster::cellStats(ras,"sum") #sum(as.array(ras),na.rm=TRUE)
    }, error=function(...){
      cat("Bad location")
      print(as.data.frame(da_locations[i,]))
    })
  }
  cat("\n")
  dist_1
}


world_pop_count_cluster <- function(da_locations, wp, d=10, cluster){
  units(d) <- "km"
  dist_1 <- rep(NA, nrow(da_locations))
  parallel::clusterExport(cluster, c("da_locations","wp","d"), envir = environment())
  parallel::clusterEvalQ(cluster, {library(sp);library(sf);library(tidyverse)})
  fun <- function(i){
    pt <- da_locations[i,c("longitude","latitude")] %>% as.numeric() %>% st_point()
    #projection(pt) <- CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
    #buf <- pt %>% st_buffer(dist=50000)
    
    buf <- st_set_crs(st_sfc(pt), st_crs("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))  %>% 
      st_transform(3488) %>% 
      st_buffer(dist=d) %>%
      st_transform("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
    res <- 0
    try({
      wp_crop <- raster::crop(wp, raster::extent(st_bbox(buf)[c(1,3,2,4)]))
      ras <- raster::mask(wp_crop,st_as_sf(buf))
      res <- raster::cellStats(ras,"sum") #sum(as.array(ras),na.rm=TRUE)
    })
    res
  }
  dist_1 <- parallel::parLapplyLB(cluster,1:nrow(da_locations), fun) %>% unlist()
  dist_1
}
