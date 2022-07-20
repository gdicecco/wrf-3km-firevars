library(ncdf4)
library(raster)
library(tidyverse)

## Pulled 2060-2064 3-km daily tmax, precip, t avg, windspeed avg from AWS
# https://wrf-cmip6-noversioning.s3.amazonaws.com/index.html#downscaled_products/gcm/cnrm-esm2-1_r1i1p1f2_ssp370/postprocess/d03/

## Set up grid
grid <- nc_open("wrfinput_d03_coord.nc")
lat <- ncvar_get(grid, "lat2d")
lon <- ncvar_get(grid, "lon2d")

## List wrf daily summary files
wrf_files <- list.files("wrf_data/")

# wrf proj "+proj=lcc +lon_0=-119.12109375 +lat_0=37.3846130371094 +lat_1=30 +lat_2=60"

## Produce annual avgs from daily summaries
## Summaries as data frames - convert to raster at end because of irregular grid
## Calc Mar-Oct annual avgs
## DOY 61-305

map(wrf_files, ~{
  f <- .
  
  year <- parse_number(str_sub(f, -7, -3))
  varname <- word(f, 1, 1, sep = "\\.")
  
  print(paste0(Sys.time(), " starting variable ", varname, " for year ", year))
  
  nc <- nc_open(paste0("wrf_data/", f))
  t <- ncvar_get(nc, "day")
  
  wrf.array <- ncvar_get(nc, varname) # store the data in a 3-dimensional array
  dim(wrf.array) 
  fillvalue <- ncatt_get(nc, varname, "_FillValue")
  
  wrf.array[wrf.array == fillvalue$value] <- NA
  
  wrf_df <- reshape2::melt(wrf.array)
  colnames(wrf_df) <- c('lon_cell', 'lat_cell', 'day', 'value')

  wrf_season_mean <- wrf_df %>%
    filter(day >= 61 & day <= 305) %>%
    group_by(lon_cell, lat_cell) %>%
    summarize(value_mean = mean(value, na.rm = T))
  
  filename <- paste0("wrf_annual/wrf3km_cnrm-esm2-1_ssp370_", year, "_", varname, ".csv")
  
  write.csv(wrf_season_mean, filename, row.names = F)
  
  print(paste0(Sys.time(), " wrote file ", varname, " for year ", year))
  
  nc_close(nc)
  
})

## Produce 5 year averages for each clim variable & regrid to regular raster

variables <- c("prec", "rh", "t2", "t2max", "wspd10max", "prec_max")

annual_files <- list.files("wrf_annual/")[grepl("csv", list.files("wrf_annual/"))]

map(variables, ~{
  v <- .
  
  if(v == "prec") {
    var_files <- annual_files[grepl(v, annual_files) & !grepl("max", annual_files)]
  } else {
    var_files <- annual_files[grepl(v, annual_files)]
  }
    
  fiveyr_mean <- data.frame(file = var_files) %>%
    group_by(file) %>%
    nest() %>%
    mutate(data = map(file, ~read_csv(paste0("wrf_annual/", .)))) %>%
    unnest(cols = c("data")) %>%
    group_by(lat_cell, lon_cell) %>%
    summarize(fiveyr_mean = mean(value_mean, na.rm = T))
  
  # filename <- paste0("wrf_summ/wrf3km_cnrm-esm2-1_ssp370_2060-2064_mean_", v, ".csv")
  # 
  # write.csv(fiveyr_mean, filename, row.names = F)
  
  # Regrid to raster for five year means
  
  #Rebuild the WRF Lambert Cylindrical Grid
  fiveyr_mean$xlat<- apply(fiveyr_mean, 1, function(x) lat[x['lon_cell'],x['lat_cell']])
  fiveyr_mean$xlon<- apply(fiveyr_mean, 1, function(x) lon[x['lon_cell'],x['lat_cell']])
  
  #Convert irregular WRF grid to spatial pixels data frame
  pixels <- SpatialPixelsDataFrame(data.frame(fiveyr_mean$xlon, fiveyr_mean$xlat), tolerance = 0.916421, fiveyr_mean)
  
  proj4string(pixels) = CRS("+proj=lcc +lon_0=-119.12109375 +lat_0=37.3846130371094 +lat_1=30 +lat_2=60 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m ") #passing the middle of grid from points2grid
  
  #This works!
  rg <- raster(ncols=244,nrows=493) #create a raster to project onto
  extent(rg) <- c(min(lon),max(lon),min(lat),max(lat))
  
  mean_raster <- rasterize(pixels,rg,field='fiveyr_mean')
  
  filename <- paste0("wrf_summ/wrf3km_cnrm-esm2-1_ssp370_2060-2064_mean_", v, ".tiff")
  writeRaster(mean_raster, filename, overwrite = T)

})

