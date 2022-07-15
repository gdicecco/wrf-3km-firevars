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

## Produce annual avgs from daily summaries
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
  
  wrf_brick <- brick(wrf.array, ymn=min(lon), ymx=max(lon), xmn=min(lat), xmx=max(lat), 
                     crs=CRS("+proj=lcc +lon_0=-119.12109375 +lat_0=37.3846130371094 +lat_1=30 +lat_2=60"))
  
  r <- t(wrf_brick)
  r_season <- subset(r, 61:305)
  
  r_mean <- mean(r_season)
  # plot(r_mean)
  
  filename <- paste0("wrf_annual/wrf3km_cnrm-esm2-1_ssp370_", year, "_", varname, ".tiff")
  
  writeRaster(r_mean, filename, overwrite = T)
  
  print(paste0(Sys.time(), " wrote raster file ", varname, " for year ", year))
  
  nc_close(nc)
  
})

## Produce 5 year averages for each clim variable

variables <- c("prec", "rh", "t2", "t2max", "wspd10max", "prec_max")

annual_files <- list.files("wrf_annual/")[grepl("tiff$", list.files("wrf_annual/"))]

map(variables, ~{
  v <- .
  
  if(v == "prec") {
    var_files <- annual_files[grepl(v, annual_files) & !grepl("max", annual_files)]
  } else {
    var_files <- annual_files[grepl(v, annual_files)]
  }
    
  years_brick <- stack(paste0("wrf_annual/", var_files))

  years_mean <- mean(years_brick)
  
  filename <- paste0("wrf_summ/wrf3km_cnrm-esm2-1_ssp370_2060-2064_mean_", v, ".tiff")
  
  writeRaster(years_mean, filename, overwrite = T)
  
})

## Check Maxent performance with this suite of 5 variables 

