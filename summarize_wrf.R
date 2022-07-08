library(ncdf4)
library(raster)
library(tidyverse)

## Pull 2050-2065 3-km daily tmax, precip, t avg, windspeed avg from AWS
# https://wrf-cmip6-noversioning.s3.amazonaws.com/index.html#downscaled_products/gcm/cnrm-esm2-1_r1i1p1f2_ssp370/postprocess/d03/

wrf_files <- list.files("wrf_data/")

grid <- nc_open("wrfinput_d03_coord.nc")
lat <- ncvar_get(grid, "lat2d")
lon <- ncvar_get(grid, "lon2d")

## Calc Mar-Oct annual avgs + number of wet days
nc <- nc_open(paste0("wrf_data/", wrf_files[16]))
t <- ncvar_get(nc, "day")
wrf.array <- ncvar_get(nc, "t2") # store the data in a 3-dimensional array
dim(wrf.array) 
fillvalue <- ncatt_get(nc, "t2", "_FillValue")
fillvalue
nc_close(nc_data) 

wrf.array[wrf.array == fillvalue$value] <- NA

wrf.slice <- wrf.array[, , 1] 

r <- raster(t(wrf.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=lcc +lon_0=-119.12109375 +lat_0=37.3846130371094 +lat_1=30 +lat_2=60"))

plot(r)

## Check Maxent performance with this suite of 5 variables 

