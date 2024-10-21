Sys.setenv(https_proxy="http://127.0.0.1:7890")
Sys.setenv(http_proxy="http://127.0.0.1:7890")
library(rgee)
ee_Initialize(drive=T,gcs=T)

library(sf)
library(terra)
library(rgeeExtra)

# 指定所需数据范围
roi =  ee$Geometry$BBox(103,
                        33,
                        111,
                        38)

# EVI 月度最大值合成
## 采用的EVI数据信息见:
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_MOD09GA_006_EVI#bands

MVC_EVI = function(data,i){
  return(data$
           filter(ee$Filter$calendarRange(i, field = "month"))$
           max())
}

download.evi = function(t){
  evi = ee$ImageCollection("MODIS/MOD09GA_006_EVI")$ 
           filter(ee$Filter$calendarRange(t,t,"year"))$
           filterBounds(roi)
  
  img = MVC_EVI(evi,1)
  for (i in 2:12){
    img = c(img,MVC_EVI(evi,i))
  }
  
  img = ee$ImageCollection$fromImages(img)$mean()$rename(paste0("EVI",t))
  
  ee_as_rast(
    image = img,
    region = roi,
    scale = 1000,
    dsn = paste0('./datasets/EVI/EVI',t,'.tif'),
    via = "drive"
  )
}

for (t in 2001:2020){
  download.evi(t)
}


# https://developers.google.com/earth-engine/datasets/catalog/USGS_SRTMGL1_003#bands
elev = ee$Image("USGS/SRTMGL1_003")$
  select('elevation')$
  clip(roi)$
  rename('elevation')
slope = ee$Terrain$slope(elev)$rename('slope')
aspect = ee$Terrain$aspect(elev)$rename('aspect')

ee_as_rast(
  image = elev,
  region = roi,
  scale = 100,
  dsn = './datasets/Topography/elev.tif',
  via = "drive"
)


ee_as_rast(
  image = slope,
  region = roi,
  scale = 100,
  dsn = './datasets/Topography/slope.tif',
  via = "drive"
)

ee_as_rast(
  image = aspect,
  region = roi,
  scale = 100,
  dsn = './datasets/Topography/aspect.tif',
  via = "drive"
)

# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD12Q1#bands


download.lc = function(t){
  lc = ee$ImageCollection("MODIS/061/MCD12Q1")$
          select('LC_Type1')$
          filter(ee$Filter$calendarRange(t,t,"year"))$
          mosaic()$
          clip(roi)
    
    ee_as_rast(
      image = lc,
      region = roi,
      scale = 500,
      dsn = paste0('./datasets/LC/LC',t,'.tif'),
      via = "drive"
    )
  }

for (t in 2001:2020){
  download.lc(t)
}

