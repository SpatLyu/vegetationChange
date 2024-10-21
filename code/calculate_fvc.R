library(terra)
library(tidyverse)

evi.file = list.files('./datasets/EVI/',pattern = ".tif$") %>%  
  paste0('./datasets/EVI/',.)

#FVC 植被覆盖度
## EVIsoil 取EVI 1 分位数
## EVIveg  取EVI 99分位数
## FVC = (EVI - EVIsoil) / (EVIveg - EVIsoil)

calcul_fvc = function(evi_file){
  evi = rast(evi_file)
  evi.soil = global(evi,quantile,
                     probs=0.01,na.rm=T)[1,1]
  evi.veg = global(evi,quantile,
                    probs=0.99,na.rm=T)[1,1]
  fvc = (evi - evi.soil) / (evi.veg - evi.soil)
  
  fvc|> 
    as.matrix() |> 
    na.omit() |> 
    max() -> max.fvc
  
  fvc|> 
    as.matrix() |> 
    na.omit() |> 
    min() -> min.fvc
  
  fvc = (fvc - min.fvc) / (max.fvc - min.fvc)
  names(fvc) = str_c('FVC',evi_file |> str_sub(-8,-5))
  writeRaster(fvc,str_c('./datasets/FVC/',names(fvc),'.tif'))
  return(fvc)
}

for (f in evi.file){
  calcul_fvc(f)
}
