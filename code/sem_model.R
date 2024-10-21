library(sf)
library(terra)
library(lavaan)
library(semPlot)
library(tidyverse)

read_csv('./datasets/ds.csv') |> 
  st_as_sf(coords = c('Long','Lat'),
           crs = 'EPSG:4326')-> ds

# scale
ds |> 
  mutate(across(Aspect:FVCC,
                ~scales::rescale(.x))) -> ds

fvc_trend = rast('./result/mk_test/fvc_trend.tif')
# fvc_trend值含义：
# -2严重退化
# -1轻微退化
# 0稳定不变
# 1轻微改善
# 2明显改善

terra::extract(fvc_trend,
               vect(ds)) |> 
  pull() -> ds$`FVC_Change`

ds_value = -2:2
ds_name = c('严重退化','轻微退化','稳定不变','轻微改善','明显改善')

unlink('./datasets/zone/',recursive = TRUE)
dir.create('./datasets/zone/')

set.seed(42)

sem.sample = function(zone){
  ds |> 
    dplyr::filter(FVC_Change == zone) |> 
    spsurvey::grts(n_base = 120,projcrs_check = FALSE) -> dt
  
  dt$sites_base |> 
    st_drop_geometry() |> 
    as_tibble() |> 
    select(Aspect:FVCC) |> 
    #select(Aspect:FVCC,X,Y) |> 
    #rename(Long = X,Lat = Y) |> 
    mutate(across(where(is.numeric),
                  ~scale(.x)[,1])) -> sample_result
  return(sample_result)
}

seq(1,5) |> 
  map(\(i) sem.sample(ds_value[i]) |> 
        write_csv(str_c('./datasets/zone/',
                        ds_name[i],'.csv')))

indepent_var = names(ds)[-13]
repose_var = "FVCC"
paste(indepent_var,collapse = "+") %>%
  paste(repose_var,'~',.) -> lm.model 

lm(lm.model,data=ds) %>% summary()

# PreMaxC + PreMinC + PreTotalC + TmpMaxC + TmpMinC + TmpAvgC
# Bulid The SEM Model
sem.model = '
         TOPO =~ Aspect + Elev + Slope 
         CC =~ PreTotalC + TmpAvgC
         HAC =~ NtlC + PdC + LuC
        
         HAC ~ TOPO
         FVCC ~ HAC + CC 
        '

sem.fit =  sem(sem.model, data = ds)
summary(sem.fit, standardized = T)

fitMeasures(sem.fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))

semPaths(sem.fit,
         what = "std",
         layout = "tree2",
         fade = F,
         residuals = F,
         nCharNodes	= 0
) 