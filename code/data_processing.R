library(sf)
library(terra)
library(spsurvey)
library(tidyverse)

# load the spatial data
weihe = read_sf('./datasets/渭河流域边界/')
water = read_sf('./datasets/省市县-湖泊/渭河流域湖泊.shp')

weihe |> 
  st_transform(32648) |> 
  st_geometry() |> 
  st_make_grid(cellsize = 1500,
               what = "centers") |> 
  st_transform(4326) |> 
  st_as_sf() |> 
  st_filter(weihe,.predicate = st_intersects) |> 
  st_filter(water,.predicate = st_disjoint) -> sample.point

sample.point |> 
  st_coordinates() |> 
  as_tibble() |> 
  write_csv('./datasets/coord.csv')

vect(sample.point) -> sample.point

extract_value = function(x){
  r = rast(ncol=250,nrow=240)
  terra::extract(rast(x),
                 sample.point |> project(crs(rast(x)))) |> 
    pull() -> values(r)
  return(r)
}

# fun_sen = function(x){
#   if(length(na.omit(x))<20) return(c(NA, NA, NA))   
#   MK_estimate = trend::sens.slope(
#     ts(na.omit(x),start=2001,end = 2020,frequency=1)
#   ) 
#   return(MK_estimate$estimates)
# }

fun_slope = function(x){
  n = length(x)
  d = n * sum(c(1:n) ^ 2) - sum(c(1:n)) ^ 2
  m = n * sum(c(1:n) * x) - sum(c(1:n)) * sum(x)
  slope = m / d
  return(slope)
}

sample_value = function(path){
  list.files(path,'.tif$',
             full.names = TRUE) |> 
    map(extract_value) |> 
    rast() |> 
    app(fun_slope, cores=6) -> r
  return(values(r)[1:59949] |> 
           as_tibble())
}

c('./datasets/Climate/pre/pre_max/',
  './datasets/Climate/pre/pre_min/',
  './datasets/Climate/pre/pre_sum/',
  './datasets/Climate/tmp/tmp_max/',
  './datasets/Climate/tmp/tmp_min/',
  './datasets/Climate/tmp/tmp_mean/',
  './datasets/Ntl/',
  './datasets/Pop/',
  './datasets/FVC/') |> 
  map(sample_value) |> 
  list_cbind() |> 
  set_names(c('PreMaxC','PreMinC','PreSumC',
              'TmpMaxC','TmpMinC','TmpAvgC',
              'NtlC','PdC','FVCC')) -> ds

extractTopography = function(x){
  return(terra::extract(rast(x),
                 sample.point) |> 
    pull() |> 
    as_tibble()) 
}

list.files('./datasets/Topography/','.tif$',
           full.names = TRUE) |> 
  map(extractTopography) |> 
  list_cbind() |> 
  set_names(c('Aspect','Elev','Slope'))|> 
  bind_cols(ds) -> ds

# reclassify aspect 
# (1) shady slopes (0◦ to 45◦ or 315◦ to 360◦)
# (2) semi-shady slopes (45◦ to 135◦)
# (3) flat slopes (−1)
# (4) semi-positive slopes (225◦ to 315◦)
# (5) positive slopes (135◦ to 225◦)
ds |> 
  mutate(Aspect = case_when(between(Aspect,0,45) | between(Aspect,315,360) ~  'Shady_Slopes',
                            between(Aspect,45,135) ~  'Semi_shady_Slopes',
                            between(Aspect,225,315) ~  'Semi_positive_Slopes',
                            between(Aspect,135,225) ~  'Positive_Slopes')) -> ds

# LuC
lc2001 = rast('./datasets/LC/LC2001.tif')
lc2020 = rast('./datasets/LC/LC2020.tif')

terra::extract(lc2001,
               sample.point) |> 
  pull() -> ds$LC2001
terra::extract(lc2020,
               sample.point) |> 
  pull() -> ds$LC2020

ds |> 
  mutate(lc = LC2020 - LC2001) |> 
  mutate(LuC = if_else(lc == 0,'LC_UnChange',str_c('LC',LC2001,'_',LC2020))) |> 
  select(-c(LC2001,LC2020,lc)) |> 
  relocate(LuC,.before = FVCC) -> ds

# Use rf to get a weight to convert the aspect and landuse category from string to double
## 1st, Use one-hot encoding to process the Aspect and Luc
model.matrix(~ ds$Aspect - 1) |> 
  as_tibble() -> aspect
aspect |> 
  set_names(names(aspect) |> 
              str_sub(10,-1)) -> aspect
  
model.matrix(~ ds$LuC - 1) |> 
  as_tibble() -> luc
luc |> 
  set_names(names(luc) |> 
              str_sub(7,-1)) -> luc

ds |> 
  select(-c(Aspect,LuC)) |> 
  mutate(across(where(is.numeric),
                ~scales::rescale(.x))) |> 
  bind_cols(aspect,luc) |> 
  relocate(FVCC,-1) -> dt

## 2nd, Build a RF model to get the feature importance
library(mlr3verse)

task = as_task_regr(dt, target = 'FVCC')
learner = lrn("regr.ranger", importance = "impurity")
rr = resample(task, learner, rsmp("cv", folds = 10), store_models = TRUE)
impt = purrr::map_dfr(rr$learners, ~ .x$importance()) |> purrr::map_dbl(mean)

impt |> 
  as_tibble_row() |> 
  writexl::write_xlsx('./datasets/FeatureWeight.xlsx')

impt = readxl::read_xlsx('./datasets/FeatureWeight.xlsx') |> as_vector()

getFeatureValues = function(featureName) return(impt[featureName])

ds |> 
  mutate(Aspect = getFeatureValues(Aspect),
         LuC = getFeatureValues(LuC)) -> ds

## 3nd,scale the weight from the RF Feature Importance to (0,1)
ds |> 
  select(Aspect,LuC) |> 
  mutate(across(where(is.numeric),
                ~scales::rescale(.x))) |> 
  bind_cols(ds |> select(-c(Aspect,LuC))) |> 
  relocate(LuC,.before = FVCC) -> ds

# Add the Geographical Location 
sample.point |> 
  st_as_sf() |> 
  st_coordinates() -> ds[c('Long','Lat')]

ds |> 
  write_csv('./datasets/ds.csv')

# Based on the 2001-2020 vegetation change trend zone,We use GRTS to spatial 
# sampling the data to build the SEM model

ds = read_csv('./datasets/ds.csv') |> 
  mutate(across(Aspect:FVCC,~scales::rescale(.x))) |> 
  st_as_sf(coords = c('Long','Lat'),
           crs = 'EPSG:4326')

# fvc_trend值含义：
# -2严重退化
# -1轻微退化
# 0稳定不变
# 1轻微改善
# 2明显改善

rast('./result/mk_test/fvc_trend.tif') |> 
  terra::extract(vect(ds),na.rm=T) |> 
  as_tibble() |> 
  pull(`z-statistic`) -> ds$FVCC_Zone

ds_value = -2:2
ds_name = c('严重退化','轻微退化','稳定不变','轻微改善','明显改善')

unlink('./datasets/zone/',recursive = TRUE)
dir.create('./datasets/zone/')

set.seed(42)

sem.sample = function(zone){
  ds |> 
    dplyr::filter(FVCC_Zone == zone) |> 
    spsurvey::grts(n_base = 120,projcrs_check = FALSE) -> dt
  
  dt$sites_base |> 
    st_drop_geometry() |> 
    as_tibble() |> 
    select(Aspect:FVCC) -> sample_result
    #select(Aspect:FVCC,X,Y) |> 
    #rename(Long = X,Lat = Y) |> 
    # mutate(across(where(is.numeric),
    #               ~scale(.x)[,1]))
  return(sample_result)
}

seq(1,5) |> 
  map(\(i) sem.sample(ds_value[i]) |> 
        write_csv(str_c('./datasets/zone/',
                        ds_name[i],'.csv')))