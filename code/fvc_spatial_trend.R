#########################
# 1. Preprocessing data #
#########################

library(sf)
library(stars)
library(terra)
library(trend)
library(dplyr)

list.files('./datasets/FVC/',pattern = ".tif$") %>%  
  paste0('./datasets/FVC/',.)-> tif.file

fun_sen = function(x){
  if(length(na.omit(x))<20) return(c(NA, NA, NA))   
  MK_estimate = trend::sens.slope(
    ts(na.omit(x),start=2001,end = 2020,frequency=1)
    ) 
  slope = MK_estimate$estimates
  MK_test = MK_estimate$p.value
  Zs = MK_estimate$statistic
  return(c(slope,MK_test,Zs))
}

fvc_sen = app(rast(tif.file),fun_sen,cores = 6) 
names(fvc_sen) = c("sen-slope","p-value","z-statistic")
plot(fvc_sen)

writeRaster(fvc_sen,
            filename = "./result/mk_test/fvc_sen.tif",
            overwrite = T)

fvc.sen = rast('./result/mk_test/fvc_sen.tif')


max.slope = global(fvc.sen[[1]],'max',na.rm = T)[1,1]
min.slope = global(fvc.sen[[1]],'min',na.rm = T)[1,1]

# slope <= -0.0005 植被退化,重分类为-1
# -0.0005 <= slope <= 0.0005 植被生长稳定,重分类为0
# slope >= 0.0005 植被改善,重分类为1
fvc.sen$`sen-slope` %>%
  classify(.,rbind(c(min.slope,-0.0005,-1),
                   c(-0.0005,0.0005,0),
                   c(0.0005,max.slope,1)),
           include.lowest=T) -> new.slope

# 取出最大和最小Z值
max.z = global(fvc.sen[[3]],'max',na.rm = T)[1,1]
min.z = global(fvc.sen[[3]],'min',na.rm = T)[1,1]

# 对Z值进行重分类，确定显著性
# # |Z| <= 1.96 未通过95%置信度检验，不显著
# # |Z| >= 1.96 通过95%置信度检验，显著
fvc.sen$`z-statistic` %>%
  classify(.,rbind(c(min.z,-1.96,2),
                   c(1.96,max.z,2),
                   c(-1.96,1.96,1)),
           include.lowest=T) -> new.z

# 将Slope和Z值计算结果相乘，最后得到趋势变化划分
fvc.trend = new.z * new.slope
plot(fvc.trend)

# fvc.trend值含义：
# -2严重退化
# -1轻微退化
# 0稳定不变
# 1轻微改善
# 2明显改善

weihe = vect('./datasets/渭河流域边界/')
fvc.trend %>% 
  crop(weihe,mask=T) -> fvc.trend

writeRaster(fvc.trend,'./result/mk_test/fvc_trend.tif',overwrite = T)
fvc.trend = rast('./result/mk_test/fvc_trend.tif')

global(fvc.trend, fun="isNA")

fvc.trend |> 
  as.matrix() |> 
  na.omit() |> 
  table() |> 
  prop.table()

'
        -2         -1          0          1          2 
0.04576890 0.08737156 0.09114927 0.31066905 0.46504121 
'
# 下面的方式更优雅:
# cellSize(fvc.trend,unit='km') |> 
#   global(fun = 'mean') -> pixelarea
# pixelarea = pixelarea[1,1]
# 
# fvc.trend |> 
#   freq() |> 
#   as_tibble() |> 
#   select(-1) |> 
#   mutate(count = pixelarea * count / 1e4) |> 
#   mutate(value = case_when(
#     value == -2 ~ '严重退化',
#     value == -1 ~ '轻微退化',
#     value ==  0 ~ '稳定不变',
#     value ==  1 ~ '轻微改善',
#     value ==  2 ~ '明显改善'
#   )) |> 
#   set_names(c('植被变化类型','面积(万km2)')) |> 
#   mutate(`比例(%)` = `面积(万km2)` / sum(`面积(万km2)`) * 100)
'
# A tibble: 5 × 3
  植被变化类型 `面积(万km2)` `比例(%)`
  <chr>                <dbl>     <dbl>
1 严重退化             0.622      4.58
2 轻微退化             1.19       8.74
3 稳定不变             1.24       9.11
4 轻微改善             4.22      31.1 
5 明显改善             6.31      46.5 
'

##################################
# 2. Plot the spatial trend of FVC 
##################################

library(sf)
library(terra)
library(tmap)

fvc.trend = rast('./result/mk_test/fvc_trend.tif')
weihe = vect('./datasets/渭河流域边界')
vect('./datasets/省市县-湖泊/渭河流域地级市region.shp') %>% 
  crop(weihe) %>% 
  st_as_sf() -> city

city %>% 
  dplyr::filter(!NAME %in% (c('汉中市','安康市',
                              '中卫市','甘南藏族自治州',
                              '商洛市','陇南市',
                              '白银市','吴忠市'))) -> city.main

citypoint = tibble::tibble(NAME = city.main$NAME)

city.main %>% 
  st_transform(32648) %>% 
  st_geometry() %>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  st_coordinates() -> citypoint[c('long','lat')]

citypoint %>% 
  st_as_sf(coords = c('long','lat'),
           crs='epsg:4326') -> citypoint

citypoint$name = c('GuYuan','TianShui','DingXi','PingLiang','QingYang','XianYang',
                   'BaoJi',"Yan'an",'YuLin','WeiNan',"Xi'an",'TongChuan')

weihe = st_as_sf(weihe)

tm_shape(fvc.trend,bbox = c(103.9,33.6,110.3,37.5)) + 
  tm_raster(style = 'cat',n = 5,
            title = "FVC Variation Trend",
            labels = c('Severely Decreased',
                       'Slightly Decreased',
                       'Stably Unchanged',
                       'Slightly Increased',
                       'Significantly Increased'),
            palette = c('#d7191c',
                        '#fdae61',
                        '#ffffbf',
                        '#abdda4',
                        '#2b83ba')) +
  tm_shape(weihe) + 
  tm_borders() + 
  tm_shape(city) +
  tm_borders(lwd = 1) + 
  tm_shape(citypoint) +
  tm_dots(size = 0.02, shape = 20) +
  tm_text("name",size = .65,
          just = c("left", "bottom"),) + 
  tm_compass(type = 'arrow', size = 1.5,
             position = c("right", "top")) +
  tm_scale_bar(position = c('left','bottom'),
               breaks = c(0,50,100),
               lwd = 1.5, text.size = 0.5) +
  tm_add_legend(title = "City Boundary",type = 'line',
                col = 'grey',lwd = 1.5) +
  tm_add_legend(type = "symbol", col = "black",
                title = "City", size = 0.15) +
  tm_layout(legend.position = c(0.02,0.5),
            legend.text.size = .65,
            legend.title.size = .85,
            frame = F,
            fontfamily = 'serif') -> fig

tmap_save(fig,'./figure/spatial_trend.jpg',dpi = 300,
          width = 5,height = 3.5)
tmap_save(fig,'./figure/spatial_trend.pdf',
          width = 5,height = 3.5)

##################################################
# 3. Combine the temporal and spatial trend of FVC
##################################################

library(figpatch)
library(ggplot2)
library(cowplot)

img1 = fig('./figure/temporal_trend.jpg')
img2 = fig('./figure/spatial_trend.jpg')

plot_grid(img1,img2,nrow = 1,label_fontfamily = 'serif',
          labels = paste0('(',letters[1:2],')'),
          label_fontface = 'plain',label_size = 10,
          hjust = -1.5,align = 'hv')  -> p

ggsave(filename = './figure/fvc_trend.pdf',plot = p,
       width = 10, height = 3.5,bg = 'transparent',device = cairo_pdf)
pdftools::pdf_convert('./figure/fvc_trend.pdf',dpi = 300,
                      filenames = './figure/fvc_trend.jpg')