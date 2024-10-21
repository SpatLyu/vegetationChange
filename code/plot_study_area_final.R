library(sf)
library(terra)
library(scales)
library(tidyverse)
library(ggspatial)
library(tidyterra)
library(cowplot)

#--------------------------------------------------
# 1. The relative postition of Weihe Basin in China
#--------------------------------------------------

albers = "+proj=aea +lat_1=25 +lat_2=47 +lat_0=0 +lon_0=110 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

province = read_sf('./datasets/province.geojson')
nineline = read_sf('./datasets/nineline.geojson')
weihe = read_sf('./datasets/渭河流域边界/')
weihe |> 
  st_centroid() |> 
  st_coordinates() -> center
lon = center[1]
lat = center[2]
ortho= paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon,
              ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
circle = suppressMessages(
  sf::st_point(x = c(0, 0)) %>%
    sf::st_buffer(dist = 3.8e6) %>%
    sf::st_sfc(crs = ortho)) 
world = rnaturalearth::ne_coastline(scale = 110, returnclass = "sf") |> 
  st_transform(st_crs(ortho)) |> 
  st_intersection(circle)

ggplot() +
  geom_sf(data = circle,colour="black",fill="grey95") +
  geom_sf(data = world,fill="grey80",colour="grey80") +
  geom_sf(data = province,fill="white",size=.1,color="grey") + 
  geom_sf(data = nineline,size=1.2,color="#9d98b7") +
  geom_sf(data=weihe, fill = "#E31A1C",alpha = 0.5) +
  ggfx::with_outer_glow(geom_sf(data = nineline,
                                lwd = .3,
                                color = 'black'),
                        colour = "#9d98b7",
                        sigma = 5,
                        expand = 5) +
  coord_sf(crs = ortho) +
  theme_void() -> cn

ggsave(filename = './figure/study_area/cn.jpg',plot = cn,
       dpi = 600,bg = 'transparent',width = 3.5,height = 3.4)

#----------------------------------------
# 2. The absolute position of Weihe Basin
#----------------------------------------
weihe = vect('./datasets/渭河流域边界/')
rast('./datasets/Topography_1000m/elev.tif') %>% 
  mask(weihe) -> elev
slope = terrain(elev, "slope", unit = "radians")
aspect = terrain(elev, "aspect", unit = "radians")
hill = shade(slope, aspect, 30, 270)
names(hill) = "shades"
pal_greys = hcl.colors(1000, "Grays")
index = hill %>%
  mutate(index_col = scales::rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_cols = pal_greys[index]

hill_plot = ggplot() +
  geom_spatraster(
    data = hill, fill = vector_cols,
    alpha = 1
  )
hill_plot

elev_plot = hill_plot +
  geom_spatraster(data = elev) +
  scale_fill_hypso_tint_c(
    palette = "dem_poster",
    alpha = 0.5,
    labels = label_comma(),
    breaks = c(327,3810)) +
  labs(fill = "Elevation(m)")
elev_plot 

river1 = read_sf('./datasets/weihebasin_rivers/river3.shp') %>% 
  st_geometry() %>% 
  st_union() %>%
  st_sfc(crs=4326) %>% 
  st_sf(name='river-1j',
        geometry=.)

library(pinyin)
vect('./datasets/省市县-湖泊/渭河流域地级市region.shp') %>% 
  crop(weihe)  |>  
  st_as_sf() |> 
  mutate(NAME = str_sub(NAME,1,2)) |> 
  mutate(name = py(NAME, 
                   dic = pydic(dic = c("pinyin2"))) 
         %>% str_remove_all("\\d{1,}")) |> 
  mutate(name = str_replace(name, "_", " ")|>
           str_to_title() |>
           str_replace(' ',''))-> city
city_boundry = city |> 
  st_make_valid() |> 
  st_geometry() |> 
  st_boundary() |> 
  st_union() |> 
  st_sfc(crs=4326) %>% 
  st_sf(name='city-boundry',
        geometry=.)

elev_plot +
  geom_sf(data = city_boundry,aes(color = 'city_boundry'),
          lwd = 0.35,alpha = .9) +
  geom_sf(data = river1,aes(color = 'river1'),lwd = 1) +
  geom_point(aes(geometry = geometry, color = 'city'),stat = "sf_coordinates",
             data = city,shape = 21,size = 0.8,fill = "black") +
  scale_color_manual(name = '',
                     breaks = c('river1','city_boundry','city'),
                     values = c('river1' = "#045a8d",
                                'city_boundry' = "grey10",
                                'city' = 'black'),
                     labels = c('Weihe River','City Boundary','City')) +
  ggrepel::geom_text_repel(aes(label=name,geometry=geometry),
                           data = city,size = 3.5,
                           color = "black",stat = "sf_coordinates",
                           bg.colour = "white", bg.r = .2) +
  annotation_scale(location = "bl",text_cex = 0.8) +
  annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering(
    fill = c("grey40", "white"), line_col = "grey20")) +
  coord_sf(ylim = c(33.685,37.415),
           xlim = c(103.965,110.280),
           expand = FALSE) +
  guides(
    fill = guide_colorbar(order = 1,
                          theme = theme(legend.key.height = unit(25,'pt'))),
    color = guide_legend(order = 0,
                         theme = theme(legend.key.height = unit(10,'pt')))
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 7.5,family = "Times"),
    axis.ticks = element_line(linetype = 1,linewidth = 0.25),
    legend.title = element_text(colour = "black",size = 10,family = "Times"),
    legend.text = element_text(size = 8,family = "Times"),
    legend.position = c(0.05,0.95),
    legend.justification = c(0,1),
    legend.spacing.y = unit(-0.5,'cm'),
    panel.grid = element_blank(),
    panel.background = element_rect("white", color = NA),
    panel.border = element_rect(fill = NA,linetype = 1,linewidth = 0.5),
    plot.margin = unit(c(0,0,0,0),"cm")) -> study.weihe
ggsave(filename = './figure/study_area/weihe.jpg',plot = study.weihe,
       dpi = 600,bg = 'transparent',width = 6.5,height = 5)

#-------------------------
# 3. Arrange the two plots
#-------------------------
library(figpatch)
a = fig('./figure/study_area/cn.jpg')
b = fig('./figure/study_area/weihe.jpg')

ggdraw() +
  draw_plot(a,0,0,.4,1,scale = 1) +
  draw_plot(b,.4,0,.6,1,scale = 1) +
  geom_segment(aes(x = 0.199,y = 0.518,
                   xend = 0.44,yend = 0.854),
               lwd = .25) +
  geom_segment(aes(x = 0.201,y = 0.485,
                   xend = 0.44,yend = 0.174),
               lwd = .25) -> fig
ggsave(filename = './figure/study_area/studyarea.pdf',plot = fig,
       width = 8.5, height = 5,bg = 'transparent',device = cairo_pdf)
pdftools::pdf_convert('./figure/study_area/studyarea.pdf',dpi = 300,
                      filenames = './figure/study_area/studyarea.jpg')

library(magick)
image_read('./figure/study_area/studyarea.jpg') |> 
  image_crop("2500x1100+50+200") |> 
  image_write('./figure/study_area/studyarea.jpg')