library(terra)
library(stars)
library(tidyverse)

lc2001 = rast('./datasets/LC/LC2001.tif')
names(lc2001) = 'LC2001'
lc2020 = rast('./datasets/LC/LC2020.tif')
names(lc2020) = 'LC2020'

LC_Type = c(
  'Evergreen Needleleaf Forests','Evergreen Broadleaf Forests','Deciduous Needleleaf Forests','Deciduous Broadleaf Forests',
  'Mixed Forests','Closed Shrublands','Open Shrublands','Woody Savannas','Savannas','Grasslands','Permanent Wetlands','Croplands',
  'Urban and Built-up Lands','Cropland','Permanent Snow and Ice','Barren','Water Bodies'
)

LC_Type_Color_Table = c(
  '#05450a', #	Evergreen Needleleaf Forests: dominated by evergreen conifer trees (canopy >2m). Tree cover >60%.
  '#086a10', #	Evergreen Broadleaf Forests: dominated by evergreen broadleaf and palmate trees (canopy >2m). Tree cover >60%.
  '#54a708', #	Deciduous Needleleaf Forests: dominated by deciduous needleleaf (larch) trees (canopy >2m). Tree cover >60%.
  '#78d203', #	Deciduous Broadleaf Forests: dominated by deciduous broadleaf trees (canopy >2m). Tree cover >60%.
  '#009900', #	Mixed Forests: dominated by neither deciduous nor evergreen (40-60% of each) tree type (canopy >2m). Tree cover >60%.
  '#c6b044', #	Closed Shrublands: dominated by woody perennials (1-2m height) >60% cover.
  '#dcd159', #	Open Shrublands: dominated by woody perennials (1-2m height) 10-60% cover.
  '#dade48', #	Woody Savannas: tree cover 30-60% (canopy >2m).
  '#fbff13', #	Savannas: tree cover 10-30% (canopy >2m).
  '#b6ff05', #	Grasslands: dominated by herbaceous annuals (<2m).
  '#27ff87', #	Permanent Wetlands: permanently inundated lands with 30-60% water cover and >10% vegetated cover.
  '#c24f44', #	Croplands: at least 60% of area is cultivated cropland.
  '#a5a5a5', #	Urban and Built-up Lands: at least 30% impervious surface area including building materials, asphalt and vehicles.
  '#ff6d4c', #	Cropland/Natural Vegetation Mosaics: mosaics of small-scale cultivation 40-60% with natural tree, shrub, or herbaceous vegetation.
  '#69fff8', #	Permanent Snow and Ice: at least 60% of area is covered by snow and ice for at least 10 months of the year.
  '#f9ffa4', #	Barren: at least 60% of area is non-vegetated barren (sand, rock, soil) areas with less than 10% vegetation.
  '#1c0dff'  #  Water Bodies: at least 60% of area is covered by permanent water bodies.
)

ggplot() +
  geom_stars(data = st_as_stars(lc2020),aes(fill=factor(LC2020.tif))) +
  scale_fill_manual(name = "Category",
                    na.value = NA,
                    values = LC_Type_Color_Table,
                    labels = LC_Type) +
  coord_sf(crs = crs(lc2020))+
  theme_bw()+
  theme(plot.background = element_blank(),
        panel.grid = element_blank())

# https://www.statology.org/r-convert-table-to-matrix/#:~:text=You%20can%20use%20the%20following%20basic%20syntax%20to,my_matrix%20%3C-%20matrix%20%28my_table%2C%20ncol%3Dncol%20%28my_table%29%2C%20dimnames%3Ddimnames%20%28my_table%29%29
# way1
crosstab(c(lc2001,lc2020)) %>% 
  matrix(ncol=ncol(.),
         dimnames = list(rownames(.), colnames(.)))

# way2
crosstab(c(lc2001,lc2020),long=TRUE) |> 
  as_tibble() |> 
  mutate(LC2001 = as.character(LC2001),
         LC2020 = as.character(LC2020)) |> 
  pivot_wider(names_from = 'LC2020',
              values_from = n) |>
  mutate(across(where(is.integer),~replace_na(.x,0)))-> LUC

sort(colnames(LUC)[2:15]) == sort(LUC$LC2001)

LUC |> 
  select(LC2001,LUC$LC2001) |> 
  View()

crosstab(c(lc2001,lc2020),long=TRUE) |> 
  as_tibble() |> 
  mutate(LC2001 = as.character(LC2001),
         LC2020 = as.character(LC2020)) |> 
  pivot_wider(names_from = 'LC2020',
              values_from = n) |>
  mutate(across(where(is.integer),~replace_na(.x,0))) |> 
  select(LC2001,LUC$LC2001) -> LUC