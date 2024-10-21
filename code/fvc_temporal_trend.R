# library(terra)
# library(tidyverse)
# 
# list.files('./datasets/FVC/',pattern = ".tif$") %>%  
#   paste0('./datasets/FVC/',.) -> tif.file
# 
# weihe = vect('./datasets/渭河流域边界/')
# 
# for (t in tif.file){
#   crop(rast(t),weihe,mask = T) |> 
#     writeRaster(str_c('./datasets/fvc_wh/',str_sub(t,-8,-5),'.tif'))
# }
# 
# list.files('./datasets/fvc_wh/',pattern = ".tif$") %>%  
#   paste0('./datasets/fvc_wh/',.) -> tif.file
# 
# fvc_value = tibble(year = 2001:2020)
# 
# fvc = rast(tif.file)
# fvc_value$value = global(fvc,mean,na.rm = T) %>% pull()
# 
# trend::mk.test(fvc_value$value)
# u = pheno::seqMK(fvc_value$value)
# uf = u$prog
# ub = u$retr
# 
# tibble(year = 2001:2020,
#        uf = uf,
#        ub = ub,
#        fvc = fvc_value$value) |> 
#   writexl::write_xlsx('./result/mk_test/fvc_temporal_trend.xlsx')

library(tidyverse)
fvc_value = readxl::read_xlsx('./result/mk_test/fvc_temporal_trend.xlsx')

trend::mk.test(fvc_value$fvc)
pheno::seqMK(fvc_value$fvc)

ggplot(data = fvc_value,aes(x = year)) +
  geom_line(aes(y = uf, color = "UF"), linewidth = 1.25) +
  geom_line(aes(y = ub, color = "UB"), linewidth = 1.25,
            linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed", 
             color = "black", linewidth = .5) +
  geom_hline(yintercept = 1.96, linetype = "dotted", 
             color = "black", linewidth = .35) +
  geom_hline(yintercept = -1.96, linetype = "dotted", 
             color = "black", linewidth = .35) +
  scale_x_continuous(breaks = seq(2000,2020,by = 5)) +
  scale_color_manual(name = '',breaks = c('UF','UB'),
                     labels = c('UF','UB'),
                     values = c("UF" = "#1b805e", "UB" = "red")) +
  labs(x = NULL,y = NULL) +
  guides(color = guide_legend(override.aes = list(linewidth = .45))) +
  theme_bw() +
  theme(panel.grid=element_blank(),
        plot.margin = margin(rep(4.5,4)),
        legend.position = 'inside',
        legend.position.inside = c(.15, .75),
        axis.text = element_text(size = 15,family = 'serif'),
        axis.title = element_text(size = 15,family = 'serif'),
        legend.text = element_text(family = 'serif',size = 13.5),
        legend.background = element_blank(),
        legend.key = element_blank()) -> fig.seqmk
ggsave(filename = './figure/seqmk.jpg',plot = fig.seqmk,
       dpi = 600,bg = 'transparent',width = 3.5,height = 2.5)

ggplot(aes(x = year,y = fvc),data = fvc_value) +
  geom_line(aes(color = "FVC Time Series", 
                linetype = "FVC Time Series"), 
            linewidth = .95) + 
  scale_x_continuous(name = "Year",expand = c(0,0),
                     limits = c(2001,2021),
                     breaks = seq(2001,2021,by = 2))+
  scale_y_continuous(name = "FVC",expand = c(0,0),
                     limits = c(0.44,0.53),
                     breaks = seq(0.44,0.54,by = 0.01)) +
  geom_smooth(aes(color = 'Trend Line Total', 
                  linetype = 'Trend Line Total'),
              method = 'lm',se = F,linewidth = .65) +
  scale_color_manual(name = '',
                     breaks = c('FVC Time Series','Trend Line Total'),
                     values = c('FVC Time Series' = '#df0f85',
                                'Trend Line Total' = '#407eb2'),
                     labels = c('FVC Time Series','Linear Trend Line')) +
  scale_linetype_manual(name = '',
                        breaks = c('FVC Time Series','Trend Line Total'),
                        values = c('FVC Time Series' = 1,
                                   'Trend Line Total' = 2),
                        labels = c('FVC Time Series','Linear Trend Line')) +
  guides(linetype = guide_legend(override.aes = list(linewidth = .45))) +
  annotate("text", x = 2009, y = 0.445, family = 'serif',
           parse = T, size = 5,color = '#df0f85',
           label = latex2exp::TeX("$R^2=0.4316,P<0.01$")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.margin = margin(rep(4.5,4)),
        legend.position = "inside", 
        legend.position.inside = c(.8, .2),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.width = unit(.85,'cm'),
        axis.text = element_text(size = 10,family = 'serif'),
        axis.title = element_text(size = 12.5,family = 'serif'),
        legend.text = element_text(family = 'serif',size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(family = 'serif')) -> fig
ggsave(filename = './figure/line_trend.jpg',plot = fig,
       dpi = 600,bg = 'transparent',width = 5,height = 3.5)

library(figpatch)
library(cowplot)

a = fig('./figure/line_trend.jpg')
b = fig('./figure/seqmk.jpg')

ggdraw() +
  draw_plot(a,0,0,1,1,scale = 1) +
  draw_plot(b,-0.06,0.42,.7,.7,scale = .5) +
  theme(plot.margin = margin(rep(1,4))) -> fig.lt
ggsave(filename = './figure/temporal_trend.pdf',plot = fig.lt,
       width = 5, height = 3.5,bg = 'transparent',device = cairo_pdf)
pdftools::pdf_convert('./figure/temporal_trend.pdf',dpi = 300,
                      filenames = './figure/temporal_trend.jpg')