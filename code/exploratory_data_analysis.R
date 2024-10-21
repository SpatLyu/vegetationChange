library(sf)
library(tidyverse)

ds = read_csv('./datasets/ds.csv') |> 
  select(Aspect:FVCC)

# 绘制相关性变量图

ds %>% 
  cor() %>% 
  round(2) -> corr


# 相关性热力图
library(ggcorrplot)

ggcorrplot(corr, 
           method = "circle",
           type = "upper") %>%  
  ggsave("./figure/corr.png",
         ., 
         width = 7, height = 6,
         bg = 'white',
         dpi=600)

indepent_var = names(ds)[-13]
repose_var = "FVCC"
paste(indepent_var,collapse = "+") %>%
  paste(repose_var,'~',.) -> lm.model 

lm(lm.model,data=ds) |> 
  summary()

lm(lm.model,data=ds) |> 
  olsrr::ols_vif_tol()
'
   Variables Tolerance      VIF
1     Aspect 0.9745401 1.026125
2       Elev 0.3866526 2.586301
3      Slope 0.7446650 1.342886
4    PreMaxC 0.8340643 1.198948
5    PreMinC 0.6384673 1.566251
6    PreSumC 0.6633241 1.507559
7    TmpMaxC 0.3511354 2.847904
8    TmpMinC 0.1965434 5.087934
9    TmpAvgC 0.1850781 5.403124
10      NtlC 0.8906134 1.122822
11       PdC 0.9846225 1.015618
12       LuC 0.9459095 1.057184
'

# 拟合散点图 + 一次拟合曲线
library(ggpmisc)

# 参考https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

plot_line = function(x,ds,xlabel,ylabel){
  ggplot(data = ds, aes(x = .data[[x]], y = FVCC)) +
    geom_point(size=0.15,color='grey') +
    geom_smooth(method = "lm", 
                se = FALSE, 
                formula = y ~ x,
                color='#b10026') +
    stat_poly_eq(aes(
      label = paste0("atop(", after_stat(eq.label), 
                     ",", 
                     after_stat(rr.label), ")")), 
      formula = y ~ x, 
      label.x = xlabel, 
      label.y = ylabel,
      parse = TRUE)+
      theme_bw() +
      theme(panel.grid=element_blank()) -> line.plot
  
  return(line.plot)
}


# 组合图
library(patchwork)

varname = names(ds)[1:11]

plot_line(varname[1],ds,"right","bottom") -> p1
for (i in 2:11){
  if (i %in% c(5,8,9)){
    p1+plot_line(varname[i],ds,"left","top") -> p1
  } else if (i %in% 2:4){
    p1+plot_line(varname[i],ds,"right","bottom") -> p1
  } else {
    p1+plot_line(varname[i],ds,"right","top") -> p1
  }
}

p1 +
  plot_layout(nrow=3,ncol=4, 
              widths = 3, 
              heights = 3) ->  fig

ggsave("./figure/lm.png",
       fig, 
       width = 12, height = 9,
       dpi=600)