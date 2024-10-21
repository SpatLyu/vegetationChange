library(tidyverse)
library(scales)

ds = read_csv('./datasets/ds.csv') |> 
  select(-c(Long,Lat))
shap_interaction = read_csv('./result/xgboost_shap/shap_interaction_values.csv')
shap_independent = read_csv('./result/xgboost_shap/shap_independent_values.csv')

# tibble(name=names(shap_independent),
#        value=shap_independent |> abs() |> colMeans()) |> 
#   arrange(desc(value)) |> 
#   ggplot(aes(x=reorder(name,value),y=value))+
#   geom_col(fill = '#f99e17')+
#   geom_text(aes(label=format(value,scientific = TRUE,digits = 2),
#                 y = value + 3e-05),
#             color='#6c196f',
#             vjust = 0.5,
#             size=3)+
#   xlab('')+
#   scale_y_continuous(name = 'mean(|SHAP Independent Value|)',
#                      limits = c(0,0.00055),
#                      breaks = seq(0,0.0005,by=0.0001),
#                      expand=c(0,0))+
#   coord_flip()+
#   theme_bw()+
#   theme(panel.border = element_blank(),
#         axis.line.x = element_line(),
#         axis.line.y = element_line(),
#         panel.grid = element_blank()) -> fig.independent
#   ggsave('./figure/xgboost_shap/Independent-feature-SHAP-value-order.jpg',
#          fig.independent,
#          width =6, height = 4,
#          dpi=600)
#   
# tibble(name=names(shap_interaction),
#        value=shap_interaction |> abs() |> colMeans()) |> 
#     arrange(desc(value)) |> 
#     ggplot(aes(x=reorder(name,value),y=value))+
#   geom_col(fill = '#f99e17')+
#   geom_text(aes(label=format(value,scientific = TRUE,digits = 2),
#                 y = value + 3e-05),
#             color='#6c196f',
#             vjust = 0.5,
#             size=3)+
#     xlab('')+
#     scale_y_continuous(name = 'mean(|SHAP Interaction Value|)',
#                        limits = c(0,0.00055),
#                        breaks = seq(0,0.0005,by=0.0001),
#                        expand=c(0,0))+
#     coord_flip()+
#     theme_bw()+
#     theme(panel.border = element_blank(),
#           axis.line.x = element_line(),
#           axis.line.y = element_line(),
#           panel.grid = element_blank()) -> fig.interaction
#   ggsave('./figure/xgboost_shap/Interaction-feature-SHAP-value-order.jpg',
#          fig.interaction,
#          width =6, height = 4,
#          dpi=600)
 
shap.plot.summary = function(data_long, x_bound = NULL, dilute = FALSE,
                             min_color_bound = "#FFCC33", max_color_bound = "#6600CC", 
                             kind = c("sina", "bar")) {
    kind <- match.arg(kind)
    if (kind == "bar") {
      imp <- shap.importance(data_long)
      p <- ggplot(imp, aes(x = variable, y = mean_abs_shap)) + 
        geom_bar(stat = "identity", fill = max_color_bound) + 
        coord_flip() + scale_x_discrete(limits = rev(levels(imp[["variable"]]))) + 
        theme_bw() + theme(axis.title.x = element_text(size = 10)) + 
        labs(x = element_blank(), y = "Avg(|SHAP|)")
      return(p)
    }
    N_features <- data.table::setDT(data_long)[, data.table::uniqueN(variable)]
    if (is.null(dilute)) 
      dilute = FALSE
    nrow_X <- nrow(data_long)/N_features
    if (dilute != 0) {
      dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute))))
      set.seed(1234)
      data_long <- data_long[sample(nrow(data_long), min(nrow(data_long)/dilute, 
                                                         nrow(data_long)/2))]
    }
    x_bound <- if (is.null(x_bound)) 
      max(abs(data_long$value)) * 1.1
    else as.numeric(abs(x_bound))
    plot1 = ggplot(data = data_long) + 
      coord_flip(ylim = c(-x_bound, x_bound)) + 
      geom_hline(yintercept = 0) + 
      ggforce::geom_sina(aes(x = variable,y = value, color = stdfvalue), 
                             method = "counts", maxwidth = 0.7,alpha = 0.7) + 
      geom_text(data = unique(data_long[, c("variable", "mean_value")]), 
                aes(x = variable, y = x_bound * 0.9, 
                    label = format(mean_value,scientific = TRUE,digits = 4)),
                    family = 'serif', fontface = "bold", size = 4) + 
      scale_color_gradient(low = min_color_bound,
                           high = max_color_bound, 
                           breaks = c(0, 1), labels = c(" Low","High "), 
                           guide = guide_colorbar(barwidth = 12, barheight = 0.3)) + 
      theme_bw() + 
      theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "bottom", 
            legend.title = element_text(size = 12), axis.text = element_text(size = 10),
            legend.text = element_text(size = 10), axis.title.x = element_text(size = 12)) + 
      scale_x_discrete(limits = rev(levels(data_long$variable)), 
                       labels = rev(levels(data_long$variable))) + 
      labs(y = "SHAP value (impact on model output)", x = "", 
           color = "Feature value  ")
    return(plot1)
  }
    
shap.plot = function(shap_score, X, top_n, dilute = TRUE) 
  {
    if (missing(top_n)) 
      top_n = dim(X)[2]
    if (!top_n %in% c(1:dim(X)[2])) 
      stop("supply correct top_n")
    shap_long2 = SHAPforxgboost::shap.prep(shap_contrib = shap_score, 
                                           X_train = X, 
                                           top_n = top_n)
    shap.plot.summary(shap_long2,dilute = dilute)
  }
  
shap.plot(shap_score = shap_independent,
          X = ds |> select(-FVCC)) +
    ylab('SHAP Independent Value (impact on model output)') +
  theme(axis.text = element_text(family = 'serif', face = "bold"),
        axis.title = element_text(family = 'serif', face = "bold"))  -> independent.plot

independent.plot %>%
  ggsave("./figure/xgboost_shap/Independent-Feature-order.pdf",
         plot = .,width = 6, height = 5,device = cairo_pdf)
pdftools::pdf_convert("./figure/xgboost_shap/Independent-Feature-order.pdf",dpi = 300,
                      filenames = "./figure/xgboost_shap/Independent-Feature-order.jpg")

  
shap.plot(shap_score = shap_interaction,
          X=ds[,1:12]) +
    ylab('SHAP Interaction Value (impact on model output)') +
  theme(axis.text = element_text(family = 'serif', face = "bold"),
        axis.title = element_text(family = 'serif', face = "bold")) -> interaction.plot
  
interaction.plot %>% 
  ggsave("./figure/xgboost_shap/Interaction-Feature-order.pdf",
         plot = .,width = 6, height = 5,device = cairo_pdf)
pdftools::pdf_convert("./figure/xgboost_shap/Interaction-Feature-order.pdf",dpi = 300,
                      filenames = "./figure/xgboost_shap/Interaction-Feature-order.jpg")


# Feature Contribution Variation Curve

name_independent = tibble(name=names(shap_independent),
                          value=shap_independent |> abs() |> colMeans()) |> 
                   arrange(desc(value)) |> 
                   pull(name)

name_interaction = tibble(name=names(shap_interaction),
                          value=shap_interaction |> abs() |> colMeans()) |> 
                   arrange(desc(value)) |> 
                   pull(name)

plot_shap_independent = function(name){
  if (name %in% name_independent[seq(1,12,4)]){
    yaxisname = 'SHAP Independent Value'
  } else{
    yaxisname = ''
  }
  
  tibble(value = ds[[name]],
         shap = shap_independent[[name]]) |> 
    ggplot(aes(x = value, y = shap))+
    geom_point(size = 0.15,color = '#000000')+
    geom_hline(aes(yintercept=0),
               color='#3585bb',
               linewidth = 1,
               linetype = 2)+
    geom_smooth(color='#b10026')+
    xlab(name)+
    scale_y_continuous(name = yaxisname,
                       #limits = c(min(shap_independent),0.002)
                       )+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 13,
                                   family = 'serif', 
                                   face = "bold"),
          axis.title = element_text(size = 15,
                                    family = 'serif',
                                    face = "bold")) -> fig
  return(fig)
}

plot_shap_interaction = function(name){
  if (name %in% name_interaction[seq(1,12,4)]){
    yaxisname = 'SHAP Interaction Value'
  } else{
    yaxisname = ''
  }
  
  tibble(value = ds[[name]],
         shap = shap_interaction[[name]]) |> 
    ggplot(aes(x = value, y = shap))+
    geom_point(size = 0.15,color = '#000000')+
    geom_hline(aes(yintercept = 0),
               color = '#3585bb',
               linewidth = 1,
               linetype = 2)+
    geom_smooth(color = '#b10026')+
    xlab(name)+
    scale_y_continuous(name = yaxisname,
                       #limits = c(min(shap_interaction),0.0045)
                       )+
    theme_bw()+
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 13,
                                   family = 'serif', 
                                   face = "bold"),
          axis.title = element_text(size = 15,
                                    family = 'serif',
                                    face = "bold")) -> fig
  return(fig)
}

library(cowplot)

map(name_independent,plot_shap_independent) %>% 
  plot_grid(plotlist = .,nrow = 3,ncol = 4,
            rel_widths = 4, rel_heights = 3.1,
            labels = paste0('(',letters[2:13],')'), 
            #labels = "AUTO",
            label_x = rep(c(0.03,0,0,0),times = 3),
            label_size = 12
            ) -> independentCurve
ggsave("./figure/xgboost_shap/Independent-Feature-SHAP-Value.pdf",
       plot = independentCurve,width = 16, height = 9.3,device = cairo_pdf)
pdftools::pdf_convert("./figure/xgboost_shap/Independent-Feature-SHAP-Value.pdf",dpi = 300,
                      filenames = "./figure/xgboost_shap/Independent-Feature-SHAP-Value.jpg")

map(name_interaction,plot_shap_interaction) %>% 
  plot_grid(plotlist = .,nrow = 3,ncol = 4,
            rel_widths = 4,rel_heights = 3,
            labels = paste0('(',letters[2:13],')'), 
            #labels = "AUTO",
            label_x = 0,
            label_size = 12
  ) -> interactionCurve
ggsave("./figure/xgboost_shap/Interaction-Feature-SHAP-Value.pdf",
       plot = interactionCurve,width = 16, height = 9,device = cairo_pdf)
pdftools::pdf_convert("./figure/xgboost_shap/Interaction-Feature-SHAP-Value.pdf",dpi = 300,
                      filenames = "./figure/xgboost_shap/Interaction-Feature-SHAP-Value.jpg")

# Compare independent and interactive effects
tibble(name=names(shap_independent),
       value=shap_independent |> abs() |> colMeans()) |>
  arrange(desc(value)) -> mean_independent

tibble(name=names(shap_interaction),
       value=shap_interaction |> abs() |> colMeans()) |>
  arrange(desc(value)) -> mean_interaction

mean_interaction |> 
  left_join(mean_independent,by='name') |> 
  rename(interaction = `value.x`,
         independent = `value.y`) -> shap

shap$name = factor(shap$name,levels = rev(shap$name))
shap

library(ggalt)

ggplot(aes(x = independent,xend = interaction,y = name),
       data = shap) +
  geom_dumbbell(colour_x = "#FFB6C1",
                colour_xend = "#4169E1",
                size_x = 5,size_xend = 5,
                dot_guide_size = 6,
                color = "gray") +
  geom_point(aes(x = independent,y = name,
                 size = independent,
                 color = "independent"),
             alpha = 0.5) +
  geom_point(aes(x = interaction,y = name,
                 size = interaction,
                 color = "interaction"),
             alpha = 0.5) +
  scale_color_manual(name = '',
                     breaks = c("independent",
                                "interaction"),
                     values = c("independent" = "#FFB6C1",
                                "interaction" = "#4169E1"),
                     labels = c("Independent Effect", 
                                "Interactive Effect")) +
  guides(size = "none",
         color = guide_legend(override.aes = list(size = 8))) +
  xlab("SHAP Mean Vaule") +
  ylab("")+
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(1, 'cm'),
        legend.position = c(.85, .2),
        plot.title = element_text(face = "bold", 
                                  size = 18),
        axis.text = element_text(size = 18,
                                 family = 'serif', 
                                 face = "bold"),
        legend.text = element_text(family = 'serif',
                                   size = 20, 
                                   face = "bold"),
        axis.title = element_text(size = 20,
                                  family = 'serif',
                                  face = "bold"),
        plot.subtitle = element_text(face = "italic")) -> shap_compare
ggsave("./figure/xgboost_shap/Compare-independent-and-interactive-effects.pdf",
       shap_compare, width = 13.5, height = 10,device = cairo_pdf)
pdftools::pdf_convert("./figure/xgboost_shap/Compare-independent-and-interactive-effects.pdf",dpi = 300,
                      filenames = "./figure/xgboost_shap/Compare-independent-and-interactive-effects.jpg")