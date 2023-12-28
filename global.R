library(shinydashboard)
library(shiny)
library(shinyWidgets)

# Import libraries
library(DT)
library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(glue)
library(plotly)
library(tidyr)
library(zoo)
library(forcats)
library(leaflet)
library(grid)
library(factoextra)
library(FactoMineR)
library(gridExtra)
library(grid)
library(lattice)
library(tibble)
library(bruceR)
library(RColorBrewer)
library(echarts4r)
library(highcharter)
library(png)

# Settingan Agar tidak muncul numeric value
options(scipen = 9999)

# read data
customer_order <- readRDS("data/customer_order.rds")
df_order <- readRDS("data/df_orders.rds")
customer_rfm <- readRDS("data/customer_rfm.rds")
lrfm <- readRDS("data/lrfm.rds")
database <- readRDS("data/database.rds")
cluster_lrfm <- readRDS("data/cluster_lrfm1.rds")
lrfm_scale <- readRDS("data/lrfm_scale.rds")
cluster_lrfm_full <- readRDS("data/cluster_lrfm_full.rds")
data <- readRDS("data/data_full.rds")


cluster_lrfm_gathered <- cluster_lrfm %>%
  gather("features", "values", L, R, F, M) %>% 
  mutate(
    popup = glue("Jumlah Customer: unique{customer}"))



# MY PLOT THEME
my_plot_theme <- function (base_size, base_family="Segoe UI Semibold"){ 
  dark_color="#222629"
  facet_header = "#78767647"
  dark_text = "#222629"
  
  half_line <- base_size/2
  theme_algoritma <- theme(
    
    plot.background = element_rect(fill=NA,colour = NA), #background plot
    plot.title = element_text(size = rel(1.2), margin = margin(b = half_line * 1.2), 
                              color= dark_text, hjust = 0, family=base_family, face = "bold"),
    plot.subtitle = element_text(size = rel(1.0), margin = margin(b = half_line * 1.2), color= dark_text, hjust=0),
    plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
    #plot.margin=unit(c(0.5,r=5,1,0.5),"cm"),
    
    panel.background = element_rect(fill="#18181800",colour = "#e8e8e8"), #background chart
    panel.border = element_rect(fill=NA,color = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color="#e8e8e8", linetype=2),
    panel.grid.minor.y = element_blank(),
    #panel.margin = unit(0.8*half_line, "mm"), 
    panel.margin.x = NULL, 
    panel.margin.y = NULL, 
    panel.ontop = FALSE,
    panel.spacing = unit(1.2,"lines"),
    
    legend.background = element_rect(fill="#18181800",colour = NA),
    legend.text = element_text(size = rel(0.7),color=dark_text),
    legend.title =  element_text(colour = dark_text, size = base_size, lineheight = 0.8),
    legend.box = NULL, 
    
    # text = element_text(colour = "white", size = base_size, lineheight = 0.9, 
    #                    angle = 0, margin = margin(), debug = FALSE),
    axis.text = element_text(size = rel(0.8), color=dark_text),
    axis.text.x = element_text(colour = dark_text, size = base_size, margin = margin(t = 0.8 * half_line/2)),
    axis.text.y = element_text(colour = dark_text, size = base_size, margin = margin(r = 0.8 * half_line/2)),
    axis.title.x = element_text(colour = dark_text, size = base_size, lineheight = 0.8,
                                margin = margin(t = 0.8 * half_line, b = 0.8 * half_line/2)), 
    axis.title.y = element_text(colour = dark_text, size = base_size, lineheight = 0.8,
                                angle = 90, margin = margin(r = 0.8 * half_line, l = 0.8 * half_line/2)),
    axis.ticks = element_blank(),
    
    strip.background = element_rect(fill=facet_header,colour = NA),
    strip.text = element_text(colour = dark_text, size = rel(0.8)), 
    strip.text.x = element_text(margin = margin(t = half_line*0.8, b = half_line*0.8)), 
    strip.text.y = element_text(angle = -90, margin = margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"),
    complete = TRUE
    
  )
  

}

