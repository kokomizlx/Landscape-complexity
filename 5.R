setwd("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity")
rm(list=ls())

library(landscapemetrics)
library(terra)
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(sp)  # vector data
library(sf)
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(spdep)  # spatial dependence

# install.packages("devtools")
devtools::install_github("r-lib/conflicted")
library(tidyverse)
library(ggplot2)
library(conflicted)
library(dplyr)
library(readxl)
library(openxlsx)

H_category_2010 <- read_excel("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/1_data/H_category_2010.xlsx")  
H_category_2020 <- read_excel("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/1_data/H_category_2020.xlsx")  
combined_data <- read_excel("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/1_data/H_change_20102020.xlsx")

#************************************************************************************************************************************************
####2010多个作物####
#************************************************************************************************************************************************
world <- map_data("world") # 获取世界地图坐标

H_category_2010$double_tag <- as.factor(H_category_2010$double_tag)

H_category_2010_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = H_category_2010,
    aes(x, y, fill = double_tag),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#f7fcb9", 
      "2" = "#e5f5e0", 
      "3" = "#c7e9c0",
      "4" = "#a1d99b",
      "5" = "#74c476",
      "6" = "#41ab5d",
      "7" = "#238b45",
      "8" = "#005a32"
    ),
    name = 'Number of Category'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(H_category_2010_plot)
ggsave((filename = "3_output/2_map/2_All crop category/2010_Category distribution.png"), plot = H_category_2010_plot, 
       width = 8, height = 5.2, limitsize = FALSE)

#************************************************************************************************************************************************
####2020多个作物####
#************************************************************************************************************************************************
world <- map_data("world") # 获取世界地图坐标

H_category_2020$double_tag <- as.factor(H_category_2020$double_tag)

H_category_2020_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = H_category_2020,
    aes(x, y, fill = double_tag),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "1" = "#f7fcb9", 
      "2" = "#e5f5e0", 
      "3" = "#c7e9c0",
      "4" = "#a1d99b",
      "5" = "#74c476",
      "6" = "#41ab5d",
      "7" = "#238b45",
      "8" = "#005a32"
    ),
    name = 'Number of Category'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(H_category_2020_plot)
ggsave((filename = "3_output/2_map/2_All crop category/2020_Category distribution.png"), plot = H_category_2020_plot, 
       width = 8, height = 5.2, limitsize = FALSE)

#************************************************************************************************************************************************
####计算年际变化####
#************************************************************************************************************************************************
# 合并数据
# 提取特定列
change_2010 <- subset(H_category_2010, select = c("x", "y", "double_tag"))
change_2020 <- subset(H_category_2020, select = c("x", "y", "double_tag"))

combined_data=full_join(change_2010,change_2020, by = c("x", "y"))

# 计算变化
combined_data$double_tag.x <- as.numeric(combined_data$double_tag.x)
combined_data$double_tag.y <- as.numeric(combined_data$double_tag.y)

combined_data[is.na(combined_data)] <- 0

combined_data <- combined_data %>%
  mutate(change = double_tag.y - double_tag.x)

# write.xlsx(combined_data, file = "3_output/3_excel/H_change_20102020.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(combined_data, file = "3_output/3_excel/H_change_20102020.xlsx", rowNames = FALSE)

# 绘制地图
combined_data$change <- as.numeric(combined_data$change)

change_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = combined_data,
    aes(x, y, fill = change),  # Use change for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_gradient(
    low = "#f7fcb9",  # Color for low values
    high = "#005a32", # Color for high values
    name = 'Change Value'  # Custom legend title
  ) +                                                                                                                                                                                          
  theme(legend.position = "bottom")

print(change_plot)

# 拿其他数据举例
H2010_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = H_category_2010,
    aes(x, y, fill = double_tag),  # Use change for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_gradient(
    low = "#f7fcb9",  # Color for low values
    high = "#005a32", # Color for high values
    name = 'Number of category'  # Custom legend title
  ) +                                                                                                                                                                                          
  theme(legend.position = "bottom")

print(H2010_plot)

H2020_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = H_category_2020,
    aes(x, y, fill = double_tag),  # Use change for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_gradient(
    low = "#f7fcb9",  # Color for low values
    high = "#005a32", # Color for high values
    name = 'Number of category'  # Custom legend title
  ) +                                                                                                                                                                                          
  theme(legend.position = "bottom")

print(H2020_plot)
