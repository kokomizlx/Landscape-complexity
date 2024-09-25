setwd("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity")
rm(list=ls())

library(readxl)
library(openxlsx)
library(landscapemetrics)
library(terra)
library(raster)
library(tidyverse)
library(ggsci)
library(sp)
library(sf)
library(plotrix)
library(data.table)
library(dplyr)

H_category_2020 <- read_excel("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/1_data/H_category_2020.xlsx")  # 替换为您的文件路径

#****************************************************************************************************
# LC文件预处理成raster
#****************************************************************************************************
# 提取 x, y 和 double_tag 列并形成新的数据框
# LC_2020 <- H_category_2020 %>%
#   select(x, y, double_tag)

LC_2020 <- H_category_2020 %>%
  dplyr::select(x, y, double_tag)



# 设置数据的范围
xmin <- -180
xmax <- 180
ymin <- -90
ymax <- 90

# 创建栅格对象，设置分辨率（例如，0.1）
r <- raster(xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, res = 0.05)

# 栅格化
LC_2020_raster <- rasterize(LC_2020[, c("x", "y")], r, LC_2020$double_tag, fun = mean)
check_landscape(LC_2020_raster)
plot(LC_2020_raster)

# Web Mercator 是一种常用于在线地图（如 Google Maps 和 OpenStreetMap）的投影，单位为米，适合全球范围的应用。
crs_web_mercator <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# 使用UTM（通用横轴墨卡托投影）
utm_crs <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")

# 投影转换(这个有问题)
LC_2020_raster_2 <- projectRaster(LC_2020_raster, crs = utm_crs)
plot(LC_2020_raster_2)

check_landscape(LC_2020_raster_2)

Legume_2010_H_raster_long <- projectRaster(Legume_2010_H_raster_long, crs = utm_crs)