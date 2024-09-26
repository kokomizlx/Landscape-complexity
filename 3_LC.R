setwd("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity")
rm(list=ls())

install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')

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
library(spData)
library(spDataLarge)

H_category_2020 <- read_excel("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/1_data/H_category_2020.xlsx")  # 替换为您的文件路径

#****************************************************************************************************
# LC文件预处理成raster
#****************************************************************************************************
# 提取 x, y 和 double_tag 列并形成新的数据框
# LC_2020 <- H_category_2020 %>%
#   select(x, y, double_tag)

LC_2020 <- H_category_2020 %>%
  dplyr::select(x, y, double_tag)

# 将 double_tag 列转换为整型
LC_2020$double_tag <- as.integer(LC_2020$double_tag)

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
check_landscape(projected_raster)

writeRaster(LC_2020_raster, filename = "3_output/1_tiff/LC_2020.tif", format = "GTiff") # 保存合并后的栅格文件

###不同的投影
# World Robinson投影
wr = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
LC_2020_raster_wr <- projectRaster(LC_2020_raster, crs = wr)
# 将栅格数据转换为整数
LC_2020_raster_wr_int <- round(LC_2020_raster_wr)  # 或者使用 as.integer()

# 查看不同的类别
unique(values(LC_2020_raster_wr))
plot(LC_2020_raster_wr_int)
check_landscape(LC_2020_raster_wr_int)

writeRaster(LC_2020_raster, filename = "3_output/1_tiff/LC_2020_raster_wr_int.tif", format = "GTiff") # 保存合并后的栅格文件

#****************************************************************************************************
# Using landscapemetrics
# https://r-spatialecology.github.io/landscapemetrics/articles/get_started.html
#****************************************************************************************************
# Calculate e.g. perimeter of all patches
lsm_p_perim(LC_2020_raster_wr_int)
