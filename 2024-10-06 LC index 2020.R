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
library(spData)
library(spDataLarge)
library(tmap)
library(writexl)

H_category_2020 <- read_excel("D:/5-onedrive data/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/1_data/H_category_2020.xlsx")  

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
writeRaster(LC_2020_raster, filename = "3_output/1_tiff/LC_2020.tif", format = "GTiff", overwrite=TRUE) # 保存合并后的栅格文件

# 将栅格数据转换为整数
LC_2020_raster_wr_int <- round(LC_2020_raster_wr)  # 或者使用 as.integer()

# 查看不同的类别
unique(values(LC_2020_raster_wr))
plot(LC_2020_raster_wr_int)

#****************************************************************************************************
# Checking landscape
#****************************************************************************************************
check_landscape(LC_2020_raster_wr_int)

writeRaster(LC_2020_raster, filename = "3_output/1_tiff/LC_2020_raster_wr_int.tif", format = "GTiff", overwrite=TRUE) # 保存合并后的栅格文件

#****************************************************************************************************
# Using landscapemetrics
# https://r-spatialecology.github.io/landscapemetrics/articles/get_started.html
# https://r-spatialecology.github.io/landscapemetrics/reference/index.html#landscape-level-metrics
#****************************************************************************************************
# Landscape Level Metrics
Landscapemetrcis_2020 = calculate_lsm(LC_2020_raster_wr_int, 
                                      what = c("lsm_l_ai", "lsm_l_area_cv", "lsm_l_area_mn", "lsm_l_area_sd", 
                                               "lsm_l_cai_cv", "lsm_l_cai_mn", "lsm_l_cai_sd", "lsm_l_cohesion", 
                                               "lsm_l_condent", "lsm_l_contag", "lsm_l_contig_cv", 
                                               "lsm_l_contig_mn", "lsm_l_contig_sd", "lsm_l_core_cv", 
                                               "lsm_l_core_mn", "lsm_l_core_sd", "lsm_l_dcad", "lsm_l_dcore_cv", 
                                               "lsm_l_dcore_mn", "lsm_l_dcore_sd", "lsm_l_division", "lsm_l_ed", 
                                               "lsm_l_enn_cv", "lsm_l_enn_mn", "lsm_l_enn_sd", "lsm_l_ent", 
                                               "lsm_l_frac_cv", "lsm_l_frac_mn", "lsm_l_frac_sd", "lsm_l_gyrate_cv", 
                                               "lsm_l_gyrate_mn", "lsm_l_gyrate_sd", "lsm_l_iji", "lsm_l_joinent", 
                                               "lsm_l_lpi", "lsm_l_lsi", "lsm_l_mesh", "lsm_l_msidi", "lsm_l_msiei", 
                                               "lsm_l_mutinf", "lsm_l_ndca", "lsm_l_np", "lsm_l_pafrac", 
                                               "lsm_l_para_cv", "lsm_l_para_mn", "lsm_l_para_sd", "lsm_l_pd", 
                                               "lsm_l_pladj", "lsm_l_pr", "lsm_l_prd", "lsm_l_relmutinf", 
                                               "lsm_l_shape_cv", "lsm_l_shape_mn", "lsm_l_shape_sd", "lsm_l_shdi", 
                                               "lsm_l_shei", "lsm_l_sidi", "lsm_l_siei", "lsm_l_split", "lsm_l_ta", 
                                               "lsm_l_tca", "lsm_l_te"),
                                      full_name = TRUE)

write_xlsx(Landscapemetrcis_2020, "3_output/3_excel/Landscape level metrics 2020.xlsx")
