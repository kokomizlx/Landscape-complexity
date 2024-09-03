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

library(tidyverse)  # 加载整个 tidyverse
# 或者
library(tidyr)     # 仅加载 tidyr
library(dplyr)
Legume_2020_H <- fread("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/06-12 Nutrition profile/4_csv/SPAM_2020_global_H_TA.csv", 
                       select = c("x", "y", "SOYB_A", "PIGE_A", "LENT_A", "COWP_A", "CHIC_A"))

setnames(Legume_2020_H, old = "SOYB_A", new = "Soybean") #列名替换
setnames(Legume_2020_H, old = "PIGE_A", new = "Pigeon_Pea") 
setnames(Legume_2020_H, old = "LENT_A", new = "Lentil") 
setnames(Legume_2020_H, old = "COWP_A", new = "Cowpea") 
setnames(Legume_2020_H, old = "CHIC_A", new = "Chickpea") 

world <- map_data("world") # 获取世界地图坐标

save_path <- "D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/3_output/2_map/"
#************************************************************************************************************************************************
####Soybean 大豆/黄豆####
#************************************************************************************************************************************************
soybean_2020 = Legume_2020_H[, .(x, y, Soybean)]
soybean_2020 <- soybean_2020[Soybean != 0]

soybean_2020_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = soybean_2020,
    aes(x, y, fill = Soybean),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "darkseagreen1", high = "darkseagreen4")  # Add a color scale with log10 transformation

print(soybean_2020_plot)
ggsave(filename = paste0(save_path, "Soybean_2020.png"), plot = soybean_2020_plot, width = 8, height = 4, limitsize = FALSE)

#************************************************************************************************************************************************
####Pigeon pea####
#************************************************************************************************************************************************
pige_2020 = Legume_2020_H[, .(x, y, Pigeon_Pea)]
pige_2020 <- pige_2020[Pigeon_Pea != 0]

pige_2020_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = pige_2020,
    aes(x, y, fill = Pigeon_Pea),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightsalmon1", high = "lightsalmon4")  # Add a color scale with log10 transformation

print(pige_2020_plot)
ggsave(filename = paste0(save_path, "Pigeon Pea_2020.png"), plot = pige_2020_plot, width = 8, height = 4, limitsize = FALSE)

#************************************************************************************************************************************************
####Lentil 兵豆/小扁豆####
#************************************************************************************************************************************************
lentil_2020 = Legume_2020_H[, .(x, y, Lentil)]
lentil_2020 <- lentil_2020[Lentil != 0]

lentil_2020_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = lentil_2020,
    aes(x, y, fill = Lentil),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "lightskyblue1", high = "lightskyblue4")  # Add a color scale with log10 transformation

print(lentil_2020_plot)
ggsave(filename = paste0(save_path, "Lentil_2020.png"), plot = lentil_2020_plot, width = 8, height = 4, limitsize = FALSE)

#************************************************************************************************************************************************
####Cowpea 豇豆####
#************************************************************************************************************************************************
cowpea_2020 = Legume_2020_H[, .(x, y, Cowpea)]
cowpea_2020 = cowpea_2020[Cowpea != 0]

cowpea_2020_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = cowpea_2020,
    aes(x, y, fill = Cowpea),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "indianred", high = "indianred4")  # Add a color scale with log10 transformation

print(cowpea_2020_plot)
ggsave(filename = paste0(save_path, "Cowpea_2020.png"), plot = cowpea_2020_plot, width = 8, height = 4, limitsize = FALSE)

#************************************************************************************************************************************************
####Chickpea 鹰嘴豆####
#************************************************************************************************************************************************
chickpea_2020 = Legume_2020_H[, .(x, y, Chickpea)]
chickpea_2020 = chickpea_2020[Chickpea != 0]

chickpea_2020_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_tile(
    data = chickpea_2020,
    aes(x, y, fill = Chickpea),  # fill >> column name
    alpha = 0.7
  ) +
  scale_fill_continuous(trans = "log10",low = "burlywood1", high = "burlywood4")  # Add a color scale with log10 transformation

print(chickpea_2020_plot)
ggsave(filename = paste0(save_path, "Chickpea_2020.png"), plot = chickpea_2020_plot, width = 8, height = 4, limitsize = FALSE)

#************************************************************************************************************************************************
####多个作物####
#************************************************************************************************************************************************
Legume_long <- Legume_2020_H %>%
  pivot_longer(cols = c(Soybean, Pigeon_Pea, Lentil, Cowpea, Chickpea), 
               names_to = "crop", 
               values_to = "value")

Legume_long <- Legume_long[Legume_long$value != 0, ]

Legume_2020_H_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = Legume_long,
    aes(x, y, fill = crop),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "Soybean" = "darkseagreen3",
      "Pigeon_Pea" = "lightskyblue2", 
      "Lentil" = "lightsalmon", 
      "Cowpea" = "indianred",
      "Chickpea" = "burlywood1"
    ),
    name = 'Legume Type'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(Legume_2020_H_plot)
ggsave(filename = paste0(save_path, "Legume_2020.png"), plot = Legume_2020_H_plot, width = 8, height = 4.5, limitsize = FALSE)
