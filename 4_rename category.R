setwd("D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity")
rm(list=ls())

library(ggplot2)
library(maps)
library(readxl)
library(openxlsx)
library(data.table)  
library(dplyr)

save_path = "D:/1_download/1_onedrive/OneDrive - 西湖大学/1_Project/2024/08-25 Landscape complexity/3_output/2_map/2_All crop category"


#****************************************************************************************************
# Data preparation
#****************************************************************************************************
H_2010 <- read.csv("1_data/SPAM_2010_global_H_TA.csv")

setnames(H_2010, old = "whea_a", new = "Wheat") #列名替换
setnames(H_2010, old = "rice_a", new = "Rice") 
setnames(H_2010, old = "maiz_a", new = "Maize")
setnames(H_2010, old = "barl_a", new = "Barley") 
setnames(H_2010, old = "smil_a", new = "Small Millet") 
setnames(H_2010, old = "pmil_a", new = "Pearl Millet")
setnames(H_2010, old = "sorg_a", new = "Sorghum")
setnames(H_2010, old = "ocer_a", new = "Other Cereals")
setnames(H_2010, old = "pota_a", new = "Potato")
setnames(H_2010, old = "swpo_a", new = "Sweet Potato")
setnames(H_2010, old = "yams_a", new = "Yams")
setnames(H_2010, old = "cass_a", new = "Cassava")
setnames(H_2010, old = "orts_a", new = "Other Roots")
setnames(H_2010, old = "bean_a", new = "Bean")
setnames(H_2010, old = "chic_a", new = "Chickpea")
setnames(H_2010, old = "cowp_a", new = "Cowpea")
setnames(H_2010, old = "pige_a", new = "Pigeon Pea")
setnames(H_2010, old = "lent_a", new = "Lentil")
setnames(H_2010, old = "opul_a", new = "Other Pulses")
setnames(H_2010, old = "soyb_a", new = "Soybean")
setnames(H_2010, old = "grou_a", new = "Groundnut")
setnames(H_2010, old = "cnut_a", new = "Coconut")
setnames(H_2010, old = "oilp_a", new = "Oilpalm")
setnames(H_2010, old = "sunf_a", new = "Sunflower")
setnames(H_2010, old = "rape_a", new = "Rapeseed")
setnames(H_2010, old = "sesa_a", new = "Sesame Seed")
setnames(H_2010, old = "ooil_a", new = "Other Oil Crops")
setnames(H_2010, old = "sugc_a", new = "Sugarcane")
setnames(H_2010, old = "sugb_a", new = "Sugarbeet")
setnames(H_2010, old = "cott_a", new = "Cotton")
setnames(H_2010, old = "ofib_a", new = "Other Fibre Crops")
setnames(H_2010, old = "acof_a", new = "Arabic Coffee")
setnames(H_2010, old = "rcof_a", new = "Robust Coffee")
setnames(H_2010, old = "coco_a", new = "Cocoa")
setnames(H_2010, old = "teas_a", new = "Tea")
setnames(H_2010, old = "toba_a", new = "Tobacco")
setnames(H_2010, old = "bana_a", new = "Banana")
setnames(H_2010, old = "plnt_a", new = "Plantain")
# setnames(H_2010, old = "citr_a", new = "Citrus")
setnames(H_2010, old = "trof_a", new = "Other Tropical Fruit")
setnames(H_2010, old = "temf_a", new = "Temperate Fruit")
# setnames(H_2010, old = "toma_a", new = "Tomato")
# setnames(H_2010, old = "onio_a", new = "Onion")
setnames(H_2010, old = "vege_a", new = "Other Vegetables")
# setnames(H_2010, old = "rubb_a", new = "Rubber")
setnames(H_2010, old = "rest_a", new = "Rest Of Crops")

H_2010[, 10:51] <- lapply(H_2010[, 10:51], as.numeric)  # 将第1到第3列转换为数值
H_2010[, 5:6] <- lapply(H_2010[, 5:6], as.numeric)  # 将第1到第3列转换为数值


H_2010 <- H_2010 %>%
  mutate(Cereals_value = rowSums(select(., contains(c("Wheat", "Rice", "Maize", "Barley", "Small Millet", 
                                                      "Pearl Millet", "Sorghum", "Other Cereals")))))

H_2010 <- H_2010 %>%
  mutate(Roots_value = rowSums(select(., contains(c("Potato", "Sweet Potato", "Yams", "Cassava", "Other Roots")))))

H_2010 <- H_2010 %>%
  mutate(Pulses_value = rowSums(select(., contains(c("Bean", "Chickpea", "Cowpea", "Pigeon Pea", "Lentil", 
                                                     "Other Pulses")))))

H_2010 <- H_2010 %>%
  mutate(Oil_value = rowSums(select(., contains(c("Soybean", "Groundnut", "Coconut", "Oilpalm", 
                                                  "Sunflower", "Rapeseed", "Sesame Seed","Other Oil Crops")))))

H_2010 <- H_2010 %>%
  mutate(Fibre_value = rowSums(select(., contains(c("Sugarcane", "Sugarbeet", "Cotton", "Other Fibre Crops")))))

H_2010 <- H_2010 %>%
  mutate(Tropical_fruits_value = rowSums(select(., contains(c("Arabic Coffee", "Robust Coffee", "Tea", 
                                                              "Tobacco", "Banana", "Plantain", "Citrus", 
                                                              "Other Tropical Fruits")))))

H_2010 <- H_2010 %>%
  mutate(Temperate_fruits_value = rowSums(select(., contains(c("Temperate Fruits")))))

H_2010 <- H_2010 %>%
  mutate(Vegetables_value = rowSums(select(., contains(c("Tomato", "Onion", "Other Vegetables")))))

H_2010 <- H_2010 %>%
  mutate(Rest_value = rowSums(select(., contains(c("Rubber", "Rest of Crops")))))

#****************************************************************************************************
# Tag
#****************************************************************************************************
# 提取特定列
H_category_2010 <- subset(H_2010, select = c("x", "y", "Cereals_value", "Roots_value", "Pulses_value", "Oil_value", 
                                             "Fibre_value", "Tropical_fruits_value", "Temperate_fruits_value",
                                             "Vegetables_value" ,"Rest_value"))

H_category_2010$Cereals_tag <- NA
H_category_2010$Roots_tag <- NA
H_category_2010$Pulses_tag <- NA
H_category_2010$Oil_tag <- NA
H_category_2010$Fibre_tag <- NA
H_category_2010$Tropical_fruits_tag <- NA
H_category_2010$Temperate_fruits_tag <- NA
H_category_2010$Vegetables_tag <- NA
H_category_2010$Rest_tag <- NA

H_category_2010 <- H_category_2010 %>%
  mutate(Cereals_tag = ifelse(Cereals_value != 0, "Cereals", Cereals_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Roots_tag = ifelse(Roots_value != 0, "Roots", Roots_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Pulses_tag = ifelse(Pulses_value != 0, "Pulses", Pulses_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Oil_tag = ifelse(Oil_value != 0, "Oil", Oil_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Fibre_tag = ifelse(Fibre_value != 0, "Fibre", Fibre_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Tropical_fruits_tag = ifelse(Tropical_fruits_value != 0, "Tropical_fruits", Tropical_fruits_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Temperate_fruits_tag = ifelse(Temperate_fruits_value != 0, "Temperate_fruits", Temperate_fruits_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Vegetables_tag = ifelse(Vegetables_value != 0, "Vegetables", Vegetables_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(Rest_tag = ifelse(Rest_value != 0, "Rest", Rest_tag))

H_category_2010 <- H_category_2010 %>%
  mutate(category = paste(Cereals_tag, Roots_tag, Pulses_tag, Oil_tag, Fibre_tag, 
                          Tropical_fruits_tag, Temperate_fruits_tag, Vegetables_tag,
                          Rest_tag, sep = " + "))

# 替换 category 列中的 "+ NA" 为 ""
H_category_2010 <- H_category_2010 %>%
  mutate(category = gsub("\\+ NA", "", category))

# 替换 category 列中的 "NA +" 为 ""
H_category_2010 <- H_category_2010 %>%
  mutate(category = gsub("NA \\+", "", category))

# 替换 category 列中的 "NA" 为 ""
H_category_2010 <- H_category_2010 %>%
  mutate(category = gsub("NA", "", category))

# 替换 category 列中的 " " 为 ""
H_category_2010 <- H_category_2010 %>%
  mutate(category = gsub("^\\s*$", "", category))

# 去掉 category 列中的多余空格
H_category_2010 <- H_category_2010 %>%
  mutate(category = trimws(gsub("\\s+", " ", category)))

# 删除 category 列中以 "+ " 开头的内容
H_category_2010 <- H_category_2010 %>%
  mutate(category = gsub("^\\+ ", "", category))

# 删除 category 列中内容为空的整行
H_category_2010 <- H_category_2010 %>%
  filter(category != "")

unique(H_category_2010$category)

category_counts <- H_category_2010 %>%
  count(category) %>%          # 统计每个类别的数量
  arrange(desc(n))             # 按数量从多到少排列

print(category_counts)

#****************************************************************************************************
# Double tag
#****************************************************************************************************
# 单一类作物赋值1
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Cereals", "Roots", "Pulses", "Oil", 
                                             "Vegetables", "Fibre", 
                                             "Tropical_fruits", "Temperater_fruits", 
                                             "Rest"), 1, 0))

# 两类作物组合赋值2
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Cereals + Roots",
                                             "Cereals + Pulses",
                                             "Cereals + Oil",
                                             "Cereals + Fibre",
                                             "Cereals + Tropical_fruits",
                                             "Cereals + Temperate_fruits",
                                             "Cereals + Vegetables",
                                             "Cereals + Rest",
                                             "Roots + Pulses",
                                             "Roots + Oil",
                                             "Roots + Fibre",
                                             "Roots + Tropical_fruits",
                                             "Roots + Temperate_fruits",
                                             "Roots + Vegetables",
                                             "Roots + Rest",
                                             "Pulses + Oil",
                                             "Pulses + Fibre",
                                             "Pulses + Tropical_fruits",
                                             "Pulses + Temperate_fruits",
                                             "Pulses + Vegetables",
                                             "Pulses + Rest",
                                             "Oil + Fibre",
                                             "Oil + Tropical_fruits",
                                             "Oil + Temperate_fruits",
                                             "Oil + Vegetables",
                                             "Oil + Rest",
                                             "Fibre + Tropical_fruits",
                                             "Fibre + Temperate_fruits",
                                             "Fibre + Vegetables",
                                             "Fibre + Rest",
                                             "Tropical_fruits + Temperate_fruits",
                                             "Tropical_fruits + Vegetables",
                                             "Tropical_fruits + Rest",
                                             "Temperate_fruits + Vegetables",
                                             "Temperate_fruits + Rest",
                                             "Vegetables + Rest"), 2, double_tag))

# 三类作物组合赋值3
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Cereals + Roots + Pulses",
                                             "Cereals + Roots + Oil",
                                             "Cereals + Roots + Fibre",
                                             "Cereals + Roots + Tropical_fruits",
                                             "Cereals + Roots + Temperate_fruits",
                                             "Cereals + Roots + Vegetables",
                                             "Cereals + Roots + Rest",
                                             "Cereals + Pulses + Oil",
                                             "Cereals + Pulses + Fibre",
                                             "Cereals + Pulses + Tropical_fruits",
                                             "Cereals + Pulses + Temperate_fruits",
                                             "Cereals + Pulses + Vegetables",
                                             "Cereals + Pulses + Rest",
                                             "Cereals + Oil + Fibre",
                                             "Cereals + Oil + Tropical_fruits",
                                             "Cereals + Oil + Temperate_fruits",
                                             "Cereals + Oil + Vegetables",
                                             "Cereals + Oil + Rest",
                                             "Cereals + Fibre + Tropical_fruits",
                                             "Cereals + Fibre + Temperate_fruits",
                                             "Cereals + Fibre + Vegetables",
                                             "Cereals + Fibre + Rest",
                                             "Cereals + Tropical_fruits + Temperate_fruits",
                                             "Cereals + Tropical_fruits + Vegetables",
                                             "Cereals + Tropical_fruits + Rest",
                                             "Cereals + Temperate_fruits + Vegetables",
                                             "Cereals + Temperate_fruits + Rest",
                                             "Cereals + Vegetables + Rest",
                                             "Roots + Pulses + Oil",
                                             "Roots + Pulses + Fibre",
                                             "Roots + Pulses + Tropical_fruits",
                                             "Roots + Pulses + Temperate_fruits",
                                             "Roots + Pulses + Vegetables",
                                             "Roots + Pulses + Rest",
                                             "Roots + Oil + Fibre",
                                             "Roots + Oil + Tropical_fruits",
                                             "Roots + Oil + Temperate_fruits",
                                             "Roots + Oil + Vegetables",
                                             "Roots + Oil + Rest",
                                             "Roots + Fibre + Tropical_fruits",
                                             "Roots + Fibre + Temperate_fruits",
                                             "Roots + Fibre + Vegetables",
                                             "Roots + Fibre + Rest",
                                             "Roots + Tropical_fruits + Temperate_fruits",
                                             "Roots + Tropical_fruits + Vegetables",
                                             "Roots + Tropical_fruits + Rest",
                                             "Roots + Temperate_fruits + Vegetables",
                                             "Roots + Temperate_fruits + Rest",
                                             "Roots + Vegetables + Rest",
                                             "Pulses + Oil + Fibre",
                                             "Pulses + Oil + Tropical_fruits",
                                             "Pulses + Oil + Temperate_fruits",
                                             "Pulses + Oil + Vegetables",
                                             "Pulses + Oil + Rest",
                                             "Pulses + Fibre + Tropical_fruits",
                                             "Pulses + Fibre + Temperate_fruits",
                                             "Pulses + Fibre + Vegetables",
                                             "Pulses + Fibre + Rest",
                                             "Pulses + Tropical_fruits + Temperate_fruits",
                                             "Pulses + Tropical_fruits + Vegetables",
                                             "Pulses + Tropical_fruits + Rest",
                                             "Pulses + Temperate_fruits + Vegetables",
                                             "Pulses + Temperate_fruits + Rest",
                                             "Pulses + Vegetables + Rest",
                                             "Oil + Fibre + Tropical_fruits",
                                             "Oil + Fibre + Temperate_fruits",
                                             "Oil + Fibre + Vegetables",
                                             "Oil + Fibre + Rest",
                                             "Oil + Tropical_fruits + Temperate_fruits",
                                             "Oil + Tropical_fruits + Vegetables",
                                             "Oil + Tropical_fruits + Rest",
                                             "Oil + Temperate_fruits + Vegetables",
                                             "Oil + Temperate_fruits + Rest",
                                             "Oil + Vegetables + Rest",
                                             "Fibre + Tropical_fruits + Temperate_fruits",
                                             "Fibre + Tropical_fruits + Vegetables",
                                             "Fibre + Tropical_fruits + Rest",
                                             "Fibre + Temperate_fruits + Vegetables",
                                             "Fibre + Temperate_fruits + Rest",
                                             "Fibre + Vegetables + Rest",
                                             "Tropical_fruits + Temperate_fruits + Vegetables",
                                             "Tropical_fruits + Temperate_fruits + Rest",
                                             "Tropical_fruits + Vegetables + Rest",
                                             "Temperate_fruits + Vegetables + Rest"), 3, double_tag))

# 四类作物组合赋值4
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Roots + Pulses + Fibre + Vegetables",
                                             "Cereals + Pulses + Oil + Fibre",
                                             "Cereals + Roots + Oil + Fibre",
                                             "Cereals + Roots + Pulses + Vegetables",
                                             "Cereals + Roots + Pulses + Oil",
                                             "Cereals + Roots + Vegetables + Rest",
                                             "Cereals + Roots + Oil + Vegetables",
                                             "Cereals + Roots + Pulses + Fibre",
                                             "Cereals + Pulses + Fibre + Vegetables",
                                             "Cereals + Pulses + Oil + Vegetables",
                                             "Pulses + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Pulses + Vegetables + Rest",
                                             "Roots + Pulses + Vegetables + Rest",
                                             "Cereals + Fibre + Vegetables + Rest",
                                             "Cereals + Roots + Fibre + Vegetables",
                                             "Roots + Fibre + Vegetables + Rest",
                                             "Cereals + Oil + Fibre + Vegetables",  
                                             "Roots + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Oil + Vegetables + Rest",
                                             "Cereals + Pulses + Fibre + Rest",
                                             "Roots + Oil + Fibre + Vegetables",
                                             "Cereals + Pulses + Oil + Rest",
                                             "Pulses + Oil + Fibre + Rest",
                                             "Pulses + Oil + Fibre + Vegetables",
                                             "Cereals + Oil + Fibre + Rest",
                                             "Pulses + Oil + Tropical_fruits + Vegetables",
                                             "Pulses + Oil + Vegetables + Rest",
                                             "Roots + Pulses + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Tropical_fruits + Vegetables",
                                             "Cereals + Oil + Tropical_fruits + Vegetables",
                                             "Oil + Fibre + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Vegetables",
                                             "Cereals + Tropical_fruits + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Fibre",
                                             "Cereals + Pulses + Oil + Tropical_fruits",
                                             "Cereals + Pulses + Fibre + Tropical_fruits",
                                             "Roots + Tropical_fruits + Vegetables + Rest",
                                             "Pulses + Oil + Fibre + Tropical_fruits",
                                             "Cereals + Roots + Fibre + Tropical_fruits",
                                             "Roots + Oil + Fibre + Rest",
                                             "Cereals + Oil + Fibre + Tropical_fruits",
                                             "Cereals + Pulses + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Pulses + Rest",
                                             "Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Pulses + Tropical_fruits",
                                             "Cereals + Roots + Tropical_fruits + Rest",
                                             "Oil + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Oil + Rest",
                                             "Cereals + Roots + Oil + Tropical_fruits",
                                             "Roots + Oil + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Tropical_fruits",
                                             "Cereals + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Fibre + Tropical_fruits + Rest",
                                             "Roots + Oil + Fibre + Tropical_fruits",
                                             "Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Oil + Tropical_fruits + Rest",
                                             "Pulses + Oil + Tropical_fruits + Rest",
                                             "Cereals + Pulses + Tropical_fruits + Rest",
                                             "Cereals + Roots + Fibre + Rest",
                                             "Roots + Pulses + Oil + Rest",
                                             "Roots + Oil + Tropical_fruits + Vegetables",
                                             "Pulses + Fibre + Vegetables + Rest",
                                             "Pulses + Fibre + Tropical_fruits + Vegetables",
                                             "Roots + Pulses + Tropical_fruits + Rest",
                                             "Pulses + Fibre + Tropical_fruits + Rest",
                                             "Roots + Oil + Tropical_fruits + Rest",
                                             "Roots + Pulses + Fibre + Tropical_fruits",
                                             "Roots + Fibre + Tropical_fruits + Rest",
                                             "Oil + Fibre + Tropical_fruits + Rest",
                                             "Roots + Pulses + Fibre + Rest"),4, double_tag))

# 五类作物组合赋值5
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Cereals + Roots + Pulses + Oil + Fibre",
                                             "Cereals + Roots + Pulses + Oil + Vegetables",
                                             "Cereals + Roots + Pulses + Fibre + Vegetables",
                                             "Cereals + Roots + Oil + Fibre + Vegetables",
                                             "Cereals + Roots + Oil + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Vegetables + Rest",
                                             "Cereals + Pulses + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Fibre + Vegetables",
                                             "Roots + Pulses + Oil + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Fibre + Rest",
                                             "Roots + Oil + Fibre + Vegetables + Rest",
                                             "Roots + Pulses + Fibre + Vegetables + Rest",
                                             "Cereals + Roots + Fibre + Vegetables + Rest",
                                             "Cereals + Oil + Fibre + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Fibre + Vegetables",
                                             "Cereals + Roots + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Oil + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Pulses + Oil + Rest",
                                             "Cereals + Pulses + Oil + Fibre + Tropical_fruits",
                                             "Pulses + Oil + Fibre + Vegetables + Rest",
                                             "Cereals + Pulses + Fibre + Vegetables + Rest",
                                             "Cereals + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Tropical_fruits + Vegetables",
                                             "Roots + Pulses + Oil + Tropical_fruits + Vegetables",
                                             "Roots + Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Pulses + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Oil + Fibre + Tropical_fruits",
                                             "Cereals + Pulses + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Pulses + Oil + Tropical_fruits",
                                             "Cereals + Oil + Tropical_fruits + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Fibre + Tropical_fruits",
                                             "Pulses + Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Fibre + Tropical_fruits + Rest",
                                             "Cereals + Roots + Oil + Tropical_fruits + Rest",
                                             "Cereals + Pulses + Oil + Tropical_fruits + Rest",
                                             "Cereals + Roots + Pulses + Fibre + Tropical_fruits",
                                             "Cereals + Roots + Oil + Fibre + Rest",
                                             "Cereals + Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Fibre + Tropical_fruits + Vegetables",
                                             "Roots + Oil + Tropical_fruits + Vegetables + Rest",
                                             "Pulses + Oil + Fibre + Tropical_fruits + Rest",
                                             "Pulses + Oil + Tropical_fruits + Vegetables + Rest",
                                             "Roots + Oil + Fibre + Tropical_fruits + Rest",
                                             "Roots + Pulses + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Fibre + Rest",
                                             "Cereals + Roots + Pulses + Tropical_fruits + Rest",
                                             "Cereals + Pulses + Fibre + Tropical_fruits + Rest",
                                             "Roots + Pulses + Oil + Tropical_fruits + Rest",
                                             "Roots + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Oil + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Roots + Pulses + Fibre + Tropical_fruits + Rest",
                                             "Cereals + Oil + Fibre + Tropical_fruits + Rest",
                                             "Roots + Pulses + Fibre + Tropical_fruits + Vegetables",
                                             "Pulses + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Fibre + Rest"),5, double_tag))

# 六类作物组合赋值6
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Cereals + Roots + Pulses + Oil + Fibre + Vegetables",
                                             "Cereals + Roots + Pulses + Oil + Fibre + Rest",
                                             "Cereals + Roots + Pulses + Oil + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Fibre + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Fibre + Vegetables + Rest",
                                             "Cereals + Roots + Oil + Fibre + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Fibre + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Pulses + Oil + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Oil + Tropical_fruits + Vegetables",
                                             "Cereals + Pulses + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Oil + Fibre + Tropical_fruits",
                                             "Cereals + Roots + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Oil + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Fibre + Tropical_fruits + Vegetables",
                                             "Roots + Pulses + Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Cereals + Roots + Pulses + Oil + Tropical_fruits + Rest",
                                             "Roots + Pulses + Oil + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Oil + Fibre + Tropical_fruits + Rest",
                                             "Pulses + Oil + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Fibre + Tropical_fruits + Rest",
                                             "Cereals + Oil + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Roots + Oil + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Roots + Pulses + Oil + Fibre + Tropical_fruits + Rest",
                                             "Roots + Pulses + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Fibre + Tropical_fruits + Rest"),6, double_tag))

# 七类作物组合赋值7
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Cereals + Roots + Pulses + Oil + Fibre + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Oil + Fibre + Tropical_fruits + Vegetables",
                                             "Roots + Pulses + Oil + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Pulses + Oil + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Oil + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Oil + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Fibre + Tropical_fruits + Vegetables + Rest",
                                             "Cereals + Roots + Pulses + Oil + Fibre + Tropical_fruits + Rest"),7, double_tag))

# 八类作物组合赋值8
H_category_2010 <- H_category_2010 %>%
  mutate(double_tag = ifelse(category %in% c("Cereals + Roots + Pulses + Oil + Fibre + Tropical_fruits + Vegetables + Rest"),8, double_tag))

unique(H_category_2010$double_tag)

H_category_2010[, 3:10] <- lapply(H_2010[, 3:10], as.numeric)  # 将第1到第3列转换为数值
# H_category_2010[, 22] <- lapply(H_2010[, 22], as.numeric) #跑这条会报错

# 检测是否存在NA值
has_na <- any(is.na(H_category_2010$double_tag))
if (has_na) {
  print("double_tag 列中存在 NA 值")
} else {
  print("double_tag 列中没有 NA 值")
}

# 导出数据框到 Excel 文件
write.xlsx(H_category_2010, file = "3_output/3_excel/H_category_2010.xlsx", rowNames = FALSE)

#****************************************************************************************************
# 初步可视化分析 -- 单一类作物
#****************************************************************************************************
world <- map_data("world") # 获取世界地图坐标

one_category <- droplevels(subset(H_category_2010, H_category_2010$category %in% 
                                    c("Cereals", "Roots", "Pulses", "Fibre", "Oil", "Rest", "Vegetable",
                                      "Tropical_fruits", "Temperate_fruits")))

one_category_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = one_category ,
    aes(x, y, fill = category),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "Cereals" = "burlywood1",
      "Roots" = "mediumpurple1", 
      "Pulses" = "seagreen", 
      "Fibre" = "indianred",
      "Oil" = "olivedrab1",
      "Rest" = "lightsalmon4",
      "Vegetable" = "darkseagreen3",
      "Tropical_fruits" = "lightsalmon",
      "Temperate_fruits" = "lightskyblue2"
    ),
    name = 'Crop Type'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(one_category_plot)
ggsave(filename = "3_output/2_map/2_All crop category/2010_Areas had only one category.png", plot = one_category_plot, 
       width = 8, height = 5.2, limitsize = FALSE)

#****************************************************************************************************
# 初步可视化分析 -- Cereals复合种植
#****************************************************************************************************
Cereals_two_category <- droplevels(subset(H_category_2010, H_category_2010$category %in% 
                                            c("Cereals + Fibre", "Cereals + Pulses", "Cereals + Rest",
                                              "Cereals + Roots", "Cereals + Vegetables", "Cereals + Oil",
                                              "Cereals + Temperate_fruits", "Cereals + Tropical_fruits")))

Cereals_two_category_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = Cereals_two_category ,
    aes(x, y, fill = category),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "Cereals + Roots" = "mediumpurple1", 
      "Cereals + Pulses" = "seagreen", 
      "Cereals + Fibre" = "indianred",
      "Cereals + Oil" = "olivedrab1",
      "Cereals + Rest" = "lightsalmon4",
      "Cereals + Vegetables" = "darkseagreen3",
      "Cereals + Tropical_fruits" = "lightsalmon",
      "Cereals + Temperate_fruits" = "lightskyblue2"
    ),
    name = 'Crop Type'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(Cereals_two_category_plot)
ggsave(filename = "3_output/2_map/2_All crop category/2010_Cereals with another crop.png", plot = Cereals_two_category_plot, 
       width = 8, height = 5.2, limitsize = FALSE)

#****************************************************************************************************
# 初步可视化分析 -- Roots复合种植
#****************************************************************************************************
Roots_two_category <- droplevels(subset(H_category_2010, H_category_2010$category %in% 
                                          c("Roots + Fibre", "Roots + Pulses", "Roots + Rest",
                                            "Cereals + Roots", "Roots + Vegetables", "Roots + Oil",
                                            "Roots + Temperate_fruits", "Roots + Tropical_fruits")))

Roots_two_category_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = Roots_two_category ,
    aes(x, y, fill = category),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "Cereals + Roots" = "mediumpurple1", 
      "Roots + Pulses" = "seagreen", 
      "Roots + Fibre" = "indianred",
      "Roots + Oil" = "olivedrab1",
      "Roots + Rest" = "lightsalmon4",
      "Roots + Vegetables" = "darkseagreen3",
      "Roots + Tropical_fruits" = "lightsalmon",
      "Roots + Temperate_fruits" = "lightskyblue2"
    ),
    name = 'Crop Type'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(Roots_two_category_plot)
ggsave((filename = "3_output/2_map/2_All crop category/2010_Roots with another crop.png"), plot = Roots_two_category_plot, 
       width = 8, height = 5.2, limitsize = FALSE)

#****************************************************************************************************
# 初步可视化分析 -- Pulses复合种植
#****************************************************************************************************
Pulses_two_category <- droplevels(subset(H_category_2010, H_category_2010$category %in% 
                                           c("Pulses + Fibre", "Cereals + Pulses", "Pulses + Rest",
                                             "Roots + Pulses", "Pulses + Vegetables", "Pulses + Oil",
                                             "Pulses + Temperate_fruits", "Pulses + Tropical_fruits")))

Pulses_two_category_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = Pulses_two_category ,
    aes(x, y, fill = category),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "Roots + Pulses" = "mediumpurple1", 
      "Cereals + Pulses" = "seagreen", 
      "Pulses + Fibre" = "indianred",
      "Pulses + Oil" = "olivedrab1",
      "Pulses + Rest" = "lightsalmon4",
      "Pulses + Vegetables" = "darkseagreen3",
      "Pulses + Tropical_fruits" = "lightsalmon",
      "Pulses + Temperate_fruits" = "lightskyblue2"
    ),
    name = 'Crop Type'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(Pulses_two_category_plot)
ggsave((filename = "3_output/2_map/2_All crop category/2010_Pulses with another crop.png"), plot = Pulses_two_category_plot, 
       width = 8, height = 5.2, limitsize = FALSE)

#****************************************************************************************************
# Cereals + Roots + Pulses + Oil + Fibre + Tropical_fruits + Vegetables + Rest
#****************************************************************************************************
Eight_category <- droplevels(subset(H_category_2010, H_category_2010$category %in% 
                                      c("Cereals + Roots + Pulses + Oil + Fibre + Tropical_fruits + Vegetables + Rest")))

Eight_category_plot <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", linewidth = 0.1
  ) +
  geom_tile(
    data = Eight_category ,
    aes(x, y, fill = category),  # Use crop for fill
    alpha = 0.7,
    color = NA  # No border color for tiles
  ) +
  scale_fill_manual(
    values = c(
      "Cereals + Roots + Pulses + Oil + Fibre + Tropical_fruits + Vegetables + Rest" = "darkseagreen3" ),
    name = 'Crop Type'  # Custom legend title
  ) + 
  theme(legend.position = "bottom")

print(Eight_category_plot)
ggsave((filename = "3_output/2_map/2_All crop category/2010_Eight categories.png"), plot = Eight_category_plot, 
       width = 8, height = 5.2, limitsize = FALSE)
