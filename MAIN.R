# setup
rm(list = ls())
if (require("pacman") == F){install.packages("pacman")}else{require("pacman")}

pacman::p_load(tidyverse)
# run code
list = c(# 01_importar
         "01_import/02_scripts/01_scrapping_data_ignacio_page.R",
         
         # 02_prepare_data
         "02_prepare_data/02_scripts/01_prepare_data.R", 
         
         # 03_regression
         "03_regression/02_scripts/01_reg_age_wage.R", 
         "03_regression/02_scripts/02_reg_wage_female_gap.R", 
         "03_regression/02_scripts/03_peak_gender_age.R", 
         
         #04_prediction
         "04_prediction/02_scripts/01_prediction.R", 
         "04_prediction/02_scripts/02_prediction_without_high_leveregae_point.R",

         # 05_visuals
         "05_visual/02_script/01_descriptives.R",
         "05_visual/02_script/02_reg_wage_age.R",
         "05_visual/02_script/03_reg_gender_gap.R",
         "05_visual/02_script/04_prediction.R",
         "05_visual/02_script/05_diff_medias_tabla.R")

run = walk(.x = list, .f = function(x){
  print(paste0("Running: ", x))
  Sys.sleep(3)
  source(x)}
)
