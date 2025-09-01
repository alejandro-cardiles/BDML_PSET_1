#______________________
# Laura Camila Ortiz 
# Fecha: 31 de agosto 2025
#_____________________

#---------------------#
# ==== 0. Setup ====
#---------------------#

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, data.table,caret)

#---------------------#
# ==== 1. import ====
#---------------------#

df = import('02_prepare_data/03_output/01_main_data.rds', setclass = 'tibble') 
colnames(df)

#------------------------------#
# ==== 2. model regression ====
#------------------------------#

model1 = lm(log(y_total_m_ha) ~ female, data = df) 
model_sum = summary(model1)
model_sum

#modelsummary(model, output = "latex")

str(df[, c("age", "female", "max_educ_level")])

df = df %>% mutate(female = as.factor(female),
                   max_educ_level = as.factor(max_educ_level))

#----------------------------------#
# ==== 3. FWL model regression ====
#-----------------------------------#

# age, maxeduclevel informal

auto<-auto %>% mutate(FemaleResidF=lm(Female~,auto)$residuals) #Residuals of weight~foreign 







