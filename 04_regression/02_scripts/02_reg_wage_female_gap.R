#______________________
# Laura Camila Ortiz 
# Fecha: 31 de agosto 2025
#_____________________

#---------------------#
# ==== 0. Setup ====
#---------------------#

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, janitor,data.table,caret,stargazer)

#---------------------#
# ==== 1. import ====
#---------------------#

df = import('02_prepare_data/03_output/01_main_data.rds', setclass = 'tibble') 
colnames(df)


#------------------------------#
# ==== 2. model regression ====
#------------------------------#

model1 = lm(log(y_total_m_ha) ~ sex, data = df) 
model_sum = summary(model1)
model_sum


#modelsummary(model, output = "latex")

str(df[, c("age", "sex", "max_educ_level", "formalidad")])

df = df %>% mutate(sex = as.numeric(sex),
                   max_educ_level = as.factor(max_educ_level))

#----------------------------------#
# ==== 3. FWL model regression ====
#-----------------------------------#

model2 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + formalidad, data = df) 
model2_sum = summary(model2)
model2_sum

# age, maxeduclevel informal

df <- df %>% mutate(FemaleResidF = lm(sex ~ age + I(age^2) + max_educ_level 
                                      + oficio + formalidad, df)$residuals) #Residuals of weight~foreign 

df <- df %>% mutate(CovResidF = lm( log(y_total_m_ha) ~ age + I(age^2) + 
                                      max_educ_level +  oficio + formalidad, df)$residuals) #Residuals of mpg~foreign 

model3 <-lm(CovResidF~FemaleResidF,df)

stargazer(model2,model3,type="text",digits=7) 


library(boot)

# Define bootstrap function
fwl_fn <- function(data, indices) {
  d <- data[indices, ]
  y_res <- lm(log(y_total_m_ha) ~ age + I(age^2) + max_educ_level + oficio + formalidad, data = d)$residuals
  x_res <- lm(sex ~ age + I(age^2) + max_educ_level + oficio + formalidad, data = d)$residuals
  return(coef(lm(y_res ~ x_res))[2])  # Coefficient of female_resid
}

# Run bootstrap (e.g. 1000 reps)
set.seed(123)
boot_fwl <- boot(df, statistic = fwl_fn, R = 1000)

# Estimate & SE
boot_fwl$t0             # point estimate
sd(boot_fwl$t)          # bootstrap SE
boot.ci(boot_fwl, type = "perc")



