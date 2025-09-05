#______________________
# Laura Camila Ortiz 
# Fecha: 31 de agosto 2025
#_____________________

#---------------------#
# ==== 0. Setup ====
#---------------------#

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, janitor,data.table,caret,stargazer,boot)

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

export(model1, "04_regression/03_output/model1_reg_gap_female.rds")

#modelsummary(model, output = "latex")

str(df[, c("age", "sex", "max_educ_level", "formalidad")])

df = df %>% mutate(sex = as.numeric(sex),
                   max_educ_level = as.factor(max_educ_level))

#----------------------------------#
# ==== 3. FWL model regression ====
#-----------------------------------#

df <- df %>%  mutate(female = as.integer(sex %in% c("Femenino")))


model2 = lm(log(y_total_m_ha) ~ female + age + I(age^2) + max_educ_level + oficio + relab + size_firm + formalidad, data = df) 
model2_sum = summary(model2)
model2_sum

export(model2, "04_regression/03_output/model2_reg_gap_female.rds")


model3 = lm(log(y_total_m_ha) ~ female + age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = df) 
model3_sum = summary(model3)
model3_sum

export(model3, "04_regression/03_output/model3_reg_gap_female.rds")

## ===== 3.1 first model =====

m_y <- lm(log(y_total_m_ha) ~ age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = df)
m_x <- lm(female ~ age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = df)

df$YResid      <- resid(m_y)
df$FemaleResid <- resid(m_x)

model4 <- lm(YResid ~ FemaleResid, data = df)

export(model4, "04_regression/03_output/model4_reg_gap_female.rds")

## ==== 3.2 second model ====


df <- df %>% mutate(FemaleResidF = lm(log(y_total_m_ha) ~ age + I(age^2) + max_educ_level + oficio + relab + size_firm, df)$residuals) #Residuals of weight~foreign 
df <- df %>% mutate(CovResidF = lm(female ~ age + I(age^2) + max_educ_level + oficio + relab + size_firm, df)$residuals) #Residuals of mpg~foreign 

model5 <-lm(FemaleResidF ~ CovResidF,df)
model5_sum = summary(model5)
model5_sum
export(model5, "04_regression/03_output/model5_reg_gap_female.rds")

#=======================================#
# ==== 4. FWL: regresión de residuos ====
#=======================================#

# Define bootstrap function

d <- df[indices, ]

fwl_fn <- function(data, indices) {
  d <- data[indices, ]
  y_res <- lm(log(y_total_m_ha) ~  age + I(age^2) + max_educ_level + oficio + 
                relab + size_firm + formalidad, data = d)$residuals
  x_res <- lm(female ~  age + I(age^2) + max_educ_level + oficio + relab + 
                size_firm + formalidad, data = d)$residuals
  return(coef(lm(y_res ~ x_res))[2])  # Coefficient of female_resid
}


# Run bootstrap (e.g. 1000 reps)
set.seed(324)
boot_fwl <- boot(df, statistic = fwl_fn, R = 1000)

# Estimate & SE
boot_fwl$t0             # point estimate
sd(boot_fwl$t)          # bootstrap SE
boot.ci(boot_fwl, type = "perc")



