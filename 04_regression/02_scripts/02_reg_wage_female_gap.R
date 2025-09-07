#______________________
# Laura Camila Ortiz 
# Fecha: 31 de agosto 2025
#_____________________

#---------------------#
# ==== 0. Setup ====
#---------------------#

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, janitor,data.table,caret,stargazer,boot,fixest)

#---------------------#
# ==== 1. import ====
#---------------------#

df = import('02_prepare_data/03_output/01_main_data.rds', setclass = 'tibble') 
colnames(df)


#------------------------------#
# ==== 2. model regression ====
#------------------------------#

model1 = feols(log(y_total_m_ha) ~ factor(sex), data = df) 
model_sum = summary(model1)
model_sum


export(model1, "04_regression/03_output/model1_reg_gap_female.rds")



#----------------------------------#
# ==== 3. FWL model regression ==== 
#-----------------------------------#


model2 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + relab + oficio + formalidad + size_firm , data = df) 
summary(model2)


export(model2, "04_regression/03_output/model2_reg_gap_female.rds")


model3 = feols(log(y_total_m_ha) ~ sex + age + I(age^2)  + max_educ_level + relab + oficio + formalidad + size_firm + total_menores + total_seniors_inactivos, data = df) 
summary(model3)

export(model3, "04_regression/03_output/model3_reg_gap_female.rds")

## ===== 3.1 first model =====

m_y <- lm(log(y_total_m_ha) ~ age + I(age^2) + max_educ_level + relab + oficio + formalidad + size_firm, data = df)
m_x <- lm(sex ~  age + I(age^2) + max_educ_level + relab + oficio + formalidad + size_firm , data = df)

df$YResid      <- resid(m_y)
df$FemaleResid <- resid(m_x)

model4 <- feols(YResid ~ FemaleResid, data = df)
summary(model4)
export(model4, "04_regression/03_output/model4_reg_gap_female.rds")


## ==== 3.2 second model ====


m_y <- lm(log(y_total_m_ha) ~ age + I(age^2)  + max_educ_level + relab + oficio + formalidad + size_firm + total_menores + total_seniors_inactivos, data = df)
m_x <- lm(sex ~   age + I(age^2)  + max_educ_level + relab + oficio + formalidad + size_firm + total_menores + total_seniors_inactivos, data = df)

df$YResid      <- resid(m_y)
df$FemaleResid <- resid(m_x)

model5 <- feols(YResid ~ 0 + FemaleResid, data=df)
summary(model5)
export(model5, "04_regression/03_output/model5_reg_gap_female.rds")


#=======================================#
# ==== 4. FWL: regresión de residuos ====
#=======================================#

# Dummy
df$female <- as.integer(df$sex %in% c("Femenino"))

# new df

df_boot <- data.frame(
  y_total_m_ha = df$y_total_m_ha,
  female = df$female,
  age = df$age,
  max_educ_level = df$max_educ_level,
  relab = df$relab,
  oficio = df$oficio,
  formalidad = df$formalidad,
  size_firm = df$size_firm,
  total_menores = df$total_menores,
  total_seniors_inactivos = df$total_seniors_inactivos
)

fwl_fn <- function(data, indices) {
  d <- data[indices, , drop = FALSE]  
  
  y_res <- resid(lm(log(y_total_m_ha) ~ age + I(age^2) + max_educ_level + relab +
                      oficio + formalidad + size_firm + total_menores +
                      total_seniors_inactivos, data = d))
  
  x_res <- resid(lm(female ~ age + I(age^2) + max_educ_level + relab +
                      oficio + formalidad + size_firm + total_menores +
                      total_seniors_inactivos, data = d))
  
  unname(coef(lm(y_res ~ x_res))[2])  
}

set.seed(324)
boot_fwl <- boot(df_boot, statistic = fwl_fn, R = 1000)
boot_fwl$t0           # estimación puntual
sd(boot_fwl$t)        # EE bootstrap
quantile(boot_fwl$t, c(.025,.975))  # IC 95%
