#======================#
# Yesenia Fontalvo
# CDATE:  2025-08-31
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, janitor, stats, tidyverse, data.table, boot, fixest)

#==== import data ====# 
data = import("02_prepare_data/03_output/01_main_data.rds", #ajustar para halarlo de preparar data
              setclass = 'tibble') 

#==== run the reg ====# 
model = feols(log(y_total_m_ha) ~ age + I(age^2), data = data) 


#==== in sample fit (metric) ====# 
model_sum = summary(model)

#==== function peak age ====# 
peak.fn = function(data, index) {
      d = data[index, ] 
      f = lm(log(y_total_m_ha) ~ age + I(age^2), data = d)
      b = coef(f)
      if (b["I(age^2)"] < 0) { 
        return(-b["age"] / (2 * b["I(age^2)"]))
      } else {
        return(NA) 
      }
    }

set.seed(2003)
boot_peak = boot(data = data, statistic = peak.fn, R = 1000)

#==== peak age ====# 
peak_est = peak.fn(data, 1:nrow(data))

#==== IC boostrap ====# 
ci_peak = boot.ci(boot_peak, type = "perc")

#==== visual - graph  ====# 
plot_data = data.frame(age = seq(min(data$age), max(data$age), 1),
                        wage = exp(predict(model, newdata = data.frame(age = seq(min(data$age), max(data$age), 1))))) #predigo para el vector de edades

peak_point = data.frame(age  = peak_est,
                         wage = exp(predict(model, newdata = data.frame(age = peak_est)))) # prediction for peak age

#==== export  ====# 
export(plot_data, "04_regression/03_output/plot_data_reg_wage_age.xlsx")
export(peak_point, "04_regression/03_output/peak_point_reg_wage_age.xlsx")
export(model, "04_regression/03_output/model_reg_wage_age.rds")
