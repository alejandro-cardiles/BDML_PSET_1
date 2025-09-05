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
peak.fn <- function(data, index) {
      d <- data[index, ] 
      f <- lm(log(y_total_m_ha) ~ age + I(age^2), data = d)
      b <- coef(f)
      if (b["I(age^2)"] < 0) { 
        # edad pico
        age_star <- -b["age"] / (2 * b["I(age^2)"])
        # salario en esa edad pico
        wage_star <- exp(predict(f, newdata = data.frame(age = age_star)))
        return(c(age = age_star, wage = wage_star))
      } else {
        return(c(age = NA, wage = NA))
      }
    }


set.seed(2003)
boot_peak = boot(data = data, statistic = peak.fn, R = 1000)

#==== peak age ====# 
peak_est = peak.fn(data, 1:nrow(data))

# CI peak_age
ci_peak_age <- quantile(boot_peak$t[,1], probs = c(0.025, 0.975), na.rm = TRUE)

# CI_peak_wage
ci_peak_wage <- quantile(boot_peak$t[,2], probs = c(0.025, 0.975), na.rm = TRUE)

peak_point <- data.frame(peak_age   = peak_est["age.age"],
                         peak_wage  = peak_est["wage.age"],
                         ci_wage_lo = ci_peak_wage[1],
                         ci_wage_hi = ci_peak_wage[2])

#==== IC Boostrap wage - age ====#
age = seq(min(data$age), max(data$age), 1)

predict_age.fn =function(data, index, ages) {
              dat =data[index, ]
              mod =lm(log(y_total_m_ha) ~ age + I(age^2), data = dat)
              pred =predict(mod, newdata = data.frame(age = ages))
              return(exp(pred))  # pasamos a nivel
            }

set.seed(2003)

boot_pred =boot(
            data = data, 
            statistic = function(d, i) predict_age.fn(d, i, age), 
            R = 1000)


ci_wage_age =apply(boot_pred$t, 2, function(x) {
             quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
            })

# Añadir columnas al plot_data
age = as.data.frame(age)
plot_data = data.frame(age, wage = exp(predict(model, newdata = age)))
plot_data$ci_lower =ci_wage_age[1, ]
plot_data$ci_upper =ci_wage_age[2, ]


#==== export  ====# 
export(plot_data, "04_regression/03_output/plot_data_reg_wage_age.xlsx")
export(peak_point, "04_regression/03_output/peak_point_reg_wage_age.xlsx")
export(model, "04_regression/03_output/model_reg_wage_age.rds")
