#======================#
# Yesenia Fontalvo
# CDATE:  2025-08-31
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, janitor, stats, tidyverse, data.table, boot, ggplot())

#==== import data ====# 
data = import("01_import/03_output/01_data_scrapping_web_page.rds") #ajustar para halarlo de preparar data

#==== run the reg ====# 
model = lm(log(y_total_m_ha) ~ age + I(age^2), data = data) 
model_sum = summary(model)

#==== in sample fit (metric) ====# 
r2     = model_sum$r.squared
r2adj  = model_sum$adj.r.squared
rmse   = sqrt(mean(residuals(model_sum)^2))
mae    = mean(abs(residuals(model_sum)))
aic_v  = AIC(model_sum)
bic_v  = BIC(model_sum)

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
                        wage = exp(predict(model, newdata = data.frame(age = age_seq)))) #predigo para el vector de edades

peak_point = data.frame(age  = peak_est,
                         wage = exp(predict(model, newdata = data.frame(age = peak_est)))) # prediction for peak age

# plot
plot = ggplot(plot_data, aes(age, wage)) +
        geom_line(color = "blue", size = 1) +
        geom_vline(xintercept = peak_point$age, linetype = "dashed", color = "darkgreen") +
        geom_point(data = peak_point, aes(age, wage), color = "darkgreen", size = 3) +
        labs(title = "Perfil Edad–Ingresos estimado",
             x = "Edad", y = "Ingresos predichos") +
        theme_classic()

#==== export  ====# 
ggsave(plot, filename = "04_regression/03_output/plot_reg_wage_age.png")
