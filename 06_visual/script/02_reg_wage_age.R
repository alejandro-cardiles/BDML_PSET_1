#======================#
# Yesenia Fontalvo
# CDATE:  2025-08-31
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, tidyverse, fixest)

#==== import data ====# 

#for table: 
model = import("04_regression/03_output/model_reg_wage_age.rds")

#for graph: 
plot_data = import("04_regression/03_output/plot_data_reg_wage_age.xlsx")
peak_point = import("04_regression/03_output/peak_point_reg_wage_age.xlsx")

#==== visual point  ====#

#table

etable(model,
       dict = c("(Intercept)" = "Constante",
                 "age"         = "Edad",
                 "I(age^2)"    = "Edad^2"),
       depvar = TRUE,
       digits = 3,
       title = "OLS estimation, Dep. Var.: log(ingreso mensual por horas)", 
       file="06_visual/output/02_model_wage_age.tex")

# plot
plot = ggplot(plot_data, aes(age, wage))+
       expand_limits(y = max(plot_data$wage) * 1.1) +
       geom_line(aes(color = "Ingreso predicho"), size = 1) +
       geom_segment(aes(x = peak_point$age, xend = peak_point$age, y = 0, yend = peak_point$wage, color = "Edad pico"),
                   linetype = "longdash") +
       geom_point(data = peak_point, aes(age, wage, color = "Edad pico"), size = 3) +
       geom_text(data = peak_point, aes(x = age, y = wage, label = round(age, 1)),
                vjust = -1.2, hjust = 0.6,
                color = "red", fontface = "italic", inherit.aes = FALSE) +
       scale_color_manual(name = "Leyenda",
                         values = c("Ingreso predicho" = "blue", "Edad pico" = "red")) +
       labs(title = "",
             x = "Edad", 
             y = "Ingresos predichos\n por horas (COP)") +
       theme_classic(); plot

#==== export  ====# 
ggsave(plot, filename = "06_visual/output/02_plot_reg_wage_age.png",   width = 8, height = 4, units = "in", dpi = 300)
