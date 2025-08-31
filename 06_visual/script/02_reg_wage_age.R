#======================#
# Yesenia Fontalvo
# CDATE:  2025-08-31
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, tidyverse)

#==== import data ====# 

#for table: 
model = import("04_regression/03_output/model_reg_wage_age.rds")

#for graph: 
plot_data = import("04_regression/03_output/plot_data_reg_wage_age.xlsx")
peak_point = import("04_regression/03_output/peak_point_reg_wage_age.xlsx")

#==== visual point  ====#

#table





# plot
plot = ggplot(plot_data, aes(age, wage)) +
        geom_line(color = "blue", size = 1) +
        geom_vline(xintercept = peak_point$age, linetype = "dashed", color = "darkgreen") +
        geom_point(data = peak_point, aes(age, wage), color = "darkgreen", size = 3) +
        labs(title = "Perfil Edad–Ingresos estimado",
             x = "Edad", y = "Ingresos predichos") +
        theme_classic()

#==== export  ====# 
ggsave(plot, filename = "06_visual/output/plot_reg_wage_age.png")
