#======================#
# Yesenia Fontalvo
# CDATE:  2025-08-31
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, tidyverse, fixest,magick,pdftools)

#==== import data ====# 

#for table: 
model = import("03_regression/03_output/01_model_reg_wage_age.rds")

#for graph: 
plot_data = import("03_regression/03_output/01_plot_data_reg_wage_age.xlsx")
peak_point = import("03_regression/03_output/01_peak_point_reg_wage_age.xlsx")

#==== visual point  ====#

#table
etable(model,
       dict = c("(Intercept)" = "Constante",
                "age"         = "Edad",
                "I(age^2)"    = "Edad^2",
                "log(y_total_m_ha)"   = "log(Ingreso laboral por hora)"),  
       depvar = TRUE,
       digits = 3,
       title = "Resultados de la estimación",
       view = T,tex = T,
       fitstat = ~ r2 + ar2 + rmse, file = "05_visual/03_output/02_model_wage_age.tex", replace = T)

# plot
plot <- ggplot(plot_data, aes(x = age, y = wage)) +
        scale_y_continuous(limits = c(1000, 6400), breaks = seq(1000,6400, by = 1000), labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
        scale_x_continuous(limits = c(18, 92), breaks = seq(10,92, by = 10)) + 
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                    fill = "darkgray", alpha = 0.3) +
        geom_line(aes(y = ci_lower, color = "Límite inferior"), linetype = "dashed") +
        geom_line(aes(y = ci_upper, color = "Límite superior"), linetype = "dashed") +
        geom_line(aes(color = "Ingreso predicho"), size = 1) +
        geom_segment(data = peak_point,
               aes(x = peak_age, xend = peak_age,
                   y = 1000, yend = peak_wage, color = "Edad pico"),
               linetype = "longdash", size = 0.7) +
        geom_point(data = peak_point, 
                   aes(x = peak_age, y = peak_wage, color = "Edad pico"), 
                   size = 3, inherit.aes = FALSE) +
        geom_text(data = peak_point, 
                  aes(x = peak_age, y = peak_wage, label = round(peak_age, 1)), 
                  vjust = -1.2, hjust = 0.6,
                  color = "black", fontface = "italic", inherit.aes = FALSE) +
        scale_color_manual(name = NULL,
                           values = c("Ingreso predicho" = "blue", 
                                      "Edad pico" = "red",
                                      "Límite inferior" = "black",
                                      "Límite superior" = "black")) +
        labs(title = "",
             x = "Edad", 
             y = "Ingresos predichos\npor hora (COP)") +
        
        theme_classic() 

plot



#==== export  ====# 
ggsave(plot, filename = "05_visual/03_output/02_plot_reg_wage_age.png",   width = 8, height = 4.5, units = "in", dpi = 700)
