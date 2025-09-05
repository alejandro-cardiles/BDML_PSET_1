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
                "I(age^2)"    = "Edad^2",
                "log(wage)"   = "log(salario)"),  
       depvar = TRUE,
       digits = 3,
       title = "Resultados de la estimación", 
       fitstat = ~ r2 + ar2 + rmse,   # nombres correctos en fixest
       notes = NULL,                      
       file = "06_visual/output/02_model_wage_age.tex")


# plot
plot <- ggplot(plot_data, aes(x = age, y = wage)) +
        expand_limits(y = max(plot_data$ci_upper) * 1.1) +
        
        # Banda de confianza bootstrap
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                    fill = "darkgray", alpha = 0.3) +
        
        # Bordes del intervalo
        geom_line(aes(y = ci_lower, color = "Límite inferior"), linetype = "dashed") +
        geom_line(aes(y = ci_upper, color = "Límite superior"), linetype = "dashed") +
        
        # Línea central (curva de ingresos)
        geom_line(aes(color = "Ingreso predicho"), size = 1) +
        
        # Línea vertical en la edad pico
        geom_segment(aes(x = peak_point$peak_age, xend = peak_point$peak_age, 
                         y = 0, yend = peak_point$peak_wage, 
                         color = "Edad pico"),
                     linetype = "longdash") +
        
        # Punto en la edad pico
        geom_point(data = peak_point, 
                   aes(x = peak_age, y = peak_wage, color = "Edad pico"), 
                   size = 3, inherit.aes = FALSE) +
        
        # Texto con el valor de la edad pico
        geom_text(data = peak_point, 
                  aes(x = peak_age, y = peak_wage, label = round(peak_age, 1)), 
                  vjust = -1.2, hjust = 0.6,
                  color = "black", fontface = "italic", inherit.aes = FALSE) +
        
        # Colores de la leyenda
        scale_color_manual(name = "Leyenda",
                           values = c("Ingreso predicho" = "blue", 
                                      "Edad pico" = "red",
                                      "Límite inferior" = "black",
                                      "Límite superior" = "black")) +
        
        # Etiquetas
        labs(title = "",
             x = "Edad", 
             y = "Ingresos predichos\npor hora (COP)") +
        
        theme_classic()

plot



#==== export  ====# 
ggsave(plot, filename = "06_visual/output/02_plot_reg_wage_age.png",   width = 8, height = 4, units = "in", dpi = 300)
