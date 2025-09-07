#======================#
# Laura Camila Ortiz Perez
# CDATE:  2025-09-05
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, tidyverse, fixest, modelsummary, stargazer, leaps)

#==== import data ====# 

#for table: 
model1 = import("03_regression/03_output/02_model1_reg_gap_female.rds")
model4 = import("03_regression/03_output/02_model4_reg_gap_female.rds")
model5 = import("03_regression/03_output/02_model5_reg_gap_female.rds")

# for graph
plot_data = import( "03_regression/03_output/03_plot_data.rds")
peak_point = import( "03_regression/03_output/03_peak_points.rds")


#table

etable(model1, model4, model5,
       dict = c("log(y_total_m_ha)" = "ln(salario por hora)",
                "(Intercept)" = "Constante",
                "factor(sex)Femenino" = "Mujer",
                "FemaleResid" = "Residuales Mujer",
                "YResid"      = "Residuales de salario"),  
       extralines = list("Controles laborales" = c("No", "Sí", "Sí"),
                         "Controles de cuidado" = c("No", "No", "Sí") ),
       depvar = TRUE,
       digits = 3,
       title = "Resultados de la estimación", 
       fitstat = ~  n + r2 + ar2 + rmse,   # nombres correctos en fixest
       notes = c(
         "Errores estándar robustos en paréntesis. *** p<0.01, ** p<0.05, * p<0.1.",
         "Controles laborales: edad, edad$^2$, nivel educativo, relación laboral, oficio y tamaño de la firma.",
         "Controles de cuidado: número de menores en el hogar y número de mayores inactivos.",
         "La columna (1) es el modelo base; (2) añade controles laborales; (3) añade controles de cuidado."
       ),                 
       file = "05_visual/03_output/03_model_gender_gap.tex", replace = TRUE)




# Gráfico
# Asegúrate que los nombres coincidan exactamente con plot_data$sex y peak_point$sexo
pal_line  <- c("Femenino" = "#800080", "Masculino" = "#0000CD")
pal_fill  <- c("Femenino" = "#D1B3D8", "Masculino" = "#B3C5FF")  # tonos suaves


plot <- ggplot() +
  # Bandas por sexo
  geom_ribbon(data = plot_data,
              aes(x = age, ymin = ci_lower, ymax = ci_upper, fill = sex),
              alpha = 0.2, color = NA) +
  
  # Curvas por sexo
  geom_line(data = plot_data,
            aes(x = age, y = wage, color = sex),
            linewidth = 0.3) +
  
  # Líneas de pico (mismo color que el punto, pero sin leyenda)
  geom_vline(data = peak_point,
             aes(xintercept = peak_age, color = sexo),
             linetype = "longdash", linewidth = 0.3, show.legend = FALSE) +
  
  # Puntos de pico con categorías especiales para la leyenda
  geom_point(data = peak_point,
             aes(x = peak_age, y = peak_wage,
                 color = ifelse(sexo == "Femenino", "Edad pico M", "Edad pico H")),
             size = 2, inherit.aes = FALSE) +
  
  geom_text(data = peak_point,
            aes(x = peak_age, y = peak_wage,
                label = round(peak_age, 1),
                color = "black"),
            vjust = -1.2, fontface = "bold",
            inherit.aes = FALSE, show.legend = FALSE) +
  
  # Paletas y etiquetas de leyenda
  scale_color_manual(
    values = c("Femenino"        = "#800080",   # morado
               "Masculino"       = "#0000CD",   # azul rey
               "Edad pico M"     = "#800080",
               "Edad pico H"     = "#0000CD"),
    labels = c("Femenino"    = "Ingreso predicho M",
               "Masculino"   = "Ingreso predicho H",
               "Edad pico M" = "Edad pico M",
               "Edad pico H" = "Edad pico H")
  ) +
  scale_fill_manual(
    values = c("Femenino" = "#E5CCF2", "Masculino" = "#BFD1FF"), guide = "none") +
  
  labs(x = "Edad", y = "Ingreso predicho \npor hora (COP)", color = NULL, fill = NULL) +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 7.5)   # más pequeña la leyenda
  ) +
  guides(color = guide_legend(override.aes = list(size = 2)))
plot
#==== export  ====# 
ggsave(plot, filename = "05_visual/03_output/03_plot_reg_wage_age_gender.png",   width = 8, height = 4, units = "in", dpi = 300)
