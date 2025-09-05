#======================#
# Laura Camila Ortiz Perez
# CDATE:  2025-09-05
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, tidyverse, fixest, modelsummary, stargazer())

#==== import data ====# 

#for table: 
model1 = import("04_regression/03_output/model1_reg_gap_female.rds")
model2 = import("04_regression/03_output/model4_reg_gap_female.rds")
model4 = import("04_regression/03_output/model5_reg_gap_female.rds")


#table


stargazer(model1, model2, model4,
          type = "text",                      # "latex" o "html"
          dep.var.labels = "log salario por hora",
          covariate.labels = c("Mujer","Edad","Edad^2","Educación","Ocupación",
                               "Relación laboral","Tamaño firma (dums)","Industria (dums)","Año (dums)"),
          omit.stat = c("f","ser"),
          no.space = TRUE,
          title = "Brecha salarial de género: tres especificaciones",
          notes = "Errores estándar OLS entre paréntesis.",
          star.cutoffs = c(.1, .05, .01)
)