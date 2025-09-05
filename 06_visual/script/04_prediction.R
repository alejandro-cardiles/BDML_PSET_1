#------------------------#
# Alejandro Cardiles 
# 2025-09-05      
#------------------------#

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, tidymodels, yardstick, fixest, kableExtra)
options(scipen =  999)


##==: 1. Import data
data = import("02_prepare_data/03_output/01_main_data.rds")
resultados_modelos = import("05_prediction/03_output/01_test_metrics.rds")

##==: 2. test vs train set     

# separar por grupos 
set.seed(10101)
split = initial_split(data, prop = 0.7)
train = training(split)
test = testing(split)

##==: 3. tabla de error  
tabla = lapply(X = 1:length(resultados_modelos), FUN = function(x) {
  return(resultados_modelos[[x]]$metric)
}) |> bind_rows()

tabla = tabla %>%
        filter(.metric == "rmse") |> 
        mutate(.metric = paste0("Modelo ", row_number()))

# pasar a latex
tabla = tabla |> 
        mutate(.estimate = as.character(round(.estimate, 2)), 
              model_parameters = as.character(model_parameters)) |> 
        pivot_longer(cols = c(.estimate, model_parameters)) |> 
        pivot_wider(names_from = ".metric", values_from = "value") |> 
        select(-.estimator) |> 
        mutate(name = c("RMSE", "Parametros"))

tabla = tabla |> 
        kable(format = "latex", escape = F, booktabs = T, longtable = T, col.names = c("", names(tabla)[-1]), align = c("lc")) |> 
        row_spec(0, bold = T) 

##==: 4. grafico de residuos
writeLines(text = tabla,con = "06_visual/output/04_prediction_table.tex")
