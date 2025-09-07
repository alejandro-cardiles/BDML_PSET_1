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
resultados_modelos_sin_outlieres = import("05_prediction/03_output/02_test_metrics_without_high_leverage.rds")

##==: 2. test vs train set     

# separar por grupos 
set.seed(10101)
split = initial_split(data, prop = 0.7)
train = training(split)
test = testing(split)

##==: 3. tabla de error  
tabla = lapply(X = 1:length(resultados_modelos), FUN = function(x) {
        metric = resultados_modelos[[x]]$metric  
        modelo = resultados_modelos[[x]]$modelo

        h <- hatvalues(modelo)       
        h[h>0.9999999] = NA
        e <- residuals(modelo)          
        loo_resid <- e / (1 - h)     
        loo_err   <- mean(loo_resid^2, na.rm = T)  

        metric = metric |> 
                 mutate(loocv = loo_err, )


}) |> bind_rows() |> 
      filter(.metric == "rmse")  |> 
      mutate(.metric = paste0("Modelo ", row_number())) |> 
      select(-.estimator)

tabla_sin_outliers = lapply(X = 1:length(resultados_modelos_sin_outlieres), FUN = function(x) {
        metric = resultados_modelos_sin_outlieres[[x]]$metric  
        modelo = resultados_modelos_sin_outlieres[[x]]$modelo

        h <- hatvalues(modelo)       
        h[h>0.9999999] = NA
        e <- residuals(modelo)          
        loo_resid <- e / (1 - h)     
        loo_err   <- mean(loo_resid^2, na.rm = T)  

        metric = metric |> 
                mutate(loocv = loo_err)


}) |> bind_rows() |> 
      filter(.metric == "rmse")  |> 
      mutate(.metric = paste0("Modelo ", row_number())) |> 
      select(-.estimator)

tabla = left_join(tabla, tabla_sin_outliers, by =c(".metric") )

# pasar a latex
tabla = tabla |> 
        mutate(.estimate.x = formatC(.estimate.x, format = "f", digits = 2),
               loocv.x = formatC(loocv.x, format = "f", digits = 2),
               .estimate.y = formatC(.estimate.y, format = "f", digits = 2), 
               loocv.y = formatC(loocv.y, format = "f", digits = 2)) 

tabla = tabla |> 
        select(.metric, loocv.x, loocv.y, .estimate.x, .estimate.y )   |>     
        kable(format = "latex", 
              col.names = c("", "Con outliers", "Sin outliers", "Con outliers", "Sin outliers"), 
              align = c("lccccc"),
              booktabs = TRUE) |> 
        add_header_above(header = c("", "LOOCV RMSE" = 2, "Test RMSE" = 2)) |> 
        kable_styling(latex_options = "hold_position", full_width = F) |> 
        footnote(
                general = c(
                paste0("Train Obs. (Outliers-No outliers): ", resultados_modelos[[9]]$train_size, "-", resultados_modelos_sin_outlieres[[9]]$train_size), 
                "Dos observaciones fueron eliminadas en LOOCV para evitar leverage de 1 en el test set"
                ),
                general_title = "", 
                escape = FALSE
        )

##==: 4. grafico de residuos
writeLines(text = tabla,con = "06_visual/output/04_prediction_table.tex")
