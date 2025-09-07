#------------------------#
# Alejandro Cardiles 
# 2025-09-05      
#------------------------#

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, tidymodels, kableExtra, janitor)
options(scipen =  99999)


##==: 1. Import data
data = import("02_prepare_data/03_output/01_main_data.rds")
resultados_modelos = import("05_prediction/03_output/01_test_metrics.rds")

##==: 2. test vs train set     

# separar por grupos 
set.seed(10101)
split = initial_split(data, prop = 0.7)
train = training(split)
test = testing(split)

##==: 3. outliers

# agregar columna de outliers a el conjunto de test
test = test |> 
        mutate(predicted = resultados_modelos[[9]]$resul$prediction,
               mse = resultados_modelos[[9]]$resul$mse)

# calcular cuartiles
q1 = quantile(test$mse, 0.25)
q3 = quantile(test$mse, 0.75)
iqr = q3 - q1

# definir outliers
test = test |> 
        mutate(outliers = case_when(mse > (q3 + 1.5*iqr) ~ "over", 
                                    mse < (q1 - 1.5*iqr) ~ "under", 
                                    TRUE ~ "No"))
test$outliers = factor(test$outliers, levels = c("No", "over", "under"))

test = test |> 
       mutate(y_total_m_ha_log = log(y_total_m_ha))

rm(iqr, q1, q3)

##==: 4. table de diferencia de medias para continuas

## funcion para sacar tabla de diferencia de medias en valores continuos
c_test = function(data, interes_col, outlier_col){
  
        # test for over and under
        data = data |> rename(interes_col = interes_col, outlier_col = outlier_col)
        
        over = t.test(formula = interes_col ~ outlier_col, data = data |> filter(outlier_col %in% c("over","No")), alternative = "two.sided")         
        under = t.test(formula = interes_col ~ outlier_col, data = data |> filter(outlier_col %in% c("under","No")), alternative = "two.sided")         

        # statistics
        base_mean = over$estimate["mean in group No"] |> as.numeric()
        base_sd = sd(data$interes_col[data$outlier_col == "No"])
     
        over_mean = over$estimate["mean in group over"]  |> as.numeric()
        over_sd = sd(data$interes_col[data$outlier_col == "over"])

        under_mean = under$estimate["mean in group under"] |> as.numeric()
        under_sd = sd(data$interes_col[data$outlier_col == "under"])
  
        diff_1 = over_mean - base_mean
        sd_diff_1 = over$stderr
  
        diff_2 = under_mean - base_mean
        sd_diff_2 = under$stderr
  
        # estrellitas
        f_star = function(x){case_when(x<0.01 ~ "***",  x<0.05 ~ "**", x<0.1 ~ "*", .default = "")}
        fmt <- function(x) formatC(x, format = "f", digits = 2)
  
        # Tabla
        output = tibble(var = c(interes_col, NA ),
                        base_mean = c(fmt(base_mean),  paste0("(",fmt(base_sd),")")), 
                        over_mean =  c(fmt(over_mean),  paste0("(",fmt(over_sd),")")), 
                        under_mean =  c(fmt(under_mean),  paste0("(",fmt(under_sd),")")), 
                        diff_1 = c(paste0(fmt(diff_1), f_star(over$p.value)), paste0("(",fmt(sd_diff_1),")")), 
                        diff_2 = c(paste0(fmt(diff_2), f_star(under$p.value)), paste0("(",fmt(sd_diff_2),")")))
        
  return(output) 

}

continuas = bind_rows(c_test(data = test, interes_col = "age", outlier_col = "outliers"), 
                      c_test(data = test, interes_col = "y_total_m_ha_log", outlier_col = "outliers")) 

continuas$var = c("Edad", NA, "Ingresos por hora", NA)


##==: 4. table de diferencia de medias para discretas

## funcion para sacar tabla de diferencia de medias en valores discretos
p_test = function(interes_col, outlier_col){
  
        # Agrupaciones
        tab <- table(interes_col, outlier_col)

        # calcular columnas de si y totales
        yes <- tab[2, ]     
        n <- colSums(tab)

        # Calculando differencia de medias
        p_test_1 = prop.test(x = yes[-3], n = n[-3], alternative = "two.sided")
        p_test_2 = prop.test(x = yes[-2], n = n[-2], alternative = "two.sided")
        
        # Calculando standar deviation
        se_base = sqrt(p_test_1$estimate["prop 1"] * (1 - p_test_1$estimate["prop 1"]) / sum(tab[,1]))
        se_over = sqrt(p_test_1$estimate["prop 2"] * (1 - p_test_1$estimate["prop 2"]) / sum(tab[,2]))
        se_under = sqrt(p_test_2$estimate["prop 2"] * (1 - p_test_2$estimate["prop 2"]) / sum(tab[,3]))
        se_diff_1 = sqrt((p_test_1$estimate["prop 2"] * (1 - p_test_1$estimate["prop 2"]) / sum(tab[,2])) + (p_test_1$estimate["prop 1"] * (1 - p_test_1$estimate["prop 1"]) / sum(tab[,1])))
        se_diff_2 = sqrt((p_test_2$estimate["prop 2"] * (1 - p_test_2$estimate["prop 2"]) / sum(tab[,2])) + (p_test_2$estimate["prop 1"] * (1 - p_test_2$estimate["prop 1"]) / sum(tab[,1])))
  
        # estrellitas
        f_star = function(x){case_when(x<0.01 ~ "***",  x<0.05 ~ "**", x<0.1 ~ "*", .default = "")}
        fmt <- function(x) formatC(x, format = "f", digits = 2)
  
        # Tabla
        output = tibble(var = c(row.names(tab)[2], NA ),
                      base_prop = c(fmt(p_test_1$estimate["prop 1"]),  paste0("(",fmt(se_base),")")), 
                      over_prop =  c(fmt(p_test_1$estimate["prop 2"]),  paste0("(",fmt(se_over),")")), 
                      under_prop =  c(fmt(p_test_2$estimate["prop 2"]),  paste0("(",fmt(se_under),")")),
                      diff_1 = c(paste0(fmt(p_test_1$estimate["prop 2"] - p_test_1$estimate["prop 1"]), f_star(p_test_1$p.value)), paste0("(",fmt(se_diff_1),")")),
                      diff_2 = c(paste0(fmt(p_test_2$estimate["prop 2"] - p_test_2$estimate["prop 1"]), f_star(p_test_2$p.value)), paste0("(",fmt(se_diff_2),")")))
  
        return(output) 
  
}

## aplicar a variables discretas
discretas <- lapply(c("max_educ_level", "formalidad", "relab", "sex", "size_firm", "oficio") , function(var){

        cats <- as.character(unique(test[[var]]))
        print(var)
        var_2 = var

        output =  bind_rows(lapply(cats, function(x){
                cat(x, "\n")
                df = test %>%
                        mutate(temp_var = ifelse(.data[[var]] == x, x, "otro"),
                                temp_var = factor(temp_var, levels = c("otro", x)))

                df = p_test(df$temp_var, df$outliers)
                
                return(df) 
        })) 
  
        output = output |>
                mutate(category = var_2) |> 
                relocate(category, .before  = var)
        cat("\n")
        return(output)
}) 


##==: 5. exportar resultados
options(knitr.kable.NA = '')
  
# continuas
continuas_table <- continuas |>
  kable(
    col.names = c(
      "",
      "Referencia",
      "Superior",
      "Inferior",
      "Sup. - Ref.",
      "Inf. - Ref."
    ),
    format = "latex",
    align = c("lccccc"),
    booktabs = TRUE
  ) |>
  add_header_above(c(" " = 1, "Media" = 3, "Diferencias" = 2)) |>
  kable_styling(latex_options = "hold_position") |>
  footnote(
    general = c(
      "Desviaciones estándar entre paréntesis.",
      "* p$<$0.10; ** p$<$0.05; *** p$<$0.01",
      paste0("Observaciones (Referencia, Superior, Inferior): ",
             sum(test$outliers == "No"), ", ",
             sum(test$outliers == "over"), ", ",
             sum(test$outliers == "under"))
    ),
    general_title = "", 
    escape = FALSE
  )

writeLines(text = continuas_table, con = "06_visual/output/05_diff_mean_tables/05_diff_medias_tabla_continuas.txt")

# discretas

map(discretas, function(x){
        
continuas_table <- x |>
        select(-category) |> 
        kable(
        col.names = c(
        "",
        "Referencia",
        "Superior",
        "Inferior",
        "Sup. - Ref.",
        "Inf. - Ref."
        ),
        format = "latex",
        align = c("lccccc"),
        booktabs = TRUE
        ) |>
        add_header_above(c(" " = 1, "Media" = 3, "Diferencias" = 2)) |>
        kable_styling(latex_options = "hold_position") |>
        footnote(
        general = c(
        "Desviaciones estándar entre paréntesis.",
        "* p$<$0.10; ** p$<$0.05; *** p$<$0.01",
        paste0("Observaciones (Referencia, Superior, Inferior): ",
                sum(test$outliers == "No"), ", ",
                sum(test$outliers == "over"), ", ",
                sum(test$outliers == "under"))
        ),
        general_title = "", 
        escape = FALSE
        )

        writeLines(text = continuas_table, con = paste0("06_visual/output/05_diff_mean_tables/05_diff_medias_tabla_discreta_",unique(x$category),".txt"))
})
