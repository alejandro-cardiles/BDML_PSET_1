### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse,rio,janitor,data.table,moments, ### Data wrangling
       modelsummary,ggpubr,gt,gtsummary,GGally,skimr,kableExtra,knitr) ### Data visualisation

##==: 1. Load data
df = import('02_prepare_data/03_output/01_main_data.rds',setclass = 'tibble') %>% 
     select(-directorio,-secuencia_p,-orden,-f_weights) ### Remove ids and sample weights

##==: 2. Visualize data

### 2.1 Continuous variables
table_continuous = df %>% 
                   select(where(is.numeric)) %>% 
                   rename('Ingresos laborales por hora' = y_total_m_ha,
                          'Edad' = age,
                          'N. menores de edad' = total_menores,
                          'N. personas mayores inactivas' = total_seniors_inactivos)

### Define descriptive statistics
 Correlation = function(x){
                            correlation_test_result = cor.test(df$y_total_m_ha,x, method = "pearson")
                            
                            estimate = correlation_test_result$estimate %>% round(.,3)
                            p_value = correlation_test_result$p.value

                            estimate = fcase(p_value <= 0.01, paste0(estimate, '***'),
                                             p_value <= 0.05, paste0(estimate, '**'),
                                             p_value <= 0.1,  paste0(estimate, '*'),
                                             default = as.character(estimate))
                            
                            return(estimate)                        
                            }

P10 = function(x) quantile(x,probs = 0.1)

P90 = function(x) quantile(x,probs = 0.9)

P_90_10 = function(x) {round(P90(x = x)/P10(x = x),2)}

Skewness = function(x) skewness(x)

### Compute stats for table
table_continuous = datasummary(formula = All(table_continuous) ~ N + Mean + SD + P10 + Median + P90 + P_90_10 + Skewness + Correlation,
                               data = table_continuous,
                               output = 'data.frame',fmt = function(x) format(round(x,2),big.mark = ',')) %>%
                               arrange(desc(Mean)) %>% 
                               mutate(P_90_10 = ifelse(P_90_10 == 7.29,P_90_10,' '),
                                      N = format(N,big.mark = ',',dec.mark = '.'))
### Rename stats
table_continuous = table_continuous %>% 
                   rename('Promedio' = "Mean",
                          'SD' = "SD",
                          'Mediana' = "Median",
                          'Correlación' = 'Correlation',
                          'Razón P90/P10' = P_90_10,
                          'Asimetría' = 'Skewness')

### Make table
table_continuous = table_continuous %>% 
                   gt(groupname_col = 'role',
                      row_group_as_column = F,
                      auto_align = T,
                      process_md = T)  %>%
                   tab_style(style = list(cell_text(weight = "bold")),
                             locations = cells_body(` `)) %>% 
                   tab_options(latex.use_longtable = TRUE)

### 2.2 Categorical variables

### Get vectors of variables to iterate over
vars_factores = df %>% 
                select(where(is.factor)) %>% 
                colnames()

### Compute summary statistics
table_categorical = map(.x = vars_factores,.f = function(x){

                        df_percent = df %>% 
                                    select(var = any_of(x)) %>% 
                                    count(var) %>% 
                                    mutate(prop = n/sum(n),
                                           prop = round(prop,3),
                                           prop = format(prop*100,dec.mark = '.'),
                                           prop = paste0(prop,'%'),
                                           n = format(n,big.mark = ','))

                        df_collapse = df %>% 
                                      select(var = any_of(x),y_total_m_ha) %>% 
                                      group_by(var) %>% 
                                      summarise(average = mean(y_total_m_ha),
                                                sd = sd(y_total_m_ha)) %>% 
                                      mutate(across(c(average,sd),.fns = function(x) round(x,0)),
                                             across(c(average,sd),.fns = function(x) format(x,big.mark = ',')))

                        df_stats = left_join(x = df_percent,y = df_collapse) %>% 
                                   mutate(group = x)
}) %>% list_rbind()


### Relabel variables
table_categorical = table_categorical %>% 
                    mutate(group = case_when(group == 'sex' ~ 'Sexo',
                                            group == 'max_educ_level' ~ 'Máximo nivel educativo',
                                            group == 'formalidad' ~ 'Formalidad',
                                            group == 'relab' ~ 'Posición ocupacional',
                                            group == 'size_firm' ~ 'Cantidad de trabajadores \nde la empresa en que trabaja',
                                            group == 'oficio' ~ 'Oficio',
                                            .default = group))


### Rename variables
table_categorical = table_categorical %>% 
                    select(' ' = var,
                           'N' = n,
                           '%' = prop,
                           'Promedio' = average,
                           'SD'  = sd,
                            group)

### Make descriptive table
table_categorical = table_categorical %>% 
                    gt(groupname_col = 'group',
                       row_group_as_column = F,
                       auto_align = T,
                       process_md = T)  %>%
                     tab_style(style = list(cell_text(weight = "bold")),
                               locations = cells_row_groups()) |>
                     tab_options(latex.use_longtable = TRUE,
                                 row_group.border.top.style = 'none',
                                 footnotes.multiline = FALSE)

##==: 3. Add table title and footnote

table_categorical = table_categorical %>% 
                    tab_header(title = 'Distribución de las variables categóricas') %>% 
                    tab_footnote(footnote = 'Esta tabla muestra la cantidad de observaciones en cada nivel de cada variable, 
                                             así como la proprción que representan del total de distribución.
                                             Las columnas de Promedio y SD, muestran respectivamente, el promedio y la distribución estándar
                                             de la variable dependiente para cada uno de los niveles de las covariables. 
                                             El tamaño de la muestra es de 14,632 observaciones.')

table_continuous = table_continuous %>% 
                   tab_header(title = 'Distribución de las variables númericas') %>% 
                   tab_footnote(footnote = 'Esta tabla muestra un conjunto de estadísticas descriptivas de las variables continuas incluidas en el conjunto de datos.
                                            Las columnas de Promedio y SD muestran la desviación estándar de las variables. 
                                            Mientras que las columnas P10, Mediana y P90 muestran los valores de la distribución en los percentiles 10, 50 y 90.
                                            La columna Razón P90/P10 muestra el cociente entre los percentiles 90 y 10.
                                            La columna Asimetría muestra el coeficiente de asimetría de la distribución,
                                            Finalmente la columna Correlación muestra el valor de el Coeficiente de Correlación de Pearson con la variable de Ingresos laborales por hora.')

##==: 4. Export tables
table_continuous %>% as_latex() %>% write_lines(x = .,file = '06_visual/output/01_tabla_continuas.tex')
table_categorical %>% as_latex() %>% write_lines(x = .,file = '06_visual/output/01_tabla_categoricas.tex')

##==: 5. Reimport tables
latex_table_continuous = read_lines('06_visual/output/01_tabla_continuas.tex')
latex_table_categoricas = read_lines('06_visual/output/01_tabla_categoricas.tex')

### Subset tables
latex_table_continuous = latex_table_continuous[3:25]
latex_table_categoricas = latex_table_categoricas[3:70]

### Append tables
latex_table_continuous = append(c("\\centering \\footnotesize \\setlength{\\tabcolsep}{4pt} \\renewcommand{\\arraystretch}{0.8}",
                                  "\\begingroup",
                                  "\\fontsize{9.0pt}{8.5pt}\\selectfont"),
                                latex_table_continuous)

latex_table_continuous = append(c("\\centering \\footnotesize \\setlength{\\tabcolsep}{4pt} \\renewcommand{\\arraystretch}{0.8}",
                                  "\\begingroup",
                                  "\\fontsize{9.0pt}{8.5pt}\\selectfont"),
                                latex_table_categoricas)

##==: 6. Reexport tables
latex_table_continuous %>% write_lines(x = .,file = '06_visual/output/01_tabla_continuas.tex')
latex_table_categoricas %>% write_lines(x = .,file = '06_visual/output/01_tabla_categoricas.tex')

