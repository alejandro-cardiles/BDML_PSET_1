### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse,rio,janitor,data.table,moments, ### Data wrangling
       modelsummary,ggpubr,gt,gtsummary,GGally,skimr) ### Data visualisation

##==: 1. Load data
df = import('02_prepare_data/03_output/01_main_data.rds',setclass = 'tibble') %>% 
     select(-directorio,-secuencia_p,-orden,-f_weights) ### Remove ids and sample weights

##==: 2. Visualize data

### 2.1 Continuous variables
table_continuous = df %>% 
                   select(where(is.numeric)) %>% 
                   rename('Ingresos laborales por hora' = y_total_m_ha,
                          'Edad' = age)

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

P25 = function(x) quantile(x,probs = 0.25)

P75 = function(x) quantile(x,probs = 0.75)

### Compute stats for table
table_continuous = datasummary(formula = All(table_continuous) ~ Mean + SD + Min + P25 + Median + P75 + Max +  Correlation,
                               data = table_continuous,
                               output = 'data.frame',fmt = function(x) format(round(x,2),big.mark = ',')) %>%
                               arrange(desc(Mean))

### Rename stats
table_continuous = table_continuous %>% 
                   rename('Promedio' = "Mean",
                          'Desviación \nestándar' = "SD",
                          'Mediana' = "Median",
                          'Correlación con \n variable de resultado' = 'Correlation')

### Make table
table_continuous = table_continuous %>% 
                   gt(groupname_col = 'role',
                      row_group_as_column = F,
                      auto_align = T,
                      process_md = T)  %>%
                   tab_style(style = list(cell_text(weight = "bold")),
                             locations = cells_row_groups())
  
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
                                                sd = sd(y_total_m_ha),
                                                median = median(y_total_m_ha)) %>% 
                                      mutate(across(c(average,sd,median),.fns = function(x) round(x,0)),
                                             across(c(average,sd,median),.fns = function(x) format(x,big.mark = ',')))

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
                    rename(' ' = var,
                            'N' = n,
                            '%' = prop,
                            'Promedio' = average,
                            'Desviación \nestándar' = sd,
                            'Mediana' = median)

### Make descriptive table
table_categorical = table_categorical %>% 
                    gt(groupname_col = 'group',
                       row_group_as_column = F,
                       auto_align = T,
                       process_md = T)  

##==: 3. Export tables

table_continuous %>% as_latex() %>% write_lines(x = .,file = '06_visual/output/01_tabla_continuas.tex')
table_categorical %>% as_latex() %>% write_lines(x = .,file = '06_visual/output/01_tabla_categoricas.tex')

