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
                          'Edad' = age,
                          'Jornada Laboral' = hours_work_usual,
                          'Tamaño del hogar' = household_size,
                          'N. Ocupados en el hogar' = n_ocupados_hog,
                          'N. Informales en el hogar' = n_informales_hog,
                          'N. Inactivos en el hogar' = n_inactivos_hog,
                          'N. Niños en el hogar' = n_children_hog,
                          'N. Adultos Mayores en el hogar' = n_seniors_hog)

### 2.2 Define descriptive statistics
 Correlation = function(x){
                            correlation_test_result = cor.test(df$y_total_m_ha,x, method = "pearson")
                            
                            estimate = correlation_test_result$estimate %>% round(.,3)
                            p_value = correlation_test_result$p.value

                            estimate = fcase(p_value <= 0.1,paste0(estimate,'*'),
                                            p_value <= 0.05,paste0(estimate,'**'),
                                            p_value <= 0.01,paste0(estimate,'***'),
                                            default = as.character(estimate))
                            
                            return(estimate)                        
                            }

P25 = function(x) quantile(x,probs = 0.25)

P75 = function(x) quantile(x,probs = 0.25)

### 2.3 Compute stats for table
table_continuous = datasummary(formula = All(table_continuous) ~ Mean + SD + Min + P25 + Median + P75 + Max +  Correlation,
                               data = table_continuous,
                               add_columns = tibble(role = c(rep('Predictores continuos',8),'Resultado')),
                               output = 'data.frame',fmt = 2) %>%
                               arrange(desc(role))

### 2.4 Correct values
table_continuous = table_continuous %>% 
  mutate(across(where(is.numeric),.fns = function(x) format(x,big.mark = ',',dec.mark = '.')))

### 2.4 Rename stats
table_continuous %>% 
  rename('Promedio' = "Mean",
         'Desviación Estándar' = "SD",
         'Mediana' = "Median",
         'Correlación con la variable de resultado' = 'Correlation') %>% 
            gt(groupname_col = 'role',
              row_group_as_column = F,
              auto_align = T,
              process_md = T)  %>%
            tab_style(
              style = list(cell_text(weight = "bold")),
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
                                            group == 'relab' ~ 'Posición Ocupacional',
                                            group == 'size_firm' ~ 'Cantidad de trabajadores de la empresa',
                                            group == 'oficio' ~ 'Oficio',
                                            group == 'has_another_job' ~ '¿Tiene más de un empleo?',
                                            group == 'estrato_energia' ~ 'Estrato de energía eléctrica',
                                            group == 'household_head' ~ '¿Es jefe/a de hogar?',
                                            group == 'household_head_spouse' ~ '¿Es cónyuge del jefe/a del hogar?',
                                            .default = group))

### Rename variables
table_categorical = table_categorical %>% 
                    rename(' ' = var,
                            'N' = n,
                            '%' = prop,
                            'Promedio de ingresos laborales por hora' = average,
                            'Desviación estándar de los ingresos laborales por hora' = sd,
                            'Mediana de los ingresos laborales por hora' = median)

### Make descriptive table
table_categorical = table_categorical %>% 
                    gt(groupname_col = 'group',
                      row_group_as_column = F,
                      auto_align = T,
                      process_md = T)  %>%
                    tab_style(
                      style = list(cell_text(weight = "bold")),
                      locations = cells_row_groups())
