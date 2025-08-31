#------------------------#
# Angel Castillo Negrete #
#     2025-08-31         #
#------------------------#

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse,rio,janitor,data.table,DescTools,tidymodels)

##==: 1. Import data
db = import('01_import/03_output/01_data_scrapping_web_page.rds',
             setclass = 'tibble') %>% 
     clean_names() 

##==: 2. Select covariates
df = db %>% 
     select(directorio,secuencia_p,orden, ### Idenfiticadores únicos de vivienda, hogar y persona
            estrato_energia = estrato1, ### Caracteristica de la vivienda
            sex,age,max_educ_level,relation_to_household_head = p6050, ### Caracteristicas sociales
            desocupado = dsi,ocupado = ocu,inactivo = inac,pet, ### Estado laboral
            informal,relab,size_firm,oficio,has_another_job = p7040, ### Caracteristicas laborales
            y_total_m_ha, ### Variables dependiente ingresos totales mensuales por hora
            f_weights = fex_c) ### Factor de expansión para representar a la poblacion
 
##==: 3. Wrangle predictors

### 3.1 Individual level predictors
df = df %>% 
     mutate(
       female = ifelse(sex == 0,1,0), ### Female indicator
       has_another_job = ifelse(has_another_job == 1,1,0), ### Has another job
       household_head = ifelse(relation_to_household_head == 1,1,0), ### Household head
       household_head_spouse = ifelse(relation_to_household_head == 2,1,0) ### Spouse of household head
      ) %>% 
      select(-sex,-relation_to_household_head)

### 3.2 Household level predictors
household_predictors = df %>% 
                       group_by(directorio,secuencia_p) %>% 
                       mutate(children = ifelse(age < 18,1,0),
                               seniors = ifelse(age > 65,1,0)) %>% 
                       summarise(household_size = n(),
                                 n_ocupados_hog = sum(ocupado),
                                 n_informales_hog = sum(informal,na.rm=T),
                                 n_inactivos_hog = sum(inactivo),
                                 n_children_hog = sum(children),
                                 n_seniors_hog = sum(seniors)) %>% 
                       ungroup()

##==: 4. Merge covariates together
data = df %>% 
       filter(ocupado == 1 & age > 18) %>% 
       left_join(x = .,y = household_predictors,by = c("directorio","secuencia_p")) %>% 
       select(-ocupado,-desocupado,-inactivo,-pet)
  
### 4.2 reorder variables
data = data %>% 
       relocate(directorio,secuencia_p,orden,
                age,female,max_educ_level,
                informal,relab,size_firm,oficio,
                estrato_energia,starts_with('household'),starts_with('n_'),
                y_total_m_ha,
                f_weights)

##==: 5. Impute missing values of predictors
data = data %>% 
       mutate(max_educ_level = ifelse(is.na(max_educ_level) == T,yes = data$max_educ_level %>% Mode(na.rm = T) %>% as.numeric(),no = max_educ_level)) 

##==: 6. Impute Dependent variable

### 6.1 Compute descriptive stat to impute missing data with
impute_dependent_variable = data %>%
                            drop_na(y_total_m_ha) %>% 
                            group_by(female,max_educ_level,informal) %>% 
                            summarise(y_total_m_ha = mean(y_total_m_ha,na.rm = T)) %>% 
                            ungroup()

### 6.2 Impute missing values of dependent variable
data = data %>% 
       rows_patch(x = .,y = impute_dependent_variable,by = c("female","max_educ_level","informal")) 

##==: 7. Export 
export(data,'02_prepare_data/03_output/01_main_data.rds') 

