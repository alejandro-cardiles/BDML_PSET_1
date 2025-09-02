#------------------------#
# Angel Castillo Negrete #
#     2025-08-31         #
#------------------------#

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse,rio,janitor,data.table,DescTools)

##==: 1. Import data
db = import('01_import/03_output/01_data_scrapping_web_page.rds',
             setclass = 'tibble') %>% 
     clean_names() 

##==: 2. Select covariates
df = db %>% 
     select(directorio,secuencia_p,orden, ### Idenfiticadores únicos de vivienda, hogar y persona
            estrato_energia = estrato1, ### Caracteristica de la vivienda
            sex,age,max_educ_level,relation_to_household_head = p6050, ### Caracteristicas sociales
            ocupado = ocu,inactivo = inac, ### Estado laboral
            informal,relab,size_firm,oficio,has_another_job = p7040,hours_work_usual, ### Caracteristicas laborales
            y_total_m_ha, ### Variables dependiente ingresos totales mensuales por hora
            f_weights = fex_c) ### Factor de expansión para representar a la poblacion
 
##==: 3. Wrangle predictors

### 3.1 Individual level predictors
df = df %>% 
     mutate(
       has_another_job = ifelse(has_another_job == 1,'Yes','No'), ### Has another job
       household_head = ifelse(relation_to_household_head == 1,'Yes','No'), ### Household head
       household_head_spouse = ifelse(relation_to_household_head == 2,'Yes','No'), ### Spouse of household head
       
       has_another_job = factor(has_another_job),
       household_head = factor(household_head),
       household_head_spouse = factor(household_head_spouse),

       has_another_job = relevel(has_another_job,ref = 'No'),
       household_head = relevel(household_head,ref = 'No'),
       household_head_spouse = relevel(household_head_spouse,ref = 'No')) %>% 
      select(-relation_to_household_head)

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
       select(-ocupado,-inactivo)
  
##==: 5. Impute missing values of covariates
data = data %>% 
       mutate(max_educ_level = ifelse(is.na(max_educ_level) == T,yes = data$max_educ_level %>% Mode(na.rm = T) %>% as.numeric(),no = max_educ_level)) 

##==: 6. Relabel covariates

### Educacion
data = data %>%
       mutate(max_educ_level = case_when(
                                          max_educ_level == 1 ~ "None",
                                          max_educ_level == 2 ~ "Preschool",
                                          max_educ_level == 3 ~ "Primary incomplete",
                                          max_educ_level == 4 ~ "Primary complete",
                                          max_educ_level == 5 ~ "Secondary incomplete",
                                          max_educ_level == 6 ~ "Secondary complete",
                                          max_educ_level == 7 ~ "Tertiary",
                                          max_educ_level == 9 ~ NA,   # Reemplazar 9 por NA
                                          TRUE ~ NA),
              max_educ_level = as.factor(max_educ_level))

### Sex
data = data %>% 
       mutate(sex = ifelse(sex == 0,'Female','Male'),
              sex = factor(sex,levels = c('Female','Male')),
              sex = relevel(sex,ref = 'Male'))

### Relab
data = data %>%
       mutate(relab = case_when(
                            relab == 1 ~ "Obrero o empleado de empresa particular",
                            relab == 2 ~ "Obrero o empleado del gobierno",
                            relab == 3 ~ "Empleado doméstico",
                            relab == 4 ~ "Trabajador por cuenta propia",
                            relab == 5 ~ "Patrón o empleador",
                            relab == 6 ~ "Trabajador familiar sin remuneración",
                            relab == 7 ~ "Trabajador sin remuneración en empresas o negocios de otros hogares",
                            relab == 8 ~ "Jornalero o peón",
                            relab == 9 ~ "Otro",
                            TRUE ~ NA),
              relab = as.factor(relab))

### Formalidad
data = data %>% 
       mutate(formalidad = ifelse(informal == 1,'Informal','Formal'),
              formalidad = factor(formalidad,levels = c('Informal','Formal')),
              formalidad = relevel(formalidad,ref = 'Formal')) %>% 
       select(-informal)

### Size of firm
data = data %>%
  mutate(size_firm = case_when(
                              size_firm == 1 ~ "Self-employed",
                              size_firm == 2 ~ "2-5 workers",
                              size_firm == 3 ~ "6-10 workers",
                              size_firm == 4 ~ "11-50 workers",
                              size_firm == 5 ~ ">50 workers"),
         size_firm = as.factor(size_firm))

### Estrato energia
data = data %>%
       mutate(estrato_energia = factor(estrato_energia))

### Oficios 
data = data %>%
       mutate(oficio = case_when(
                                  oficio %in% c(1,2,3,4,5,6,7,8,9,11,12) ~ "Profesionales científicos y técnicos",
                                  oficio %in% c(13,14,15,16,17,19) ~ "Educación, religión y cultura",
                                  oficio %in% c(18) ~ "Arte, deporte y medios",
                                  oficio %in% c(20,21,30,31,32,33,34,35,36,37,38,39,40,50,51,60) ~ "Administración y gestión",
                                  oficio %in% c(41,42,43,44,45,49) ~ "Comercio y ventas",
                                  oficio %in% c(52,53,54,55,56,57,58,59) ~ "Servicios personales y de seguridad",
                                  oficio %in% c(61,62,63,64,99) ~ "Agricultura, pesca y oficios rurales",
                                  oficio %in% c(70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,93,94,95) ~ "Industria y construcción",
                                  oficio %in% c(88,89,90,91,92) ~ "Textiles y manufactura artesanal",
                                  oficio %in% c(96,97,98) ~ "Operarios y trabajos no calificados"),
              oficio = factor(oficio)) 

##==: 7. Impute Dependent variable

### 7.1 Compute descriptive stat to impute missing data with
impute_dependent_variable = data %>%
                            drop_na(y_total_m_ha) %>% 
                            group_by(sex,max_educ_level,formalidad) %>% 
                            summarise(y_total_m_ha = mean(y_total_m_ha,na.rm = T)) %>% 
                            ungroup()

### 7.2 Impute missing values of dependent variable
data = data %>% 
       rows_patch(x = .,y = impute_dependent_variable,by = c("sex","max_educ_level","formalidad")) 

##==: 8. Reorder covariates
data = data %>% 
       relocate(directorio,secuencia_p,orden,
                age,sex,max_educ_level,
                formalidad,relab,size_firm,oficio,hours_work_usual,has_another_job,
                estrato_energia,starts_with('household'),starts_with('n_'),
                y_total_m_ha,
                f_weights)

##==: 9. Export 
export(data,'02_prepare_data/03_output/01_main_data.rds') 
