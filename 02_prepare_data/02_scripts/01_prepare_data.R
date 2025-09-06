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

##==: 2. Select main covariates 
db = db %>% 
     select(directorio,secuencia_p,orden, ### Idenfiticadores únicos de vivienda, hogar y persona
            sex,age,max_educ_level, ### Caracteristicas sociales
            ocupado = ocu,inactivo = inac, ### Indicador de ocupación laboral
            informal,relab,size_firm,oficio, ### Caracteristicas laborales
            y_total_m_ha, ### Variables dependiente ingresos totales mensuales por hora
            f_weights = fex_c) ### Factor de expansión para representar a la poblacion
 
##==: 3. Create indicator variables
db = db %>% 
     mutate(menores_edad = ifelse(age < 18,1,0),
            seniors_inactivos = ifelse(age > 65 & inactivo==1,1,0)) %>% 
     group_by(directorio,secuencia_p) %>% 
     mutate(total_menores = sum(menores_edad),
            total_seniors_inactivos = sum(seniors_inactivos)) %>% 
     ungroup()

##==: 4. Impute missing values of covariates
db = db %>% 
     mutate(max_educ_level = ifelse(is.na(max_educ_level) == T,yes = db$max_educ_level %>% Mode(na.rm = T) %>% as.numeric(),no = max_educ_level)) 

##==: 5. Relabel covariates

### Educacion
db = db %>%
     mutate(max_educ_level = case_when(
                                       max_educ_level == 1 ~ "Ninguno",
                                       max_educ_level == 2 ~ "Preescolar",
                                       max_educ_level == 3 ~ "Primaria incompleta",
                                       max_educ_level == 4 ~ "Primaria completa",
                                       max_educ_level == 5 ~ "Secundaria incompleta",
                                       max_educ_level == 6 ~ "Secundaria completa",
                                       max_educ_level == 7 ~ "Terciaria",
                                       max_educ_level == 9 ~ NA,   # Reemplazar 9 por NA
                                       TRUE ~ NA),
              max_educ_level = as.factor(max_educ_level))

### Sex
db = db %>% 
     mutate(sex = ifelse(sex == 0,'Femenino','Masculino'),
            sex = factor(sex,levels = c('Femenino','Masculino')),
            sex = relevel(sex,ref = 'Masculino'))

### Relab
db = db %>%
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
db = db %>% 
     mutate(formalidad = ifelse(informal == 1,'Informal','Formal'),
            formalidad = factor(formalidad,levels = c('Informal','Formal')),
            formalidad = relevel(formalidad,ref = 'Formal')) %>% 
     select(-informal)

### Size of firm
db = db %>%
     mutate(size_firm = case_when(
                                  size_firm == 1 ~ "Independiente",
                                  size_firm == 2 ~ "2-5 trabajadores",
                                  size_firm == 3 ~ "6-10 trabajadores",
                                  size_firm == 4 ~ "11-50 trabajadores",
                                  size_firm == 5 ~ ">50 trabajadores"),
                                   
            size_firm = factor(size_firm,levels = c("Independiente","2-5 trabajadores",
                                                     "6-10 trabajadores","11-50 trabajadores",
                                                     ">50 trabajadores")))

### Oficios 
db = db %>%
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
 
##==: 6. Remove NA from outcome variable
db = db %>% 
     drop_na(y_total_m_ha)

##==: 7. Remove observations that by definition do not recieve any labor income
db = db %>% 
     filter(!relab %in% c("Trabajador familiar sin remuneración","Trabajador sin remuneración en empresas o negocios de otros hogares"))

##==: 8. Keep only people who have a job and are older than 18 years
db = db %>% 
     filter(ocupado == 1 & age > 18) %>% 
     select(-ocupado)

##==: 9. Select covariates
db = db %>% 
     select(directorio,secuencia_p,orden,
              age,sex,max_educ_level,
              formalidad,relab,size_firm,oficio,
              total_menores,total_seniors_inactivos,
              y_total_m_ha,
              f_weights)

##==: 10. Export 
export(db,'02_prepare_data/03_output/01_main_data.rds') 
