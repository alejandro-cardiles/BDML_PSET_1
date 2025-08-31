#______________________
# Laura Camila Ortiz 
# Fecha: 31 de agosto 2025
#_____________________

#---------------------#
# ==== 1. Setup ====
#---------------------#

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, data.table,caret)


#---------------------#
# ==== 2. Import ====
#---------------------#

data = import("01_import/03_output/01_data_scrapping_web_page.rds") #ajustar para halarlo de preparar data

#-----------------------------#
# ==== 3. inspecting data ====
#-----------------------------#

vars <- c("age", "sex", "maxEducLevel", "college",
          "clase", "y_salary_m_hu", "y_salary_m",
          "inac", 'informal', "dsi", "cuentaPropia")

# Apply sum(is.na()) to each

missing_counts <- map_dbl(data[vars], ~sum(is.na(.x)))
print(missing_counts)


ggplot(data, aes(x = y_salary_m)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Histograma de y_salary_m", x = "Salario")


df = data[data$age>18 & data$ocu == 1,]

12424/22209 # tengo una perdida del 55% de los datos

ggplot(df, aes(x = y_salary_m)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 30) +
  theme_minimal() +
  labs(title = "Histograma de y_salary_m", x = "Salario")


missing_counts <- map_dbl(df[vars], ~sum(is.na(.x)))
print(missing_counts)

df <- df %>% drop_na(maxEducLevel)

df$sex <- as.factor(df$sex)
df$college <- as.factor(df$college)
df$maxEducLevel  <- as.factor(df$maxEducLevel)
df$age <- as.numeric(df$age)


str(df[, c("age", "sex", "college", "maxEducLevel")])

str(df$y_salary_m)
table(sapply(df$y_salary_m, length))  

# Cambiar los niveles de sex y college
levels(df$sex)     <- c("Hombre", "Mujer")  # o lo que corresponda
levels(df$college) <- c("No", "Sí")

#-----------------------------------#
# ==== 4. Handle missing values ====
#-----------------------------------#

# 1. Subconjunto completo para entrenar modelo
df_train <- df %>%
  filter(!is.na(y_salary_m), !is.na(age), !is.na(sex), !is.na(college), !is.na(maxEducLevel))

# 2. Entrenar modelo KNN
set.seed(123)
knn_model <- train(
  y_salary_m ~ age + sex + college + maxEducLevel,
  data = df_train,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneLength = 5
)

# 3. Subconjunto con valores faltantes
df_missing <- df %>%
  filter(is.na(y_salary_m), !is.na(age), !is.na(sex), !is.na(college), !is.na(maxEducLevel))

# 4. Predecir valores faltantes
predicted_vals <- predict(knn_model, newdata = df_missing)

# 5. Insertar predicciones
df$y_salary_m[is.na(df$y_salary_m)] <- predicted_vals



# Antes de imputar
ggplot(df_train, aes(x = y_salary_m)) +
  geom_histogram(fill = "skyblue", bins = 40) +
  labs(title = "Distribución original")

# Después de imputar
ggplot(df, aes(x = y_salary_m)) +
  geom_histogram(fill = "forestgreen", bins = 40) +
  labs(title = "Después de imputar con KNN")