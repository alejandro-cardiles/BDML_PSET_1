#______________________
# Laura Camila Ortiz 
# Fecha: 31 de agosto 2025
#_____________________

#---------------------#
# ==== 1. Setup ====
#---------------------#

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, data.table,caret,janitor,mice)


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



df <- df %>%
  mutate(obs_y = as.integer(!is.na(y_salary_m_hu)))   # indicator: 1 if y observed

# 1a) How much missing?
mean(is.na(df$y_salary_m_hu))


df = df %>% mutate(sex = as.numeric(sex),
                   max_educ_level = as.factor(maxEducLevel),
                   oficio = as.factor(oficio),
                   sizeFirm = as.factor(sizeFirm))


colnames(df)
# 1b) Is missingness related to observables? (MAR diagnostic)
m_miss <- glm(obs_y ~ sex + age + I(age^2) + max_educ_level + oficio  + sizeFirm + informal, data = df, family = binomial())
summary(m_miss)
# If covariates significantly predict obs_y, listwise deletion likely biased unless MAR given X.

tabyl(df$obs_y)

df =  df %>% mutate(sex = ifelse(sex == 0, 1, 0)) 
model1 <- lm(
  y_salary_m_hu ~ sex + age + I(age^2) + max_educ_level + oficio  + sizeFirm ,
  data = df
)
summary(model1)


df %>%
  group_by(sex, informal) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(perc = 100 * n / sum(n))


df %>% filter(is.na(y_salary_m_hu)) %>% 
  group_by(sex, informal) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(perc = 100 * n / sum(n))



imp_vars <- df[, c("y_salary_m_hu","sex","age","maxEducLevel","oficio","relab","sizeFirm","informal")]
ini  <- mice(imp_vars, maxit=0, printFlag=FALSE)
meth <- ini$method; meth["y_salary_m_hu"] <- "pmm"
pred <- ini$predictorMatrix; pred["y_salary_m_hu",] <- 1; pred["y_salary_m_hu","y_salary_m_hu"] <- 0

imp <- mice(imp_vars, m=30, method=meth, predictorMatrix=pred, seed=123)
fit_mi <- with(imp, lm(y_salary_m_hu ~ sex + age + I(age^2) + maxEducLevel + oficio + relab + sizeFirm + informal))
summary(pool(fit_mi))


# Suppose you already ran multiple imputation

# Quick visualization of imputations
# This plots observed vs imputed distributions
densityplot(imp, ~ y_salary_m_hu)         # density curves
stripplot(imp, y_salary_m_hu ~ .imp)      # stripplot across imputations

# If you want a histogram of imputed vs observed:
completed1 <- complete(imp, 1)   # first imputed dataset
completed2 <- complete(imp, 2)   # second imputed dataset

# Compare observed vs imputed
observed_y <- df$y_salary_m_hu[!is.na(df$y_salary_m_hu)]
imputed_y  <- completed1$y_salary_m_hu[is.na(df$y_salary_m_hu)]  # imputed values only (dataset 1)

par(mfrow=c(1,2))
hist(observed_y, breaks=30, main="Observed Y", col="skyblue", xlab="Y")
hist(imputed_y, breaks=30, main="Imputed Y (draw 1)", col="tomato", xlab="Y")

#-----------------------------------#
# ==== 4. Handle missing values ====
#-----------------------------------#

# 1. Subconjunto completo para entrenar modelo
df_train <- df %>%
  filter(!is.na(y_salary_m), !is.na(age), !is.na(sex), !is.na(college), !is.na(maxEducLevel))

# Antes de imputar
ggplot(df_train, aes(x = y_salary_m)) +
  geom_histogram(fill = "skyblue", bins = 40) +
  labs(title = "Distribución original")

# Después de imputar
ggplot(df, aes(x = y_salary_m)) +
  geom_histogram(fill = "forestgreen", bins = 40) +
  labs(title = "Después de imputar con KNN")