#------------------------#
# Angel Castillo Negrete #
#     2025-08-31         #
#------------------------#

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, tidymodels, yardstick, fixest)
options(scipen =  999)

##==: 1. Import data
data = import("02_prepare_data/03_output/01_main_data.rds")

##==: 2. split model     

# separar por grupos 
set.seed(10101)
split = initial_split(data, prop = 0.7)
train = training(split)
test = testing(split)

##==: 3. get metrics for previus models 
model1 = feglm(log(y_total_m_ha) ~ age + I(age^2), data = train ) 
model2 = lm(log(y_total_m_ha) ~ sex, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
model3 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
model4 = lm(log(y_total_m_ha) ~ sex + age + I(age^2) + max_educ_level + oficio + relab + size_firm + formalidad, data = train %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 


model1_prediction = predict(model1, newdata = test) 
model2_prediction = predict(model2, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
model3_prediction = predict(model3, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 
model4_prediction = predict(model4, newdata = test %>% mutate(sex = as.numeric(sex), max_educ_level = as.factor(max_educ_level))) 


##==: 4. Create new models
# M5
model5 = lm(log(y_total_m_ha) ~ sex + poly(age, 3) + max_educ_level + oficio + relab + size_firm + formalidad,
            data = train)

# M6
model6 = lm(log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level * formalidad + oficio + relab + size_firm,
            data = train)

# M7
model7 = lm(log(y_total_m_ha) ~ sex + poly(age, 2) + max_educ_level + oficio + relab*size_firm + formalidad,
            data = train)

# M8
model8 = lm(log(y_total_m_ha) ~ sex + poly(age, 2) + oficio + formalidad + max_educ_level * formalidad + relab*size_firm,
            data = train)

# M9
model9 = lm(log(y_total_m_ha) ~  sex + poly(age, 2) + oficio + formalidad + max_educ_level*formalidad*age + relab*size_firm*age  ,
            data = train)


# predictions
model5_pred = predict(model5, newdata = test)
model6_pred = predict(model6, newdata = test)
model7_pred = predict(model7, newdata = test)
model8_pred = predict(model8, newdata = test)
model9_pred = predict(model9, newdata = test)


##==: 5. extact metrics for new models
output = lapply(X = list(model1_prediction, model2_prediction, model3_prediction, model4_prediction, model5_pred, model6_pred, model7_pred, model8_pred, model9_pred), FUN = function(x) {

  print(1)
  output = metrics(tibble(pred = x, 
                          truth = log(test$y_total_m_ha)), 
                          truth = truth, 
                          estimate  = pred
                          )
  
  output = output |> 
           filter(.metric %in% c("rmse"))

  return(output)

}) 

output = bind_rows(output)
print(output)
