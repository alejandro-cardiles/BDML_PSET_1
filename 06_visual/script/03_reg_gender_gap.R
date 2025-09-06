#======================#
# Laura Camila Ortiz Perez
# CDATE:  2025-09-05
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, tidyverse, fixest, modelsummary, stargazer, leaps)

#==== import data ====# 

#for table: 
model1 = import("04_regression/03_output/model1_reg_gap_female.rds")
model2 = import("04_regression/03_output/model4_reg_gap_female.rds")
model4 = import("04_regression/03_output/model5_reg_gap_female.rds")


#table


#table

modelsummary(model1,
             coef_map = c("(Intercept)" = "Constante", 
                          "sexFemale" = "Mujer"),
             statistic = "({std.error})",
             gof_omit  = "IC|Log|Adj|F|RMSE",
             stars     = c('*'=.1,'**'=.05,'***'=.01),
             title     = "OLS estimation, Dep. Var.: log(ingreso mensual por horas)",
             output    = "06_visual/output/02_model_test.tex")
