#______________________
# Laura Camila Ortiz 
# Fecha: 31 de agosto 2025
#_____________________

#---------------------#
# ==== 0. Setup ====
#---------------------#

rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, janitor,data.table,caret,stargazer,boot,fixest)

#---------------------#
# ==== 1. import ====
#---------------------#

df = import('02_prepare_data/03_output/01_main_data.rds', setclass = 'tibble') 
colnames(df)



#---------------------#
# ==== 2. model    ====
#---------------------#


model2 = lm(log(y_total_m_ha) ~ sex * age + sex*I(age^2) + max_educ_level + relab 
            + oficio + formalidad + size_firm + total_menores +
              total_seniors_inactivos , data = df) 
summary(model2)

# Coefs
b <- coef(model2)

# Helper to grab interaction names safely regardless of order "a:b" vs "b:a"
getb <- function(nm1, nm2 = NULL){
  if(is.null(nm2)) return(unname(b[nm1]))
  i1 <- paste0(nm1, ":", nm2)
  i2 <- paste0(nm2, ":", nm1)
  unname(b[ifelse(i1 %in% names(b), i1, i2)])
}

# Baseline terms
b_age  <- getb("age")
b_age2 <- getb("I(age^2)")

# Interactions for Female (adjust the level name if your factor is different)
# e.g., if your level is "F", change "sexFemale" to "sexF".
b_age_F  <- getb("sexFemenino", "age")
b_age2_F <- getb("sexFemenino", "I(age^2)")

# Peak ages
peak_age_m <- - b_age / (2 * b_age2)
peak_age_f <- - (b_age + b_age_F) / (2 * (b_age2 + b_age2_F))

peak_age_m; peak_age_f


peak_age_fn_gender <- function(data, index, formula) {
  d <- data[index, , drop = FALSE]
  d$sex <- factor(d$sex, levels = levels(data$sex))
  f <- lm(formula, data = d); b <- coef(f)
  
  getb <- function(nm1, nm2 = NULL){
    if (is.null(nm2)) return(if (nm1 %in% names(b)) unname(b[[nm1]]) else 0)
    i1 <- paste0(nm1, ":", nm2); i2 <- paste0(nm2, ":", nm1)
    hit <- c(i1, i2)[c(i1, i2) %in% names(b)]
    if (length(hit) == 0) return(0)
    unname(b[[hit[1]]])
  }
  
  female_lab <- levels(d$sex)[2]; sex_term <- paste0("sex", female_lab)
  
  a_m <- getb("age"); q_m <- getb("I(age^2)")
  a_f <- a_m + getb(sex_term, "age")
  q_f <- q_m + getb(sex_term, "I(age^2)")
  
  c(peak_age_male   = if (q_m < 0) -a_m/(2*q_m) else NA_real_,
    peak_age_female = if (q_f < 0) -a_f/(2*q_f) else NA_real_)
}



form1 = log(y_total_m_ha) ~ sex * age + sex*I(age^2) + max_educ_level + 
  relab + oficio + formalidad + size_firm + total_menores +
  total_seniors_inactivos

set.seed(2003)

bootA <- boot(df, function(d,i) peak_age_fn_gender(d,i, form1), R=1000)

point_est <- peak_age_fn_gender(df, 1:nrow(df), form1)

colnames(bootA$t) <- c("peak_age_male","peak_age_female")

ci <- function(x) quantile(x, c(.025,.975), na.rm = TRUE)
ci_m <- ci(bootA$t[, "peak_age_male"])
ci_f <- ci(bootA$t[, "peak_age_female"])

# 4) Tabla final (lista para exportar)
tabla_picos <- data.frame(
  sexo     = c("Masculino","Femenino"),
  peak_age = c(point_est["peak_age_male"], point_est["peak_age_female"]),
  ci_lo    = c(ci_m[1], ci_f[1]),
  ci_hi    = c(ci_m[2], ci_f[2])
)
tabla_picos

# Ajustar modelo
m <- lm(form1, data = df)

# Curva promedio por sexo
ages <- seq(min(df$age, na.rm = TRUE), max(df$age, na.rm = TRUE), by = 1)
avg_prof <- lapply(df, function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else names(sort(table(x), TRUE))[1]) |> as.data.frame()

build_curve <- function(sex_label){
  nd <- do.call(rbind, replicate(length(ages), avg_prof, simplify = FALSE))
  nd$age <- ages
  nd$sex <- factor(sex_label, levels = levels(df$sex))
  pr <- predict(m, newdata = nd, se.fit = TRUE)
  data.frame(
    sex = sex_label,
    age = ages,
    wage = exp(pr$fit),
    ci_lower = exp(pr$fit - 1.96*pr$se.fit),
    ci_upper = exp(pr$fit + 1.96*pr$se.fit)
  )
}

# ==== 3) Curvas predichas ====

plot_data <- rbind(build_curve("Masculino"), build_curve("Femenino"))

# Picos (usando tu tabla_picos + salarios en esas edades)
tabla_picos$peak_wage <- sapply(1:nrow(tabla_picos), function(i){
  nd <- avg_prof
  nd$age <- tabla_picos$peak_age[i]
  nd$sex <- factor(tabla_picos$sexo[i], levels=levels(df$sex))
  exp(predict(m, newdata=nd))
})
peak_point <- tabla_picos

export(plot_data, "03_regression/03_output/03_plot_data.rds")
export(peak_point, "03_regression/03_output/03_peak_points.rds")


