#------------------------#
# Alejandro Cardiles 
# 2025-09-05      
#------------------------#

### setup
cat("\f")
rm(list = ls())
library(pacman)
p_load(tidyverse, rio, tidymodels, yardstick, fixest, kableExtra, moments)
options(scipen =  999)

##==: 1. Import data
data = import("02_prepare_data/03_output/01_main_data.rds")
resultados_modelos = import("04_prediction/03_output/01_test_metrics.rds")
resultados_modelos_sin_outlieres = import("04_prediction/03_output/02_test_metrics_without_high_leverage.rds")

##==: 2.prepare data
m_completa = tibble(proviene = "Muestra Completa", 
                    residuales = resultados_modelos[[9]]$modelo$residuals)

out_completa = tibble(proviene = "Muestra sin High Leverage", 
                      residuales = resultados_modelos[[9]]$modelo$residuals)

output = bind_rows(m_completa, out_completa) 


skew_data <- output %>%
  group_by(proviene) %>%
  summarise(skew = round(skewness(residuales), 3))

# graficar histograma + texto con skewness
plot = ggplot(output, aes(x = residuales)) +
  geom_histogram(binwidth = 0.01, fill = "blue", alpha = 0.5) +
  labs(title = "",
       x = "Residuales", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~proviene, scales = "free_x") +
  geom_text(data = skew_data,
            aes(x = 0, y = Inf,
                label = paste0("Skewness = ", skew)),
            vjust = 1.5, inherit.aes = FALSE)


ggsave(plot, file = "05_visual/03_output/06_histograma_residuales.png", width = 10, height = 8, units = "in", dpi = 300)