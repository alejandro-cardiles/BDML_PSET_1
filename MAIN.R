# install pacman
if (require("pacman") == F){install.packages("pacman")}else{require("pacman")}

# run code
list = c("01_import/02_scripts/01_scrapping_data_ignacio_page.R",
         )

run = lapply(X = list, FUN = function(x){ source(x)})
