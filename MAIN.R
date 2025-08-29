# install pacman
if (require("pacman") == F){install.packages("pacman")}else{require("pacman")}

# run code
list = c("01_import/02_scripts/",
         )

run = lapply(X = list, FUN = function(x){ source(x)})
