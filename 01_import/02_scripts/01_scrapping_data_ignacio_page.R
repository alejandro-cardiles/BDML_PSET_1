#======================#
# Alejandro Cardiles
# CDATE:  2025-08-28
# MDATE:  2025-08-29
#======================#

# setup 
rm(list = ls())
pacman::p_load(rio, rvest, tidyverse, jsonlite, httr, data.table)

#=====================#
# Import
#=====================#
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

#=====================#
# scrapping
#=====================#

#--- Data
page <- read_html(url)

# obtener links internos de pagina principal
links <- page %>% 
         html_elements("a") %>% 
         html_attr("href") %>% 
         c() %>% 
         .[str_detect(string = ., pattern = "page")] %>% 
         paste0(url,.)

# ingresar a pagina y hacer scrapping
data = map(.x = links, .f = function(x){
  
    Sys.sleep(5)
  
    link = read_html(x) %>% 
           as.character(page) %>% 
           str_extract_all(pattern = "pages/geih_page.+\\.html") %>% 
           unlist() %>% 
           paste0(url, .)
    print(link)
    
    # importar datos
    data = read_html(link) %>% 
           html_table()
    
    return(data[[1]])
    
})

data = rbindlist(data)

#--- dictionaries
dict = read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html") %>% 
              html_table() %>% 
              .[[1]]

labels = read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html") %>% 
              html_table() %>% 
              .[[1]]

dictionary = list("dictionarie" = dict, 
                   "labels" = labels)

#=====================#
# export
#=====================#
export(data, "01_import/03_output/01_data_scrapping_web_page.rds")
export(dictionary, "99_other/dictionary.xlsx")
