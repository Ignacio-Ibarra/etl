#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(sjlabelled)
library(WDI)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(2)
fecha_ultima_actualizacion <- as.Date("2024-07-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://data.worldbank.org/indicator/NY.GDP.PCAP.KD"
indicator_code <- str_split_1(url, "/") %>% tail(.,1)


# Descargo data usando wrapper https://github.com/vincentarelbundock/WDI
data <- WDI(indicator=indicator_code, country = 'all')

# Me quedo con el nombre del indicador 
ind_label <- get_label(data[, c(indicator_code)])

make_filename <- function(database_abb, indicator_label){
  indicator_label <- str_to_lower(indicator_label)
  indicator_label <- unlist(str_extract_all(indicator_label, "[[:alpha:]]+"))
  indicator_label <- paste0(indicator_label, collapse = "_")
  filename <- sprintf("%s_%s.csv",database_abb, indicator_label)
  
  return(filename)
  
}



database_abb <- "WDI"

download_filename <- make_filename(database_abb, indicator_label = ind_label)

data %>% write_csv_fundar(glue::glue("{tempdir()}/{download_filename}"))

# agregar_fuente_raw(url = url,
#                    nombre = ind_label,
#                    institucion = "Banco Mundial",
#                    actualizable = T,
#                    fecha_actualizar = as.character(fecha_actualizar),
#                    path_raw = download_filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 220,
                      fecha_actualizar = as.character(fecha_actualizar) 
)