library(sjlabelled)
library(WDI)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

url <- "https://data.worldbank.org/indicator/NE.EXP.GNFS.KD"
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

data %>% write_csv_fundar(sprintf("data/_FUENTES/raw/%s",download_filename))

# agregar_fuente_raw(url = url,
#                    nombre = ind_label,
#                    institucion = "Banco Mundial",
#                    actualizable = T,
#                    dir = "data/_FUENTES/raw/",
#                    path_raw = download_filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = "R87C0 ", actualizable = T, dir = "data/_FUENTES/raw/")
