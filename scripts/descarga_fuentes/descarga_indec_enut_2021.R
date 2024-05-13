# Encuesta Nacional de Uso del Tiempo 2021

limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

url <- "https://www.indec.gob.ar/ftp/cuadros/sociedad/resultados_definitivos_enut_2021.xls"  

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/resultados_definitivos_enut_2021.xls"))


# agregar_fuente_raw(url = url,
#                    nombre = "Encuesta Nacional de Uso del Tiempo 2021",
#                    institucion = "INDEC", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = "resultados_definitivos_enut_2021.xls",
#                    script = code_name
# )

actualizar_fuente_raw(
  id = "R93C0",
  url = url,
  actualizable = T,
  path_raw = "resultados_definitivos_enut_2021.xls",
  fecha_descarga = Sys.Date()
)