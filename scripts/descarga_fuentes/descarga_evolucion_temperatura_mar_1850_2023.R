# evolución temperatura mar 1850-2023  -----------

temp_mar_1850_2023_url <- "https://www.metoffice.gov.uk/hadobs/hadsst4/data/data/HadSST.4.1.1.0_monthly_GLOBE.csv"


archivo <- "temp_mar_1850.csv"

download.file(url = temp_mar_1850_2023_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{archivo}"))

# agregar_fuente_raw(url = "https://www.metoffice.gov.uk/hadobs/hadsst4/data/csv/HadSST.4.0.1.0_monthly_GLOBE.csv", 
#                    institucion = "National Meteorological Service UK - Met Office Hadley Centre observations", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "temp_mar_1850_2023.csv", 
#                    dir = tempdir(),
#                    script = "descarga_evolucion_temperatura_mar_1850_2023.R",
#                    nombre = "Evolución temperatura mar 1850-"
# )

comparar_archivos(x =  glue::glue("{tempdir()}/{archivo}"), y = get_fuente_path("R121C0"))

actualizar_fuente_raw(id_fuente=121,
                      nombre = "Evolución temperatura mar desde 1850",
                      path_raw = archivo,
                      fecha_actualizar = "Sin informacion",
                      url = temp_mar_1850_2023_url,
                      fecha_descarga = Sys.Date())

list.files(tempdir())
limpiar_temps()
