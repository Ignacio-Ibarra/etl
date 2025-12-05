
#   -----------

#   -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


# emisiones_sector_global_2016 co2  -----------

# traigo url del gráfico en owid al que corresponde el dataset 

url <- "https://raw.githubusercontent.com/lambwf/UNEP-Gap-Report-2025-Chapter-2/refs/heads/main/results/UNEP-Gap-Report-2025-Chapter-2-Data-Global.xlsx"

filename <- "data_global_egr_un.xlsx"

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{filename}"))


# # # agrego la fuente
# agregar_fuente_raw(url = url,
#                    institucion = "United Nations Environment Programme",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    fecha_actualizar = "2026/11/01",
#                    path_raw = filename,
#                    script =  code_name,
#                    nombre = "Emissions Gap Report 2025: Off Target - Continued Collective inaction puts Global Temperature Goal at Risk - Appendices. https://wedocs.unep.org/20.500.11822/48855."
# )
# 




actualizar_fuente_raw(id_fuente=471 , url = url,
                      fecha_actualizar = "Sin informacion")

#limpiar_temps()
#list.files(tempdir())

#fuentes() %>% 
#  view()

