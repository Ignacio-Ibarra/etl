#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(httr)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


# 
# fecha_actualizar <- "2024-07-31"
# 
url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/4_Mortality/WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx"
# 
# # Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)
# 
download_filename <- "WPP2024_MORT_F05_1_LIFE_EXPECTANCY_BY_AGE_BOTH_SEXES.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")
# 
download.file(url, destfile = destfile, mode = "wb")
# 
# # agregar_fuente_raw(url = url,
# #                    nombre = "World Population Prospectes. Life expectancy at exact age, both sexes",
# #                    institucion = "United Nations. Department of Economic and Social Affairs. Population Division",
# #                    actualizable = T,
# #                    path_raw = download_filename,
# #                    script = code_name,
# #                    fecha_actualizar = fecha_actualizar,
# #                    api = T
# # )

actualizar_fuente_raw(id_fuente = 215,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename
)
