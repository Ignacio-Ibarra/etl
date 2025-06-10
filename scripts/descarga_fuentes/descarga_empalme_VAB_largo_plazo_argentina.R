

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(httr)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(24)
fecha_ultima_actualizacion <- as.Date("2022-12-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://raw.githubusercontent.com/datos-Fundar/valor_agregado_bruto_largo_plazo_argentina/refs/heads/main/tablas/empalme_series_pbg_pob_vab_pc.csv"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "empalme_series_pbg_pob_vab_pc.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = "Serie anual de Valor Agregado Bruto a precios básicos en pesos constantes de 2004 (1895-actualidad).",
#                    institucion = "Fundar",
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = as.character(fecha_actualizar),
#                    api = F
# )


actualizar_fuente_raw(id_fuente = 222,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename
)
