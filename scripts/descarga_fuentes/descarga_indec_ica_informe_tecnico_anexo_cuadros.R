#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-02-28")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/indec_scraper_links.R")


result <- INDEC.intercambio_comercial_argentino.extraer_links(id = 40, pattern = "ica_anexo_cuadros")

# Obtener el mes de ejecución
mes_actual <- format(Sys.Date(), "%m")

# Ejecutar el scraper
result <- INDEC.intercambio_comercial_argentino.extraer_links(id = 40, pattern = "ica_anexo_cuadros")

if (mes_actual != "01") {
  if (interactive()) {
    # Intentamos readline(), si falla usamos showPrompt()
    nueva_url <- tryCatch({
      utils::readline("\nSi desea actualizar la fuente, ingrese la URL del informe de Enero: ")
    }, error = function(e) {
      rstudioapi::showPrompt(
        title = "Actualizar URL",
        message = "Ingrese la URL del informe de Enero:",
        default = ""
      )
    })
    
    result$url <- nueva_url
  } else {
    stop("Error: Este script sólo puede ser corrido de manera no interactiva en Enero.\nEn caso contrario, debe correrse de manera interactiva para proporcionar la URL correcta.")
  }
}

institucion <- "Instituto Nacional de Estadísticas y Censos"

nombre <- "Intercambio Comercial Argentino - Informe Técnico (Anexo de cuadros)"

url <- result$url

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")


download.file(url, destfile = destfile, mode = "wb")

# 
# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 335,
                      url = url,
                      institucion = institucion,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)