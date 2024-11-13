
#limpio la memoria
rm( list=ls())  #Borro todos los objetos
gc()   #Garbage Collection

# Función para obtener la ruta del archivo, compatible tanto en RStudio como en la consola
get_file_location <- function() {
  # Intenta obtener la ruta del archivo en RStudio
  if (interactive() && "rstudioapi" %in% rownames(installed.packages())) {
    return(rstudioapi::getSourceEditorContext()$path)
  }
  
  # Alternativa para obtener la ruta si se usa source()
  this_file <- (function() { attr(body(sys.function(1)), "srcfile") })()
  
  # Si no se obtiene el path (e.g., en consola sin RStudio), asigna un valor por defecto
  if (!is.null(this_file)) {
    return(this_file$filename)
  } else {
    return("Archivo no especificado o ruta predeterminada")
  }
}

code_name <- get_file_location() %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-09-30")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/mteyss_oede_scraper_links.R")

busqueda_links <- mteyss.oede.provinciales.remuneracions_empleo_y_empresas()

resultado <- busqueda_links %>% 
  dplyr::filter(h4 =="Empleo") %>% 
  dplyr::filter(grepl(".*cuatro dígitos.*", detalle)) 

url <- resultado %>% pull(link)

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- str_split_1(url, "/") %>% tail(.,1)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre <- glue::glue("OEDE - {resultado$h3} - {resultado$h4} - {resultado$detalle} - {resultado$fecha_publicacion}")

# Localizacion del empleo

# agregar_fuente_raw(url = url,
#                    institucion = "OEDE",
#                    nombre = "Serie de empleo registrado por rama de actividad y Provincia",
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 247,
                      nombre = nombre,
                      institucion = "Ministerio de Trabajo, Empleo y Seguridad Social. Subsecretaría de Planificación, Estudios y Estadísticas. Dirección General de Estudios y Estadísticas Laborales",
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename,
                      script = code_name)


