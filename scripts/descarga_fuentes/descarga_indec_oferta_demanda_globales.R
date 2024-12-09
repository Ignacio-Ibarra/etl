source("scripts/utils/indec_scraper_links.R")

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


url <- INDEC.cuentas_nacionales.extraer_links(id = 47, pattern = ".*sh_oferta_demanda_\\d{2}_\\d{2}\\..*" )

download_filename <- "sh_oferta_demanda.xls"

destfile <- glue::glue("{tempdir()}/{download_filename}")


download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = destfile)


# agregar_fuente_raw(url = oyd_cn_indec_url,institucion = "INDEC", actualizable = T,
#                fecha_descarga = Sys.Date(),path_raw = "sh_oferta_demanda.xls",
#                script = "descarga_oferta_demanda_indec.R",
#                nombre = "Series trimestrales de oferta y demanda globales"
#                 )

actualizar_fuente_raw(id_fuente = 38,
                      url = url,
                      nombre = "Series trimestrales de oferta y demanda globales",
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)


