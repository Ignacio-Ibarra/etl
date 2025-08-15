#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(data.table)
library(httr)


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

source("scripts/utils/wrapper_presupuesto_catalogo.R")

compilar_presupuesto <- function(df_urls, output_file) {
  
  if(nrow(df_urls) == 0){stop("No hay recursos para descargar")}
  
  df_urls %>%
    purrr::pmap(function(ejercicios, urlArchivo) {
      datos <- PRESUPUESTO.get_data(ejercicios, urlArchivo) %>% 
        select(!matches("*_desc")) %>% 
        mutate(across(.cols = matches("credito_*"), ~ as.numeric(str_replace(.x, ",","."))))
      datos$anio_id <- ejercicios
      if (!is.null(datos)) {
        write_header <- !file.exists(output_file)
        fwrite(datos, output_file, append = !write_header, col.names = write_header, quote = TRUE, qmethod = "double")
        message("Ejercicio ", ejercicios, " procesado y agregado a ", output_file)
      }
      invisible(NULL)
    })
}

catalogo_completo <- PRESUPUESTO.get_catalog()

titulo_recurso <- "Presupuesto de gastos y su ejecución detallada - agrupación anual"

recursos<- catalogo_completo %>% 
  dplyr::filter(nombreRecurso == titulo_recurso, periodicidad == "eventual") %>% 
  mutate(ejercicios = as.integer(ejercicios)) %>% 
  arrange(ejercicios) %>% 
  select(ejercicios, urlArchivo)

anio_min <- min(recursos$ejercicios)
anio_max <- max(recursos$ejercicios)

url <- "https://www.presupuestoabierto.gob.ar/sici/datos-abiertos"

nombre <- glue::glue("{titulo_recurso} - ({anio_min}-{anio_max})")

institucion <- "Ministerio de Economía. Secretaría de Hacienda. Subsecretaría de Presupuesto."

download_filename <- glue::glue("mecon_{titulo_recurso %>% janitor::make_clean_names()}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

busqueda <- fuentes_raw() %>% 
  dplyr::filter(path_raw == download_filename) %>% 
  select(id_fuente, path_raw)

if (nrow(busqueda) == 1){
  
  anios_descargados <- argendataR::get_raw_path(busqueda$id_fuente) %>% 
    read_csv(., col_select = 'anio_id') %>% 
    pull() %>% unique()
  
  descarga_recursos <- recursos %>% 
    dplyr::filter(!(ejercicio %in% anios_descargados))
  
  
}else{
  
  descarga_recursos <- copy(recursos)
  
}

compilar_presupuesto(descarga_recursos, output_file = destfile)



# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 430,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)




