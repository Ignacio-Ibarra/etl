# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


library(httr)
library(rvest)

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


url <- "https://censo.gob.ar/index.php/historia/"

# Descargar ignorando SSL
res <- GET(url, config(ssl_verifypeer = 0L))
webpage <- read_html(content(res, "text"))

# Cada bloque de censo está en un contenedor grande
bloques <- webpage %>% html_nodes("div.elementor-widget-wrap")

resultados <- lapply(bloques, function(bloque) {
  textos <- bloque %>% html_text(trim = TRUE)
  
  # año (primer número de 4 dígitos que aparece)
  anio <- str_extract(textos, "\\b[0-9]{4}\\b")
  
  # habitantes
  habitantes <- str_extract(textos, "[0-9\\.]+(?= habitantes)")
  habitantes <- as.numeric(str_remove_all(habitantes, "\\."))
  
  # viviendas / casas
  viviendas <- str_extract(textos, "[0-9\\.]+(?= (casas|viviendas))")
  viviendas <- as.numeric(str_remove_all(viviendas, "\\."))
  
  if (!is.na(anio) & !is.na(habitantes)) {
    tibble(anio = anio, habitantes = habitantes, viviendas = viviendas)
  } else {
    NULL
  }
})

df_raw <- bind_rows(resultados) %>%
  distinct() %>%                   
  group_by(anio) %>% 
  summarise(
    habitantes = max(habitantes, na.rm = TRUE),
    viviendas  = if (all(is.na(viviendas))) NA_real_ else max(viviendas, na.rm = TRUE),
    .groups = "drop"
  )


nombre <- "Censo 2022. Historia"

institucion <- "Instituto Nacional de Estadística y Censos"

download_filename <- "historia_censos.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(destfile)

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 445,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)


