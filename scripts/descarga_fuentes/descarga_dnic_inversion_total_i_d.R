# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-12-20")
fecha_actualizar <- "Sin informacion"


json_data <- jsonlite::read_json("https://sheets.googleapis.com/v4/spreadsheets/1TRUnNnlwBBLy97DOFA6HinhFrOGe9UsySl2PIm4X3DE/values/Hoja%201?key=AIzaSyCq2wEEKL9-6RmX-TkW23qJsrmnFHFf5tY&alt=json")

# Extraer y limpiar las URLs de los campos con Markdown
extract_url <- function(markdown_str) {
  markdown_str %>%
    sub(".*\\(([^)]+)\\).*", "\\1", .) %>%
    sub("^blank:#", "", .)
}

dnic_sources <- tail(json_data$values, -2)  %>%
  purrr::map_dfr(~{
    tibble(
      tema                = .x[[1]],
      subtema             = .x[[2]],
      titulo              = .x[[3]],
      descripcion         = .x[[4]],
      periodo             = .x[[5]],
      fecha_actualizacion = .x[[6]],
      metadatos           = extract_url(.x[[7]]),
      descarga            = extract_url(.x[[8]])
    )
  })


target <- dnic_sources %>%
  dplyr::filter(titulo == "Inversión en I+D en Argentina")

nombre <- target %>%
  mutate(concat = str_c(tema, subtema, titulo, descripcion, periodo, fecha_actualizacion, sep = ". ")) %>%
  pull(concat)


url <- target$descarga

institucion <- "Dirección Nacional de Información Científica. Subsecretaría de Ciencias y Tecnología"

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 423,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)