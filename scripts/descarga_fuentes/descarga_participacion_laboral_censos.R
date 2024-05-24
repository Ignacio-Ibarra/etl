# Este script es especial porque trabaja con una fuente que
# por el momento no está disponible públicamente
# para ello se sube al server el archivo,luego se genera un script para
# generar la fuente raw
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Muevo archivo al tempdir para que lo suba desde ahi al filesystem remoto y genere la entrada en fuentes_raw

raw_file <- "censos ocupados.xlsx"
# from_path <- glue::glue("./data/_FUENTES/raw/fuentes_sin_source/{raw_file}")
from_path <- glue::glue("{Sys.getenv('FUENTE_SIN_SOURCE_PATH')}/{raw_file}")
to_path <- glue::glue("{tempdir()}/{raw_file}")

file.copy(from = from_path, to = to_path ) 

# agregar_fuente_raw(url = "https://docs.google.com/spreadsheets/d/1s-fNXGKqC2yqFxZHbPdFPOlzN86vskV4UR6ifaZqYRA/edit#gid=680584765",
#                    nombre = "Daniel Schteingart - Participación laboral, según sexo, en habitantes mayores a 14 años a través de los Censos (1869 - 2022)",
#                    institucion = "Fundar",
#                    actualizable = F,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = raw_file,
#                    script = code_name,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 108, dir = tempdir())