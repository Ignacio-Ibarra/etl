# Este script es especial porque trabaja con una fuente que
# por el momento no está disponible públicamente
# para ello se sube al server el archivo,luego se genera un script para
# generar la fuente raw
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Muevo archivo al tempdir para que lo suba desde ahi al filesystem remoto y genere la entrada en fuentes_raw

raw_file <- "DatabaseTUS85 countries.xlsx"
from_path <- glue::glue("{Sys.getenv('FUENTE_SIN_SOURCE_PATH')}/{raw_file}")
to_path <- glue::glue("{tempdir()}/{raw_file}")

file.copy(from = from_path, to = to_path ) 

agregar_fuente_raw(url = "https://doi.org/10.1016/j.wss.2022.100081",
                   nombre = "Variety and change of patterns in the gender balance between unpaid care-work, paid work and free time across the world and over time: A measure of wellbeing?. Wellbeing, Space and Society. Volume 3. 2022",
                   institucion = "Charmes, Jacques",
                   actualizable = F,
                   fecha_descarga = Sys.Date(),
                   directorio = tempdir(),
                   path_raw = raw_file,
                   script = code_name,
                   api = F
)

actualizar_fuente_raw(id_fuente = 96, dir = tempdir())