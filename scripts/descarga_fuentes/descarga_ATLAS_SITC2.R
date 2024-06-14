# Este script es especial porque trabaja con una fuente de
# dificil acceso (se necesita completar un formulario)
# para ello se sube al server el archivo,luego se genera un script para
# generar la fuente raw


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fuente_sitic <- "International Trade Data (SITC, Rev. 2)"
url_fuente <- "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/H8SFD2"
institution <- "Center for International Development at Harvard University"


## country_partner_sitcproductsection_year.csv ----

raw_file <- "country_partner_sitcproductsection_year.csv"



# Muevo archivo al tempdir para que lo suba desde ahi al filesystem remoto y genere la entrada en fuentes_raw
from_path <- glue::glue("{Sys.getenv('ATLAS_SITIC2_PATH')}/{raw_file}")
to_path <- glue::glue("{tempdir()}/{raw_file}")
file.copy(from = from_path, to = to_path ) 

#agregar_fuente_raw(url = url_fuente,
#                   nombre = glue::glue("{fuente_sitic}-{raw_file}"),
#                   institucion = institution,
#                   actualizable = F,
#                   fecha_descarga = Sys.Date(),
#                   directorio = tempdir(),
#                   path_raw = raw_file,
#                   script = code_name,
#                   api = F
#)

actualizar_fuente_raw(id_fuente = "R101C0", dir = tempdir())


## country_sitcproductsection_year.csv ----


raw_file <- "country_sitcproductsection_year.csv"




# Muevo archivo al tempdir para que lo suba desde ahi al filesystem remoto y genere la entrada en fuentes_raw
from_path <- glue::glue("{Sys.getenv('ATLAS_SITIC2_PATH')}/{raw_file}")
to_path <- glue::glue("{tempdir()}/{raw_file}")
file.copy(from = from_path, to = to_path ) 

#agregar_fuente_raw(url = url_fuente,
#                   nombre = glue::glue("{fuente_sitic}-{raw_file}"),
#                   institucion = institution,
#                   actualizable = F,
#                   fecha_descarga = Sys.Date(),
#                   directorio = tempdir(),
#                   path_raw = raw_file,
#                   script = code_name,
#                   api = F
#)

actualizar_fuente_raw(id_fuente = "R102C0", dir = tempdir())



## sitc_product-dta.csv ----


raw_file <- "sitc_product-dta.csv"




# Muevo archivo al tempdir para que lo suba desde ahi al filesystem remoto y genere la entrada en fuentes_raw
from_path <- glue::glue("{Sys.getenv('ATLAS_SITIC2_PATH')}/{raw_file}")
to_path <- glue::glue("{tempdir()}/{raw_file}")
file.copy(from = from_path, to = to_path ) 

#agregar_fuente_raw(url = url_fuente,
#                   nombre = glue::glue("{fuente_sitic}-{raw_file}"),
#                   institucion = institution,
#                   actualizable = F,
#                   fecha_descarga = Sys.Date(),
#                   directorio = tempdir(),
#                   path_raw = raw_file,
#                   script = code_name,
#                   api = F
#)

actualizar_fuente_raw(id_fuente = "103C0", dir = tempdir())


## location.csv ----



raw_file <- "location.csv"





# Muevo archivo al tempdir para que lo suba desde ahi al filesystem remoto y genere la entrada en fuentes_raw
from_path <- glue::glue("{Sys.getenv('ATLAS_SITIC2_PATH')}/{raw_file}")
to_path <- glue::glue("{tempdir()}/{raw_file}")
file.copy(from = from_path, to = to_path ) 


#agregar_fuente_raw(url = url_fuente,
#                   nombre = glue::glue("{fuente_sitic}-{raw_file}"),
#                   institucion = institution,
#                   actualizable = F,
#                   fecha_descarga = Sys.Date(),
#                   directorio = tempdir(),
#                   path_raw = raw_file,
#                   script = code_name,
#                   api = F
#)

actualizar_fuente_raw(id_fuente = "R104C0", dir = tempdir())

# Elimino objetos de enviroment
rm(list = c("from_path", "raw_file", "to_path"))
