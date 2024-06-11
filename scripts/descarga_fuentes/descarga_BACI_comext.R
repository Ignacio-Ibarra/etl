# Este script es especial porque trabaja con una fuente de
# dificil acceso (se necesita completar un formulario)
# para ello se sube al server el archivo,luego se genera un script para
# generar la fuente raw


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fuente_baci <- "BACI"
url_fuente <- "http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37"
institution <- "Centre d'Études Prospectives et d'Informations Internationales (CEPII)"

####
####
####
####     ACA SE PUED EVALUAR rm RAW FILES del /srv/ y ejectura sentencia en script de wget 
####
####


# Specify the directory to search in
directory_path <- glue::glue("{Sys.getenv('BACI_PATH')}")


# Get the list of all zip files in the specified directory
all_files <- list.files(directory_path, full.names = T, pattern = "zip")

directorio_temporal <- tempdir() # ZIP files

# Unzip in temp
unzip(all_files[1], exdir = directorio_temporal) # [1] "/srv/server_data/argendata/baci_comext/BACI_HS07_V202401b.zip"

unzip(all_files[2], exdir = directorio_temporal) # [2] "/srv/server_data/argendata/baci_comext/BACI_HS96_V202401b.zip"


# Get the list of all files in the specified directory
all_csv_files <- list.files(directorio_temporal, full.names = T)

# Filter the files that contain both "HS96" and ".csv"

HS96files <- grep("BACI_HS96.*\\.csv$", all_csv_files, value = TRUE)


# Filter the files that contain both "HS07" and ".csv"

HS07files <- grep("BACI_HS07.*\\.csv$", all_csv_files, value = TRUE)


## "archivo RAW" ----

raw_file <- "datasets_BACI_raw.txt"


glue::glue("* Esta archivo contiene información sobre el flujo ad-hoc de fuente cruda BACI ({institution})
          
           * La misma contiene dos grupos de datos, disponibles en el file system del servidor en la siguiente
           ruta: {directory_path}. 
           
           * Los datasets son BACI_HS96 ({length(HS96files)} archivos) y BACI_HS07 ({length(HS07files)} archivos).
           
              - HS07files contiene un .csv para cada año del período {period <- str_extract(HS07files, '(?<=_Y)[[:digit:]]{4}'); period[1]} - {period <- str_extract(HS07files, '(?<=_Y)[[:digit:]]{4}'); period[length(period)]} 
              
              - HS96files contiene un .csv para cada año del período {period <- str_extract(HS96files, '(?<=_Y)[[:digit:]]{4}'); period[1]} - {period <- str_extract(HS07files, '(?<=_Y)[[:digit:]]{4}'); period[length(period)]} 
           
           ") %>% 
  write_lines(file = glue::glue("{Sys.getenv('BACI_PATH')}/{raw_file}"))



# agregar_fuente_raw(url = url_fuente,
#                    nombre = glue::glue("{raw_file}"),
#                    institucion = institution,
#                    actualizable = F,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = raw_file,
#                    script = code_name,
#                    api = F
# )


actualizar_fuente_raw(id_fuente = "R113C0", dir = tempdir()) 
