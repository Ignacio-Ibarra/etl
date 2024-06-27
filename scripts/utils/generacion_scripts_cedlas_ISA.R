#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

# source("scripts/utils/funciones_descarga_cedlas_desde_metadata.R")

SUBTOP <- "SALING"
scripts.folder <- glue::glue("scripts/subtopicos/{SUBTOP}")
scripts.files <- list.files(scripts.folder)
scripts.df <- data.frame(scripts_names = scripts.files[grepl(".R$", scripts.files)]) %>% 
  mutate(id = str_extract(scripts_names, "(.*).R$", group = 1))

metadata.df <- argendataR::metadata(subtopico = SUBTOP) %>%
  dplyr::filter(grepl("https://www.cedlas.econo.unlp.edu.ar/*", url_path))

# Busco links en la metadata de SUBTOP
datasets.cedlas <- metadata.df %>%
  mutate(path_raw = str_replace(url_path, "https://www.cedlas.econo.unlp.edu.ar/wp/wp-content/uploads/", "")) %>% 
  distinct(orden_grafico, dataset_archivo, path_raw) %>% 
  mutate(id = purrr::map2_chr(orden_grafico, str_extract(dataset_archivo, "(.*).csv", group = 1), ~paste0(.x, "_", .y))) %>% 
  select(-orden_grafico)


datasets.cedlas_jn <- datasets.cedlas %>% left_join(scripts.df, join_by(id)) %>% 
  group_by(dataset_archivo) %>% 
  mutate(no_va = first(scripts_names) != scripts_names) %>% 
  ungroup()

scripts_borrar <- datasets.cedlas_jn  %>% 
  dplyr::filter(no_va) %>% 
  select(scripts_names) %>% pull()

if (length(scripts_borrar)>0){
  file.remove(paste0(scripts.folder,"/",scripts_borrar))
}


datasets.cedlas_generar <- datasets.cedlas_jn %>% 
  dplyr::filter(!no_va) %>% 
  select(-no_va)

fuentes.raw <- fuentes_raw() %>% 
  dplyr::filter(path_raw %in% datasets.cedlas_generar$path_raw) 

lineas_base <- readLines("./scripts/utils/template_creacion_scripts_cedlas_ISA.txt")
lineas_string <- paste(lineas_base, collapse = "\n")


for (i in 1:nrow(datasets.cedlas_generar)){
  
  raw_filename <- datasets.cedlas_generar$path_raw[[i]]
  SCRIPT_NAME <- datasets.cedlas_generar$scripts_names[[i]]
  script_path <- glue::glue("{scripts.folder}/{SCRIPT_NAME}")
  OUTPUT_NAME <- datasets.cedlas_generar$dataset_archivo[[i]]
  
  INPUT_RAW_ID = fuentes.raw %>% 
    filter(path_raw == raw_filename) %>% select(id_fuente) %>% pull()
  
  pkeys <- metadata.df %>% 
    dplyr::filter(dataset_archivo == OUTPUT_NAME & as.logical(primary_key)) %>%
    distinct(variable_nombre) %>% 
    pull() 
  
  indicadores <- metadata.df %>% 
    dplyr::filter(dataset_archivo == OUTPUT_NAME) %>% 
    distinct(variable_nombre, descripcion, unidad_medida) %>% 
    dplyr::filter(!(variable_nombre %in% pkeys))
  
  ETIQUETAS_INDICADORES <- paste(glue::glue("'{indicadores$variable_nombre}' = '{indicadores$descripcion}'"), collapse =" ,")
  
  UNIDADES <- paste(glue::glue("'{indicadores$variable_nombre}' = '{indicadores$unidad_medida}'"), collapse =" ,")
  
  PK <- glue::glue("'{pkeys}'")
  PK <- paste0(PK, collapse = ",")
  
  
    
  output_script_str <- glue::glue(lineas_string, 
                                  SUBTOP = SUBTOP,
                                  INPUT_RAW_ID = INPUT_RAW_ID, 
                                  OUTPUT_NAME = OUTPUT_NAME,
                                  PK = PK,
                                  ETIQUETAS_INDICADORES = ETIQUETAS_INDICADORES,
                                  UNIDADES = UNIDADES
                                  )
  
  file.remove(script_path)
  
  stringi::stri_write_lines(output_script_str, con = script_path)
  cat("Generando script: ", SCRIPT_NAME, "\n")
}


escribir_descarga_fuentes <- fuentes.raw %>% select(nombre, codigo)

lines_descarga <- c()
for (i in 1:nrow(escribir_descarga_fuentes)){
  
  comment <- glue::glue("# {escribir_descarga_fuentes$nombre[[i]]}")
  codigo_descarga <-glue::glue("descargar_fuente('{escribir_descarga_fuentes$codigo[[i]]}')") 
  lines_descarga <- c(lines_descarga, comment,"\n", codigo_descarga, "\n\n")
  
}

stringi::stri_write_lines(lines_descarga, con = glue::glue("{scripts.folder}/fuentes_{SUBTOP}.R"), sep="")
