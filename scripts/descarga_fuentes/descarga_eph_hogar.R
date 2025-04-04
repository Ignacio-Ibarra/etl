# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-04-01")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad  

source("scripts/utils/funciones_descarga_eph.R")

# Parametros para guardar archivo
tipo_encuesta <- "hogar"
version <- glue::glue("v{format(Sys.Date(), '%Y%m%d')}")
ext <- "csv"

# Cuanto comenzó la EPH
eph_init_year <- 2003

# Año actual
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Año anterior al actual
last_year <-  current_year - 1

# Años transcurridos
anios_efectivos <- eph_init_year:last_year

# Fuentes_raw
fuentes_raw_df <- fuentes_raw()

# Años descargados
anios_descargados <- fuentes_raw_df %>% 
  filter(grepl("Encuesta Permanente de Hogares, Hogar*",nombre)) %>% 
  select(nombre) %>% 
  pull() %>% 
  str_extract(., "\\d{4}") %>% 
  as.numeric(.)

# Años anteriores no descargados
anios_anteriores_a_agregar <- setdiff(anios_efectivos, anios_descargados)

stopifnot(length(anios_anteriores_a_agregar) != 0)


# Si no tengo algun anio anterior lo descarga
if (length(anios_anteriores_a_agregar)>0){
  
  for (anio in anios_anteriores_a_agregar){
    
    funcion_por_anio(anio = anio, 
                     fuentes_raw_df = fuentes_raw_df, 
                     tipo_encuesta = tipo_encuesta,
                     version = version, 
                     ext = ext,
                     fecha_actualizar = fecha_actualizar, 
                     code_name = code_name)
    
  }
  
}


# Setear en TRUE si se quiere actualizar
ACTUALIZAR_ANIOS_ANTERIORES <- FALSE 


# Agregar años anteriores si no están descargados o los actualiza
if (ACTUALIZAR_ANIOS_ANTERIORES){
  
  # itero sobre los descargados porque los 
  # que no estaban descargados fueron descargados
  # en las lineas de arriba
  for (anio in anios_descargados){
    
    funcion_por_anio(anio = anio, 
                     fuentes_raw_df = fuentes_raw_df, 
                     tipo_encuesta = tipo_encuesta,
                     version = version, 
                     ext = ext,
                     output_folder = output_folder,
                     fecha_actualizar = fecha_actualizar, 
                     code_name = code_name)
    
  }
}


# Descarga de año actual
fecha_prim_trim <- as.Date(sprintf("%s-08-05", current_year))
current_date <- as.Date(Sys.Date())
if ( current_date > fecha_prim_trim ){
  
  funcion_por_anio(anio = current_year, 
                   fuentes_raw_df = fuentes_raw_df, 
                   tipo_encuesta = tipo_encuesta,
                   version = version, 
                   ext = ext,
                   fecha_actualizar = fecha_actualizar, 
                   code_name = code_name)
  
}

cat("Fin de script!")