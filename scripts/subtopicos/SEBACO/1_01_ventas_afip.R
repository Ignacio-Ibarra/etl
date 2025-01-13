#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "01_ventas_afip"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R250C119' # Anuario AFIP 2022
fuente2 <- 'R251C120' # Anuario AFIP 2021
fuente3 <- 'R252C121' # Anuario AFIP 2020
fuente4 <- 'R253C122' # Anuario AFIP 2019
fuente5 <- 'R254C123' # Anuario AFIP 2018
fuente6 <- 'R255C124' # Anuario AFIP 2017
fuente7 <- 'R256C125' # Anuario AFIP 2016
fuente8 <- 'R257C126' # Anuario AFIP 2015
fuente9 <- 'R258C127' # Anuario AFIP 2014


# Fuentes con años correspondientes
sources <- list(
  "R250C119" = 2022,
  "R251C120" = 2021,
  "R252C121" = 2020,
  "R253C122" = 2019,
  "R254C123" = 2018,
  "R255C124" = 2017,
  "R256C125" = 2016,
  "R257C126" = 2015,
  "R258C127" = 2014
)

# Función para leer, castear columnas como character y agregar el año
read_with_year <- function(file, year) {
  df <- arrow::read_parquet(file) %>%
    mutate(anio = as.integer(year))  # Agregar columna con el año
  return(df)
}

# Obtener paths de los archivos
all_paths <- purrr::map_chr(names(sources), ~ argendataR::get_clean_path(.x))

# Leer y combinar datasets agregando el año correspondiente
df_afip_ventas <- map2_dfr(all_paths, sources, ~ read_with_year(.x, .y))


df_vtas_totales <- df_afip_ventas %>% 
  dplyr::filter(detalle == "Ventas totales", destino_venta == "Total", nivel_agregacion %in% c("letra", NA)) %>% 
  group_by(anio) %>% 
  summarise(
    vta_total = sum(valor, na.rm = T)
  ) %>% 
  ungroup()



# Sectores elegidos 
# Selección de sectores 
sectores_elegidos <- c(592000, # Equivalente a Edición de grabaciones (2213)
                       303000, # Equivalente a Fabricación y reparación de aeronaves (3530)
                       c(620000:639999), # SSI
                       c(721010:722020), # I+D
                       c(691001,691002), # Juridicos
                       692000, # Contabilidad
                       c(702091:702099), # Servicios de asesoramiento (dentro de contabilidad)
                       732000, # Estudios de mercado (dentro de contabilidad)
                       c(731001,731009), # Servicios de publicidad
                       780000, # Dotación de personal
                       742000, # Servicios de fotografía
                       c(591110,591120,591200) # Filmes
)
clae3_elegidos <- str_remove(sectores_elegidos,'[0-9]{3}$')
clae3_elegidos <- unique(as.character(clae3_elegidos))

df_sbc <- df_afip_ventas %>% 
  dplyr::filter(detalle == "Ventas totales", destino_venta == "Total", cod_act %in% clae3_elegidos) %>% 
  mutate(sector = "Servicios basados en conocimiento") %>% 
  group_by(anio, sector) %>% 
  summarise(
    ventas = sum(valor, na.rm = T)
  ) %>% 
  ungroup()


df_output <- df_sbc %>% 
  left_join(df_vtas_totales, join_by(anio)) %>% 
  mutate(
    share_ventas = ventas / vta_total
  ) %>% 
  select(anio, sector, share_ventas)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(sector = "Servicios basados en conocimiento",
         anio = as.integer(anio))



comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('anio','sector'),
  drop_joined_df =  F
)


#-- Exportar Output ----

armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("sector", 
                      "share_ventas"),
  descripcion = c("Rama de actividad agregada en 'Servicios basados en conocimiento', iuncluye CLAE a 3 dígitos: 303, 591, 592, 620, 621, 622, 623, 624, 625, 626, 627, 628, 629, 630, 631, 632, 633, 634, 635, 636, 637, 638, 639, 691, 692, 702, 721, 722, 731, 732, 742, 780)",
                  "Proporción de las ventas totales del país explicadas por 'Servicios basados en conocimiento'")
)


descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)

colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('anio', 'sector'),
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("share_ventas" = "unidades")
  )
