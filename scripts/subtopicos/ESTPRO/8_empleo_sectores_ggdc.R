################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "empleo_sectores_ggdc"
analista = "Gisella Pascuariello"

fuente1 <- "R231C102"


get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}



letra_desc_abrev <- list(
  'A' =  'Agro y pesca',
  'B' = 'Petróleo y minería',
  'C' = 'Industria manufacturera',
  'D+E' = 'Electricidad, gas y agua',
  'F' = 'Construcción',
  'G+I' ="Comercio, hoteles y restaurantes",
  'H' = 'Transporte',
  'J+M+N' = 'Servicios empresariales',
  'K' = 'Finanzas',
  'L' =  'Servicios inmobiliarios',
  'M' = 'Enseñanza',
  'O+P+Q' =  "Administración pública, defensa, educación y salud",
  'R+S+T+U' = 'Otros servicios'
)

df_output <- arrow::read_parquet(get_clean_path(fuente1))  %>% 
  dplyr::filter(var_code == "EMP") %>% 
  select(-sector, -var_code, -var_desc) %>% 
  rename(sector_codigo =  isic_4_code,
         iso3 = cnt,
         anio = year,
         empleo_miles = value) %>% 
  mutate( sector_desc = recode(sector_codigo, !!!letra_desc_abrev)) %>% 
  dplyr::filter(anio>=1950) %>% 
  group_by(anio, iso3) %>% 
  mutate( share_empleo = empleo_miles / sum(empleo_miles, na.rm = T)) %>% 
  mutate( 
    gran_sector = ifelse(sector_codigo %in% c("A", "B", "C", "D+E", "F"), "Bienes", "Servicios")) %>% 
  select(anio, iso3, gran_sector, sector_codigo, sector_desc, empleo_miles, share_empleo)






# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre = "empleo_sectores_ggdc_1950_2018", subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate(sector_codigo = case_when(
    sector_codigo == "DE" ~ "D+E",
    sector_codigo == "GI" ~ "G+I",
    sector_codigo == "JMN" ~ "J+M+N",
    sector_codigo == "OPQ" ~ "O+P+Q",
    sector_codigo == "RSTU" ~ "R+S+T+U",
    TRUE ~ sector_codigo)) 

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","iso3", "sector_codigo"), # variables pk del dataset para hacer el join entre bases
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
  dplyr::filter(grepl(paste0("empleo_sectores_ggdc_1950_2018",".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


# etiquetas_nuevas <- data.frame(
#   variable_nombre = c("puestos", 
#                       "share_puestos",
#                       "vab_pb"),
#   descripcion = c("Cantidad de puestos",
#                   "Participación del sector en el total de puestos de trabajo, en proporción",
#                   "Valor agregado bruto a precios básicos en millones de pesos a precios corrientes")
# )

descripcion <- armador_descripcion(metadatos = metadatos,
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
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio", "iso3", "sector_codigo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    cambio_nombre_output = list("nombre_nuevo" = "empleo_sectores_ggdc",
                                "nombre_anterior" = "empleo_sectores_ggdc_1950_2018"),
    unidades = list("empleo_miles" = "miles", 
                    "share_empleo" =  "proporcion")
  )


