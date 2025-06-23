#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "05_expo_pesquera_por_especie_ultimo_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R319C188' # INDEC_COMEX
fuente2 <- 'R338C211' # INDEC Complejos Exportadores. Revisión 2018. 2021 a ultimo anio

df_indec <- argendataR::get_clean_path(fuente1) %>%
  arrow::read_parquet(.) 

df_complejos <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)


pescado <- c("03","1504","1604","1605","230120", "05080000", "051191","16030090") #obtenidos de https://www.indec.gob.ar/ftp/cuadros/economia/nota_metodologica_complejos_exportadores_2024.pdf

pattern_pescado <- paste0("^(", paste(pescado, collapse = "|"), ")")


# Accediendo a una base de datos de exportaciones de pesca a 12 dígitos NCM 
# se clasificaron las posiciones a 8 dígitos en especies de pescado. 
# esta clasificación debe adaptarse todos los años dado que hay posiciones nuevas que podrían
# aparecer en algunos años que antes no se exportaban y, sobre todo
especies_ncm <- c(
  "03061710" = "Langostino",
  "03074310" = "Calamar Illex",
  "03061790" = "Langostino",
  "03047400" = "Merluza Hubbsi",
  "03038311" = "Otras especies",
  "03036600" = "Merluza Hubbsi",
  "03079200" = "Otras especies",
  "03038910" = "Otras especies",
  "03061400" = "Otras especies",
  "23012010" = "Otras especies",
  "03049500" = "Merluza Hubbsi",
  "03038990" = "Otras especies",
  "03038951" = "Otras especies",
  "03048990" = "Otras especies",
  "03056300" = "Otras especies",
  "03038920" = "Otras especies",
  "16041900" = "Otras especies",
  "03038200" = "Otras especies",
  "03074310" = "Otras especies",
  "03047900" = "Otras especies",
  "03044200" = "Otras especies",
  "16042090" = "Otras especies",
  "03048510" = "Otras especies",
  "03039910" = "Otras especies",
  "03072200" = "Otras especies",
  "03044400" = "Merluza Hubbsi",
  "03021100" = "Otras especies",
  "03061710" = "Otras especies",
  "16041600" = "Otras especies",
  "03032500" = "Otras especies",
  "15042000" = "Otras especies",
  "03053900" = "Otras especies",
  "03039990" = "Merluza Hubbsi",
  "03038319" = "Otras especies",
  "03036990" = "Otras especies",
  "03048890" = "Otras especies",
  "03039100" = "Otras especies",
  "16051000" = "Otras especies",
  "03061790" = "Otras especies",
  "03048200" = "Otras especies",
  "03038942" = "Otras especies",
  "03036600" = "Otras especies",
  "03036910" = "Otras especies",
  "23012090" = "Otras especies",
  "03049219" = "Otras especies",
  "03038954" = "Otras especies",
  "03048930" = "Otras especies",
  "03049211" = "Otras especies",
  "03049212" = "Otras especies",
  "16055400" = "Otras especies",
  "03028921" = "Otras especies",
  "03035920" = "Otras especies",
  "03039200" = "Otras especies",
  "03038190" = "Otras especies",
  "03036800" = "Otras especies",
  "03032490" = "Otras especies",
  "03049300" = "Otras especies",
  "03044990" = "Otras especies",
  "03038955" = "Otras especies",
  "03057200" = "Otras especies",
  "05119190" = "Otras especies",
  "03028990" = "Otras especies",
  "16056900" = "Otras especies",
  "03075200" = "Otras especies",
  "16055600" = "Otras especies",
  "03038946" = "Otras especies",
  "03038943" = "Otras especies",
  "03073900" = "Otras especies",
  "03073200" = "Otras especies",
  "03075900" = "Otras especies",
  "03038963" = "Otras especies",
  "03031300" = "Otras especies",
  "03072900" = "Otras especies",
  "03077900" = "Otras especies",
  "03031400" = "Otras especies",
  "03028500" = "Otras especies",
  "03048700" = "Otras especies",
  "03077200" = "Otras especies",
  "03069500" = "Otras especies",
  "03057100" = "Otras especies",
  "03035400" = "Otras especies",
  "03039190" = "Otras especies"
)


df_expo_especies <- df_indec %>% 
  drop_na(fob) %>% 
  dplyr::filter(grepl(pattern_pescado, ncm8), anio > 2022) %>%  # filtro anio para que no haya problemas con HS
  group_by(anio, ncm8) %>% 
  summarise(
    fob_miles_usd = sum(fob, na.rm = T)/1000
  ) %>% 
  ungroup() %>% 
  mutate(
    especie = especies_ncm[ncm8],
   ) %>% 
  group_by(anio, especie) %>% 
  summarise(
    fob_mill_usd = sum(fob_miles_usd)/1000,
  ) %>% 
  ungroup()


df_total_complejo_pesquero <- df_complejos %>% 
  dplyr::filter(grepl("pesquero", complejos)) %>%
  select(anio, total_complejo = expo)


df_stage <- df_expo_especies %>% 
  left_join(df_total_complejo_pesquero, join_by(anio)) %>% 
  dplyr::filter(anio == max(anio))

total_expo <- unique(df_stage$total_complejo)


df_stage[df_stage$especie == "Otras especies", c("fob_mill_usd")] <- total_expo - sum(df_stage$fob_mill_usd, na.rm = T)


df_output <- df_stage %>% 
  select(-total_complejo) %>% 
  mutate(share_fob = fob_mill_usd / sum(fob_mill_usd))
  
df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("especie"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

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
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  c("especie"),
    es_serie_tiempo = F,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )