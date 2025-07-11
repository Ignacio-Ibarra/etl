################################################################################
##                              Dataset: nombre                               ##
################################################################################
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "principales_mercados_agro_argentina.csv"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R337C0" # BACI HS17
fuente2 <- "R339C212" # Diccionario HS17 a Grandes Rubros
fuente3 <- "R340C213" # Codigos HS con descripciones en portugués, inglés y español
 

geonomenclador <- argendataR::get_nomenclador_geografico_front()

diccionario_ncm8 <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.) 

diccionario_hs17_6d <- diccionario_ncm8 %>% 
  mutate(ncm6 = str_sub(ncm8, start = 1L, end = 6L)) %>% 
  distinct(ncm6, gran_rubro)

diccionario_espaniol <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.) %>% 
  distinct(co_sh6, no_sh6_esp)

source("scripts/utils/baci_data.R")

con <- argendataR::get_raw_path(fuente1) %>% 
  BACI.get_db_from_zip(.)

dbWriteTable(con, "diccionario_gran_rubro", diccionario_hs17_6d, overwrite = TRUE)

dbWriteTable(con, "diccionario_espaniol", diccionario_espaniol, overwrite = TRUE)

# productos_tbl <- BACI.get_table(con, "productos")
# 
# paises_tbl <- BACI.get_table(con, "paises")
# 
# comercio_tbl <- BACI.get_table(con, "comercio")
# 
# dic_gran_rubro_tbl <- BACI.get_table(con, "diccionario_gran_rubro")
# 
# dic_espaniol_tbl <- BACI.get_table(con, "diccionario_espaniol")


query_aux <- "WITH filtro AS (
  SELECT k as code, SUM(v) AS expo
  FROM comercio
  WHERE t = (SELECT MAX(t) FROM comercio)
  AND i = 32
  GROUP BY k
)
SELECT
  f.code as ncm6,
  d_esp.no_sh6_esp as desc_ncm6,
  f.expo,
  d_g.gran_rubro
FROM filtro f
LEFT JOIN diccionario_gran_rubro d_g ON f.code = d_g.ncm6
LEFT JOIN diccionario_espaniol d_esp ON f.code = d_esp.co_sh6
WHERE d_g.gran_rubro IN ('PP', 'MOA')
ORDER BY f.expo DESC
LIMIT 10;"



posiciones_arg <- dbGetQuery(con, query_aux) %>%
  select(ncm6, desc_ncm6)


posiciones_str <- posiciones_arg %>% pull(ncm6) %>% paste0(., collapse=", ")

query_output <- glue::glue(
  "SELECT c.i as m49_code, p.country_iso3 as iso3, c.k as ncm6, SUM(c.v) as expo
   FROM comercio as c
   LEFT JOIN paises as p ON c.i = p.country_code  -- ✅ El JOIN ahora está antes del WHERE
   WHERE c.k IN ({posiciones_str})
   AND c.t = (SELECT MAX(t) FROM comercio)
   GROUP BY c.i, p.country_iso3, c.k"
)


df_query <- dbGetQuery(con, query_output)

df_output <- df_query %>%
  left_join(posiciones_arg, join_by(ncm6)) %>%
  mutate(ncm6 = str_pad(ncm6, width = 6, side = "left", pad = "0")) %>%
  group_by(ncm6) %>%
  mutate(
    share = round(100*expo / sum(expo, na.rm = T),3),
    ranking = as.integer(rank(-expo))
  ) %>%
  ungroup() %>%
  left_join(geonomenclador %>% select(iso3 = geocodigo, pais_nombre = name_short), join_by(iso3)) %>%
  select(iso3, pais_nombre, ncm6, desc_ncm6, expo, share, ranking)





# Esto lo hago como hack para poder hacer la comparación.
df_anterior <- df_output


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("iso3","ncm6"), # variables pk del dataset para hacer el join entre bases
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


# No hay metadatos, los creo artificialmente. 
metadatos <- data.frame(
  variable_nombre = c("iso3",
                      "pais_nombre",
                      "ncm6",
                      "desc_ncm6",
                      "expo",
                      "share",
                      "ranking"),
  descripcion = c("Código de país ISO 3166-1 alfa-3",
                  "Nombre de país de referencia",
                  "Código de producto del Sistema Armonizado (HS, enmienda 2017) a 6 dígitos de desagregación",
                  "Descripción en español del producto de referencia",
                  "Exportaciones en miles de dólares",
                  "Participación del país en el comercio global del producto",
                  "Puesto que ocupa el país en el comercio global del producto"
  )
)

# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
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
    pk =  c("iso3","ncm6"),
    es_serie_tiempo = F,
    control = comparacion, 
    descripcion_columnas = descripcion,
    unidades = list("expo" = "miles de dólares",
                    "share" = "porcentaje")
  )