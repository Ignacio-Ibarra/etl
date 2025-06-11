#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "03_composicion_exportaciones_pesca_producto.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R319C188' # INDEC_COMEX


df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

pescado <- c("03","1504","1604","1605","230120", "05080000", "051191","16030090") #obtenidos de https://www.indec.gob.ar/ftp/cuadros/economia/nota_metodologica_complejos_exportadores_2024.pdf

pattern_pescado <- paste0("^(", paste(pescado, collapse = "|"), ")")

tabla_str <- "partida;descripcion;gran_rubro
0301;Peces vivos;PP
0302;Pescado fresco o refrigerado, exc. filetes;PP
0303;Pescado congelado, exc. filetes;PP
0304;Filetes y demás carnes de pescado;MOA
0305;Pescado sec./sal./en salm. har./pol./pell. aptos p/c humano;MOA
0306;Crustáceos;PP
0307;Moluscos;PP
051191;Prod. no exp. en otros capítulos. No aptos p/c humano;MOA
16030090;Extractos y jugos de pescado y mariscos;MOA
1504;Grasas y aceites de pescado;MOA
1604;Preparaciones y conservas de pescado;MOA
1605;Preparaciones y conservas de mariscos;MOA
230120;Harina, polvo y pellets de pescado. No aptos p/c humano;MOA"


diccionario_magyp <- readr::read_delim(I(tabla_str), delim = ";")


df_output <- df_indec %>% 
  drop_na(fob) %>% 
  dplyr::filter(grepl(pattern_pescado, ncm8), anio == max(anio)) %>%
  mutate(
    toneladas = pnet_kg/1000,
    fob_miles_usd = fob/1000,
    producto = map_chr(ncm8, ~ {
      match <- diccionario_magyp %>% filter(str_starts(.x, partida)) %>% pull(descripcion)
      if (length(match) > 0) match[1] else NA_character_
        }
      ),
    gran_rubro = map_chr(ncm8, ~ {
      match <- diccionario_magyp %>% filter(str_starts(.x, partida)) %>% pull(gran_rubro)
      if (length(match) > 0) match[1] else NA_character_
      }
    )
  ) %>% 
  group_by(gran_rubro, producto) %>% 
  summarise(
    toneladas = sum(toneladas, na.rm = T),
    fob_miles_usd = sum(fob_miles_usd, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    share_toneladas = toneladas / sum(toneladas),
    share_fob = fob_miles_usd / sum(fob_miles_usd)
  )



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("producto"), # variables pk del dataset para hacer el join entre bases
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
    pk =  c("producto"),
    es_serie_tiempo = F,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )

