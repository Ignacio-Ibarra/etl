# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_consolidado_largo.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"


fuente1 <- 'R325C200' # Consolidado
fuente2 <- 'R428C0' # Porto


impute_backward <- function(A, B) {
  # Calcular las variaciones relativas de B
  result <- rep(NA_real_, length(A))
  
  VarB <- B / dplyr::lag(B)
  
  # Encontrar el primer índice con un valor no nulo en A
  t0 <- min(which(!is.na(A)))
  
  result[t0] = A[t0]
  
  # Imputar hacia atrás
  for (t in (t0 - 1):1) {
    if (!is.na(VarB[t + 1]) & is.na(A[t])) {
      result[t] <- result[t + 1] / VarB[t + 1]
    }
  }
  
  return(result)
}


df_mecon <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_porto <- argendataR::get_raw_path(fuente2) %>% 
  read_csv()


df_output <- df_mecon %>% 
  dplyr::filter(codigo == "1.0") %>% 
  select(anio, gasto_consolidado_pib_mecon = valores) %>% 
  full_join(df_porto %>% 
              select(anio, gasto_consolidado_pib_porto = gasto_consolidado_pib ), 
            join_by(anio)) %>% 
  arrange(anio) %>% 
  mutate(gasto_consolidado_pib_back = impute_backward(gasto_consolidado_pib_mecon, gasto_consolidado_pib_porto),
         gasto_publico_consolidado_pib_empalme = ifelse(is.na(gasto_consolidado_pib_back), gasto_consolidado_pib_mecon, gasto_consolidado_pib_back)) %>% 
  select(anio, gasto_publico_consolidado_pib_empalme)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  rename(gasto_publico_consolidado_pib_empalme = gasto_publico_consolidado)

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("anio")
)


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
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("gasto_publico_consolidado_pib_empalme"),
  descripcion = c("Gasto público consolidado como porcentaje del PIB (empalme entre las series de Porto y MECON")
)


descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = c("nivel_de_gobierno", "codigo"),
    descripcion_columnas = descripcion,
    unidades = list("gasto_publico_consolidado_pib_empalme" = "porcentaje")
  )

# 
# ggplot(comparacion$joined_df %>% pivot_longer(!anio), aes(x = anio, y = value, color = name)) + geom_line() + theme_minimal()
