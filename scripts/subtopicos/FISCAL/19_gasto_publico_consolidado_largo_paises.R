# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_consolidado_largo_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R424C272' # FMI
fuente2 <- 'R325C200' # Consolidado
fuente3 <- 'R428C0' # Porto


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


df_imf <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_mecon <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_porto <- argendataR::get_raw_path(fuente3) %>% 
  read_csv()


df_arg_empalme <- df_mecon %>% 
  dplyr::filter(codigo == "1.0") %>% 
  select(anio, gasto_consolidado_pib_mecon = valores) %>% 
  full_join(df_porto %>% 
              select(anio, gasto_consolidado_pib_porto = gasto_consolidado_pib ), 
            join_by(anio)) %>% 
  arrange(anio) %>% 
  mutate(gasto_consolidado_pib_back = impute_backward(gasto_consolidado_pib_mecon, gasto_consolidado_pib_porto),
         gasto_publico_consolidado_pib_empalme = ifelse(is.na(gasto_consolidado_pib_back), gasto_consolidado_pib_mecon, gasto_consolidado_pib_back),
         geocodigoFundar = "ARG",
         geonombreFundar = "Argentina",
         fuente = "Porto + MECON") %>% 
  select(anio, geocodigoFundar, geonombreFundar, gasto_publico_consolidado = gasto_publico_consolidado_pib_empalme, fuente)

df_output <- df_imf %>% 
  dplyr::filter(iso3 != "ARG") %>% 
  mutate(fuente = "FMI") %>% 
  select(anio, geocodigoFundar = iso3, geonombreFundar = pais_nombre, gasto_publico_consolidado = exp, fuente) %>% 
  bind_rows(df_arg_empalme) %>% 
  drop_na(gasto_publico_consolidado) %>% 
  dplyr::filter(anio >= 1900) %>% 
  mutate(anio = as.integer(anio))
  



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  rename(geocodigoFundar = codigo_pais, geonombreFundar = pais) %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar", "anio")
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
  variable_nombre = c("geocodigoFundar", 
                      "geonombreFundar",
                      "fuente"),
  descripcion = c("Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país",
                  "Fuente de información utilizada")
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
    pk = c("geocodigoFundar", "anio"),
    descripcion_columnas = descripcion,
    unidades = list("gasto_publico_consolidado" = "porcentaje")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
