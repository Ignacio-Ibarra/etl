################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "empleo_minero"
analista = "Kevin Corfield"
fuente1 <- "R275C144" 
fuente2 <- "R238C145"


df_siacam <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

df_oede <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) 


puestos_oede_mineria <- df_oede %>%
  dplyr::filter(ciiu_rev3_2d %in% c("13", "14")) %>% 
  select(anio, rama_de_actividad, cant_promedio_puestos_privados) %>% 
  group_by(anio) %>% 
  summarise(puestos_oede_mineria = sum(cant_promedio_puestos_privados, na.rm = T)) %>% 
  ungroup()

puestos_oede_total <- df_oede %>%
  dplyr::filter(rama_de_actividad == "Total") %>% 
  select(anio, rama_de_actividad, puestos_oede_total = cant_promedio_puestos_privados) %>% 
  select(-rama_de_actividad)
  
puestos_siacam <- df_siacam %>% 
  select(anio_mes, provincia, genero, rubro, cantidad) %>% 
  complete(anio_mes, provincia, genero, rubro, fill = list(cantidad = 0)) %>% 
  arrange(provincia, genero, rubro) %>% 
  mutate(anio = year(anio_mes)) %>% 
  group_by(anio, provincia, genero, rubro) %>% 
  mutate(
    anio_completo = n() == 12
  ) %>% 
  ungroup() %>% 
  filter(anio_completo) %>% 
  select(-anio_completo) %>% 
  group_by(anio, anio_mes) %>%
  summarise(
    puestos_siacam = sum(cantidad, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  summarise( puestos_siacam = mean(puestos_siacam, na.rm = T)) %>% 
  ungroup()




impute_forward <- function(A, B) {
  
  result <- rep(NA_real_, length(A))
  
  # Calcular las variaciones relativas de B
  VarB <- B / dplyr::lag(B)
  
  # Encontrar el último índice con un valor no nulo en A
  t0 <- max(which(!is.na(A)))
  
  result[t0] = A[t0]
  
  # Imputar hacia adelante
  for (t in (t0 + 1):length(A)) {
    if (!is.na(VarB[t]) & is.na(A[t])) {
      result[t] <- result[t - 1] * VarB[t]
    }
  }
  
  return(result)
}

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

df_output <- puestos_oede_mineria %>% 
  left_join(puestos_siacam, join_by(anio)) %>% 
  mutate(puestos_empalme_fwd = impute_forward(puestos_siacam, puestos_oede_mineria),
         puestos_empalme_bck = impute_backward(puestos_siacam, puestos_oede_mineria),
         puestos_empalme = case_when(
           !is.na(puestos_siacam) ~ puestos_siacam,
           !is.na(puestos_empalme_fwd) ~ puestos_empalme_fwd,
           !is.na(puestos_empalme_bck) ~ puestos_empalme_bck
         )) %>% 
  select(anio, puestos_mineria = puestos_empalme) %>% 
  left_join(puestos_oede_total, join_by(anio)) %>% 
  mutate(puestos_mineria_share = 100 * puestos_mineria / puestos_oede_total ) %>% 
  select(-puestos_oede_total)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>%
  pivot_wider(id_cols = c(anio, empleo_minero_perc_formal_total), 
              names_from = empleo_base, 
              values_from = cantidad_puestos) %>%
  select(anio, puestos_mineria = empleo_minero_siacam, puestos_mineria_share = empleo_minero_perc_formal_total) %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("puestos_mineria",
                      "puestos_mineria_share"),
  descripcion = c("Suma de puestos privados registrados en industrias ligadas a la minería (según SIACAM, ver metodología en https://cdn.produccion.gob.ar/cdn-mineria/Datos-Abiertos-SIACAM/Metodologia-Empleo.pdf)",
                  "Participación de la suma de puestos privados registrados mineros en el total de puestos privados registrados")
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

aclaracion <- paste0("Se pasó del formato long a wide, debido a que era incorrecto como estaba. ",
                     "Se cambiaron los nombres de las columnas y las descripciones de la misma, para ser más clara la exposición. ",
                     "Se realizó empalme hacia atrás de SIACAM con datos de puestos de trabajo privados de los CIIU 13 y 14 de OEDE. ",
                     "Se realiza empalme hacia adelante de SICAM con datos de puestos privados de los CIIU 13 y 14 de OEDE, en caso de que SIACAM y OEDE estén actualizados al mismo año entonces se toma SIACAM",
                     collapse = " ")


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("puestos_mineria" = "unidades",
                    "puestos_mineria_share" = "porcentaje"),
    aclaracion = aclaracion
  )


