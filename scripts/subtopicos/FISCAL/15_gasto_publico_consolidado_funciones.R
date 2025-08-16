#Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_consolidado_funciones.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"


fuente1 <- 'R325C200' # Gasto Público consolidado % PIB


df_mecon <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_exp <- df_mecon %>%
  dplyr::filter(codigo != "1.0") %>% 
  separate(codigo, into = c("nivel1","nivel2","nivel3","nivel4"), sep = "\\.", fill = "right", remove = FALSE) %>%
  mutate(
    cod_finalidad  = if_else(!is.na(nivel2), str_c(nivel1, nivel2, sep = "."), NA_character_),
    cod_funcion    = if_else(!is.na(nivel3), str_c(nivel1, nivel2, nivel3, sep = "."), NA_character_),
    cod_subfuncion = if_else(!is.na(nivel4), str_c(nivel1, nivel2, nivel3, nivel4, sep = "."), NA_character_)
  ) %>% 
  select(anio, codigo, cod_finalidad, cod_funcion, cod_subfuncion, gasto_publico_consolidado = valores)


lookup <- df_mecon %>% 
  mutate(nombre_apertura = str_remove(nombre_apertura, "^[IVX\\.0-9]+\\s+")) %>%
  mutate(nivel = str_count(codigo, "\\."),
         tipo = factor(nivel, levels = c(1,2,3), labels = c("finalidad", "funcion", "subfuncion"))) %>% 
  distinct(codigo, nivel, tipo, nombre_apertura)

df_output <- df_exp %>%
  left_join(lookup %>% 
              dplyr::filter(tipo == "finalidad") %>% 
              distinct(codigo, nombre_apertura), by = c("cod_finalidad" = "codigo")) %>%
  rename(finalidades = nombre_apertura) %>%
  left_join(lookup %>% 
              dplyr::filter(tipo == "funcion") %>% 
              distinct(codigo, nombre_apertura), by = c("cod_funcion" = "codigo")) %>%
  rename(funciones = nombre_apertura) %>%
  left_join(lookup %>% 
              dplyr::filter(tipo == "subfuncion") %>% 
              distinct(codigo, nombre_apertura), by = c("cod_subfuncion" = "codigo")) %>%
  rename(subfunciones = nombre_apertura) %>% 
  left_join(lookup %>% select(codigo, nivel), join_by(codigo)) %>% 
  select(anio, codigo, nivel, finalidades, funciones, subfunciones, gasto_publico_consolidado) %>% 
  dplyr::filter(
    # condición 1: el código es 1.4
    codigo == "1.4" |
      
      # condición 2: funciones sin sub-funciones
      (nivel == 2 & !codigo %in% str_remove(df_mecon$codigo[str_count(df_mecon$codigo, "\\.") == 3], "\\.[^.]+$")) |
      
      # condición 3: todas las subfunciones
      nivel == 3
  ) %>% 
  dplyr::filter(codigo != "1.4.1") %>% 
  select(anio, codigo, finalidades, funciones, subfunciones, gasto_publico_consolidado)




df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  mutate(anio = as.integer(anio))


comparable_df <- df_output %>% 
  mutate(funciones = janitor::make_clean_names(funciones, allow_dupes = T),
         subfunciones = janitor::make_clean_names(subfunciones, allow_dupes = T)) %>% 
  select(anio, funciones, subfunciones, gasto_publico_consolidado)



comparacion <- argendataR::comparar_outputs(
  df = comparable_df,
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
  variable_nombre = c("anio", 
                      "finalidades",
                      "funciones",
                      "gasto_publico_consolidado",
                      "codigo"),
  descripcion = c("Año de referencia",
                  "Clasificación por finalidades del gasto público consolidado",
                  "Clasificación por funciones del gasto público consolidado",
                  "Gasto público consolidado, como porcentaje del PIB",
                  "Código identificador de finalidad/función/subfunción")
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
    pk = c("anio"),
    descripcion_columnas = descripcion,
    unidades = list("gasto_publico_consolidado" = "porcentaje")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
