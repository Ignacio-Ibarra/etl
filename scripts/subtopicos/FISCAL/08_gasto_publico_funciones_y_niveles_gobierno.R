# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_funciones_y_niveles_gobierno.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"


fuente1 <- 'R326C201' # Nacional
fuente2 <- 'R327C202' # Provincial
fuente3 <- 'R328C203' # Municipal

labels <- c("Nacional", "Provincial", "Municipal")
sources <- c(fuente1, fuente2, fuente3)

# Read, label, and combine
df_mecon <- purrr::map2_dfr(sources, labels, ~ {
  argendataR::get_clean_path(.x) %>% 
    arrow::read_parquet(.) %>%
    mutate(nivel_gobierno = .y)  # Add distinguishing column
}) 


df_output <- df_mecon %>% 
  dplyr::filter(anio == max(anio)) %>% 
  dplyr::filter(nchar(codigo) == 5 & !(codigo %in% c("1.2.1", "1.4.1")) | grepl("1\\.2\\.1\\..*", codigo) | codigo == "1.4" | codigo == "1.0") %>% 
  group_by(anio, codigo, nombre_apertura) %>% 
  mutate(participacion_en_el_gasto_publico_consolidado = valores / sum(valores)) %>% 
  ungroup() %>% 
  rename(nivel_de_gobierno = nivel_gobierno, gasto_publico_porcenataje_del_pib = valores, finalidad_funcion = nombre_apertura) %>% 
  mutate(finalidad_funcion = str_remove(finalidad_funcion, "^[IVX\\.0-9]+\\s+")) %>% 
  select(anio, nivel_de_gobierno, codigo, finalidad_funcion, gasto_publico_porcenataje_del_pib, participacion_en_el_gasto_publico_consolidado)

comparable_df <- df_output %>% 
  mutate(finalidad_funcion = janitor::make_clean_names(finalidad_funcion, allow_dupes = T),
         nivel_de_gobierno = tolower(nivel_de_gobierno)) %>% 
  select(-c(anio, codigo))

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) 

comparacion <- argendataR::comparar_outputs(
  df = comparable_df,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("nivel_de_gobierno", "finalidad_funcion")
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
                      "codigo"),
  descripcion = c("Año de referencia",
                  "Código identificador de la finalidad/función")
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
    unidades = list("gasto_publico_porcenataje_del_pib" = "proporcion",
                    "participacion_en_el_gasto_publico_consolidado" = "proporcion")
  )
