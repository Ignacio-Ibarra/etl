# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "indice_gasto_publico_consolidado_per_capita.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"


fuente1 <- 'R325C276' # Gasto Público consolidado $ corrientes
fuente2 <- 'R38C194' # Cuentas Nacionales. Agregados macroeconómicos (PIB). Series trimestrales de oferta y demanda globales. Años 2004-2024 - Cuadro 9: Indice de Precios Implícitos Indec (Cuadro 9)
fuente3 <- 'R429C275' # Kidyba, Susana y Suárez, Luis (2021). Documento de trabajo N° 5. Aplicación de los Índices Encadenados al empalme de series Argentina (1935-2020). Anexo estadístico (C2)
fuente4 <- 'R46C0' # Population, total


df_mecon <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()%>% 
  dplyr::filter(codigo == "1.0") %>% 
  mutate(gasto_consolidado = ifelse(anio < 1987, valores * 1000, valores * 1000000)) %>% 
  select(anio, gasto_consolidado)

df_indec_ipi <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  dplyr::filter(trim == "Total", indicador == "producto_interno_bruto") %>% 
  mutate(ipi = valor /100) %>% 
  select(anio, ipi)

df_picna_ipi <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet() %>% 
  mutate(anio = as.integer(anio)) %>% 
  dplyr::filter(variables == "indice_de_precios_implicitos_encadenado", anio < 2004) %>% 
  select(anio, ipi = valores)

df_wb_pob <- argendataR::get_raw_path(fuente4) %>% 
  read_csv() %>% 
  dplyr::filter(iso3c == "ARG") %>% 
  select(anio = year, poblacion = `SP.POP.TOTL`) 


df_ipi <- bind_rows(df_picna_ipi, df_indec_ipi)


df_output <- df_mecon %>% 
  left_join(df_wb_pob, join_by(anio)) %>% 
  left_join(df_ipi, join_by(anio)) %>% 
  arrange(anio) %>% 
  mutate(gasto_publico_constante_pc = (gasto_consolidado / ipi) / poblacion, 
         indice_gasto_publico_consolidado_per_capita = gasto_publico_constante_pc / gasto_publico_constante_pc[anio == first(anio)]) %>% 
  select(anio, indice_gasto_publico_consolidado_per_capita)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) 

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


descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
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
    unidades = list("indice_gasto_publico_consolidado_per_capita" = "indice")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
