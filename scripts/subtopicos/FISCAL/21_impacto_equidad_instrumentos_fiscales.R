# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "impacto_equidad_instrumentos_fiscales.csv"
analista <- "María Fernanda Villafañe, Micaela Fernandez Erlauer & Daniel Schteingart"

fuente1 <- 'R431C277' # CEQ Master Workbook. Argentina. Fiscal interventions, (versión 19 de Marzo de 2018) - E11.m+p FiscalInterventions

df_ceq <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_output <- df_ceq %>% 
  dplyr::filter(variable %in% c("CONCENTRATION PER CAPITA (US PPP DOLLARS)",  
                                "FISCAL INCIDENCE WITH RESPECT TO MARKET INCOME + PENSIONS", 
                                "CONCENTRATION SHARES"),
                desagregacion == 9 | name_esp == "Total – Subsidios",
                decil != "TOTAL") %>%
  mutate(decil = as.integer(decil),
         variable = case_when(
           variable == "CONCENTRATION PER CAPITA (US PPP DOLLARS)" ~ "Concentración per cápita (en dólares PPP 2011)",
           variable == "FISCAL INCIDENCE WITH RESPECT TO MARKET INCOME + PENSIONS" ~ "Incidencia fiscal respecto al ingreso de mercado más pensiones",
           variable == "CONCENTRATION SHARES" ~ "Participación en la concentración",
           TRUE ~ NA_character_
         )) %>% 
  select(decil, codigo, desagregacion, gran_categoria, categoria, subcategoria, rubro, instrumento = name_esp, variable, valor = value) %>% 
  arrange(codigo, variable, decil)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  mutate(decil = as.integer(decil))


comparacion <- argendataR::comparar_outputs(
  df = df_output %>% select(decil, codigo, variable, valor),
  df_anterior = df_anterior %>% select(decil, codigo, variable, valor),
  nombre = output_name,
  pk = c("decil", "codigo", "variable")
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
    pk = c("decil", "codigo", "variable"),
    descripcion_columnas = descripcion,
    unidades = list("valor" = "unidad")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
