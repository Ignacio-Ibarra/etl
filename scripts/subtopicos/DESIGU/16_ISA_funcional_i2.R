#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)
subtopico <- 'DESIGU'
output_name <- 'ISA_funcional_i2'


fuente_1 <- "R35C76"
fuente_2 <- "R35C78"
fuente_3 <- "R35C79"


# Cuenta Generacion del Ingeso (RTA pp) - INDEC
rta_df <- arrow::read_parquet(argendataR::get_temp_path(fuente_1)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(anio, `Remuneración al trabajo asalariado` = participacion)

# Cuenta Generacion del Ingeso (IBM pp) - INDEC
ibm_df <- arrow::read_parquet(argendataR::get_temp_path(fuente_2)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(anio, `Ingreso mixto bruto` = participacion)

# Cuenta Generacion del Ingeso (EEB pp) - INDEC
eeb_df <- arrow::read_parquet(argendataR::get_temp_path(fuente_3)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(anio, `Excedente de explotación bruto` = participacion)


df_output <- rta_df %>% 
  left_join(ibm_df, by=join_by(anio)) %>% 
  left_join(eeb_df, by=join_by(anio)) %>% 
  pivot_longer(-anio, names_to = "categoria", values_to = "participacion")


df_anterior <- argendataR::descargar_output(nombre ='ISA_funcional_i2', 
                                            subtopico = "DESIGU", 
                                            entrega_subtopico = "datasets_primera_entrega") %>% 
  select(anio = ano, categoria = variable, participacion = valor) %>% 
  mutate(categoria  = case_when(
    categoria == "remuneraciontrabajo" ~ "Remuneración al trabajo asalariado",
    categoria == "ingresobrutomixto" ~ "Ingreso mixto bruto",
    TRUE ~ "Excedente de explotación bruto"
  ))


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("anio","categoria"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df = F
)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

etiquetas <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(descripcion) %>% 
  as.list()

names(etiquetas) <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(variable_nombre)

pks <- meta_desigu %>% 
  filter(dataset_archivo == output_name & primary_key == "TRUE") %>% 
  pull(variable_nombre)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_1, fuente_2, fuente_3),
    analista = "",
    pk = c("anio","categoria"),
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = "anio",
    aclaraciones = "El dataset posee algunas diferencias con respecto al realizado por el analista",
    etiquetas_indicadores = list("participacion" = "Participación en el Valor Agregado Bruto a precios básicos"),
    unidades = list("participacion" = "porcentaje")
  )


