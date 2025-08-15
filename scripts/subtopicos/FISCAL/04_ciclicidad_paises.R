# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "ciclicidad_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R426C274'

df_wb <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_output <- df_wb %>% 
  select(codigo_pais = iso3, pais = pais_nombre, tipo_pais = desarrollo_economia, correlacion_ciclicidad_fiscal = corr_gasto_ciclico_gdp) %>% 
  mutate(correlacion_ciclicidad_fiscal = as.numeric(correlacion_ciclicidad_fiscal))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T)   


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("codigo_pais")
)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = c("codigo_pais"),
    columna_geo_referencia = "codigo_pais",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("correlacion_ciclicidad_fiscal" = "Correlación entre el componente cíclico del gasto público real y el PBI real (2000-2022)"),
    unidades = list("correlacion_ciclicidad_fiscal" = "unidades")
  )
