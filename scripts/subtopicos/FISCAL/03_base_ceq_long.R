# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "base_ceq_long"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R425C273'

df_ceq <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_output <- df_ceq %>% 
  dplyr::filter(name %in% c("Market income plus pensions", "Final income"), 
                concept == "PDI") %>% 
  select(iso3, pais_nombre, 
         anio = start_year, 
         ingreso = name, 
         gini = indicator_value) %>% 
  drop_na(gini) %>% 
  rename(pais = pais_nombre) %>% 
  mutate(ingreso  = ifelse(ingreso == 'Final income', "Ingreso final", "Ingresos de mercado más pensiones"),
         gini = gini/100)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T)   %>%
  mutate(ingreso = ifelse(ingreso == 'mercado', "Ingresos de mercado más pensiones", "Ingreso final"))


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("pais", "anio", "ingreso")
)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = c("iso3", "anio", "ingreso"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("gini" = "Índice de Gini"),
    unidades = list("gini" = "indice")
  )

