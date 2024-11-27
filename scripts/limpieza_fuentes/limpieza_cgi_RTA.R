serie_cgi <- serie_cgi[,!sapply(serie_cgi, function(x) {sum(is.na(x)) == length(x)})]

# pivoteo a la long estricto, agrego col unidades y paso valores de millones a unidades
df_clean <- serie_cgi %>% 
  pivot_longer(cols = -c(anio, trim),
               names_to = "indicador", values_to = "valor_agregado_bruto", values_transform = as.numeric)

norm_sheet <- str_to_lower(SHEET_NAME) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = glue::glue("Cuenta Generacion del Ingreso - Cuadro: {SHEET_NAME} - INDEC"),
#                      descripcion = "La limpieza consiste en llevar los datos de formato en Excel a formato tabular plano listo para poder consumir",
#                      script = code_name)

head(df_clean)

control <- comparar_fuente_clean(df_clean, id  = 83, pk = c("anio", "trim","indicador"))


actualizar_fuente_clean(id_fuente_clean = 83,
                        df =df_clean, comparacion = control)
