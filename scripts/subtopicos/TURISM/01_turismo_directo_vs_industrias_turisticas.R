# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "turismo_directo_vs_industrias_turisticas.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R470C308'


df_yvera <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_output <- df_yvera %>% 
  dplyr::filter(unidad_medida == "en % sobre el total", 
                grepl("\\(VABIT\\)|(VABDT)", indicador),
                anio>= 2016)


