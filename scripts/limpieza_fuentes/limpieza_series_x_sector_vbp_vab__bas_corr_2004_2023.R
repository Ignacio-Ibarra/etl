## traigo data
df <- readxl::read_excel(get_temp_path("R212C0"),
                         sheet = 5, skip = 3, n_max = 9)

#me quedo con columnas sin na
df_clean <- df %>%
  slice(-2) %>% 
  select(-which(colSums(is.na(df[1:2, ])) == 2))

## elimino indicadores que no van
df_clean <- df[-c(1,2,3,4,6,7,8,9), ]

## elimnino columnas con valores na vacios en excel
df_clean <- df_clean %>%
  select(where(~ !any(is.na(.))))

# armo rango de años 
start_year <- 2004
end_year <- as.numeric(format(Sys.Date(), "%Y"))
num_years <- end_year - start_year + 1

## 
column_names <- c("indicador")  
for (year in start_year:end_year) {
  column_names <- c(column_names,
                    paste(year, "T1", sep = "_"),
                    paste(year, "T2", sep = "_"),
                    paste(year, "T3", sep = "_"),
                    paste(year, "T4", sep = "_"),
                    paste(year, "total", sep = "_"))
  }

## le asigno nombres a las columnas
colnames(df_clean) <- column_names

## paso a long
df_long <- df_clean %>%
  pivot_longer(
    cols = -indicador,
    names_to = c("year", "quarter"),
    #names_pattern = "([0-9]{4})_(T[1-4]|Total)",
    names_pattern = "([0-9]{4})_(T[1-4]|total)",
    values_to = "value"
  ) %>% 
  mutate(value = as.numeric(value))

# guardo csv
write_csv_fundar(x = df_long,
                 file = glue::glue("{tempdir()}/vab_prec_bas_por_actividad_econ_2004_2023.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 212, 
                     dir = tempdir(),
                     path_clean = "vab_prec_bas_por_actividad_econ_2004_2023.csv",
                     nombre = "Valor Agregado Bruto a precios básicos por rama de actividad económica. Valores anuales en millones de pesos a precios corrientes. Años 2004-2023",
                     script = "limpieza_series_x_sector_vbp_vab__bas_corr_2004_2023.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 81)

