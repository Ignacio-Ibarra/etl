
## traigo data
df <- readxl::read_excel(get_temp_path("R208C0"),
                           sheet = 9, skip = 3, n_max = 3)

#me quedo con columnas sin na
df_clean <- df %>%
  slice(-2) %>% 
  select(-which(colSums(is.na(df[1:2, ])) == 2))

# armo rango de años 
start_year <- 2004
end_year <- as.numeric(format(Sys.Date(), "%Y"))
num_years <- end_year - start_year + 1

## empiezo la lista de nombres de columnas vacía
column_names <- c()

# armo nombres de columnas para cada año
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

## armo variable indicador 
df_clean <- df_clean %>%
  mutate(indicador = "PBI") %>%
  select(indicador, everything()) 

## elimino primera fila 
df_clean <- df_clean[-1, ]

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
                 file = glue::glue("{tempdir()}/ofe_dem_glob_trimestrales_prec_corr_pbi_20023.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 208, 
                     dir = tempdir(),
                     path_clean = "ofe_dem_glob_trimestrales_prec_corr_pbi_20023.csv",
                     nombre = "Oferta y demanda globales. Valores trimestrales en millones de pesos a precios corrientes",
                     script = "limpieza_of_dem_glob_trimest_precios_corr_pbi.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 80)
