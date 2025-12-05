## descargo la fuente

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 161
fuente_raw <- sprintf("R%sC0",id_fuente)

## descargo la fuente

## hay traer dos fuentes, una para mundo y otro para argentina
## la data hay que filtrarla para 2016 en ambas fuentes

## descargo fuente raw para mundo 
#descargar_fuente_raw(id_fuente = 161, tempdir())

df <- readr::read_table(
  file = get_raw_path(fuente_raw),
  comment = "#",
  col_names = c(
    "year", "month", "decimal_date",
    "co2", "co2_deseason", "ndays",
    "stdev_days", "uncertainty"
  ),
  col_types = cols(
    year          = col_integer(),
    month         = col_integer(),
    decimal_date  = col_double(),
    co2           = col_double(),
    co2_deseason  = col_double(),
    ndays         = col_integer(),
    stdev_days    = col_double(),
    uncertainty   = col_double()
  ),
  na = c("-99.99", "-999.99")
) %>%
  mutate(
    ndays       = if_else(ndays < 0L, NA_integer_, ndays),
    stdev_days  = if_else(stdev_days < 0, NA_real_, stdev_days),
    uncertainty = if_else(uncertainty < 0, NA_real_, uncertainty),
    date        = make_date(year, month, 15)
  ) %>%
  relocate(date, .after = month)

# guardo csv
# write_csv_fundar(x = co2_1958_2024_df,
#                  file = glue::glue("{tempdir()}/co2_2958_2024_noaa.csv"))
# 
# # agrego fuente clean
# agregar_fuente_clean(id_fuente_raw = 161, 
#                      dir = tempdir(),
#                      path_clean = "co2_2958_2024_noaa.csv",
#                      nombre = "Concentración co2 1958-2024",
#                      script = "limpieza_co2_1958_2024_NOAA.R")

# lista_comparacion <- comparar_fuente_clean(df = co2_1958_2024_df, id = 71, pk = c("anio", "mes"))

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 71, script = code_name,
                        comparacion = list("Cambio formato de salida", "No comparable", "Incluyo todos los meses"),
                        df = df)



