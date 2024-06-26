
## descargo la fuente
## hay traer dos fuentes, una para mundo y otro para argentina
## la data hay que filtrarla para 2016 en ambas fuentes

## descargo fuente raw para mundo 
descargar_fuente_raw(id_fuente = 162, tempdir())

# traigo la data
nc_file_nivelmar<- ncdf4::nc_open(glue::glue("{tempdir()}/evol_nivel_mar_1993_2022.nc"))

# extraigo los objetos/variables de la lista que baja cuándo alevantamos archivo .nc
sla <- ncvar_get(nc_file_nivelmar, "sla")
sla_filtered <- ncvar_get(nc_file_nivelmar, "sla_filtered")
sla_tpacorr <- ncvar_get(nc_file_nivelmar, "sla_tpacorr")
sla_filtered_tpacorr <- ncvar_get(nc_file_nivelmar, "sla_filtered_tpacorr")
time <- ncvar_get(nc_file_nivelmar, "time")

# traigo información sobre la variable de tiempo para armar fecha que es un entero
time_var <- ncvar_get(nc_file_nivelmar, "time")
time_units <- ncatt_get(nc_file_nivelmar, "time", "units")

#  unidad de tiempo 
time_units <- ncatt_get(nc_file_nivelmar, "time", "units")$value

# Convierto los enteros de la variable "time" a fechas
fecha_origen <- as.Date("1950-01-01")
fechas <- fecha_origen + time

#   data frame con los datos y variables que nocesitamos
df_sea_level <- data.frame(
  fecha = fechas,
  altura_nivel_mar_corr_tpac_drift = sla_tpacorr,
  altura_nivel_mar_filtrada_corr_tpac_drift = sla_filtered_tpacorr
)

# guardo csv
write_csv_fundar(x = df_sea_level,
                 file = glue::glue("{tempdir()}/evolucion_nivel_del_mar_1993_2022.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 162, 
                     dir = tempdir(),
                     path_clean = "evolucion_nivel_del_mar_1993_2022.csv",
                     nombre = "Anomalias del nivel del mar",
                     script = "limpieza_nivel_mar_1993_2022.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 72)



