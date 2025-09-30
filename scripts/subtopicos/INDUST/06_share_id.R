library(rsdmx)
library(tidyverse)

# URL de tu consulta SDMX
url_sdmx <- "https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_RDS_BERD@DF_BERD_INDU,1.0/AUT+BEL+CAN+CHL+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+ARG+BGR+CHN+HRV+ROU+SGP+TWN+AUS.A.......MA.USD_PPP.V?startPeriod=2006&dimensionAtObservation=AllDimensions"

# JSON Argendata 
dicc_argendata <- jsonlite::fromJSON('https://raw.githubusercontent.com/argendatafundar/geonomencladores/refs/heads/main/geonomenclador.json')
dicc_argendata <- dicc_argendata %>% 
  select(geocodigo,name_long)

# rutas 
outstub <- 'indust/input'

# Descargar los datos
datos_berd <- readSDMX(url_sdmx)

# Convertir a data frame
df_berd <- as.data.frame(datos_berd)
df_berd <- as_tibble(df_berd)

# Seleccionar sectores
df_berd <- df_berd %>% 
  filter(ACTIVITY %in% c('C','_T'))

# Seleccionar columnas 
df_berd <-df_berd %>% 
  select(TIME_PERIOD,REF_AREA,ACTIVITY,obsValue) %>% 
  pivot_wider(names_from=ACTIVITY,values_from=obsValue)

# Arreglar variables y nombres
df_berd <- janitor::clean_names(df_berd) 
df_berd <- df_berd %>% 
  mutate(time_period = as.numeric(time_period))
df_berd <- df_berd %>% 
  rename(iso3 = ref_area)

# Calcular mundo 
df_berd_aux <- df_berd %>% 
  group_by(time_period) %>% 
  summarize(c = sum(c,na.rm=T),
            t = sum(t,na.rm=T))
df_berd_aux <- df_berd_aux %>% 
  mutate(share_indust_id = c / t)
# Calcular share indust 
df_berd <- df_berd %>% 
  mutate(share_indust_id = c / t)
df_berd <- df_berd %>% 
  filter(!is.na(share_indust_id))

# Seleccionar columnas 
df_berd <- df_berd %>% 
  select(anio = time_period,iso3,share_indust_id)
df_berd_aux <- df_berd_aux %>% 
  mutate(iso3 = 'WLD') %>% 
  select(anio=time_period,iso3,share_indust_id)
df_berd <- bind_rows(df_berd,df_berd_aux)

# Agregar nombre de pais 
df_berd <- df_berd %>% 
  left_join(dicc_argendata,by=c('iso3'='geocodigo'))

# Guardar resultado 
write_csv(df_berd,file.path(outstub,'06_share_id.R'))
