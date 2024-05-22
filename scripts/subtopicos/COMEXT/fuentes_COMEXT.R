
# Exploración metadata COMEXT

matadatos <- metadata(subtopico = "COMEXT", fuentes = T)

# Descarga fuentes en local ----

argendataR::descargar_fuente(codigo = "R43C22") ## sector-externo-fundacion-norte-y-sur.xlsx R43C22 ----

argendataR::descargar_fuente(codigo = "R44C23") ## indices_comex_indec.xls R44C0 ----
 
## WDI Indicators - COMEXT ----

argendataR::descargar_fuente(codigo = "R98C0") ## WDI_wdi_bm_comext.csv R98C0 (Compilacion de otras fuentes) 


## BACI ----



argendataR::descargar_fuente(codigo = "R94C0") ## wto_composicion_exportaciones_servicios_EBOPS_2digitos_agrupado.csv R94C0 ----

argendataR::descargar_fuente(codigo = "R95C0") ## indec_estimacion_experimental_servicios_internacionales_CABPS.csv R95C0 ----


################################  BASES AD HOC EN SERVER  ###################################

## Harvard Atlas Economic Complexity (from server) ----

read_csv("/srv/server_data/argendata/atlas_economic_complexity/country_partner_sitcproductsection_year.csv") %>% 
  write_csv_fundar(file = glue::glue("{tempdir()}/country_partner_sitcproductsection_year.csv")) # country_partner_sitcproductsection_year.csv

read_csv("/srv/server_data/argendata/atlas_economic_complexity/country_sitcproductsection_year.csv") %>% 
  write_csv_fundar(file = glue::glue("{tempdir()}/country_sitcproductsection_year.csv")) # country_sitcproductsection_year.csv

read_csv("/srv/server_data/argendata/atlas_economic_complexity/sitc_product-dta.csv") %>% 
  write_csv_fundar(file = glue::glue("{tempdir()}/sitc_product-dta.csv")) # sitc_product-dta.csv


# LOCATION # 

read_csv("/srv/server_data/argendata/atlas_economic_complexity/location.csv") %>%  # Origin .tab (1)
  write_csv_fundar(file = glue::glue("{tempdir()}/location.csv"))

read_csv("/srv/server_data/argendata/atlas_economic_complexity/location-dta.csv") %>% 
  dplyr::filter(level == "region") %>% 
  write_csv_fundar(file = glue::glue("{tempdir()}/location-dta_region.csv")) # File para corregir base location.csv (1)

