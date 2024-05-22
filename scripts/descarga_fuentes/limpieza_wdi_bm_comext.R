# WDI Indicators - Individuales (hacer join) ############################################################

argendataR::descargar_fuente(codigo = "R85C0") # WDI_goods_exports_bop_current_us.csv	R85C0
argendataR::descargar_fuente(codigo = "R86C0") # WDI_service_exports_bop_current_us.csv	R86C0
argendataR::descargar_fuente(codigo = "R87C0") # WDI_exports_of_goods_and_services_constant_us.csv	R87C0
argendataR::descargar_fuente(codigo = "R88C0") # WDI_exports_of_goods_and_services_of_gdp.csv	R88C0
argendataR::descargar_fuente(codigo = "R89C0") # WDI_export_unit_value_index.csv	R89C0
argendataR::descargar_fuente(codigo = "R90C0") # WDI_export_volume_index.csv	R90C0
argendataR::descargar_fuente(codigo = "R91C0") # WDI_trade_of_gdp.csv	R91C0

 WDI_goods_exports_bop_current_us.csv <- read_csv(get_temp_path('R85C0')) %>% 
   rename(goods_exports_bop_current_us = 5) 
 
 WDI_service_exports_bop_current_us.csv <- read_csv(get_temp_path('R86C0')) %>% 
   rename(service_exports_bop_current_us = 5) 
 
 WDI_exports_of_goods_and_services_constant_us.csv <- read_csv(get_temp_path('R87C0')) %>% 
   rename(exports_of_goods_and_services_constant_us = 5) 
 
 WDI_exports_of_goods_and_services_of_gdp.csv <- read_csv(get_temp_path('R88C0')) %>% 
   rename(exports_of_goods_and_services_of_gdp = 5) 
 
 WDI_export_unit_value_index.csv <- read_csv(get_temp_path('R89C0')) %>% 
   rename(export_unit_value_index = 5) 
 
 WDI_export_volume_index.csv <- read_csv(get_temp_path('R90C0')) %>% 
   rename(export_volume_index = 5) 
 
 WDI_trade_of_gdp.csv <- read_csv(get_temp_path('R91C0')) %>% 
   rename(trade_of_gdp = 5) 
 

 
 
 wdi_comext <- WDI_goods_exports_bop_current_us.csv %>% left_join(
   WDI_service_exports_bop_current_us.csv) %>% left_join(
   WDI_exports_of_goods_and_services_constant_us.csv) %>% left_join(
   WDI_exports_of_goods_and_services_of_gdp.csv) %>% left_join(
   WDI_export_unit_value_index.csv) %>% left_join(
   WDI_export_volume_index.csv) %>% left_join(
   WDI_trade_of_gdp.csv )
 
 
 # Preparar variables
 WDI <- mutate(wdi_comext,
               goodsexportsbop = goods_exports_bop_current_us  / 1000000,
               serviceexportsbop = service_exports_bop_current_us / 1000000,
               servicesexportsbop_pc_v2 = serviceexportsbop / (goodsexportsbop + serviceexportsbop) * 100,
               exportsconstant = exports_of_goods_and_services_constant_us / 1000000,
               exportsconstant_goods_v2 = exportsconstant * (1 - servicesexportsbop_pc_v2 / 100),
               exportsconstant_servi_v2 = exportsconstant * servicesexportsbop_pc_v2 / 100,
               exportsofgoodsandservicesofgdp = exports_of_goods_and_services_of_gdp,
               exportunitvalueindex = export_unit_value_index,
               tradeofgdp = trade_of_gdp
 ) %>% 
   mutate(baseyear2000 = if_else(year == 2000, 1, 0)) %>%
   group_by(iso3c) %>%
   arrange(iso3c, baseyear2000) %>%
   mutate(exportunitvalueindex_2000 = if_else(baseyear2000 == 1, 100, 100 * exportunitvalueindex / first(exportunitvalueindex[baseyear2000 == 1]))) 
   
 
 # Renombrar variables
 data <- rename(WDI, 
               countryname = country,
               iso3 = iso3c,
               Exportaciones_bienes_servicios_PIB = exportsofgoodsandservicesofgdp,
               Indice_valores_unitarios_exportacion_bienes_2000 = exportunitvalueindex_2000) %>% 
   # Fixes de strings
   mutate(countryname = str_replace_all(countryname, "&", 'and')) %>% 
   # Inputacion de geocodigo del nomenclador (NAs desde la fuente)
   mutate(iso3 = case_when(
     countryname == 'High income' ~ 'HIC',
     countryname == 'Low income' ~ 'LIC',
     countryname == 'Lower middle income' ~ 'LMC',
     countryname == 'Upper middle income' ~ 'UMC', TRUE ~ iso3
   )) 
 
  data <- data %>% dplyr::filter(!str_detect(countryname, "Not classified")) # descarto dato sin clasificar
 


 
 # guardar como csv
 write_csv_fundar(data,
                  file = glue::glue("{tempdir()}/wdi_bm_comext.csv"))  
 

 # Agregar como fuente RAW  (compilacion de fuentes RAW multiples)
 
# agregar_fuente_raw(url = "https://data.worldbank.org/indicator/", 
#                    nombre = "Compilación indicadores WDI (COMEXT)",
#                    institucion =  "Banco Mundial", actualizable = T, fecha_descarga = Sys.Date(), 
#                    path_raw =  "wdi_bm_comext.csv", api = F, script = "limpieza_wdi_bm_comext.R")
# 
 
 
 actualizar_fuente_raw(id_fuente = "R98C0")
 
 
 
 