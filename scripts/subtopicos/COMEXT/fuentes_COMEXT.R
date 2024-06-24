
# Exploración metadata COMEXT

matadatos <- metadata(subtopico = "COMEXT", fuentes = T)

# Descarga fuentes en local ----

argendataR::descargar_fuente(codigo = "R43C22") ## sector-externo-fundacion-norte-y-sur.xlsx R43C22 ----

argendataR::descargar_fuente(codigo = "R44C23") ## indices_comex_indec.xls R44C0 ----
 
argendataR::descargar_fuente(codigo = "R98C0") ## WDI_wdi_bm_comext.csv R98C0 (Compilacion de otras fuentes) 

argendataR::descargar_fuente(codigo = "R94C0") ## wto_composicion_exportaciones_servicios_EBOPS_2digitos_agrupado.csv R94C0 ----

argendataR::descargar_fuente(codigo = "R95C0") ## indec_estimacion_experimental_servicios_internacionales_CABPS.csv R95C0 ----



### ATLAS


argendataR::descargar_fuente("R102C0") # ATLAS - country_sitcproductsection_year


argendataR::descargar_fuente("R103C0") # ATLAS - 	sitc_product-dta.csv 


argendataR::descargar_fuente("R101C0") # ATLAS - country_partner_sitcproductsection_year

### LOCATIONS #####
argendataR::descargar_fuente("R154C0") # ATLAS - locations.dta


argendataR::descargar_fuente("R155C0") # ATLAS - locations.tab



################################  BASES AD HOC EN SERVER  ###################################


## BACI 


#BACI_HS07 <- arrow::open_dataset(glue::glue("{Sys.getenv('BACI_PATH')}/BACI_HS07/")


raw_file <- "datasets_BACI_raw.txt"

datasets_BACI_raw <- read_lines(glue::glue("{Sys.getenv('BACI_PATH')}/{raw_file}"))

      
      ### BACI CLEAN
      
      argendataR::descargar_fuente("R113C57") # BACI HS96 Comoposicion EXPO - IMPO Sectores Brambilla y Porto
      
      argendataR::descargar_fuente("R113C59") # BACI HS96 Comoposicion EXPO - IMPO  Diferenciados  BERININI


#### ISO 
      
      argendataR::descargar_fuente("R158C0") # Dataset construido a partir de tabulado de sitio oficial ISO https://www.iso.org/obp/ui/es/#home