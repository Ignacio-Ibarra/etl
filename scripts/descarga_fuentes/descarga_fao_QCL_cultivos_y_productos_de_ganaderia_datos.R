#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(6)
fecha_ultima_actualizacion <- as.Date("2024-07-10")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/fao_scraper_bulk_download_links.R")


dominios <- FAO.list_domains() 

dominio <- dominios %>% 
  dplyr::filter(domain_code == "QCL") %>% 
  select(group_name, domain_code, domain_name, date_update)


group_name <- dominio$group_name

domain_name <- dominio$domain_name

domain_release <- dominio$date_update

dataset_code <- dominio$domain_code

nombre <- glue::glue("{group_name}. {domain_name}. Release {domain_release}")

dataset_metadata <- FAO.get_dataset_metadata("QCL") 

institucion <- dataset_metadata %>% 
  dplyr::filter(metadata_label %in% c("Contact organization", "Contact organization unit")) %>% 
  pull(metadata_text) %>% 
  paste0(., collapse = ". ")

bulkdownload_urls <- FAO.get_bulkdownload_urls(dataset_code)

url <- bulkdownload_urls %>% 
  dplyr::filter(Type == "All") %>% 
  pull(URL)

zip_filename <- 'Production_Crops_Livestock_E_All_Data.zip'

zip_destfile <- glue::glue("{tempdir()}/{zip_filename}")

download_filename <- "Production_Crops_Livestock_E_All_Data.csv"

FAO.descargar_archivo(url_descarga = url, zip_destfile = zip_destfile, file_to_download = download_filename)


# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar, 
#                    api = T)

actualizar_fuente_raw(id_fuente = 297,
                      url = url,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)


