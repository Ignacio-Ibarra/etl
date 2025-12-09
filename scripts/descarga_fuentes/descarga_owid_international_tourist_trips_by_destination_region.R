# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


url <- "https://ourworldindata.org/grapher/international-tourist-trips-by-destination-region"

df_raw <- argendataR::owid_scraper(url)

nombre_archivo <- "International tourist trips by destination region. Trips by those who stay overnight and whose main purpose for visiting is not commercial."


institucion <- "United Nations World Tourism Organization - World Tourism Barometer (2019) – processed by Our World in Data"

download_filename <- basename(url) %>% paste0(., ".csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% 
  argendataR::write_csv_fundar(., destfile)

 
# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre_archivo,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 491,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)