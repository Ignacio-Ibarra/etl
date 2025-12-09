# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(googlesheets4)


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

url <- "https://app.powerbi.com/view?r=eyJrIjoiYWUxMzc5NWItZjA3Ny00YmM1LTkzODktMDdiMzUzNjczZmYzIiwidCI6IjRiMWJkNWRiLTY3ODItNDY2YS1hMWM1LTRlOTc1NjQ4ZjhlNSIsImMiOjl9"

url_sheet <- "https://docs.google.com/spreadsheets/d/1xPr8cAjoCsWrf6lnRUmMoHEQYT_lLNA7ZXGfLBoiIns/edit?gid=1969439957#gid=1969439957"


nombre_archivo <- "UN Tourism Data Dashboard - Int. Tourism Arraivals Million (Extracción Manual 2022-2024)"

institucion <- "World Tourism Organization"

nombre_archivo_normalized <- nombre_archivo %>% janitor::make_clean_names()

df_raw <- googlesheets4::read_sheet(url_sheet)

download_filename <- glue::glue("{nombre_archivo_normalized}.csv")

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

actualizar_fuente_raw(id_fuente = 476,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
