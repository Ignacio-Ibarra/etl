# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

url <- "https://tulane.app.box.com/s/4lt0l5p7demsxvf7h76ngc0u0pgmrcm8/file/837284586342"

nombre <- glue::glue("CEQ Master Workbook. Argentina. Fiscal interventions, (versión 19 de Marzo de 2018)")

institucion <- "CEQ Data Center on Fiscal Redistribution. Commitment to Equity (CEQ) Institute. Tulane University"

download_filename <- "ARG17CQN_PDI_E11_2011PPP_Jan27_2021.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

 
# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 431,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)