# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("scripts/utils/wpp_downloads.R")

mgroup <- "CSV format"
sgroup <- "Life Tables"
idescription <- "Single age life tables up to age 100 - Both Sexes. Only medium is available."
ftitle <- "1950-2023"



search <- WPP_get_data_links() %>% 
  dplyr::filter(MajorGroup == mgroup, 
                SubGroup == sgroup, 
                Item_Description == idescription,
                File_Title == ftitle)


nombre <- glue::glue("World Population Proscpects - {sgroup}. {ftitle}. {idescription}. {mgroup}")

url <- search$download_url

institucion <- "United Nations, Department of Economic and Social Affairs, Population Division "

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 441,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
