#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 469
fuente_raw <- sprintf("R%sC0",id_fuente)

exdir <- tempdir()

zip_file <- argendataR::get_raw_path(fuente_raw)

filepath <- zip_file %>% 
  unzip(.,list = T) %>% 
  dplyr::filter(Length > 0) %>% 
  dplyr::filter(grepl("UN_Tourism_inbound_expenditure", Name)) %>% 
  pull(Name) 

filename <- basename(filepath)


unzip(zip_file, files = filepath, junkpaths = T, exdir = exdir)


extracted_path <- glue::glue("{tempdir()}/{filename}")


sheet_names <- extracted_path %>% 
  readxl::excel_sheets()



df_clean <- extracted_path %>% 
  readxl::read_excel(., 
                     sheet = sheet_names[sheet_names!='Overview']) %>% 
  janitor::clean_names() 



indicator <- "Inbound Expenditure"


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

normalized_indicator <- indicator %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_indicator}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - {indicator}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)


id_fuente_clean <- 309
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('reporter_area_code', 'partner_area_code', 'year', 'indicator_code')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)