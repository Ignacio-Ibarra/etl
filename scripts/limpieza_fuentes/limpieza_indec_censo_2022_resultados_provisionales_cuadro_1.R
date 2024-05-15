# Codigo de limpieza de datos de Censo 2022 - Resultados Provisionales

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 99
fuente_raw1 <- sprintf("R%sC0",id_fuente)

descargar_fuente_raw(id_fuente = id_fuente, tempdir())

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    dplyr::filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

## funciones
norm_string <- function(string){
  # print(string)
  string <- str_to_lower(string)
  # print(string)
  string <- iconv(string, to = "ASCII//TRANSLIT")
  string <- str_split_1(string, " ")
  # print(string)
  string <- gsub("[^[:alpha:]]", "", string)
  # print(string)
  string <- paste0(string, collapse = "_")
  # print(string)
  return(string)
  
}



sheet <- "Cuadro 1"
cell_range <- "A3:G30"


censo_clean <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), 
                              sheet = sheet, 
                              range = cell_range, 
                              col_names = T,
                              col_types = "text", 
                              na = c("", "///", "-")) %>% 
  dplyr::filter(`Jurisdicción` != "Total" )


names(censo_clean) <- censo_clean %>% colnames(.) %>% purrr::map(., norm_string) %>% unlist()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

norm_sheet <- str_to_lower(sheet) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.csv")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

censo_clean %>% write_csv_fundar(., file = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = sprintf("Censo 2022 - Resultados Provisionales (%s)",sheet),
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 25,
                        dir = tempdir())
