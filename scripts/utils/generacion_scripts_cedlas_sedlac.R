#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

output_folder <- "./scripts/limpieza_fuentes"

INPUT_RAW_ID <- 116
TOPIC_PARAM <- "Years of Education"
fuente_raw <- sprintf("R%sC0",INPUT_RAW_ID)

descargar_fuente_raw(id_fuente = INPUT_RAW_ID, tempdir())

sheet_names <- tidyxl::xlsx_sheet_names(argendataR::get_temp_path(fuente_raw))
sheet_names <- sheet_names[sheet_names != 'index']

lineas_base <- readLines("./scripts/utils/template_limpieza_cedlas_sedlac.txt")

lineas_string <- paste(lineas_base, collapse = "\n")


for (SHEET_PARAM in sheet_names){


topic <- str_to_lower(TOPIC_PARAM) %>% str_replace(., " ", "_")
sheet <- str_to_lower(SHEET_PARAM) %>% str_replace(., " ", "_")
  
  
output_script_str <- glue::glue(lineas_string, 
                                INPUT_RAW_ID = INPUT_RAW_ID, 
                                TOPIC_PARAM = TOPIC_PARAM,
                                SHEET_PARAM = SHEET_PARAM)

stringi::stri_write_lines(output_script_str, con = glue::glue("./scripts/limpieza_fuentes/limpieza_cedlas_sedlac_{topic}_{sheet}.R"))
}
