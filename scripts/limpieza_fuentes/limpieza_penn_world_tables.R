
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 92
fuente1 <- sprintf("R%sC0",id_fuente)

descargar_fuente_raw(id_fuente = id_fuente, tempdir())

# Como limpieza solo me quedo con la sheet correspondiente y lo guardo como csv
pwt_raw <- readxl::read_excel(argendataR::get_temp_path(fuente1), sheet="Data")


path_clean <- glue::glue("{tempdir()}/penn_world_table1001_CLEAN.csv")

pwt_raw %>% write_csv_fundar(., file = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = "penn_world_table1001_CLEAN.csv",
#                      dir = tempdir(),
#                      nombre = "Penn World Tables - 10.01",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = "R92C15", dir = tempdir())

