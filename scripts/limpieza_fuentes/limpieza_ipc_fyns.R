descargar_fuente(codigo = "R120C0")

archivo <- "ipc_1810_2018_fyns.csv"

ipc_fyns <- readxl::read_excel(get_temp_path("R120C0"), sheet = "IPC ", skip = 1)


ipc_fyns <- ipc_fyns[1:226,]

ipc_fyns <- ipc_fyns[,-4]

ipc_fyns <- ipc_fyns %>% 
  janitor::clean_names()

ipc_fyns <- ipc_fyns %>% 
  rename(anio = ano,
         var_percent_promedio_anual = var_percent_anual_3 ,
         var_percent_promedio_anual_dic =var_percent_anual_6,
          indice_promedio_anual_base_2015 = promedio_anual_diciembre_2015_100,
          indice_diciembre_base_diciembre_2015 = a_diciembre_diciembre_2015_100
          )

ipc_fyns <- ipc_fyns %>% 
  filter(!if_all(-anio, is.na))


ipc_fyns <- ipc_fyns %>% 
  mutate(across(everything(), as.numeric))

write_csv_fundar(ipc_fyns, file = glue::glue("{tempdir()}/{archivo}"))

agregar_fuente_clean(id_fuente_raw = 120, path_clean = archivo,
                     nombre = "Indice y variacion de precios 1809 a 2018",
                     script = "limpieza_ipc_fyns.R",
                     descripcion = "La tabla presenta dos indices: uno calculado como promedio anual del nivel de precios y otro como nivel de precios a diciembre de cada año. En ambos casos la base es el año 2015.")

actualizar_fuente_clean(id_fuente_clean = 49)