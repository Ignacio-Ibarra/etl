descargar_fuente(codigo = "R117C0")

ponderaciones <- readxl::read_xls(get_temp_path("R117C0"),
                                  sheet = "Ponderaciones")


ponderaciones <- ponderaciones %>% 
  dplyr::filter(!is.na(`Índice de Precios al Consumidor con cobertura nacional. Resultados por región`)) %>% 
  .[2:15,] 

colnames(ponderaciones) <- ponderaciones[1,]

ponderaciones <- ponderaciones[-1,]


ponderaciones <- ponderaciones %>% 
  pivot_longer(cols = -1, names_to = "region", values_to = "valor") 

pesos_regiones <- ponderaciones %>% 
  dplyr::filter(str_detect(Divisiones, "Importancia relativa de la reg"))


ponderaciones <- ponderaciones %>% 
  dplyr::filter(!str_detect(Divisiones, "Importancia relativa de la reg"))

ponderaciones <- left_join(ponderaciones, pesos_regiones, by = "region")

ponderaciones <- ponderaciones %>% 
  select(region, peso_region = valor.y, division = Divisiones.x, peso_division = valor.x)

ponderaciones <- ponderaciones %>% 
  mutate(peso_region = as.numeric(peso_region), 
         peso_division = as.numeric(peso_division))

ponderaciones_nac <- ponderaciones %>% 
  group_by(division) %>% 
  summarise(peso_division = sum(peso_region*peso_division)) %>% 
  ungroup() %>% 
  mutate(region = "Nacional", peso_region = 1)

ponderaciones <- ponderaciones %>% 
  bind_rows(ponderaciones_nac)

archivo <- "ponderaciones_ipc.csv"

write_csv_fundar(x = ponderaciones, file = glue::glue("{tempdir()}/{archivo}"))

agregar_fuente_clean(id_fuente_raw = 117,
                     path_clean = archivo, 
                     nombre = "Ponderadores IPC - 2016", script = "ponderadores_ipc_indec.R",
                     descripcion = "Ponderadores de divisiones del IPC por región y para total nacional. Año 2016", directorio = tempdir())
