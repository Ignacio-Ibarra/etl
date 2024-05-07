url <- "https://cdsrenovables.cammesa.com/exhisto/RenovablesService/getCapacidadesRegiones"

cammesa_data <- jsonlite::fromJSON(url)

cammesa_data <- cammesa_data %>% 
  select(-idRegion) %>% 
  pivot_longer(cols = -nemoRegion) %>% 
  mutate(name = case_when(
    name == "hidraulica" ~ "hidraulica_menorigual_50",
    name == "hidraulica50" ~ "hidraulica_mayor_50",
    T ~ name
  )) %>% 
  mutate(unidad = "MW", anio = 2022)

cammesa_data <- cammesa_data %>% 
  select(anio, nemoRegion, name, unidad, value)

cammesa_data %>% 
  write_csv_fundar(normalizePath(sprintf("%s/potencia_instalada_renovables_arg_2022.csv", tempdir())))


agregar_fuente_raw(
  url = url,
  nombre = "Potencia instalada por region 2022, Argentina",
  institucion =  "CAMMESA",
  actualizable = F,
  path_raw = "potencia_instalada_renovables_arg_2022.csv",
  directorio = tempdir(), script = "cammesa_potencia_instalada.R",api = F
)

actualizar_fuente_raw(81)