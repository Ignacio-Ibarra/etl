# esta fuente no se scrapear



df <- read_csv("feutnes_loca.csv")

df %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/feutnes_loca.csv")))


agregar_fuente_raw(
  url = "Sin url",
  nombre = "Greenhouse gas emissions by gas, World, 1850 to 2022",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "emisiones_gases_invernadero.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(82,
                      directorio = tempdir())



