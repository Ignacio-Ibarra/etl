# limpieza a formato tidy de la maddison db


descargar_fuente_raw(id_fuente = 37,
                      dir = "data/_FUENTES/raw/")


# carga

data <- readxl::read_excel("data/_FUENTES/raw/mpd2020.xlsx",
                           sheet = "Full data")

# rename columns

data <- data %>% 
  rename(iso3 = countrycode, anio  = year)

head(data)

data <-  data %>% 
  pivot_longer(cols = c(gdppc, pop), names_to = "indicador",
               values_to = "valor") 


data <- data %>% 
  filter(!is.na(valor))


ultimo_dato <- data %>% pull(anio) %>% last()
# guardar
write_csv_fundar(data, file = "data/_FUENTES/clean/mpd2020.csv")  

agregar_fuente_clean(id_fuente_raw = 37, path_clean = "mpd2020.csv",
                     nombre = "Maddison Project Database 2020",
                     script = "limpieza_maddison.R")

actualizar_fuente_clean(id_fuente_clean = 1, fecha = Sys.Date())
