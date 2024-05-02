library(sjlabelled)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


indicator_code <- 'SL.TLF.CACT.FM.NE.ZS'

# Descargo data usando wrapper https://github.com/vincentarelbundock/WDI
data <- WDI(indicator=indicator_code, country = 'all')

# Me quedo con el nombre del indicador 
indicator_label <- get_label(data$SL.TLF.CACT.FM.NE.ZS)

download_filename <- "WDI_ratio_female_to_male_force_participation_rate.csv"

data %>% write_csv_fundar(sprintf("data/_FUENTES/raw/%s",download_filename))

# agregar_fuente_raw(url = "https://data.worldbank.org/indicator/SL.TLF.CACT.FM.NE.ZS",
#                    nombre = indicator_label, 
#                    institucion = "Banco Mundial",
#                    actualizable = T, 
#                    dir = "data/_FUENTES/raw/",
#                    path_raw = download_filename,
#                    script = code_name, 
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 42, actualizable = T, dir = "data/_FUENTES/raw/")


