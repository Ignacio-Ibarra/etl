# limpieza a formato tidy de la maddison db

path <- "data/_FUENTES/raw/mpd2020.xlsx"

readxl::excel_sheets(path)

# carga

data <- readxl::read_excel(path,
                           sheet = "Full data")

# rename columns

data <- data %>% 
  rename(iso3 = countrycode, anio  = year)

# guardar
write_csv(data, file = "data/_INSUMOS/raw/maddison_db.csv",
          na = "", eol = "\n")  

