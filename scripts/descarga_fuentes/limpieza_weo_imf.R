
# weo imf -----------------------------------------------------------------

# carga
path <- "data/_INSUMOS/raw/WEOOct2023all.xls"
data <- read_tsv(path)

colnames(data)

# pivot longer
data <- data %>% 
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio", values_to = "valor")

# limpiar col names
data <- data %>% 
  janitor::clean_names()

# rename cols en base a estandares fundar

data <- data %>% 
  rename(iso3 =  iso)

# limpiar columnas

# convertir a NA caracters que marcan dato faltante o dato que no es computable
data$valor[data$valor %in% c("n/a", "--")] <- NA

# chequear no digitos en valor
unique(str_extract(data$valor, "\\D"))
# contar NAs (no deberian aumentar dps de parsear)
sum(is.na(data$valor))
# pasar a numeric
data$valor <- as.numeric(gsub(",","",data$valor))
#controlar na de nuevo
sum(is.na(data$valor))

head(data)

# guardar
write_csv(data, file = "data/_INSUMOS/raw/weo_imf.csv",
          na = "", eol = "\n")

# file.remove("data/_INSUMOS/raw/WEOOct2023all.xls")
