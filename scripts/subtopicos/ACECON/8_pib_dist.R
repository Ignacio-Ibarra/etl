# 8_pib_dist
# descripcion

# vars config del script
output_name <- "8_pib_dist"
# periodo, etc.,

# Descargas -------

# Valor agregado bruto e insumo de mano de obra por sector de actividad económica. Serie CGI indec
download.file("https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_01_24.xls",
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/{subtopico}/datasets/raw/serie_cgi.xls"))

# Lectura -------

# rta como % pbi - cgi indec
serie_cgi <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/serie_cgi.xls"), sheet = "RTA pp")

# Remuneración al trabajo asalariado como porcentaje del PIB desde 1935 hasta 2020.
ceped <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/ceped.xlsx"))

# Procesamiento -------

# la participación de la remuneración al trabajo asalariado en 2020, 2021 y 2022.
serie_cgi <- tidy_indec(x = serie_cgi, tabla = "serie_cgi")

# seleccion de anios de interes para la expansion de serie ceped
serie_cgi <- serie_cgi %>% 
  filter(trim == "Total" & anio %in% 2020:2022) %>% 
  select(anio, total_general)

# calculo variaciones ia
serie_cgi <- serie_cgi %>% 
  mutate(across(everything(), as.numeric)) %>% 
  arrange(anio) %>% 
  mutate(var = total_general/lag(total_general))

# seleccion de anios con variaciones
serie_cgi <- serie_cgi %>% 
  filter(anio > 2020)

# renombro variables segun output
colnames(ceped) <- c("anio", "remuneracion_al_trabajo_asalariado")

# a numerico
ceped <- ceped %>% 
  mutate(across(everything(), as.numeric))

# elimino filas de encabezado
ceped <- ceped %>% 
  filter(!is.na(anio))

# uno las series
ceped <- ceped %>% 
  bind_rows(serie_cgi)

# expando ceped x vars indec cgi
ceped <- ceped %>% 
  mutate(remuneracion_al_trabajo_asalariado = expansor_xvar(remuneracion_al_trabajo_asalariado, var))

# seleccion de columnas finales y calculo del complemento del RTA como % pbi
df_output <- ceped %>% 
  select(-c(total_general, var)) %>% 
  mutate(otros = 100 - remuneracion_al_trabajo_asalariado)

# Control vs output previo -------

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
  mutate(across(-c(), as.numeric))

vs <- out_prev %>% 
  left_join(df_output, by = c("anio"))

diff <-  comparar_cols(vs)

diff <- diff %>% 
  filter(if_any(-anio, \(x) round(x,2) != 0))


diff %>% 
  write_argendata(file_name = glue::glue("_diff_{output_name}.csv"),
  subtopico =  subtopico)

# Write output ------


df_output %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
  subtopico = subtopico)
