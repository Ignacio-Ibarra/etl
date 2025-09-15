# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

txt <- "periodo,crec_total_miles,vegetativo_miles,vegetativo_pct,migratorio_miles,migratorio_pct
1870-1875,267.9,168.0,62.7,99.9,37.3
1875-1880,258.8,215.8,83.4,43.0,16.6
1880-1885,392.9,243.1,61.9,149.8,38.1
1885-1890,856.4,253.9,29.6,602.5,70.4
1890-1895,454.3,297.8,65.6,156.6,34.4
1895-1900,643.3,342.0,53.2,301.3,46.8
1900-1905,703.8,459.3,65.3,244.5,34.7
1905-1910,1373.6,588.2,42.8,785.4,57.2
1910-1915,1465.6,728.6,49.7,737.0,50.3
1915-1920,736.7,805.7,109.4,-69.0,-9.4
1920-1925,1452.4,950.3,65.4,502.1,34.6
1925-1930,1510.9,1029.0,68.1,481.9,31.9
1930-1935,1156.8,1030.0,89.0,126.8,11.0
1935-1940,1060.2,895.1,84.4,165.1,15.6
1940-1945,1137.9,1049.1,92.2,88.8,7.8
1945-1950,1631.0,1268.2,77.8,362.8,22.2
1950-1955,1824.5,1465.6,80.3,358.9,19.7
1955-1960,1703.6,1546.6,90.8,157.0,9.2
1960-1965,1573.7,1537.2,97.7,36.5,2.3
1965-1970,1565.3,1531.3,97.8,34.0,2.2"

df_raw <- read.csv(text = txt, stringsAsFactors = FALSE)

url <- "https://www.estadisticaciudad.gob.ar/pgmedia/672.zip"

nombre <- "Lattes, A. E., Recchini de Lattes, Z. L. (1975). La Población de Argentina. Cuadro 2.2: Crecimiento total, vegetativo y migratorio por períodos quinquenales, 1870-1970"

institucion <- "Lattes, A. E., Recchini de Lattes, Z. L."

download_filename <- "lattes_la_poblacion_argentina_1975_cuadro22.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(destfile)

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 442,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
