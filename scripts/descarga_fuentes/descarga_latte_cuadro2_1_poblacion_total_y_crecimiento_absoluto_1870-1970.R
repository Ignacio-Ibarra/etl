# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

txt <- "anio,poblacion_total,incremento_absoluto,incremento_porcentual
1870,1819.0,NA,NA
1875,2087.0,268.0,14.7
1880,2345.8,258.8,12.4
1885,2738.7,392.9,16.7
1890,3595.1,856.4,31.3
1895,4049.4,454.3,12.6
1900,4692.7,643.3,15.9
1905,5396.5,703.8,15.0
1910,6770.1,1373.6,25.5
1915,8235.7,1465.6,21.6
1920,8972.4,736.7,8.9
1925,10424.8,1452.4,16.2
1930,11935.7,1510.9,14.5
1935,13092.5,1156.8,9.7
1940,14152.7,1060.2,8.1
1945,15290.6,1137.9,8.0
1950,16921.6,1631.0,10.7
1955,18746.1,1824.5,10.8
1960,20449.7,1703.6,9.1
1965,22023.4,1573.7,7.7
1970,23588.7,1565.3,7.1"

df_raw <- read.csv(text = txt, stringsAsFactors = FALSE)

url <- "https://www.estadisticaciudad.gob.ar/pgmedia/672.zip"

nombre <- "Lattes, A. E., Recchini de Lattes, Z. L. (1975). La Población de Argentina. Cuadro 2.1: Población total e incrementos absolutos y porcentuales por quinquenios, 1870-1970"

institucion <- "Lattes, A. E., Recchini de Lattes, Z. L."

download_filename <- "lattes_la_poblacion_argentina_1975_cuadro21.csv"

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

actualizar_fuente_raw(id_fuente = 443,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)