# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

txt <- "quinquenio,poblacion_inicial,nacimientos,defunciones,crecimiento_vegetativo,inmigracion,emigracion,saldo_migratorio,crecimiento_total
1870-1874,1819048,479095,311045,168050,274414,174526,99888,267938
1875-1879,2086986,543356,327622,215734,250598,207556,43042,258776
1880-1884,2345762,622078,378900,243178,372381,222598,149783,392961
1885-1889,2738723,724600,470770,253830,949198,346701,602497,856327
1890-1894,3595050,840774,542956,297818,548828,392276,156552,454370
1895-1899,4049420,972558,630548,342010,669325,368036,301289,643299
1900-1904,4692719,1118302,659031,459271,784298,539806,244492,703763
1905-1909,5396482,1279050,690873,588177,1709434,924013,785421,1373598
1910-1914,6770080,1469220,740693,728527,2031428,1294384,737044,1465571
1915-1919,8235651,1568136,762287,805849,681617,750664,-69047,736802
1920-1924,8972453,1677742,727470,950272,1334623,832561,502062,1452334
1925-1929,10424787,1815062,786013,1029049,1872637,1390735,481902,1510951
1930-1934,11935738,1815388,785354,1030034,1663074,1536310,126764,1156798
1935-1939,13092536,1749051,853998,895053,1989365,1824301,165064,1060117
1940-1944,14152653,1878676,829557,1049119,1811628,1722834,88794,1137913
1945-1949,15290566,2108472,840274,1268198,2371749,2008914,362835,1361033"


df_raw <- read.csv(text = txt, stringsAsFactors = FALSE)

url <- "https://www.estadisticaciudad.gob.ar/pgmedia/672.zip"

nombre <- "Lattes, A. E., Recchini de Lattes, Z. L. (1975). La Población de Argentina. Anexo, Tabla 1: Población al inicio de cada quinquenio y componentes del crecimiento total,
vegetativo y migratorio, por quinquenios, 1870-1949"

institucion <- "Lattes, A. E., Recchini de Lattes, Z. L."

download_filename <- "lattes_la_poblacion_argentina_1975_tabla1_anexo.csv"

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

actualizar_fuente_raw(id_fuente = 444,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
