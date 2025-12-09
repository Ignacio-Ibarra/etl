# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


str_extracted <- "temporada,totales,autos,autos_pct,micros,micros_pct,trenes,trenes_pct,aviones,aviones_pct
1886-87,1416,,,,,,,,
1887-88,2510,,,,,,,,
1900-01,10000,,,,,,,,
1920-21,40370,,,,,,,,
1930-31,65010,,,,,,,,
1935-36,121276,22792,19,12876,1,85598,70.5,,
1940-41,376893,197366,53,58980,16,159947,42.4,,
1945-46,504517,139950,27.8,188194,37.3,173484,34.4,2979,0.6
1950-51,990542,403786,40.7,213345,21.6,366329,37,6882,0.7
1955-56,1141536,434695,38.1,405455,35.5,295040,25.8,6070,0.5
1956-57,1044170,382217,36.6,324505,31.1,322766,30.9,4884,0.5
1957-58,1209324,485727,40.2,412190,34.1,300758,24.9,10469,0.8
1958-59,1211061,423889,35,455637,37.6,318633,26.3,6902,0.6
1959-60,1303052,475322,36.5,472367,36.2,334755,25.7,20608,1.6
1960-61,1450817,596809,41.1,538934,37.1,291018,20.5,24056,1.7
1963-64,1623808,806187,49.6,540138,23.3,255337,15.7,2682,0.2
1964-65,1184920,526470,44.3,444532,37.6,198865,16.8,14773,1.2
1965-66,1355448,686893,50.7,399289,29.4,254967,18.2,14259,1.1
1966-67,1493907,848052,56.8,382154,25.6,244584,16.4,19117,1.3
1967-68,2026201,1202002,59.3,467054,23.1,327778,16.2,27368,1.3
1970-71,2027222,1231804,60.8,458933,22.6,278396,13.7,58089,2.9
1971-72,2475772,1341329,54.2,742565,29.9,328388,13.3,63490,2.6
1972-73,2868593,1651316,54.6,713540,24.9,454140,15.9,49597,1.7
1974-75,2493591,1495625,60,465970,18.7,477573,19.1,54426,2.2
1975-76,2290242,1210981,52.9,505784,22.9,498374,21.8,75103,3.3"


df_raw <- read.csv(text = str_extracted)


nombre_archivo <- "Pastoriza, Elisa (2008). Estado, gremios y hoteles. Mar del Plata y el peronismo. Estudios Sociales, número 34, primer semestre de 2008. Cuadro 2: Pasajeros ingresados a Mar del Plata (temporadas verano)"

url <- "https://historiapolitica.com/datos/biblioteca/pastoriza.pdf"

institucion <- "Departamento de Historia. Facultad de Humanidades. Universidad Nacional de Mar del Plata"

download_filename <- "pastoriza_ingresos_mar_del_plata.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")


df_raw %>% 
  argendataR::write_csv_fundar(., destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre_archivo,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 482,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)