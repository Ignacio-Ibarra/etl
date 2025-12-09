# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


str_extracted <- "anio,turistas_cantidad
1992,5737818
1993,5710545
1994,5766451
1995,5524671
1996,5826828
1997,6002039
1998,6673122
1999,6325776
2000,6053886
2001,5506697
2002,5311763
2003,6180341
2004,6747239
2005,7125273
2006,7721767
2007,7973709
2008,8041846
2009,7989082
2010,8187257
2011,8542696
2012,8621946
2013,8476928
2014,8437644
2015,8846314
2016,8452695
2017,8570980
2018,8355934
2019,8063025
2020,3685937
2021,6644442
2022,8853245"


df_raw <- read.csv(text = str_extracted)


nombre_archivo <- "De 1992 a 2022: así fue la evolución del turismo en Mar del Plata durante los últimos 30 años"

url <- "https://www.lacapitalmdp.com/de-1992-a-2022-asi-fue-la-evolucion-del-turismo-en-mar-del-plata-durante-los-ultimos-30-anos/"

institucion <- "Ente Municipal de Turismo (via La Capital, 4 de enero de 2023)"

download_filename <- "emtur_la_capital_turismo_mar_del_plata.csv"

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

actualizar_fuente_raw(id_fuente = 483,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)