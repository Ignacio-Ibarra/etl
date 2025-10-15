#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


url <- "https://revistasacademicas.unsam.edu.ar/index.php/redd/article/view/909/1334"

texto <- '
sector_indec,sector,1914,1935,1946,1953,1963,1973,1984,1993,2003
Elaboración de productos alimenticios y bebidas,Alimentos y bebidas,3229,2818,8143,9595,12476,14254,10553,11108,16235
Elaboración de productos de tabaco,Tabaco,387,123,1141,1678,1661,490,1467,2731,391
Fabricación de productos textiles,Textiles,141,729,4659,6312,5095,5645,4586,1919,2228
Fabricación de prendas de vestir; terminación y teñido de pieles,Prendas de vestir,120,644,2344,2681,1805,2008,1722,1336,1315
"Curtido y terminación de cueros; fabricación de artículos de marroquinería, talabartería y calzado y de sus partes",Calzado y cueros,479,371,1614,1741,1137,1076,1092,1178,1381
"Producción de madera y fabricación de productos de madera y corcho, excepto muebles; fabricación de artículos de paja y de materiales trenzables","Madera, corcho y paja",762,345,1393,1351,777,1191,693,574,1018
Fabricación de papel y de productos de papel,Papel y Productos de papel,50,104,622,856,1400,1651,1498,1220,2746
Edición e impresión; reproducción de grabaciones,Edición e impresión,235,842,1323,1506,1443,1694,1440,2480,2317
"Fabricación de coque, productos de la refinación del petróleo y combustible nuclear",Refinación de petróleo,10,158,824,1884,3128,4665,10426,6377,5008
Fabricación de sustancias y productos químicos,Productos químicos,244,340,2310,3412,5242,7091,6544,6044,10297
Fabricación de productos de caucho y plástico,Productos de Caucho y plásticos,3,74,240,440,1827,2652,2026,1767,3080
Fabricación de productos minerales no metálicos,Productos minerales no metálicos,422,276,1552,2154,2415,3324,2117,1922,2331
Fabricación de metales comunes,Fabricación de metales comunes,52,275,1061,1688,2918,6325,2994,1984,5561
"Fabricación de productos elaborados de metal, excepto maquinaria y equipo",Productos elaborados de metal,493,348,1417,2250,3359,3918,3174,1794,2256
Fabricación de maquinaria y equipo n.c.p.,Maquinaria y equipo,0,159,581,1482,2641,3743,2322,2625,2872
"Fabricación de maquinaria de oficina, contabilidad e informática",Maquinaria de oficina,0,0,0,0,144,94,115,101,194
Fabricación de maquinaria y aparatos eléctricos n.c.p.,Maquinaria y aparatos eléctricos,38,61,410,1799,1954,2246,1482,1033,886
"Fabricación de equipos y aparatos de radio, televisión y comunicaciones",Frabricación de aparatos de radio,0,24,138,326,619,1057,778,961,323
"Fabricación de instrumentos médicos, ópticos y de precisión; fabricación de relojes",Instrumentos médicos,25,13,113,114,284,372,217,306,311
"Fabricación de vehículos automotores, remolques y semirremolques",Fabricación de vehículos,0,273,153,311,4010,5692,3987,3484,3033
Fabricación de equipo de transporte n.c.p.,Equipo de transporte,134,556,921,1564,1999,1517,754,277,404
Fabricación de muebles y colchones; industrias manufactureras n.c.p.,Muebles y colchones,324,442,1199,1731,994,1233,660,843,928
Industria manufacturera,Industria total,7148,8975,32158,44875,57328,71938,60647,52064,65115
'


df_raw <- read.csv(text = texto, sep = ",", stringsAsFactors = FALSE, quote = '"')


download_filename <- "kulfas_salles_2020.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% argendataR::write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url,
#                    institucion = "Escuela de Economía y Negocios. Universidad Nacional de San Martín",
#                    nombre = "Kulfas, Matías y Salles, Andrés (2020). Evolución histórica de la industria manufacturera argentina. Un análisis a partir de la homogeneización de los censos industriales, 1895-2005. Revista Economía y Desafíos del Desarrollo | Año 3. Volúmen 1. Número 5 | Diciembre 2019 - Mayo 2020",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = "Sin informacion",
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 460,
                      url = url, 
                      nombre = "Kulfas, Matías y Salles, Andrés (2020). Evolución histórica de la industria manufacturera argentina. Un análisis a partir de la homogeneización de los censos industriales, 1895-2005. Revista Economía y Desafíos del Desarrollo | Año 3. Volúmen 1. Número 5 | Diciembre 2019 - Mayo 2020. CUADRO 5: VALOR AGREGADO BRUTO POR RAMA CIIU A DOS DÍGITOS (PAR-TICIPACIÓN EN % DEL TOTAL)",
                      fecha_actualizar = "Sin informacion",
                      script = code_name,
                      path_raw = download_filename)
