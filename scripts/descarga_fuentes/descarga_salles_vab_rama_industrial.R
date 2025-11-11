#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


url <- "https://www.argentina.gob.ar/sites/default/files/la_industria_argentina_en_su_tercer_siglo_-_version_digital.pdf"

texto <- '
Provincia,1913,1935,1946,1953,1963,1973,1984,1993,2003
Buenos Aires,0.2356,0.265,0.3086,0.3729,0.4348,0.4805,0.536,0.4705,0.48
Catamarca,0.0018,0.0009,0.0015,0.001,0.0005,0.0006,0.0016,0.0049,0.0052
Chaco,0.0093,0.0135,0.0107,0.0115,0.0086,0.0066,0.0055,0.0035,0.0043
Chubut,0.0005,0.0064,0.0042,0.0025,0.0051,0.0062,0.015,0.0085,0.0169
Ciudad de Buenos Aires,0.3719,0.4676,0.4549,0.388,0.2747,0.2062,0.1364,0.2211,0.1483
Córdoba,0.0351,0.0341,0.0319,0.0385,0.0609,0.0657,0.0587,0.061,0.0632
Corrientes,0.0087,0.0068,0.0041,0.0053,0.0077,0.0048,0.0073,0.0131,0.0067
Entre Ríos,0.0305,0.016,0.0105,0.0127,0.0117,0.0094,0.0088,0.0103,0.0153
Formosa,0.0012,0.0019,0.0016,0.0015,0.0011,0.0015,0.001,0.001,0.0011
Jujuy,0.0124,0.0078,0.0044,0.0055,0.0086,0.0114,0.0112,0.0056,0.0067
La Pampa,0.0025,0.0024,0.0024,0.0015,0.0016,0.0016,0.0023,0.0023,0.002
La Rioja,0.0034,0.0009,0.0015,0.0009,0.0006,0.0008,0.0046,0.0061,0.0106
Mendoza,0.0611,0.016,0.0366,0.0262,0.0374,0.0452,0.0431,0.0521,0.0423
Misiones,0.0039,0.0016,0.0027,0.003,0.0044,0.0054,0.0082,0.0058,0.0139
Neuquén,0.0007,0.0005,0.0012,0.0009,0.0021,0.0027,0.0051,0.0036,0.0072
Río Negro,0.0006,0.0027,0.0037,0.0044,0.0046,0.0047,0.0047,0.0039,0.0043
Salta,0.0052,0.0064,0.0077,0.0126,0.0166,0.0104,0.0142,0.006,0.0084
San Juan,0.0138,0.0042,0.0071,0.0061,0.0058,0.0088,0.0049,0.0102,0.0103
San Luis,0.005,0.0032,0.0021,0.0019,0.0014,0.0018,0.0051,0.0275,0.0273
Santa Cruz,0.0006,0.0016,0.0009,0.001,0.0006,0.0005,0.0007,0.001,0.0018
Santa Fe,0.1059,0.0874,0.0719,0.0741,0.0891,0.0972,0.0817,0.0594,0.1012
Santiago del Estero,0.0172,0.0063,0.009,0.0061,0.0017,0.0022,0.002,0.0019,0.0018
Tierra del Fuego,0.0001,0.0007,0.0003,0.0005,0.0003,0.0004,0.0118,0.009,0.0082
Tucumán,0.073,0.0461,0.0203,0.0214,0.0202,0.0256,0.0296,0.0116,0.0129
'


df_raw <- read.csv(text = texto, sep = ",", stringsAsFactors = FALSE, quote = '"')

nombre = "Salles, A. (2021) “Estadísticas industriales en el largo plazo”. En: La industria argentina en su tercer siglo: Una historia multidisciplinar (1810-2020). Capítulo de libro. Compilador: Marcelo Rougier. Cuadro 15. Argentina. Industria manufacturera: porcentaje del valor agregado
por provincia, 1913-2003"

institucion = "Ministerio de Desarrollo Productivo de la Nación"


download_filename <- "salles_2021_pbg_largo_plazo.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% argendataR::write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = "Sin informacion",
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 461,
                      url = url, 
                      nombre = nombre,
                      fecha_actualizar = "Sin informacion",
                      script = code_name,
                      path_raw = download_filename)