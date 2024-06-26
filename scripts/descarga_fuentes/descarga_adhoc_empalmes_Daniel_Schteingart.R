# Este script es especial porque trabaja con una fuente que
# por el momento no está disponible públicamente
# para ello se sube al server el archivo,luego se genera un script para
# generar la fuente raw
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Muevo archivo al tempdir para que lo suba desde ahi al filesystem remoto y genere la entrada en fuentes_raw

raw_file <- "pbg por provincia.xlsx"
folder.path <- "./data/_FUENTES/raw/data_adhoc"

agregar_fuente_raw(url = "https://docs.google.com/spreadsheets/d/1s3g05z8G2qL3Ez9IFLh94asqFp8QV3tc/edit#gid=1920186194",
                   nombre = "EMPALME - Fuente Aráoz, F., Nicolini, E. y Talassino, M. (2020)+ CEPAL-MECON + INDEC (1895 a 2022)",
                   institucion = "Fundar",
                   actualizable = F,
                   fecha_descarga = Sys.Date(),
                   directorio = folder.path,
                   path_raw = raw_file,
                   script = code_name,
                   api = F
)

actualizar_fuente_raw(id_fuente = 160, dir = "./data/_FUENTES/raw/data_adhoc")

                      