# Este script es especial porque trabaja con una fuente de
# dificil acceso (se necesita completar un formulario)
# para ello se sube al server el archivo,luego se genera un script para
# generar la fuente raw


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fuente_iso <- "CountryCodes"
url_fuente <- "https://www.iso.org/obp/ui/#search"
institution <- "ISO"


# Specify the directory to search in
directory_path <- glue::glue("{Sys.getenv('COMEX_ISO')}")

country_codes_iso_comext <- haven::read_dta( glue::glue("{directory_path}/country_codes.dta"))


path <- "country_codes_iso_comext.csv"


country_codes_iso_comext %>% 
  write_csv_fundar(glue::glue("{tempdir()}/{path}"))


# agregar_fuente_raw(url = url_fuente,
#                    nombre = "Codigos ISO - COMEXT",
#                    institucion = institution,
#                    actualizable = F,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = path,
#                    script = code_name,
#                    api = F
# )


actualizar_fuente_raw(id_fuente = "R158C0", dir = tempdir()) 



