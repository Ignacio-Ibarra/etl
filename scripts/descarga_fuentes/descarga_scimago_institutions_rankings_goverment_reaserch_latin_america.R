# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-02-28")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad  


source("scripts/utils/scraper_scimago_institutions_rankings.R")

ranking = "Reaserch"

country = "Latin America"

start_year <- 2009

end_year <- year(Sys.Date())-1

years = start_year:end_year

sector = 'Government'

df_raw <- SCIMAGO.get_data(ranking = ranking, country = country, year = years, sector = sector)

url_general <- "https://www.scimagoir.com/rankings.php"

nombre <- glue::glue("SCImago Institutions Rankings - {ranking} Ranking - Sector: {sector} - Country: {country}")

institucion <- "SCIMAGO LAB"

georef <- str_replace_all(country,'\\s+','_')

download_filename <- glue::glue("SCIMAGO_{ranking}_{sector}_{georef}_{start_year}_{end_year}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)


# agregar_fuente_raw(url = url_general,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 353,
                      url = url_general, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)