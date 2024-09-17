#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 219
fuente_raw <- sprintf("R%sC0",id_fuente)

sheet_name = "Regional data"


lookup <-c('East Asia' = 'EAS_MPD',
              'Eastern Europe' = 'EEU_MPD',
              'Latin America' = 'LAC_MPD',
              'Middle East and North Africa' = 'MNA_MPD',
              'South and South East Asia' = 'SEA_MPD',
              'Sub Saharan Africa' = 'SSA_MPD',
              'Western Europe' = 'WEU_MPD',
              'Western Offshoots' = 'WOF_MPD',
              'World' = 'WLD')

regiones_MDP <- c("EAS_MPD" = "Asia Oriental (Maddison Project Database)",
                  "EEU_MPD" = "Europa Oriental (Maddison Project Database)",
                  "LAC_MPD" = "América Latina (Maddison Project Database)",
                  "MNA_MPD" = "Medio Oriente y África del Norte (Maddison Project Database)",
                  "SEA_MPD" = "Asia del Sur y Sudeste (Maddison Project Database)",
                  "SSA_MPD" = "África Subsahariana (Maddison Project Database)",
                  "WEU_MPD" = "Europa Occidental (Maddison Project Database)",
                  "WOF_MPD" = "Ramificaciones de Occidente (Maddison Project Database)",
                  "WLD" = "Mundo")


df_raw <- readxl::read_excel(get_raw_path(fuente_raw), sheet = sheet_name, col_names = FALSE) 

df_raw_gdppc <- df_raw[3:nrow(df_raw), 1:10]
cols <- c("anio", df_raw[2,2:9] %>% as.character(), "World")
colnames(df_raw_gdppc) <- cols

df_clean_gdppc <- df_raw_gdppc %>% 
  pivot_longer(cols = -anio, names_to = "region", values_to = 'gdppc') %>% 
  dplyr::filter(!is.na(gdppc))


df_raw_pop <- df_raw[3:nrow(df_raw), c(1,12:20)]
cols <- c("anio", df_raw[2,12:19] %>% as.character(), "World")
colnames(df_raw_pop) <- cols


df_clean_pop <- df_raw_pop %>% 
  pivot_longer(cols = -anio, names_to = "region", values_to = 'pop') %>% 
  dplyr::filter(!is.na(pop)) %>% 
  mutate(region = ifelse(region == "Sub Saharan SSA", "Sub Saharan Africa", region))


df_clean <- df_clean_gdppc %>% 
  left_join(df_clean_pop, join_by(anio, region)) %>% 
  mutate(iso3 = lookup[region],
         region = regiones_MDP[iso3],
         gdppc = as.numeric(gdppc),
         pop = as.numeric(pop)*1000,
         pib = gdppc * pop
         ) %>% 
  select(anio, iso3, region, gdppc, pop, pib)



nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

clean_filename <- glue::glue("{nombre_archivo_raw}_regional_data_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = "Maddison Project Database (2023) - Solapa: Regional Data",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 91, path_clean = clean_filename, directorio = tempdir())
