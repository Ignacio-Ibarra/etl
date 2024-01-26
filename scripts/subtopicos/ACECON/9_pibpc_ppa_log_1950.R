# 9_pibpc_ppa_log_1950
# descripcion

# vars config del script
output_name <- "9_pibpc_ppa_log_1950"
# periodo, etc.,

# Descargas -------


# Lectura -------

# maddison database
# GDP pc	Real GDP per capita in 2011$
# Population	Population, mid-year (thousands)


pibpc_maddison_db <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"),
                                        sheet = "GDP pc", skip = 1)

pop_maddison_db <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"),
                                      sheet = "Population", skip = 1)


# imf weo
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)

weo_imf <- read_tsv(glue::glue("data/{subtopico}/datasets/raw/WEOOct2023all.xls"))

weo_imf_dictionary <- weo_imf %>% 
  select(2:9)
  
# Procesamiento -------


#  países de América Latina & Caribe que tenían más de 2,5 millones de habitantes en 2018
paises_sel <- tibble(pais = c("Argentina",
                "Bolivia",
                "Brasil",
                "Chile ",
                "Colombia ",
                "Costa Rica ",
                "Cuba ",
                "República Dominicana ",
                "Ecuador ",
                "Guatemala ",
                "Honduras ",
                "Haití ",
                "Jamaica ",
                "México ",
                "Nicaragua ",
                "Panamá ",
                "Perú ",
                "Puerto Rico ",
                "Paraguay ",
                "El Salvador ",
                "Uruguay",
                "Venezuela",
                "Hong Kong",
                "Corea del Sur",
                "Singapur",
                "Taiwán"
                ))

paises_iso <- get_iso_paises()
 
paises_sel <- left_join(paises_sel %>% 
                          mutate(pais = textclean::replace_non_ascii(tolower(pais))),
                        paises_iso %>% 
                            mutate(pais = textclean::replace_non_ascii(tolower(pais)))) %>% 
  filter()

paises_sel_al <- paises_sel %>% 
  filter(! pais %in% textclean::replace_non_ascii(tolower(c("Hong Kong",
                     "Corea del Sur",
                     "Singapur",
                     "Taiwán"))))

# maddison db
# pib pc
pibpc_maddison_db <- pibpc_maddison_db %>% 
  filter(year %in% 1950:2018)

pibpc_maddison_db <- pibpc_maddison_db %>% 
  # excluyo columnas que solo tienen NA
  select(where(\(x){all(!is.na(x))}))

pibpc_maddison_db <- pibpc_maddison_db %>%
  #  paso a formato largo
  pivot_longer(cols = -year, names_to = "iso", values_to = "ngdprppppc")

  
pibpc_maddison_db <- pibpc_maddison_db %>%
  filter(iso %in% paises_sel$iso3)


# poblacion
pop_maddison_db <- pop_maddison_db %>% 
  filter(year %in% 1950:2018)

pop_maddison_db <- pop_maddison_db %>% 
  # excluyo columnas que solo tienen NA
  select(where(\(x){all(!is.na(x))}))

pop_maddison_db <- pop_maddison_db %>%
  #  paso a formato largo
  pivot_longer(cols = -year, names_to = "iso", values_to = "pop")


pop_maddison_db <- pop_maddison_db %>%
  filter(iso %in% paises_sel$iso3)

pop_maddison_db <- pop_maddison_db %>%
  mutate(pop = pop*10^3)

# reuno datos de maddison
maddison_db <- left_join(pibpc_maddison_db, pop_maddison_db)

maddison_db <- maddison_db %>% 
  mutate(ngdprppp = ngdprppppc*pop)

maddison_db <- maddison_db %>% 
  group_by(iso = ifelse(iso %in% paises_sel_al$iso3, "AMLAT", "TIGRES"),
           year) %>% 
  summarise(ngdprppp = sum(ngdprppp),
            pop = sum(pop), 
            ngdprppppc = ngdprppp/pop) %>% 
  ungroup() %>% 
  bind_rows(maddison_db)

maddison_db <- maddison_db %>%
  rename(anio = year)

# datos fmi world economic outlook

weo_imf <- tidy_weo(weo_imf)

weo_imf <- weo_imf %>% 
  filter(iso %in% paises_sel$iso3 & anio %in% 2018:2022) %>% 
  select(anio, iso, ngdprppppc, pop = lp) %>%
  mutate(pop = pop*10^6,
         ngdprppp = ngdprppppc*pop) 

weo_imf <- weo_imf %>% 
  group_by(iso = ifelse(iso %in% paises_sel_al$iso3, "AMLAT", "TIGRES"),
           anio) %>% 
  summarise(ngdprppp = sum(ngdprppp),
            pop = sum(pop), 
            ngdprppppc = ngdprppp/pop) %>% 
  ungroup() %>% 
  bind_rows(weo_imf)

weo_imf <- weo_imf %>% 
  select(anio, iso, ngdprppppc) %>% 
  group_by(iso) %>% 
  arrange(anio) %>% 
  mutate(var_ngdprppppc = ngdprppppc/lag(ngdprppppc))  %>% 
  ungroup() %>% 
  filter(anio != 2018) %>% 
  select(-ngdprppppc)

df_output <- maddison_db %>% 
  bind_rows(weo_imf) %>% 
  arrange(iso, anio) %>% 
  mutate(ngdprppppc = expansor_xvar(ngdprppppc, var_ngdprppppc)) %>% 
  select(anio, iso, ngdprppppc) 

df_output <- df_output %>% 
  left_join(paises_iso, by = c("iso" = "iso3")) %>%
  mutate(pais = case_when(
    iso == "AMLAT" ~ "America Latina",
    iso == "TIGRES" ~ "Tigres Asiaticos",
    T ~ pais
  )) %>% 
  rename(pais_o_region = pais,
         pbi_per_capita_ppa = ngdprppppc)

# Control vs output previo -------

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
  mutate(across(-c(pais_o_region), as.numeric))

vs <- out_prev %>% 
  left_join(df_output, by = c("anio", "pais_o_region"))

vs <-  vs %>% 
   mutate(across(where(is.numeric), \(x) round(x, 2))) 

diff <- comparar_cols(vs) 



# Write output ------


df_output %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
  subtopico = subtopico)
