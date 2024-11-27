# library(tidyverse)

# "DESIGU",
# "POBREZ",
# "SALING"
# read fuentes from data --------------------------------------------------
data_path <- "~/data/"

subtopicos <- list.files(data_path)

# subtopicos <- c("DESIGU" 
#                  )

archivos <- list()

for (i in subtopicos) {
  
  archivos[[i]] <- list.files(glue::glue("{data_path}{i}"), full.names = T)
  archivos[[i]] <- grep("\\.json$", archivos[[i]], value = T)
  print(archivos[[i]])
}

archivos <- unlist(archivos)

fuentes <- list()

for (i in archivos) {
  
  name <- gsub(".*/([^/]*$)", "\\1", i)
  fuentes[[name]] <- jsonlite::fromJSON(txt = readLines(i, encoding = "utf-8"))$fuentes
  
}


fuentes <- map2(fuentes, names(fuentes), function(x, y) {
  tibble(dataset = y, fuente = x)
})

fuentes <- bind_rows(fuentes)

fuentes <- fuentes %>% 
  mutate(fuente_raw = gsub("C\\d{1,3}$", "C0", fuente))


df_fr<- fuentes_raw() %>% 
  distinct(codigo, script)


fuentes <- fuentes %>% 
  left_join(df_fr, by = c("fuente_raw" = "codigo")) %>% 
  rename(script_raw =  script)

df_cl<- fuentes_clean() %>% 
  distinct(codigo, script)

fuentes <- fuentes %>% 
  left_join(df_cl, by = c("fuente" = "codigo"))
  
fuentes <- fuentes %>% 
  select(dataset, fuente, fuente_raw, script_clean = script, script_raw)

print(scripts_clean)

