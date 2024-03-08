# Preparacion de la estructura de carpetas
# Se lee la estructura de carpetas de datasets de subtopicos para replicarla


walk(subtopicos()$name, \(x) {
  if (!dir.exists(glue::glue("scripts/subtopicos/{x}"))) {
    
    dir.create(glue::glue("scripts/subtopicos/{x}"))
  }
})

if (!dir.exists("data")) {dir.create("data")}

walk(subtopicos()$name, \(x) {
  
  
  if (!dir.exists(glue::glue("data/{x}"))) {
    
    dir.create(glue::glue("data/{x}"))
    dir.create(glue::glue("data/{x}/datasets"))
    dir.create(glue::glue("data/{x}/datasets/outputs"))
    dir.create(glue::glue("data/{x}/datasets/raw"))
  }
})


