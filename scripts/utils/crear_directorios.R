# Preparacion de la estructura de carpetas
# Se lee la estructura de carpetas de datasets de subtopicos para replicarla


walk(argendataR::subtopicos()$tree$name, function(x) {
  if (!dir.exists(glue::glue("scripts/subtopicos/{x}"))) {
    
    dir.create(glue::glue("scripts/subtopicos/{x}"))
  }
})

if (!dir.exists("data")) {dir.create("data")}

walk(subtopicos()$tree$name, function(x) {
  
  
  if (!dir.exists(glue::glue("data/{x}"))) {
    
    dir.create(glue::glue("data/{x}"))
  }
})

if (!dir.exists("data/_FUENTES")) {
  dir.create("data/_FUENTES")
  dir.create("data/_FUENTES/clean")
  dir.create("data/_FUENTES/raw")
}

if (!dir.exists("data/_FUENTES/raw")) {
  
  dir.create("data/_FUENTES/raw")
  
}

if (!dir.exists("data/_FUENTES/clean")) {
  
  dir.create("data/_FUENTES/clean")
  
}
