# Indicadores Sociales de Argentina: Perfil de ingresos laborales y educación - Ingreso laboral - 2022
descargar_fuente('R186C0')

# Indicadores Sociales de Argentina: Años de educación por quintiles de ingreso - Brecha de ingresos entre el quintil más rico y el más pobre.
descargar_fuente('R187C0')

# Indicadores Sociales de Argentina: Índice de igualdad de oportunidades educativas
descargar_fuente('R188C0')

# Indicadores Sociales de Argentina: Número de niños por hogar por quintiles - Quintiles de ingreso parental
descargar_fuente('R189C0')

# Indicadores Sociales de Argentina: Grado de emparejamiento selectivo - Coeficiente de correlación de años de educación entre miembros de una pareja
descargar_fuente('R190C0')

# Indicadores Sociales de Argentina: Desigualdad de ingresos - Participación de deciles en el ingreso total, 2022.
descargar_fuente('R191C0')

# Indicadores Sociales de Argentina: Desigualdad de ingresos en Argentina - Coeficiente de Gini de la distribución del ingreso per cápita familiar
descargar_fuente('R192C0')



# Indicadores Sociales de Argentina: Desigualdad de ingresos - Brecha de ingresos entre el decil más rico y el más pobre
descargar_fuente('R193C0')

# Indicadores Sociales de Argentina: Desigualdad de los ingresos laborales y del ingreso per cápita - Coeficiente de Gini
descargar_fuente('R194C0')

# Indicadores Sociales de Argentina: Brecha salarial entre trabajadores calificados y no calificados
descargar_fuente('R195C0')

# Indicadores Sociales de Argentina: Tasas de empleo y horas trabajadas por nivel educativo
descargar_fuente('R196C0')

# Indicadores Sociales de Argentina: Brecha de género en el salario horario - Brecha condicionada en variables observables
descargar_fuente('R197C0')

# Indicadores Sociales de Argentina: Tasas de participación laboral
descargar_fuente('R198C0')

# Indicadores Sociales de Argentina: Indicador de movilidad intergeneracional educativa
descargar_fuente('R199C0')

# Indicadores Sociales de Argentina: Movilidad intergeneracional educativa en el mundo
descargar_fuente('R200C0')

# Indicadores Sociales de Argentina: La escalera de la desigualdad en el mundo - Coeficiente de Gini de la distribución del consumo per cápita familiar
descargar_fuente('R201C0')

# Indicadores Sociales de Argentina: El “exceso de desigualdad” - Coeficiente de Gini del consumo per cápita familiar e ingreso nacional per cápita
descargar_fuente('R202C0')

# Indicadores Sociales de Argentina: Desigualdad de ingresos en Argentina y América Latina - Coeficiente de Gini del ingreso per cápita familiar
descargar_fuente('R203C0')

# Indicadores Sociales de Argentina: Desigualdad de ingresos por regiones - Coeficiente de Gini de la distribución del ingreso per cápita familiar
descargar_fuente('R204C0')

# Indicadores Sociales de Argentina: Brechas regionales de ingreso - Coeficiente de variación del ingreso entre regiones y brecha de ingresos entre GBA y Noreste
descargar_fuente('R205C0')

# Indicadores Sociales de Argentina: Propiedad de la vivienda por quintiles - Proporción de hogares propietarios de su vivienda
descargar_fuente('R206C0')

# Indicadores Sociales de Argentina: Brecha en el acceso a servicios entre quintil 5 y quintil 1
descargar_fuente('R207C0')

# EPH descarga de todos los años
ids.eph <- fuentes_raw() %>% filter(grepl("Encuesta Permanente de Hogares, Individual*", nombre)) %>% select(codigo) %>% pull()
purrr::map(ids.eph, descargar_fuente)
