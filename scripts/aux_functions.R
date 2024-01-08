
write_argendata <- function(data, file_name, subtopico) {
  write.csv(x = data, file = glue::glue("data/{subtopico}/datasets/outputs/{file_name}"),
            eol = "\n", dec = ".", na = "",row.names = F, fileEncoding = "UTF-8")
}

# para empalmar una serie usando variaciones de otra serie
# dada una columna x con los valores de la serie a expandir: Xi a Xn de 1 a N 
# y dada una columna var donde VARi = Xi/Xi-1 como las proporciones entre un valor de x y su antecedente
# si Xi es NA lo calcula y reemplaza como Xi-1*VARi, si no es NA lo deja tal cual

expansor_imf_maddison <- function(x,var) {
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i-1]*var[i]
    } 
  }
  x
}
