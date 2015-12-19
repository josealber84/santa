# Decoder

rm(list = ls())
cat("\014")
source("functions.R")


# Read pass
pass <- readline("Enter password: ")

# Encrypt files
for(path in list.files(path = "./encoded")){
  cat("Encrypting ", path, "...\n")
  texto <- read_file(paste0("./encoded/", path))
  dec.text <- DecodeText(texto, pass)
  write(dec.text, file = paste0("./code2/", path))
}