# Encoder

rm(list = ls())
cat("\014")
source("functions.R")

# Read pass
pass <- readline("Enter password: ")

# Encrypt files
for(path in list.files(path = "./code")){
  cat("Encrypting ", path, "...\n")
  texto <- read_file(paste0("./code/", path))
  enc.text <- EncodeText(texto, pass)
  write(enc.text, file = paste0("./encoded/", path))
}
