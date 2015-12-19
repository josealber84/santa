library(digest)
library(magrittr)
library(readr)


CreateKey <- function(pass){
  
  raw.pass <- charToRaw(pass)
  number.zeros <- 32 - length(raw.pass)
  zeros <- as.raw(rep(0, number.zeros))
  key <- c(zeros, raw.pass)
  
  key
  
}

EncodeText <- function(texto, pass){
  
  key <- CreateKey(pass)
  
  raw.text <- charToRaw(texto)
  number.spaces <- 16 - (length(raw.text) %% 16)
  spaces <- rep(" ", number.spaces) %>% paste(collapse = "")
  total.text <- paste0(texto, spaces)
  
  aes <- AES(key, "ECB")
  coded.text <- aes$encrypt(total.text)
  
  coded.text
  
}

DecodeText <- function(texto, pass){
  
  key <- CreateKey(pass)
  texto.limpio <- gsub(pattern = "\n", replacement = " ", x = texto)
  raw.text <- texto.limpio %>% strsplit(split = " ") %>% unlist %>%
    as.hexmode() %>% unlist() %>% as.raw()
  
  aes <- AES(key, "ECB")
  decoded.text <- aes$decrypt(raw.text)
  
  decoded.text
  
}