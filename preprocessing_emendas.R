rm(list=ls())
library(tm)

#################### FUNCOES AUXILIARES ####################

busca_termo_bin <- function(x, termo){
  resultado <- grep(termo, x)
  if (length(resultado) > 0) return(1)
  else return(0)
}

busca_termo_posicao <- function(x, termo){
  resultado <- grep(termo, x)
  if (length(resultado) > 0) return(resultado)
  else return(NA)
}

primeiro_termo <- function(x){
  return(x[1])
}

remove_texto <- content_transformer(
  function(x, termo, fim = T){
    posicao <- busca_termo_posicao(x, termo)
    posicao <- primeiro_termo(posicao)
    texto <- as.character(x)
    if (!is.na(posicao)){
      if (fim == T) return(texto[1:(posicao - 1)])
      else return(texto[(posicao + 1):length(as.character(x))])
    }
    else return(texto)
  }
)

remove_linhas_nulas <- content_transformer(
  function(x){
    texto <- as.character(x)
    linhas <- (nchar(texto) > 0)
    return(texto[linhas])
  }
)

remove_espaco_inicio <- content_transformer(
  function(x){
    texto <- as.character(x)
    linhas <- (substr(texto, 1, 1) == " ")
    texto[linhas] <- substr(texto, 2, nchar(texto))
    return(texto)
  }
)

#################### CORPUS TESTE ####################

pastaOut <- "/home/lasagnadesktop/emendas_old_files/proposicoes_2014_txt"
corpusEmendas <- Corpus(DirSource(pastaOut, encoding = "UTF-8"), readerControl = list(language = "por"))
#corpusEmendas <- corpusEmendas[1:5]

corpusEmendas <- tm_map(corpusEmendas, content_transformer(tolower))
#corpusEmendas <- tm_map(corpusEmendas, removePunctuation)
#corpusEmendas <- tm_map(corpusEmendas, removeNumbers)
#corpusEmendas <- tm_map(corpusEmendas, removeWords, stopwords("portuguese"))
#corpusEmendas <- tm_map(corpusEmendas, removeWords, c("câmara", "deputados"))
corpusEmendas <- tm_map(corpusEmendas, stripWhitespace)
#corpusEmendas <- tm_map(corpusEmendas, stemDocument)

# Tipo emenda
cod <- names(corpusEmendas)
cod <- substr(cod, 1, (nchar(cod) - 4))
substitutiva <- unlist(lapply(corpusEmendas, busca_termo_bin, "emenda substitutiva"))
modificativa <- unlist(lapply(corpusEmendas, busca_termo_bin, "emenda modificativa"))
supressiva <- unlist(lapply(corpusEmendas, busca_termo_bin, "emenda supressiva"))
aditiva <- unlist(lapply(corpusEmendas, busca_termo_bin, "emenda aditiva"))
sem_tipo <- unlist(lapply(corpusEmendas, busca_termo_bin, "emenda nº"))

tipo_emenda <- data.frame(cod, substitutiva, modificativa, supressiva, aditiva, sem_tipo)

# Corta justificativa
corpusEmendas <- tm_map(corpusEmendas, remove_texto, "justificação", fim = T)
corpusEmendas <- tm_map(corpusEmendas, remove_texto, "justificativa", fim = T)

# Corta cabecalho
corpusEmendas <- tm_map(corpusEmendas, remove_texto, "emenda substitutiva", fim = F)
corpusEmendas <- tm_map(corpusEmendas, remove_texto, "emenda modificativa", fim = F)
corpusEmendas <- tm_map(corpusEmendas, remove_texto, "emenda supressiva", fim = F)
corpusEmendas <- tm_map(corpusEmendas, remove_texto, "emenda aditiva", fim = F)
corpusEmendas <- tm_map(corpusEmendas, remove_texto, "emenda nº", fim = F)

# Remove linhas nulas
corpusEmendas <- tm_map(corpusEmendas, remove_linhas_nulas)

# Remove espaco inicio
corpusEmendas <- tm_map(corpusEmendas, remove_espaco_inicio)


###################################

for (i in 1:50){
  cat("\014")  
  print(as.character(corpusEmendas[[i]]))
  Sys.sleep(2)
}


# Termos
caput <- unlist(lapply(corpusEmendas, busca_termo_posicao, "§"))
paragrafo <- unlist(lapply(corpusEmendas, busca_termo_posicao, "parágrafo"))
artigo_abrv <- unlist(lapply(corpusEmendas, busca_termo_posicao, "art."))
artigo_extn <- unlist(lapply(corpusEmendas, busca_termo_posicao, "artigo")) 

###################################

summary(corpusEmendas)
inspect(corpusEmendas)
inspect(corpusEmendas[1:5])
meta(corpusEmendas[[3]])
as.character(corpusEmendas[[3]])
lapply(corpusEmendas[1:2], as.character)
getTransformations()
dtm <- DocumentTermMatrix(corpusEmendas)
inspect(dtm[1:5, 80:100])
findFreqTerms(dtm, 50)

