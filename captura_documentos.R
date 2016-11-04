library(bRasilLegis); library(XML)

dir.create("~/pos_doc/teste_2014")
setwd("~/pos_doc/teste_2014")


listarProposicoesAno <- function(ano, silent = F) {
  output <- data.frame()
  tiposProposicoes <- listarSiglasTipoProposicao()
  for (sigla in trimws(tiposProposicoes$tipoSigla)){
    if (silent == F) print(paste("Obtendo", sigla, "de", ano))
    tabela <- try(listarProposicoes(sigla = sigla, ano = ano), silent=TRUE)
    if (!('try-error' %in% class(tabela))){
      output <- rbind(output, tabela)
    }
    Sys.sleep(0.2)
  }
  return(output)
}

proposicoes <- listarProposicoesAno(2014)

save.image("proposicoes_id.RData")

####################################

rm(list=ls())
library(bRasilLegis); library(XML)
setwd("/home/acsa/Leo/pos_doc/teste_2014")
load("proposicoes_id.RData")

url_download <- data.frame()
dados <- data.frame()
url.inteiro.teor <- "http://www2.camara.leg.br/proposicoesWeb/"
url.proposicao <- "http://www2.camara.leg.br/proposicoesWeb/fichadetramitacao?idProposicao="

for (i in 1:length(proposicoes$id)){
  
  print(i)
  
  # Proposicao principal
  idProp <- proposicoes$id[i]
  url <- paste0(url.proposicao, idProp)
  page <- xmlRoot(htmlParse(readLines(url)))
  final.url.doc <- try(xpathApply(page, 
                                  "//a[@class='rightIconified iconDetalhe linkDownloadTeor']", 
                                  xmlGetAttr, name = "href")[[1]])
  if (!('try-error' %in% class(final.url.doc))) {
    url <- paste(url.inteiro.teor, final.url.doc, sep = "")
    nome.arquivo <- file.path(getwd(), paste(idProp, "_000000", ".pdf", sep = ""))
    url_download <- rbind(url_download, data.frame("i" = i ,'url' = url, 
                                                   'arquivo' = nome.arquivo, 'cod' = 0,
                                                   stringsAsFactors = F))
  } 

  # Proposicao Apensadas  
  emendas <- obterEmendasSubstitutivoRedacaoFinal(proposicoes$tipoProposicao.sigla[i], proposicoes$numero[i], proposicoes$ano[i])
  if (!is.null(emendas)){
    print("emendas")
    for (cod in emendas$CodProposicao){
      url <- paste(url.proposicao, cod, sep = "")
      page <- xmlRoot(htmlParse(readLines(url)))
      final.url.doc <- try(xpathApply(page,
                                  "//a[@class='rightIconified iconDetalhe linkDownloadTeor']", 
                                  xmlGetAttr, name = "href")[[1]])
      if (!('try-error' %in% class(final.url.doc))) {
        url <- paste0(url.proposicao, final.url.doc)
        nome.arquivo <- file.path(getwd(), paste(idProp, "_", cod, ".pdf", sep = ""))
        url_download <- rbind(url_download, data.frame("i" = i ,'url' = url, 
                                                       'arquivo' = nome.arquivo, 'cod' = cod, 
                                                       stringsAsFactors = F))
      }
    }
    dados <- rbind(dados, merge(proposicoes[i,], emendas))
  }
}

documentos_erro <- data.frame()
for (j in 1:nrow(url_download)){
  print(j)
  erro_download <- try(download.file(url_download$url[j], url_download$arquivo[j]))
  if ('try-error' %in% class(erro_download)) {
    documentos_erro <- rbind(documentos_erro, url_download[j, ])
  }
}


