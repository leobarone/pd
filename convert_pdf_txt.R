library(tm)

pdfToText <- function(arquivo){
  texto <- readPDF(control = list(text = "-layout"))(elem = list(uri = arquivo),
                                                     language = "pt", id = "id1")
  texto <- as.character(texto)
  return(texto)  
}


#http://electricarchaeology.ca/2014/07/15/doing-ocr-within-r/
ocr_convert <- function(pdfFileIn, txtFileOut){
  system((paste0("pdftoppm ", pdfFileIn, " -f 1 -l 10 -r 600 ocrbook")))
  system((paste0("convert *.ppm ", pdfFileIn, ".tif")))
  system((paste0("tesseract ", pdfFileIn, ".tif ", txtFileOut, " -l por")))
  file.remove(paste0(pdfFileIn, ".tif" ))
}

convert_pdf_txt <- function(pastaIn, pastaOut){
  lista.arquivos <- list.files(pastaIn)
  
  for (i in 1:length(lista.arquivos)){
    print(i)
    nome.arquivo <- lista.arquivos[i]
    arquivo <- file.path(pastaIn, nome.arquivo)
    arquivoOut <- file.path(pastaOut, paste0(substr(nome.arquivo, 1, (nchar(nome.arquivo)-4))))
    
    texto <- pdfToText(arquivo)
    
    
    if (length(texto) > 1){
      writeLines(texto, paste0(arquivoOut, ".txt"))
    }
    else{
      ocr_convert(arquivo, arquivoOut)
    }
  }
}

pastaIn <- "/home/lasagnadesktop/emendas/proposicoes_2014"
pastaOut <- "/home/lasagnadesktop/emendas/proposicoes_2014_txt_com_ocr"

convert_pdf_txt(pastaIn, pastaOut)

