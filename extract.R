library(pdftools)
library(stringr)
library(plyr)
library(tidyverse)
library(here)

#### settings ####

# list of "sub-labs" in your institution with INSTAND and RfB customer numbers

labLists <- list(
  'Gerinnung' = c('0000074', '57909'),  
  'Haula'  = c('0000007', '7025'),
  'Immu' = c('0000072', '57362'),
  'POCT' = c('0000073', '57870'),
  'Drug' = c('7024'),
  'Blutdepot' = c('0000075', '59639'),
  'molDiagn' = c('0000076')
)

# the year to extract

year <- '2021'


#### RfB ####

rfbPdf <- pdf_text(here('data', year, 'RfB-Programmheft.pdf'))
regex <- paste0("(\\w+/",substr(year, 3,4),
                ")\\s+(\\d{2}.\\d{2}.\\d{2})\\s+(\\d{2}.\\d{2}.\\d{2})\\s+(\\d{2}.\\d{2}.\\d{2})\\s+(\\d{2}.\\d{2}.\\d{2})\\s+(\\d{2}.\\d{2}.\\d{2})")
rfbMatches <- str_match_all(rfbPdf, regex)

res <- ldply(rfbMatches, function(x){
  if (!is.null(x) & ncol(x) == 7){
    data.frame(x)
  }
  else{
    data.frame
  }
})

resForm <- res %>%
  transmute(rv = str_split(X2, "\\d{1}/", simplify = TRUE)[,1],
            round = as.numeric(str_extract_all(str_extract_all(X2,"\\d{1}/"), "\\d{1}")),
            von = X5, bis = X6) 

files <- list.files(here('data', year))
rfbFiles <- str_extract(files, 'overview.+') %>% na.exclude()

rfbReadouts <- data.frame()

for(f in rfbFiles){

  bestelltPdf <- pdf_text(here('data', year,  f))
  regexBestellt <- "(\\w{2,10})\\s+[A|B]{1}\\s+(\\S{1,3})\\s+(\\S{1,3})\\s+(\\S{1,3})\\s+(\\S{1,3})\\s+(\\S{1,3})\\s+(\\S{1,3})\\s+(\\S{1,3})\\s+(\\S{1,3})"
  
  bestelltMatches <- str_match_all(bestelltPdf, regexBestellt)
  
  resBestellt <- ldply(bestelltMatches, function(x){
    if (!is.null(x) & ncol(x) == 10){
      data.frame(x)
    }
    else{
      data.frame
    }
  })
  
  lab <- str_match(f, '\\d+')
  
  resBestelltForm <- resBestellt %>%
    select(2:10) %>%
    gather(best, value, 2:9) %>%
    filter(value != '---') %>%
    transmute(round = as.numeric(str_sub(best, 2))-2, rv = as.character(X2)) %>%
    mutate(lab = lab)
  
  rfbReadouts <- rbind(rfbReadouts, resBestelltForm)
}
  
  
rfb <- rfbReadouts %>%
    inner_join(resForm) %>%
    mutate( eqa = 'RfB',
            von = as.POSIXct(strptime(von, format = "%d.%m.%y")),
            bis = as.POSIXct(strptime(bis, format = "%d.%m.%y"))) %>%
    dplyr::arrange(von)
  
  
#### Instand ####

files <- list.files(here('data', year))
instandFiles <- str_extract(files, 'Anmeldebestätigung.+') %>% na.exclude()

instandReadouts <- data.frame()

for(f in instandFiles){
  
  lab <- str_match(f, 'tln_(\\d+)')[2]
  
  best <- pdf_text(here("data", year, f))
  
  regInstand <- "(\\d{3})([\\s\\S]+?)Anzahl Probensätze"
  
  mInst <- str_match_all(best, regInstand)
  
  mx <- do.call("rbind", mInst)
  
  instand <- adply(mx[,1], 1, function(l){
    regRv <- "(\\d{3}.+)\\s+(\\d{1,3},\\d{2})"
    rv <- str_match(l, regRv)[1,2]
    rv <- str_replace(rv, ' A ', '') %>% str_trim()
    
    regZeiten <- "(\\s+(\\d{2}\\.\\d{2}\\.\\d{4}))?(\\s+(\\d{2}\\.\\d{2}\\.\\d{4}))?(\\s+(\\d{2}\\.\\d{2}\\.\\d{4}))?(\\s+(\\d{2}\\.\\d{2}\\.\\d{4}))?(\\s+(\\d{2}\\.\\d{2}\\.\\d{4}))?(\\s+(\\d{2}\\.\\d{2}\\.\\d{4}))?"
    versandM <- str_match_all(l, paste0("Versandtermin", regZeiten))
    versand <- versandM[[1]][, c(3,5,7,9,11,13)]
    
    
    rueckM <- str_match_all(l, paste0("Rücksendetermin", regZeiten))
    rueck <- rueckM[[1]][, c(3,5,7,9,11,13)]
    
    data.frame(rv = rv, von = versand[!is.na(versand)], 
               bis = rueck[!is.na(rueck)], round = 1:length(versand[!is.na(versand)]))
  })
  
  instand <- instand %>%
    transmute(
      eqa = 'Instand',
      rv = as.character(rv),
      round = as.numeric(round),
      von = as.POSIXct(strptime(von, format = "%d.%m.%Y")),
      bis = as.POSIXct(strptime(bis, format = "%d.%m.%Y"))) %>%
    mutate(lab = lab)
  
  instandReadouts <- rbind(instandReadouts, instand)
}
  


#### combine ####
combineAll <- rbind(instandReadouts, rfb) %>%
  arrange(von) %>%
  mutate_at(vars(von, bis), format, format = "%Y-%m-%d")




dir.create(here('generated', year), showWarnings = FALSE)

for(i in 1:length(labLists)){
  l <- names(labLists)[i]
  
  data <- combineAll %>%
    filter(lab %in% labLists[[i]]) %>%
    select(-lab)
  
  file <- paste0(l, '.csv')
  
  write_excel_csv(data, here('generated', year, file))
}
  

