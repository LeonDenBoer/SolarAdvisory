library(readr)
library(dplyr)
library(plyr)
library(tidyverse)
library(tidytext)

#variable
personen = c(1,2,3,4,5)
KwU = c(1850, 2860, 3400, 3930, 4180)
huishouden <- data.frame(personen, KwU)


#bereken het kilowatt per uur aan dat een huis kan regelen
calc_kwu <- function(pc, date) {
  huis <- utrecht[utrecht$postcode == pc,]
  print(huis)
  
  if(huis$woningtype != "Flat"){#Flat betekend meestal dat je zelf geen huis eigenaar bent en geen recht hebt
    
    if(huis$hellingshoek != 0){ #platdak heeft meer ruimte nodig voor de zonnepanelen
      zonnepanelen <- floor(huis$oppervlakte / 1.5)
    }else{
    
      zonnepanelen <- floor(huis$oppervlakte / 2.5)
   
    }
    
      secondes <- as.numeric((as.POSIXct(date, origin = "1970-01-01")))
      cm_oppervlakte <- zonnepanelen * 1000
      year_joule <- cm_oppervlakte * predict(modelFit, data.frame(secondes = secondes)) * 365
      kw <- year_joule / 3600000
      column <- select(opbrengt_ori, toString(round_any(huis$ori.ntatie, 5)))
      row <- which(opbrengt_ori$hellingshoek == round_any(huis$hellingshoek, 10))
      kwu <- kw * (column[row,] /100)
    
  }else(
    kwu <- 0
  )
  print("1")
  print(kwu)
  return(kwu)
  
}





calc_rendement <- function(postcode, pp){
  Date <- Sys.Date()
  huis <- utrecht[utrecht$postcode == postcode,]

  if(pp < 20){
    if(pp > 5){pp <- 5}
    huishoud <- huishouden[huishouden$personen == pp,]
    energie <- as.integer(huishoud$KwU)
  }else{
    energie <- pp
  }
  #voor het moment gaan we uit van 240 euro per m2 en 600 installatie kosten
  kosten <- huis$oppervlakte * 240 + 600
  verdienen <- 0
  jaren_bezig <- 0
  print(kosten)
  repeat {
    year_kwu <- calc_kwu(postcode, as.Date(Date)+jaren_bezig)
    print(year_kwu)
    print("2")
    print(energie)
    extra <- energie - year_kwu
    print("extra")
    print(extra)
    if(extra < 0){
      earning <- abs(extra) * 0.1054
      standaard_kosten <- energie * 0.22
      verdienen <- verdienen + earning + standaard_kosten
      print("extra 2")
      print(earning)
      print(standaard_kosten)
      print(verdienen)
    }else{
      verdienen <- verdienen + (year_kwu * 0.22)
    }
    
    
    jaren_bezig <- jaren_bezig + 365
    print("energie")
    print(as.integer(energie * 0.22))
    print(verdienen)
    print(jaren_bezig/365)
    if (kosten < verdienen) break
  }
   print(jaren_bezig/365)
  
  
}
