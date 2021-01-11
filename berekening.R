library(readr)
library(dplyr)
library(plyr)
library(tidyverse)
library(tidytext)

#variable
personen = c(1,2,3,4,5)
KwU = c(1850, 2860, 3400, 3930, 4180)
huishouden <- data.frame(personen, KwU)

panelen = c(11,16,20,22,25,26)
kosten_p = c(5100,7600,9100,10200,11700,12000)
zon_kos = data.frame(panelen,kosten_p)



#bereken het kilowatt per uur aan dat een huis kan regelen
calc_kwu <- function(pc, zonnepanelen, date) {
  huis <- utrecht[utrecht$postcode == pc,]
  
  if(huis$woningtype != "Flat"){#Flat betekend meestal dat je zelf geen huis eigenaar bent en geen recht hebt
    

    
  
      secondes <- as.numeric((as.POSIXct(date, origin = "1970-01-01")))
      oppvervlakte <- zonnepanelen * 1.63
      cm_oppervlakte <- oppvervlakte * 1000
      year_joule <- cm_oppervlakte * predict(modelFit, data.frame(secondes = secondes)) * 365
      
      kw <- year_joule / 3600000
      column <- select(opbrengt_ori, toString(round_any(huis$ori.ntatie, 10)))
      row <- which(opbrengt_ori$hellingshoek == round_any(huis$hellingshoek, 10))
      kwu <- kw * (column[row,] /100)
      print(zonnepanelen)
    
  }else(
    kwu <- 0
  )
  print("1")
  return(kwu)

  
}





calc_rendement <- function(postcode, pp){
  Date <- Sys.Date()
  huis <- utrecht[utrecht$postcode == postcode,]
  zonnepanelen <- calc_zonnepanelen(huis, pp)
  if(pp < 20){
    if(pp > 5){pp <- 5}
    huishoud <- huishouden[huishouden$personen == pp,]
    energie <- as.integer(huishoud$KwU)
  }else{
    energie <- pp
  }
  #voor het moment gaan we uit van 240 euro per m2 en 600 installatie kosten
  kosten <- zon_kos[which.min(abs(zonnepanelen-zon_kos$panelen)),]$kosten
  verdienen <- 0
  jaren_bezig <- 0
  print("kosten")
  print(kosten)
  repeat {
    year_kwu <- calc_kwu(postcode, zonnepanelen, as.Date(Date)+jaren_bezig)
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
  
  co2 <- year_kwu* 0.649
  print(co2)
  print(jaren_bezig/365)
  
  
}

calc_zonnepanelen <- function(huis, pp){
  kwu <- 1000000
  
  if(pp < 20){
    if(pp > 5){pp <- 5}
    huishoud <- huishouden[huishouden$personen == pp,]
    personen_kwh <- as.integer(huishoud$KwU)
  }else{
    personen_kwh <- pp
  }
  if(huis$hellingshoek != 0){ #platdak heeft meer ruimte nodig voor de zonnepanelen
    zonnepanelen <- floor(huis$oppervlakte / 1.63)
  }else{
  
    zonnepanelen <- floor(huis$oppervlakte / 2.5)
  
  }
  
  zonnepanelen <- zonnepanelen + 1
  rotaties <- 0
  repeat{
    rotaties <- rotaties + 1
   
    if (kwu < personen_kwh) break
    
    
    kwu <- calc_kwu(huis$postcode, zonnepanelen, as.Date(Sys.Date()))
    print(zonnepanelen)
    zonnepanelen <- zonnepanelen - 1
  }
  if(rotaties = 1)
  
  return(zonnepanelen)
}



