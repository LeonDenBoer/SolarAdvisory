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

saldering = c(100,100,100,91,82,73,64,55,46,37,28,0)
jaren = c(2020,2021,2022,2023,2024,2025,2026,2027,2028,2029,2030,2031)
saldering_procent = data.frame(saldering, jaren)



#checkt of de pp het aantal mensen is enzo ja hoeveel KwH ze verbruiken.
check_pp <- function(pp){
  if(pp < 20){#meer dan 20 wordt er waarschijnlijk bedoeld dat dat hun hoeveelheid KwH is
    if(pp > 5){
      pp <- 5
    }
    #check wat de databases zegt over hoeveel KwH je gebruikt met deze hoeveelheid personen.
    huishoud <- huishouden[huishouden$personen == pp,]
    personen_kwh <- as.integer(huishoud$KwU)
  }else{
    personen_kwh <- pp
  }
  return <- personen_kwh
}


#bereken het kilowatt per uur aan dat een huis kan regelen
calc_kwu <- function(pc, zonnepanelen, date) {
  huis <- utrecht[utrecht$postcode == pc,]
  
  if(huis$woningtype != "Flat"){#Flat betekend meestal dat je zelf geen huis eigenaar bent en geen recht hebt
      secondes <- as.numeric((as.POSIXct(date, origin = "1970-01-01")))
      oppvervlakte <- zonnepanelen * 1.63
      cm_oppervlakte <- oppvervlakte * 1000# bereken het oppervlakte van de zonnepanelen in centimeters
      year_joule <- cm_oppervlakte * predict(modelFit, data.frame(secondes = secondes)) * 365 #Gebruik het liniear model om te bereken wat we kunnen verwacht per jaar aan joule.
      
      kw <- year_joule / 3600000
      #check het rendement met de hellingsgraad en de dak orintatie
      column <- select(opbrengt_ori, toString(round_any(huis$ori.ntatie, 10)))
      row <- which(opbrengt_ori$Hellingshoek == round_any(huis$hellingshoek, 10))
      kwu <- kw * (column[row,] /100) 
    
  }else(
    kwu <- 0
  )
  return(kwu)

}

#berekent de hoeveelheid zonnepanelen er op een huis het beste kan staan.
calc_zonnepanelen <- function(huis, pp){
  
  personen_kwh <- check_pp(pp)  
  
  if(huis$hellingshoek != 0){ #platdak heeft meer ruimte nodig voor de zonnepanelen
    max_zonnepanelen <- floor(huis$oppervlakte / 1.63)
  }else{
    
    max_zonnepanelen <- floor(huis$oppervlakte / 2.5)
    
  }
  
  zonnepanelen <- 1
  repeat{ #Voor elke zonnepaneel dat erbij komt wordt er gekeken naar de hoeveelheid KwH het opwekt.
    #Het moment dat de hoeveelheid KwH dat opgewekt wordt meer is dan de hoeveelheid die verbruikt wordt is het meest voordelig.
    kwu <- calc_kwu(huis$postcode, zonnepanelen, as.Date(Sys.Date()))
    zonnepanelen <- zonnepanelen + 1
    if (kwu > personen_kwh | zonnepanelen > max_zonnepanelen) break
  }
  return(zonnepanelen)
}




calc_rendement <- function(postcode, pp){
  Date <- Sys.Date()
  huis <- utrecht[utrecht$postcode == postcode,]
  zonnepanelen <- calc_zonnepanelen(huis, pp)
  energie <- check_pp(pp) 
  kosten <- zon_kos[which.min(abs(zonnepanelen-zon_kos$panelen)),]$kosten
  verdienen <- 0
  jaren_bezig <- 0
  kosten <- kosten - ((kosten/121) * 21)
  repeat {
    year_kwu <- calc_kwu(postcode, zonnepanelen, as.Date(Date)+jaren_bezig)
    extra <- energie - year_kwu
    if(extra < 0){#check of je meer KwH produceert dan verbruikt
      
      year <- as.integer(format(as.Date(as.Date(Date)+jaren_bezig, format="%d/%m/%Y"),"%Y"))
      saldering_afloop <- saldering_procent[which.min(abs(year-saldering_procent$jaren)),]$saldering
      
      
      
      earning <- (abs(extra) * 0.1054) * (saldering_afloop / 100)
      standaard_kosten <- energie * 0.22
      erbij <- earning + standaard_kosten
      verdienen <- verdienen + erbij
    }else{
      erbij <- year_kwu * 0.22
      verdienen <- verdienen + erbij
      
    }
    jaren_bezig <- jaren_bezig + 365
    if (kosten < verdienen) break
  }
  
  co2 <- year_kwu* 0.649
  cat("wij raden aan om ", zonnepanelen," zonnepanelen te nemen. \n")
  cat("De kosten voor ", zonnepanelen, " zonnepanelen is: ", kosten, "Euro \n")
  cat("hiermee verdien je ", as.integer(erbij), " per jaar mee. \n")
  cat("Zo veel jaar voordat je rendabel bent: ", jaren_bezig/365, "Jaar\n")
  cat("Zo veel kilo CO2 verbruik je minder per jaar: ",as.integer(co2), "Kilo\n")
}







