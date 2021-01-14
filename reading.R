library(readr)
library(readxl)
opbrengt_ori <- read_excel("data/solar_rendement.xlsx")
utrecht <- read.csv("data/huizen.csv", sep = ";")
bilt_joule <- read.delim("data/KNMI_joule_2000_2021.txt",header = TRUE, sep = ",")
bilt_joule$YYYYMMDD <- as.character(bilt_joule$YYYYMMDD)
bilt_joule$YYYYMMDD <- as.Date(bilt_joule$YYYYMMDD, format = "%Y%m%d" )

