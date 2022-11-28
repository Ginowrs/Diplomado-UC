
library("tidyverse")
library("readr")
file <- url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto38/CasosFallecidosPorComuna.csv")
covid <- read_csv(file)