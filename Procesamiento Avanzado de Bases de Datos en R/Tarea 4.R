#Tarea 4
#Gino Ocampo

# Usa el siguiente c?gigo para cargar la base de datos sobre Covid-19 usados en clase y seleccionar s?lo las
# variables num?ricas m?s el identificador de continente.
library("tidyverse")
library("cowplot")
library("lubridate")
library("viridis")
path <- url("https://raw.githubusercontent.com/mebucca/dar_soc4001/master/slides/class_12/covid_data.csv")
# leer archivo csv
covid_data <- read_delim(path, delim=";")
covid_data <- covid_data %>% select(continent, 
                                    total_cases_per_million, 
                                    total_deaths_per_million, 
                                    new_cases_per_million,
                                    new_deaths_per_million) 
# Los datos deben verse as?:
str(covid_data)

                                    
                                    
# Ejercicio ---------------------------------------------------------------


                                    
# 1. Usando las funciones nest() y map()del paquete purrr, para cada continente crea un matriz de
# correlaciones entre todas las variables.
# . Nota: para crear la matriz de correlaciones la funci?n map() debe tomar el siguente argumento: .f
# = ~ cor(., use = "pairwise.complete.obs"). Guarda la matriz en una nueva columna llamada
# corrs y asigna el resultado a un nuevo objecto llamado miscors.
miscors <- covid_data %>% nest(-continent) %>% 
  mutate(corrs=map(data,.f= ~ cor(., use = "pairwise.complete.obs")) )
  

# El objeto miscors debe verse as?:
miscors
                                   
# y cada matriz de correlaci?n contenida en corrs de ve as?:
miscors[[3]]                             
# 2. Continua trabajando con el objeto miscors. Usando las funciones nest() y map()del paquete purrr,
# crea nueva columna llamada mean_cor que contenga el promedio de cada matriz de correlacciones
# almacenada en la columna corrs.

miscors <- miscors %>% mutate(mean_cor= map(corrs, .f= ~mean(., na.rm=T)))


# . Nota: para el promedio de las correlaciones la funci?n map() debe tomar el siguente argumento: .f
# = ~mean(., na.rm=T). Asigna el resultado a un nuevo objecto llamado miscors. El nuevo objeto
# miscors debe verse as?:

miscors

# 3. Usando la funci?n unnest() crea un nueva base de datos que contenga solo la correlaci?n promedio
# mean_cor por continente. Asigna el resultado a un nuevo objecto llamado miscors. El nuevo objeto
# miscors debe verse as?:

miscors<- miscors %>% select(continent, mean_cor) %>% unnest()

# 4. En base a los datos almacenados en miscors crea el siguiente gr?fico usando la geometr?a
# geom_bar(stat = "identity"):

miscors %>% ggplot(aes(x= continent, y=mean_cor))+
  geom_bar(stat = "identity", fill="blue", colour= "gray")+
  theme_bw()+
  labs(title="Mi primer gr?fico :)",
         x= "Continente",
       y="Correlaci?n promedio")