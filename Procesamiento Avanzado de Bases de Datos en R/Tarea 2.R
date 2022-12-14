# Formato: Desarrollar esta tarea en un RScript, agregando comentarios cuando sea necesario. 
# Gino Ocampo
# 
# 1) Carga la base de datos "Chile" del paquete `carData` y crea un objeto que los contenga los datos. Llama tal objeto "datos_chile".  Carga la librer?a `tidyverse` y ejecuta la siguientes operaciones usando las herramientas contenidas de `tidyverse`:
library(carData)
datos_chile <- carData::Chile
library(tidyverse)
# 2) A?ade a "datos_chile" un variable llamada "year" con valor 1988 en todas las filas
datos_chile$year<-1988
# 3) Calcula el a?o de nacimiento de cada individuo. A?ade a "datos_chile" un variable llamada "birthyear" que contenga esta informaci?n
datos_chile$birthyear<- datos_chile$year - datos_chile$age
# 4) Usando la funci?n `if_else()` a?ade a "datos_chile" un variable llamada "vote_no" que tome valor 1 si la persona declara que votar? por el No y valor 0 en cualquier otra caso. 
datos_chile$vote_no<- if_else(datos_chile$vote=="N",1,0)  
# 5) Usando la funci?n `case_when()` a?ade a "datos_chile" un variable llamada "cohort73" que tome valor 1 si la persona ten?a 18 a?o o m?s el a?o del golpe de estado (1973) y valor 0 si ten?a menos de 18. Trata las observaciones que no cumplan ninguna de estas condiciones como valores perdidos. 
#1973-18=1955
min(datos_chile$birthyear,na.rm=T)
datos_chile$cohort73 <- case_when(datos_chile$birthyear <= 1955 ~ 1,
                                  TRUE ~ 0)
# 6) Usando la funci?n `group_by()` a?ade a "datos_chile" un variable llamada "no_by_groups" que contenga el promedio de la variable "vote_no" por regi?n, nivel educacional y cohorte (cohort73). 
datos_chile <- datos_chile %>% group_by(region,education,cohort73) %>% 
  mutate(no_by_groups= mean(vote_no, na.rm= TRUE))
  
# 7) Usando la funciones `summarise()` y  `group_by()`  calcula el promedio de la variable "vote_no" por regi?n, nivel educacional y cohorte (cohort73) y almac?nalo en una variable llamada "datos_chile". Asigna el resultado a un nuevo objeto llamado `resultados`.
resultados <- datos_chile %>% 
  group_by(region, education,cohort73) %>% 
  summarise(datos_chile = mean(vote_no, na.rm= TRUE))

# 8) Usando la funciones `summarise()`, `across())`  `group_by()`  calcula el promedio y la deviaci?n est?ndar de las variables "vote_no" e "income" por regi?n, nivel educacional y cohorte (cohort73). Asigna el resultado a un nuevo objeto llamado `resultados`.
resultados <- datos_chile %>% 
  group_by(region, education,cohort73) %>% 
  summarise(across(c("vote_no","income"), list(media = ~mean(.x, na.rm = TRUE),desviacion_standar= ~sd(.x, na.rm = TRUE))))
