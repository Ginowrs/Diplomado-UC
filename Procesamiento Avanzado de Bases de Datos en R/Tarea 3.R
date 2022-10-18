#Tarea 3
#Gino Ocampo

library("tidyverse")
library("readr")

path <- url("https://raw.githubusercontent.com/ANID-GITHUB/Historico-de-Proyectos-Adjudicados/da63cab4fa424eaaac2a108b0582c83fa4f229c1/BDH_Proyectos.csv")

data_anid <- read_delim(path, delim = ";")

data_anid <- data_anid %>% rename(codigo_proyecto = CODIGO_PROYECTO, anno = ANO_FALLO, sexo = SEXO, area = AREA_OCDE, duracion =  DURACION_MESES, monto = `MONTO ADJUDICADO (MILES $)`) %>% select(codigo_proyecto,anno, sexo,area,monto)

# 1) Usando los comandos `group_by()` y `summarise()` produce la siguiente tabla y asígnala al objeto `tabla_1`

tabla_1<- data_anid %>%  
  group_by(area,anno,sexo) %>%
  summarise(monto= round(mean(monto, na.rm=T),0)) 

# 2) Carga la base de datos con el IPC anual y guárdala en un objecto llamado `datos_ipc`. 
#Para los años con valores perdidos en la variable `datos_ipc$ipc`, usa la función `fill()` para asignales el valor correspondiente al año siguiente. Conserva sólo las variables `anno` e `ipc`.
setwd("C:/Users/Gino/Desktop/Nueva carpeta/dar_soc4001/homework")
datos_ipc <- read.csv("ipc.csv")

datos_ipc<-datos_ipc[,-1] %>% 
  complete(anno) %>% 
  fill(ipc,.direction = c("up")) %>% 
  summarise(anno,ipc=round(ipc,2))
  
# 3) Usando algunos de los comandos `_join()` junta los datos en `tabla_1` y `datos_ipc` preservando toda la información disponible en `tabla_1`.

tabla_1 <- tabla_1 %>% left_join(datos_ipc, by="anno")
  
# 4) Crea la nueva variable `monto_precios2021` multiplicando las variables `monto` e `ipc`. Posteriormente remueve las variables `monto` e `ipc`.

tabla_1 <- tabla_1  %>% mutate(monto_precios2021=monto*ipc)%>% select(!c(monto,ipc))
  
# 5) Usando el comando `pivot_wider()` transforma los datos de la siguiente manera.

tabla_1 <- tabla_1  %>% pivot_wider(names_from = sexo, values_from = monto_precios2021)

# 6)  Usa la función `replace_na()` para reemplazar los valores perdidos en las variables `HOMBRE` y `MUJER` por ceros.

tabla_1 <- tabla_1  %>% replace_na(list(HOMBRE=0,
                             MUJER=0))

# 7) Crea una nueva variable llamada `dif_hombremujer` que mida la diferencia entre el monto asignado a hombres y mujeres ` = HOMBRE - MUJER`. Posteriormente conserva sólo las variables `anno`, `area` y `dif_hombremujer`.

tabla_1 <- tabla_1  %>% mutate(dif_hombremujer=HOMBRE - MUJER )%>% select(c(anno,area, dif_hombremujer))

# 8) Usando el comando `pivot_wider()` modifica la tabla producida en (7) y produce la siguiente tabla

tabla_1 <- tabla_1  %>% pivot_wider(names_from = area, values_from = dif_hombremujer)

# 9) Elige el valor correspondiente a una celda cualquiera y describe la información que comunica.

tabla_1[39,5]#fila 39 corresponde al año 2020 y columna 5 al área de ciencias sociales.
# A tibble: 1 x 1
# CIENCIAS SOCIALES
# <dbl>
#   1       1057.
# Durante el año 2020 en el área de Ciencias Sociales, la diferencia del monto asignado para proyectos Fondecyt entre hombres y mujeres fue de 1057 (MILES $) a precios del ipc 2021.

