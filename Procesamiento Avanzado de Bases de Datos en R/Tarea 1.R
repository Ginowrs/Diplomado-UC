###############################TAREA 1##########################################

#Gino Ocampo

#1. Instalar y cargar el paquete CarData
install.packages("carData")
library("carData")

#2. Usa la documentación del paquete carData para identificar los datos correspondientes a "Self-Report of Height and Weight"
?carData::Davis

#3. Carga los datos y crea un objeto que los contenga. Llama a tal objeto "datos_davis"
datos_davis<-Davis

#4. Muestra las primeras y últimas observaciones de la base de datos en la consola.
head(datos_davis)
tail(datos_davis)

#5. Crea una base de datos que contenga sólo las variables sex, height y repht de "datos_davis". Llama a tal objeto
#"subdatos_davis". Muestra las dimensiones de la nueva base de datos.

subdatos_davis <- datos_davis[,c('sex', 'height', 'repht')]
dim(subdatos_davis)
#200 3

#6. Presenta un resumen estadístico de las variables en "subdatos_davis"
summary(subdatos_davis)

#7. Crea una variable llamada "ratio" que mida razón (división) entre la altura real (height) y la altura reportada (repht) 
#por los individuos y añadela a subdatos_davis.

subdatos_davis$ratio <- subdatos_davis$height/subdatos_davis$repht

#8. Chequea la presencia de datos perdidos en la variable "ratio". Luego crea una nueva base de datos 
#que contenga sólo las observaciones con datos completos en todas las variables en "subdatos_davis". 
#Llama a este objeto "subdatos_davis_full" y presenta un resumen estadístico de las variables.

sum(is.na(subdatos_davis$ratio)) # 17 casos perdidos

subdatos_davis_full<- na.omit(subdatos_davis)# Quitar perdidos

summary(subdatos_davis_full)#Resumen estadístico

# sex         height        repht           ratio       
# F:101   Min.   : 57   Min.   :148.0   Min.   :0.3497  
# M: 82   1st Qu.:164   1st Qu.:160.5   1st Qu.:1.0055  
# Median :169   Median :168.0   Median :1.0127  
# Mean   :170   Mean   :168.5   Mean   :1.0089  
# 3rd Qu.:178   3rd Qu.:175.0   3rd Qu.:1.0188  
# Max.   :197   Max.   :200.0   Max.   :1.0667 

#9. Crea una nueva variable llamada "sex_num". 
# Asigna valor 1 a "sex_num" para aquellas observaciones en las cuales la variable "sex" toma el valor "F" mujer. 
# Asigna 0 a "sex_num" para aquellas observaciones en las cuales la variable "sex"  toma un valor "M" hombre.

subdatos_davis_full$sex_num[subdatos_davis_full$sex == "F"] <- 1
subdatos_davis_full$sex_num[subdatos_davis_full$sex == "M"] <- 0

#10. Usa un loop para calcular la media de la variable "ratio" para las observaciones en cada uno de los 
# niveles de la variable "sex" (es decir para hombres y mujeres). No olvides usar el comando print()
# para mostrar los cálculas ejecutados dentro del loop.


sex <- unique(subdatos_davis_full$sex_num)

mean_ratio <- matrix(NA, ncol = length(sex)) 

col=1
for(i in sex){
  mean_ratio[1,col] <- mean(subdatos_davis_full$ratio[subdatos_davis_full$sex_num==i], na.rm = T)
  col = col + 1
}
print(paste("media del ratio (altura/altura reportada) entre hombres",mean_ratio[2]))
print(paste("media del ratio (altura/altura reportada) entre mujeres",mean_ratio[1]))
