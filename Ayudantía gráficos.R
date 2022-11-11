library("tidyverse")

file <- url("https://github.com/mebucca/dar_soc4001/blob/master/slides/class_6/sample_casen2017.csv?raw=true")
data_casen2017 <- read_csv(file)

#1 Haz el siguiente gráfico usando geom_histogram():
str(data_casen2017)

data_casen2017 %>% filter(edad >18 & edad< 65 & sexo==1) %>% 
  ggplot(aes(x= yautcor/1000))+
  geom_histogram(colour= "black", fill="green")+
  labs(title="Distribución del ingreso autónomo en Chile",
       subtitle ="Hombres entre 18 y 65 años" ,
       x="Ingreso autónomo corregido (miles de peso)",
       y= "Frecuencia")

# 2. Haz el siguiente gráfico. Prueba primero usando geom_point() y luego trata con geom_jitter():
data_casen2017 %>% filter(edad>18 & edad<65) %>% 
  mutate(sexo_t=case_when(sexo==1~"Hombre",sexo==2~"Mujer")) %>% 
  ggplot(aes(x=esc,y=log(yautcor),group=sexo_t, colour=sexo_t))+
  #geom_point(alpha=0.3)+
  geom_jitter(alpha=0.2)+
  labs(title = "Relación entre educación e ingresos",
       subtitle = "individuos entre 18 y 65 años",
       x="años de escolaridad",
       y="Log autónomo corregido",
       colour="sexo")

file <- url("https://raw.githubusercontent.com/mebucca/dar_soc4001/master/slides/class_11/aus_weather.csv")
clima_australia <- read_csv(file)

clima_australia %>% glimpse()

#Haz el siguiente gráfico usando geom_line() y geom_point():

str(clima_australia)

clima_australia %>% group_by(Year) %>% 
  mutate(daily_avg= if_else(daily_avg==0,NA_real_,daily_avg )) %>% 
  summarise(temperatura= mean(daily_avg, na.rm=T)) %>% 
  ggplot(aes(x= Year ,y=temperatura))+
  geom_line(colour="blue")+
  geom_point(alpha= 0.5, colour="red")+
  labs(title = "Promedio de temperatura anual",
       x= "Año",
       y= " Temperatura (Celcius)")

# 4.Usa el siguiente código para transformar crear una variable de fecha a partir de las variables Year, Month, Day.
str(clima_australia)  
library("lubridate")
clima_australia <- clima_australia %>% 
  mutate(fecha = make_datetime(Year, Month, Day))  
  
ggplot(clima_australia,aes(x=fecha, y=rainfall_mm))+
  geom_line()


