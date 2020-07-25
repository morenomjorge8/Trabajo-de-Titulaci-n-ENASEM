
#Script de tratamiento de datos de Enasem 2015

#Carga de paquetería a utilizar
if (!require(pacman)) install.packages("pacman") 
library(pacman)
p_load("readstata13" , #Lector STATA .DTA
       "tidyverse",    #ManipulaciÃ³n de datos
       "survey",       #Marco Estadistico
       "stargazer",    #PresentaciÃ³n de tablas
       "readxl",       #Excel  
       "corrplot")     #Visualización de Correlaciones 

setwd("C:/Users/jorge.monreal/Desktop/ENASEM") # MODIFICAR! UBICACIÓN DE LOS DATOS

#Carga de Data Frame: df
df <- read.dta13("dta/2015/sect_a_c_d_e_pc_f_h_i_2015.dta")

#En este script se pretende extraer las  siguienter varaibles de interés 
#para el trabajo de invertigación: 

# Variables de salud
# SEXO, EDAD, ESCOLARIDAD, RURAL, AltaMigración
# Número de hijos
# IMC, DAIBETES, HIPERTENSIÓN, Tabaquismo
# Ingreso, Pensión, Transferencias

#Merge y grardar .csv

### Variables de salud
##Auto Evaluación de la Salud #aes#
df <- df %>% 
  mutate(
    aes= if_else(as.integer(c1_15)<=3,0,if_else(as.integer(c1_15)<=5,1,NaN))
  )

#Revisamos la variable
df %>% 
  group_by(c1_15) %>% 
  summarise(
    n=n(),
    aes=mean(aes, na.rm = TRUE),
    edad=mean(age_15,na.rm=TRUE)
  )

##Limitaciones para hacer Actividades Fisicas #laf#
summary(df$h1_15)

df <- df %>% 
  mutate(
    h1= if_else(h1_15=="2.No",0,1), #Caminar varias cuadras
    h5= if_else(h5_15=="2.No",0,1), #Levantarse de una silla despues de un largo tiempo
    h6= if_else(h6_15=="2.No",0,1), #Subir varios pisos de escaleras
    h8= if_else(h8_15=="2.No",0,1), #Arodillarse, agacharse o ponerse de cunclillas
    h9= if_else(h9_15=="2.No",0,1), #Extender los brazos arriba de los hombros
    h11= if_else(h11_15=="2.No",0,1), #Levantar o transportar objetos  que pesan más de 5 kilos
    h12= if_else(h12_15=="2.No",0,1), #Rocojer una moneda de un peso de la mesa
    laf= h1+h5+h6+h8+h9+h11+h12
  )

#Revisamos la variable
df %>% 
  group_by(laf) %>% 
  summarise(
    n=n(),
    h1=mean(h1, na.rm = TRUE),
    h5=mean(h5, na.rm = TRUE),
    h6=mean(h6, na.rm = TRUE),
    h8=mean(h1, na.rm = TRUE),
    h9=mean(h5, na.rm = TRUE),
    h11=mean(h6, na.rm = TRUE),
    h12=mean(h12, na.rm = TRUE),
    aes=mean(aes, na.rm=TRUE),
    edad=mean(age_15,na.rm=TRUE)
  )

#Limitaciones en Actividades de la Vida Diaraia  #lavd#
summary(df$h15a_15)

df <- df %>% 
  mutate(
    h15= if_else(h15a_15=="2.No",0,1), #Caminar
    h16= if_else(h16a_15=="2.No",0,1), #Bañarse
    h17= if_else(h17a_15=="2.No",0,1), #Comer
    h18= if_else(h18a_15=="2.No",0,1), #Ir a la cama
    h19= if_else(h19a_15=="2.No",0,1), #Usar el excusado
    lavd= if_else(h15==1|h16==1|h17==1|h18==1|h19==1,1,0),
    IGNORAR= h15+h16+h17+h18+h19
  )

#Revisamos la variable
df %>% 
  group_by(IGNORAR) %>% 
  summarise(
    n=n(),
    h15=mean(h15, na.rm = TRUE),
    h16=mean(h16, na.rm = TRUE),
    h17=mean(h17, na.rm = TRUE),
    h18=mean(h18, na.rm = TRUE),
    h19=mean(h19, na.rm = TRUE),
    aes=mean(aes, na.rm=TRUE),
    laf=mean(laf, na.rm=TRUE),
    lavd=mean(lavd,na.rm=TRUE)
  )


###SEXO, EDAD, ESCOLARIDAD, RURAL, AltaMigración
df$age_15[df$age_15 == 999] <- NA
df$age_15[df$age_15 == 888] <- NA

df <- df %>% 
  mutate(
    edad= age_15,
    edad2= age_15 * age_15,
    sexo_mujer= as.integer(sex_15)-1,
    escolaridad= yrschool,
    escolaridad2= yrschool * yrschool,
    rural= if_else(tam_loc_15=="1.Population = 100,000+",0,1),
    altamigración=eam_15
  )

#Revisamos las variables
df %>% 
  group_by(sexo_mujer) %>% 
  summarise(
    n=n(),
    edad=mean(edad, na.rm = TRUE),
    edad2=mean(edad2, na.rm = TRUE),
    escolaridad=mean(escolaridad,na.rm=TRUE),
    escolaridad2=mean(escolaridad2,na.rm=TRUE),
    rural=mean(rural,na.rm=TRUE),
    altamigración=mean(altamigración,na.rm=TRUE)
  )


###Número de hijos
summary(as.factor(df$a7_2_15))
df$a7_2_15[df$a7_2_15 == 99] <- NA
df$a7_2_15[df$a7_2_15 == 88] <- NA
summary(df$a7_2_15) #¿Cuántos hijos /hijas que nacieron vivos ha tenido usted?


df <- df %>% 
  mutate(
    n_hijos= a7_2_15,
  )

#Revisamos las variables
df %>% 
  group_by(rural) %>% 
  summarise(
    n=n(),
    Promedio_de_hijos=mean(n_hijos, na.rm = TRUE)
  )

#IMC, DAIBETES, HIPERTENSIÓN, Tabaquismo
##IMC (peso/altura^2)
summary(as.factor(df$c66_15))
df$c66_15[df$c66_15 == 999] <- NA
df$c66_15[df$c66_15 == 888] <- NA
summary(as.factor(df$c66_15))
summary(df$c66_15) #¿Como cuántos kilos pesa usted ahora?

summary(as.factor(df$c67_1_15)) #¿Como cuánto mide usted sin zapatos? – Metros
summary(as.factor(df$c67_2_15)) #¿Como cuánto mide usted sin zapatos? – Centímetros

df$c67_1_15[df$c67_1_15== 999] <- NA
df$c67_1_15[df$c67_1_15 == 888] <- NA
df$c67_2_15[df$c67_2_15== 999] <- NA
df$c67_2_15[df$c67_2_15 == 888] <- NA


df<-df %>% mutate(
  talla_cm_total=  (100) + c67_2_15, #Centímetros totales
  kilos= c66_15,
  imc= kilos/(talla_cm_total*talla_cm_total/10000),
  obes= if_else(imc >= 30,1,0)
)

#DAIBETES, HIPERTENSIÓN
summary(df$c6_15) #¿Alguna vez le ha dicho un doctor o personal
                  # médico que usted tiene diabetes?
df$c6_15[df$c6_15== "9.DK"] <- NA
df$c6_15[df$c6_15 == "8.RF"] <- NA

summary(df$c4_15) #¿Alguna vez le ha dicho un doctor o personal
                  # médico que usted tiene hipertensión o presión alta?
df$c4_15[df$c4_15== "9.DK"] <- NA
df$c4_15[df$c4_15 == "8.RF"] <- NA

df<-df %>% mutate(
  diabetes=if_else(c6_15=="1.Yes",1,0),
  hipertens=if_else(c4_15=="1.Yes",1,0)
)

#Tabaquismo
#¿Fuma cigarros actualmente? - c54_15
summary(df$c54_15)
df$c54_15[is.na(df$c54_15)] <- "2.No"


df<-df %>% mutate(
  tabaquismo=if_else(c54_15=="1.Yes",1,0)
)


#Revisamos las variables ----
df %>% 
  group_by(obes) %>% 
  summarise(
    n(),
    aes=mean(aes, na.rm = TRUE),
    laf=mean(laf, na.rm = TRUE),
    lavd=mean(lavd, na.rm = TRUE),
    imc=mean(imc, na.rm = TRUE),
    )

df %>% 
  group_by(diabetes) %>% 
  summarise(
    n(),
    aes=mean(aes, na.rm = TRUE),
    laf=mean(laf, na.rm = TRUE),
    lavd=mean(lavd, na.rm = TRUE),
    imc=mean(imc, na.rm = TRUE)
  )

df %>% 
  group_by(hipertens) %>% 
  summarise(
    n(),
    aes=mean(aes, na.rm = TRUE),
    laf=mean(laf, na.rm = TRUE),
    lavd=mean(lavd, na.rm = TRUE),
    imc=mean(imc, na.rm = TRUE)
  )

df %>% 
  group_by(tabaquismo) %>% 
  summarise(
    n(),
    aes=mean(aes, na.rm = TRUE),
    laf=mean(laf, na.rm = TRUE),
    lavd=mean(lavd, na.rm = TRUE),
    imc=mean(imc, na.rm = TRUE)
  )

#----

# Ingreso, Gasto, Pensión, Transferencias
df_income <- read.dta13("dta/2015/Income_2015.dta")

df$subhog_15 <- as.numeric(df$subhog_15)
df <- left_join(df,df_income,by = c("cunicah", "subhog_15", "np"))

glimpse(df)

df <- df[c("cunicah","np","subhog_15","factori_15","aes","laf","lavd",
           "tabaquismo","hipertens","diabetes","obes","imc","kilos","n_hijos",
           "altamigración","rural","escolaridad2","escolaridad","sexo_mujer",
           "edad2","edad","IGNORAR",
           "h1","h5","h6","h8","h9","h11","h12","h15","h16","h17","h18","h19")]


write.csv(df,"resultados/DataFrame_Variables_brutas.csv")



##########intentar
############intentar
#Gasto en Hospitales, medicinas, dentista, etc
df_gasto <- read.dta13("dta/2015/Sect_D_imputed_2015.dta") 
summary(as.factor(df_gasto$np))
summary(as.factor(df$np))

df_gasto$np2 <- df_gasto$np
df$np2 <- df$np


df2 <- left_join(df,df_gasto,by = c("cunicah", "subhog_15", "np2"))
