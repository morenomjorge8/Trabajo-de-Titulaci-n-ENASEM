setwd("C:/Users/jorge.monreal/Desktop/ENASEM/dta/2015")

#Crear df3
if (!require(pacman)) install.packages("pacman") 
library(pacman)
p_load("readstata13" , #Lector STATA .DTA
       "tidyverse",    #Manipulaci√≥n de datos
       "survey",       #Marco Estadistico
       "stargazer",    #Presentaci√≥n de tablas
       "readxl",       #Excel  
       "corrplot")     #VisualizaciÛn de Correlaciones 
# "knitr","tinytex") #Knit pdf


df <- read.csv("DataFrame_Variables_brutas.csv")


#GastoSalud
df$costo_hospital <-df$imamd6_15
df$costo_curandero <-df$imamd9_1_15
df$costo_dentista <-df$imamd9_2_15
df$costo_ambulatorios <-df$imamd9_3_15
df$costo_visitas_doctor <-df$imamd9_4_15
df$costo_medicamentos <-df$imamd12a_15


#DUMMYS
df2<-df %>% 
  mutate(
    inc_earned_dummy=if_else(inc_earned_15>0,1,0),
    inc_pension_dummy=if_else(inc_pension_15>0,1,0),
    inc_trans_dummy=if_else(inc_trans_15>0,1,0),
    inc_business_dummy=if_else(inc_business_15>0,1,0),
    inc_property_dummy=if_else(inc_property_15>0,1,0),
    inc_capital_dummy=if_else(inc_capital_15>0,1,0),
    inc_family_dummy=if_else(inc_family_15>0,1,0)
  )


df3 <- df2 %>% 
  filter(edad>=50)

###HAY DOS VALORES A TIPICOS EN PENSIONES QUE TIENEN INGRESOS DE 9MILLONES DE PESOS POR PENSI”N Y
###LASTIMAN LOS RESULTADOS, LOS VOY A ELIMINAR A CUALUIQE PERSONA CON INGRESOS POR PENSI”N MAYORES A 1 MILL”N DE PESOS
df3 <- df3 %>% 
  filter(inc_pension_15<1000000)


df3<-df3%>%
mutate(
  aes_dum = aes,
  laf_dum = if_else(laf>=2,1,0),
  lavd_dum = lavd ,
  ingreso= income_15/1000
)


df3m <- df3 %>% 
  filter(sexo_mujer==1)
df3h <- df3 %>% 
  filter(sexo_mujer==0)

#Resumen
glimpse(df3)

#Correlaciones

m <- na.omit(df3[c("aes_dum","diabetes","hipertens","obes","tabaquismo","edad",
                   "edad2","escolaridad","escolaridad2","rural","n_hijos",
                   "altamigraciÛn","costo_medicamentos","ingreso")])


C1 <- abs(cor(m)) #Con esto se ordenan de mayor 
# a menor la correlacion con las variables

#Visual
m <- cor(m)
corrplot(m, method = "number",col = "black", 
         number.cex = .2, tl.cex = .4 , type="upper")

#Modelos
sd <- svydesign(id=~1, weights=~factori_15, data=df3)

#El orden ser· el siguiente: ("escolaridad2","escolaridad",
# "hipertens","diabetes","n_hijos","rural","edad","edad2",
# "tabaquismo","obes","costo_medicamentos","altamigraciÛn")


log_1 <- svyglm(aes_dum~ escolaridad2 , sd,family=binomial(link="logit"))
log_2 <- svyglm(aes_dum~ escolaridad2 + escolaridad, sd,family=binomial(link="logit"))
log_3 <- svyglm(aes_dum~ escolaridad2 + escolaridad+hipertens , sd,family=binomial(link="logit"))
log_4 <- svyglm(aes_dum~ escolaridad2  +hipertens , sd,family=binomial(link="logit"))
log_5 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes, sd,family=binomial(link="logit"))
log_6 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                  n_hijos , sd,family=binomial(link="logit"))
log_7 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                  n_hijos+rural, sd,family=binomial(link="logit"))
log_8 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                  rural, sd,family=binomial(link="logit"))
log_9 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                  rural+edad, sd,family=binomial(link="logit"))
log_10 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                  rural+edad+edad2, sd,family=binomial(link="logit"))
log_11 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                   rural+edad2+tabaquismo, sd,family=binomial(link="logit"))
log_12 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                   rural +tabaquismo+ obes, sd,family=binomial(link="logit"))
log_13 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                   rural + obes + costo_medicamentos , sd,family=binomial(link="logit"))
log_14 <- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                   rural  + costo_medicamentos , sd,family=binomial(link="logit"))
log_15<- svyglm(aes_dum~ escolaridad2  +hipertens+diabetes+
                   rural  + costo_medicamentos+altamigraciÛn, sd,family=binomial(link="logit"))

stargazer(log_1, log_2,  log_3,  log_4,  log_5, log_6, log_7, log_8, log_9, log_10,
          log_11, log_12, log_13, log_14, log_15,
          type = "latex", out="Modelos/AES_log.html")

#3tablas
stargazer(log_11, log_12, log_13, log_14, log_15,
          type = "latex", out="Modelos/AES1_log.html")



###LAF##

#Correlaciones

n <- na.omit(df3[c("laf_dum","diabetes","hipertens","obes","tabaquismo","edad",
                   "edad2","escolaridad","escolaridad2","rural","n_hijos",
                   "altamigraciÛn","costo_medicamentos","ingreso")])


C1 <- abs(cor(n)) #Con esto se ordenan de mayor 
# a menor la correlacion con las variables

# En el caso de LAF, el orden ser· el siguiente: ("edad","edad2","hipertens","escolaridad",
# "escolaridad2","diabetes","n_hijos","obes","tabaquismo","costo_medicamentos",
# "rural")


laf_log_1 <- svyglm(laf_dum~ edad , sd,family=binomial(link="logit"))
laf_log_2 <- svyglm(laf_dum~ edad + edad2 , sd,family=binomial(link="logit"))
laf_log_3 <- svyglm(laf_dum~ edad + edad2 + hipertens, sd,family=binomial(link="logit"))
laf_log_4 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad, sd,family=binomial(link="logit"))
laf_log_5 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad + escolaridad2, sd,family=binomial(link="logit"))
laf_log_6 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad2, sd,family=binomial(link="logit"))
laf_log_7 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad + diabetes, sd,family=binomial(link="logit"))
laf_log_8 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad + diabetes + n_hijos, sd,family=binomial(link="logit"))
laf_log_9 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad + diabetes + n_hijos +obes, sd,family=binomial(link="logit"))
laf_log_10 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad + diabetes +obes, sd,family=binomial(link="logit"))
laf_log_11 <- svyglm(laf_dum~ edad + edad2 + hipertens
                    + escolaridad + diabetes + obes
                    + tabaquismo, sd,family=binomial(link="logit"))
laf_log_12 <- svyglm(laf_dum~ edad + edad2 + hipertens
                     + escolaridad + diabetes + obes
                     + tabaquismo + costo_medicamentos, sd,family=binomial(link="logit"))
laf_log_13 <- svyglm(laf_dum~ edad + edad2 + hipertens
                     + escolaridad + diabetes + obes
                     + costo_medicamentos, sd,family=binomial(link="logit"))
laf_log_14 <- svyglm(laf_dum~ edad + edad2 + hipertens
                     + escolaridad + diabetes + obes
                     + costo_medicamentos + rural, sd,family=binomial(link="logit"))
laf_log_15 <- svyglm(laf_dum~ edad + edad2 + hipertens
                     + escolaridad + diabetes + obes
                     + costo_medicamentos + rural +altamigraciÛn , sd,family=binomial(link="logit"))
laf_log_16 <- svyglm(laf_dum~ edad + edad2 + hipertens
                     + escolaridad + diabetes + obes
                     + costo_medicamentos +altamigraciÛn , sd,family=binomial(link="logit"))


stargazer(laf_log_1, laf_log_2, laf_log_3, laf_log_4, laf_log_5, laf_log_6,
          laf_log_7, laf_log_8, laf_log_9, laf_log_10, laf_log_11, laf_log_12,
          laf_log_13, laf_log_14, laf_log_15,laf_log_16,
          type = "latex", out="Modelos/LAF_log.html")


stargazer(laf_log_11, laf_log_12,
          laf_log_13, laf_log_14, laf_log_15,laf_log_16,
          type = "latex", out="Modelos/LAF3_log.html")


###LAF##

#Correlaciones

e <- na.omit(df3[c("lavd_dum","diabetes","hipertens","obes","tabaquismo","edad",
                   "edad2","escolaridad","escolaridad2","rural","n_hijos",
                   "altamigraciÛn","costo_medicamentos","ingreso")])


C1 <- abs(cor(e)) #Con esto se ordenan de mayor 
# a menor la correlacion con las variables

# En el caso de LAvd, el orden ser· el siguiente: ("edad2","edad","escolaridad","escolaridad2"
# "n_hijos", "diabetes", "hipertens", "costo_medicamentos", "tabaquismo", "altamigraciÛn",
# "rural","obes")

lavd_log_1 <- svyglm(lavd_dum~ edad2 , sd,family=binomial(link="logit"))
lavd_log_2 <- svyglm(lavd_dum~ edad2 + edad , sd,family=binomial(link="logit"))
lavd_log_3 <- svyglm(lavd_dum~ edad2 + edad + escolaridad, sd,family=binomial(link="logit"))
lavd_log_4 <- svyglm(lavd_dum~ edad2 + edad + escolaridad + escolaridad2, sd,family=binomial(link="logit"))
lavd_log_5 <- svyglm(lavd_dum~ edad2 + edad + escolaridad2, sd,family=binomial(link="logit"))
lavd_log_6 <- svyglm(lavd_dum~ edad2 + edad + escolaridad + n_hijos, sd,family=binomial(link="logit"))
lavd_log_7 <- svyglm(lavd_dum~ edad2 + edad + escolaridad + n_hijos
                     + diabetes, sd,family=binomial(link="logit"))
lavd_log_8 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                     + diabetes, sd,family=binomial(link="logit"))
lavd_log_9 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                     + diabetes + hipertens, sd,family=binomial(link="logit"))
lavd_log_10 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                     + diabetes + hipertens + costo_medicamentos, sd,family=binomial(link="logit"))
lavd_log_11 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos, sd,family=binomial(link="logit"))
lavd_log_12 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos + tabaquismo, sd,family=binomial(link="logit"))
lavd_log_13 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos + tabaquismo
                      + altamigraciÛn, sd,family=binomial(link="logit"))
lavd_log_14 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos 
                      + altamigraciÛn, sd,family=binomial(link="logit"))
lavd_log_15 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos 
                      + altamigraciÛn + rural, sd,family=binomial(link="logit"))
lavd_log_16 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos 
                      + altamigraciÛn + rural+obes, sd,family=binomial(link="logit"))
lavd_log_17 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos 
                      + altamigraciÛn +obes, sd,family=binomial(link="logit"))
lavd_log_18 <- svyglm(lavd_dum~ edad2 + edad + escolaridad 
                      + diabetes  + costo_medicamentos 
                      +obes, sd,family=binomial(link="logit"))

stargazer(lavd_log_1, lavd_log_2, lavd_log_3, lavd_log_4,  lavd_log_5, lavd_log_6,  lavd_log_7,
          lavd_log_8, lavd_log_9,  lavd_log_10,  lavd_log_11,  lavd_log_12,  lavd_log_13,
          lavd_log_14,  lavd_log_15,lavd_log_16,lavd_log_17,lavd_log_18,
          type = "latex", out="Modelos/LAVD_log.html")

##partir tabla

stargazer(lavd_log_13, lavd_log_14,  lavd_log_15,lavd_log_16,lavd_log_17,lavd_log_18,
          type = "latex", out="Modelos/LAVD4_log.html")



#####Tablas, info
#Con svymean se construyeron las tablas de estadÌsticas descriptivas
#con sdm(MUJER) y sdh(Hombre) se hiciron los promedios por sexo, y con sd para la poblaciÛn general
sdm <- svydesign(id=~1, weights=~factori_15, data=df3m)
sdh <- svydesign(id=~1, weights=~factori_15, data=df3h)

svymean(~costo_medicamentos ,na.rm = T,sd)
svymean(~tabaquismo ,na.rm = T,sdh)
svymean(~tabaquismo ,na.rm = T,sdm)

summary(df3h$laf_dum)

#Tabla variables de control
stargazer(log_14, laf_log_13, lavd_log_11, type = "latex", out="Modelos/3_log.html")



####Variable en miles de pesos
df2<-df %>% 
  mutate(
    inc_earned_mil=inc_earned_15/1000,
    inc_pension_mil=inc_pension_15/1000,
    inc_trans_mil=inc_trans_15/1000,
    inc_business_mil=inc_business_15/1000,
    inc_property_mil=inc_property_15/1000,
    inc_capital_mil=inc_capital_15/1000,
    inc_family_mil=inc_family_15/1000
  )


df3 <- df2 %>% 
  filter(edad>=50)

df3 <- df3 %>% 
  filter(inc_pension_15<1000000)


df3<-df3%>%
  mutate(
    aes_dum = aes,
    laf_dum = if_else(laf>=2,1,0),
    lavd_dum = lavd ,
    ingreso= income_15/1000
  )

df3m <- df3 %>% 
  filter(sexo_mujer==1)
df3h <- df3 %>% 
  filter(sexo_mujer==0)

summary(df3[c("inc_earned_mil","inc_pension_mil","inc_trans_mil",
              "inc_business_mil","inc_property_mil","inc_capital_mil",
              "inc_family_mil","ingreso")])


sd <- svydesign(id=~1, weights=~factori_15, data=df3)
sdh <- svydesign(id=~1, weights=~factori_15, data=df3h)
sdm <- svydesign(id=~1, weights=~factori_15, data=df3m)
#AES MIL
AES_NETO_M <- svyglm(aes_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                     inc_property_mil+inc_capital_mil+inc_family_mil, sd,family=binomial(link="logit"))

AES_ROBUST_M <- svyglm(aes_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                       inc_property_mil+inc_capital_mil+inc_family_mil
                       +escolaridad2+hipertens+diabetes+rural+costo_medicamentos,
                     sd,family=binomial(link="logit"))

AES_ROBUST_H_M <- svyglm(aes_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                         inc_property_mil+inc_capital_mil+inc_family_mil+escolaridad2  
                       +hipertens+diabetes+ rural+ costo_medicamentos,
                       sdh,family=binomial(link="logit"))

AES_ROBUST_M_M <- svyglm(aes_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                         inc_property_mil+inc_capital_mil+inc_family_mil+escolaridad2  
                       +hipertens+diabetes+ rural+ costo_medicamentos,
                       sdm,family=binomial(link="logit"))

stargazer(AES_NETO_M, AES_ROBUST_M, AES_ROBUST_H_M,AES_ROBUST_M_M, type = "latex", out="Modelos/AES_F_M.html")


#TABLA FINAL LAF MIL


laf_NETO_M <- svyglm(laf_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                     inc_property_mil+inc_capital_mil+inc_family_mil, sd,family=binomial(link="logit"))

laf_ROBUST_M <- svyglm(laf_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                       inc_property_mil+inc_capital_mil+inc_family_mil
                       edad + edad2 + hipertens + diabetes
                       + escolaridad + obes + costo_medicamentos, 
                     sd,family=binomial(link="logit"))

laf_ROBUST_H_M <- svyglm(laf_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                         inc_property_mil+inc_capital_mil+inc_family_mil
                         edad + edad2 + hipertens + diabetes
                         + escolaridad + obes + costo_medicamentos, 
                       sdh,family=binomial(link="logit"))

laf_ROBUST_M_M <-svyglm(laf_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                        inc_property_mil+inc_capital_mil+inc_family_mil
                        edad + edad2 + hipertens + diabetes
                        + escolaridad + obes + costo_medicamentos,
                      sdm,family=binomial(link="logit"))

stargazer(laf_NETO_M, laf_ROBUST_M, laf_ROBUST_H_M,laf_ROBUST_M_M, type = "latex", out="Modelos/LAF_F_M.html")




#TABLA FINAL LAVD MIL

lavd_NETO_M <- svyglm(lavd_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                      inc_property_mil+inc_capital_mil+inc_family_mil, sd,family=binomial(link="logit"))

lavd_ROBUST_M <- svyglm(lavd_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                        inc_property_mil+inc_capital_mil+inc_family_mil+ 
                          edad+edad2 + escolaridad 
                        + diabetes  + costo_medicamentos  ,
                      sd,family=binomial(link="logit"))

lavd_ROBUST_H_M <- svyglm(lavd_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                          inc_property_mil+inc_capital_mil+inc_family_mil+
                            edad+edad2 + escolaridad 
                          + diabetes  + costo_medicamentos  ,
                        sdh,family=binomial(link="logit"))

lavd_ROBUST_M_M <-svyglm(lavd_dum~ inc_earned_mil+inc_pension_mil+inc_trans_mil+inc_business_mil+
                         inc_property_mil+inc_capital_mil+inc_family_mil+
                        edad+edad2 + escolaridad 
                       + diabetes  + costo_medicamentos  ,
                       sdm,family=binomial(link="logit"))

stargazer(lavd_NETO_M, lavd_ROBUST_M, lavd_ROBUST_H_M,lavd_ROBUST_M_M, type = "latex", out="Modelos/LAVD_F_M.html")

### mapa de correlaciones

m2 <- na.omit(df3[c("inc_earned_mil","inc_pension_mil","inc_trans_mil",
                     "inc_business_mil","inc_property_mil","inc_capital_mil",
                     "inc_family_mil")])



#Visual
m <- cor(m2)
corrplot(m, method = "number",col = "black", 
         number.cex = .7, tl.cex = 1 , type="upper")
corrplot.mixed(m)

