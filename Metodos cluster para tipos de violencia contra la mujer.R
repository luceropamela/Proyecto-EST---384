library(foreign)
library(dplyr)
library(tidyr)
library(cluster)
setwd("C:\\Users\\JULIA\\Documents\\PAMELA\\R Segundo semestre\\BASES DE DATOS\\violencia mujer")
dir()
mc<-read.spss("EPCVCM_Casadas.sav",to.data.frame = T)
mp<-read.spss("EPCVCM_Persona.sav",to.data.frame = T)
ms<-read.spss("EPCVCM_Separadas.sav",to.data.frame = T)
msol<-read.spss("EPCVCM_Solteras.sav",to.data.frame = T)


# Bases de datos de mujeres mayor o igual a 15 años de edad
bdper<-mp %>% mutate(edad=s2_03,sexo=s2_02,estado_civil=s2_10) %>% filter(sexo=="2. Mujer" & edad >=15) %>% 
  select(folio,depto,sexo,edad,estado_civil)



# BASE DE DATOS DE VIOLENCIA PSICOLOGICA
viol_psicologica<-c("S3_10_01","S3_10_02","S3_10_03","S3_10_04","S3_10_05","S3_10_06",
                    "S3_10_17")
bdc<-mc %>% select(folio,area,depto,viol_psicologica)
bdsol<-msol %>% select(folio,area,depto,viol_psicologica)
bds<-ms %>% select(folio,area,depto,viol_psicologica)

bdviol_psi<-rbind(bdc,bdsol,bds)
violencia_psicologica<-merge(bdper,bdviol_psi)
violencia_psicologica<-na.omit(violencia_psicologica)
violencia_psicologica<-violencia_psicologica %>% select(-c(1,3))
violencia_psicologica<-violencia_psicologica %>% filter(depto=="La Paz"&estado_civil=="2. CASADO/A"&edad>40&area=="Rural")
violencia_psicologica<-violencia_psicologica %>% select(-c(1,3,4))
# BASE DE DATOS VIOLENCIA ECONÓMICA
viol_econom<-c("S3_10_07","S3_10_08","S3_10_09","S3_10_10","S3_10_11")
bdce<-mc %>% select(folio,area,depto,viol_econom)
bdsole<-msol %>% select(folio,area,depto,viol_econom)
bdse<-ms %>% select(folio,area,depto,viol_econom)

bdviol_eco<-rbind(bdce,bdsole,bdse)
violencia_economica<-merge(bdper,bdviol_eco)
violencia_economica<-na.omit(violencia_economica)
violencia_economica<-violencia_economica %>% select(-c(1,3))
violencia_economica<-violencia_economica %>% filter(depto=="La Paz"&estado_civil=="2. CASADO/A"&edad>40&area=="Rural")
violencia_economica<-violencia_economica %>% select(-c(1,3,4))

# HALLANDO LAS MATRICES DE DISTANCIAS DE LAS BASES DE DATOS Y LOS MEJORES VALORES DE K

# VIOLENCIA PSICOLOGICA
dvp<-daisy(violencia_psicologica,metric = "gower")
modvp<-hclust(dvp,method = "complete")
# Definiendo cual valor de k nos conviene con el coeficiente de silueta
for (i in 2:6) {
  avp<-cutree(modvp,i)
  plot(silhouette(avp,dvp))
}
# el mejor valor de k es 2 con un coeficiente de siluetas de 0.3
plot(modvp,hang = -0.1,cex=0.8,main = "Dendograma de violencia psicológica")
rect.hclust(modvp,k=2,border = 6)


# VIOLENCIA ECONÓMICA
dve<-daisy(violencia_economica,metric = "gower")
modve<-hclust(dve,method = "complete")
# Definiendo cual valor de k nos conviene con el coeficiente de silueta
for (i in 2:6) {
  ave<-cutree(modve,i)
  plot(silhouette(ave,dve))
}
# el mejor valor de k es 2 con un coeficiente de siluetas de 0.3
plot(modve,hang = -0.1,cex=0.8,main = "Dendograma de violencia económica")
rect.hclust(modve,k=2,border = 6)



## DIVIDIENDO LOS GRUPOS CON EL MEJOR VALOR DE K DE CADA BASE DE DATOS

### VIOLENCIA PSICOLOGICA
avp<-cutree(modvp,2)
violencia_psicologica<-violencia_psicologica %>% mutate(k=avp)
grupo1vp<-violencia_psicologica %>% filter(k==1)
grupo2vp<-violencia_psicologica %>% filter(k==2)

# Hallando sus características
# Grupo 1: Violencia psicologica
v1g1<-grupo1vp %>% group_by(S3_10_01) %>% summarise(medad=mean(edad),n=n())
v2g1<-grupo1vp %>% group_by(S3_10_02) %>% summarise(medad=mean(edad),n=n())
v3g1<-grupo1vp %>% group_by(S3_10_03) %>% summarise(medad=mean(edad),n=n())
v4g1<-grupo1vp %>% group_by(S3_10_04) %>% summarise(medad=mean(edad),n=n())
v5g1<-grupo1vp %>% group_by(S3_10_05) %>% summarise(medad=mean(edad),n=n())
v6g1<-grupo1vp %>% group_by(S3_10_06) %>% summarise(medad=mean(edad),n=n())
v7g1<-grupo1vp %>% group_by(S3_10_17) %>% summarise(medad=mean(edad),n=n())
# Grupo 2: Violencia psicologica
v1g2<-grupo2vp %>% group_by(S3_10_01) %>% summarise(medad=mean(edad),n=n())
v2g2<-grupo2vp %>% group_by(S3_10_02) %>% summarise(medad=mean(edad),n=n())
v3g2<-grupo2vp %>% group_by(S3_10_03) %>% summarise(medad=mean(edad),n=n())
v4g2<-grupo2vp %>% group_by(S3_10_04) %>% summarise(medad=mean(edad),n=n())
v5g2<-grupo2vp %>% group_by(S3_10_05) %>% summarise(medad=mean(edad),n=n())
v6g2<-grupo2vp %>% group_by(S3_10_06) %>% summarise(medad=mean(edad),n=n())
v7g2<-grupo2vp %>% group_by(S3_10_17) %>% summarise(medad=mean(edad),n=n())


# VIOLENCIA ECONÓMICA
ave<-cutree(modve,2)
names(violencia_economica)
violencia_economica<-violencia_economica %>% mutate(k=ave)
grupo1ve<-violencia_economica %>% filter(k==1)
grupo2ve<-violencia_economica %>% filter(k==2)
# Hallando sus características
# Grupo 1: Violencia psicologica
v1veg1<-grupo1ve %>% group_by(S3_10_07) %>% summarise(medad=mean(edad),n=n())
v2veg1<-grupo1ve %>% group_by(S3_10_08) %>% summarise(medad=mean(edad),n=n())
v3veg1<-grupo1ve %>% group_by(S3_10_09) %>% summarise(medad=mean(edad),n=n())
v4veg1<-grupo1ve %>% group_by(S3_10_10) %>% summarise(medad=mean(edad),n=n())
v5veg1<-grupo1ve %>% group_by(S3_10_11) %>% summarise(medad=mean(edad),n=n())
# Grupo 2: Violencia psicologica
v1veg2<-grupo2ve %>% group_by(S3_10_07) %>% summarise(medad=mean(edad),n=n())
v2veg2<-grupo2ve %>% group_by(S3_10_08) %>% summarise(medad=mean(edad),n=n())
v3veg2<-grupo2ve %>% group_by(S3_10_09) %>% summarise(medad=mean(edad),n=n())
v4veg2<-grupo2ve %>% group_by(S3_10_10) %>% summarise(medad=mean(edad),n=n())
v5veg2<-grupo2ve %>% group_by(S3_10_11) %>% summarise(medad=mean(edad),n=n())


