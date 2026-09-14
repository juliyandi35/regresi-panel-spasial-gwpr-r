#Package Uji Asumsi
library(lmtest)
library(MASS)
library(car) #Uji Multikol
library(plm) #Analisis Data Panel
#Package Analisis Spasial
library(TH.data)
library(GWmodel) #geographically weighted model#
library(sp)
library(spdep) #untuk pembobotan#
library(spgwr) #geographically weighted regression (GWR)#
#Package Pemetaan
library(plotly)
library(sf)
library(ggplot2)
library(ggpubr)
library(rgdal)
library(indonesia) # Download data peta provinsi Indonesia
#Package Penggabungan Data
library(tidyr)
library(dplyr)
library(readxl)

#IMPORT DATA
Data.mapping=read_excel("Data_Panel_PDRBIDN.xlsx")
Data.Panel=read_excel("Data_Panel_PDRB_Indonesia.xlsx")
within.trans=read_excel("Data_GWR_PANEL_PDRB.xlsx")

#--------------Eksploarasi Data Aktual dengan Pemetaan-----------------#
#----------------------------------------------------------------------#
##IMPORT PETA SHP
shp.IDN=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN=left_join(shp.IDN,Data.mapping,by="OBJECTID")
View(gabung.IDN)

#Pemetaan PDRB dalam 6 TAHUN
varY <- gabung.IDN %>% select("PDRB_2017","PDRB_2018","PDRB_2019","PDRB_2020","PDRB_2021","PDRB_2022",geometry) %>%
  gather(VAR, PDRB, -geometry)%>%
  mutate(PDRB.INDO = cut_number(PDRB, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = PDRB.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 3) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan IKA dalam 6 TAHUN
varY <- gabung.IDN %>% select("IKA_2017","IKA_2018","IKA_2019","IKA_2020","IKA_2021","IKA_2022",geometry) %>%
  gather(VAR, IKA, -geometry)%>%
  mutate(IKA.INDO = cut_number(IKA, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = IKA.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan IKU dalam 6 TAHUN
varY <- gabung.IDN %>% select("IKU_2017","IKU_2018","IKU_2019","IKU_2020","IKU_2021","IKU_2022",geometry) %>%
  gather(VAR, IKU, -geometry)%>%
  mutate(IKU.INDO = cut_number(IKU, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = IKU.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan IKTL dalam 6 TAHUN
varY <- gabung.IDN %>% select("IKTL_2017","IKTL_2018","IKTL_2019","IKTL_2020","IKTL_2021","IKTL_2022",geometry) %>%
  gather(VAR, IKTL, -geometry)%>%
  mutate(IKTL.INDO = cut_number(IKTL, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = IKTL.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan INV dalam 6 TAHUN
varY <- gabung.IDN %>% select("INV_2017","INV_2018","INV_2019","INV_2020","INV_2021","INV_2022",geometry) %>%
  gather(VAR, INV, -geometry)%>%
  mutate(INV.INDO = cut_number(INV, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = INV.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan HDI dalam 6 TAHUN
varY <- gabung.IDN %>% select("HDI_2017","HDI_2018","HDI_2019","HDI_2020","HDI_2021","HDI_2022",geometry) %>%
  gather(VAR, HDI, -geometry)%>%
  mutate(HDI.INDO = cut_number(HDI, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = HDI.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#Pemetaan JP dalam 6 TAHUN
varY <- gabung.IDN %>% select("JP_2017","JP_2018","JP_2019","JP_2020","JP_2021","JP_2022",geometry) %>%
  gather(VAR, JP, -geometry)%>%
  mutate(JP.INDO = cut_number(JP, n = 5,dig.lab=5 ))

ggplot(data = varY, aes(fill = JP.INDO)) +
  geom_sf() +
  facet_wrap(~VAR, ncol = 2) +
  scale_fill_brewer(type = "seq", palette = "YlGnBu")

#----------------------------------------------------------------------#
#                        UJI EFEK SPASIAL
#----------------------------------------------------------------------#
#Uji pengaruh waktu
plmtest(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,
        data=Data.Panel,type="bp",effect = "time",index = c("No","Tahun"))
#Uji pengaruh Lokasi
plmtest(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Data.Panel,type="bp",effect = "individual",index = c("No","Tahun"))
#Uji pengaruh gabungan
plmtest(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Data.Panel,type="bp",effect = "twoways",index = c("No","Tahun"))

#----------------------------------------------------------------------------
### MODEL PLS, FIXED, dan RANDOM
#----------------------------------------------------------------------------
#Model Random (REM)
modelpanel1<-plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Data.Panel,model="random",
                 index = c("No","Tahun"))
summary(modelpanel1)

##Model Fixed (FEM)
modelpanel2<-plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP, data=Data.Panel,model="within",
                 index = c("No","Tahun"))
summary(modelpanel2)

#Model OLS
modelpanel3<-plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Data.Panel,model="pooling",
                 index = c("No","Tahun"))
summary(modelpanel3)


#----------------------------------------------------------
#                      UJI PEMILIHAN MODEL
#----------------------------------------------------------
### uji chow ### (membandingkan OLS dengan FEM)
pFtest(modelpanel2,modelpanel3)

### uji hausman ### (membandingkan FEM dengan REM)
phtest(modelpanel2,modelpanel1)

##Dengan anggapan bahwa model adalah FEM sehingga uji BP_Test langsung modelpanel1
#Uji Keragaman Spasial
bptest(modelpanel1,studentize = FALSE)

#------------------------------------------------------------------------#
#                              GWR PANEL
#------------------------------------------------------------------------#
#----------------------------------------------------------------------------
#PEMODELAN GWR PANEL
#Merubah data ke Spasial Titik Data Frame
data.sp.GWPR=within.trans
coordinates(data.sp.GWPR)=4:5 #kolom 4 dan 5 menyatakan letak Long-Lat
class(data.sp.GWPR)
head(data.sp.GWPR)

bwd.GWPRGAUS<-bw.gwr(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = data.sp.GWPR, approach = "CV",kernel = "gaussian",adaptive = T)
bwd.GWPRBISQUR<-bw.gwr(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = data.sp.GWPR, approach = "CV",kernel = "bisquare",adaptive = T)
bwd.GWPREXPO<-bw.gwr(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = data.sp.GWPR, approach = "CV",kernel = "exponential",adaptive = T)

hasil.GWPRGAUS<-gwr.basic(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = data.sp.GWPR,bw=bwd.GWPRGAUS, kernel = "gaussian",adaptive = T)
summary(hasil.GWPRGAUS)
hasil.GWPRBISQUR<-gwr.basic(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = data.sp.GWPR,bw=bwd.GWPRBISQUR, kernel = "bisquare",adaptive = T)
summary(hasil.GWPRBISQUR)
hasil.GWPREXPO<-gwr.basic(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = data.sp.GWPR,bw=bwd.GWPREXPO, kernel = "exponential",adaptive = T)
summary(hasil.GWPREXPO)

cbind(hasil.GWPRGAUS$GW.diagnostic,
      hasil.GWPRBISQUR$GW.diagnostic,
      hasil.GWPREXPO$GW.diagnostic)

#----------------------------------------------------------------------------
# penduga parameter
#Estimasi Model GWPR Lokal (Provinsi)
longlat <- cbind(Data.Panel$LONG[1:34], Data.Panel$LAT[1:34])
Lokasi_Bandwidth <- gw.adapt(dp = longlat, fp = longlat,
                             quant = hasil.GWPRBISQUR$GW.arguments$bw/204) #204 jumlah data

#Nilai Estimasi Parameter, Std. Error dan T-Value Tiap Provinsi
Parameter_GWPR <- as.data.frame(hasil.GWPRBISQUR$SDF)
Parameter_GWPR[1:34,]
writexl::write_xlsx(Parameter_GWPR,"Hasil_Parameter_GWPR.xlsx")

#Nilai P-Value Masing-Masing Parameter Tiap Provinsi
PValue_GWPR <- as.data.frame(gwr.t.adjust(hasil.GWPRBISQUR)$results$p)
PValue_GWPR[1:34,]
writexl::write_xlsx(PValue_GWPR,"Hasil_PValue_GWPR.xlsx")

#Nilai R-Squared Dari Model Tiap Provinsi
RLocal_GWPR <- as.data.frame(hasil.GWPRBISQUR$SDF$Local_R2)
RLocal_GWPR[1:34,]
writexl::write_xlsx(RLocal_GWPR,"Hasil_Rlocal_GWPR.xlsx")

#Pembuatan jarak euclidean
n <- 34 #jumlah wilayah
U <- Data.Panel$LONG #data Longitude
V <- Data.Panel$LAT #data Latitude
d <- matrix(0,n,n)
for (i in 1:n) {
  for (j in 1:n) {
    d[i,j] <- sqrt(((U[i]-U[j])^2)+((U[i]-U[j])^2))
  }
}
d
writexl::write_xlsx(RLocal_GWPR,"Hasil_Jarak_Euclidean.xlsx")

#Menentukan Bobot Penimbang
bdwtBisquare=ggwr.sel(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = Data.Panel,
                      coords=cbind(Data.Panel$LAT,Data.Panel$LONG),adapt=TRUE,gweight=gwr.bisquare)
GRTGB=ggwr(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data = Data.Panel,
           coords=cbind(Data.Panel$LAT,Data.Panel$LONG),adapt = bdwtBisquare,gweight=gwr.bisquare)
GRTGB$bandwidth

bdwtBisquare<- GRTGB$bandwidth
bdwtBisquare<- as.matrix(bdwtBisquare)
bdwtBisquare
i<-nrow(bdwtBisquare)
pembobotB<-matrix(nrow=34,ncol=34)
for(i in 1:34){
  for(j in 1:34){
    pembobotB[i,j]=(1-(d[i,j]/bdwtBisquare[i,])**2)**2
    pembobotB[i,j]<-
      ifelse(d[i,j]<bdwtBisquare[i,],pembobotB[i,j],0)}
}


pembobotB

#----------------------------------------------------------------------------
#Export Hasil analisis GWPR ke Excel
OBJECTID=within.trans$OBJECTID
output.GWPR=as.data.frame(cbind(OBJECTID,Parameter_GWPR,PValue_GWPR,RLocal_GWPR))
writexl::write_xlsx(output.GWPR,"hasil GWR Panel Terbaik.xlsx")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel IKA)
#---------------------------------------------------------------#
output.GWPR$signfikansi_IKA <- NA

# Signifikan
output.GWPR[(output.GWPR$IKA_p <= 0.05), "signfikansi_IKA"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$IKA_p >= 0.05), "signfikansi_IKA"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
#IMPORT PETA SHP
shp.IDN2=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")

#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")

ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_IKA)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi IKA")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel IKU)
#---------------------------------------------------------------#
output.GWPR$signfikansi_IKU <- NA

# Signifikan
output.GWPR[(output.GWPR$IKU_p <= 0.05), "signfikansi_IKU"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$IKU_p >= 0.05), "signfikansi_IKU"] <- "Tidak Signifikan"

#------------------------------------------------
#Menggabungkan Data ke file SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")

ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_IKU)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi IKU")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel IKTL)
#---------------------------------------------------------------#
output.GWPR$signfikansi_IKTL <- NA

# Signifikan
output.GWPR[(output.GWPR$IKTL_p <= 0.05), "signfikansi_IKTL"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$IKTL_p >= 0.05), "signfikansi_IKTL"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")

ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_IKTL)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi IKTL")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel INV)
#---------------------------------------------------------------#
output.GWPR$signfikansi_INV <- NA

# Signifikan
output.GWPR[(output.GWPR$INV_p <= 0.05), "signfikansi_INV"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$INV_p >= 0.05), "signfikansi_INV"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")

ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_INV)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi INV")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel HDI)
#---------------------------------------------------------------#
output.GWPR$signfikansi_HDI <- NA

# Signifikan
output.GWPR[(output.GWPR$HDI_p <= 0.05), "signfikansi_HDI"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$HDI_p >= 0.05), "signfikansi_HDI"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")

ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_HDI)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi HDI")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel JP)
#---------------------------------------------------------------#
output.GWPR$signfikansi_JP <- NA

# Signifikan
output.GWPR[(output.GWPR$JP_p <= 0.05), "signfikansi_JP"] <- "Signifikan"
# Tidak Signifikan
output.GWPR[(output.GWPR$JP_p >= 0.05), "signfikansi_JP"] <- "Tidak Signifikan"

#------------------------------------------------
#Gabung data GWR dengan SHP
gabung.IDN2=left_join(shp.IDN2,output.GWPR,by="OBJECTID")

ggplot(data=gabung.IDN2) +
  geom_sf(mapping=aes(fill =signfikansi_JP)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi JP")

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
names(output.GWPR) # Check ada di kolom berapa Signifikansi IKA sampai JP berada
Significant_Map <- data.frame(gabung.IDN2[,1:12],output.GWPR[,39:44])

# Buat kolom baru untuk kombinasi signfikansi
Significant_Map <- Significant_Map %>%
  mutate(Variabel_Signifikan = case_when(
    # Utuh
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,IKTL,INV,HDI,JP",
    
    # Eliminasi 1
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" ~ "IKA,IKU,IKTL,INV,HDI",
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKA,IKU,IKTL,INV,JP",
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,IKTL,HDI,JP",
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,INV,HDI,JP",
    signfikansi_IKA == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKTL,INV,HDI,JP",
    signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKU,IKTL,INV,HDI,JP",
     
    # Eliminasi 2
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" ~ "IKA,IKU,IKTL,INV",
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,IKTL,JP",
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,HDI,JP",
    signfikansi_IKA == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,INV,HDI,JP",
    signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKTL,INV,HDI,JP",
    signfikansi_IKU == "Signifikan" & signfikansi_IKTL == "Signifikan" & 
      signfikansi_INV == "Signifikan" & signfikansi_HDI == "Signifikan" ~ "IKU,IKTL,INV,HDI",
    
    # Eliminasi 3
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_IKTL == "Signifikan" ~ "IKA,IKU,IKTL",
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKA,IKU,JP",
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" & 
      signfikansi_JP == "Signifikan" ~ "IKA,HDI,JP",
    signfikansi_INV == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "INV,HDI,JP",
    signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_HDI == "Signifikan" ~ "IKTL,INV,HDI",
    signfikansi_IKU == "Signifikan" & signfikansi_IKTL == "Signifikan" & 
      signfikansi_INV == "Signifikan" ~ "IKU,IKTL,INV",
    
    # Eliminasi 4
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" ~ "IKA,IKU",
    signfikansi_IKA == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,JP",
    signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "HDI,JP",
    signfikansi_INV == "Signifikan" & signfikansi_HDI == "Signifikan" ~ "INV,HDI,",
    signfikansi_IKTL == "Signifikan" & signfikansi_INV == "Signifikan" ~ "IKTL,INV",
    signfikansi_IKU == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "IKU,IKTL",
    
    # Eliminasi 5
    signfikansi_IKA == "Signifikan" ~ "IKA",
    signfikansi_JP == "Signifikan" ~ "JP",
    signfikansi_HDI == "Signifikan" ~ "HDI",
    signfikansi_INV == "Signifikan" ~ "INV",
    signfikansi_IKTL == "Signifikan" ~ "IKTL",
    signfikansi_IKU == "Signifikan" ~ "IKU",
    
    TRUE ~ "Tidak Signifikan"
  ))

# Buat skema warna kustom
warna_custom <- c(
  "IKA,IKU,IKTL,INV,HDI,JP" = "black",
  "IKA,IKU,IKTL,INV,HDI" = "red",
  "IKA,IKU,IKTL,INV,JP" = "green",
  "IKA,IKU,IKTL,HDI,JP" = "blue",
  "IKA,IKU,INV,HDI,JP" = "cyan",
  "IKA,IKTL,INV,HDI,JP" = "magenta",
  "IKU,IKTL,INV,HDI,JP" = "yellow",
  "IKA,IKU,IKTL,INV" = "gray",
  "IKA,IKU,IKTL,JP" = "darkgray",
  "IKA,IKU,HDI,JP" = "lightgray",
  "IKA,INV,HDI,JP" = "orange",
  "IKTL,INV,HDI,JP" = "brown",
  "IKU,IKTL,INV,HDI" = "pink",
  "IKA,IKU,IKTL" = "violet",
  "IKA,IKU,JP" = "purple",
  "IKA,HDI,JP" = "orchid",
  "INV,HDI,JP" = "lavender",
  "IKTL,INV,HDI" = "plum",
  "IKU,IKTL,INV" = "maroon",
  "IKA,IKU" = "firebrick",
  "IKA,JP" = "tomato",
  "HDI,JP" = "aquamarine",
  "INV,HDI" = "turquoise",
  "IKTL,INV" = "skyblue",
  "IKU,IKTL" = "dodgerblue",
  "IKA" = "steelblue",
  "JP" = "royalblue",
  "HDI" = "navyblue",
  "INV" = "midnightblue",
  "IKTL" = "cornflowerblue",
  "IKU" = "darkslateblue",
  "Tidak Signifikan" = "white"
)

ggplot(data = Significant_Map) +
  geom_sf(mapping=aes(geometry = geometry,fill = Variabel_Signifikan)) +
  scale_fill_manual(values = warna_custom)+
  labs(fill="Variabel Signifikan")
