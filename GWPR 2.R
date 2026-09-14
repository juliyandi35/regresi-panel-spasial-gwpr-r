library(kableExtra)
library(stats)
library(graphics)
library(grDevices)
library(utils)
library(datasets)
library(methods)
library(base)
library(readxl)
library(dplyr)
library(ggplot2)
library(gplots)
library(corrplot)
library(car)
library(broom)
library(lmtest)
library(sf)

# Import dataset
Dataset <- read_excel("Data_GWPR_PDRB.xlsx")
Dataset$PROVINSI <- as.factor(Dataset$PROVINSI)
Dataset$Tahun <- as.factor(Dataset$Tahun)
str(Dataset)

# Pre-processing
colSums(is.na(Dataset))

# Analisis deskriptif
# Keragaman waktu
ggplot(data=Dataset,aes(x=Tahun,y=PDRB))+
  geom_line()+
  labs(x="Tahun",y="PDRB")+
  theme(legend.position = "none")+
  theme_bw()

# Keragaman antar individu
plotmeans(PDRB~PROVINSI,main="Keragaman antar Jumlah PDRB",data=Dataset,n.label = F,xlab = "a")

# Correlation Plot
corrplot(cor(Dataset[-c(1:4)]),method = "color",type = "upper",tl.pos = 'tp')
corrplot(cor(Dataset[-c(1:4)]),method = "number",diag = F,add = T, type = "lower",tl.pos = 'n',cl.pos = 'n')

# Time series plot
ggplot(data=Dataset, aes(x=Tahun, y=PDRB, group = PROVINSI, colour = PROVINSI))+ theme_bw()+
  geom_line(size=1.2) +
  geom_point(size=3, shape=19, fill="red") + 
  labs(colour="PROVINSI", title = "Jumlah PDRB", subtitle = "Tahun 2017-2022") +
  theme(plot.title = element_text(face = "bold"))

# Plot peta
shp.IDN=read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")
ggplot() +
  geom_sf(data = shp.IDN) +
  labs(title = "Peta Provinsi di Indonesia")

Dataset=merge(shp.IDN,Dataset,by="PROVINSI")
Dataset <- Dataset[,-c(2:4)]
Dataset

# Uji Multikolinieritas
check_model <- lm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Dataset)
data.frame(t(vif(check_model)))

# Model Common Effect
library(plm)
cem <- plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Dataset, model = "pooling")
summary(cem)

# Model Fixed Effect
fem <- plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Dataset, index = c("PROVINSI", "Tahun"), model = "within", effect= "individual")
summary(fem)
summary(fixef(fem, effect="individual"))

# Model FEM dengan waktu
fem_time <- plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Dataset, index = c("PROVINSI", "Tahun"), model = "within", effect= "time")
summary(fem_time)
summary(fixef(fem_time, effect="time"))

# Model FEM 2 arah
fem_twoways <- plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP,data=Dataset, index = c("PROVINSI", "Tahun"), model = "within", effect= "twoways")
summary(fem_twoways)
data.frame(summary(fixef(fem_twoways, effect="twoways")))

# Uji Signifikasi Pengaruh Individu / Waktu / Two ways
# Uji pengaruh individu
plmtest(fem_twoways, type = "bp", effect = "individual")

# Uji pengaruh waktu
plmtest(fem_twoways, type = "bp", effect = "time")

# Uji pengaruh twoways
plmtest(fem_twoways, type = "bp", effect = "twoways")

# Nilai Kebaikan Model
# Sum Squared Error
dsse <- data.frame(Individu=sum(fem$residuals^2),Time=sum(fem_time$residuals^2),Twoways=sum(fem_twoways$residuals^2))

# AIC
lsdv_ind <- lm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP + PROVINSI,data=Dataset)
lsdv_time <- lm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP + Tahun,data=Dataset)
lsdv_twoways <- lm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP + PROVINSI + Tahun,data=Dataset)

daic <- data.frame(Individu=AIC(lsdv_ind),Time=AIC(lsdv_time),Twoways=AIC(lsdv_twoways))

# MAPE
mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual)) * 100
}
dmape <- data.frame(Individu=mape(Dataset$PDRB,predict(fem)),
                    Time=mape(Dataset$PDRB,predict(fem_time)),
                    Twoways=mape(Dataset$PDRB,predict(fem_twoways)))
# BIC
dbic <- data.frame(Individu=BIC(lsdv_ind),Time=BIC(lsdv_time),Twoways=BIC(lsdv_twoways))

# R-squared
ind <- summary(fem)
time <- summary(fem_time)
tways <- summary(fem_twoways)
drsq <- data.frame(Individu=ind$r.squared,Time=time$r.squared,Twoways=tways$r.squared)

# Perbandingan
compare <- t(rbind(dsse,daic,dbic,dmape,drsq))
colnames(compare) <- c("SSE","AIC","BIC", "MAPE","R-Squared","Adj R-Squared")
compare

# FEM VS CEM
pooltest(cem, fem_twoways) # Keputusan pilih FEM

# REM dengan Generalized Least Square
rem_gls <- plm(PDRB ~ IKA + IKU + IKTL + INV + HDI + JP, data = Dataset, 
               index = c("PROVINSI", "Tahun"), 
               effect = "twoways", model = "random", random.method = "nerlove")
summary(rem_gls)

#efek individu
plmtest(rem_gls,type = "bp", effect="individu")

#efek waktu 
plmtest(rem_gls,type = "bp", effect="time")

#efek twoways 
plmtest(rem_gls,type = "bp", effect="twoways")

tidy_ranef_ind <- tidy(ranef(rem_gls, effect="individual"))
colnames(tidy_ranef_ind) <- c("Provinsi", "Pengaruh Acak Individu")
tidy_ranef_ind

tidy_ranef_time <- tidy(ranef(rem_gls, effect="time"))
colnames(tidy_ranef_time) <- c("Tahun", "Pengaruh Acak Waktu")
tidy_ranef_time

# FEM VS REM
# Uji Haussman
phtest(fem_twoways, rem_gls) # Pilih model FEM

# Uji diagnostik residu
# Uji normalitas
ks.test(fem_twoways$residuals, "pnorm", 
        mean=mean(fem_twoways$residuals), 
        sd=sd(fem_twoways$residuals))

shapiro.test(fem_twoways$residuals)

# Histogram
ggplot(as.data.frame(fem_twoways$residuals), aes(x = fem_twoways$residuals)) +
  geom_histogram(aes(y = after_stat(density)), color = "white", fill = "steelblue") +
  geom_density(color = "red", linewidth = 1) +
  theme_minimal()

# Uji Autokorelasi
pbgtest(fem_twoways)

# Uji Heteroskedastisitas
library(lmtest)
bptest(fem_twoways)

# Modeling GWPR
library(GWmodel)
library(sp)
Dataset.df <- read_excel("Data_GWPR_PDRB.xlsx")
Dataset.df$ID <- match(Dataset.df$PROVINSI,unique(Dataset.df$PROVINSI))

Dataset.sdf <- read_sf("BATAS PROVINSI DESEMBER 2019 DUKCAPIL/BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL.shp")
names(Dataset.sdf)
Dataset.sdf <- Dataset.sdf[,-c(3,4)]
colnames(Dataset.sdf) <- c("ID","PROVINSI","geometry")
Dataset.sdf <- as(st_zm(Dataset.sdf),"Spatial")
class(Dataset.sdf)

# Menentukan fungsi pembobot spasial terbaik
#adaptive bisquare
library(GWPR.light)
bw <- bw.GWPR(formula = PDRB ~ IKA + IKU + IKTL + INV + HDI + JP, data = Dataset.df,
                    index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = TRUE,
                    effect = "twoways", model = "within",
                    kernel = "bisquare", longlat = FALSE,approach = "CV")

# library(devtools)
# devtools::install_github(repo = "https://github.com/MichaelChaoLi-cpu/GWPR.light")

library(GWPR.light)
result <- GWPR(bw = bw, formula = PDRB ~ IKA + IKU + IKTL + INV + HDI + JP, data = Dataset.df,
                     index = c("ID", "Tahun"), SDF = Dataset.sdf, adaptive = TRUE,
                     effect = "twoways", model = "within",
                     kernel = "bisquare", longlat = FALSE)

# Define the Exponential kernel function
library(spdep)
Dataset <- st_as_sf(st_zm(Dataset))
x = coordinates(as(Dataset,"Spatial"))[,1]
y = coordinates(as(Dataset,"Spatial"))[,2]
coords <-cbind(x,y)
jarak <-as.matrix(1/dist(coords))

bisquare_kernel <- function(dists, bw) {
  ifelse(dists < bw, (1-(dists/bw)^2)^2, 0)
}
weights <- bisquare_kernel(jarak, bw)
# writexl::write_xlsx(weights,"Pembobot Kernel Bisquare.xlsx")

# Perbandingan hasil
Compare2 <- data.frame(R.Squared = c(result$R2,summary(fem_twoways)$r.squared))
rownames(Compare2) <- c("GWPR","FEM","Adjusted R FEM")
Compare2

#Pembuatan jarak euclidean
n <- 34 #jumlah wilayah
U <- Dataset$LONG #data Longitude
V <- Dataset$LAT #data Latitude
d <- matrix(0,n,n)
for (i in 1:n) {
  for (j in 1:n) {
    d[i,j] <- sqrt(((U[i]-U[j])^2)+((U[i]-U[j])^2))
  }
}
d
# writexl::write_xlsx(data.frame(d),"Hasil_Jarak_Euclidean.xlsx")

#----------------------------------------------------------------------------
#Export Hasil analisis GWPR ke Excel
GWPR.Result = st_as_sf(result$SDF)
# writexl::write_xlsx(GWPR.Result,"hasil GWR Panel Terbaik.xlsx")

#---------------------------------------------------------------#
#    koefisien variabel (variabel IKA)
#---------------------------------------------------------------#
ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =IKA)) +
  scale_fill_gradient2(
    midpoint = 0, low = "#28E2E5", mid = "white", high = "#DF536B"
  ) +
  theme_bw()+
  ggtitle("Koefisien IKA")

#---------------------------------------------------------------#
#    koefisien variabel (variabel IKU)
#---------------------------------------------------------------#
ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =IKU)) +
  scale_fill_gradient2(
    midpoint = 0, low = "#28E2E5", mid = "white", high = "#DF536B"
  ) +
  theme_bw()+
  ggtitle("Koefisien IKU")

#---------------------------------------------------------------#
#    koefisien variabel (variabel IKTL)
#---------------------------------------------------------------#
ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =IKTL)) +
  scale_fill_gradient2(
    midpoint = 0, low = "#28E2E5", mid = "white", high = "#DF536B"
  ) +
  theme_bw()+
  ggtitle("Koefisien IKTL")

#---------------------------------------------------------------#
#    koefisien variabel (variabel INV)
#---------------------------------------------------------------#
ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =INV)) +
  scale_fill_gradient2(
    midpoint = 0, low = "#28E2E5", mid = "white", high = "#DF536B"
  ) +
  theme_bw()+
  ggtitle("Koefisien INV")

#---------------------------------------------------------------#
#    koefisien variabel (variabel HDI)
#---------------------------------------------------------------#
ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =HDI)) +
  scale_fill_gradient2(
    midpoint = 0, low = "#28E2E5", mid = "white", high = "#DF536B"
  ) +
  theme_bw()+
  ggtitle("Koefisien HDI")

#---------------------------------------------------------------#
#    koefisien variabel (variabel JP)
#---------------------------------------------------------------#
ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =JP)) +
  scale_fill_gradient2(
    midpoint = 0, low = "#28E2E5", mid = "white", high = "#DF536B"
  ) +
  theme_bw()+
  ggtitle("Koefisien JP")

#---------------------------------------------------------------#
#    Local R2
#---------------------------------------------------------------#
ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =Local_R2)) +
  scale_fill_gradient2(
    midpoint = mean(GWPR.Result$Local_R2), low = "#28E2E5", mid = "white", high = "#DF536B"
  ) +
  theme_bw()+
  ggtitle("Local R2")

#---------------------------------------------------------------#
#    signfikansi variabel (variabel IKA)
#---------------------------------------------------------------#
GWPR.Result$signfikansi_IKA <- NA
# Signifikan
GWPR.Result[(GWPR.Result$IKA_TVa <= -1.97246199 | GWPR.Result$IKA_TVa >= 1.97246199), "signfikansi_IKA"] <- "Signifikan"

# Tidak Signifikan
GWPR.Result[(GWPR.Result$IKA_TVa > -1.97246199 & GWPR.Result$IKA_TVa < 1.97246199), "signfikansi_IKA"] <- "Tidak Signifikan"

ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =signfikansi_IKA)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi IKA")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel IKU)
#---------------------------------------------------------------#
GWPR.Result$signfikansi_IKU <- NA
# Signifikan
GWPR.Result[(GWPR.Result$IKU_TVa <= -1.97246199 | GWPR.Result$IKU_TVa >= 1.97246199), "signfikansi_IKU"] <- "Signifikan"

# Tidak Signifikan
GWPR.Result[(GWPR.Result$IKU_TVa > -1.97246199 & GWPR.Result$IKU_TVa < 1.97246199), "signfikansi_IKU"] <- "Tidak Signifikan"

ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =signfikansi_IKU)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi IKU")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel IKTL)
#---------------------------------------------------------------#
GWPR.Result$signfikansi_IKTL <- NA
# Signifikan
GWPR.Result[(GWPR.Result$IKTL_TVa <= -1.97246199 | GWPR.Result$IKTL_TVa >= 1.97246199), "signfikansi_IKTL"] <- "Signifikan"

# Tidak Signifikan
GWPR.Result[(GWPR.Result$IKTL_TVa > -1.97246199 & GWPR.Result$IKTL_TVa < 1.97246199), "signfikansi_IKTL"] <- "Tidak Signifikan"

ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =signfikansi_IKTL)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi IKTL")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel INV)
#---------------------------------------------------------------#
GWPR.Result$signfikansi_INV <- NA
# Signifikan
GWPR.Result[(GWPR.Result$INV_TVa <= -1.97246199 | GWPR.Result$INV_TVa >= 1.97246199), "signfikansi_INV"] <- "Signifikan"

# Tidak Signifikan
GWPR.Result[(GWPR.Result$INV_TVa > -1.97246199 & GWPR.Result$INV_TVa < 1.97246199), "signfikansi_INV"] <- "Tidak Signifikan"

ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =signfikansi_INV)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi INV")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel HDI)
#---------------------------------------------------------------#
GWPR.Result$signfikansi_HDI <- NA
# Signifikan
GWPR.Result[(GWPR.Result$HDI_TVa <= -1.97246199 | GWPR.Result$HDI_TVa >= 1.97246199), "signfikansi_HDI"] <- "Signifikan"

# Tidak Signifikan
GWPR.Result[(GWPR.Result$HDI_TVa > -1.97246199 & GWPR.Result$HDI_TVa < 1.97246199), "signfikansi_HDI"] <- "Tidak Signifikan"

ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =signfikansi_HDI)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi HDI")

#---------------------------------------------------------------#
#    signfikansi variabel (misal variabel JP)
#---------------------------------------------------------------#
GWPR.Result$signfikansi_JP <- NA
# Signifikan
GWPR.Result[(GWPR.Result$JP_TVa <= -1.97246199 | GWPR.Result$JP_TVa >= 1.97246199), "signfikansi_JP"] <- "Signifikan"

# Tidak Signifikan
GWPR.Result[(GWPR.Result$JP_TVa > -1.97246199 & GWPR.Result$JP_TVa < 1.97246199), "signfikansi_JP"] <- "Tidak Signifikan"

ggplot(data=GWPR.Result) +
  geom_sf(mapping=aes(fill =signfikansi_JP)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi JP")

#---------------------------------------------------------------#
#    Plotting variabel signifikan
#---------------------------------------------------------------#
Significant_Map <- GWPR.Result

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
      signfikansi_IKTL == "Signifikan" & signfikansi_HDI == "Signifikan" & 
      signfikansi_JP == "Signifikan" ~ "IKA,IKU,IKTL,HDI,JP",
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_INV == "Signifikan" & signfikansi_HDI == "Signifikan" & 
      signfikansi_JP == "Signifikan" ~ "IKA,IKU,INV,HDI,JP",
    signfikansi_IKA == "Signifikan" & signfikansi_IKTL == "Signifikan" & 
      signfikansi_INV == "Signifikan" & signfikansi_HDI == "Signifikan" & 
      signfikansi_JP == "Signifikan" ~ "IKA,IKTL,INV,HDI,JP",
    signfikansi_IKU == "Signifikan" & signfikansi_IKTL == "Signifikan" & 
      signfikansi_INV == "Signifikan" & signfikansi_HDI == "Signifikan" & 
      signfikansi_JP == "Signifikan" ~ "IKU,IKTL,INV,HDI,JP",
    
    # Eliminasi 2
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_INV == "Signifikan" ~ "IKA,IKU,HDI,INV",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "IKA,IKU,HDI,IKTL",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,HDI,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_INV == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "IKA,IKU,INV,IKTL",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_INV == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,INV,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" & 
      signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,IKU,IKTL,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_INV == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "IKA,HDI,INV,IKTL",
    
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_INV == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,HDI,INV,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,HDI,IKTL,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,INV,IKTL,JP",
    
    signfikansi_IKU == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_INV == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "IKU,HDI,INV,IKTL",
    
    signfikansi_IKU == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_INV == "Signifikan" & signfikansi_JP == "Signifikan"~ "IKU,HDI,INV,JP",
    
    signfikansi_IKU == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKU,HDI,IKTL,JP",
    
    signfikansi_IKU == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKU,INV,IKTL,JP",
    
    signfikansi_HDI == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "HDI,INV,IKTL,JP", 

    # Eliminasi 3
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_HDI == "Signifikan" ~ "IKA,IKU,HDI",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_INV == "Signifikan" ~ "IKA,IKU,INV",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" & 
      signfikansi_IKTL == "Signifikan" ~ "IKA,IKU,IKTL",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKA,IKU,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_INV == "Signifikan" ~ "IKA,HDI,INV",
    
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" & 
      signfikansi_IKTL == "Signifikan" ~ "IKA,HDI,IKTL",
    
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKA,HDI,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_IKTL == "Signifikan" ~ "IKA,INV,IKTL",
    
    signfikansi_IKA == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKA,INV,JP",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKTL == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKA,IKTL,JP",
    
    signfikansi_IKU == "Signifikan" & signfikansi_HDI == "Signifikan" & 
      signfikansi_INV == "Signifikan" ~ "IKU,HDI,INV",
    
    signfikansi_IKU == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_IKTL == "Signifikan" ~ "IKU,HDI,IKTL",
    
    signfikansi_IKU == "Signifikan" & signfikansi_HDI == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKU,HDI,JP",
    
    signfikansi_IKU == "Signifikan" & signfikansi_INV == "Signifikan" & 
      signfikansi_IKTL == "Signifikan" ~ "IKU,INV,IKTL",
    
    signfikansi_IKU == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKU,INV,JP",
    
    signfikansi_IKU == "Signifikan" & signfikansi_IKTL == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "IKU,IKTL,JP",
    
    signfikansi_HDI == "Signifikan" & signfikansi_INV == "Signifikan" & 
      signfikansi_IKTL == "Signifikan" ~ "HDI,INV,IKTL",
    
    signfikansi_HDI == "Signifikan" & signfikansi_INV == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "HDI,INV,JP",
    
    signfikansi_HDI == "Signifikan" & signfikansi_IKTL == "Signifikan" &
      signfikansi_JP == "Signifikan" ~ "HDI,IKTL,JP",
    
    signfikansi_INV == "Signifikan" & signfikansi_IKTL == "Signifikan" & 
      signfikansi_JP == "Signifikan" ~ "INV,IKTL,JP",
    
    # Eliminasi 4
    signfikansi_IKA == "Signifikan" & signfikansi_IKU == "Signifikan" ~ "IKA,IKU",
    
    signfikansi_IKA == "Signifikan" & signfikansi_HDI == "Signifikan" ~ "IKA,HDI",
    
    signfikansi_IKA == "Signifikan" & signfikansi_INV == "Signifikan" ~ "IKA,INV",
    
    signfikansi_IKA == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "IKA,IKTL",
    
    signfikansi_IKA == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKA,JP",
    
    signfikansi_IKU == "Signifikan" & signfikansi_HDI == "Signifikan" ~ "IKU,HDI",
    
    signfikansi_IKU == "Signifikan" & signfikansi_INV == "Signifikan" ~ "IKU,INV",
    
    signfikansi_IKU == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "IKU,IKTL",
    
    signfikansi_IKU == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKU,JP",
    
    signfikansi_HDI == "Signifikan" & signfikansi_INV == "Signifikan" ~ "HDI,INV",
    
    signfikansi_HDI == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "HDI,IKTL",
    
    signfikansi_HDI == "Signifikan" & signfikansi_JP == "Signifikan" ~ "HDI,JP",
    
    signfikansi_INV == "Signifikan" & signfikansi_IKTL == "Signifikan" ~ "INV,IKTL",
    
    signfikansi_INV == "Signifikan" & signfikansi_JP == "Signifikan" ~ "INV,JP",
    
    signfikansi_IKTL == "Signifikan" & signfikansi_JP == "Signifikan" ~ "IKTL,JP",
    
    # Eliminasi 5
    signfikansi_IKA == "Signifikan" ~ "IKA",
    signfikansi_IKU == "Signifikan" ~ "IKU",
    signfikansi_HDI == "Signifikan" ~ "HDI",
    signfikansi_JP == "Signifikan" ~ "JP",
    signfikansi_INV == "Signifikan" ~ "INV",
    signfikansi_IKTL == "Signifikan" ~ "IKTL",
    
    TRUE ~ "Tidak Signifikan"
  ))

# Buat skema warna kustom
warna_custom <- c(
  # Utuh
  "IKA,IKU,IKTL,INV,HDI,JP" = "black",
  
  # Eliminasi 1
  "IKA,IKU,IKTL,INV,HDI" = "red",
  "IKA,IKU,IKTL,INV,JP" = "yellow",
  "IKA,IKU,IKTL,HDI,JP" = "blue",
  "IKA,IKU,INV,HDI,JP" = "green",
  "IKA,IKTL,INV,HDI,JP" = "purple",
  "IKU,IKTL,INV,HDI,JP" = "orange",
  
  # Eliminasi 2
  "IKA,IKU,HDI,INV" = "pink",
  "IKA,IKU,HDI,IKTL" = "brown",
  "IKA,IKU,HDI,JP" = "gray",
  "IKA,IKU,INV,IKTL" = "cyan",
  "IKA,IKU,INV,JP" = "magenta",
  "IKA,IKU,IKTL,JP" = "turquoise",
  "IKA,HDI,INV,IKTL" = "lavender",
  "IKA,HDI,INV,JP" = "maroon",
  "IKA,HDI,IKTL,JP" = "gold",
  "IKA,INV,IKTL,JP" = "coral",
  "IKU,HDI,INV,IKTL" = "beige",
  "IKU,HDI,INV,JP" = "navy",
  "IKU,HDI,IKTL,JP" = "tan",
  "IKU,INV,IKTL,JP" = "salmon",
  "HDI,INV,IKTL,JP" = "ivory",
  
  # Eliminasi 3
  "IKA,IKU,HDI" = "khaki",
  "IKA,IKU,INV" = "orchid",
  "IKA,IKU,IKTL" = "azure",
  "IKA,IKU,JP" = "sienna",
  "IKA,HDI,INV" = "plum",
  "IKA,HDI,IKTL" = "skyblue",
  "IKA,HDI,JP" = "forestgreen",
  "IKA,INV,IKTL" = "olivedrab",
  "IKA,INV,JP" = "lavenderblush",
  "IKA,IKTL,JP" = "midnightblue",
  "IKU,HDI,INV" = "rosybrown",
  "IKU,HDI,IKTL" = "slategray",
  "IKU,HDI,JP" = "darkorchid",
  "IKU,INV,IKTL" = "lightsalmon",
  "IKU,INV,JP" = "darkseagreen",
  "IKU,IKTL,JP" = "cornflowerblue",
  "HDI,INV,IKTL" = "darkgoldenrod",
  "HDI,INV,JP" = "powderblue",
  "HDI,IKTL,JP" = "palevioletred",
  "INV,IKTL,JP" = "mediumaquamarine",
  
  # Eliminasi 4
  "IKA,IKU" = "aquamarine",
  "IKA,HDI" = "lightsteelblue",
  "IKA,INV" = "darkkhaki",
  "IKA,IKTL" = "indianred",
  "IKA,JP" = "mediumslateblue",
  "IKU,HDI" = "lightcoral",
  "IKU,INV" = "mediumpurple",
  "IKU,IKTL" = "darkolivegreen",
  "IKU,JP" = "lightskyblue",
  "HDI,INV" = "mediumorchid",
  "HDI,IKTL" = "honeydew",
  "HDI,JP" = "hotpink",
  "INV,IKTL" = "bisque",
  "INV,JP" = "blueviolet",
  "IKTL,JP" = "burlywood",
  
  # Eliminasi 5
  "IKA" = "cadetblue",
  "IKU" = "chartreuse3",
  "HDI" = "chocolate",
  "JP" = "deeppink",
  "INV" = "darkorchid4",
  "IKTL" = "cornsilk4",
  
  "Tidak Signifikan"  = "white"
)
Var_Sig = c(Significant_Map$Variabel_Signifikan[1:4],Significant_Map$Variabel_Signifikan[34],Significant_Map$Variabel_Signifikan[5:33])
Sig_IKA = c(Significant_Map$signfikansi_IKA[1:4],Significant_Map$signfikansi_IKA[34],Significant_Map$signfikansi_IKA[5:33])
Sig_IKU = c(Significant_Map$signfikansi_IKU[1:5],Significant_Map$signfikansi_IKU[34],Significant_Map$signfikansi_IKU[6:33])
Sig_IKTL = c(Significant_Map$signfikansi_IKTL[1:5],Significant_Map$signfikansi_IKTL[34],Significant_Map$signfikansi_IKTL[6:33])
Sig_INV = c(Significant_Map$signfikansi_INV[1:4],Significant_Map$signfikansi_INV[34],Significant_Map$signfikansi_INV[5:33])
Sig_HDI = c(Significant_Map$signfikansi_HDI[1:4],Significant_Map$signfikansi_HDI[34],Significant_Map$signfikansi_HDI[5:33])
Sig_JP = c(Significant_Map$signfikansi_JP[1:3],Significant_Map$signfikansi_JP[34],Significant_Map$signfikansi_JP[4:33])

Plotting_data <- data.frame(ID = Significant_Map$id,
                            Sig_IKA = Sig_IKA,
                            Sig_IKU = Sig_IKU,
                            Sig_IKTL = Sig_IKTL,
                            Sig_INV = Sig_INV,
                            Sig_HDI = Sig_HDI,
                            Sig_JP = Sig_JP,
                            Var_Sig = Var_Sig,
                            geometry = Significant_Map$geometry)
Plotting_data <- st_as_sf(Plotting_data)

ggplot(data=Plotting_data) +
  geom_sf(mapping=aes(fill =Sig_IKA)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi IKA")

ggplot(data=Plotting_data) +
  geom_sf(mapping=aes(fill =Sig_IKU)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi IKU")

ggplot(data=Plotting_data) +
  geom_sf(mapping=aes(fill =Sig_IKTL)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi IKTL")

ggplot(data=Plotting_data) +
  geom_sf(mapping=aes(fill =Sig_INV)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi INV")

ggplot(data=Plotting_data) +
  geom_sf(mapping=aes(fill =Sig_HDI)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi HDI")

ggplot(data=Plotting_data) +
  geom_sf(mapping=aes(fill =Sig_JP)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="Signifikansi")+ggtitle("Signifikansi JP")


ggplot(data = Plotting_data) +
  geom_sf(mapping=aes(geometry = geometry,fill = Var_Sig))+
  scale_fill_manual(values = warna_custom)
  labs(fill="Variabel Signifikan")

writexl::write_xlsx(GWPR.Result,"Hasil GWPR.xlsx")
writexl::write_xlsx(Significant_Map,"Variabel Signifikan 2.xlsx")

ggplot(data=Significant_Map) +
  geom_sf(mapping=aes(fill =signfikansi_IKA)) +
  scale_fill_manual(values = c("#28E2E5", "#DF536B"))+
  labs(fill="signfikansi")+ggtitle("Signifikansi IKA")
