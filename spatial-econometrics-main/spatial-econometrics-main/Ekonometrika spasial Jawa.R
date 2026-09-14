######### Packages  #########
packages <- c("readxl", "tseries","dynlm","mFilter","quantmod","forecast",
              "lmtest","ggplot2","ggfortify","vars","lmtest","dplyr",
              "leaflet","Rcpp","geosphere","ggmap","tidyverse",
              "spData","rgeos", "ggmap", "rgdal","maptools", "tidyr","sf",
              "raster", "maps", "sp","spdep","splm","spData","spatialreg",
              "stargazer", "ggmap", "plm")
sapply(packages, require, character.only = TRUE) 


######## Model ########
panel <- read_excel("Dataset.xlsx")
View(panel)

## Dapatkan data raster Indonesia
ind <- raster::getData("GADM", country = "Indonesia", level = 1)
summary(ind)

# Menentukan batas wilayah Pulau Jawa
batas_jawa <- extent(106, 114, -11, -6)

# Memotong raster sesuai dengan batas wilayah yang telah ditentukan
jawa <- crop(ind, batas_jawa)

jawa_nb <- poly2nb(jawa, queen=TRUE)
coords <- coordinates(jawa)
plot(jawa,border="grey",lwd=1.5)
plot(jawa_nb, coordinates(jawa), add=TRUE, col="darkred")

## k = 2 adalah jarak maksimal
jawa_k <- knn2nb((knearneigh(coords,k=2)))
jawa_k
wjawa <- nb2listw(jawa_k)#funciona
plot(jawa_k, coordinates(jawa), add=TRUE, col="darkred")
summary(jawa_k)

## Regresi linear berganda
names(panel)
reg_multi <- lm(PDRB ~IKA + IKU+INVESTASI+LAYANAN_KESEHATAN, data = panel)
summary(reg_multi)

resid <- reg_multi$residuals[1:24]
length(resid)

## Definisikan persamaannya
eq <- PDRB ~ IKA + IKU + INVESTASI + LAYANAN_KESEHATAN

## Model tanpa memperhitungkan efek spasial
model_pooled <- plm(eq, data=panel, model="pooling") # CEM

model_fe1  <- plm(eq, data = panel, model = "within", effect="individual") #FEM

model_re1  <- plm(eq, data = panel, model = "random", effect="individual") # REM

summary(model_pooled)
summary(model_fe1)
summary(model_re1)

## Uji Hausman dengan memperhitungkan efek spasial

hausman_panel <- phtest(eq, data = panel)

spat_hausman_ML_SEM <- sphtest(eq, data=panel, listw=wjawa, spatial.model = "error", method="ML")

spat_hausman_ML_SAR<-sphtest(eq, data=panel, listw =wjawa, spatial.model = "lag", method="ML")

hausman_panel
spat_hausman_ML_SEM
spat_hausman_ML_SAR

# Fixed effects model
# Test 1
slmtest(eq, data=panel, listw = wjawa, test="lml", model="within")

# Test 2
slmtest(eq, data=panel, listw = wjawa, test="lme", model="within")

# Test 3
slmtest(eq, data=panel, listw = wjawa, test="rlml", model="within")

# Test 4
slmtest(eq, data=panel, listw = wjawa, test="rlme", model="within")

## Model
# Likelihood Maximum estimation
model_SAR_pool <- spml(eq, data = panel, listw = wjawa, lag=TRUE ,model="pooling")
summary(model_SAR_pool)

# Fixed-effect SAR
model_SAR_FE1 <- spml(eq, data = panel, listw = wjawa, lag=TRUE, model="within", effect="individual", spatial.error="b")
summary(model_SAR_FE1)

model_SAR_FE2 <- spml(eq, data = panel, listw = wjawa, lag=TRUE, model="within", effect="individual", spatial.error="kkp")
summary(model_SAR_FE1)
