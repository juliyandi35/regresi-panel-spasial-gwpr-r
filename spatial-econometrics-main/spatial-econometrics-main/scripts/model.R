######### Packages  #########
packages <- c("readxl", "tseries","dynlm","mFilter","quantmod","forecast",
              "lmtest","ggplot2","ggfortify","vars","lmtest","dplyr",
              "leaflet","Rcpp","geosphere","ggmap","tidyverse",
              "spData","rgeos", "ggmap", "rgdal","maptools", "tidyr","sf",
              "raster", "maps", "sp","spdep","splm","spData","spatialreg",
              "stargazer", "ggmap", "plm")
sapply(packages, require, character.only = TRUE) 

### Ubuntu Bionic correr en terminal 
###  sudo apt-get install libgdal-dev gdal-bin libproj-dev proj-data proj-bin libgeos-dev

######## Modelo ########
panel <- read_excel("Base.xlsx")
View(panel)

##Matriz de contactos
arg <- raster::getData("GADM", country = "Argentina", level = 1)
summary(arg)

arg_nb <- poly2nb(arg, queen=TRUE)
coords <- coordinates(arg)
plot(arg,border="grey",lwd=1.5)
plot(arg_nb, coordinates(arg), add=TRUE, col="darkred")

#no funciona xq tierra del fuego queda sin vecionos 
#dlist <- nbdists(nbarg, coords)
#idw <- lapply(dlist, function(x) 1/(x/1000))
#nb2listw(nbarg,glist=idw,style="B",zero.policy = TRUE) #nofunciona

##los k=4 vecinos mas cercanos
arg_k <- knn2nb((knearneigh(coords,k=4)))
arg_k
warg <- nb2listw(arg_k)#funciona
plot(arg_k, coordinates(arg), add=TRUE, col="darkred")
summary(arg_k)

##regresion lineal de prueba
names(panel)
reg_multi <- lm(Pbg_p ~Pob + num_uni, data = panel)
summary(reg_multi)

resid <- reg_multi$residuals[1:24]
length(resid)

##Defino ecuacion a estimar
eq <- log(Pbg_p) ~ log(Pob) + log(num_uni)

##modelo sin tener en cuenta la autocorrección espacial
verdoon_pooled <- plm(eq, data=panel, model="pooling")

verdoorn_fe1  <- plm(eq, data = panel, model = "within", effect="individual")

verdoorn_re1  <- plm(eq, data = panel, model = "random", effect="individual")

summary(verdoon_pooled)
summary(verdoorn_fe1)
summary(verdoorn_re1)

## Prueba de hausman

hausman_panel <- phtest(eq, data = panel)

spat_hausman_ML_SEM <- sphtest(eq, data=panel, listw=warg, spatial.model = "error", method="ML")

spat_hausman_ML_SAR<-sphtest(eq, data=panel, listw =warg, spatial.model = "lag", method="ML")

hausman_panel
spat_hausman_ML_SEM
spat_hausman_ML_SAR

# Fixed effects model
# Test 1
slmtest(eq, data=panel, listw = warg, test="lml", model="within")

# Test 2
slmtest(eq, data=panel, listw = warg, test="lme", model="within")

# Test 3
slmtest(eq, data=panel, listw = warg, test="rlml", model="within")

# Test 4
slmtest(eq, data=panel, listw = warg, test="rlme", model="within")

##modelo
# Likelihood Maximum estimation
verdoorn_SAR_pool <- spml(eq, data = panel, listw = warg, lag=TRUE ,model="pooling")
summary(verdoorn_SAR_pool)

# Fixed-effect SAR
verdoorn_SAR_FE1 <- spml(eq, data = panel, listw = warg, lag=TRUE, model="within", effect="individual", spatial.error="b")
summary(verdoorn_SAR_FE1)

verdoorn_SAR_FE2 <- spml(eq, data = panel, listw = warg, lag=TRUE, model="within", effect="individual", spatial.error="kkp")
summary(verdoorn_SAR_FE1)
