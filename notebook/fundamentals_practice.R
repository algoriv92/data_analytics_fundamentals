##############################################################

# ANALISIS FINANCIERO DEL MERCADO DE LAS CRIPTOMONEDAS, XRP ADA
# Y ETHEREUM EN ESTE CASO. EL PERÍODO COMPRENDIDO ENTRE EL 1-09- 
# 2019 Y EL 1-09-2021. NUESTRA VARIABLE CATEGÓRICA SERÁ EL COVID
# PARA VER SU IMPACTO EN ESTE TIPO DE DIVISAS.

##############################################################

###############################################################
###############################################################
###############################################################
###############################################################

# install.packages("devtools")
# install.packages("coinmarketcapr")

library(coinmarketcapr)
library(tidyverse)
key <- "32d3cc1d-379a-4a3d-8220-93be4e034ef4"
coinmarketcapr::setup(key)

###############################################################
###############################################################

# NOW WE HAVE SET UP OUR KEY
# LET'S SEE THE AVAILABLE CRYPTOCURRENCIES

criptos <- get_crypto_listings(limit=50)

marketcap<- get_global_marketcap()
View(marketcap)

# devtools::install_github("amrrs/coinmarketcapr")

###############################################################
###############################################################

# LET'S CHECK THE TOP 10 CRYPTOCURRENCIES TODAY AND PLOT EM

top_10 <- criptos %>% 
  head(10) %>%
  select(c('name','symbol', 'USD_price','USD_market_cap_dominance'))

# PLOTTING THE TOP 10 SEEING NAME AND DOMINANCE 

top_10<-top_10[order(top_10$USD_market_cap_dominance,decreasing = T),]

top_10 %>% 
  ggplot()+
  geom_col(aes(x=name, y = USD_price, color=USD_market_cap_dominance), size=0.6, alpha=0.7)+
  coord_flip()

top_10 %>% 
  ggplot()+
  geom_col(aes(x=name, y = USD_market_cap_dominance))+
  coord_flip()


###############################################################
###############################################################
###############################################################


# AÑADIMOS USD_percent_change_90d Y VEMOS LA EVOLUCION % DE LAS MONEDAS Y LAS 
# DIBUJAMOS

top_10_2 <- criptos %>% 
  head(10) %>% 
  select(c('name','symbol', 'USD_price','USD_market_cap_dominance','USD_percent_change_90d'))

# VEMOS EL CRECIMIENTO MEDIO EN LOS ÚLTIMOS 90 DÍAS

top_10_2 %>% 
  ggplot()+
  geom_col(aes(x=name,y=USD_percent_change_90d), col="blue", fill="lightblue")+
  labs(title="90 Days Growth", x="Coins", y="Growth_percentage")

###############################################################
###############################################################

# DOMINANCE CON SECTORES

pie(top_10_2$USD_market_cap_dominance, top_10_2$name, main="Dominance",
    col= rainbow(length(top_10_2)))


# CARGAMOS LOS DATOS SEGÚN EL PRECIO DE APERTURA DIARIO:

rm(list=ls())
datos_crypto<-read.csv("c:/Users/alexg/Desktop/CUNEF/M�ster en Data Science/Fundamentos para el an�lisis de datos/Temario asignatura/Directrices trabajo/Trabajo final/crypto.csv")
names(datos_crypto)
attach(datos_crypto)
head(datos_crypto)
tail(datos_crypto)
datos_crypto

nrow(datos_crypto)
ncol(datos_crypto)

##############################################################

# CREAMOS LA VARIABLE CATEGÓRICA COVID DESDE EL 11 DE MARZO DE 
# 2020 QUE ES CUANDO LA OMS DECLARÓ LA PANDEMIA:

once_marzo <- 192 
covid_crypto<- c(rep(0,once_marzo),rep(1,nrow(datos_crypto)-once_marzo)) 
covid_crypto

##############################################################
##############################################################

# CALCULO DE LOS RENDIMIENTOS DE CADA CRIPTOMONEDA

n <- dim(datos_crypto)[1]
n

RBTC  <- btc[2:n] / btc[1:n-1] - 1  
RETH  <- eth[2:n] / eth[1:n-1] - 1
RXRP  <- xrp[2:n] / xrp[1:n-1] - 1
RADA  <- ada[2:n] / ada[1:n-1] - 1


covid_crypto_rendimientos <- covid_crypto[2:n]
covid_crypto_rendimientos <-as.factor(covid_crypto_rendimientos)

##############################################################################
##############################################################################
##############################################################################

# NORMALIZAMOS LAS VARIABLES

btc_norm<-btc/max(btc)
eth_norm<-eth/max(eth)
xrp_norm<-xrp/max(xrp)
ada_norm<-ada/max(ada)

# NUEVO DATA FRAME CON LAS MONEDAS NORMALIZADAS

datos_crypto_norm <- data.frame(Date=as.Date(datos_crypto$Date, format="%d/%m/%Y"),
                                btc_norm=btc_norm,
                                eth_norm=eth_norm,
                                xrp_norm=xrp_norm,
                                ada_norm=ada_norm)

# COMPROBAMOS QUE LA NUEVA DF NO TIENE DUPLICADOS

table(duplicated(datos_crypto_norm$Date))
dim(datos_crypto_norm)
head(datos_crypto_norm)
tail(datos_crypto_norm)

# PIVOTAMOS EL DF PARA TENERLO EN ESTILO TIDY

datos_crypto_norm_longer <- pivot_longer(datos_crypto_norm, cols =
                                       c(btc_norm,eth_norm,xrp_norm,ada_norm),
                                       values_to = "normalizada", names_to = "Crypto")

ggplot(datos_crypto_norm_longer) +
  geom_line(aes(x=Date,y=normalizada, col=Crypto))+
   scale_x_date(date_breaks = "1 month")
  
ggplot(datos_crypto_norm_longer) +
  geom_smooth(aes(x=Date,y=normalizada, col=Crypto))+
  scale_x_date(date_breaks = "1 month")



# Cuando llamas a plot crea una "hoja en blanco", y lines van sobre el plot anterior.
# cuando llamas a lines, quitamos el main. ES OTRA FORMA DE HACER LO DE ANTES 

head(datos_crypto_norm)

plot(btc_norm,type = "l", col="lightgreen", main = "Criptomonedas Normalizadas",
     ylab="", xlab="")

lines(eth_norm,type="l",col="lightblue")

lines(xrp_norm,type="l", col="orange")

lines(ada_norm,type="l", col="black")

legend(0,.99 , legend=c("btc_norm","eth_norm","xrp_norm","ada_norm"),
               col=c("lightgreen","lightblue","orange","black"))


##############################################################################

# COMPARAMOS LA EVOLUCIÓN DE LOS PRECIOS DE LAS DISTINTAS MONEDAS EN EL TIEMPO

# par(mfrow=c(2,2))

plot(btc,type = "l", col="lightgreen", main = "Precio Bitcoin")

plot(eth,type="l",col="lightblue", main = "Precio Ethereum")

plot(xrp,type="l", col="orange", main ="Precio XRP")

plot(ada,type="l", main ="Precio ADA")

##############################################################################

# CREAMOS UN DATA FRAME DE TODOS LOS RENDIMIENTOS JUNTOS Y COMENTAMOS

Rendimientos_crypto <- cbind(RBTC, RETH , RXRP, RADA) 
Rendimientos_crypto <- as.data.frame(Rendimientos_crypto)
summary(Rendimientos_crypto)


# COMPARAMOS LOS RENDIMIENTOS ANTES Y DESPUÉS DEL COVID CON BOXPLOTS

library(ggplot2)
par(mfrow=c(2,2))
boxplot(RBTC ~ covid_crypto_rendimientos, col="lightgreen", xlab = "Rendimientos")
boxplot(RETH ~ covid_crypto_rendimientos, col="lightblue", xlab="Rendimientos")
boxplot(RXRP ~ covid_crypto_rendimientos, col="orange", xlab="Rendimientos")
boxplot(RADA ~ covid_crypto_rendimientos, xlab="Rendimientos") 

##############################################################################

# PODEMOS HACER LO DE ANTES PERO DE UNA
# par(mfrow=c(2,2))

for (j in 1:4) {
  boxplot(Rendimientos_crypto[,j]
          ~covid_crypto_rendimientos,main=colnames(Rendimientos_crypto)[j],ylab="",xlab="",col="orange") 
}

##############################################################################
##############################################################################

# HISTOGRAMAS

par(mfrow=c(2,2))  


hist(RBTC,prob=T,main="Rendimientos BTC", ylim= c (0,10))
lines(density(RBTC))
box()
hist(RETH,prob=T,main="Rendimientos ETH", ylim= c (0,10))
lines(density(RETH))
box()
hist(RXRP,prob=T,main="Rendimientos XRP", ylim= c (0,10))
lines(density(RXRP))
box()
hist(RADA,prob=T,main="Rendimientos ADA", ylim = c(0,10))
lines(density(RADA))
box()

##############################################################################

# PAREJAS DE SERIES:

plot(RADA,RBTC)

pairs(Rendimientos_crypto, panel = panel.smooth, main = "Rendimientos", col="orange")

##############################################################################
# PASAMOS A VER LA CORRELACIÓN QUE HAY ENTRE LAS VARIABLES

cor(Rendimientos_crypto)
cov(Rendimientos_crypto)

##############################################################################

install.packages("Hmsic")
library(Hmisc)

rh <- rcorr(as.matrix(Rendimientos_crypto),type="pearson") 
rh # SI HAY CORRELATION YA QUE LOS VALORES ESTAN CERCA DE 1

rh$r

##############################################################################

# REPRESENTATION GRÁFICA DE LAS CORRELACIONES

install.packages("corrplot")

library(corrplot)

corrplot(rh$r, type = "upper", order="hclust", tl.col="black", tl.srt=45)














