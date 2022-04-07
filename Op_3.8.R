rm(list = ls())

#assumes auto.csv is in your working directory.
auto = read.csv("auto.csv") 
lm_auto= lm(auto$mpg ~ auto$horsepower, data =auto)
summary(lm_auto)

#a.i) Sí existe una relación lineal, el P-value=2e-16 rechaza hipótesis de que no existe relación lineal.
#a.ii) El estadístico R^2(0.6059) indica que existe una relación del 60% entre el predictor y la variable de respuesta. 
#a.iii) La relación es negativa, esta indicado con el signo del coeficiente  de relación estimado -0.157845 
#a.iv) 24.46416, [14.9671249, 34.28272]
new_data = data.frame(horsepower= c(98, 0, 90))
mpg_98=predict(lm_auto, newdata=new_data, interval="predict") 
plot(auto$mpg,auto$horsepower)
# standard line of best fit - black line
abline(lm(auto$mpg ~ auto$horsepower, data =auto))

#EJ 3.9
#a)
pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year + origin , data= auto, lower.panel=panel.smooth)
#b)
auto_noname=auto[,-9]
mydata.cor = cor(auto_noname)
#c
lm_auto_mult= lm(auto$mpg ~ auto$cylinders + auto$displacement + auto$horsepower + auto$weight+ auto$acceleration+ auto$year + auto$origin , data =auto)
summary(lm_auto_mult)
#i)Sí
#ii)Las variables: displacement,weight, year y origin
#iii) Es estadísticamente significativa

#Ej. 3.10
library(ISLR)
attach(Carseats)
carseats <- Carseats
#a
lm_carseats_mult= lm(carseats$Sales ~ carseats$Price + carseats$Urban + carseats$US, data =carseats)
summary(lm_carseats_mult)
#b) Relación negativa para Price y Urabn; relación positiva para US 
#c) Sales= 13.043469 -0.054459*Price + a*Urban + b*US 
# Donde:Urban-No a=0, Para Urban-Yes a=-0.021916
# Donde: US-No b=0, Para US-Yes b=1.200573
#d) Variables Price y US-Yes son estadisticamente significativas
#e)
lm_carseats_mult2= lm(carseats$Sales ~ carseats$Price + carseats$US, data =carseats)
summary(lm_carseats_mult2)
#f) R^2 modelo1= 0.2393, R^2 modelo2=0.2393
#g)
price_95=cbind(CIlower = mean(carseats$Price) - 1.96 * 5 / 10, CIupper = mean(carseats$Price) + 1.96 * 5 / 10)
sales_95=cbind(CIlower = mean(carseats$Sales) - 1.96 * 5 / 10, CIupper = mean(carseats$Sales) + 1.96 * 5 / 10)