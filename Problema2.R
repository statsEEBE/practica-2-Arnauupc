#Codigo para problema 2

mis_dades <- iris
mis_dades



# buscar pendent

x <- mis_dades$Petal.Length
x
y <- mis_dades$Sepal.Length
y

plot(x,y)

x_bar <- mean(x)
y_bar <- mean(y)

#pendent de la recta
m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
m
#trobar b
b <- y_bar-m*x_bar
b

#prediccio petallenght 1.5
m*1.5+b


#com fer la linia que passara per tots els valors x
x_pred <- x
x_pred
y_pred <- m*x_pred+b

plot(x, y)
lines(x_pred, y_pred)

#calcular el coeficient de determinacio
Rsq <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)
Rsq
#coeficient de correlacio
cor <- sqrt(Rsq)
cor

#linear regression
mod <- lm(y~x)
mod
#taula que et diu la informacio de la pendent la interccecio i R
summary(mod)

#Una altre manera de calcular R
cor.test(x,y)

y_pred2 <- predict(mod, data.frame(x=x))
y_pred2
y_pred1.5 <- predict(mod, data.frame(x=1.5))
y_pred1.5
#Codigo para problema 2
