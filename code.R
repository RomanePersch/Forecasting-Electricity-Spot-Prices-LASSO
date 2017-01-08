install.packages("glmnet")
install.packages("pbs")
install.packages("pomp")
library(questionr)
library(ggplot2)
library(glmnet)
library(splines)
library(pbs)
library(pomp)
library(moments)

setwd("C:/Users/Alexandre/Documents/projet electricite/")
bdd<-read.csv("C:/Users/Alexandre/Documents/projet electricite/prix spot germany/bdd_finales.csv",sep=";",header=TRUE)
bdd<-bdd[,-1]
bdd$obs<-1:nrow(bdd)
bdd[,5]<- as.numeric(gsub(",",".",bdd[,5]))
bdd[,3]<- as.numeric(gsub(",",".",bdd[,3]))

acf(bdd[,6], lag= 72)
acf(na.omit(bdd[,4]), lag= 72)
diff.prix <- diff(bdd[,6], lag = 1)
diff.vent <- diff(na.omit(bdd[,4]), lag = 1)
acf(diff.vent, lag = 72)
acf(diff.prix, lag = 72)

x<- na.omit(bdd[,4])
skewness(x)
kurtosis(x)
sd(x)
hist(x)

plot(bdd$obs, bdd[,3], type="l")

qplot(bdd[,1], diff.prix)+ geom_line(size=0.2)
qplot(bdd[1:240,5], diff.vent[1:240])+ geom_line(size=0.2)

ggplot(bdd, aes(x=X, y=vent) , xlab = c("2010","2011","2012","2013","2014"),shape=Cond, color=blue) + geom_line()



##################################### etude via b-splines #####################
##########################"" Prix ##########"#####

perio = periodic.bspline.basis(bdd[,6], nbasis = 6, degree = 3, period = 24, names = NULL)
#plot(perio[1:240,1], type="l")

perio_annuel  = periodic.bspline.basis(bdd[,6], nbasis = 12, degree = 3, period = 6264, names = NULL)
#plot(perio_annuel[,12], type="l")

bdd_lasso = cbind(bdd[,5], 1:nrow(bdd), perio,perio_annuel)
bdd_lasso= as.ts(bdd_lasso)
#34 coeff
lags= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,48,72,96,120,240,360,480,600,720,840)

for (u in lags){
  new_vect = lag(bdd[,5], -u)
  bdd_lasso = cbind(bdd_lasso, new_vect)
}

#2 j +1h comme ds l'article
for (u in 1:49){
  new_vect = lag(bdd[,3],  -u)
  bdd_lasso = cbind(bdd_lasso, new_vect)
}

#bspline quotidiens*lag prix
for (u in lags){
  for(i in 1:6)
  {
  new_vect = lag(bdd[,5], -u)*perio[,i]
  bdd_lasso = cbind(bdd_lasso, new_vect)
  }
}

#bspline annuels*lag prix
for (u in lags){
  for(i in 1:12)
  {
    new_vect = lag(bdd[,5], -u)*perio_annuel[,i]
    bdd_lasso = cbind(bdd_lasso, new_vect)
  }
}


df_lasso = data.frame(bdd_lasso)


names <- c("price","temps")
for (k in 1:6){
  nm <- paste("bspline_hebdo",k,sep="")
  names <- c(names,nm)
}


for (k in 1:12){
  nm <- paste("bspline_annuel",k,sep="")
  names <- c(names, nm)
}

for (u in lags){
  nm <- paste("lag",u,sep="")
  names <- c(names,nm)
}

for (u in 1:49){
  nm <- paste("lag_vent",u,sep="")
  names <- c(names,nm)
}

for(u in lags)
 {for (i in 1:6){
  nm <- paste("lag",u, "_bs_quot", i,sep="")
  names <- c(names,nm)
 }
}


for(u in lags)
{for (i in 1:12){
  nm <- paste("lag",u, "_bs_ann", i,sep="")
  names <- c(names,nm)
}
}

colnames(df_lasso) <- names

df_lasso<-na.omit(df_lasso)
x_prix =as.matrix(df_lasso[,2:ncol(df_lasso)])
y_prix =as.matrix(df_lasso[,1])

write.csv(x_prix,"C:/Users/Alexandre/Documents/projet electricite/prix spot germany/x_prix.csv")
write.csv(y_prix,"C:/Users/Alexandre/Documents/projet electricite/prix spot germany/y_prix.csv")
#####################  vent ###############################

perio = periodic.bspline.basis(bdd[,6], nbasis = 6, degree = 3, period = 24, names = NULL)
#plot(perio[1:240,1], type="l")

perio_annuel  = periodic.bspline.basis(bdd[,6], nbasis = 6, degree = 3, period = 6264, names = NULL)
#plot(perio_annuel[,6], type="l")

bdd_lasso =NULL
bdd_lasso = cbind(bdd[,3], 1:nrow(bdd), perio, perio_annuel)
bdd_lasso= as.ts(bdd_lasso)
#36 lags
lags= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,48,49,72,73,96,97,120,121,240,360,480)

for (u in lags){
  new_vect = lag(bdd[,3], -u)
  bdd_lasso = cbind(bdd_lasso, new_vect)
}

#bspline quotidiens*lag prix
for (u in lags){
  for(i in 1:6)
  {
    new_vect = lag(bdd[,3], -u)*perio[,i]
    bdd_lasso = cbind(bdd_lasso, new_vect)
  }
}

#bspline annuels*lag prix
for (u in lags){
  for(i in 1:6)
  {
    new_vect = lag(bdd[,3], -u)*perio_annuel[,i]
    bdd_lasso = cbind(bdd_lasso, new_vect)
  }
}


df_lasso = data.frame(bdd_lasso)
##names

names <- c("vent","temps")
for (k in 1:6){
  nm <- paste("bspline_hebdo",k,sep="")
  names <- c(names,nm)
}


for (k in 1:6){
  nm <- paste("bspline_annuel",k,sep="")
  names <- c(names, nm)
}


for (u in lags){
  nm <- paste("lag",u,sep="")
  names <- c(names,nm)
}

for(u in lags)
{for (i in 1:6){
  nm <- paste("lag",u, "_bs_quot", i,sep="")
  names <- c(names,nm)
}
}

for(u in lags)
{for (i in 1:6){
  nm <- paste("lag",u, "_bs_ann", i,sep="")
  names <- c(names,nm)
}}

colnames(df_lasso) <- names

df_lasso<-na.omit(df_lasso)
x_vent =as.matrix(df_lasso[,2:ncol(df_lasso)])
y_vent =as.matrix(df_lasso[,1])

write.csv(x_vent,"C:/Users/Alexandre/Documents/projet electricite/prix spot germany/x_vent.csv")
write.csv(y_vent,"C:/Users/Alexandre/Documents/projet electricite/prix spot germany/y_vent.csv")


##### evaluation des coeff de régression pour prix 
x_prix_coeff = as.matrix(x_prix[1:26400,]) 
y_prix_coeff = as.matrix(y_prix[1:26400])

cvfit = cv.glmnet(x_prix_coeff , y_prix_coeff, intercept = TRUE)
plot(cvfit)
cvfit$lambda.min
coeffs_prix = coef(cvfit, s = "lambda.min") ## lambda = 0.003451849 -> 2.124466e-05
coeffs_prix = as.matrix(coeffs_prix) #On a un coeff devant trend qui est négatif comme l'article
write.csv(coeffs_prix,"C:/Users/Alexandre/Documents/projet electricite/prix spot germany/coeff_prix.csv")

##### evaluation des coeff de régression pour vent
x_vent_coeff = as.matrix(x_vent[1:26400,]) 
y_vent_coeff = as.matrix(y_vent[1:26400])

cvfit = cv.glmnet(x_vent_coeff, y_vent_coeff, intercept = TRUE)
plot(cvfit)
cvfit$lambda.min
coeffs_vent = coef(cvfit, s = "lambda.min") #### 0.36 ok car 0.3592449/sqrt(26400)->0.002211 graph
coeffs_vent = as.matrix(coeffs_vent) #On a un coeff devant trend qui est négatif comme l'article
write.csv(coeffs_vent,"C:/Users/Alexandre/Documents/projet electricite/prix spot germany/coeff_vent.csv")

#pourcentage non nul
count =0
for(i in 1:nrow(coeffs_vent))
{
  if(coeffs_vent[i,1]>0){count=count+1}
}
show(100*count)
############################ PREDICTION ################
coeffs_prix<-read.csv("C:/Users/Alexandre/Documents/projet electricite/prix spot germany/coeff_prix.csv", sep=",", header=TRUE)
coeffs_vent<-read.csv("C:/Users/Alexandre/Documents/projet electricite/prix spot germany/coeff_vent.csv", sep=",", header=TRUE)
coeffs_prix= as.matrix(coeffs_prix[,-1])
coeffs_vent= as.matrix(coeffs_vent[,-1])


lags1= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,48,72,96,120,240,360,480,600,720,840)
lags2= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,48,49,72,73,96,97,120,121,240,360,480)

y_predicted_prix = y_prix[1:26400]
y_predicted_vent = y_vent[1:26400]

for (h in 1:480){
  
  new_prix_x = matrix(x_prix[26400 + h,1:19], nrow=1)
  new_vent_x = matrix(x_vent[26400 + h,1:13], nrow=1)
  
  for (u in lags1){
    lag_p = y_predicted_prix[26400+h-u]
    new_prix_x = cbind(new_prix_x, lag_p)
  }
  
  for (u in 1:49){
    lag_p = y_predicted_vent[26400+h-u]
    new_prix_x = cbind(new_prix_x, lag_p)
  }
  
  for (u in lags1){
    for( k in 1:6)
    {
    lag_p = y_predicted_prix[26400+h-u]*new_prix_x[k+1]
    new_prix_x = cbind(new_prix_x, lag_p)
    }
  }
  
  for (u in lags1){
    for( k in 1:12)
    {
      lag_p = y_predicted_prix[26400+h-u]*new_prix_x[k+7]
      new_prix_x = cbind(new_prix_x, lag_p)
    }
  }
  
  ###### vent
  
  for (u in lags2){
    lag_p = y_predicted_vent[26400+h-u]
    new_vent_x = cbind(new_vent_x, lag_p)
  }
  
  for (u in lags2){
    for( k in 1:6)
    {
      lag_p = y_predicted_vent[26400+h-u]*new_vent_x[k+1]
      new_vent_x= cbind( new_vent_x, lag_p)
    }
  }
  
  for (u in lags2){
    for( k in 1:6)
    {
      lag_p = y_predicted_vent[26400+h-u]*new_vent_x[k+7]
      new_vent_x= cbind( new_vent_x, lag_p)
    }
  }
  
  new_prix_x = cbind(1, new_prix_x)
  new_vent_x = cbind(1, new_vent_x)
  y_predicted_prix[26400+h] = as.matrix(new_prix_x) %*% coeffs_prix
  y_predicted_vent[26400+h] = as.matrix(new_vent_x) %*% coeffs_vent
}

y_predicted_prix[26401:26880] 
y_predicted_vent[26401:26880] 

y_vrai = as.matrix(y_prix[26401:26880])
y_pred = as.matrix(y_predicted_prix[26401:26880]) 

Y_test = data.frame(y_pred,y_vrai)
ggplot(data=Y_test,aes(x=1:nrow(Y_test))) + geom_line(aes(y=y_vrai,colour="Valeur réelle")) + geom_line(aes(y=y_pred,colour="Prédiction")) + labs(x="heures", y = "prix en Euros", title="Comparaison entre prix réel et prix estimé")

y_vrai_vent = as.matrix(y_vent[26401:26880])
y_pred_vent = as.matrix(y_predicted_vent[26401:26880]) 

Y_test_vent = data.frame(y_pred_vent,y_vrai_vent)
ggplot(data=Y_test,aes(x=1:nrow(Y_test_vent))) + geom_line(aes(y=y_vrai_vent,colour="Valeur réelle")) + geom_line(aes(y=y_pred_vent,colour="Prédiction")) + labs(x="heures", y = "prix en Euros", title="Comparaison entre prix réel et prix estimé")

