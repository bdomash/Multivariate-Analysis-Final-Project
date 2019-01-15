setwd('Documents/COLLEGE/_Senior Fall/Stat 456')
qb = read.csv("QB Merged Data.csv",stringsAsFactors = F)
qb$X = NULL

#circle plot; dis4
plot(Yds~Rush.Yds,data=qb, pch=10,
     ylab = "Passing Yards", xlab = "Rushing Yards",main="Passing Yards vs Rushing Yards, with respect to Age")
with(qb,symbols(Rush.Yds,Yds,circles = Age, inches = .2, add=T))

library(tidyverse)
ggplot(data=qb) + 
  geom_point(mapping=aes(x=Int,y=TD,color=Age)) +
  scale_color_gradient(low="blue", high="red")


age = qb



age$tier = "Young"
age[age$Age>25 & age$Age<=30,]$tier = "Middle"
age[age$Age>30,]$tier = "Old"

ggplot(data=age) + 
  geom_point(mapping=aes(x=TD,y=Int)) +
  facet_wrap(~tier)
factor(cut(qb$Age,c(20,25,30,41)),labels = c("Young","Middle","Old"))
library("lattice")
plot(xyplot(Yds ~ Cmp.PCT | cut(Age,c(20,25,30,41)), data=qb,layout=c(3,1)))
?cut
#Corr matrix
library(reshape2)
passing = c("Cmp.PCT","Yds","TD","First.DownPCT","Passes20Yd")
plot(qb[,passing])
boxplot(qb$W.PCT) #CJ Beathard outlier alert
boxplot(qb$Cmp.PCT) #Josh Allen Outler
cormat = round(cor(qb[,passing]),2)
cormat[upper.tri(cormat)] = NA
head(cormat)
melted_cormat = melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color='white')+
  geom_text(aes(Var1,Var2,label=value),color='black',size=4)+
  scale_fill_gradient2(low="blue",high='red',mid = 'white',midpoint=0,limit=c(-1,1),space='Lab')


negative = c("Sacks","Int","Fumbles")
plot(qb[,negative])
cormat = round(cor(qb[,negative]),2)
cormat[upper.tri(cormat)] = NA
head(cormat)
melted_cormat = melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color='white')+
  geom_text(aes(Var1,Var2,label=value),color='black',size=4)+
  scale_fill_gradient2(low="blue",high='red',mid = 'white',midpoint=0,limit=c(-1,1),space='Lab')

rushing = c("Rush.Att","Rush.Yds","Rush.TD")
plot(qb[,rushing])
cormat = round(cor(qb[,rushing]),2)
cormat[upper.tri(cormat)] = NA
head(cormat)
melted_cormat = melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color='white')+
  geom_text(aes(Var1,Var2,label=value),color='black',size=4)+
  scale_fill_gradient2(low="blue",high='red',mid = 'white',midpoint=0,limit=c(-1,1),space='Lab')


aptitude = c("QBR","QB.Rating","SB","W.PCT")
plot(qb[,aptitude])
cormat = round(cor(qb[,aptitude]),2)
cormat[upper.tri(cormat)] = NA
head(cormat)
melted_cormat = melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color='white')+
  geom_text(aes(Var1,Var2,label=value),color='black',size=4)+
  scale_fill_gradient2(low="blue",high='red',mid = 'white',midpoint=0,limit=c(-1,1),space='Lab')

cols = c("Age","QBR","QB.Rating","SB","W.PCT")
#Bivariate Boxplot
library(MVA)
x = qb[, c("Age","TD")]
lab <- c("Patrick Mahomes")
outliers <- match(lab,qb$Player)

bvbox(x,xlab='Age',y='Touchdowns')
text(x$Age[outliers],x$TD[outliers],labels=abbreviate(lab),cex=1,
     pos=c(2))

#Hist, Boxplot, Scatter
names = abbreviate(qb$Player)
dev.off()
par(cex.lab=.8,cex.axis=.8,cex.main=1)
par(mar=c(3,3,3,3))
par(fig=c(0,0.7,0,0.7))
plot(TD~Age,data=qb,type='n')
text(qb$Age,qb$TD,cex=.5,labels=abbreviate(qb$Player))
par(fig=c(0,0.7,0.65,1),new=TRUE)
hist(qb$Age,main="")
par(fig=c(0.65,1,0,0.7),new=TRUE)
with(qb,boxplot(TD,horizontal=FALSE))

#Star Plot
#stars right, winds counterclockwise
stars(qb[,c('TD','Int','Sacks')],cex=.5,labels = qb$Player)

#Scatterplot matrix
cols = c("Sacks","Sack.Yds","Int","Fumbles","Fum.Lost")
pairs(qb[,cols],pch='o',panel=function(x,y,...){
  points(x,y,...)
  abline(lm(y ~ x),col="red")})

#cut
library(lattice)
plot(xyplot(qb$Sacks~qb$Rush.Yds|cut(qb$Age,3)),data=qb)



