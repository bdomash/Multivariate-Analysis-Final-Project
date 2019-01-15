rm(list = ls())

qb = read.csv("QB Merged Data.csv")

qb$X = NULL




#-pg85: make data such that larger is "better"
row.names(qb) = abbreviate(qb$Player)
pass = qb[,passing]

qbn = qb[,sapply(qb, is.numeric)]

qbn$Int = with(qbn, max(Int)-Int)
qbn$Sacks = with(qbn, max(Sacks)-Sacks)
qbn$Fumbles = with(qbn, max(Fumbles)-Fumbles)

pca = princomp(pcas,cor=T)
summary(pca,loadings = T)

#PCA of data

passing_pca = princomp(pass,cor = T)
summary(passing_pca,loadings = T)

rushing_pca = princomp(qb[,rushing],cor = T)
summary(rushing_pca,loadings = T)

n = qb[,negative]
n$Int = with(qbn, max(Int)-Int)
n$Sacks = with(qbn, max(Sacks)-Sacks)
n$Fumbles = with(qbn, max(Fumbles)-Fumbles)
negative_pca = princomp(n,cor = T)

summary(rushing_pca,loadings = T)

app_pca = princomp(qb[,aptitude],cor = T)
summary(app_pca,loadings = T)

pcas = cbind(passing_pca$scores[,1],rushing_pca$scores[,1],negative_pca$scores[,1],app_pca$scores[,1])
colnames(pcas) = c("passing","rushing","turnovers","Aptitude")
pairs(pcas)
plot(rushing_pca$scores[,1],app_pca$scores[,1],xlab="PC1",ylab="PC2",type='n')
text(rushing_pca$scores[,1],app_pca$scores[,1],cex=.5,labels=abbreviate(qb$Player))


plot(pca$scores[,1],pca$scores[,2],xlab="PC1",ylab="PC2",type='n')
text(pca$scores[,1],pca$scores[,2],cex=.5,labels=abbreviate(qb$Player))

group = qbn$Age>30
plot(pca$scores[,1],pca$scores[,2],xlab="PC1",ylab="PC2",type='n')

points(pca$scores[,1][group],pca$scores[,2][group],pch=1,col='red')
points(pca$scores[,1][!group],pca$scores[,2][!group],pch=4,col='blue')
legend('bottomleft',legend=c("Age 30+","Under 30"),col=c("Red","Blue"),pch=c(1,4))


plot(pca$scores[,1],pca$scores[,3],xlab="PC1",ylab="PC3",type='n')
text(pca$scores[,1],pca$scores[,3],cex=.5,labels=abbreviate(qb$Player))

plot(pca$scores[,2],pca$scores[,3],xlab="PC2",ylab="PC3",type='n')
text(pca$scores[,2],pca$scores[,3],cex=.5,labels=abbreviate(qb$Player))

#Cor between PCA and QBR
pca2 = prcomp(qbn,scale. = T)
summary(pca2)
pca2$rotation[,1]
cor(pca2$x[,1],qb$QB.Rating)
plot(pca2$x[,1],qb$QBR)


#Scatterplot matrix of first-3 PCA and bivariate boxplot

library(MVA)
r <- range(pca$scores[,1:3])
pairs(pca$scores[,1:3],xlim=r,ylim=r,
      panel=function(x,y,...){
        text(x,y,abbreviate(qb$Player),cex=.85,col="red")
        bvbox(cbind(x,y),add=TRUE)
      })

#Predict W%
qb2 = qb
rownames(qb2) = abbreviate(qb2$Player)
qb2 = qb2[,sapply(qb2, is.numeric)]
qb2$Int = with(qb2, max(Int)-Int)
qb2$Sacks = with(qb2, max(Sacks)-Sacks)
qb2$Sack.Yds = with(qb2, max(Sack.Yds)-Sack.Yds)
qb2 = qb2[,-c(3,25,26)] #removing QBR,QBr, and win%

pca3 = princomp(qb2,cor = T)

summary(pca3,loadings = T)
r <- range(pca$scores[,1:3])
pairs(pca3$scores[,1:3],xlim=r,ylim=r,
      panel=function(x,y,...){
        text(x,y,abbreviate(qb$Player),cex=.85,col="red")
        bvbox(cbind(x,y),add=TRUE)
      })

plot(pca3$scores[,1],qbn$QBR)
text(pca3$scores[,1],qbn$QBR,abbreviate(qb$Player),cex=.85,col="red")
cor(pca3$scores[,1],qbn$QBR)

win_pct = lm(qbn$QBR~pca3$scores)
summary(qbr_reg)


biplot(pca4,col=c("red","black"))

#PCA on throwing plays
colnames(qb)
throwing = qb
rownames(throwing) = abbreviate(qb$Player)
throwing = throwing[,c(5,6,7,8,9,10)]
pca4 = princomp(throwing,cor = T)
summary(pca4,loadings = T)
plot(pca4$scores[,1],pca4$scores[,2],xlab="PC1",ylab="PC2",type='n')
text(pca4$scores[,1],pca4$scores[,2],cex=.5,labels=abbreviate(qb$Player))

#PCA on negative plays
bad = qb
colnames(bad)
rownames(bad) = abbreviate(qb$Player)
bad = bad[,c(10,12,13,24,25)]
pca5 = princomp(bad,cor = T)
summary(pca5,loadings = T)
plot(pca5$scores[,1],pca5$scores[,2],xlab="PC1",ylab="PC2",type='n')
text(pca5$scores[,1],pca5$scores[,2],cex=.5,labels=abbreviate(qb$Player))

#CCA -> don't use
#Is there a relationship between positive throwing stats (positive) and turnovers (negative)

ca = qb
rownames(ca) = abbreviate(ca$Player)
colnames(ca)
ca = ca[,c(5,7,8,9,10,12,24)]
round(cor(ca),3)

#cor matrix
plot(ca)
cormat = round(cor(ca),2)
cormat[upper.tri(cormat)] = NA
head(cormat)
melted_cormat = melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color='white')+
  geom_text(aes(Var1,Var2,label=value),color='black',size=4)+
  scale_fill_gradient2(low="blue",high='red',mid = 'white',midpoint=0,limit=c(-1,1),space='Lab')

colnames(ca)
x1 = with(ca,scale(Cmp))
x2 = with(ca,scale(Cmp.PCT))
x3 = with(ca,scale(Yds))
x4 = with(ca,scale(TD))
y1 = with(ca,scale(Int))
y2 = with(ca,scale(Sacks))
y3 = with(ca,scale(Fumbles))
round(cor(cbind(x1,x2,x3,x4,y1,y2,y3)),3)
cormat = round(cor(cbind(x1,x2,x3,x4,y1,y2,y3)),3)
cormat[upper.tri(cormat)] = NA
melted_cormat = melt(cormat)
ggplot(data=melted_cormat,aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color='white')+
  geom_text(aes(Var1,Var2,label=value),color='black',size=4)+
  scale_fill_gradient2(low="blue",high='red',mid = 'white',midpoint=0,limit=c(-1,1),space='Lab')

cormat
r11 <- cormat[1:4,1:4]
r12 <- cormat[1:4,5:7]
r22 <- cormat[5:7,5:7]
r21 <- t(r12)
e1 <- solve(r11) %*% r12 %*% solve(r22) %*% r21
e2 <- solve(r22) %*% r21 %*% solve(r11) %*% r12
eigen(e1)
eigen(e2)
#Hard interpretation; highly correlated variables


#####CLUSTER####
lab = cutree(cc,k = 3)
c1 = qb[lab==1,-1]
p1 = prcomp(c1,scale. = T)
summary(p1)

c2 = qb[lab==2,-1]
p2 = prcomp(c1,scale. = T)
summary(p2)

c3 = qb[lab==3,-1]
p3 = prcomp(c1,scale. = T)
summary(p3)

