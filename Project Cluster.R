#clustering
qb_dist = dist(qb[,-1])
plot(cs <- hclust(qb_dist,method = "single"))

plot(cc <- hclust(qb_dist,method = "complete"))
plot(ca <- hclust(qb_dist,method = "average"))

clus_pca = princomp(qb[,-1],cor=TRUE)
xlim = range(clus_pca$scores[,1])

par(mfrow = c(1,1))
lab = cutree(cc,k = 3)
plot(clus_pca$scores[,1:2],type='n',xlim=xlim,ylim=xlim)
text(clus_pca$scores[,1][group],clus_pca$scores[,2][group],labels=lab[group],cex=.8,col='red')
text(clus_pca$scores[,1][group],clus_pca$scores[,2][!group],labels=lab[group],cex=.8,col='black')


z1 <- v %*% eig$vector[,1]
z2 <- v %*% eig$vector[,2]
plot(z1,z2,xlab="PC1",ylab="PC2",type="n") 
group <- sb == 1 
points(z1[group],z2[group],pch=1,col="red")
points(z1[!group],z2[!group],pch=4,col="blue")
legend("bottomleft",legend=c("Super Bowl Winner","No Super Bowl"),cex=.5,
       col=c("red","blue"),pch=c(1,4))


qb_dist = dist(qb[,-1])
plot(cs <- hclust(qb_dist,method = "single"))

plot(cc <- hclust(qb_dist,method = "complete"))
plot(ca <- hclust(qb_dist,method = "average"))

clus_pca = princomp(qb[,-1],cor=TRUE)
xlim = range(clus_pca$scores[,1])

par(mfrow = c(1,1))
group <- qb2$Age2 
lab = cutree(cc,k = 3)
plot(clus_pca$scores[,1:2],type='n',xlim=xlim,ylim=xlim)
text(clus_pca$scores[,1][group],clus_pca$scores[,2][group],labels = lab[group],cex=.8,col='red')
text(clus_pca$scores[,1][!group],clus_pca$scores[,2][!group],labels=lab[!group],cex=.8,col='black')
legend("topleft",legend=c("Super Bowl Winner","No Super Bowl"),cex=.75,
       col=c("red","black"),pch='|')

lab = cutree(cc,k = 3)
qb2 = qb
qb2$Age2 = factor(cut(qb$Age,c(20,25,30,41)),labels = c("Young","Middle","Old"))
group <- qb2$Age2 
plot(clus_pca$scores[,1:2],type='n',xlim=xlim,ylim=xlim)
text(clus_pca$scores[,1][group=="Young"],clus_pca$scores[,2][group=="Young"],labels = lab[group=="Young"],cex=.8,col='red')
text(clus_pca$scores[,1][group=="Middle"],clus_pca$scores[,2][group=="Middle"],labels=lab[group=="Middle"],cex=.8,col='green')
text(clus_pca$scores[,1][group=="Old"],clus_pca$scores[,2][group=="Old"],labels=lab[group=="Old"],cex=.8,col='blue')
legend("topleft",legend=c("Young","Middle","Old"),cex=.75,
       col=c("red","green","blue"),pch='|')


clus_pca = princomp(qb[,-1],cor=TRUE)
xlim = range(clus_pca$scores[,1])

group <- qb$W.PCT >.5
lab = cutree(cc,k = 3)
plot(clus_pca$scores[,1:2],type='n',xlim=xlim,ylim=xlim)
text(clus_pca$scores[,1][group],clus_pca$scores[,2][group],labels=lab[group],cex=.8,col='red')
text(clus_pca$scores[,1][!group],clus_pca$scores[,2][!group],labels=lab[!group],cex=.8,col='black')
legend("topleft",legend=c("Team is over .500","Team is .500 or below"),cex=.8,
       col=c("red","black"),pch='|')



qb3 = qb
qb3$Cluster = lab
as.character(unlist(qb3[qb3$Cluster==1,"Player"]))
as.character(unlist(qb3[qb3$Cluster==2,"Player"]))
as.character(unlist(qb3[qb3$Cluster==3,"Player"]))

