#Multi Dimensional Scaling
rm(list = ls())
qb = read.csv("QB Merged Data.csv")
qb$X = NULL


D = dist(qb[,-1])

fit = cmdscale(D,eig=T,k=2)
x <- fit$points[, 1]
y <- fit$points[, 2]
group = qb$SB ==1
plot(x, y,type = "n")
text(x[group],y[group],labels = abbreviate(qb$Player)[group],col='red')
text(x[!group],y[!group],labels = abbreviate(qb$Player)[!group],col='blue')

legend("topleft",legend=c("Super Bowl Winners","No Super Bowl"),cex=.75,
       col=c("blue","red"),pch='|')
