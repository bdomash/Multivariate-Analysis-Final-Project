install.packages('openxlsx')
library(openxlsx)

ref = read.xlsx('QB Data.xlsx',sheet = 1)
nfl = read.xlsx('QB Data.xlsx',sheet = 2)
rush = read.xlsx('QB Data.xlsx',sheet = 3)
fum = read.xlsx('QB Data.xlsx',sheet = 4)
adv = read.xlsx('QB Data.xlsx',sheet = 5,startRow = 2)

columns(ref)[1]= "Player"
columns(nfl)[1]= "Player"
columns(rush)[1]= "Player"
columns(fum)[1]= "Player"
columns(adv)[1]= "Player"

ref_keep = c(1,4,6,11:16,18,20,25:28,32,33)
nfl_keep = c(1:5)
rush_keep = c(1:5)
fum_keep = c(1:3)
adv_keep = c(1,3,4,7,17)

ref = ref[,ref_keep]
nfl = nfl[,nfl_keep]
rush = rush[,rush_keep]
fum = fum[,fum_keep]
adv = adv[,adv_keep]

data = merge(ref, nfl, by = "Player", all.x = TRUE)
data = merge(data,rush,all.x = TRUE)
data = merge(data,fum,all.x = TRUE)
data = merge(data,adv,all.x = TRUE)

data[is.na(data)] = 0

colnames(data)[12] = "QB.Rating"
colnames(data)[14] = "Sacks"
colnames(data)[15] = "Sack.Yds"
colnames(data)[16] = "4Q.Comebacks"
colnames(data)[17] = "Game.Winning.Drives"
colnames(data)[22] = "Rush.Att"
colnames(data)[23] = "Rush.Yds"
colnames(data)[24] = "Rush.TD"
colnames(data)[25] = "Rush.Y/G"
colnames(data)[26] = "Fum.Lost"
colnames(data)[27] = "Rush.Y/G"
colnames(data)[28] = "Time.to.Throw"
colnames(data)[29] = "Comp.Air.Yds"
colnames(data)[31] = "Comp%.Abv.Exp"


write.csv(data,'QB Merged Data.csv')
