library(dplyr)
library(readr)
library(ggplot2)
library(readxl)
library(colorspace)
library(corrgram)
library(ellipse)
library(GGally)
library(MASS)
library(missForest)
library(leaps)
library(car)
library("SignifReg")



# Time and Space

## index weight
subsystem.weight = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/weight_subsystem.csv") 
econ.weight = as.numeric(subsystem.weight[1, 2:12])
socia.weight = as.numeric(subsystem.weight[2, 13:22])
eco.weight = as.numeric(subsystem.weight[3, 23:25])

## Subsystem Score
## Preprocess
tianjin.data = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/tianjinFilled.csv")
shanxi.data = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/shanxiFilled.csv")
beijing.data = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/beijingFilled.csv")
shandong.data = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/shandongFilled.csv")
henan.data = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/henanFilled.csv")
hebei.data = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/hebeiFilled.csv")

### scale the indices by positive/negative
pn = rep(1, 24)
pn[13] = pn[18] = -1
year = 0:20

tianjin.scale = tianjin.data
for(i in 2 : 25)
{
  min = min(tianjin.data[,i], na.rm = T)
  max = max(tianjin.data[,i], na.rm = T)
  if(pn[i - 1] == 1)
  {
    tianjin.scale[,i] = (tianjin.data[,i] - min)/(max - min)
  }
  else
  {
    tianjin.scale[,i] = (max - tianjin.data[,i])/(max - min)
  }
}

shanxi.scale = shanxi.data
for(i in 2 : 25)
{
  min = min(shanxi.data[,i], na.rm = T)
  max = max(shanxi.data[,i], na.rm = T)
  if(pn[i - 1] == 1)
  {
    shanxi.scale[,i] = (shanxi.data[,i] - min)/(max - min)
  }
  else
  {
    shanxi.scale[,i] = (max - shanxi.data[,i])/(max - min)
  }
}


beijing.scale = beijing.data
for(i in 2 : 25)
{
  min = min(beijing.data[,i], na.rm = T)
  max = max(beijing.data[,i], na.rm = T)
  if(pn[i - 1] == 1)
  {
    beijing.scale[,i] = (beijing.data[,i] - min)/(max - min)
  }
  else
  {
    beijing.scale[,i] = (max - beijing.data[,i])/(max - min)
  }
}


shandong.scale = shandong.data
for(i in 2 : 25)
{
  min = min(shandong.data[,i], na.rm = T)
  max = max(shandong.data[,i], na.rm = T)
  if(pn[i - 1] == 1)
  {
    shandong.scale[,i] = (shandong.data[,i] - min)/(max - min)
  }
  else
  {
    shandong.scale[,i] = (max - shandong.data[,i])/(max - min)
  }
}


henan.scale = henan.data
for(i in 2 : 25)
{
  min = min(henan.data[,i], na.rm = T)
  max = max(henan.data[,i], na.rm = T)
  if(pn[i - 1] == 1)
  {
    henan.scale[,i] = (henan.data[,i] - min)/(max - min)
  }
  else
  {
    henan.scale[,i] = (max - henan.data[,i])/(max - min)
  }
}

hebei.scale = hebei.data
for(i in 2 : 25)
{
  min = min(hebei.data[,i], na.rm = T)
  max = max(hebei.data[,i], na.rm = T)
  if(pn[i - 1] == 1)
  {
    hebei.scale[,i] = (hebei.data[,i] - min)/(max - min)
  }
  else
  {
    hebei.scale[,i] = (max - hebei.data[,i])/(max - min)
  }
}



### beijing subsystem score

beijing.eco_score = rep(0, length(beijing.data[,2]))
for(i in 1:length(beijing.data[,2]))
{
  for(j in 1:3)
  {
    beijing.eco_score[i] = beijing.eco_score[i] + beijing.scale[i,j + 22] * eco.weight[j]
  }
}

beijing.econ_score = rep(0, length(beijing.data[,2]))
for(i in 1:length(beijing.data[,2]))
{
  for(j in 1:length(econ.weight))
  {
    beijing.econ_score[i] = beijing.econ_score[i] + beijing.scale[i,j + 1] * econ.weight[j]
  }
}

beijing.socia_score = rep(0, length(beijing.data[,2]))
for(i in 1:length(beijing.data[,2]))
{
  for(j in 1:length(socia.weight))
  {
    beijing.socia_score[i] = beijing.socia_score[i] + beijing.scale[i,j + 12] * socia.weight[j]
  }
}

beijing.subscore = data.frame(row.names = 0:20)
beijing.subscore$beijing.eco_score = beijing.eco_score
beijing.subscore$beijing.econ_score = beijing.econ_score
beijing.subscore$beijing.socia_score = beijing.socia_score

ggplot(data = beijing.subscore, aes(x = year, y = beijing.eco_score)) + geom_smooth() + geom_point()
ggplot(data = beijing.subscore, aes(x = year, y = beijing.econ_score)) + geom_smooth() + geom_point()
ggplot(data = beijing.subscore, aes(x = year, y = beijing.socia_score)) + geom_smooth() + geom_point()

write.csv(beijing.subscore, file = "beijing_subscore.csv")

### tianjin subsystem score

tianjin.eco_score = rep(0, length(tianjin.data[,2]))
for(i in 1:length(tianjin.data[,2]))
{
  for(j in 1:3)
  {
    tianjin.eco_score[i] = tianjin.eco_score[i] + tianjin.scale[i,j + 22] * eco.weight[j]
  }
}

tianjin.econ_score = rep(0, length(tianjin.data[,2]))
for(i in 1:length(tianjin.data[,2]))
{
  for(j in 1:length(econ.weight))
  {
    tianjin.econ_score[i] = tianjin.econ_score[i] + tianjin.scale[i,j + 1] * econ.weight[j]
  }
}

tianjin.socia_score = rep(0, length(tianjin.data[,2]))
for(i in 1:length(tianjin.data[,2]))
{
  for(j in 1:length(socia.weight))
  {
    tianjin.socia_score[i] = tianjin.socia_score[i] + tianjin.scale[i,j + 12] * socia.weight[j]
  }
}

tianjin.subscore = data.frame(row.names = 0:20)
tianjin.subscore$tianjin.eco_score = tianjin.eco_score
tianjin.subscore$tianjin.econ_score = tianjin.econ_score
tianjin.subscore$tianjin.socia_score = tianjin.socia_score

write.csv(tianjin.subscore, file = "tianjin_subscore.csv")

### hebei subsystem score

hebei.eco_score = rep(0, length(hebei.data[,2]))
for(i in 1:length(hebei.data[,2]))
{
  for(j in 1:3)
  {
    hebei.eco_score[i] = hebei.eco_score[i] + hebei.scale[i,j + 22] * eco.weight[j]
  }
}

hebei.econ_score = rep(0, length(hebei.data[,2]))
for(i in 1:length(hebei.data[,2]))
{
  for(j in 1:length(econ.weight))
  {
    hebei.econ_score[i] = hebei.econ_score[i] + hebei.scale[i,j + 1] * econ.weight[j]
  }
}

hebei.socia_score = rep(0, length(hebei.data[,2]))
for(i in 1:length(hebei.data[,2]))
{
  for(j in 1:length(socia.weight))
  {
    hebei.socia_score[i] = hebei.socia_score[i] + hebei.scale[i,j + 12] * socia.weight[j]
  }
}

hebei.subscore = data.frame(row.names = 0:20)
hebei.subscore$hebei.eco_score = hebei.eco_score
hebei.subscore$hebei.econ_score = hebei.econ_score
hebei.subscore$hebei.socia_score = hebei.socia_score

write.csv(hebei.subscore, file = "hebei_subscore.csv")



### shanxi subsystem score


shanxi.eco_score = rep(0, length(shanxi.data[,2]))
for(i in 1:length(shanxi.data[,2]))
{
  for(j in 1:3)
  {
    shanxi.eco_score[i] = shanxi.eco_score[i] + shanxi.scale[i,j + 22] * eco.weight[j]
  }
}

shanxi.econ_score = rep(0, length(shanxi.data[,2]))
for(i in 1:length(shanxi.data[,2]))
{
  for(j in 1:length(econ.weight))
  {
    shanxi.econ_score[i] = shanxi.econ_score[i] + shanxi.scale[i,j + 1] * econ.weight[j]
  }
}

shanxi.socia_score = rep(0, length(shanxi.data[,2]))
for(i in 1:length(shanxi.data[,2]))
{
  for(j in 1:length(socia.weight))
  {
    shanxi.socia_score[i] = shanxi.socia_score[i] + shanxi.scale[i,j + 12] * socia.weight[j]
  }
}

shanxi.subscore = data.frame(row.names = 0:20)
shanxi.subscore$shanxi.eco_score = shanxi.eco_score
shanxi.subscore$shanxi.econ_score = shanxi.econ_score
shanxi.subscore$shanxi.socia_score = shanxi.socia_score

write.csv(shanxi.subscore, file = "shanxi_subscore.csv")


### shandong subsystem score


shandong.eco_score = rep(0, length(shandong.data[,2]))
for(i in 1:length(shandong.data[,2]))
{
  for(j in 1:3)
  {
    shandong.eco_score[i] = shandong.eco_score[i] + shandong.scale[i,j + 22] * eco.weight[j]
  }
}

shandong.econ_score = rep(0, length(shandong.data[,2]))
for(i in 1:length(shandong.data[,2]))
{
  for(j in 1:length(econ.weight))
  {
    shandong.econ_score[i] = shandong.econ_score[i] + shandong.scale[i,j + 1] * econ.weight[j]
  }
}

shandong.socia_score = rep(0, length(shandong.data[,2]))
for(i in 1:length(shandong.data[,2]))
{
  for(j in 1:length(socia.weight))
  {
    shandong.socia_score[i] = shandong.socia_score[i] + shandong.scale[i,j + 12] * socia.weight[j]
  }
}


shandong.subscore = data.frame(row.names = 0:20)
shandong.subscore$shandong.eco_score = shandong.eco_score
shandong.subscore$shandong.econ_score = shandong.econ_score
shandong.subscore$shandong.socia_score = shandong.socia_score

write.csv(shandong.subscore, file = "shandong_subscore.csv")









### henan subsystem score


henan.eco_score = rep(0, length(henan.data[,2]))
for(i in 1:length(henan.data[,2]))
{
  for(j in 1:3)
  {
    henan.eco_score[i] = henan.eco_score[i] + henan.scale[i,j + 22] * eco.weight[j]
  }
}

henan.econ_score = rep(0, length(henan.data[,2]))
for(i in 1:length(henan.data[,2]))
{
  for(j in 1:length(econ.weight))
  {
    henan.econ_score[i] = henan.econ_score[i] + henan.scale[i,j + 1] * econ.weight[j]
  }
}

henan.socia_score = rep(0, length(henan.data[,2]))
for(i in 1:length(henan.data[,2]))
{
  for(j in 1:length(socia.weight))
  {
    henan.socia_score[i] = henan.socia_score[i] + henan.scale[i,j + 12] * socia.weight[j]
  }
}


henan.subscore = data.frame(row.names = 0:20)
henan.subscore$henan.eco_score = henan.eco_score
henan.subscore$henan.econ_score = henan.econ_score
henan.subscore$henan.socia_score = henan.socia_score

write.csv(henan.subscore, file = "henan_subscore.csv")


### Plot the Subsystem Score
year = 0:20
Subsystem.Score_eco = data.frame(row.names = year)
Subsystem.Score_econ = data.frame(row.names = year)
Subsystem.Score_socia = data.frame(row.names = year)

Subsystem.Score_eco$beijing.eco_score = beijing.eco_score
Subsystem.Score_eco$tianjin.eco_score = tianjin.eco_score
Subsystem.Score_eco$hebei.eco_score = hebei.eco_score
Subsystem.Score_eco$shanxi.eco_score = shanxi.eco_score
Subsystem.Score_eco$shandong.eco_score = shandong.eco_score
Subsystem.Score_eco$henan.eco_score = henan.eco_score

Subsystem.Score_econ$beijing.econ_score = beijing.econ_score
Subsystem.Score_econ$tianjin.econ_score = tianjin.econ_score
Subsystem.Score_econ$hebei.econ_score = hebei.econ_score
Subsystem.Score_econ$shanxi.econ_score = shanxi.econ_score
Subsystem.Score_econ$shandong.econ_score = shandong.econ_score
Subsystem.Score_econ$henan.econ_score = henan.econ_score

Subsystem.Score_socia$beijing.socia_score = beijing.socia_score
Subsystem.Score_socia$tianjin.socia_score = tianjin.socia_score
Subsystem.Score_socia$hebei.socia_score = hebei.socia_score
Subsystem.Score_socia$shanxi.socia_score = shanxi.socia_score
Subsystem.Score_socia$shandong.socia_score = shandong.socia_score
Subsystem.Score_socia$henan.socia_score = henan.socia_score



###Plot it!
ggplot(data = Subsystem.Score_eco, aes(x = year, y = beijing.eco_score))


## total score
subweight = c(0.320144, 0.333288, 0.346568)

### beijing total score
beijing.tot_score = rep(0, length(beijing.data[,2]))
for (i in 1:length(beijing.data[,2]))
{
  beijing.tot_score[i] = subweight[1] * beijing.econ_score[i] + subweight[2] * beijing.socia_score[i] + subweight[3] * beijing.eco_score[i]
}

beijing.subscore$beijing.tot_score = beijing.tot_score

write.csv(beijing.subscore, file = "beijing_subscore.csv")


### tianjin total score
tianjin.tot_score = rep(0, length(tianjin.data[,2]))
for (i in 1:length(tianjin.data[,2]))
{
  tianjin.tot_score[i] = subweight[1] * tianjin.econ_score[i] + subweight[2] * tianjin.socia_score[i] + subweight[3] * tianjin.eco_score[i]
}

tianjin.subscore$tianjin.tot_score = tianjin.tot_score

write.csv(tianjin.subscore, file = "tianjin_subscore.csv")


### hebei total score
hebei.tot_score = rep(0, length(hebei.data[,2]))
for (i in 1:length(hebei.data[,2]))
{
  hebei.tot_score[i] = subweight[1] * hebei.econ_score[i] + subweight[2] * hebei.socia_score[i] + subweight[3] * hebei.eco_score[i]
}

hebei.subscore$hebei.tot_score = hebei.tot_score

write.csv(hebei.subscore, file = "hebei_subscore.csv")

### shanxi total score
shanxi.tot_score = rep(0, length(shanxi.data[,2]))
for (i in 1:length(shanxi.data[,2]))
{
  shanxi.tot_score[i] = subweight[1] * shanxi.econ_score[i] + subweight[2] * shanxi.socia_score[i] + subweight[3] * shanxi.eco_score[i]
}

shanxi.subscore$shanxi.tot_score = shanxi.tot_score

write.csv(shanxi.subscore, file = "shanxi_subscore.csv")

### shandong total score
shandong.tot_score = rep(0, length(shandong.data[,2]))
for (i in 1:length(shandong.data[,2]))
{
  shandong.tot_score[i] = subweight[1] * shandong.econ_score[i] + subweight[2] * shandong.socia_score[i] + subweight[3] * shandong.eco_score[i]
}

shandong.subscore$shandong.tot_score = shandong.tot_score

write.csv(shandong.subscore, file = "shandong_subscore.csv")

### henan total score
henan.tot_score = rep(0, length(henan.data[,2]))
for (i in 1:length(henan.data[,2]))
{
  henan.tot_score[i] = subweight[1] * henan.econ_score[i] + subweight[2] * henan.socia_score[i] + subweight[3] * henan.eco_score[i]
}

henan.subscore$henan.tot_score = henan.tot_score

write.csv(henan.subscore, file = "henan_subscore.csv")


## calculate the coherence
year = 0 : 20

### beijing
beijing.cohe = rep(0,length(beijing.data[,1]))
for(i in 1 : length(beijing.data[,1]))
{
  beijing.cohe[i] = 3 * (beijing.econ_score[i] * beijing.socia_score[i] * beijing.eco_score[i])^(1/3)/(beijing.econ_score[i] + beijing.socia_score[i] + beijing.eco_score[i])
}

beijing.decohe = 10000 * (1 - beijing.cohe)

beijing.d_cohe = sqrt(beijing.cohe*beijing.tot_score)

ggplot(data = NULL, aes(x = year, y = beijing.decohe)) + geom_smooth() + geom_point()

ggplot(data = NULL, aes(x = year, y = beijing.d_cohe)) + geom_smooth() + geom_point()

beijing.subscore$beijing.d_cohe = beijing.d_cohe
write.csv(beijing.subscore, file = "beijing_subscore.csv")

### tianjin
tianjin.cohe = rep(0,length(tianjin.data[,1]))
for(i in 1 : length(tianjin.data[,1]))
{
  tianjin.cohe[i] = 3 * (tianjin.econ_score[i] * tianjin.socia_score[i] * tianjin.eco_score[i])^(1/3)/(tianjin.econ_score[i] + tianjin.socia_score[i] + tianjin.eco_score[i])
}

tianjin.decohe = 10000 * (1 - tianjin.cohe)

tianjin.d_cohe = sqrt(tianjin.cohe*tianjin.tot_score)

ggplot(data = NULL, aes(x = year, y = tianjin.decohe)) + geom_smooth() + geom_point()

ggplot(data = NULL, aes(x = year, y = tianjin.d_cohe)) + geom_smooth() + geom_point()

tianjin.subscore$tianjin.d_cohe = tianjin.d_cohe

write.csv(tianjin.subscore, file = "tianjin_subscore.csv")

### hebei
hebei.cohe = rep(0,length(hebei.data[,1]))
for(i in 1 : length(hebei.data[,1]))
{
  hebei.cohe[i] = 3 * (hebei.econ_score[i] * hebei.socia_score[i] * hebei.eco_score[i])^(1/3)/(hebei.econ_score[i] + hebei.socia_score[i] + hebei.eco_score[i])
}

hebei.decohe = 10000 * (1 - hebei.cohe)

hebei.d_cohe = sqrt(hebei.cohe*hebei.tot_score)

ggplot(data = NULL, aes(x = year, y = hebei.decohe)) + geom_smooth() + geom_point()

ggplot(data = NULL, aes(x = year, y = hebei.d_cohe)) + geom_smooth() + geom_point()

hebei.subscore$hebei.d_cohe = hebei.d_cohe

write.csv(hebei.subscore, file = "hebei_subscore.csv")

### shanxi
shanxi.cohe = rep(0,length(shanxi.data[,1]))
for(i in 1 : length(shanxi.data[,1]))
{
  shanxi.cohe[i] = 3 * (shanxi.econ_score[i] * shanxi.socia_score[i] * shanxi.eco_score[i])^(1/3)/(shanxi.econ_score[i] + shanxi.socia_score[i] + shanxi.eco_score[i])
}

shanxi.decohe = 10000 * (1 - shanxi.cohe)

shanxi.d_cohe = sqrt(shanxi.cohe*shanxi.tot_score)

ggplot(data = NULL, aes(x = year, y = shanxi.decohe)) + geom_smooth() + geom_point()

ggplot(data = NULL, aes(x = year, y = shanxi.d_cohe)) + geom_smooth() + geom_point()

shanxi.subscore$shanxi.d_cohe = shanxi.d_cohe

write.csv(shanxi.subscore, file = "shanxi_subscore.csv")


### shandong
shandong.cohe = rep(0,length(shandong.data[,1]))
for(i in 1 : length(shandong.data[,1]))
{
  shandong.cohe[i] = 3 * (shandong.econ_score[i] * shandong.socia_score[i] * shandong.eco_score[i])^(1/3)/(shandong.econ_score[i] + shandong.socia_score[i] + shandong.eco_score[i])
}

shandong.decohe = 10000 * (1 - shandong.cohe)

shandong.d_cohe = sqrt(shandong.cohe*shandong.tot_score)

ggplot(data = NULL, aes(x = year, y = shandong.decohe)) + geom_smooth() + geom_point()

ggplot(data = NULL, aes(x = year, y = shandong.d_cohe)) + geom_smooth() + geom_point()

shandong.subscore$shandong.d_cohe = shandong.d_cohe

write.csv(shandong.subscore, file = "shandong_subscore.csv")

### henan
henan.cohe = rep(0,length(henan.data[,1]))
for(i in 1 : length(henan.data[,1]))
{
  henan.cohe[i] = 3 * (henan.econ_score[i] * henan.socia_score[i] * henan.eco_score[i])^(1/3)/(henan.econ_score[i] + henan.socia_score[i] + henan.eco_score[i])
}

henan.decohe = 10000 * (1 - henan.cohe)

henan.d_cohe = sqrt(henan.cohe*henan.tot_score)

ggplot(data = NULL, aes(x = year, y = henan.decohe)) + geom_smooth() + geom_point()

ggplot(data = NULL, aes(x = year, y = henan.d_cohe)) + geom_smooth() + geom_point()

henan.subscore$henan.d_cohe = henan.d_cohe

write.csv(henan.subscore, file = "henan_subscore.csv")


### summary 
Subsystem.cohe = data.frame(row.names = year)
Subsystem.cohe$beijing.d_cohe = beijing.d_cohe
Subsystem.cohe$tianjin.d_cohe = tianjin.d_cohe
Subsystem.cohe$hebei.d_cohe = hebei.d_cohe
Subsystem.cohe$shanxi.d_cohe = shanxi.d_cohe
Subsystem.cohe$shandong.d_cohe = shandong.d_cohe
Subsystem.cohe$henan.d_cohe = henan.d_cohe

write.csv(Subsystem.cohe, file = "Subsystem.cohe.csv")







# Linear Regression (Analyzing Beijing as a Representative)
econ.data = beijing.data[, 2:12]
socia.data = beijing.data[, 13:22]
eco.data = beijing.data[, 23:25]

## EDA

summary(econ.data)
summary(socia.data)
summary(eco.data)


## correlation plot 

econ.corr = cor(econ.data)
corrgram(econ.corr, order = T, lower.panel = panel.shade, upper.panel = panel.pie)

socia.corr = cor(socia.data)
corrgram(socia.corr, order = T, lower.panel = panel.shade, upper.panel = panel.pie)

eco.corr = cor(eco.data)
corrgram(eco.corr, order = T, lower.panel = panel.shade, upper.panel = panel.pie)


ggpairs(data = econ.data) + ggtitle("The Economy Pairplot")
ggpairs(data = socia.data) + ggtitle("The Society Pairplot")
ggpairs(data = eco.data) + ggtitle("The Ecology Pairplot")




## PCA
beijing.pca_data = read.csv("/Users/shuimuqinghua/Desktop/作业/大二下作业/线性回归分析/Project/Province Filled Data/reduced.csv")
beijing.pca_data = beijing.pca_data[,-1]
beijing.pca_data$response = beijing.d_cohe


reg.general = lm(response ~., data = beijing.pca_data)
summary(reg.general)
par(mfrow = c(2,2))
plot(reg.general)
par(mfrow = c(1,1))
vif(reg.general)






# Remedies

## Variable Selection
par(mfrow = c(1,1))
### Step-wise (Fitting ability)
nullmodel = lm(response ~ 1, data = beijing.pca_data)
fullmodel = lm(response ~ ., data = beijing.pca_data)
scope = list(lower = formula(nullmodel), upper = formula(fullmodel))
SignifReg(nullmodel,alpha = 0.05, criterion = "r-adj",scope = scope,direction = "both") #### adjusted-R square

### Step-wise (AIC and BIC)
SignifReg(nullmodel,alpha = 0.05, criterion = "AIC",scope = scope,direction = "both") 
SignifReg(nullmodel,alpha = 0.05, criterion = "BIC",scope = scope,direction = "both") 

### Step-wise (Predicting ability)
SignifReg(nullmodel,alpha = 0.05, criterion = "PRESS",scope = scope,direction = "both") 

### Mallow's Cp

x.select = 1:10
leapset.general = leaps(x = beijing.pca_data[,x.select], y = beijing.pca_data$response, method = "Cp", nbest = 3)
leapset.general
ggplot(data = NULL, aes(x = leapset.general$size, y = leapset.general$Cp)) + geom_point() + geom_abline(slope = 1, intercept = 0, color = "blue") + labs(x = "Variable Size", y = "Mallow's Cp")
leapset.general$which[order(leapset.general$Cp,decreasing = F)[1:3],]


regsubsets.general = regsubsets(x = beijing.pca_data[,x.select], y = beijing.pca_data$response, nbest = 3)
plot(regsubsets.general, scale = "Cp")
title(main = "Mallow's Cp")

reg.step = lm(formula = response ~ social_1 + Annual.Rainfall + Social.erosion.control.area, 
              data = beijing.pca_data)
summary(reg.step)
par(mfrow = c(2,2))
plot(reg.step)
par(mfrow = c(1,1))
vif(reg.step)




### Outlier, High Leverage or Both! (Primitive Model)
standresid.general = stdres(reg.general) # standardized residuals to detect outliers
outlier.general_std = round(standresid.general, digits = 4)
outlier.general_std[which(outlier.general_std > 2 | outlier.general_std < -2)]

stu.del.resids.general = rstudent(reg.general) # studentized residuals to detect outliers
outlier.general_stu = round(stu.del.resids.general, digits = 4)
outlier.general_stu[which(outlier.general_stu > 2 | outlier.general_stu < -2)]

outlierTest(reg.general) # Simple test to detect outlies

leverage.general = round(hatvalues(reg.general), digits = 4) # hatvalues to detect high leverage
leverage.general[which(leverage.general > 0.5)]

dfbeta(reg.general) # dfbetas to detect influential cases
dfbetas(reg.general) > 1

dffits.general = dffits(reg.general)
dffits.general[which(abs(dffits(reg.general))> 1.5)]

influence.general = influence.measures(reg.general)
which(apply(influence.general$is.inf, 1, any))


#### Plot it!
beijing.pca_data$group = rep("OK", length(beijing.pca_data$response))
outlier.general_stu.rank = which(outlier.general_stu > 2 | outlier.general_stu < -2)
leverage.general.rank = which(leverage.general > 1)
influence.general.rank = which(abs(dffits(reg.general))> 1.5)


for(i in 1 : length(outlier.general_stu.rank))
{
  beijing.pca_data$group[outlier.general_stu.rank[i]] = c("Outlier")
}

for(i in 1 : length(leverage.general.rank))
{
  beijing.pca_data$group[leverage.general.rank[i]] = c("High Leverage")
}

for(i in 1 : length(influence.general.rank))
{
  beijing.pca_data$group[influence.general.rank[i]] = c("High Influence")
}

ggplot(data = beijing.pca_data, aes(x = response - reg.general$residuals, y = response, color = group)) + geom_point() + labs(x = "Prediction", y = "Coherence", title = "Outlier, High Leverage or Both!")


### Box-Cox Transformation

loglike=boxcox(response ~ social_1 + Annual.Rainfall + Social.erosion.control.area, data = beijing.pca_data,lambda=seq(1, 4, length=10))


# Negative Binomial Distribution(Since the response is not a counting variable, this model might not fit)
reg.nb = glm.nb(1000*response ~., data = beijing.pca_data)
summary(reg.nb)

















