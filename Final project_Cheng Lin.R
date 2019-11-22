library(FactoMineR)
library(dplyr)
library(explor)
library(ade4)
library(factoextra)
options(digits = 2)
dat <- read.csv("http://ollion.cnrs.fr/wp-content/uploads/2019/05/Valkompass2012_ShortRecoded_anon.csv",stringsAsFactors = F)
# data cleaning
Encoding(dat$Region) <- "UTF-8"
# the number of candidates
nrow(dat)
# age
dat$YearBirth<- (2019-dat$YearBirth)
# define empty value as NA
dat[dat==""] <- NA
# create new variable "average incomes for 5 years" to replace 5 years incomes
for (i in 1:nrow(dat)) {
  dat$Income2008[i]<- sum(dat[i,8:12])/(5-sum(dat[i,8:12]==0))
}
mean(na.omit(dat$Income2008))
sd(na.omit(dat$Income2008))

#For each variable, determine what is the dominant class, then recode the data
for (i in 18:62){
  dat[,i] <- as.character(dat[,i])
  f <- length(dat[dat[,i] == "InFavor",i]) 
  a <- length(dat[dat[,i] == "Against",i])
  if (f >= a){dat[dat[,i] == "NoOpinion",i] <- "InFavor"} else {dat[dat[,i] == "NoOpinion",i] <- "Against"}}
#delete 5 year incomes variables
dat <- dat[,-c(9:12)]
# rename variables
colnames(dat)[8] <- "avg_income"
colnames(dat)[4] <- "age"

## Using indicator matrix to do MCA
# Create as disjunctive (indicator matrix) table
xxx <- acm.disjonctif(dat[,14:58])
dat2 <- cbind(dat[,1:13], xxx)
dataforMCA2 <- select(dat2,14:103,
                     c(
                     #qualitative supplementary vars
                     3,5,9,
                     10,12,13,
                     #quantatitive supplementary vars
                     4,8))

# balance test
prop.table(table(na.omit(dataforMCA2[,91]))*100)
prop.table(table(na.omit(dataforMCA2[,92]))*100)
prop.table(table(na.omit(dataforMCA2[,93]))*100)
prop.table(table(na.omit(dataforMCA2[,94]))*100)
prop.table(table(na.omit(dataforMCA2[,95]))*100)
dataforMCA2[dataforMCA2[,96]=="UniversityDiploma",]$HighestDiploma <- "UniversityDiploma "
prop.table(table(na.omit(dataforMCA2[,96]))*100)
sd(na.omit(dataforMCA2[,97]))
mean(na.omit(dataforMCA2[,97]))
# missing value imputation
dataforMCA2[is.na(dataforMCA2)] <- 0
# transfer to factor
for (i in 1:96) {
  dataforMCA2[,i] <- as.factor(dataforMCA2[,i])
}

#perform MCA
res2<-MCA(dataforMCA2, ncp=10,
         quanti.sup = c(97:98), 
         quali.sup=91:96, graph= F) 
explor(res2)

## according to different distance matrix and linking strategies to test robustness of clustering.
res2.hcpc <- HCPC(res2)
res2.hcpc.ward1<- HCPC(res2,metric="manhattan",method = "ward")
res2.hcpc.ward2<- HCPC(res2,metric="euclidean",method = "ward")
res2.hcpc.complete1<- HCPC(res2,metric="manhattan",method = "complete")
res2.hcpc.complete2<- HCPC(res2,metric="euclidean",method = "complete")
res2.hcpc.single1<- HCPC(res2,metric="manhattan",method = "single")
res2.hcpc.single2<- HCPC(res2,metric="euclidean",method = "single")
fviz_cluster(res2.hcpc)


##using original data to do MCA
dataforMCA <- select(dat,14:58,
                     c(
                       #qualitative supplementary vars
                       3,5,9,
                       10,12,13,
                       4,8))
head(dataforMCA)
prop.table(table(na.omit(dataforMCA[,46]))*100)
prop.table(table(na.omit(dataforMCA[,47]))*100)
prop.table(table(na.omit(dataforMCA[,48]))*100)
prop.table(table(na.omit(dataforMCA[,49]))*100)
prop.table(table(na.omit(dataforMCA[,50]))*100)
dataforMCA[dataforMCA[,51]=="UniversityDiploma",]$HighestDiploma <- "UniversityDiploma "
prop.table(table(na.omit(dataforMCA[,51]))*100)
sd(na.omit(dataforMCA[,52]))
mean(na.omit(dataforMCA[,52]))

dataforMCA[is.na(dataforMCA)] <- 0
for (i in 1:51) {
  dataforMCA[,i] <- as.factor(dataforMCA[,i])
}

res<-MCA(dataforMCA, ncp=10,
         quanti.sup = c(52:53), 
         quali.sup=46:51, graph= F) 
explor(res)

## according to different distance matrix and linking strategies to test robustness of clustering.
res.hcpc <- HCPC(res)
res.hcpc.ward1<- HCPC(res,metric="manhattan",method = "ward")
res.hcpc.ward2<- HCPC(res,metric="euclidean",method = "ward")
res.hcpc.complete1<- HCPC(res,metric="manhattan",method = "complete")
res.hcpc.complete2<- HCPC(res,metric="euclidean",method = "complete")
res.hcpc.single1<- HCPC(res,metric="manhattan",method = "single")
res.hcpc.single2<- HCPC(res,metric="euclidean",method = "single")
fviz_cluster(res.hcpc)

####
# using indicator matrix or original data, we can get quite similar patterns.
