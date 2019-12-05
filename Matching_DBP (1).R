################################################################
#Script: Matching_DBP.R
################################################################
#Project: Gov 2001 Project; Do Businesses Pay to Do Science?; Extension of Giuri P. and Mariani M., "When Distance Disappears", 2013
#Script Goal: LoadMultiple Imputed Data, Reduce to relevant (firm) observations, employ coarsened exact matching on all datasets, output list of all data
#Inputs: 
#- 'outdata#.csv' files outputted by 'MultipleImputation_DBP.r'
#Outputs: Multiple Imputed Data in 'DataList.R' and 'DataCEMList.R'
###############################################################
rm(list=ls())

##Packages
#install.packages("MatchIt")
#install.packages("cem")
#install.packages("MatchingFrontier")
#install.packages("Zelig")
#install.packages("ZeligChoice")
#install.packages("plyr")
library(plyr)
library(MatchingFrontier)
library(MatchIt)
library(cem)
library(Zelig)
library(ZeligChoice)

##Working Directory, Temp Folder
root<-"ENTER YOUR WORKING DIRECTORY HERE"
setwd(root)

##Set Seed
set.seed(02127)

################################################################
#Preparing Imputed Data files for matching / analysis
################################################################
#number of imputed data sets
num.imputations <- 10

#create lists to hold matching results
data.balanced.list <- list()
data.balanced.cem.list <- list()
data.balanced.prop.list <- list()
data.balanced.mahal.list <- list()

#loop over all data
for (i in 1:num.imputations) {

## Read data. drop false id variable
data <- read.csv(paste("WIP/ImputedData/","outdata",i,".csv",sep=""))
data <- data[,!(names(data) %in% ("X"))]

## Recalculate dummy variables utilized in analysis
data$CE_dummy<-0
data$CE_dummy[data$CloseExt>0]<-1
data$DE_dummy<-0
data$DE_dummy[data$DistExt>0]<-1
data$Low_High_School<-0
data$Low_High_School[data$EDU==1]<-1
data$UniMasDegree<-0
data$UniMasDegree[data$EDU==2]<-1
data$PhDDegree<-0
data$PhDDegree[data$EDU==3]<-1

#Outcome Variable Dummies
#Sci
data$Sci_dummy <- 0
data$Sci_dummy[data$SK_ScLit>0] <- 1
#Conf
data$Conf_dummy <- 0
data$Conf_dummy[data$SK_Tech>0] <- 1
#Com
data$Com_dummy <- 0
data$Com_dummy[data$reasonCommExploit>0] <- 1
#Lic
data$Lic_dummy <- 0
data$Lic_dummy[data$reasonLic>0] <- 1
#Imit
data$Imit_dummy <- 0
data$Imit_dummy[data$reasonImit>0] <- 1

#Ratio Outcome Variables
data$reason_sum <- data$reasonCommExploit + data$reasonLic + data$reasonImit

#Com
data$Com_ratio <- 0
data$Com_ratio <- (as.numeric(data$reasonCommExploit) / as.numeric(data$reason_sum))
data$Com_ratio[data$Com_ratio>1] <- 1
data$Com_ratio[data$Com_ratio<0] <- 0

#Lic
data$Lic_ratio <- 0
data$Lic_ratio <- (as.numeric(data$reasonLic) / as.numeric(data$reason_sum))
data$Lic_ratio[data$Lic_ratio>1] <- 1
data$Lic_ratio[data$Lic_ratio<0] <- 0

#Imit
data$Imit_ratio <- 0
data$Imit_ratio <- (as.numeric(data$reasonImit) / as.numeric(data$reason_sum))
data$Imit_ratio[data$Imit_ratio>1] <- 1
data$Imit_ratio[data$Imit_ratio<0] <- 0

#length of pre-balance data
length.before <- length(data[,1])

## Create Balanced Panel by eliminating those observations for which NA remains after imputation
#list of variables to check on
exclusion.varlist <- c("CE_dummy","DE_dummy","Age","Male","UniMasDegree","PhDDegree","link_coinv_diff_nuts_share1_b","no_pastcoinv","SK_ScLit","year_first_patent","residence_degree","reg_mob_nuts3In","reg_mob_nuts3Out","herfinv1","zeroExp","lEmployeesMiss","RDint","PRI_Applic","Individual_Applic","Ninventors","reasonCommExploit","reasonLic","reasonImit","AppYear1994","AppYear1995","AppYear1996","AppYear1997","AppYear1998","nuts2_d_res_country","TechClass","invtotprob","Applicant","AppYear")
exclusion.varlist.locations = which(colnames(data) %in% exclusion.varlist)

#Setup factor classifications
data$nuts2_d_res_country<-factor(data$nuts2_d_res_country)
data$nuts3_d_res <- factor(data$nuts3_d_res)
data$TechClass<-factor(data$TechClass)

#Drop all obersvations which are missing data in those variables
data.balanced <- data[complete.cases(data[,c(exclusion.varlist.locations)]),]
data.balanced <-data[complete.cases(data),]

#Remove patents by non-organization affiliated individuals (IE Applicant!="Firm")
data.balanced <- data.balanced[data.balanced$Applicant=="Firm",]

#length of post-balance data
length.after <- length(data.balanced[,1])

################################################################
#Matching Treatments and Controls based on t=1 if EDU=2 or 3 with non-singular cases
################################################################
## Create 'treatment' (EDU=3) indicator variable
data.balanced$t <- 0
data.balanced$t[data.balanced$EDU>2]<-1
#data.balanced$t[data.balanced$EDU>2]<-1

## Identifying cutoffs by examining distributions of treatment and control covariate data
treats <- data.balanced[data.balanced$t==1,]
controls <- data.balanced[data.balanced$t==0,]

par(mfrow = c(2,1)) 
#age
hist(treats$Age)
hist(controls$Age)
agecut <- c(16,25,30,35,40,45,50,55,60,70,100)

#male
hist(treats$Male)
hist(controls$Male)
malecut <- c(-1,0.5,2)

#lexperience
hist(treats$lexperience)
hist(controls$lexperience)
expcut <- c(-1,0,2,3,10)

#herfinv1
hist(treats$herfinv1)
hist(controls$herfinv1)
herfcut <- c(-1,0.2,0.4,0.6,0.8,1.5)

#breadthexp01
hist(treats$breadthexp01)
hist(controls$breadthexp01)
bexpcut <- c(-1,0.5,1.5)

#link_coinv_diff_nuts_share1_b
hist(treats$link_coinv_diff_nuts_share1_b)
hist(controls$link_coinv_diff_nuts_share1_b)
linkcoinvcut <- c(-1,0,0.5,0.9,1)

#residence_degree
hist(treats$residence_degree)
hist(controls$residence_degree)
rescut <- c(-1,0.5,1.5)

#reg_mob_nuts3In
hist(treats$reg_mob_nuts3In)
hist(controls$reg_mob_nuts3In)
regincut <- c(-1,0.5,1.5)

#reg_mob_nuts3Out
hist(treats$reg_mob_nuts3Out)
hist(controls$reg_mob_nuts3Out)
regoutcut <- c(-1,0.5,1.5)

##Variables Removed from matching as arguably post-treatment (after hiring decision)
#year_first_patent
hist(treats$year_first_patent)
hist(controls$year_first_patent)
yr1stcut <- c(1976,1992,2000)


#no_pastcoinv
nopastcut <- c(-1,0.5,1.5)

#AppYear
hist(treats$AppYear)
hist(controls$AppYear)
appyrcut <- c(1992,1994.1,1995.1,1996.1,1997.1,1998.1)

#lEmployeesMiss
hist(treats$lEmployeesMiss)
hist(controls$lEmployeesMiss)
empcut <- c(-1,4,13,18)

#RDint
hist(treats$RDint)
hist(controls$RDint)
rdintcut <- c(-0.08,0.1,0.41)

#Ninventors
hist(treats$Ninventors)
hist(controls$Ninventors)
quantile(treats$Ninventors, c(.10,.2,.25,.5,.75,.8,.9,.95,.99)) 
max(treats$Ninventors)
quantile(controls$Ninventors, c(.10,.2,.25,.5,.75,.8,.9,.95,.99)) 
max(controls$Ninventors)
ninvcut <- c(0,1,5,16)

#Commericial Exploit
hist(treats$reasonCommExploit)
hist(controls$reasonCommExploit)
commcut <- c(-1,0.5,6)

#Licenscing 
hist(treats$reasonLic)
hist(controls$reasonLic)
liccut <- c(-6,1,6)

#Imitation
hist(treats$reasonImit)
hist(controls$reasonImit)
imitcut <- c(-3,3,7)

#SK_ScLit
hist(treats$SK_ScLit)
hist(controls$SK_ScLit)
scicut <- c(-3,0.1,8)

#SK_Tech (ie conferences)
hist(treats$SK_Tech)
hist(controls$SK_Tech)
confcut <- c(-3,0,8)

## Groupings
#Applicant
summary(treats$Applicant)
summary(controls$Applicant)
applicant.group <- list(c("PRI"),c("Firm"))

#NUTS 2 Regional Grouping - NOT USED, but for later ease of use
summary(treats$nuts2_d_res_country)
summary(controls$nuts2_d_res_country)
nuts2.group <- list(c("AT13","AT21"),
                    c("BE21"),
                    c("CH03","CH04","CH05","CH07"),
                    c("DE_OTH","DE11","DE12","DE13",
                      "DE14","DE21","DE22","DE23",
                      "DE24","DE25","DE26","DE27",
                      "DE30","DE40","DE50","DE6",
                      "DE71","DE72","DE73","DE91",
                      "DE92","DE93","DE94","DEA1",
                      "DEA2","DEA3","DEA4","DEA5",
                      "DEB1","DEB3","DED1","DED2",
                      "DED3","DEE2","DEF0","DEG0"),
                    c("ES_OTH","ES12","ES21","ES22",
                      "ES24","ES3","ES41","ES43","ES51",
                      "ES52","ES53","ES61","ES70"),
                    c("FR_OTH","FR41"),
                    c("IT_OTH","IT11","IT13","IT20",
                      "IT31","IT32","IT33","IT40",
                      "IT51","IT52","IT53","IT60",
                      "IT71","IT80","IT91","ITA0","ITB0"),
                    c("NL31","NL32","NL33","NL34","NL41",
                      "NL42","NL04"),
                    c("OTHER"),
                    c("UK_OTH","UKC1","UKC2","UKD1",
                      "UKD2","UKD3","UKD4","UKD5",
                      "UKE1","UKE2","UKE3","UKE4",
                      "UKF1","UKF2","UKF3","UKG1",
                      "UKG2","UKG3","UKH1","UKH2",
                      "UKH3","UKI1","UKI2","UKJ1",
                      "UKJ2","UKJ3","UKJ3","UKJ4",
                      "UKK1","UKK2","UKK3","UKK4",
                      "UKL1","UKL2","UKM1","UKM2",
                      "UKM3","UKN","UKN0","UKZZ"))

cutpoints <- list(Age=agecut,Male=malecut,herfinv1=herfcut,
                  residence_degree=rescut,reg_mob_nuts3In=regincut,
                  reg_mob_nuts3Out=regoutcut,lexperience=expcut,
                  link_coinv_diff_nuts_share1_b=linkcoinvcut,
                  breadthexp01=bexpcut)
groupings <- list(Applicant=applicant.group)

## Variables in original specification but not yet recalculated
#D_miss_empl, D_missC_RD

## Variables for first specification
## Using Location in place of nuts2_d_res_country, as we would be classifying it that way anyways
varlist.includes <- c("t","Age","Male","EDU","herfinv1","lexperience","link_coinv_diff_nuts_share1_b",
                      "residence_degree","reg_mob_nuts3In","reg_mob_nuts3Out","Location","CE_dummy",
                      "DE_dummy","invtotprob","breadthexp01")

vec <- names(data.balanced)
varlist.drops <- vec [! vec %in% varlist.includes]

## Coarsened Exact Matching using CEM package, cutoffs identified above
cem.match <- cem(treatment="t",
                 data=data.balanced,
                 drop=c("t","CE_dummy","DE_dummy","invtotprob","EDU",varlist.drops),
                 cutpoints=cutpoints,
                 grouping=groupings,
                 eval.imbalance=TRUE)
cem.match

data.balanced.cem <- data.balanced
data.balanced.cem$w <- cem.match$w
data.balanced.cem <- data.balanced.cem[data.balanced.cem$w>0,]

## Propensity Score Matching
pscores <- glm(t ~ rep(1,length(data.balanced[,1])) + Age + Male + herfinv1 + lexperience +
                 link_coinv_diff_nuts_share1_b + residence_degree + reg_mob_nuts3In + 
                 reg_mob_nuts3Out + Location + breadthexp01 ,
               family = "binomial", data = data.balanced)
fittedvalues <- pscores$fitted
pscores.treat <- fittedvalues[data.balanced$t==1]
pscores.control <- fittedvalues[data.balanced$t==0]

## Use matchit to do nearest neighbor matching on the propoensity scores
#nearest neighbors
nearest.match.prop <- matchit(formula = t ~ rep(1,length(data.balanced[,1])) + Age + Male + herfinv1 + lexperience +
                                link_coinv_diff_nuts_share1_b + residence_degree + reg_mob_nuts3In + 
                                reg_mob_nuts3Out + Location + breadthexp01, 
                              data = data.balanced, 
                              method = "nearest", 
                              distance = "logit", 
                              discard = "treat")

data.balanced.prop <- match.data(nearest.match.prop)

data.prop.imbalance <- imbalance(group=data.balanced.prop$t,
                             data=data.balanced.prop,
                             drop=c("t","CE_dummy","DE_dummy","invtotprob","EDU",varlist.drops,"ones"))
data.prop.imbalance

data.balanced.list[[i]] <- data.balanced
data.balanced.cem.list[[i]] <- data.balanced.cem
data.balanced.prop.list[[i]] <- data.balanced.prop
}

save(data.balanced.list,file="WIP/DataList.RData")
save(data.balanced.cem.list,file="WIP/DataCemList.RData")

