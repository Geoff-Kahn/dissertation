library(readstata13)
library(naniar)
library(weights)
library(questionr)

wave1 <- read.dta13(file='L:/NSCAW2RR_Waves1to3/cps_n2_t2/STATA/cps_n2.dta', 
                    convert.factors = FALSE)
wave2 <- read.dta13(file='L:/NSCAW2RR_Waves1to3/cps_n2_t2/STATA/cps_n2_w2.dta', 
                    convert.factors = FALSE)
wave3 <- read.dta13(file='L:/NSCAW2RR_Waves1to3/cps_n2_t2/STATA/cps_n2_w3.dta', 
                    convert.factors = FALSE)

## Child Outcome variable asked of children 11 and older, limiting dataset to those ages
wave1.age <- wave1[which(wave1$chdage==4),]
wave2.age <- wave2[which(wave2$CH2AGE==4),]
wave3.age <- wave3[which(wave3$CH3AGE==4),]

## Issue with merging above datasets: if interview not conducted then 
# age var = some missing value (not 4), so cases not included
# Then it becomes hard to tell why after merge, no missing codes, just missing from merge
# Remedy: compile list of nscawid's in any of 3 datasets, pull those id's from full data
wave1.id <- wave1.age[,4]
wave2.id <- wave2.age[,4]
wave3.id <- wave3.age[,4]
include.id <- c(wave1.id,wave2.id,wave3.id)
include.id <- unique(include.id)
include.id <- sort(include.id)

wave1.work <- wave1[wave1$nscawid %in% include.id,]
wave2.work <- wave2[wave2$nscawid %in% include.id,]
wave3.work <- wave3[wave3$nscawid %in% include.id,]

# Now dropping cases where child is known to be younger than 11
wave1.work2 <- wave1.work[!(wave1.work$chdage==3),]
wave2.work2 <- wave2.work[!(wave2.work$CH2AGE==3),]

##Provided codebook has more variables than are imported here (and in SAS, SAS and R equal)
##Use proc contents in SAS to get column numbers
## Wave 1: YYB18a is 2682, PBC18a is 5561, NANALWT is 10579
## Wave 2: YB218a is 2682, BC218a is 5667, NANALWT2 is 10415
## Wave 3: YB318a is 2682, BC318a is 5725, NANALWT3 is 10408, NANALW23 is 10409

outcomes.1 = wave1.work2[,c(4,2682:2684,5561,10579)]
outcomes.2 = wave2.work2[,c(4,2682:2684,5667,10415)]
outcomes.3 = wave3.work[,c(4,2682:2684,5725,10408:10409)]

## For all instrument variables the following values apply:
# -1 = Don't know
# -2 = Refused to answer
# -3 = Not applicable
# -4 = Missing
# -5 = Inadvertant skip
# -6 = Non-interview
# -7 = Legitimate skip
# -8 = Partial skip/breakoff

outcomes.p <- merge(outcomes.1,outcomes.2, by = "nscawid")
outcomes <- merge(outcomes.p,outcomes.3, by = "nscawid")

## Adding binary outcomes, recoding missing indicators to NA 
# and recoding youth 2 week/plan vars to include zeroes if main var = 0
miss.ind = c(-8,-7,-6,-5,-4,-3,-2,-1)
outcomes <- outcomes %>% replace_with_na(outcomes, replace = list(YYB18a = miss.ind, 
                                                                  PBC18a = miss.ind, YB218a = miss.ind, BC218a = miss.ind, YB318a = miss.ind, BC318a = miss.ind))
outcomes$wave1.youth = ifelse(outcomes$YYB18a==2|outcomes$YYB18a==3,1,0)
outcomes$wave1.CG = ifelse(outcomes$PBC18a==2|outcomes$PBC18a==3,1,0)
outcomes$wave2.youth = ifelse(outcomes$YB218a==2|outcomes$YB218a==3,1,0)
outcomes$wave2.CG = ifelse(outcomes$BC218a==2|outcomes$BC218a==3,1,0)
outcomes$wave3.youth = ifelse(outcomes$YB318a==2|outcomes$YB318a==3,1,0)
outcomes$wave3.CG = ifelse(outcomes$BC318a==2|outcomes$BC318a==3,1,0)
# 1=YES  2=NO
outcomes$YYB18aa.rc = ifelse(outcomes$YYB18a==1,2,outcomes$YYB18aa)
outcomes$YB218aa.rc = ifelse(outcomes$YB218a==1,2,outcomes$YB218aa)
outcomes$YB318aa.rc = ifelse(outcomes$YB318a==1,2,outcomes$YB318aa)
outcomes$YYB18ba.rc = ifelse(outcomes$YYB18a==1,2,outcomes$YYB18ba)
outcomes$YB218ba.rc = ifelse(outcomes$YB218a==1,2,outcomes$YB218ba)
outcomes$YB318ba.rc = ifelse(outcomes$YB318a==1,2,outcomes$YB318ba)
# coding indicator for either child OR caregiver response (missing only if both are missing - 2nd line of code does this)
outcomes$wave1.SA = ifelse(outcomes$wave1.youth==1|outcomes$wave1.CG==1,1,0)
outcomes$wave1.SA = ifelse((is.na(outcomes$wave1.youth) & outcomes$wave1.CG==0)|(is.na(outcomes$wave1.CG) & outcomes$wave1.youth==0),0,outcomes$wave1.SA)
outcomes$wave2.SA = ifelse(outcomes$wave2.youth==1|outcomes$wave2.CG==1,1,0)
outcomes$wave2.SA = ifelse((is.na(outcomes$wave2.youth) & outcomes$wave2.CG==0)|(is.na(outcomes$wave2.CG) & outcomes$wave2.youth==0),0,outcomes$wave2.SA)
outcomes$wave3.SA = ifelse(outcomes$wave3.youth==1|outcomes$wave3.CG==1,1,0)
outcomes$wave3.SA = ifelse((is.na(outcomes$wave3.youth) & outcomes$wave3.CG==0)|(is.na(outcomes$wave3.CG) & outcomes$wave3.youth==0),0,outcomes$wave3.SA)


## Univariate tables

table(outcomes$wave1.SA, useNA = "ifany")
wpct(outcomes$wave1.SA, weight=outcomes$nanalwt)
wtd.table(x=outcomes$wave1.SA, weight=outcomes$nanalwt)
table(outcomes$wave2.SA, useNA = "ifany")
wpct(outcomes$wave2.SA, weight=outcomes$nanalwt)
table(outcomes$wave3.SA, useNA = "ifany")
wpct(outcomes$wave3.SA, weight=outcomes$nanalwt)

## Bivariate tables, limited to children 11 and older 
# Unweighted tables to evaluate sample sizes
table(outcomes$YYB18a,outcomes$PBC18a, useNA = "ifany", dnn = c("Youth","Caregiver"))
table(outcomes$wave1.youth,outcomes$wave1.CG, useNA = "ifany", dnn = c("Youth","Caregiver"))
table(outcomes$YB218a,outcomes$BC218a, useNA = "ifany", dnn = c("Youth","Caregiver"))
table(outcomes$wave2.youth,outcomes$wave2.CG, useNA = "ifany", dnn = c("Youth","Caregiver"))
table(outcomes$YB318a,outcomes$BC318a, useNA = "ifany", dnn = c("Youth","Caregiver"))
table(outcomes$wave3.youth,outcomes$wave3.CG, useNA = "ifany", dnn = c("Youth","Caregiver"))

# Weighted tables to evaluate percent agreement
wv1full = wtd.table(x=outcomes$YYB18a, y=outcomes$PBC18a, weights=outcomes$nanalwt)
wv1red = wtd.table(x=outcomes$wave1.youth, y=outcomes$wave1.CG, weights=outcomes$nanalwt)
wv2full = wtd.table(x=outcomes$YB218a, y=outcomes$BC218a, weights=outcomes$nanalwt)
wv2red = wtd.table(x=outcomes$wave2.youth, y=outcomes$wave2.CG, weights=outcomes$nanalwt)
wv3full = wtd.table(x=outcomes$YB318a, y=outcomes$BC318a, weights=outcomes$nanalwt)
wv3red = wtd.table(x=outcomes$wave3.youth, y=outcomes$wave3.CG, weights=outcomes$nanalwt)
prop(wv1full, digits = 2)
prop(wv1red, digits = 2)
prop(wv2full, digits = 2)
prop(wv2red, digits = 2)
prop(wv3full, digits = 2)
prop(wv3red, digits = 2)