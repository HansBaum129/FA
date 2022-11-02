library(readxl)      # to read Excel files
library(readr)       # to read csv files
library(ggplot2)     # for graphics
library(ggpubr)      # for arranging graphs in one figure
library(car)         # to produce a scatterplot matrix
library(psych)       # for EFA and descriptives
library(lavaan)      # for EFA and CFA 
library(semTools)    # to get additional fit indices and for invariance testing
library(GPArotation) # for factor rotation
library(FCO)         # for flexible cutoffs
library(RCurl)       # to fetch github data file

# set the working directory (replace xxx with your directory information)
setwd("xxx")
# read in the data from github
myfile <- getURL("https://raw.githubusercontent.com/HansBaum129/FA/main/GLOBE.csv")
rawdata <- read.csv(text = myfile, header=TRUE)
head(rawdata)
str(rawdata)
names(rawdata)
# descriptives
psych::describe(rawdata)
# check for missing values
any(is.na(rawdata))
View(rawdata)
attach(rawdata)


#############
# histograms
#############

hist11<-ggplot(data=rawdata,aes(x=UA1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist12<-ggplot(data=rawdata,aes(x=UA2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist13<-ggplot(data=rawdata,aes(x=UA3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist14<-ggplot(data=rawdata,aes(x=UA4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist15<-ggplot(data=rawdata,aes(x=UA5)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure1 <- ggpubr::ggarrange(hist11,hist12,hist13,hist14,hist15, ncol = 3, nrow = 2)
histfigure1

hist21<-ggplot(data=rawdata,aes(x=PD1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist22<-ggplot(data=rawdata,aes(x=PD2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist23<-ggplot(data=rawdata,aes(x=PD3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist24<-ggplot(data=rawdata,aes(x=PD4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure2 <- ggpubr::ggarrange(hist21,hist22,hist23,hist24, ncol = 2, nrow = 2)
histfigure2

hist31<-ggplot(data=rawdata,aes(x=COL11)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist32<-ggplot(data=rawdata,aes(x=COL12)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist33<-ggplot(data=rawdata,aes(x=COL13)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist34<-ggplot(data=rawdata,aes(x=COL14)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure3 <- ggpubr::ggarrange(hist31,hist32,hist33,hist34, ncol = 2, nrow = 2)
histfigure3

hist41<-ggplot(data=rawdata,aes(x=COL21)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist42<-ggplot(data=rawdata,aes(x=COL22)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist43<-ggplot(data=rawdata,aes(x=COL23)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist44<-ggplot(data=rawdata,aes(x=COL24)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure4 <- ggpubr::ggarrange(hist41,hist42,hist43,hist44, ncol = 2, nrow = 2)
histfigure4

hist51<-ggplot(data=rawdata,aes(x=GE1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist52<-ggplot(data=rawdata,aes(x=GE2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist53<-ggplot(data=rawdata,aes(x=GE3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist54<-ggplot(data=rawdata,aes(x=GE4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure5 <- ggpubr::ggarrange(hist51,hist52,hist53,hist54, ncol = 2, nrow = 2)
histfigure5

hist61<-ggplot(data=rawdata,aes(x=ASS1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist62<-ggplot(data=rawdata,aes(x=ASS2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist63<-ggplot(data=rawdata,aes(x=ASS3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist64<-ggplot(data=rawdata,aes(x=ASS4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure6 <- ggpubr::ggarrange(hist61,hist62,hist63,hist64, ncol = 2, nrow = 2)
histfigure6

hist71<-ggplot(data=rawdata,aes(x=FO1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist72<-ggplot(data=rawdata,aes(x=FO2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist73<-ggplot(data=rawdata,aes(x=FO3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist74<-ggplot(data=rawdata,aes(x=FO4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure7 <- ggpubr::ggarrange(hist71,hist72,hist73,hist74, ncol = 2, nrow = 2)
histfigure7

hist81<-ggplot(data=rawdata,aes(x=PO1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist82<-ggplot(data=rawdata,aes(x=PO2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist83<-ggplot(data=rawdata,aes(x=PO3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist84<-ggplot(data=rawdata,aes(x=PO4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure8 <- ggpubr::ggarrange(hist81,hist82,hist83,hist84, ncol = 2, nrow = 2)
histfigure8

hist91<-ggplot(data=rawdata,aes(x=HO1)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist92<-ggplot(data=rawdata,aes(x=HO2)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist93<-ggplot(data=rawdata,aes(x=HO3)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
hist94<-ggplot(data=rawdata,aes(x=HO4)) +
  geom_histogram(binwidth=1,color = "darkblue", fill = "lightblue") +
  stat_bin(binwidth=1, geom='text', color='black', aes(label=..count..),position=position_stack(vjust = 0.5))
histfigure9 <- ggpubr::ggarrange(hist91,hist92,hist93,hist94, ncol = 2, nrow = 2)
histfigure9


###########################################
# item analysis for each factor separately
###########################################

# r.drop is the corrected item-total correlation
psych::alpha(rawdata[,1:5])$item.stats
psych::alpha(rawdata[,2:5])$item.stats # UA1 dropped 
psych::alpha(rawdata[,2:5])

psych::alpha(rawdata[,6:9])$item.stats
psych::alpha(rawdata[,6:9])

psych::alpha(rawdata[,10:13])$item.stats
psych::alpha(rawdata[,11:13])$item.stats # COL11 dropped
psych::alpha(rawdata[,11:13])

psych::alpha(rawdata[,14:17])$item.stats
psych::alpha(rawdata[,14:17])

psych::alpha(rawdata[,18:21])$item.stats
psych::alpha(rawdata[,c(18,19,21)])$item.stats # GE3 dropped
psych::alpha(rawdata[,c(18,19,21)])

psych::alpha(rawdata[,22:25])$item.stats
psych::alpha(rawdata[,22:25])

psych::alpha(rawdata[,26:29])$item.stats
psych::alpha(rawdata[,c(26,27,29)])$item.stats # FO3 dropped
psych::alpha(rawdata[,c(26,27,29)])

psych::alpha(rawdata[,30:33])$item.stats
psych::alpha(rawdata[,30:33])

psych::alpha(rawdata[,34:37])$item.stats
psych::alpha(rawdata[,34:37])


# create three data sets for subsequent analyses:
  # globedata.full contains all 37 GLOBE variables but not the stress variable
  # globedata.part contains 33 GLOBE variables (the four "bad" variables were removed and the stress variable was also removed)
  # globedata.part.ms is the same as the previous data set but with the stress variable (needed for the multi-sample analysis)

globedata.full <- subset(rawdata, select = -c(38))
#str(globedata.full)
globedata.part <- subset(rawdata, select = -c(1,10,20,28,38))
#str(globedata.part)
globedata.part.ms <- subset(rawdata, select = -c(1,10,20,28))
#str(globedata.part.ms)


####################
# preliminary tests
####################

# Barlett test that a correlation matrix is an identity matrix
psych::cortest.bartlett(cor(globedata.full),n=435)
psych::cortest.bartlett(cor(globedata.part),n=435)

# Kaiser-Meyer-Olkin test of factor adequacy
psych::KMO(cor(globedata.full))
psych::KMO(cor(globedata.full))$MSAi
describe(psych::KMO(cor(globedata.full))$MSAi)

psych::KMO(cor(globedata.part))
psych::KMO(cor(globedata.part))$MSAi
describe(psych::KMO(cor(globedata.part))$MSAi)


##################################################
# determining the number of factors for PC and PF
##################################################

# scree test, eigenvalues and variance accounted for

psych::scree(globedata.full) # scree for 37 variables
psych::scree(globedata.full)$pcv # eigenvalues of full correlation matrix
sum(psych::scree(globedata.full)$pcv[1:9])/sum(psych::scree(globedata.full)$pcv[1:37]) #percent of total variance for 9 eigenvalues
psych::scree(globedata.full)$fv # eigenvalues of reduced correlation matrix
psych::scree(globedata.full)$fv/mean(psych::smc(globedata.full[1:37])) #eigenvalues greater than mean (initial) communality 
psych::scree(globedata.full)$fv/mean(psych::fa(globedata.full,nfactors=9,fm="pa",SMC=TRUE, rotate="none")$communality[1:37]) #eigenvalues greater than mean (final) communality 
sum(psych::scree(globedata.full)$fv[1:8])/sum(psych::smc(globedata.full[1:37])) #percent of common variance based on initial communalities
sum(psych::scree(globedata.full)$fv[1:8])/sum(psych::fa(globedata.full, fm="pa", nfactors=8,SMC=TRUE, rotate="none")$communality[1:37]) #percent of common variance based on final communalities

psych::scree(globedata.part) # scree for 33 variables
psych::scree(globedata.part)$pcv # eigenvalues of reduced correlation matrix
sum(psych::scree(globedata.part)$pcv[1:8])/sum(psych::scree(globedata.part)$pcv[1:33]) #percent of total variance for 9 eigenvalues
psych::scree(globedata.part)$fv # eigenvalues of reduced correlation matrix
psych::scree(globedata.part)$fv/mean(psych::smc(globedata.part[1:33])) #eigenvalues greater than mean (initial) communality 
psych::scree(globedata.part)$fv/mean(psych::fa(globedata.part,nfactors=9,fm="pa",SMC=TRUE, rotate="none")$communality[1:33]) #eigenvalues greater than mean (final) communality 
sum(psych::scree(globedata.part)$fv[1:8])/sum(psych::smc(globedata.part[1:33])) #percent of common variance based on initial communalities
sum(psych::scree(globedata.part)$fv[1:8])/sum(psych::fa(globedata.part, fm="pa", nfactors=8,SMC=TRUE, rotate="none")$communality[1:33]) #percent of common variance based on final communalities

# # nicer plots
# GetEigenvalues1 <- fa(globedata.full,nfactors = ncol(globedata.full), rotate = "none")
# n_factors <- length(GetEigenvalues1$e.values)
# scree     <- data.frame(
#   Factor_n =  as.factor(1:n_factors), 
#   Eigenvalue1 = GetEigenvalues1$e.values)
# ggplot(scree, aes(x = Factor_n, y = Eigenvalue1, group = 1)) + 
#   geom_point() + geom_line() +
#   xlab("Number of factors") +
#   ylab("Initial eigenvalue") +
#   labs( title = "Scree Plot", 
#         subtitle = "(Based on the full correlation matrix)")
# GetEigenvalues2 <- fa(globedata.full, nfactors = 1, fm="pa", SMC=TRUE,  rotate = "none")
# n_factors <- length(GetEigenvalues2$values)
# scree     <- data.frame(
#   Factor_n =  as.factor(1:n_factors), 
#   Eigenvalue2 = GetEigenvalues2$values)
# ggplot(scree, aes(x = Factor_n, y = Eigenvalue2, group = 1)) + 
#   geom_point() + geom_line() +
#   xlab("Number of factors") +
#   ylab("Initial eigenvalue") +
#   labs( title = "Scree Plot", 
#         subtitle = "(Based on the reduced correlation matrix)")

# parallel analysis
set.seed(13)
parallel.pc<-psych::fa.parallel(globedata.full,fa="pc",quant=.95) # PC only
set.seed(13)
parallel.pc<-psych::fa.parallel(globedata.part,fa="pc",quant=.95) # PC only
set.seed(13)
parallel.fa<-psych::fa.parallel(globedata.full,fa="fa",quant=.95) # PF only
set.seed(13)
parallel.fa<-psych::fa.parallel(globedata.part,fa="fa",quant=.95) # PF only
# MAP criterion
psych::vss(globedata.full)
psych::vss(globedata.part)


#################################################
# principal components analysis -- all variables
#################################################

# principal components analysis, 9 components, promax rotation
PC.9f.promax.globedata.full <- psych::principal(globedata.full,nfactors=9,rotate="promax")
print.psych(PC.9f.promax.globedata.full,cut=.3)
print.psych(describe(PC.9f.promax.globedata.full$communality),digits=4)


#######################################################
# principal components analysis -- 4 variables omitted
#######################################################

# principal components analysis, 9 components, promax rotation
PC.9f.promax.globedata.part <- psych::principal(globedata.part,nfactors=9,rotate="promax")
print.psych(PC.9f.promax.globedata.part,cut=.3)
print.psych(describe(PC.9f.promax.globedata.part$communality),digits=4)

#############################################
# principal factor analysis -- all variables
#############################################

# principal axis factor analysis, 9 factors with promax rotation
PF.9f.promax.globedata.full <- psych::fa(globedata.full,fm="pa", nfactors=9, SMC=TRUE, rotate="Promax")
print.psych(PF.9f.promax.globedata.full, cut=.3)
print.psych(describe(PF.9f.promax.globedata.full$communality),digits=4)


###################################################
# principal factor analysis -- 4 variables omitted
###################################################

# principal axis factor analysis, 9 factors with promax rotation
PF.9f.promax.globedata.part <- psych::fa(globedata.part,fm="pa", nfactors=9, SMC=TRUE, rotate="Promax")
print.psych(PF.9f.promax.globedata.part, cut=.3)
print.psych(describe(PF.9f.promax.globedata.part$communality),digits=4)
PF.9f.promax.globedata.part$Structure # structure matrix
PF.9f.promax.globedata.part$Phi # factor correlation

# # assessing significance of the loadings using bootstrapping (1000 bootstrap samples)
# psych::fa(globedata.part,fm="pa", nfactors=9, SMC=TRUE, rotate="Promax", n.iter=1000, max.iter=1000)

# # graph of model
# psych::fa.diagram(PF.9f.promax.globedata.part, cut=.4)

# compare different rotations
dim9.varimax <- psych::fa(globedata.part,fm="pa", nfactors=9, SMC=TRUE, rotate="varimax")
dim9.Promax <- psych::fa(globedata.part,fm="pa", nfactors=9, SMC=TRUE, rotate="Promax")
dim9.quartimin <- psych::fa(globedata.part,fm="pa", nfactors=9, SMC=TRUE, rotate="quartimin")
print.psych(dim9.quartimin, cut=.3)
dim9.geominQ <- psych::fa(globedata.part,fm="pa", nfactors=9, SMC=TRUE, rotate="geominQ")
# assess congruence of loadings between different rotation methods
factor.congruence(dim9.varimax,dim9.Promax)
factor.congruence(dim9.varimax,dim9.geominQ)
factor.congruence(dim9.varimax,dim9.quartimin)
factor.congruence(dim9.Promax,dim9.geominQ)
factor.congruence(dim9.Promax,dim9.quartimin)
factor.congruence(dim9.geominQ,dim9.quartimin)


######################################
# ML factor analysis -- all variables
######################################

# maximum likelihood factor analysis, 9 factors
ML.9f.promax.globedata.full <- psych::fa(globedata.full,fm="ml", nfactors=9, rotate="Promax")
print.psych(ML.9f.promax.globedata.full, cut=.4)


############################################
# ML factor analysis -- 4 variables omitted
############################################

# maximum likelihood factor analysis, 9 factors, Promax rotation
ML.9f.promax.globedata.part <- psych::fa(globedata.part,fm="ml", nfactors=9, rotate="Promax")
print.psych(ML.9f.promax.globedata.part, cut=.3)
ML.9f.promax.globedata.part$loadings

# maximum likelihood factor analysis, 9 factors, oblimin rotation (for comparison with lavaan)
ML.9f.oblimin.globedata.part <- psych::fa(globedata.part,fm="ml", nfactors=9, rotate="oblimin")
print.psych(ML.9f.oblimin.globedata.part, cut=.3)
ML.9f.oblimin.globedata.part$loadings

# maximum likelihood factor analysis, 9 factors, with bootstrapping (1000 samples)
#set.seed(13)
ML.9f.promax.boot.globedata.part <- psych::fa(globedata.part,fm="ml", nfactors=9, rotate="Promax",n.iter=1000)
ML.9f.promax.boot.globedata.part
#confidence intervals
ML.9f.promax.boot.globedata.part$cis$ci
#write.csv(round(ML.9f.promax.boot.globedata.part$cis$ci,2), "MLLoadings-promax.csv", row.names = TRUE)


################
# EFA in lavaan
################

efa.9f <- '
    efa("efa")*f1 + 
    efa("efa")*f2 + 
    efa("efa")*f3 + 
    efa("efa")*f4 +
    efa("efa")*f5 +
    efa("efa")*f6 +
    efa("efa")*f7 +
    efa("efa")*f8 +
    efa("efa")*f9 =~ 
       UA2+UA3+UA4+UA5+
       PD1+PD2+PD3+PD4+
       COL12+COL13+COL14+
       COL21+COL22+COL23+COL24+
       GE1+GE2+GE4+
       ASS1+ASS2+ASS3+ASS4+
       FO1+FO2+FO4+
       PO1+PO2+PO3+PO4+
       HO1+HO2+HO3+HO4
'

# using the efa function in lavaan (requires the latest version of lavaan)
# (for some reason, the significance of the non-target loadings changes from run to run)
fit.efa.9f <- efa(efa.9f,data = globedata.part, nfactors=9, rotation = "quartimin")
summary(fit.efa.9f, standardized = TRUE)

# another way to get the efa solution in lavaaan using the cfa function
fit.cfa.9f <- cfa(model = efa.9f,data = globedata.part,rotation = "quartimin",
       estimator = "ML", se = "standard")
summary(fit.cfa.9f, standardized = TRUE)
parameterEstimates(fit.cfa.9f)
lavInspect(fit.cfa.9f, "std")$lambda

# printing all the parameters that are significant at alpha=.01 (based on standardized parameters)
options(max.print = .Machine$integer.max)
partable.fit.cfa.9f <- parTable(fit.cfa.9f)
fit.cfa.9f.loadings <- subset(lavMatrixRepresentation(partable.fit.cfa.9f),
         (abs(lavMatrixRepresentation(partable.fit.cfa.9f)$est.std)/lavMatrixRepresentation(partable.fit.cfa.9f)$se.std > 2.5758) & 
         lavMatrixRepresentation(partable.fit.cfa.9f)$op=='=~')
fit.cfa.9f.loadings

#calculating the mean of the target loadings and the mean of the nontarget loadings
target.loadings <- fit.cfa.9f.loadings[c(1:4,8:11,13:15,17:20,24:26,30:33,39:41,43:46,49:52),]
nontarget.loadings <- fit.cfa.9f.loadings[-c(1:4,8:11,13:15,17:20,24:26,30:33,39:41,43:46,49:52),]
target.loadings[,c(2:4,18:19)]
describe(abs(target.loadings[,c(18:19)]))
nontarget.loadings[,c(2:4,18:19)]
describe(abs(nontarget.loadings[,c(18:19)]))

# congruence between principal axis oblimin solution and ML oblimin solution
factor.congruence(dim9.quartimin,lavInspect(fit.cfa.9f, "std")$lambda)

# # bootstrapping takes a long time
# fit.efa.9f.boot <- 
#   cfa(model = efa.9f,data = globedata.part, 
#       rotation = "oblimin",
#       estimator = "ML", se = "bootstrap", "bootstrap"=100)
# options(max.print = .Machine$integer.max)
# summary(fit.efa.9f.boot,standardized = TRUE)


################
# factor scores
################

# psych::factor.scores(globedata.de,PAF.2f.globedata.de,method=c("Thurstone","tenBerge","Anderson","Barlett","components"))

RegressionMethod <-factor.scores(globedata.part, PF.9f.promax.globedata.part, Phi = NULL, method = c("Thurstone"),impute="none")
BartlettMethod <- factor.scores(globedata.part, PF.9f.promax.globedata.part, Phi = NULL, method = c("Bartlett"),impute="none")
tenBergeMethod <- factor.scores(globedata.part, PF.9f.promax.globedata.part, Phi = NULL, method = c("tenBerge"),impute="none")


# see the article by Grice (2001) for the formulae used below

#----------
# validity
#----------

sqrt(RegressionMethod$R2) # these are the validities for the Thurstone factor scores
# to get the correct validities for the other methods look at the diagonals in the univocality analysis)

#-------------
# univocality
#-------------

# Regression Method
# structure matrix Skf
Sfk1 <- PF.9f.promax.globedata.part$Structure[1:33,1:9]
# factor score coefficients Wkf
Wkf1 <- RegressionMethod$weights
# correlation matrix of variables
Rkk1 <- cor(globedata.part)
# Css matrix
Css1 <- t(RegressionMethod$weights) %*% Rkk1 %*% RegressionMethod$weights   
# we need the inverse of the matrix containing the square root of the diagonal elements
Cssdiag1 <- sqrt(diag(Css1))
Lss1 <- diag(Cssdiag1,9,9)
invLss1 <- solve(Lss1)
# finally, we can compute the matrix of interest
Rfs1 <- t(Sfk1) %*% Wkf1 %*% invLss1
# the off-diagonal elements of this matrix are the univocalities
# estimated factor scores in the rows, true factors in the columns
Rfs1
diag(Rfs1) # these are the validities
sum(diag(Rfs1))/9 # this is the average validity

# # we have to compare the correlation between each estimated factor score and the underlying factor with the "true" correlation
# # for example:
# cbind(Rfs1[1,2],PF.9f.promax.globedata.part$Phi[2,1])
# cbind(Rfs1[2,1],PF.9f.promax.globedata.part$Phi[2,1])

# this computes the average univocality
check1 <- 0
check11 <- 0
check12 <- 0
for (i in 2:9) {
      for (j in 1:(i-1)) {
          check11 <- check11 + (PF.9f.promax.globedata.part$Phi[i,j]-Rfs1[i,j])^2
          check12 <- check12 + (PF.9f.promax.globedata.part$Phi[i,j]-t(Rfs1)[i,j])^2
          check1 <- ((check11+check12)/(9*8))/2
    }
}
check1 <- sqrt(check1)
check1


# Bartlett Method
# structure matrix Skf
Sfk2 <- PF.9f.promax.globedata.part$Structure[1:33,1:9]
# factor score coefficients Wkf
Wkf2 <- BartlettMethod$weights
# correlation matrix of variables
Rkk2 <- cor(globedata.part)
# Css matrix
Css2 <- t(BartlettMethod$weights) %*% Rkk2 %*% BartlettMethod$weights   
# we need the inverse of the matrix containing the square root of the diagonal elements
Cssdiag2 <- sqrt(diag(Css2))
Lss2 <- diag(Cssdiag2,9,9)
invLss2 <- solve(Lss2)
# finally, we can compute the matrix of interest
Rfs2 <- t(Sfk2) %*% Wkf2 %*% invLss2
# the off-diagonal elements of this matrix are the univocalities
# upper right correlations of PAi with Fi
# lower left correlation of PAi with F1
Rfs2
diag(Rfs2) # these are the validities
sum(diag(Rfs2))/9 # this is the average validity
# # we have to compare the correlation between each estimated factor score and the underlying factor with the "true" correlation
# cbind(Rfs2[1,2],PF.9f.promax.globedata.part$Phi[2,1])
# cbind(Rfs2[2,1],PF.9f.promax.globedata.part$Phi[2,1])

# this computes the average univocality
check2 <- 0
check21 <- 0
check22 <- 0
for (i in 2:9) {
  for (j in 1:(i-1)) {
    check21 <- check21 + (PF.9f.promax.globedata.part$Phi[i,j]-Rfs2[i,j])^2
    check22 <- check22 + (PF.9f.promax.globedata.part$Phi[i,j]-t(Rfs2)[i,j])^2
    check2 <- ((check21+check22)/(9*8))/2
  }
}
check2 <- sqrt(check2)
check2


# tenBerge Method
# structure matrix Skf
Sfk3 <- PF.9f.promax.globedata.part$Structure[1:33,1:9]
# factor score coefficients Wkf
Wkf3 <- tenBergeMethod$weights
# correlation matrix of variables
Rkk3 <- cor(globedata.part)
# Css matrix
Css3 <- t(tenBergeMethod$weights) %*% Rkk3 %*% tenBergeMethod$weights   
# we need the inverse of the matrix containing the square root of the diagonal elements
Cssdiag3 <- sqrt(diag(Css3))
Lss3 <- diag(Cssdiag3,9,9)
invLss3 <- solve(Lss3)
# finally, we can compute the matrix of interest
Rfs3 <- t(Sfk3) %*% Wkf3 %*% invLss3
# the off-diagonal elements of this matrix are the univocalities
# upper right correlations of PAi with Fi
# lower left correlation of PAi with F1
Rfs3
diag(Rfs3) # these are the validities
sum(diag(Rfs3))/9 # this is the average validity
# # we have to compare the correlation between each estimated factor score and the underlying factor with the "true" correlation
# cbind(Rfs3[1,2],PF.9f.promax.globedata.part$Phi[2,1])
# cbind(Rfs3[2,1],PF.9f.promax.globedata.part$Phi[2,1])

# this computes the average univocality
check3 <- 0
check31 <- 0
check32 <- 0
for (i in 2:9) {
  for (j in 1:(i-1)) {
    check31 <- check31 + (PF.9f.promax.globedata.part$Phi[i,j]-Rfs3[i,j])^2
    check32 <- check32 + (PF.9f.promax.globedata.part$Phi[i,j]-t(Rfs3)[i,j])^2
    check3 <- ((check31+check32)/(9*8))/2
  }
}
check3 <- sqrt(check3)
check3

check1
check2
check3

#------------------------
# correlational accuracy
#------------------------

PF.9f.promax.globedata.part$Phi

cor(RegressionMethod$scores)
cor(BartlettMethod$scores)
cor(tenBergeMethod$scores)

# correlational accuracy of Thurstone scores
diff1=PF.9f.promax.globedata.part$Phi-cor(RegressionMethod$scores)
sum(sqrt(diff1^2))/(9*8)
# correlational accuracy of Bartlett scores
diff2=PF.9f.promax.globedata.part$Phi-cor(BartlettMethod$scores)
sum(sqrt(diff2^2))/(9*8)
# correlational accuracy of tenBerge scores
diff3=PF.9f.promax.globedata.part$Phi-cor(tenBergeMethod$scores)
sum(sqrt(diff3^2))/(9*8)


#####################################################################################
# second-order factor analysis using averages as measures of the first-order factors
#####################################################################################

# comparison of coarse factor scores with refined (ten Berge) factor scores
tenBergeMethod <- factor.scores(globedata.part, PF.9f.promax.globedata.part, Phi = NULL, method = c("tenBerge"),impute="none")
names(tenBergeMethod)
head(tenBergeMethod$scores)

UA = (UA2+UA3+UA4+UA5)/4
PD = (PD1+PD2+PD3+PD4)/4
COL1 = (COL12+COL13+COL14)/3
COL2 = (COL21+COL22+COL23+COL24)/4
GE = (GE1+GE2+GE4)/3
ASS = (ASS1+ASS2+ASS3+ASS4)/4
FO = (FO1+FO2+FO4)/3
PO = (PO1+PO2+PO3+PO4)/4
HO = (HO1+HO2+HO3+HO4)/4

coarsescores <- cbind(UA,PD,COL1,COL2,GE,ASS,FO,PO,HO)
tenBergeScores <- as.data.frame(tenBergeMethod$scores)
head(tenBergeScores)
names(tenBergeScores) <- c("COL2Tb","HOTb","GETb","PDTb","FOTb","ASSTb","POTb","COL1Tb","UATb")
cor(tenBergeScores,coarsescores)

describe(coarsescores)
cor(coarsescores)

# PDr <- 8- PD
globe9dim <- data.frame(UA,PD,COL1,COL2,GE,ASS,FO,PO,HO)
head(globe9dim)

#---------------------------------------------
# deciding on the number of factors to retain
#---------------------------------------------

# scree test
psych::scree(globe9dim)
# # nicer plot of eigenvalues of the reduced correlation matrix
# GetEigenvalues <- fa(globe9dim,nfactors = 1, fa="ml", SMC=TRUE, rotate = "none")
# GetEigenvalues$values
# n_factors <- length(GetEigenvalues$values)
# scree     <- data.frame(
#   Factor_n =  as.factor(1:n_factors), 
#   Eigenvalue = GetEigenvalues$values)
# ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
#   geom_point() + geom_line() +
#   geom_hline(yintercept=sum(GetEigenvalues$communality)/9) +
#   xlab("Number of factors") +
#   ylab("Eigenvalues") +
#   labs( title = "Scree Plot", 
#         subtitle = "(Based on the reduced correlation matrix)")

# parallel analysis
set.seed(13)
psych::fa.parallel(globe9dim,fa="fa",quant=.95)
# MAP test
psych::vss(globe9dim)

# principal axis factor analysis on 9 dimensions
psych::fa(globe9dim,fm="pa", nfactors=3, SMC=TRUE, rotate="Promax",max.iter=5000)

# using lavaan to estimate the EFAs
efa2nd.2f <- '
    efa("efa")*f1 + 
    efa("efa")*f2 =~ 
       UA+PD+COL1+COL2+GE+ASS+FO+PO+HO
'
efa2nd.3f <- '
    efa("efa")*f1 + 
    efa("efa")*f2 + 
    efa("efa")*f3 =~ 
       UA+PD+COL1+COL2+GE+ASS+FO+PO+HO
'
# constraining the PD unique variance to be nonnegative
efa2nd.3fcon <- '
    efa("efa")*f1 + 
    efa("efa")*f2 + 
    efa("efa")*f3 =~ 
       UA+PD+COL1+COL2+GE+ASS+FO+PO+HO
    PD ~~ a*PD
    a > 0
'

# using the efa function in lavaan (requires the latest version of lavaan)
fit.efa2nd.2f <- efa(efa2nd.2f,data = globe9dim, nfactors=2, rotation = "quartimin")
summary(fit.efa2nd.2f, standardized = TRUE)
fit.efa2nd.3f <- efa(efa2nd.3f,data = globe9dim, nfactors=3, rotation = "quartimin")
summary(fit.efa2nd.3f, standardized = TRUE)

# another way to get the efa solution in lavaaan using the cfa function
fit.cfa2nd.2f <- cfa(model = efa2nd.2f,data = globe9dim, rotation = "oblimin",
                  estimator = "ML", se = "standard")
summary(fit.cfa2nd.2f, standardized = TRUE)
# parameterEstimates(fit.cfa.9f)
# lavInspect(fit.cfa.9f, "std")$lambda
fit.cfa2nd.3f <- cfa(model = efa2nd.3fcon,data = globe9dim, rotation = "oblimin",
                     estimator = "ML", se = "standard")
summary(fit.cfa2nd.3f, standardized = TRUE)

# using different oblique rotations
efa2nd.promax <- psych::fa(globe9dim,fm="ml", nfactors=3, rotate="Promax")
#efa2nd.promax
efa2nd.quartimin <- psych::fa(globe9dim,fm="ml", nfactors=3, SMC=TRUE, rotate="quartimin")
#efa2nd.quartimin
efa2nd.geominQ <- psych::fa(globe9dim,fm="ml", nfactors=3, SMC=TRUE, rotate="geominQ")
#efa2nd.geominQ

factor.congruence(efa2nd.promax,efa2nd.quartimin)
factor.congruence(efa2nd.promax,efa2nd.geominQ)
factor.congruence(efa2nd.quartimin,efa2nd.geominQ)

#-----------------------------------
## oblique rotation via GPArotation
#-----------------------------------

efa2nd.unrotated <- psych::fa(globe9dim,fm="ml", nfactors=3, rotate="none")
efa2nd.unrotated$loadings

# oblimin quartimin rotation with gamma=0
GPArotation::oblimin(efa2nd.unrotated$loadings, Tmat=diag(ncol(efa2nd.unrotated$loadings)), 
                     gam=0, normalize=FALSE, eps=1e-5, maxit=1000)
# same as previous, but quartimin specified directly
GPArotation::quartimin(efa2nd.unrotated$loadings, Tmat=diag(ncol(efa2nd.unrotated$loadings)), 
                       normalize=FALSE, eps=1e-5, maxit=1000)
# same as previous, but quartimin specified in Crawford-Ferguson with kappa=0
GPArotation::cfQ(efa2nd.unrotated$loadings, Tmat=diag(ncol(efa2nd.unrotated$loadings)), 
                 kappa=0,normalize=FALSE, eps=1e-5, maxit=1000)
# oblique geomin
GPArotation::geominQ(efa2nd.unrotated$loadings, Tmat=diag(ncol(efa2nd.unrotated$loadings)), delta=.01, normalize=FALSE, eps=1e-5, maxit=1000)

LoadingComp <- cbind(loadings(efa2nd.promax), loadings(efa2nd.quartimin), loadings(efa2nd.geominQ))
LoadingComp 
#write.csv(round(LoadingComp,2), "ObliqueRotations.csv", row.names = F)

factor.congruence(loadings(efa2nd.promax),loadings(efa2nd.quartimin))
factor.congruence(loadings(efa2nd.promax),loadings(efa2nd.geominQ))
factor.congruence(loadings(efa2nd.quartimin),loadings(efa2nd.geominQ))


#######
# CFA
#######

# CFA for all 33 items
cfa<- '
 UA =~ NA*UA2+UA3+UA4+UA5
 PD =~ NA*PD1+PD2+PD3+PD4
 COL1 =~ NA*COL12+COL13+COL14
 COL2 =~ NA*COL21+COL22+COL23+COL24
 GE =~ NA*GE1+GE2+GE4
 ASS =~ NA*ASS1+ASS2+ASS3+ASS4
 FO =~ NA*FO1+FO2+FO4
 PO =~ NA*PO1+PO2+PO3+PO4
 HO =~ NA*HO1+HO2+HO3+HO4
 UA ~~ 1*UA
 PD ~~ 1*PD
 COL1 ~~ 1*COL1
 COL2 ~~ 1*COL2
 GE ~~ 1*GE
 ASS ~~ 1*ASS
 FO ~~ 1*FO
 PO ~~ 1*PO
 HO ~~ 1*HO
' 
fit_cfa<-lavaan::cfa(model=cfa,data=globedata.part)
cfa.fitmeasures <- lavaan::fitMeasures(fit_cfa, fit.measures = "all")
cfa.fitmeasures
summary(fit_cfa, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

# flexible cutoffs
fits.single <- gen_fit(mod1 = cfa, x = globedata.part, rep = 1000)
flex_co(fits = fits.single, index = c( "RMSEA", "SRMR", "CFI", "TLI"))

# robust estimators
fit_cfa.mlm <-lavaan::cfa(model=cfa,estimator="mlm",data=globedata.part)
cfa.mlm.fitmeasures <- lavaan::fitMeasures(fit_cfa.mlm, fit.measures = "all")
cfa.mlm.fitmeasures
summary(fit_cfa.mlm, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

fit_cfa.mlr <-lavaan::cfa(model=cfa,estimator="mlr",data=globedata.part)
cfa.mlr.fitmeasures <- lavaan::fitMeasures(fit_cfa.mlr, fit.measures = "all")
cfa.mlr.fitmeasures
summary(fit_cfa.mlr, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


# check residuals
lavResiduals(fit_cfa, type = "cor", zstat = TRUE, summary = TRUE)

# Check modification indices and EPCs

# Bonferroni critical value for lambda
BonLa <- qchisq(p=(1-.05/264), df=1)
BonLa

mi_cfa<-as.data.frame(modindices(fit_cfa))
largemila <- subset(mi_cfa,(mi_cfa$op=='=~' & mi_cfa$mi>BonLa))
order(largemila$mi,decreasing=TRUE)
print(largemila[order(largemila$mi, decreasing = TRUE), ]   )

# Bonferroni critical value for theta-delta
BonTd <- qchisq(p=(1-.05/(33*32/2)), df=1)
BonTd

largemitd <- subset(mi_cfa,(mi_cfa$op=='~~' & mi_cfa$mi>BonTd))
order(largemitd$mi,decreasing=TRUE)
print(largemitd[order(largemitd$mi, decreasing = TRUE), ]   )


# factor scores in lavaan
head(lavPredict(fit_cfa, type = "lv"))
cor(tenBergeScores,lavPredict(fit_cfa, type = "lv"))


# estimate the unrestricted factor model in lavaan (for comparison)

cfa.unrestr<- '
 UA =~ NA*UA2+start(1)*UA2+start(1)*UA3+start(1)*UA4+start(1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(.1)*COL12+start(.1)*COL13+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+
       start(.1)*GE1+start(.1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(.1)*FO1+start(.1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 PD =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(1)*PD1+start(1)*PD2+start(1)*PD3+start(1)*PD4+start(.1)*COL12+start(.1)*COL13+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+
      start(.1)*GE1+start(.1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(.1)*FO1+start(.1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 COL1 =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(1)*COL12+start(1)*COL13+start(1)*COL14+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+
       start(.1)*GE1+start(.1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(.1)*FO1+start(.1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 COL2 =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(.1)*COL12+start(.1)*COL13+start(1)*COL21+start(1)*COL22+start(1)*COL23+start(1)*COL24+
         start(.1)*GE1+start(.1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(.1)*FO1+start(.1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 GE =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(.1)*COL12+start(.1)*COL13+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+
       start(1)*GE1+start(1)*GE2+start(1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(.1)*FO1+start(.1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 ASS =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(.1)*COL12+start(.1)*COL13+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+
        start(.1)*GE1+start(.1)*GE4+start(1)*ASS1+start(1)*ASS2+start(1)*ASS3+start(1)*ASS4+start(.1)*FO1+start(.1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 FO =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(.1)*COL12+start(.1)*COL13+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+
       start(.1)*GE1+start(.1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(1)*FO1+start(1)*FO2+start(1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 PO =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(.1)*COL12+start(.1)*COL13+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+
       start(.1)*GE1+start(.1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(.1)*FO1+start(.1)*FO4+start(1)*PO1+start(1)*PO2+start(1)*PO3+start(1)*PO4+start(.1)*HO2+start(.1)*HO3+start(.1)*HO4
 HO =~ NA*UA2+start(.1)*UA2+start(.1)*UA4+start(.1)*UA5+start(.1)*PD1+start(.1)*PD2+start(.1)*PD4+start(.1)*COL12+start(.1)*COL13+start(.1)*COL21+start(.1)*COL23+start(.1)*COL24+start(.1)*
       GE1+start(.1)*GE4+start(.1)*ASS1+start(.1)*ASS2+start(.1)*ASS3+start(.1)*FO1+start(.1)*FO4+start(.1)*PO1+start(.1)*PO2+start(.1)*PO3+start(1)*HO1+start(1)*HO2+start(1)*HO3+start(1)*HO4
 UA ~~ 1*UA
 PD ~~ 1*PD
 COL1 ~~ 1*COL1
 COL2 ~~ 1*COL2
 GE ~~ 1*GE
 ASS ~~ 1*ASS
 FO ~~ 1*FO
 PO ~~ 1*PO
 HO ~~ 1*HO
' 
options(max.print = .Machine$integer.max)
fit_cfa.unrestr<-lavaan::cfa(model=cfa.unrestr,data=globedata.part)
lavaan::fitMeasures(fit_cfa.unrestr, fit.measures = "all")
summary(fit_cfa.unrestr, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

#print loadings greater than .3 in absolute value
library(plyr)
globedata.part.s <- plyr::numcolwise(scale)(globedata.part)
fit_cfa.unrestr.s<-lavaan::cfa(model=cfa.unrestr,data=globedata.part.s)
summary(fit_cfa.unrestr.s,standardized=TRUE)
partable.s <- parTable(fit_cfa.unrestr.s)
#partable.s
subset(lavMatrixRepresentation(partable.s),(abs(lavMatrixRepresentation(partable.s)$est) >= .3) & 
         lavMatrixRepresentation(partable.s)$op=='=~')

# chi-square difference test between congeneric and unrestricted EFA model 
anova(fit_cfa, fit_cfa.unrestr, test = "Chisq")

# check consistency of Phi (or Psi) matrices between congeneric and exploratory factor models

fit_cfa.unrestr.phi <- lavInspect(fit_cfa.unrestr, "std")$psi
fit_cfa.unrestr.phi

fit_cfa.phi <- lavInspect(fit_cfa, "std")$psi
fit_cfa.phi

ConPhi <- matrix(NA, nrow=9, ncol=9)
for (i in 2:9) {
  for (j in 1:(i-1)) {
    ConPhi[i,j] <- sqrt((fit_cfa.unrestr.phi[i,j] - fit_cfa.phi[i,j])^2)
  }
}  

ConPhi
sum(ConPhi, na.rm=TRUE)/36

ConPhialt <- matrix(NA, nrow=9, ncol=9)
for (i in 2:9) {
  for (j in 1:(i-1)) {
    ConPhialt[i,j] <- (fit_cfa.unrestr.phi[i,j] - fit_cfa.phi[i,j])
  }
}  

ConPhialt


##############################
# compute iir, ave, cr and sv
##############################

# store standardized estimates
std_cfa<-standardizedSolution(fit_cfa) 
std_cfa

# compute iir
iir_UA<-std_cfa[1:4,4]^2
iir_PD<-std_cfa[5:8,4]^2
iir_COL1<-std_cfa[9:11,4]^2
iir_COL2<-std_cfa[12:15,4]^2
iir_GE<-std_cfa[16:18,4]^2
iir_ASS<-std_cfa[19:22,4]^2
iir_FO<-std_cfa[23:25,4]^2
iir_PO<-std_cfa[26:29,4]^2
iir_HO<-std_cfa[30:33,4]^2
c(iir_UA,iir_PD,iir_COL1,iir_COL2,iir_GE,iir_ASS,iir_FO,iir_PO,iir_HO)
sum(c(iir_UA,iir_PD,iir_COL1,iir_COL2,iir_GE,iir_ASS,iir_FO,iir_PO,iir_HO))/33

# compute AVE
ave_UA<-mean(iir_UA[1:4])
ave_PD<-mean(iir_PD[1:4])
ave_COL1<-mean(iir_COL1[1:3])
ave_COL2<-mean(iir_COL2[1:4])
ave_GE<-mean(iir_GE[1:3])
ave_ASS<-mean(iir_ASS[1:4])
ave_FO<-mean(iir_FO[1:3])
ave_PO<-mean(iir_PO[1:4])
ave_HO<-mean(iir_HO[1:4])

c(ave_UA,ave_PD,ave_COL1,ave_COL2,ave_GE,ave_ASS,ave_FO,ave_PO,ave_HO)
AVE <- as.vector(c(ave_UA,ave_PD,ave_COL1,ave_COL2,ave_GE,ave_ASS,ave_FO,ave_PO,ave_HO))
AVE

# intermediate steps for CR
sql_UA<-sum(std_cfa[1:4,4])^2
sql_PD<-sum(std_cfa[5:8,4])^2
sql_COL1<-sum(std_cfa[9:11,4])^2
sql_COL2<-sum(std_cfa[12:15,4])^2
sql_GE<-sum(std_cfa[16:18,4])^2
sql_ASS<-sum(std_cfa[19:22,4])^2
sql_FO<-sum(std_cfa[23:25,4])^2
sql_PO<-sum(std_cfa[26:29,4])^2
sql_HO<-sum(std_cfa[30:33,4])^2


res_UA<-sum(std_cfa[43:46,4])
res_PD<-sum(std_cfa[47:50,4])
res_COL1<-sum(std_cfa[51:53,4])
res_COL2<-sum(std_cfa[54:57,4])
res_GE<-sum(std_cfa[58:60,4])
res_ASS<-sum(std_cfa[61:64,4])
res_FO<-sum(std_cfa[65:67,4])
res_PO<-sum(std_cfa[68:71,4])
res_HO<-sum(std_cfa[72:75,4])


# compute CR
cr_UA<-sql_UA/(sql_UA+res_UA)
cr_PD<-sql_PD/(sql_PD+res_PD)
cr_COL1<-sql_COL1/(sql_COL1+res_COL1)
cr_COL2<-sql_COL2/(sql_COL2+res_COL2)
cr_GE<-sql_GE/(sql_GE+res_GE)
cr_ASS<-sql_ASS/(sql_ASS+res_ASS)
cr_FO<-sql_FO/(sql_FO+res_FO)
cr_PO<-sql_PO/(sql_PO+res_PO)
cr_HO<-sql_HO/(sql_HO+res_HO)

c(cr_UA,cr_PD,cr_COL1,cr_COL2,cr_GE,cr_ASS,cr_FO,cr_PO,cr_HO)

# compute SV
fit_cfa_phi <- lavInspect(fit_cfa, "std")$psi
fit_cfa_phi
SharedVariance <- fit_cfa_phi^2
SharedVariance

# Fornell and Larcker criterion
# if entry in FL[i,j] is true, the criterion is satisfied
# values below (above) the diagonal are for the ith (jth) average variance extracted

FL <- matrix(NA, nrow=9, ncol=9)
for (i in 2:9) {
  for (j in 1:(i-1)) {
    FL[i,j] <- SharedVariance[i,j] < AVE[i]
  }
}
for (i in 1:8) {
  for (j in (i+1):9) {
    FL[i,j] <- SharedVariance[i,j] < AVE[j]
  }
}

FL


########################
# multi-sample analysis
########################

describe(globedata.part.ms)
table(binstress)

# specify unconstrained CFA model
cfa_2g_uc<- '
 UA =~ UA3+UA2+UA4+UA5
 PD =~ PD4+PD1+PD2+PD3
 COL1 =~ COL13+COL12+COL14
 COL2 =~ COL22+COL21+COL23+COL24
 GE =~ GE2+GE1+GE4
 ASS =~ ASS4+ASS1+ASS2+ASS3
 FO =~ FO2+FO1+FO4
 PO =~ PO4+PO1+PO2+PO3
 HO =~ HO1+HO2+HO3+HO4
'
fit_cfa_2g_uc <-lavaan::cfa(model=cfa_2g_uc,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_uc, fit.measures = "all")
#summary(fit_cfa_2g_uc)

# specify full metric invariance CFA model
cfa_2g_mi <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l5,l5)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l20,l20)*ASS2+c(l21,l21)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l32,l32)*HO3+c(l33,l33)*HO4
'
fit_cfa_2g_mi <-cfa(model=cfa_2g_mi,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_mi, fit.measures = "all")
#summary(fit_cfa_2g_mi)

# comparing the configural and full metric invariance models
anova(fit_cfa_2g_uc, fit_cfa_2g_mi, test = "Chisq")

# to get MI's for the invariant parameters 
Bon.metric <- qchisq(p=(1-.05/24), df=1)
Bon.metric
lavTestScore(fit_cfa_2g_mi, cumulative = TRUE)
# to see the parameter labels, use the following
parTable(fit_cfa_2g_mi)

#specify partial metric invariance CFA model 1 (relax invariance constraint on ASS3)
cfa_2g_pmi1 <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l5,l5)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l20,l20)*ASS2+c(l211,l212)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l32,l32)*HO3+c(l33,l33)*HO4
'
fit_cfa_2g_pmi1 <-cfa(model=cfa_2g_pmi1,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_pmi1, fit.measures = "all")
anova(fit_cfa_2g_mi,fit_cfa_2g_pmi1, test = "Chisq")
#summary(fit_cfa_2g_pmi1, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_pmi1, cumulative = TRUE)
# to see the parameter labels, use the following
parTable(fit_cfa_2g_pmi1)

#specify partial metric invariance CFA model 2
cfa_2g_pmi2 <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l51,l52)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l20,l20)*ASS2+c(l211,l212)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l32,l32)*HO3+c(l33,l33)*HO4
'
fit_cfa_2g_pmi2 <-cfa(model=cfa_2g_pmi2,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_pmi2, fit.measures = "all")
anova(fit_cfa_2g_pmi1,fit_cfa_2g_pmi2, test = "Chisq")
#summary(fit_cfa_2g_pmi2)
# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_pmi2, cumulative = TRUE)
# to see the parameter labels, use the following
parTable(fit_cfa_2g_pmi2)


#specify partial metric invariance CFA model 3
cfa_2g_pmi3 <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l51,l52)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l201,l202)*ASS2+c(l211,l212)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l32,l32)*HO3+c(l33,l33)*HO4
'
fit_cfa_2g_pmi3 <-cfa(model=cfa_2g_pmi3,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_pmi3, fit.measures = "all")
anova(fit_cfa_2g_pmi2,fit_cfa_2g_pmi3, test = "Chisq")
#summary(fit_cfa_2g_pmi3)
# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_pmi3, cumulative = TRUE)
# to see the parameter labels, use the following
parTable(fit_cfa_2g_pmi3)


#specify partial metric invariance CFA model 4
cfa_2g_pmi4 <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l51,l52)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l201,l202)*ASS2+c(l211,l212)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l321,l322)*HO3+c(l33,l33)*HO4
'
fit_cfa_2g_pmi4 <-cfa(model=cfa_2g_pmi4,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_pmi4, fit.measures = "all")
anova(fit_cfa_2g_pmi3,fit_cfa_2g_pmi4, test = "Chisq")
summary(fit_cfa_2g_pmi4)
# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_pmi4, cumulative = TRUE)
# to see the parameter labels, use the following
parTable(fit_cfa_2g_pmi4)

# comparing the final partial metric invariance model with the full metric invariance model
anova(fit_cfa_2g_mi, fit_cfa_2g_pmi4, test = "Chisq")
# comparing the final partial metric invariance model with the configural model
anova(fit_cfa_2g_uc, fit_cfa_2g_pmi4, test = "Chisq")

# specify the final partial metric invariance CFA model using different reference indicators
# (to make sure that the implicit equality constraints are in fact satisfied)
cfa_2g_pmi4alt <- '
 UA =~ UA2+c(l1,l1)*UA3+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD3+c(l7,l7)*PD4+c(l51,l52)*PD1+c(l6,l6)*PD2
 COL1 =~ COL14+c(l11,l11)*COL13+c(l9,l9)*COL12
 COL2 =~ COL23+c(l14,l14)*COL22+c(l12,l12)*COL21+c(l15,l15)*COL24
 GE =~ GE4+c(l18,l18)*GE2+c(l16,l16)*GE1
 ASS =~ ASS1+c(l19,l19)*ASS4+c(l201,l202)*ASS2+c(l211,l212)*ASS3
 FO =~ FO1+c(l23,l23)*FO2+c(l25,l25)*FO4
 PO =~ PO2+c(l27,l27)*PO4+c(l26,l26)*PO1+c(l28,l28)*PO3
 HO =~ HO4+c(l33,l33)*HO1+c(l31,l31)*HO2+c(l321,l322)*HO3
'
fit_cfa_2g_pmi4alt <-cfa(model=cfa_2g_pmi4alt,data=globedata.part.ms, group = "binstress")
#summary(fit_cfa_2g_pmi4alt, standardized=TRUE)
fitMeasures(fit_cfa_2g_pmi4alt, fit.measures = "all")
# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_pmi4alt, cumulative = TRUE)
# to see the parameter labels, use the following
parTable(fit_cfa_2g_pmi4alt)

#specify initial partial scalar invariance CFA model
cfa_2g_psi1 <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l51,l52)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l201,l202)*ASS2+c(l211,l212)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l321,l322)*HO3+c(l33,l33)*HO4
UA3 ~ 0*1
UA2 ~ c(t101,t102)*1
UA4 ~ c(t301,t302)*1
UA5 ~ c(t401,t402)*1
PD4 ~ 0*1
PD1 ~ c(t501,t502)*1
PD2 ~ c(t601,t602)*1
PD3 ~ c(t701,t702)*1
COL13 ~ 0*1
COL12 ~ c(t901,t902)*1
COL14 ~ c(t1101,t1102)*1
COL22 ~ 0*1
COL21 ~ c(t1201,t1202)*1
COL23 ~ c(t1401,t1402)*1
COL24 ~ c(t1501,t1502)*1
GE2 ~ 0*1
GE1 ~ c(t1601,t1602)*1
GE4 ~ c(t1801,t1802)*1
ASS4 ~ 0*1
ASS1 ~ c(t1901,t1902)*1
ASS2 ~ c(t2001,t2002)*1
ASS3 ~ c(t2101,t2102)*1
FO2 ~ 0*1
FO1 ~ c(t2301,t2302)*1
FO4 ~ c(t2501,t2502)*1
PO4 ~ 0*1
PO1 ~ c(t2601,t2602)*1
PO2 ~ c(t2701,t2702)*1
PO3 ~ c(t2801,t2802)*1
HO1 ~ 0*1
HO2 ~ c(t3101,t3102)*1
HO3 ~ c(t3201,t3202)*1
HO4 ~ c(t3301,t3302)*1
UA ~ NA*1
PD ~ NA*1
COL1 ~ NA*1
COL2 ~ NA*1
GE ~ NA*1
ASS ~ NA*1
FO ~ NA*1
PO ~ NA*1
HO ~ NA*1

t101==t102
t301==t302
t401==t402
t601==t602
t701==t702
t901==t902
t1101==t1102
t1201==t1202
t1401==t1402
t1501==t1502
t1601==t1602
t1801==t1802
t1901==t1902
t2301==t2302
t2501==t2502
t2601==t2602
t2701==t2702
t2801==t2802
t3101==t3102
t3301==t3302
'

fit_cfa_2g_psi1 <-cfa(model=cfa_2g_psi1,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_psi1, fit.measures = "all")
# summary(fit_cfa_2g_psi1)
# parameterEstimates(fit_cfa_2g_psi1, output = "text")
anova(fit_cfa_2g_pmi4,fit_cfa_2g_psi1, test = "Chisq")

# Bonferroni-adjusted critical value
Bon.scalar <- qchisq(p=(1-.05/20), df=1)
Bon.scalar

# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_psi1, cumulative = TRUE)
# to see the parameter labels, use the following
options(max.print = .Machine$integer.max)
parTable(fit_cfa_2g_psi1)

#specify partial scalar invariance CFA model 2
cfa_2g_psi2 <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l51,l52)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l201,l202)*ASS2+c(l211,l212)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l321,l322)*HO3+c(l33,l33)*HO4
UA3 ~ 0*1
UA2 ~ c(t101,t102)*1
UA4 ~ c(t301,t302)*1
UA5 ~ c(t401,t402)*1
PD4 ~ 0*1
PD1 ~ c(t501,t502)*1
PD2 ~ c(t601,t602)*1
PD3 ~ c(t701,t702)*1
COL13 ~ 0*1
COL12 ~ c(t901,t902)*1
COL14 ~ c(t1101,t1102)*1
COL22 ~ 0*1
COL21 ~ c(t1201,t1202)*1
COL23 ~ c(t1401,t1402)*1
COL24 ~ c(t1501,t1502)*1
GE2 ~ 0*1
GE1 ~ c(t1601,t1602)*1
GE4 ~ c(t1801,t1802)*1
ASS4 ~ 0*1
ASS1 ~ c(t1901,t1902)*1
ASS2 ~ c(t2001,t2002)*1
ASS3 ~ c(t2101,t2102)*1
FO2 ~ 0*1
FO1 ~ c(t2301,t2302)*1
FO4 ~ c(t2501,t2502)*1
PO4 ~ 0*1
PO1 ~ c(t2601,t2602)*1
PO2 ~ c(t2701,t2702)*1
PO3 ~ c(t2801,t2802)*1
HO1 ~ 0*1
HO2 ~ c(t3101,t3102)*1
HO3 ~ c(t3201,t3202)*1
HO4 ~ c(t3301,t3302)*1
UA ~ NA*1
PD ~ NA*1
COL1 ~ NA*1
COL2 ~ NA*1
GE ~ NA*1
ASS ~ NA*1
FO ~ NA*1
PO ~ NA*1
HO ~ NA*1

t101==t102
t301==t302
t401==t402
t601==t602
t701==t702
t901==t902
t1101==t1102
t1201==t1202
t1401==t1402
t1501==t1502
t1601==t1602
t1801==t1802
t1901==t1902
#t2301==t2302
t2501==t2502
t2601==t2602
t2701==t2702
t2801==t2802
t3101==t3102
t3301==t3302
'

fit_cfa_2g_psi2 <-cfa(model=cfa_2g_psi2,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_psi2, fit.measures = "all")
#summary(fit_cfa_2g_psi2)
#parameterEstimates(fit_cfa_2g_psi2, output = "text")

# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_psi2, cumulative = TRUE)

# comparing the final partial scalar invariance model with the configural model
anova(fit_cfa_2g_uc, fit_cfa_2g_psi2, test = "Chisq")

# print results for final partial scalar invariance model
summary(fit_cfa_2g_psi2)


# specify the final partial scalar invariance CFA model using different reference indicators
# (to make sure that the implicit equality constraints are in fact satisfied)
cfa_2g_psi2alt <- '
 UA =~ UA2+c(l1,l1)*UA3+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD3+c(l7,l7)*PD4+c(l51,l52)*PD1+c(l6,l6)*PD2
 COL1 =~ COL14+c(l11,l11)*COL13+c(l9,l9)*COL12
 COL2 =~ COL23+c(l14,l14)*COL22+c(l12,l12)*COL21+c(l15,l15)*COL24
 GE =~ GE4+c(l18,l18)*GE2+c(l16,l16)*GE1
 ASS =~ ASS1+c(l19,l19)*ASS4+c(l201,l202)*ASS2+c(l211,l212)*ASS3
 FO =~ FO4+c(l25,l25)*FO2+c(l23,l23)*FO1
 PO =~ PO2+c(l27,l27)*PO4+c(l26,l26)*PO1+c(l28,l28)*PO3
 HO =~ HO4+c(l33,l33)*HO1+c(l31,l31)*HO2+c(l321,l322)*HO3
UA2 ~ 0*1
UA3 ~ c(t101,t102)*1
UA4 ~ c(t301,t302)*1
UA5 ~ c(t401,t402)*1
PD2 ~ 0*1
PD1 ~ c(t501,t502)*1
PD4 ~ c(t601,t602)*1
PD3 ~ c(t701,t702)*1
COL14 ~ 0*1
COL12 ~ c(t901,t902)*1
COL13 ~ c(t1101,t1102)*1
COL23 ~ 0*1
COL21 ~ c(t1201,t1202)*1
COL22 ~ c(t1401,t1402)*1
COL24 ~ c(t1501,t1502)*1
GE4 ~ 0*1
GE1 ~ c(t1601,t1602)*1
GE2 ~ c(t1801,t1802)*1
ASS1 ~ 0*1
ASS4 ~ c(t1901,t1902)*1
ASS2 ~ c(t2001,t2002)*1
ASS3 ~ c(t2101,t2102)*1
FO4 ~ 0*1
FO1 ~ c(t2301,t2302)*1
FO2 ~ c(t2501,t2502)*1
PO2 ~ 0*1
PO1 ~ c(t2601,t2602)*1
PO4 ~ c(t2701,t2702)*1
PO3 ~ c(t2801,t2802)*1
HO4 ~ 0*1
HO2 ~ c(t3101,t3102)*1
HO3 ~ c(t3201,t3202)*1
HO1 ~ c(t3301,t3302)*1
UA ~ NA*1
PD ~ NA*1
COL1 ~ NA*1
COL2 ~ NA*1
GE ~ NA*1
ASS ~ NA*1
FO ~ NA*1
PO ~ NA*1
HO ~ NA*1

t101==t102
t301==t302
t401==t402
t601==t602
t701==t702
t901==t902
t1101==t1102
t1201==t1202
t1401==t1402
t1501==t1502
t1601==t1602
t1801==t1802
t1901==t1902
#t2301==t2302
t2501==t2502
t2601==t2602
t2701==t2702
t2801==t2802
t3101==t3102
t3301==t3302
'

fit_cfa_2g_psi2alt <-cfa(model=cfa_2g_psi2alt,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_psi2alt, fit.measures = "all")
# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_psi2alt, cumulative = TRUE)


#test invariance of latent factor mean
cfa_2g_kainv <- '
 UA =~ UA3+c(l1,l1)*UA2+c(l3,l3)*UA4+c(l4,l4)*UA5
 PD =~ PD4+c(l51,l52)*PD1+c(l6,l6)*PD2+c(l7,l7)*PD3
 COL1 =~ COL13+c(l9,l9)*COL12+c(l11,l11)*COL14
 COL2 =~ COL22+c(l12,l12)*COL21+c(l14,l14)*COL23+c(l15,l15)*COL24
 GE =~ GE2+c(l16,l16)*GE1+c(l18,l18)*GE4
 ASS =~ ASS4+c(l19,l19)*ASS1+c(l201,l202)*ASS2+c(l211,l212)*ASS3
 FO =~ FO2+c(l23,l23)*FO1+c(l25,l25)*FO4
 PO =~ PO4+c(l26,l26)*PO1+c(l27,l27)*PO2+c(l28,l28)*PO3
 HO =~ HO1+c(l31,l31)*HO2+c(l321,l322)*HO3+c(l33,l33)*HO4
UA3 ~ 0*1
UA2 ~ c(t101,t102)*1
UA4 ~ c(t301,t302)*1
UA5 ~ c(t401,t402)*1
PD4 ~ 0*1
PD1 ~ c(t501,t502)*1
PD2 ~ c(t601,t602)*1
PD3 ~ c(t701,t702)*1
COL13 ~ 0*1
COL12 ~ c(t901,t902)*1
COL14 ~ c(t1101,t1102)*1
COL22 ~ 0*1
COL21 ~ c(t1201,t1202)*1
COL23 ~ c(t1401,t1402)*1
COL24 ~ c(t1501,t1502)*1
GE2 ~ 0*1
GE1 ~ c(t1601,t1602)*1
GE4 ~ c(t1801,t1802)*1
ASS4 ~ 0*1
ASS1 ~ c(t1901,t1902)*1
ASS2 ~ c(t2001,t2002)*1
ASS3 ~ c(t2101,t2102)*1
FO2 ~ 0*1
FO1 ~ c(t2301,t2302)*1
FO4 ~ c(t2501,t2502)*1
PO4 ~ 0*1
PO1 ~ c(t2601,t2602)*1
PO2 ~ c(t2701,t2702)*1
PO3 ~ c(t2801,t2802)*1
HO1 ~ 0*1
HO2 ~ c(t3101,t3102)*1
HO3 ~ c(t3201,t3202)*1
HO4 ~ c(t3301,t3302)*1
UA ~ c(ka11,ka12)*1
PD ~ c(ka21,ka22)*1
COL1 ~ c(ka31,ka32)*1
COL2 ~ c(ka41,ka42)*1
GE ~ c(ka51,ka52)*1
ASS ~ c(ka61,ka62)*1
FO ~ c(ka71,ka72)*1
PO ~ c(ka81,ka82)*1
HO ~ c(ka91,ka92)*1

t101==t102
t301==t302
t401==t402
t601==t602
t701==t702
t901==t902
t1101==t1102
t1201==t1202
t1401==t1402
t1501==t1502
t1601==t1602
t1801==t1802
t1901==t1902
t2501==t2502
t2601==t2602
t2701==t2702
t2801==t2802
t3101==t3102
t3301==t3302

ka11==ka12
ka21==ka22
ka31==ka32
ka41==ka42
ka51==ka52
ka61==ka62
ka71==ka72
ka81==ka82
ka91==ka92
'
fit_cfa_2g_kainv <-cfa(model=cfa_2g_kainv,data=globedata.part.ms, group = "binstress")
fitMeasures(fit_cfa_2g_kainv, fit.measures = "all")
# test invariance of latent means
anova(fit_cfa_2g_psi2, fit_cfa_2g_kainv, test = "Chisq")
#summary(fit_cfa_2g_kainv)
# to get MI's for the invariant parameters 
lavTestScore(fit_cfa_2g_kainv, cumulative = TRUE)

# using semTools to perform (full) invariance tests
semTools::measurementInvariance(model=cfa_2g_uc, data = globedata.part.ms, group = "binstress")

# using the measEq.syntax function in semTools for testing full metric invariance
head(globedata.part.ms,)
fit.metricinv <- measEq.syntax(configural.model=cfa_2g_uc, data = globedata.part.ms,
              group = "binstress", ID.fac = "unit.loading",
              group.equal = c("loadings"),
              return.fit = TRUE)
summary(fit.metricinv)


# shortcut for the various model comparison tests (only full invariance tests)
# metric invariance
summary(cfa(cfa_2g_uc, data = globedata.part.ms, group = "binstress",group.equal = "loadings"))
# scalar invariance
summary(cfa(cfa_2g_uc, data = globedata.part.ms, group = "binstress",group.equal = c("intercepts", "loadings")))


# initialize a summary table with fit indices 
fittable_mg <- data.frame(matrix(ncol = 6, nrow= 4))
colnames(fittable_mg)<-c("model","chisq","df", "rmsea", "bic", "tli")
# store fit indices in a table
fittable_mg[1,1] <- 'uc'
fittable_mg[2,1] <- 'fullmi'
fittable_mg[3,1] <- 'partialmi'
fittable_mg[4,1] <- 'partialsi'
fittable_mg[1,2:6] <- fitMeasures(fit_cfa_2g_uc,c("chisq","df", "rmsea", "bic", "tli"))
fittable_mg[2,2:6] <- fitMeasures(fit_cfa_2g_mi,c("chisq","df",  "rmsea", "bic", "tli"))
fittable_mg[3,2:6] <- fitMeasures(fit_cfa_2g_pmi4,c("chisq","df",  "rmsea", "bic", "tli"))
fittable_mg[4,2:6] <- fitMeasures(fit_cfa_2g_psi2,c("chisq","df",  "rmsea", "bic", "tli"))
fittable_mg













