# Varcov
Variance covariance for the sum of two coefficients from a mixed effect model.
Title: Variance and covariance for fixed effects (b2 and b3) and confidence intervals for |b3| - 2 *|b2|  from mixed models
Purpose: To compute variance and covariance of coefficients for the effect of Time and the interaction between logging status and time. And also, confidence intervals for the sum of the coefficients for the effect of time and the interaction between logging status and time.
This script runs the selected mixed effects model for each measure of turnover rates, and extracts
the variance for cTime, Logging status:cTime, and the covariance between cTime and Logging status:cTime.It then computes the variance for b2+b3, the standard error for this sum of coefficients and the confidence intervals too.
Files needed: Tobs_completetraits_2014_Sept26.csv, SES_completetraits_2014_Sept26.csv
Packages needed: nlme

######################################################################################################################################
#1. Preliminaries																				 #
######################################################################################################################################
#Load packages
library(nlme)

#Set working directory
setwd( "myworkingdirectory")  

#Read in files
 Tobs <- read.table("Tobs_fieldmeasured_2014_Sept26.csv", header=T, sep=",")
 SES <- read.table("SES_fieldmeasured_2014_Sept26.csv", header=T, sep=",")

head(Tobs)
head(SES)

######################################################################################################################
#2. Mixed models																				                                                             #
######################################################################################################################

Tsn2c <- lme(Tsn.corr~newlogging.status*cTime, random=~1|Transect, data=Tobs, method="ML",control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'))
a <- as.matrix(vcov(Tsn2c)) # extract variance covariance matrix 
Tsn_b2var <- a["cTime", "cTime"] # extract variance for coefficient b2 i.e. cTime
Tsn_b3var <- a["newlogging.status:cTime", "newlogging.status:cTime"] # extract variance for coefficient b3 i.e. newlogging.status:cTime 
Tsn_b2b3cov <- a["cTime","newlogging.status:cTime"] # extract covariance between b2 and b3
Tsn_b2 <- summary(Tsn2c)$tTable[3,1] #extract b2
Tsn_b3 <- summary(Tsn2c)$tTable[4,1] #extract b3
Tsn_b1 <- summary(Tsn2c)$tTable[2,1]#extract b1
Tsnb3_pval <- summary(Tsn2c)$tTable[4,5]

Tscbray2c <- lme(Tscbray.corr ~ newlogging.status*cTime, random=~1|Transect, method="ML",data= Tobs)
b <- as.matrix(vcov(Tscbray2c))
Tscbray_b2var <- b["cTime", "cTime"]
Tscbray_b3var <- b["newlogging.status:cTime", "newlogging.status:cTime"]
Tscbray_b2b3cov <- b["cTime","newlogging.status:cTime"]
Tscbray_b2 <- summary(Tscbray2c)$tTable[3,1]
Tscbray_b3 <- summary(Tscbray2c)$tTable[4,1]
Tscbray_b1 <- summary(Tscbray2c)$tTable[2,1]
Tscbrayb3_pval <- summary(Tscbray2c)$tTable[4,5]

SESbray3d <- lme(Tscbray_ses ~ newlogging.status*cTime+Latitude, random=~0+cTime|Transect, data= SES, method="ML")
c <- as.matrix(vcov(SESbray3d))
SESbray_b2var <- c["cTime", "cTime"]
SESbray_b3var <- c["newlogging.status:cTime", "newlogging.status:cTime"]
SESbray_b2b3cov <- c["cTime","newlogging.status:cTime"]
SESbray_b2 <- summary(SESbray3d)$tTable[3,1]
SESbray_b3 <- summary(SESbray3d)$tTable[5,1] #position of b3 is different because latitude is included in this selected model
SESbray_b1 <- summary(SESbray3d)$tTable[2,1]
SESbrayb3_pval <- summary(SESbray3d)$tTable[5,5]

Tscrao4c <- lme(log(Tscrao.corr) ~ newlogging.status*cTime, random=~1+cTime|Transect,method="ML",control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'), data= Tobs)
d <- as.matrix(vcov(Tscrao4c))
Tscrao_b2var <- d["cTime", "cTime"]
Tscrao_b3var <- d["newlogging.status:cTime", "newlogging.status:cTime"]
Tscrao_b2b3cov <- d["cTime","newlogging.status:cTime"]
Tscrao_b2 <- summary(Tscrao4c)$tTable[3,1]
Tscrao_b3 <- summary(Tscrao4c)$tTable[4,1]
Tscrao_b1 <- summary(Tscrao4c)$tTable[2,1]
Tscraob3_pval <- summary(Tscrao4c)$tTable[4,5]


SESTscrao3c <- lme(Tscrao_ses ~ newlogging.status*cTime, random=~0+cTime|Transect, data= SES, method="ML",control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'))
e <- as.matrix(vcov(SESTscrao3c))
SESTscrao_b2var <- e["cTime", "cTime"]
SESTscrao_b3var <- e["newlogging.status:cTime", "newlogging.status:cTime"]
SESTscrao_b2b3cov <- e["cTime","newlogging.status:cTime"]
SESTscrao_b2 <- summary(SESTscrao3c)$tTable[3,1]
SESTscrao_b3 <- summary(SESTscrao3c)$tTable[4,1]
SESTscrao_b1 <- summary(SESTscrao3c)$tTable[2,1]
SESTscraob3_pval <- summary(SESTscrao3c)$tTable[4,5]


Twdcwm2c <- lme(Twdcwm.corr~ newlogging.status*cTime, random=~1|Transect, data= Tobs, method="ML")
f <- as.matrix(vcov(Twdcwm2c))
Twdcwm_b2var <- f["cTime", "cTime"]
Twdcwm_b3var <- f["newlogging.status:cTime", "newlogging.status:cTime"]
Twdcwm_b2b3cov <- f["cTime","newlogging.status:cTime"]
Twdcwm_b2 <- summary(Twdcwm2c)$tTable[3,1]
Twdcwm_b3 <- summary(Twdcwm2c)$tTable[4,1]
Twdcwm_b1 <- summary(Twdcwm2c)$tTable[2,1]
Twdcwmb3_pval <- summary(Twdcwm2c)$tTable[4,5]


SESTwdswen3c <- lme(WDswen_ses ~ newlogging.status*cTime, random=~0+cTime|Transect, data= SES, method="ML")
g <- as.matrix(vcov(SESTwdswen3c))
SESTwdswen_b2var <- g["cTime", "cTime"]
SESTwdswen_b3var <- g["newlogging.status:cTime", "newlogging.status:cTime"]
SESTwdswen_b2b3cov <- g["cTime","newlogging.status:cTime"]
SESTwdswen_b2 <- summary(SESTwdswen3c)$tTable[3,1]
SESTwdswen_b3 <- summary(SESTwdswen3c)$tTable[4,1]
SESTwdswen_b1 <- summary(SESTwdswen3c)$tTable[2,1]
SESTwdswenb3_pval <- summary(SESTwdswen3c)$tTable[4,5]



SESTwdabsdiff3c <- lme(WDabsdif_ses ~ newlogging.status*cTime, random=~ 0+cTime|Transect, data=SES, method="ML")
h <- as.matrix(vcov(SESTwdabsdiff3c))
SESTwdabsdiff_b2var <- h["cTime", "cTime"]
SESTwdabsdiff_b3var <- h["newlogging.status:cTime", "newlogging.status:cTime"]
SESTwdabsdiff_b2b3cov <- h["cTime","newlogging.status:cTime"]
SESTwdabsdiff_b2 <- summary(SESTwdabsdiff3c)$tTable[3,1]
SESTwdabsdiff_b3 <- summary(SESTwdabsdiff3c)$tTable[4,1]
SESTwdabsdiff_b1 <- summary(SESTwdabsdiff3c)$tTable[2,1]
SESTwdabsdiffb3_pval <- summary(SESTwdabsdiff3c)$tTable[4,5]

THmaxcwm4c <- lme(sqrt(THmaxcwm.corr)~ newlogging.status*cTime, random=~ 1+cTime|Transect, data= Tobs, method="ML",control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'))
i <- as.matrix(vcov(THmaxcwm4c))
THmaxcwm_b2var <- i["cTime", "cTime"]
THmaxcwm_b3var <- i["newlogging.status:cTime", "newlogging.status:cTime"]
THmaxcwm_b2b3cov <- i["cTime","newlogging.status:cTime"]
THmaxcwm_b2 <- summary(THmaxcwm4c)$tTable[3,1]
THmaxcwm_b3 <- summary(THmaxcwm4c)$tTable[4,1]
THmaxcwm_b1 <- summary(THmaxcwm4c)$tTable[2,1]
THmaxcwmb3_pval <- summary(THmaxcwm4c)$tTable[4,5]

SESTHmaxswen3c <- lme(Hmaxswen_ses ~ newlogging.status*cTime, random=~0+cTime|Transect, data= SES, method="ML")
j <- as.matrix(vcov(SESTHmaxswen3c))
SESTHmaxswen_b2var <- j["cTime", "cTime"]
SESTHmaxswen_b3var <-j["newlogging.status:cTime", "newlogging.status:cTime"]
SESTHmaxswen_b2b3cov <-j["cTime","newlogging.status:cTime"]
SESTHmaxswen_b2 <- summary(SESTHmaxswen3c)$tTable[3,1]
SESTHmaxswen_b3 <- summary(SESTHmaxswen3c)$tTable[4,1]
SESTHmaxswen_b1 <- summary(SESTHmaxswen3c)$tTable[2,1]
SESTHmaxswenb3_pval <- summary(SESTHmaxswen3c)$tTable[4,5]

SESTHmaxabsdiff3c <- lme(Hmaxabsdif_ses ~ newlogging.status*cTime, random=~0+cTime|Transect, data= SES, method="ML")
k <- as.matrix(vcov(SESTHmaxabsdiff3c))
SESTHmaxabsdiff_b2var <- k["cTime", "cTime"]
SESTHmaxabsdiff_b3var <- k["newlogging.status:cTime", "newlogging.status:cTime"]
SESTHmaxabsdiff_b2b3cov <- k["cTime","newlogging.status:cTime"]
SESTHmaxabsdiff_b2 <- summary(SESTHmaxabsdiff3c)$tTable[3,1]
SESTHmaxabsdiff_b3 <- summary(SESTHmaxabsdiff3c)$tTable[4,1]
SESTHmaxabsdiff_b1 <- summary(SESTHmaxabsdiff3c)$tTable[2,1]
SESTHmaxabsdiffb3_pval <- summary(SESTHmaxabsdiff3c)$tTable[4,5]

Tdbhmaxcwm4c <- lme(sqrt(Tdbhmaxcwm.corr) ~ newlogging.status*cTime, random=~1+cTime|Transect, control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'),data= Tobs, method="ML")
l <- as.matrix(vcov(Tdbhmaxcwm4c))
Tdbhmaxcwm_b2var <- l["cTime", "cTime"]
Tdbhmaxcwm_b3var <- l["newlogging.status:cTime", "newlogging.status:cTime"]
Tdbhmaxcwm_b2b3cov <- l["cTime","newlogging.status:cTime"]
Tdbhmaxcwm_b2 <- summary(Tdbhmaxcwm4c)$tTable[3,1]
Tdbhmaxcwm_b3 <- summary(Tdbhmaxcwm4c)$tTable[4,1]
Tdbhmaxcwm_b1 <- summary(Tdbhmaxcwm4c)$tTable[2,1]
Tdbhmaxcwmb3_pval <- summary(Tdbhmaxcwm4c)$tTable[4,5]


SESTdbhmaxswen4c <- lme(DBHmaxswen_ses ~ newlogging.status*cTime, random=~1+cTime|Transect, control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'),data= SES, method="ML")
m <- as.matrix(vcov(SESTdbhmaxswen4c))
SESTdbhmaxswen_b2var <- m["cTime", "cTime"]
SESTdbhmaxswen_b3var <- m["newlogging.status:cTime", "newlogging.status:cTime"]
SESTdbhmaxswen_b2b3cov <- m["cTime","newlogging.status:cTime"]
SESTdbhmaxswen_b2 <- summary(SESTdbhmaxswen4c)$tTable[3,1]
SESTdbhmaxswen_b3 <- summary(SESTdbhmaxswen4c)$tTable[4,1]
SESTdbhmaxswen_b1 <- summary(SESTdbhmaxswen4c)$tTable[2,1]
SESTdbhmaxswenb3_pval <- summary(SESTdbhmaxswen4c)$tTable[4,5]


SESTdbhmaxabsdiff2c <- lme(DBHmaxabsdif_ses ~ newlogging.status*cTime, random=~1|Transect, data= SES, method="ML")
n <- as.matrix(vcov(SESTdbhmaxabsdiff2c))
SESTdbhmaxabsdiff_b2var <- n["cTime", "cTime"]
SESTdbhmaxabsdiff_b3var <- n["newlogging.status:cTime", "newlogging.status:cTime"]
SESTdbhmaxabsdiff_b2b3cov <- n["cTime","newlogging.status:cTime"]
SESTdbhmaxabsdiff_b2 <- summary(SESTdbhmaxabsdiff2c)$tTable[3,1]
SESTdbhmaxabsdiff_b3 <- summary(SESTdbhmaxabsdiff2c)$tTable[4,1]
SESTdbhmaxabsdiff_b1 <- summary(SESTdbhmaxabsdiff2c)$tTable[2,1]
SESTdbhmaxabsdiffb3_pval <- summary(SESTdbhmaxabsdiff2c)$tTable[4,5]


Tftrao2c <- lme(log(Tftrao.corr) ~ newlogging.status*cTime, random=~1|Transect, data= Tobs, method="ML")
o <- as.matrix(vcov(Tftrao2c))
Tftrao_b2var <- o["cTime", "cTime"]
Tftrao_b3var <- o["newlogging.status:cTime", "newlogging.status:cTime"]
Tftrao_b2b3cov <- o["cTime","newlogging.status:cTime"]
Tftrao_b2 <- summary(Tftrao2c)$tTable[3,1]
Tftrao_b3 <- summary(Tftrao2c)$tTable[4,1]
Tftrao_b1 <- summary(Tftrao2c)$tTable[2,1]
Tftraob3_pval <- summary(Tftrao2c)$tTable[4,5]

SESTftraoswen4c <- lme(Tftswen_ses ~ newlogging.status*cTime, random=~ 1+cTime|Transect,control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'), data= SES, method="ML")
p <- as.matrix(vcov(SESTftraoswen4c))
SESTftraoswen_b2var <- p["cTime", "cTime"]
SESTftraoswen_b3var <- p["newlogging.status:cTime", "newlogging.status:cTime"]
SESTftraoswen_b2b3cov <- p["cTime","newlogging.status:cTime"]
SESTftraoswen_b2 <- summary(SESTftraoswen4c)$tTable[3,1]
SESTftraoswen_b3 <- summary(SESTftraoswen4c)$tTable[4,1]
SESTftraoswen_b1 <- summary(SESTftraoswen4c)$tTable[2,1]
SESTftraoswenb3_pval <- summary(SESTftraoswen4c)$tTable[4,5]

SESTftrao4c <- lme(Tftrao_ses ~ newlogging.status*cTime, random=~1+cTime|Transect,control=list(msMaxIter = 10000, msVerbose = TRUE, opt='optim'), data= SES, method="ML")
q <- as.matrix(vcov(SESTftrao4c))
SESTftrao_b2var <- q["cTime", "cTime"]
SESTftrao_b3var <- q["newlogging.status:cTime", "newlogging.status:cTime"]
SESTftrao_b2b3cov <- q["cTime","newlogging.status:cTime"]
SESTftrao_b2 <- summary(SESTftrao4c)$tTable[3,1]
SESTftrao_b3 <- summary(SESTftrao4c)$tTable[4,1]
SESTftrao_b1 <- summary(SESTftrao4c)$tTable[2,1]
SESTftraob3_pval <- summary(SESTftrao4c)$tTable[4,5]

# create vectors with b2, b3, variance of b2, variance of b3 and covariance of b2 and b3 values from each model
b2 <- c(Tsn_b2, Tscbray_b2,SESbray_b2,Tscrao_b2 , SESTscrao_b2,Twdcwm_b2, SESTwdabsdiff_b2,SESTwdswen_b2 ,THmaxcwm_b2,SESTHmaxabsdiff_b2,SESTHmaxswen_b2,Tdbhmaxcwm_b2,SESTdbhmaxabsdiff_b2,SESTdbhmaxswen_b2,Tftrao_b2,SESTftrao_b2,SESTftraoswen_b2)
b3 <- c(Tsn_b3, Tscbray_b3,SESbray_b3,Tscrao_b3 , SESTscrao_b3,Twdcwm_b3,SESTwdabsdiff_b3, SESTwdswen_b3, THmaxcwm_b3,SESTHmaxabsdiff_b3,SESTHmaxswen_b3,Tdbhmaxcwm_b3,SESTdbhmaxabsdiff_b3,SESTdbhmaxswen_b3,Tftrao_b3,SESTftrao_b3,SESTftraoswen_b3)

b2var <- c(Tsn_b2var, Tscbray_b2var,SESbray_b2var,Tscrao_b2var ,SESTscrao_b2var,Twdcwm_b2var,SESTwdabsdiff_b2var,SESTwdswen_b2var, THmaxcwm_b2var, SESTHmaxabsdiff_b2var, SESTHmaxswen_b2var,Tdbhmaxcwm_b2var,SESTdbhmaxabsdiff_b2var,SESTdbhmaxswen_b2var,Tftrao_b2var,SESTftrao_b2var, SESTftraoswen_b2var)

b3var <- c(Tsn_b3var, Tscbray_b3var,SESbray_b3var,Tscrao_b3var ,SESTscrao_b3var,Twdcwm_b3var,SESTwdabsdiff_b3var,SESTwdswen_b3var, THmaxcwm_b3var, SESTHmaxabsdiff_b3var,SESTHmaxswen_b3var,Tdbhmaxcwm_b3var,SESTdbhmaxabsdiff_b3var,SESTdbhmaxswen_b3var,Tftrao_b3var,SESTftrao_b3var,SESTftraoswen_b3var)

b2b3cov <- c(Tsn_b2b3cov, Tscbray_b2b3cov,SESbray_b2b3cov,Tscrao_b2b3cov ,SESTscrao_b2b3cov,Twdcwm_b2b3cov,SESTwdabsdiff_b2b3cov,SESTwdswen_b2b3cov,THmaxcwm_b2b3cov,SESTHmaxabsdiff_b2b3cov,SESTHmaxswen_b2b3cov,Tdbhmaxcwm_b2b3cov,SESTdbhmaxabsdiff_b2b3cov, SESTdbhmaxswen_b2b3cov,Tftrao_b2b3cov,SESTftrao_b2b3cov,SESTftraoswen_b2b3cov)

# Compute variable for prediction III
|b3| - 2 *|b2|

b3minus2b2 <- abs(b3) - 2 * abs(b2)
#Estimate variance of |b3| - 2 *|b2| using formula 
#(r^2)*Var(X) + (s^2)*Var(Y) - 2*r*s*Cov(X,Y)
#Define constants
r <- 1
s <- 2


varb3minus2b2 <- (r^2)* b3var + (s^2)*b2var - 2*r*s*b2b3cov

#Estimate standard error for |b3| - 2 *|b2|

sevarb3minus2b2 <- sqrt(varb3minus2b2)

#Estimate confidence interval (CI) value 95%

CIvalue95 <- sevarb3minus2b2 * 1.96 

# Estimate upper (CIupper) and lower (CIlower) bounds of CI

b3minus2b2_CIupper95 <- b3minus2b2 + CIvalue95
b3minus2b2_CIlower95 <- b3minus2b2 - CIvalue95


#Estimate confidence interval (CI) value 99%

CIvalue99 <- sevarb3minus2b2 * 2.58

# Estimate upper (CIupper) and lower (CIlower) bounds of CI

b3minus2b2_CIupper99 <- b3minus2b2 + CIvalue99
b3minus2b2_CIlower99 <- b3minus2b2 - CIvalue99


#Estimate confidence interval (CI) value 90%

CIvalue90 <- sevarb3minus2b2 * 1.64

# Estimate upper (CIupper) and lower (CIlower) bounds of CI

b3minus2b2_CIupper90 <- b3minus2b2 + CIvalue90
b3minus2b2_CIlower90 <- b3minus2b2 - CIvalue90



#Create data frame with all variables combined

b3_pval <- c(Tsnb3_pval, Tscbrayb3_pval,SESbrayb3_pval,Tscraob3_pval, SESTscraob3_pval, Twdcwmb3_pval,SESTwdabsdiffb3_pval, SESTwdswenb3_pval, THmaxcwmb3_pval,SESTHmaxabsdiffb3_pval,SESTHmaxswenb3_pval,Tdbhmaxcwmb3_pval,SESTdbhmaxabsdiffb3_pval,SESTdbhmaxswenb3_pval,Tftraob3_pval,SESTftraob3_pval,SESTftraoswenb3_pval)

b2b3_predIII <- data.frame(b2, b3, b3_pval, b3minus2b2, b3minus2b2_CIupper90, b3minus2b2_CIlower90, b3minus2b2_CIupper95, b3minus2b2_CIlower95, b3minus2b2_CIupper99, b3minus2b2_CIlower99)

rownames (b2b3_predIII) <- c("Tsn", "Tscbray", "SESbray", "Tscrao", "SESTscrao", "Twdcwm", "SESTwdabsdiff", "SESTwdswen", "THmaxcwm", "SESTHmaxabsdiff", "SESTHmaxswen", "Tdbhmaxcwm", "SESTdbhmaxabsdiff", "SESTdbhmaxswen","Tftrao", "SESTftrao", "SESTftraoswen") 

#View data frame
head(b2b3_predIII)

#Export variance covariance and CI dataframe

write.table(b2b3_predIII, "b2b3_predIII_fieldmeasure_2014Oct28.csv", sep=",")



