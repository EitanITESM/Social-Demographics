#DATA SCIENCE AND STATISTICAL INFERENCE 
#PROJECT_DELIVERY1_VARIABLES_DISTIBUTION 
#24-02-21
#Christian Caceres Caceres A01209925
#Eitan Arakanchi Weil A01021508 



#1.PACKAGES INSTALLATION
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#Basic 
install.packages("dplyr")
install.packages("descr")
install.packages("pracma")
install.packages("vcd")
install.packages("modeest")
install.packages("devtools")
install.packages('readr', dependencies = TRUE, repos='http://cran.rstudio.com/')
devtools::install_github("BfRstats/rriskDistributions")
install.packages("rriskDistributions")

library(rriskDistributions)

#Statistical 
install.packages('FIT')



#2.DATABASE READING AND VARIABLE DEFINITION 
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#Read File  
setwd("C:\\Users\\Eitan\\Desktop\\Paper (DSSI)\\Programs")
df <-read.csv("CSV_Project_DSSI.csv")


names(df) #Assigns to the variable names the table data 

KC_State <- df$ï..KC_State
State <- df$State 
KC_Municipality <- df$KC_Municipality 
Munic <- df$Municipality

#Total Population in years 2000-2015 
TP_2015 <- df$TP_2015

#Population aged 6 to 14 that does not attend school in years 2000-2015 
PNAS_2015 <- df$PNAS_2015_2

#Population aged 15 years and over with incomplete basic education in years 2000-2015 
EI_15oP_2015 <- df$EI_15oP_2015_2

#Homes No Drainage in years 2000-2015 
HND_2015 <- df$HND_2015_2

#Homes that do not have electricity in years 2000-2015 
HNE_2015 <- df$HNE_2015_2

#Social Lag Index in years 2000-2015 
SLI_2015 <- df$SLI_2015

#Degree of Social Lag in years 2000-2015 
DSL_2015 <- df$DSL_2015

#Place occupied in the national context in years 2000-2015 
PONC_2015 <- df$PONC_2015


#----------------------------------------------------------------------------
#3.DESCRIPTIVE STATISTICS 

#Scatter plot of SLI_2015 and TP_2015 

plot(TP_2015, SLI_2015, main="Social Lag Index (SLI_2015) as a function of Total Population per Municipality (TP_2015)", xlab="Total Population per Municipality in 2015 (TP_2015)", ylab="Social Lag Index of municipalities in 2015 (SLI_2015)")
plot(TP_2015_T1,SLI_2015_T1, main="Social Lag Index (SLI_2015 T) as a function of Total Population per Municipality (TP_2015 T) with transformed variables", xlab="Total Population per Municipality in 2015 (TP_2015 T)", ylab="Social Lag Index of municipalities in 2015 (SLI_2015 T)")


#----------------------------------
#1-SLI_2015
hist(SLI_2015, main="Social Lag Index in 2015 (SLI_2015)", xlab="Social Lag Index 2015 (SLI_2015)", ylab="Frequency")
BP_SLI_2015 <- boxplot(SLI_2015, main="Social Lag Index in 2015 (SLI_2015)", ylab="Social Lag Index in 2015 (SLI_2015)")


#Transformations of SLI_2015 

#Ln 
SLI_2015_T1 <- log(SLI_2015)
hist(SLI_2015_T1, main="Natural Logarithm of Social Lag Index in 2015 (SLI_2015 T)", xlab="Natural Logarithm of Social Lag Index in 2015 (SLI_2015 T)", ylab="Frequency")
BP_SLI_2015_T1 <- boxplot(SLI_2015_T1, main="Natural Logarithm of Social Lag Index in 2015 (SLI_2015 T)",ylab="Natural Logarithm of Social Lag Index in 2015 (SLI_2015 T)")
#sqrt
SLI_2015_T2 <- sqrt(SLI_2015)
hist(SLI_2015_T2, main="Social Lag Index in 2015 (SLI_2015)", xlab="Social Lag Index 2015 (SLI_2015)", ylab="Frequency")
BP_SLI_2015_T2 <- boxplot(SLI_2015_T2, main="Social Lag Index in 2015 (SLI_2015)")

#----------------------------------
#2-TP_2015 
hist(TP_2015, main="Total Population per municipality in 2015 (TP_2015)", xlab="Total Population per municipality in 2015 (TP_2015)", ylab="Frequency")
BP_TP_2015 <- boxplot(TP_2015, main="Total Population per municipality in 2015 (TP_2015)", ylab="Total Population per municipality in 2015 (TP_2015)")

#ln
TP_2015_T1 <- log(TP_2015)
BP_TP_2015_T1 <- boxplot(TP_2015_T1, main="Natural Logarithm of Total Population per municipality in 2015 (TP_2015 T)",ylab="Natural Logarithm of Total Population per municipality in 2015 (TP_2015 T)")
hist(TP_2015_T1, main="Natural Logarithm of Total Population per municipality in 2015 (TP_2015 T) ", xlab="Natural Logarithm of Total Population per municipality in 2015 (TP_2015 T)", ylab="Frequency")


#6-PNAS_2015
hist(PNAS_2015, main="Percentage of young people not attending school in 2015 per municipality", xlab="Percentage of young people not attending school in 2015 per municipality", ylab="Frequency")
boxplot(PNAS_2015, main="Percentage of young people not attending school in 2015 per municipality")

#ln
PNAS_2015_T1 <- log(PNAS_2015)
BP_PNAS_2015_T <- boxplot(PNAS_2015_T1, main="Percentage of young people not attending school in 2015 per municipality with ln(PNAS_2015)")
hist(PNAS_2015_T1, main="Percentage of young people not attending school in 2015 per municipality with ln(PNAS_2015)", xlab="Percentage of young people not attending school in 2015 per municipality with sqrt(PNAS_2015)", ylab="Frequency")

#----------------------------------------------------------------------------


#4.DATA FITTING 
#----------------------------------
#1-SLI_2015
#Goodness of fit of the tranformation of ln(SLI_2015)
SLI_2015_F0 <- fit.cont(SLI_2015)
SLI_2015_F1 <- fit.cont(SLI_2015_T1)
SLI_2015_F2 <- fit.cont(SLI_2015_1.1.1)

#----------------------------------
#2-TP_2015 
#Goodness of fit of the tranformation of ln(SLI_2015)
TP_2015_F <- fit.cont(TP_2015_T1)

#----------------------------------
#3-EI_15oP_2015
#Goodness of fit of the tranformation of ln(SLI_2015)
EI_15oP_2015_F <- fit.cont(EI_15oP_2015)

#----------------------------------
#4-HND_2015
#Goodness of fit of the tranformation of sqrt(SLI_2015)
HND_2015_F <- fit.cont(HND_2015_T2)

#----------------------------------
#5-HNE_2015
HNE_2015_F <- fit.cont(HNE_2015_T2)

#----------------------------------
#6-PNAS_2015
PNAS_2015_F <- fit.cont(PNAS_2015_T1)


#----------------------------------------------------------------------------
#5.MLE Calculation  

x <- seq(-10, 10, by = .0001)
#---------------------------
#5.1 Hypothesis 1.1 
#mu_SLI_2015 
#var_SLI_2015 

#MLE 
SLI_2015_MLE <- rriskMLEdist(SLI_2015_T1,"norm")
SLI_2015_MLE$estimate
SLI_2015_MLE$hessian
SLI_2015_HI <- solve(SLI_2015_MLE$hessian) 
SLI_2015_CRLB <- diag(SLI_2015_HI)
SLI_2015_CRLB 



#MLE SLI_2015|TP>500000
SLI_2015_MLE_1.1.1 <- rriskMLEdist(SLI_2015_1.1.1,"norm")
SLI_2015_MLE_1.1.1$estimate
SLI_2015_MLE_1.1.1$hessian
SLI_2015_HI_1.1.1 <- solve(SLI_2015_MLE_1.1.1$hessian) 
SLI_2015_CRLB_1.1.1 <- diag(SLI_2015_HI_1.1.1)
SLI_2015_CRLB_1.1.1

#MLE SLI_2015|TP<=50000
SLI_2015_MLE_1.1.2 <- rriskMLEdist(SLI_2015_1.1.2,"norm")
SLI_2015_MLE_1.1.2$estimate
SLI_2015_MLE_1.1.2$hessian
SLI_2015_HI_1.1.2 <- solve(SLI_2015_MLE_1.1.2$hessian) 
SLI_2015_CRLB_1.1.2 <- diag(SLI_2015_HI_1.1.2)
SLI_2015_CRLB_1.1.2

#---------------------------
#5.2 Hypothesis 1.2
#mu_TP_2015
#var_TP_2015 

TP_2015_MLE <- rriskMLEdist(TP_2015_T1,"gamma")

#TP|SLI<1.6745
TP_2015_MLE_1.2.1 <- rriskMLEdist(TP_2015_1.2.1,"gamma")

#TP|SLI>=1.6745
TP_2015_MLE_1.2.2 <- rriskMLEdist(TP_2015_1.2.2,"gamma")

#--------------------------
#5.6 Hypothesis 3.2
#mu_EI_15oP_2015 
#var_EI_15oP_2015 

PNAS_2015_MLE <- rriskMLEdist(PNAS_2015_T1,"norm")

#PNAS_2015_HND_2015>=10 
PNAS_2015_MLE_1 <- rriskMLEdist(TP_2015_2.2.1,"norm")

#PNAS_2015_HND_2015<10 
PNAS_2015_MLE_2 <- rriskMLEdist(TP_2015_2.2.2,"norm")

#----------------------------------------------------------------------------
#6. SAMPLES PARTITION 

#6.1 Hypothesis 1.1
SLI_2015_1.1.1 <- SLI_2015_T1[TP_2015>50000]
SLI_2015_1.1.2 <- SLI_2015_T1[TP_2015<=50000]

#6.2 Hypothesis 1.2
TP_2015_1.2.1 <- TP_2015_T1[SLI_2015<1.6745]
TP_2015_1.2.2 <- TP_2015_T1[SLI_2015>=1.6745]

#6.3 Hypothesis 2.1
HNE_2015_2.1.1 <- HNE_2015_T2[TP_2015>20000]
HNE_2015_2.1.2 <- HNE_2015_T2[TP_2015<=20000]

#6.4 Hypothesis 2.2
TP_2015_2.2.1 <- TP_2015_T1[HND_2015>10]
TP_2015_2.2.2 <- TP_2015_T1[HND_2015<=10]


#6.5 Hypothesis 3.1
EI_15oP_2015_3.1.1 <- EI_15oP_2015[HNE_2015>=1]
EI_15oP_2015_3.1.2 <- EI_15oP_2015[HNE_2015<1]

#6.6 Hypothesis 3.2
PNAS_2015_3.2.1 <- PNAS_2015_T1[HND_2015>=10]
PNAS_2015_3.2.2 <- PNAS_2015_T1[HND_2015<10]


#----------------------------------------------------------------------------
#7 SIMULATION OF EXPECTED VS OBSERVED CRLB  

#-------------------------------------------------------
#7.1 SLI_2015_T1 --> NORMAL 
MLE_11 <- 0.5385939
MLE_12 <- 0.4181561
bias_11 <- c()
bias_12 <- c()

for (i in 1:1000){
  
  random_1<- rnorm(n=length(SLI_2015_T1),mean=MLE_11, sd=MLE_12 )
  
  MLE_sim1<- rriskMLEdist(random_1,"norm")
  MLE_s_11 <-MLE_sim1$estimate[1]
  MLE_s_12 <-MLE_sim1$estimate[2]
  
  bias_11[i]<- (MLE_s_11-MLE_11)^2
  bias_12[i]<- (MLE_s_12-MLE_12)^2
  
  bias_11 <- c(bias_11,bias_11[i])
  bias_12 <- c(bias_12,bias_12[i])
} 

plot(sort(bias_11),main="CRLB for mean of ln(SLI_2015)", xlab="samples", ylab="(MLE_Simulation-MLE)^2")
abline(h=0.000188218)

plot(sort(bias_12),main="CRLB for sd of ln(SLI_2015)", xlab="samples", ylab="(MLE_Simulation-MLE)^2")
abline(h=0.0000941)

#-------------------------------------------------------

#----------------------------------------------------------------------------
#8.GOODNESS OF FIT SIMULATION 
knitr::opts_chunk$set(echo = TRUE)
devtools::install_github("BfRstats/rriskDistributions")
library(rriskDistributions)
library(readxl)
library(MASS)

#-------------------------
#1-SLI_2015
output.df <- NULL

SLI_2015_S <- SLI_2015
P1H11 <- NA 
P2H11 <- NA 

for (i in 1:1000){
  loglike.vector <- rep(NA,3)
  loglike.max <- rep(NA,1)
  
  fit_1 <- if(!is.null(useFitdist(data2fit=SLI_2015_S, show.output = FALSE, "lnorm")))
  {useFitdist(data2fit=SLI_2015_S, show.output = FALSE, "lnorm") }
    
   else{-9999999}
  fit_2 <- if(!is.null(useFitdist(data2fit=SLI_2015_S, show.output = FALSE, "gamma")))
  {useFitdist(data2fit=SLI_2015_S, show.output = FALSE, "gamma")}
  else{-9999999}
  fit_3 <- if(!is.null(useFitdist(data2fit=SLI_2015_S, show.output = FALSE, "weibull")))
  {useFitdist(data2fit=SLI_2015_S, show.output = FALSE, "weibull")}
  else{-9999999}
  
  loglike.vector[1] <- fit_1$fit.list$Lognormal$loglik
  loglike.vector[2] <- fit_2$fit.list$Gamma$loglik
  loglike.vector[3] <- fit_3$fit.list$Weibull$loglik 
  
  loglike.max <- max(loglike.vector)
  
  if(loglike.max == loglike.vector[1]){
    distribution <- fit_1$fit.list$Lognormal$distname
    P1H11 <- fit_1$fit.list$Lognormal$estimate[1]
    P2H11 <- fit_1$fit.list$Lognormal$estimate[2]
    
    
  } else if(loglike.max == loglike.vector[2]){
    distribution <- fit_2$fit.list$Gamma$distname
    P1H11 <- NA 
    P2H11 <- NA 
    
  } else{
    distribution <- fit_3$fit.list$Weibull$distname
    P1H11 <- NA 
    P2H11 <- NA 
  }
  
  output.df <- rbind(output.df,data.frame(loglike.max,distribution,P1H11,P2H11))
  SLI_2015_S <- sample(SLI_2015,length(SLI_2015), replace = TRUE)
  
}

#output.df
lnorm<-nrow(output.df[output.df$distribution == 'lnorm',])
gamma<-nrow(output.df[output.df$distribution == 'gamma',])
weibull<-nrow(output.df[output.df$distribution == 'weibull',])

Total <- c(lnorm,gamma,weibull)
rows <- c("LogNormal","Gamma","Weibull")
dataframe <- as.data.frame(Total,row.names = rows)
dataframe


P1H11v <- na.omit(output.df$P1H11)
P2H11v <- na.omit(output.df$P2H11) 

P1H11_FDPRUEBA <- fitdistr(P1H11v,"lognormal")
P1H11_FDPRUEBA

#Prior Hyperparameters Calculation 
P1H11_FD <- fitdistr(P1H11v,"normal")
P1H11_FD
P2H11_FD <- fitdistr(P2H11v,"normal")
P2H11_FD


P1H11_PR <- P1H11_FD$estimate[1]
P1H11_PR
P2H11_PR <- P2H11_FD$estimate[1]
P2H11_PR 

#Posterior Hyperparameters Calculation 
#\H11 sigma^2 
SLI_2015_MLE <- rriskMLEdist(SLI_2015,"lnorm")
SLI_2015_m <- SLI_2015_MLE$estimate[1]
SLI_2015_sd <- SLI_2015_MLE$estimate[2]
H11_var <- (SLI_2015_sd)^2
H11_var

H11n <- P1H11_FD$n


#Sum Xi 
SUM_X_H11 <-sum((SLI_2015))


#P1H11_PO
P1H11_PO <- (1/((1/P2H11_PR)+(929/H11_var)))*((P1H11_PR/P2H11_PR)+(SUM_X_H11/H11_var))
P1H11_PO 


#P2H11_PO
P2H11_PO <- ((1/P2H11_PR)+(929/H11_var))^(-1)
P2H11_PO 


#P2H11_sd_PO 
P2H11_sd_PO <- sqrt(P2H11_PO)
P2H11_sd_PO




#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
#9. PRIOR AND POSTERIOR HYPERPARAMETERS 

#1.Hypothesis 1.1
#-------------------------

#6.1 Hypothesis 1.1
SLI_2015_1.1.1 <- SLI_2015_T1[TP_2015>50000]
SLI_2015_1.1.2 <- SLI_2015_T1[TP_2015<=50000]


#6.1.1 -SLI_2015_1.1.1 (C1)
output.df <- NULL
SLI_2015_S1 <- SLI_2015_1.1.1
P1H11_C1 <- NA 
P2H11_C1 <- NA 

for (i in 1:1000){
  fit_1 <- if(!is.null(useFitdist(data2fit=SLI_2015_S1, show.output = FALSE, "norm")))
  {useFitdist(data2fit=SLI_2015_S1, show.output = FALSE, "norm") }
  
  loglike.vector[1] <- fit_1$fit.list$Normal$loglik
  
  distribution <- fit_1$fit.list$Normal$distname
  P1H11_C1 <- fit_1$fit.list$Normal$estimate[1]
  P2H11_C1 <- fit_1$fit.list$Normal$estimate[2]
  
  output.df <- rbind(output.df,data.frame(loglike.max,distribution,P1H11_C1,P2H11_C1))
  SLI_2015_S1 <- sample(SLI_2015_1.1.1,length(SLI_2015_1.1.1), replace = TRUE)
}


#output.df
norm<-nrow(output.df[output.df$distribution == 'norm',])
Total <- c(norm)
rows <- c("Normal")
dataframe <- as.data.frame(Total,rows.names = rows)
dataframe


#ORIGINAL DISTRIBUTION PARAMETERS 
P1H11_C1v <- na.omit(output.df$P1H11_C1) #mu (Mean)  
P1H11_C1v

P2H11_C1v <- na.omit(output.df$P2H11_C1) #sigma (SD)
P2H11_C1v

#MLE and CRLB
SLI_2015_1.1.1_MLE <- rriskMLEdist(SLI_2015_1.1.1,"norm")
SLI_2015_1.1.1_P1 <- SLI_2015_1.1.1_MLE$estimate[1]
SLI_2015_1.1.1_P1
SLI_2015_1.1.1_P2 <- SLI_2015_1.1.1_MLE$estimate[2]
SLI_2015_1.1.1_P2

SLI_2015_1.1.1_HI <- solve(SLI_2015_1.1.1_MLE$hessian) 
SLI_2015_1.1.1_CRLB <- diag(SLI_2015_1.1.1_HI)
SLI_2015_1.1.1_CRLB 

#--> The parameter with less variance is the standard deviation (sigma)so we fix it for further analysis 

library(MASS)

#PRIOR PARAMETERS 
P1H11_C1v <- na.omit(output.df$P1H11_C1) #Mean 
P1H11_C1v

P2H11_C1v <- na.omit(output.df$P2H11_C1) #SD 
P2H11_C1v

P1H11_C1_PR <- fitdistr(P1H11_C1v,"normal")$estimate[1]
P1H11_C1_PR #Prior Hyperparameter 1 (mean) 
P2H11_C1_PR <- fitdistr(P2H11_C1v,"normal")$estimate[1]
P2H11_C1_PR #Prior Hyperparameter 2 (SD) 


#POSTERIOR HYPERPARAMETERS 
#Sum Xi 
SUM_X_H11_C1 <-sum(SLI_2015_1.1.1)

#n 
n_11_C1 <- length(SLI_2015_1.1.1)

#Sigma  
SLI_2015_1.1.1_P2 <- SLI_2015_1.1.1_MLE$estimate[2]
SLI_2015_1.1.1_P2

#P1H11_C1_PO
P1H11_C1_PO <- (1/((1/P2H11_C1_PR^2)+(n_11_C1/SLI_2015_1.1.1_P2^2)))*((P1H11_C1_PR/P2H11_C1_PR^2)+(SUM_X_H11_C1/SLI_2015_1.1.1_P2^2))
P1H11_C1_PO

#P2H11_C1_PO
P2H11_C1_PO <- ((1/P2H11_C1_PR^2)+(n_11_C1/SLI_2015_1.1.1_P2^2))^(-1)
P2H11_C1_PO 


#Then the Prior and Posterior Hyperparameters are 
P1H11_C1_PR 
P2H11_C1_PR #sd 
P1H11_C1_PO
P2H11_C1_PO #var 
P2H11_C1_PO_sd <- sqrt(P2H11_C1_PO) #var 
P2H11_C1_PO_sd


#--------------------------------------------------------------
#6.1.2-SLI_2015_1.1.2 (C2)
output.df <- NULL
SLI_2015_S2 <- SLI_2015_1.1.2
P1H11_C2 <- NA 
P2H11_C2 <- NA 

for (i in 1:1000){
  fit_1 <- if(!is.null(useFitdist(data2fit=SLI_2015_S2, show.output = FALSE, "norm")))
  {useFitdist(data2fit=SLI_2015_S2, show.output = FALSE, "norm") }
  
  loglike.vector[1] <- fit_1$fit.list$Normal$loglik
  
  distribution <- fit_1$fit.list$Normal$distname
  P1H11_C2 <- fit_1$fit.list$Normal$estimate[1]
  P2H11_C2 <- fit_1$fit.list$Normal$estimate[2]
  
  output.df <- rbind(output.df,data.frame(loglike.max,distribution,P1H11_C2,P2H11_C2))
  SLI_2015_S2 <- sample(SLI_2015_1.1.2,length(SLI_2015_1.1.2), replace = TRUE)
}

#output.df
norm<-nrow(output.df[output.df$distribution == 'norm',])
Total <- c(norm)
rows <- c("Normal")
dataframe <- as.data.frame(Total,rows.names = rows)
dataframe


#ORIGINAL DISTRIBUTION PARAMETERS 
P1H11_C2v <- na.omit(output.df$P1H11_C2) #mu (Mean)  
P1H11_C2v

P2H11_C2v <- na.omit(output.df$P2H11_C2) #sigma (SD)
P2H11_C2v

#MLE and CRLB
SLI_2015_1.1.2_MLE <- rriskMLEdist(SLI_2015_1.1.2,"norm")
SLI_2015_1.1.2_P1 <- SLI_2015_1.1.2_MLE$estimate[1]
SLI_2015_1.1.2_P1
SLI_2015_1.1.2_P2 <- SLI_2015_1.1.2_MLE$estimate[2]
SLI_2015_1.1.2_P2

SLI_2015_1.1.2_HI <- solve(SLI_2015_1.1.2_MLE$hessian) 
SLI_2015_1.1.2_CRLB <- diag(SLI_2015_1.1.2_HI)
SLI_2015_1.1.2_CRLB 

#--> The parameter with less variance is the standard deviation (sigma)so we fix it for further analysis 


#PRIOR PARAMETERS 
P1H11_C2v <- na.omit(output.df$P1H11_C2) #Mean 
P1H11_C2v

P2H11_C2v <- na.omit(output.df$P2H11_C2) #SD 
P2H11_C2v


P1H11_C2_PR <- fitdistr(P1H11_C2v,"normal")$estimate[1]
P1H11_C2_PR #Prior Hyperparameter 1 (mean) 
P2H11_C2_PR <- fitdistr(P2H11_C2v,"normal")$estimate[1]
P2H11_C2_PR #Prior Hyperparameter 2 (VAR) 


#POSTERIOR HYPERPARAMETERS 
#Sum Xi 
SUM_X_H11_C2 <-sum(SLI_2015_1.1.2)

#n 
n_11_C2 <- length(SLI_2015_1.1.2)

#Sigma  
SLI_2015_1.1.2_P2 <- SLI_2015_1.1.2_MLE$estimate[2]
SLI_2015_1.1.2_P2

#P1H11_C2_PO
P1H11_C2_PO <- (1/((1/P2H11_C2_PR^2)+(n_11_C2/SLI_2015_1.1.2_P2^2)))*((P1H11_C2_PR/P2H11_C2_PR^2)+(SUM_X_H11_C2/SLI_2015_1.1.2_P2^2))
P1H11_C2_PO

#P2H11_C2_PO
P2H11_C2_PO <- ((1/P2H11_C2_PR^2)+(n_11_C2/SLI_2015_1.1.2_P2^2))^(-1)
P2H11_C2_PO 


#Then the Prior and Posterior Hyperparameters are 
P1H11_C2_PR 
P2H11_C2_PR #sd 
P1H11_C2_PO
P2H11_C2_PO #var 
P2H11_C2_PO_sd <- sqrt(P2H11_C2_PO) #var 
P2H11_C2_PO_sd

#-------------------------#-------------------------#-------------------------


#----------------------------------------------------------------------------
# 10. RERESSION ANALYSIS 


#3D Plotting 
install.packages("plot3D")
library("plot3D")
scatter3D(TP_2015_T1,TP_2015_C,SLI_2015_T1, main="3D plot of the relation between the ln(SLI_2015) and the categories of TP_2015",xlab="ln(TP_2015)",
          ylab="TP_2015_C",zlab="ln(SLI_2015)")




# 10.1 LM SLI_2015 vs TP_2015 
#------------------------

#Linear model 
LM1 <- lm(SLI_2015_T1 ~ TP_2015_T1)
LM1
LM1_ANOVA <-anova(LM1)
LM1_ANOVA
LM1_SUM <- summary(LM1)
LM1_SUM

#Regression coefficients 
LM1_coef <- LM1$coefficients
LM1_coef

#Residuals 
LM1_res <- LM1$residuals
LM1_res

plot(SLI_2015_T1,LM1_res) #Residuals against Y 
plot(TP_2015_T1,LM1_res,main="Residuals of Linear Model against predictor variable TP_2015 T",xlab="ln(TP_2015)",ylab="Linear Model Residuals") #Residuals against X 

#Scatter plot + Regression line 
plot(TP_2015_T1,SLI_2015_T1,main="Linear Model of SLI_2015 T vs TP_2015 T",  xlab="ln(TP_2015)", ylab="ln(SLI_2015)")
abline(LM1,col='red')



#------------------------
#Generalized Linear model 
GLM1 <- glm(SLI_2015_T1 ~ TP_2015_T1, family = "gaussian")
GLM1
GLM1_ANOVA <-anova(GLM1)
GLM1_ANOVA
GLM1_SUM <- summary(GLM1)
GLM1_SUM

#Regression coefficients 
GLM1_coef <- GLM1$coefficients
GLM1_coef

#Residuals 
GLM1_res <- GLM1$residuals
GLM1_res

plot(SLI_2015_T1,GLM1_res) #Residuals against Y 
plot(TP_2015_T1,GLM1_res,main="Residuals of GLM1 vs predictor variable HND_2015") #Residuals against X 


#Scatter plot + Regression line 
plot(TP_2015_T1,SLI_2015_T1,main="GLM of PNAS_2015 vs HND_2015",  xlab="HND_2015", ylab="PNAS_2015")
abline(GLM1,col='blue')


#------------------------
#7.1.2 PNAS_2015 vs HND_2015_C 

#Variable Reading 
TP_2015_C <- df$TP_2015_C


#Linear model 
LM2 <- lm(SLI_2015_T1 ~ TP_2015_C)
LM2
LM2_ANOVA <-anova(LM2)
LM2_ANOVA
LM2_SUM <- summary(LM2)
LM2_SUM

#Regression coefficients 
LM2_coef <- LM2$coefficients
LM2_coef

#Residuals 
LM2_res <- LM2$residuals
LM2_res

plot(SLI_2015_T1,LM2_res) #Residuals against Y 
plot(TP_2015_C,LM2_res,main="Residuals of Linear Model against predictor variable TP_2015 C",xlab="TP_2015 C",ylab="Linear Model Residuals (ei)") #Residuals against X 





