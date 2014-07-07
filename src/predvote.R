## Description: Distribution of vote in the pre
## Author: Gonzalo 
## Date: Mon May 26 20:38:00 PDT 2014

library(mice)
library(foreign)
library(randomForest)

setwd("~/Documents/datablog/predvote/")

prel <- read.spss('dta/DA3022.sav', to.data.frame=TRUE, use.value.labels=TRUE)

#################### renames and recodes ####################
recpart <- function(x) {
    x[x >= 98] <- NA
    x[x %in% c(6, 7, 8, 9)] <- 5
    x[x %in% c(12, 13)] <- 11
    x[x %in% c(21, 22)] <- 20
    return(x)
}

#################### a bit of cleaning ####################
## Vote
prel$voto <- recpart(prel$P18)
prel$recvoto <- recpart(prel$RECUERDO)
prel$otvoto <- recpart(prel$P20)
prel$otvoto[!prel$otvoto %in% c(1, 97)] <- 2
## Ideo
prel$ideo <- prel$P27
prel$ideo[prel$ideo >= 98] <- NA
## Other voting alternatives
prel$dudavoto1 <- recpart(prel$P16B01)
prel$dudavoto2 <- recpart(prel$P16B02)
## Sympathy
prel$simpatia <- recpart(prel$P19)
## Turnout probability
prel$probvoto <- prel$P22
prel$probvoto[prel$probvoto > 10] <- NA
## Govt and opposition evaluation
prel$ppgestion <- prel$P25
prel$psoeop <- prel$P26
## Sociodems
prel$sitlab <- prel$P31
prel$sitlab[prel$sitlab == 9] <- NA
prel$sexo <- prel$P28
prel$edad <- prel$P29 - 18
prel$educ <- prel$ESTUDIOS
prel$educ[prel$educ == 9] <- NA
## attitude
prel$europ <- prel$P3
prel$europ[prel$europ == 9] <- NA

#################### imputation ####################
## Data hacks to simplify testing
prel$simpatia[prel$simpatia == 30] <- NA ## For simplicity (only one case)
prel$CCAA[prel$CCAA == "Ceuta (Ciudad autónoma de)"] <- "Melilla (Ciudad autónoma de)"
toimp <- c('voto', 'sexo', 'edad', 'educ', 'simpatia', 'recvoto',
           'ideo', 'probvoto', 'ppgestion', 'psoeop', 'sitlab', 
           'dudavoto1', 'dudavoto2', 'CCAA', 'europ', 'otvoto')
impprel <- mice(prel[, toimp], m=1, method=c("", rep("pmm", 15)), maxit=10)
impprel <- complete(impprel)

#################### modeling ####################
## training sample
target <- impprel[is.na(impprel$voto), ]
impprel <- impprel[!is.na(impprel$voto), ]
n <- 0.25

training <- sample(row.names(impprel))[1:(nrow(impprel))*(1-n)]
testing <- setdiff(row.names(impprel), training)

training <- impprel[row.names(impprel) %in% training, ]
testing <- impprel[row.names(impprel) %in% testing, ]

## Build data                                                   
my_model <-  ~ ideo + factor(simpatia) + edad + factor(educ) +  
  ppgestion + psoeop + factor(europ) +                          
  factor(sitlab) + sexo + factor(recvoto) + factor(CCAA)        
                                                                

Mtraining <- model.matrix(my_model, data = training)                            
Mtesting <- model.matrix(my_model, data = testing)                              
Mtarget <- model.matrix(my_model, data = target)                                
                                                                                
mmodel <- randomForest(y=factor(training$voto), x=Mtraining, ntree=600, mtry=6) 
tmodel <- predict(mmodel, newdata=Mtesting)                                     
target_model <- predict(mmodel, newdata=Mtarget)                                
                                                                                
## ajuste en testing                                                            
print(table(tmodel, testing$voto))                                              
                                                                                
## Predicted vote                                                               
target$voto <- target_model                                                     
impprel <- rbind(impprel, target)                                               
                                                                                
## Predicted vote distribution                                                  
prop.table(xtabs(~ impprel$voto[impprel$voto != "97"]))                         

