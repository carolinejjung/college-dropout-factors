library('dplyr')
# STEP 1: READ IN FILE & CLEAN DATA
raw <- read.csv(file.choose(), header=TRUE, sep=';') #import raw_data.csv
#dim(raw)
#colnames(raw)
sum(is.na(raw)) #no missing values

apply(raw[,c(7, 13, 20, 22:36)], MARGIN=2, FUN=summary) #quantitative variables
apply(raw[,c(1:6, 8:12, 14:19, 21, 37)], MARGIN=2, FUN=table) #categorical

cleanv1 <- raw

# drop rows that have "frequency of higher education" as the value for "qualification" variable
m <- which(cleanv1$Mother.s.qualification==6)
f <- which(cleanv1$Father.s.qualification==6)
p <- which(cleanv1$Previous.qualification==6)
cleanv1 <- cleanv1[-unique(c(m, f, p)),]

# STEP 1a: COMBINE CATEGORIES TOGETHER
# marital status
cleanv1$Marital.status[cleanv1$Marital.status==1] <- "single"
cleanv1$Marital.status[cleanv1$Marital.status %in% c(2,5)] <- "married"
cleanv1$Marital.status[cleanv1$Marital.status==3] <- "widowed"
cleanv1$Marital.status[cleanv1$Marital.status %in% c(4, 6)] <- "legally separated"

# application mode
cleanv1$Application.mode[!cleanv1$Application.mode %in% c(1,5,16,17,18)] <- "non-traditional"
cleanv1$Application.mode[cleanv1$Application.mode %in% c(1,5,16,17,18)] <- "traditional"

# course 
cleanv1$Course[cleanv1$Course %in% c(9238, 9853, 8014)] <- "public service/education"
cleanv1$Course[cleanv1$Course %in% c(9003, 33, 9119)] <- "agriculture & tech"
cleanv1$Course[cleanv1$Course %in% c(9130, 9085)] <- "animal health"
cleanv1$Course[cleanv1$Course %in% c(9500, 9556)] <- "(human) health"
cleanv1$Course[cleanv1$Course %in% c(9147, 9670, 9254, 9991)] <- "management & tourism"
cleanv1$Course[cleanv1$Course %in% c(171, 9070, 9773)] <- "design & communication"

# nationality
colnames(cleanv1)[8] <- "Nationality" #fix typo
cleanv1$Nationality[cleanv1$Nationality!=1] <- "not portuguese"
cleanv1$Nationality[cleanv1$Nationality==1] <- "portuguese"

# mother's occupation
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation == 99] <- "missing"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation == 90] <- "other situation"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation == 0] <- "student"

cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation == 1] <- "group1"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(2,122,123,125)] <- "group2"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(3,131,132,134)] <- "group3"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(4,141,143,144)] <- "group4"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(5,151,152,153)] <- "group5"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(6)] <- "group6"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(7,171,173,175)] <- "group7"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(8)] <- "group8"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(9,191,192,193,194)] <- "group9"
cleanv1$Mother.s.occupation[cleanv1$Mother.s.occupation %in% c(10)] <- "group10"
table(cleanv1$Mother.s.occupation)

# father's occupation
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation == 99] <- "missing"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation == 90] <- "other situation"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation == 0] <- "student"

cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(1,112,114)] <- "group1"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(2,121,122,123,124)] <- "group2"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(3,131,132,134,135)] <- "group3"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(4,141,143,144)] <- "group4"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(5,151,152,153,154)] <- "group5"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(6,161,163)] <- "group6"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(7,171,172,174,175)] <- "group7"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(8,181,182,183)] <- "group8"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(9,192,193,194,195)] <- "group9"
cleanv1$Father.s.occupation[cleanv1$Father.s.occupation %in% c(10,101,102,103)] <- "group10"
table(cleanv1$Father.s.occupation)

# mother's qualification
cleanv1$Mother.s.qualification[cleanv1$Mother.s.qualification %in% c(3,2,4,5,40,43,44)] <- "bachelors or higher"
cleanv1$Mother.s.qualification[cleanv1$Mother.s.qualification %in% c(39,18,22,41,42,31,33)] <- "specialized"
cleanv1$Mother.s.qualification[cleanv1$Mother.s.qualification %in% c(1,20)] <- "secondary"
cleanv1$Mother.s.qualification[cleanv1$Mother.s.qualification %in% c(19,12,10,14,15,29,38,30,26,11,37,36,35,9,13,25,27)] <- "less than secondary"
cleanv1$Mother.s.qualification[cleanv1$Mother.s.qualification==34] <- "unknown"

# father's qualification
cleanv1$Father.s.qualification[cleanv1$Father.s.qualification %in% c(3,2,4,5,40,43,44)] <- "bachelors or higher"
cleanv1$Father.s.qualification[cleanv1$Father.s.qualification %in% c(39,18,22,41,42,31,33)] <- "specialized"
cleanv1$Father.s.qualification[cleanv1$Father.s.qualification %in% c(1,20)] <- "secondary"
cleanv1$Father.s.qualification[cleanv1$Father.s.qualification %in% c(19,12,10,14,15,29,38,30,26,11,37,36,35,9,13,25,27)] <- "less than secondary"
cleanv1$Father.s.qualification[cleanv1$Father.s.qualification==34] <- "unknown"

# previous qualification
cleanv1$Previous.qualification[cleanv1$Previous.qualification %in% c(3,2,4,5,40,43,44)] <- "bachelors or higher"
cleanv1$Previous.qualification[cleanv1$Previous.qualification %in% c(39,18,22,41,42,31,33)] <- "specialized"
cleanv1$Previous.qualification[cleanv1$Previous.qualification %in% c(1,20)] <- "secondary"
cleanv1$Previous.qualification[cleanv1$Previous.qualification %in% c(19,12,10,14,15,29,38,30,26,11,37,36,35,9,13,25,27)] <- "less than secondary"
cleanv1$Previous.qualification[cleanv1$Previous.qualification==34] <- "unknown"

# target (response)
cleanv1$Target[cleanv1$Target %in% c("Enrolled", "Graduate")] <- "Not dropout"
cleanv1$Target[cleanv1$Target=="Dropout"] <- 1
cleanv1$Target[cleanv1$Target=="Not dropout"] <- 0

# STEP 1b: CHANGE CATEGORICAL VARIABLES AS FACTOR
cleanv1[,c(1:6, 8:12, 14:19, 21, 37)] <- lapply(cleanv1[,c(1:6, 8:12, 14:19, 21, 37)], FUN=as.factor) #categorical

# STEP 2: REDUCTION OF PREDICTOR VARIABLES
# STEP 2a: ELIMINATE PERFECT LINEARITY DEPENDENCY
cleanv2 <- cleanv1 %>%
  select(-contains("Curricular.units")) %>%
  select(-contains("Course")) %>%
  select(-contains("Nationality"))

cleanv2.quant <- cleanv2[,names(cleanv2) %in%
                     c("Previous.qualification..grade.", "Admission.grade", "Age.at.enrollment",
                       "Unemployment.rate", "Inflation.rate", "GDP")]

cleanv2.cat <- cleanv2[,!names(cleanv2) %in%
                         c("Previous.qualification..grade.", "Admission.grade", "Age.at.enrollment",
                           "Unemployment.rate", "Inflation.rate", "GDP")]

# STEP 2b: DEAL WITH HIGHLY CORRELATED PREDICTORS
# QUANTITATIVE VARS
library("usdm")
vifstep(x=cleanv2.quant, th=5)
# Based on vif step results, not removing anything

# CATEGORICAL VARS
library(vcd)
# Initialize empty matrix to store coefficients
empty_m <- matrix(ncol = length(cleanv2.cat[-1]),
                  nrow = length(cleanv2.cat[-1]),
                  dimnames = list(names(cleanv2.cat[-1]), 
                                  names(cleanv2.cat[-1])))
# Function that accepts matrix for coefficients and data and returns a correlation matrix
calculate_cramer <- function(m, df) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m, cleanv2.cat[-1])
cor_matrix[cor_matrix > 0.6] # The only values > 0.6 are 1, which should just be the diagonal of the matrix (confirmed visually)
# No multicollinearity > 0.6 --> keep all variables

# After checking both the quantitative and categorical variables for multicollinearity: (same as cleanv2)
cleanv3 <- cleanv2

# write.csv(cleanv3, "/Users/tayaerogers/Desktop/STAT 318/FinalProject/JungRogers_DataClean.csv", row.names=FALSE)
                     

## STEP 3: VARIABLE SELECTION AND MODEL COMPARISON
# Create train and test dataset
set.seed(1)
n <- dim(cleanv3)[1]

sample.index <- sample(1:nrow(cleanv3), size=(n*.7), replace=FALSE)
train <- cleanv3[sample.index, ]
test <- cleanv3[-sample.index, ]

# Run regressions
library('MASS')

glm_probit <- glm(Target ~ ., family = binomial(link = "probit"), data = train)
probit_AIC <- step(glm_probit,direct='both', k=2, trace=FALSE)
probit_BIC <- step(glm_probit,direct='both', k=log(n), trace=FALSE)
glm_logit <- glm(Target ~ ., family = binomial(link = "logit"), data = train) 
logit_AIC <- step(glm_logit,direct='both', k=2, trace=FALSE)
logit_BIC <- step(glm_logit,direct='both', k=log(n), trace=FALSE)
## NOTE: probit bic and logic bic have the same predictors

# Identify threshold level
library('caret')
ths.f <- data.frame(threshold=c(0.6, 0.5, 0.4, 0.3, 0.2),
                    sens_pAIC=rep(NA,5), Fmeas_pAIC=rep(NA,5),
                    sens_pBIC=rep(NA,5), Fmeas_pBIC=rep(NA,5),
                    sens_lAIC=rep(NA,5), Fmeas_lAIC=rep(NA,5),
                    sens_lBIC=rep(NA,5), Fmeas_lBIC=rep(NA,5))
reg_type <- list(probit_AIC, probit_BIC, logit_AIC, logit_BIC)
for (i in 1:dim(ths.f)[1]){
  threshold = ths.f[i,1]
  j=1
  for (reg in reg_type){
    pi.hat <- predict(reg,type="response", newdata=test)
    predicted_values <- ifelse(pi.hat>threshold,1,0)
    actual_values <- test$Target
    conf_matrix <-table(predicted_values, actual_values)
    conf_matrix
    sens <- conf_matrix[2,2]/sum(conf_matrix[,2])
    prec <- conf_matrix[2,2]/sum(conf_matrix[2,])
    F_meas <- (2*sens*prec)/(prec+sens)
    ths.f[i,j*2] <- sens
    ths.f[i,1+j*2] <- F_meas
    j = j+1
  }
}
# Based on how we want to trade-off between sensitivity and F-measure:
# --> Examine f-threshold = 0.2 and f-threshold = 0.3 for all regressions

# Compare the 4 reg models on various performance metrics
model_comp <- data.frame(reg_type=c("probit_AIC - 0.2", "probit_BIC - 0.2", "logit_AIC - 0.2", "logit_BIC - 0.2",
                                    "probit_AIC - 0.3", "probit_BIC - 0.3", "logit_AIC - 0.3", "logit_BIC - 0.3"),
                    acc=rep(NA,8), spec=rep(NA,8), sens=rep(NA,8),
                    prec=rep(NA,8), F_meas=rep(NA,8), auc=rep(NA,8))

library('pROC')
k=0
# reg_type <- list(probit_AIC, probit_BIC, logit_AIC, logit_BIC)
f_thresh <- c(0.2, 0.3)
for (thresh in f_thresh){
  for (reg in reg_type){
    k=k+1
    pi.hat2 <- predict(reg,type="response", newdata=test)
    predicted_values2 <- ifelse(pi.hat2>thresh,1,0)
    actual_values2 <- test$Target
    conf_matrix2 <-table(predicted_values2, actual_values2)
    acc <- (conf_matrix2[1,1]+conf_matrix2[2,2])/sum(conf_matrix2)
    spec <- conf_matrix2[1,1]/sum(conf_matrix2[,1])
    sens <- conf_matrix2[2,2]/sum(conf_matrix2[,2])
    prec <- conf_matrix2[2,2]/sum(conf_matrix2[2,])
    F_meas <- (2*sens*prec)/(prec+sens)
    auc <- auc(actual_values2, pi.hat2)
    
    model_comp[k,c('acc')] <- acc
    model_comp[k,c('spec')] <- spec
    model_comp[k,c('sens')] <- sens
    model_comp[k,c('prec')] <- prec
    model_comp[k,c('F_meas')] <- F_meas
    model_comp[k,c('auc')] <- auc
  }
}
# --> Based on this, choose probit_BIC/logit_BIC

# New regression with selected predictors but new coefficients:
best_model <- glm(Target ~ Mother.s.qualification + Admission.grade + Debtor + Tuition.fees.up.to.date + Gender + Scholarship.holder + Age.at.enrollment, family = binomial(link = "logit"), data = cleanv3) 

# Diagnostic plots
par(mfrow=c(1,1))
plot(best_model)
# Outliers identified: 2129, 2858, 1762, 831, 3028, 1221
cleanv3[c(2129, 2858, 1762, 831, 3028, 1221),]

# Delta deviance
library('blorr')
blr_plot_diag_difdev(best_model) + ylim(c(0,10))

## Final model
# Model summary
summary(best_model)

# Performance metrics
best_model_diag <- data.frame(reg_type=c("logit_BIC - 0.2"),
                         acc=rep(NA,1), spec=rep(NA,1), sens=rep(NA,1),
                         prec=rep(NA,1), F_meas=rep(NA,1), auc=rep(NA,1))

pi.hat_final <- predict(best_model,type="response")
predicted_values_final <- ifelse(pi.hat_final>0.2,1,0)
actual_values_final <- cleanv3$Target
conf_matrix_final <-table(predicted_values_final, actual_values_final)
acc_final <- (conf_matrix_final[1,1]+conf_matrix_final[2,2])/sum(conf_matrix_final)
spec_final <- conf_matrix_final[1,1]/sum(conf_matrix_final[,1])
sens_final <- conf_matrix_final[2,2]/sum(conf_matrix_final[,2])
prec_final <- conf_matrix_final[2,2]/sum(conf_matrix_final[2,])
F_meas_final <- (2*sens_final*prec_final)/(prec_final+sens_final)
auc_final <- auc(actual_values_final, pi.hat_final)

best_model_diag[1,c('acc')] <- acc_final
best_model_diag[1,c('spec')] <- spec_final
best_model_diag[1,c('sens')] <- sens_final
best_model_diag[1,c('prec')] <- prec_final
best_model_diag[1,c('F_meas')] <- F_meas_final
best_model_diag[1,c('auc')] <- auc_final

# Area under ROC curve (aka AUC) for chosen model
plot(roc(actual_values_final, pi.hat_final))