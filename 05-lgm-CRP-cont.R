
######################################
## GOAL: determine whether baseline CRP and HIV status influence baseline PHQ-9 score or change in score over time


##### CHILDREN

data <- kiddata

## apply log transformation to PHQ and CRP to reduce non-normality
data$PHQ <- log(data$PHQ+1)
data$CRP <- log(data$CRP+1)

## scale all continuous variables
data$age <- as.numeric(scale(data$age))
data$PHQ <- as.numeric(scale(data$PHQ))
data$CRP <- as.numeric(scale(data$CRP))
data$sex <- as.numeric(data$sex)
data$sex <- as.numeric(scale(data$sex))

data <- data %>%
  pivot_wider(id_cols = c(PID, CRP, HIV.status, age, sex),
              names_from = timepoint,
              names_prefix = "PHQ.T.",
              names_sort = T,
              values_from = PHQ,
              values_fn = function (x) {as.numeric(x[1])})

## Remove duplicated PIDs
data <- data[!duplicated(data$PID), ]

data %>% group_by(HIV.status) %>% summarise(na = sum(is.na(PHQ.T.0)))


######## FULL LGM: CHILREN WITH HIV VS CHILDREN WITHOUT HIV VS CHILDREN EXPOSED TO HIV
subset0 <- data
subset0$HIV.status <- subset0$HIV.status %>% fct_recode("0" = "Participants without HIV",
                                                        "1" = "Participants exposed to HIV",
                                                        "2" = "Participants with HIV")
subset0$HIV.status <- as.numeric(as.character(subset0$HIV.status))
subset0$HIV.status <- as.numeric(scale(subset0$HIV.status))

## Now run model with relevant covariates and regressions
## Define LGM model
model <- "
      # intercept and slope with fixed coefficients
      i =~ 1*PHQ.T.0 + 1*PHQ.T.6 + 1*PHQ.T.12 + 1*PHQ.T.18 + 1*PHQ.T.24
      s =~ 0*PHQ.T.0 + 0.5*PHQ.T.6 + 1*PHQ.T.12 + 1.5*PHQ.T.18 + 2*PHQ.T.24
      # regressions
      i ~ CRP + HIV.status + CRP:HIV.status + age + sex
      s ~ CRP + HIV.status + CRP:HIV.status + age + sex
    "

## Run model
lgm <- growth(model, data = subset0, missing = "fiml", estimator = "MLR", se = "standard")
## View model output
summary(lgm, standardized = T, ci = T, fit.measures = T)

## Save output
output <- parameterEstimates(lgm) %>% filter(!is.na(pvalue))
output <- output %>% mutate(across(where(is.numeric), ~ round(.x, digits = 3)))
write.csv(output, "Results/Children - LGM CRP Cont - HIV_Cont.csv", row.names = F)


#########################################
## Predict values of intercept and slope for each participant
kidpredictedCRP <- as.data.frame(lavPredict(lgm))
write.csv(kidpredictedCRP, "Results/Children - LavPredict CRP Cont.csv", row.names = F)






########### Because our HIV status predictor has 3 levels, we will also need to run the LGMs pairwise
########### so we will now repeat the analysis 3 times, each time removing one of the HIV status groups (so that the resulting estimates provide a comparison of the remaining two groups)


######## COMPARISON 1: CHILREN WITH HIV VS CHILDREN WITHOUT HIV
subset1 <- data
subset1$HIV.status <- subset1$HIV.status %>% fct_recode("0" = "Participants without HIV",
                                                  "1" = "Participants exposed to HIV",
                                                  "2" = "Participants with HIV")
subset1 <- subset1 %>% dplyr::filter(HIV.status != "1")
subset1$HIV.status <- droplevels(subset1$HIV.status)
subset1$HIV.status <- as.numeric(as.character(subset1$HIV.status))
subset1$HIV.status <- as.numeric(scale(subset1$HIV.status))

summary(subset1$HIV.status)
sd(subset1$HIV.status)
hist(subset1$HIV.status)

## Now run model with relevant covariates and regressions
## Define LGM model
model <- "
      # intercept and slope with fixed coefficients
      i =~ 1*PHQ.T.0 + 1*PHQ.T.6 + 1*PHQ.T.12 + 1*PHQ.T.18 + 1*PHQ.T.24
      s =~ 0*PHQ.T.0 + 0.5*PHQ.T.6 + 1*PHQ.T.12 + 1.5*PHQ.T.18 + 2*PHQ.T.24
      # regressions
      i ~ CRP + HIV.status + CRP:HIV.status + age + sex
      s ~ CRP + HIV.status + CRP:HIV.status + age + sex
    "

## Run model
lgm <- growth(model, data = subset1, missing = "fiml", estimator = "MLR", se = "standard")
## View model output
summary(lgm, standardized = T, ci = T, fit.measures = T)

## Save output
output <- parameterEstimates(lgm) %>% filter(!is.na(pvalue))
output <- output %>% mutate(across(where(is.numeric), ~ round(.x, digits = 3)))
write.csv(output, "Results/Children - LGM CRP Cont - HIV_0vs2.csv", row.names = F)

rm(model, lgm, output)




######## COMPARISON 2: CHILREN WITH HIV VS CHILDREN EXPOSED TO HIV
subset2 <- data
subset2$HIV.status <- subset2$HIV.status %>% fct_recode("0" = "Participants without HIV",
                                                        "1" = "Participants exposed to HIV",
                                                        "2" = "Participants with HIV")
subset2 <- subset2 %>% dplyr::filter(HIV.status != "0")
subset2$HIV.status <- droplevels(subset2$HIV.status)
subset2$HIV.status <- as.numeric(as.character(subset2$HIV.status))
subset2$HIV.status <- as.numeric(scale(subset2$HIV.status))

## Now run model with relevant covariates and regressions
## Define LGM model
model <- "
      # intercept and slope with fixed coefficients
      i =~ 1*PHQ.T.0 + 1*PHQ.T.6 + 1*PHQ.T.12 + 1*PHQ.T.18 + 1*PHQ.T.24
      s =~ 0*PHQ.T.0 + 0.5*PHQ.T.6 + 1*PHQ.T.12 + 1.5*PHQ.T.18 + 2*PHQ.T.24
      # regressions
      i ~ CRP + HIV.status + CRP:HIV.status + age + sex
      s ~ CRP + HIV.status + CRP:HIV.status + age + sex
    "

## Run model
lgm <- growth(model, data = subset2, missing = "fiml", estimator = "MLR", se = "standard")
## View model output
summary(lgm, standardized = T, ci = T, fit.measures = T)

## Save output
output <- parameterEstimates(lgm) %>% filter(!is.na(pvalue))
output <- output %>% mutate(across(where(is.numeric), ~ round(.x, digits = 3)))
write.csv(output, "Results/Children - LGM CRP Cont - HIV_1vs2.csv", row.names = F)

rm(model, lgm, output)




######## COMPARISON 3: CHILREN WITHOUT HIV VS CHILDREN EXPOSED TO HIV
subset3 <- data
subset3$HIV.status <- subset3$HIV.status %>% fct_recode("0" = "Participants without HIV",
                                                        "1" = "Participants exposed to HIV",
                                                        "2" = "Participants with HIV")
subset3 <- subset3 %>% dplyr::filter(HIV.status != "2")
subset3$HIV.status <- droplevels(subset3$HIV.status)
subset3$HIV.status <- as.numeric(as.character(subset3$HIV.status))
subset3$HIV.status <- as.numeric(scale(subset3$HIV.status))

## Now run model with relevant covariates and regressions
## Define LGM model
model <- "
      # intercept and slope with fixed coefficients
      i =~ 1*PHQ.T.0 + 1*PHQ.T.6 + 1*PHQ.T.12 + 1*PHQ.T.18 + 1*PHQ.T.24
      s =~ 0*PHQ.T.0 + 0.5*PHQ.T.6 + 1*PHQ.T.12 + 1.5*PHQ.T.18 + 2*PHQ.T.24
      # regressions
      i ~ CRP + HIV.status + CRP:HIV.status + age + sex
      s ~ CRP + HIV.status + CRP:HIV.status + age + sex
    "

## Run model
lgm <- growth(model, data = subset3, missing = "fiml", estimator = "MLR", se = "standard")
## View model output
summary(lgm, standardized = T, ci = T, fit.measures = T)

## Save output
output <- parameterEstimates(lgm) %>% filter(!is.na(pvalue))
output <- output %>% mutate(across(where(is.numeric), ~ round(.x, digits = 3)))
write.csv(output, "Results/Children - LGM CRP Cont - HIV_0vs1.csv", row.names = F)

rm(model, lgm, output)


## Combine lavPredict values from all 3 subsets (will need to remove duplicates)
#subset1 <- cbind(subset1, kidpredictedCRP_0vs2) %>% dplyr::select(c(PID, i, s))
#subset2 <- cbind(subset2, kidpredictedCRP_1vs2) %>% dplyr::select(c(PID, i, s))
#subset3 <- cbind(subset3, kidpredictedCRP_0vs1) %>% dplyr::select(c(PID, i, s))

#kidpredictedCRP <- full_join(subset1, subset2, by = "PID")
#kidpredictedCRP <- kidpredictedCRP %>% dplyr::select(-c(i.y, s.y, PID)) %>% dplyr::rename(i = i.x, s = s.x)
#write.csv(kidpredictedCRP, "Results/Children - LavPredict CRP Cont.csv", row.names = F)

#rm(kidpredictedCRP_0vs1, kidpredictedCRP_0vs2, kidpredictedCRP_1vs2)
rm(subset1, subset2, subset3)
rm(subset0)








##### ADULTS

data <- cgdata

## apply log transformation to PHQ and CRP to reduce non-normality
data$PHQ <- log(data$PHQ+1)
data$CRP <- log(data$CRP+1)

## scale all continuous variables
data$age <- as.numeric(scale(data$age))
data$PHQ <- as.numeric(scale(data$PHQ))
data$CRP <- as.numeric(scale(data$CRP))
data$sex <- as.numeric(data$sex)
data$sex <- as.numeric(scale(data$sex))

data <- data %>%
  pivot_wider(id_cols = c(PID, CRP, HIV.status, age, sex),
              names_from = timepoint,
              names_prefix = "PHQ.T.",
              names_sort = T,
              values_from = PHQ,
              values_fn = function (x) {as.numeric(x[1])})

## Remove duplicated PIDs
data <- data[!duplicated(data$PID), ]

data %>% group_by(HIV.status) %>% summarise(na = sum(is.na(PHQ.T.0)))

data$HIV.status <- data$HIV.status %>% fct_recode("0" = "Participants without HIV",
                                                  "1" = "Participants with HIV")
data$HIV.status <- as.numeric(as.character(data$HIV.status))
data$HIV.status <- as.numeric(scale(data$HIV.status))


## Now run model with relevant covariates and regressions
## Define LGM model
model <- "
      # intercept and slope with fixed coefficients
      i =~ 1*PHQ.T.0 + 1*PHQ.T.6 + 1*PHQ.T.12 + 1*PHQ.T.18 + 1*PHQ.T.24
      s =~ 0*PHQ.T.0 + 0.5*PHQ.T.6 + 1*PHQ.T.12 + 1.5*PHQ.T.18 + 2*PHQ.T.24
      # regressions
      i ~ CRP + HIV.status + CRP:HIV.status + age + sex
      s ~ CRP + HIV.status + CRP:HIV.status + age + sex
    "

## Run model
lgm <- growth(model, data = data, missing = "fiml", estimator = "MLR", se = "standard")
## View model output
summary(lgm, standardized = T, ci = T, fit.measures = T)

## Save output
output <- parameterEstimates(lgm) %>% filter(!is.na(pvalue))
output <- output %>% mutate(across(where(is.numeric), ~ round(.x, digits = 3)))
write.csv(output, "Results/Caregivers - LGM CRP Cont.csv", row.names = F)


#########################################
## Predict values of intercept and slope for each participant
cgpredictedCRP <- as.data.frame(lavPredict(lgm))
write.csv(cgpredictedCRP, "Results/Caregivers - LavPredict CRP Cont.csv", row.names = F)

rm(data, model, lgm, output, kidpredictedCRP, cgpredictedCRP)
