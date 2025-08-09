
######################################
## GOAL: fit unconditional linear growth curve models to children and caregiver data, and extract model fit measures


##### CHILDREN

data <- kiddata
#data <- data %>% dplyr::filter(HIV.status != "Participants exposed to HIV")
#data$HIV.status <- droplevels(data$HIV.status)

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

##########################
## Run the measurement model (only i and s modelled) and check that the fit is good

## Define LGM model
model <- "
      # intercept and slope with fixed coefficients
      i =~ 1*PHQ.T.0 + 1*PHQ.T.6 + 1*PHQ.T.12 + 1*PHQ.T.18 + 1*PHQ.T.24
      s =~ 0*PHQ.T.0 + 0.5*PHQ.T.6 + 1*PHQ.T.12 + 1.5*PHQ.T.18 + 2*PHQ.T.24
    "

## Run model
lgm <- growth(model, data = data, missing = "fiml", estimator = "MLR", se = "standard")
## Measurement model fit measures
summary(lgm, standardized = T, ci = T, fit.measures = T)

## Save model fit measures
output <- fitMeasures(lgm)
output <- as.data.frame(output)
colnames(output)[1] <- "fitMeasure"
output$fitMeasure <- round(output$fitMeasure, 3)
write.csv(output, "Results/Children - Unconditional LGM Fit Measures.csv", row.names = T)


rm(data, model, lgm, output)








##### ADULTS

data <- cgdata

## apply log transformation to PHQ and CRP to reduce non-normality
data$PHQ <- log(data$PHQ+1)
data$CRP <- log(data$CRP+1)

## scale all continuous variables
data$age <- as.numeric(scale(data$age))
data$PHQ <- as.numeric(scale(data$PHQ))
data$CRP <- as.numeric(scale(data$CRP))
data$Glu <- as.numeric(scale(data$Glu))
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

##########################
## Run the measurement model (only i and s modelled) and check that the fit is good

## Define LGM model
model <- "
      # intercept and slope with fixed coefficients
      i =~ 1*PHQ.T.0 + 1*PHQ.T.6 + 1*PHQ.T.12 + 1*PHQ.T.18 + 1*PHQ.T.24
      s =~ 0*PHQ.T.0 + 0.5*PHQ.T.6 + 1*PHQ.T.12 + 1.5*PHQ.T.18 + 2*PHQ.T.24
    "

## Run model
lgm <- growth(model, data = data, missing = "fiml", estimator = "MLR", se = "standard")
## Measurement model fit measures
summary(lgm, standardized = T, ci = T, fit.measures = T)

## Save model fit measures
output <- fitMeasures(lgm)
output <- as.data.frame(output)
colnames(output)[1] <- "fitMeasure"
output$fitMeasure <- round(output$fitMeasure, 3)
write.csv(output, "Results/Caregivers - Unconditional LGM Fit Measures.csv", row.names = T)



rm(data, model, lgm, output)
