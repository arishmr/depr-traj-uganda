rm(list = ls())

## Load data
alldata <- read.csv("Cleaned Data/Full_Dataset.csv")
alldata <- alldata %>%
  mutate(across(c(PID, HIV.status, group, sex), as.factor)) %>%
  mutate(across(c(age, CRP, Glu, PHQ, timepoint), as.numeric))

## Remove PID 5324.1 as possible CRP outlier
alldata <- alldata %>% dplyr::filter(PID != "5324.1")

## Separate datasets for kids and caregivers
kiddata <- alldata %>% dplyr::filter(group == "Children")
cgdata <- alldata %>% dplyr::filter(group == "Caregivers")




################# MISSING DATA
gg_miss_fct(alldata, group) # explore where data is missing

################# OUTCOME DISTRIBUTIONS
plotdata <- alldata %>%
  dplyr::select(PID, group,
                PHQ, CRP) %>%
  gather(key = "outcome",
         value = "value",
         PHQ, CRP,
         factor_key = T)

ggplot(plotdata, aes(value)) +
  stat_density() +
  facet_wrap(~ outcome, scale = "free") +
  theme_minimal() +
  labs(x = "value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Figures/Outcome Distributions.png", scale = 1.5, bg = "white")

plotdata <- alldata %>%
  dplyr::select(PID, group,
                PHQ, CRP) %>%
  gather(key = "outcome",
         value = "value",
         PHQ, CRP,
         factor_key = T)
plotdata$value <- plotdata$value + 1
plotdata$value <- log(plotdata$value)
ggplot(plotdata, aes(value)) +
  stat_density() +
  facet_wrap(~ outcome, scale = "free") +
  theme_minimal() +
  labs(x = "value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Figures/Outcome Distributions (Log-Transformed).png", scale = 1.5, bg = "white")

rm(plotdata)
