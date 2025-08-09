## GOAL: plot predicted intercepts and slopes for each participant against CRP, separately for caregivers and children

kidpredictedCRP <- read.csv("Results/Children - LavPredict CRP Cont.csv")
cgpredictedCRP <- read.csv("Results/Caregivers - LavPredict CRP Cont.csv")

## Create wide dataset for kiddata
data <- kiddata %>%
  pivot_wider(id_cols = c(PID, CRP, HIV.status, age, sex),
              names_from = timepoint,
              names_prefix = "PHQ.T.",
              names_sort = T,
              values_from = PHQ,
              values_fn = function (x) {as.numeric(x[1])})
## Remove duplicated PIDs
data <- data[!duplicated(data$PID), ]

## Add predicted intercepts and slopes to this dataset
kidplotdata <- cbind(data, kidpredictedCRP)
head(kidplotdata)
## Add label for participant type to this dataset
kidplotdata <- kidplotdata %>% mutate(Group = rep("Children"))
rm(data, kidpredictedCRP)

## Create wide dataset for caregiver data
data <- cgdata %>%
  pivot_wider(id_cols = c(PID, CRP, HIV.status, age, sex),
              names_from = timepoint,
              names_prefix = "PHQ.T.",
              names_sort = T,
              values_from = PHQ,
              values_fn = function (x) {as.numeric(x[1])})
## Remove duplicated PIDs
data <- data[!duplicated(data$PID), ]

## Add predicted intercepts and slopes to this dataset
cgplotdata <- cbind(data, cgpredictedCRP)
head(cgplotdata)
## Add label for participant type to this dataset
cgplotdata <- cgplotdata %>% mutate(Group = rep("Adults"))
rm(data, cgpredictedCRP)


## Combine datasets
plotdata <- rbind(kidplotdata, cgplotdata)
rm(kidplotdata, cgplotdata)
plotdata$HIV.status <- fct_relevel(plotdata$HIV.status, c("Participants without HIV", "Participants with HIV", "Participants exposed to HIV"))

####### Now create plots with facet wrapping for i and s separately for children and caregivers

i_plot <- ggplot(data = plotdata, aes(x = CRP, y = i)) +
  facet_wrap(fct_rev(Group) ~ ., scales = "free_x") +
  geom_smooth(method = "glm", aes(color = HIV.status, group = HIV.status), alpha = 0.3) +
  geom_point(aes(color = HIV.status, group = HIV.status), alpha = 0.1) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "CRP at Baseline (mg/L)", y = "Predicted Intercept of PHQ-9 Score", color = "HIV Status")
i_plot

## Keep cases only where at least two timepoints have been recorded for slope prediction
plotdata_s <- plotdata %>% rowwise() %>% filter(n_distinct(na.omit(c_across(PHQ.T.0:PHQ.T.24))) > 1)

s_plot <- ggplot(data = plotdata_s, aes(x = CRP, y = s)) +
  geom_point(aes(color = HIV.status, group = HIV.status), alpha = 0.1) +
  facet_wrap(fct_rev(Group) ~ ., scales = "free_x") +
  geom_smooth(method = "glm", aes(color = HIV.status, group = HIV.status), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "CRP at Baseline (mg/L)", y = "Predicted Slope of PHQ-9 Score", color = "HIV Status")
s_plot


### Save patchwork plot
((i_plot + theme(legend.position = "none")) / (s_plot + plot_layout(guides = "collect") & theme(legend.position = 'bottom'))) + plot_annotation(tag_levels = "A")
ggsave("Figures/Figure 2 - Interaction Plots.png", width = 8, height = 8)

rm(i_plot, s_plot)

rm(kidpredictedCRP, data, plotdata, plotdata_i, plotdata_s)
