data <- alldata


#################################
## CHILDREN TRAJECTORY PLOTS
#################################
## Goal: visualise the trajectories of depressive symptoms (PHQ-9 score) at various follow-up time-points for
## ALL kids together + all kids grouped by CRP or HIV status + kids stratified by HIV status AND CRP group

data <- data %>% dplyr::filter(group=="Children")

data <- data %>% mutate(CRPGroup = ifelse(CRP > 10, "High",
                                          ifelse(CRP > 3, "Moderate", "Low")))
data$CRPGroup <- as.factor(data$CRPGroup)

levels(data$CRPGroup)
data$CRPGroup <- fct_relevel(data$CRPGroup,
                             c("High", "Low", "Moderate"))

levels(data$HIV.status)
data$HIV.status <- fct_relevel(data$HIV.status,
                               c("Participants without HIV", "Participants exposed to HIV", "Participants with HIV"))

## Plot: overall trajectories in children

p1 <- ggplot(data, aes(as.factor(timepoint), PHQ, group = PID)) +
  geom_line(aes(color = PID), alpha = 0.1) +
  geom_smooth(method = "glm", aes(group = 1), alpha = 0.5, color = "midnightblue") +
  #stat_summary(aes(group = 1), fun = mean, geom = "line", size = 1.5, color = "midnightblue") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score")
ggsave("Figures/Children - Trajectories.png", width = 5, height = 5)

## Plot: overall trajectories in children zoomed in on y axis

p2 <- ggplot(data, aes(as.factor(timepoint), PHQ, group = PID)) +
  geom_line(aes(color = PID), alpha = 0.05) +
  geom_smooth(method = "glm", aes(group = 1), alpha = 0.5, color = "midnightblue") +
  theme_bw() +
  coord_cartesian(ylim = c(0,5)) +
  theme(legend.position = "none") +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score")
ggsave("Figures/Children - Trajectories Zoomed.png", width = 5, height = 5)

## Plot: p1 and p2 patched together

p1 + inset_element(p2, 0.4, 0.4, 0.99, 0.99) + plot_annotation(tag_levels = "A")
ggsave("Figures/Children - Trajectories Inset.png", width = 7, height = 6)

## Plot: trajectories in children by CRP group

ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = PID)) +
  #geom_line(aes(group = PID, color = CRPGroup), alpha = 0.05) +
  geom_smooth(method = "glm", aes(color = CRPGroup, group = CRPGroup), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,5)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "CRP at Baseline")
ggsave("Figures/Children - Trajectories by CRP Group.png", width = 5, height = 5)

## Plot: trajectories in children by HIV status group

ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = PID)) +
  #geom_line(aes(group = PID, color = HIV.status), alpha = 0.05) +
  geom_smooth(method = "glm", aes(color = HIV.status, group = HIV.status), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(ylim = c(0,5)) +
  theme(legend.text = element_text(size = 7), legend.title = element_text(size = 8)) +
  theme(legend.key.size = unit(4, 'mm')) +
  #guides(colour = guide_legend(nrow = 3), group = guide_legend(nrow = 3)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "HIV Status")
ggsave("Figures/Children - Trajectories by HIV status.png", width = 5, height = 5)

## Plot: trajectories in children by CRP group, with different sub-plots for each HIV status group

p5 <- ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = PID)) +
  geom_line(aes(group = PID, color = CRPGroup), alpha = 0.1) +
  geom_smooth(method = "glm", aes(color = CRPGroup, group = CRPGroup), alpha = 0.3) +
  facet_wrap( ~ HIV.status, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,NA)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "CRP at Baseline")
ggsave("Figures/Children - Trajectories by CRP x HIV status.png", width = 10, height = 5)



rm(data)




data <- alldata


#################################
## CAREGIVERS TRAJECTORY PLOTS
#################################
## Goal: visualise the trajectories of depressive symptoms (PHQ-9 score) at various follow-up time-points for
## ALL adults together + all adults grouped by CRP or HIV status + adults stratified by HIV status AND CRP group

data <- data %>% dplyr::filter(group=="Caregivers")

data <- data %>% mutate(CRPGroup = ifelse(CRP > 10, "High",
                                          ifelse(CRP > 3, "Moderate", "Low")))
data$CRPGroup <- as.factor(data$CRPGroup)

levels(data$CRPGroup)
data$CRPGroup <- fct_relevel(data$CRPGroup,
                             c("High", "Low", "Moderate"))

levels(data$HIV.status)
data$HIV.status <- fct_relevel(data$HIV.status,
                               c("Participants without HIV", "Participants with HIV"))
data$HIV.status <- droplevels(data$HIV.status)

## Plot: overall trajectories in adults

p3 <- ggplot(data, aes(as.factor(timepoint), PHQ, group = PID)) +
  geom_line(aes(color = PID), alpha = 0.1) +
  geom_smooth(method = "glm", aes(group = 1), alpha = 0.5, color = "midnightblue") +
  #stat_summary(aes(group = 1), fun = mean, geom = "line", size = 1.5, color = "midnightblue") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score")
ggsave("Figures/Caregivers - Trajectories.png", width = 5, height = 5)

## Plot: overall trajectories in adults zoomed in on y axis

p4 <- ggplot(data, aes(as.factor(timepoint), PHQ, group = PID)) +
  geom_line(aes(color = PID), alpha = 0.05) +
  geom_smooth(method = "glm", aes(group = 1), alpha = 0.5, color = "midnightblue") +
  theme_bw() +
  coord_cartesian(ylim = c(0,5)) +
  theme(legend.position = "none") +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score")
ggsave("Figures/Caregivers - Trajectories Zoomed.png", width = 5, height = 5)

## Plot: p3 and p4 patched together

p3 + inset_element(p4, 0.4, 0.4, 0.99, 0.99) + plot_annotation(tag_levels = "A")
ggsave("Figures/Caregivers - Trajectories Inset.png", width = 7, height = 6)

## Plot: trajectories in adults by CRP group

ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = PID)) +
  #geom_line(aes(group = PID, color = CRPGroup), alpha = 0.05) +
  geom_smooth(method = "glm", aes(color = CRPGroup, group = CRPGroup), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,5)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "CRP at Baseline")
ggsave("Figures/Caregivers - Trajectories by CRP Group.png", width = 5, height = 5)

## Plot: trajectories in adults by HIV status

ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = PID)) +
  #geom_line(aes(group = PID, color = HIV.status), alpha = 0.05) +
  geom_smooth(method = "glm", aes(color = HIV.status, group = HIV.status), alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(ylim = c(0,5)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "HIV Status")
ggsave("Figures/Caregivers - Trajectories by HIV status.png", width = 5, height = 5)

## Plot: trajectories in adults by CRP group, with different sub-plots for each HIV status group

p6 <- ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = PID)) +
  geom_line(aes(group = PID, color = CRPGroup), alpha = 0.1) +
  geom_smooth(method = "glm", aes(color = CRPGroup, group = CRPGroup), alpha = 0.3) +
  facet_wrap( ~ HIV.status, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,NA)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "CRP at Baseline")
ggsave("Figures/Caregivers - Trajectories by CRP x HIV status.png", width = 10, height = 5)


rm(data)

## Plot: trajectories in children and adults (final figure for publication)

p1 + (inset_element(p2, 0.5, 0.5, 0.99, 0.99) & theme(axis.title=element_text(size=8))) + 
  p3 + (inset_element(p4, 0.5, 0.5, 0.99, 0.99) & theme(axis.title=element_text(size=8))) +
  plot_annotation(tag_levels = "A")
ggsave("Figures/Figure 1 - Trajectories.png", width = 12, height = 6)

rm(plotdata, p1, p2, p3, p4)

## Plot: trajectories in children and adults by CRP group, with different sub-plots for each HIV status group

((p5 + theme(legend.position = "none")) / (p6 + plot_layout(guides = "collect") & theme(legend.position = 'bottom'))) + plot_annotation(tag_levels = "A") 
ggsave("Figures/Figure 2 - Trajectories by CRP x HIV Status.png", width = 7, height = 7)

rm(p5, p6)








#################################
## TRAJECTORY PLOTS BY HIV STATUS
#################################
## Goal: visualise the trajectories of depressive symptoms (PHQ-9 score) at various follow-up time-points for
## low vs high CRP sub-plots, all stratified by HIV status - so that we can visualise the interaction of HIV status x CRP



######## CHILDREN
data <- alldata
data <- data %>% dplyr::filter(group=="Children")

hist(data$CRP)
mean(data$CRP)
sd(data$CRP)

## how many participants have CRP > mean + 1SD
length(na.omit(data$CRP[data$CRP > mean(data$CRP, na.rm = T) + 1*sd(data$CRP, na.rm = T)]))
length(na.omit(data$CRP[data$CRP > 10]))
## how many participants have CRP < mean
length(na.omit(data$CRP[data$CRP <= mean(data$CRP, na.rm = T)]))
length(na.omit(data$CRP[data$CRP < 3]))

## Define low (< mean) and high CRP (> mean + 1SD)
data <- data %>% mutate(CRPGroup = case_when(
  CRP > mean(CRP, na.rm = T) + 1*sd(CRP, na.rm = T) ~ "High Baseline CRP",
  CRP <= mean(CRP, na.rm = T) ~ "Low Baseline CRP",
  TRUE ~ NA
  ))

data$CRPGroup <- as.factor(data$CRPGroup)
data <- data %>% dplyr::filter(CRPGroup == "High Baseline CRP" | CRPGroup == "Low Baseline CRP")
data$CRPGroup <- droplevels(data$CRPGroup)
levels(data$CRPGroup)
data$CRPGroup <- fct_relevel(data$CRPGroup,
                             c("Low Baseline CRP", "High Baseline CRP"))


levels(data$HIV.status)
data$HIV.status <- fct_relevel(data$HIV.status,
                               c("Participants without HIV", "Participants with HIV", "Participants exposed to HIV"))

## Plot: trajectories in children by HIV status, with different sub-plots for CRP groups, including individual trajectories

p1 <- ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = HIV.status)) +
  geom_line(aes(group = PID, color = HIV.status), alpha = 0.1) +
  geom_smooth(method = "glm", aes(group = HIV.status, color = HIV.status), alpha = 0.3) +
  facet_wrap( ~ CRPGroup, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,NA)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "")

## Plot: trajectories in children by HIV status, with different sub-plots for CRP groups, mean trajectories only

p2 <- ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = HIV.status)) +
  #geom_line(aes(group = PID, color = HIV.status), alpha = 0.1) +
  geom_smooth(method = "glm", aes(group = HIV.status, color = HIV.status), alpha = 0.3) +
  facet_wrap( ~ CRPGroup, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,NA)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "")

######## ADULTS
data <- alldata
data <- data %>% dplyr::filter(group=="Caregivers")


hist(data$CRP)
mean(data$CRP)
sd(data$CRP)

## how many participants have CRP > mean + 1SD
length(na.omit(data$CRP[data$CRP > mean(data$CRP, na.rm = T) + 1*sd(data$CRP, na.rm = T)]))
length(na.omit(data$CRP[data$CRP > 10]))
## how many participants have CRP < mean
length(na.omit(data$CRP[data$CRP <= mean(data$CRP, na.rm = T)]))
length(na.omit(data$CRP[data$CRP < 3]))

## Define low (< mean) and high CRP (> mean + 1SD)
data <- data %>% mutate(CRPGroup = case_when(
  CRP > mean(CRP, na.rm = T) + 1*sd(CRP, na.rm = T) ~ "High Baseline CRP",
  CRP <= mean(CRP, na.rm = T) ~ "Low Baseline CRP",
  TRUE ~ NA
))

data$CRPGroup <- as.factor(data$CRPGroup)
data <- data %>% dplyr::filter(CRPGroup == "High Baseline CRP" | CRPGroup == "Low Baseline CRP")
data$CRPGroup <- droplevels(data$CRPGroup)
levels(data$CRPGroup)
data$CRPGroup <- fct_relevel(data$CRPGroup,
                             c("Low Baseline CRP", "High Baseline CRP"))

data$HIV.status <- droplevels(data$HIV.status)
levels(data$HIV.status)
data$HIV.status <- fct_relevel(data$HIV.status,
                               c("Participants without HIV", "Participants with HIV"))

## Plot: trajectories in adults by HIV status, with different sub-plots for CRP groups, including individual trajectories

p3 <- ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = HIV.status)) +
  geom_line(aes(group = PID, color = HIV.status), alpha = 0.1) +
  geom_smooth(method = "glm", aes(group = HIV.status, color = HIV.status), alpha = 0.3) +
  facet_wrap( ~ CRPGroup, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,NA)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "")

## Plot: trajectories in adults by HIV status, with different sub-plots for CRP groups, mean trajectories only

p4 <- ggplot(data = data, aes(x = as.factor(timepoint), y = PHQ, group = HIV.status)) +
  #geom_line(aes(group = PID, color = HIV.status), alpha = 0.1) +
  geom_smooth(method = "glm", aes(group = HIV.status, color = HIV.status), alpha = 0.3) +
  facet_wrap( ~ CRPGroup, scales = "fixed") +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(axis.title = element_text(face="bold")) +
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0,NA)) +
  labs(x = "Timepoint (Months)", y = "PHQ-9 Score", color = "")

## p1 and p3 stitched together (includes individual trajectories)
(p1 / p3) + plot_annotation(tag_levels = "A", theme = theme(legend.position = "bottom")) + plot_layout(guides = "auto")
ggsave("Figures/Figure 2 - Trajectories by HIV Status x CRP (Individual).png", width = 7, height = 7)

# p2 and p4 stitched together (includes mean trajectories only)
(p2 / p4) + plot_annotation(tag_levels = "A", theme = theme(legend.position = "bottom")) + plot_layout(guides = "auto")
ggsave("Figures/Figure 2 - Trajectories by HIV Status x CRP.png", width = 7, height = 7)


rm(p1, p2, p3, p4, data)