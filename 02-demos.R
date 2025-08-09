################# MEDIAN FOLLOW-UP DURATION
demodata <- alldata
demodata %>%
  group_by(group) %>%
  summarise(followup = median(timepoint),
            q1 = quantile(timepoint, 0.25),
            q2 = quantile(timepoint, 0.75))


################# DEMO TABLE FOR CHILDREN

demodata <- alldata %>% dplyr::filter(group == "Children")
demodata$HIV.status <- droplevels(demodata$HIV.status)
demodata <- demodata %>% arrange(PID, timepoint) %>% distinct(PID, .keep_all = T) %>% dplyr::select(-c(PID, group, timepoint))
demodata <- demodata %>% mutate(PHQcat = ifelse(PHQ > 10, "High",
                                                ifelse(PHQ > 4, "Moderate", "Low")))

## Create gt_summary table by HIV status

tab1 <- tbl_summary(demodata,
                    by = HIV.status,
                    type = list(age ~ "continuous",
                                sex ~ "categorical",
                                CRP ~ "continuous",
                                Glu ~ "continuous",
                                PHQ ~ "continuous",
                                PHQcat ~ "categorical"
                    ),
                    statistic = list(
                      #c(CRP, Glu) ~ "{mean} ({sd})",
                      #c(age, PHQ) ~ "{median} ({p25}, {p75})",
                      all_continuous() ~ "{median} ({p25}, {p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 1,
                    label = list(
                      age ~ "Age (years)",
                      sex ~ "Sex",
                      CRP ~ "CRP (mg/L)",
                      Glu ~ "Glucose (mmol/L)",
                      PHQ ~ "Baseline PHQ-9 Score",
                      PHQcat ~ "Baseline PHQ-9 Score Category"
                    ),
                    missing_text = "(Missing)"
) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label() %>%
  add_significance_stars()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Demographics by Depression Status - Children.docx",
             pr_section = prop_section(page_size(orient = "landscape")))

rm(tab1, tab1.print, demodata)





################# DEMO TABLE FOR ADULTS

demodata <- alldata %>% dplyr::filter(group == "Caregivers")
demodata <- demodata %>% arrange(PID, timepoint) %>% distinct(PID, .keep_all = T) %>% dplyr::select(-c(PID, group, timepoint))
demodata$HIV.status <- droplevels(demodata$HIV.status)
demodata <- demodata %>% mutate(PHQcat = ifelse(PHQ > 10, "High",
                                                ifelse(PHQ > 4, "Moderate", "Low")))

## Create gt_summary table by HIV status

tab1 <- tbl_summary(demodata,
                    by = HIV.status,
                    type = list(age ~ "continuous",
                                sex ~ "categorical",
                                CRP ~ "continuous",
                                Glu ~ "continuous",
                                PHQ ~ "continuous",
                                PHQcat ~ "categorical"
                    ),
                    statistic = list(
                      #c(CRP, Glu) ~ "{mean} ({sd})",
                      #c(age, PHQ) ~ "{median} ({p25}, {p75})",
                      all_continuous() ~ "{median} ({p25}, {p75})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    digits = all_continuous() ~ 1,
                    label = list(
                      age ~ "Age (years)",
                      sex ~ "Sex",
                      CRP ~ "CRP (mg/L)",
                      Glu ~ "Glucose (mmol/L)",
                      PHQ ~ "Baseline PHQ-9 Score",
                      PHQcat ~ "Baseline PHQ-9 Score Category"
                    ),
                    missing_text = "(Missing)"
) %>%
  add_p() %>%
  add_overall() %>%
  add_stat_label() %>%
  add_significance_stars()

## Save gt_summary table to Word doc

tab1.print <- tab1 %>% as_flex_table()
save_as_docx(tab1.print, 
             path = "Results/Demographics by Depression Status - Caregivers.docx",
             pr_section = prop_section(page_size(orient = "landscape")))

rm(tab1, tab1.print, demodata)
