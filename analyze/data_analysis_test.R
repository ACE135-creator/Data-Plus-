
# Loading in Data####
setwd("~/Desktop/Summer2025")
PEH_data <- read.csv("death_tbl_wlinkedids_exportable.csv")

#View(PEH_data)

#view unique values in County column
table(PEH_data$l_place_rcounty_fct)

#viewing unique values in each column
for (colname in names(PEH_data)) {
  cat("Unique values in", colname, ":\n")
  print(unique(PEH_data[[colname]]))
  cat("\n")
}

library(ggplot2)

ggplot(PEH_data, aes(x = l_place_rcounty_fct)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Morbidity Per County", x = "County", y = "Number of Peopled") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Table Summary Age####

library(gtsummary)

PEH_data %>% 
  tbl_summary(
    include = c(l_person_age_num),    
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{median} ({p25}, {p75})",
      "{min}, {max}"
    ),)%>% 
  modify_caption("**Table 2. Age Distribution**")

#plotting a histogram of age distribution
library(ggplot2)

ggplot(PEH_data, aes(x = l_person_age_num)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(title = "Figure 1. Age Distribution Histogram", x = "Age", y = "Count")

#summary table of age distribution with bins
library(gtsummary)
library(dplyr)

PEH_data <- PEH_data %>%
  mutate(age_bin = cut(l_person_age_num, breaks = seq(0, 121, by = 10), right = FALSE))

PEH_data %>%
  select(age_bin) %>%
  tbl_summary(percent = "column") %>%
  bold_labels()%>%
  modify_caption("**Table 2. Summary Table of Age Distribution**")

#Table Summary Demographics####
PEH_data %>% 
  tbl_summary(include = c(l_person_raceeth_fct, l_person_sex_fct, l_person_age_num))  %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Demographic of Dataset**") %>%
  bold_labels()

PEH_data %>% 
  tbl_summary(include = c(l_place_rcounty_fct))  %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Demographic of Dataset**") %>%
  bold_labels()


#Concordance####
#PEH_data %>% 
# tbl_summary(by = l_person_raceeth_fct, include = c(linked_hmis_linkstatus, linked_vdrs_linkstatus, linked_sudors_linkstatus, linked_sudovdrs_linkstatus))  %>% 
 # add_p(pvalue_fun = label_style_pvalue(digits = 2))  %>% 
  #add_overall()  %>% 
  #bold_labels()
  
PEH_data %>% 
  tbl_summary(include = c(linked_hmis_linkstatus, linked_vdrs_linkstatus, linked_sudors_linkstatus, linked_sudovdrs_linkstatus))  %>% 
  bold_labels()%>%
  modify_caption("**Table 3. LinkedStatus**")

library(dplyr)
library(gtsummary)

# Create combined variables
PEH_data <- PEH_data %>%
  mutate(
    sudor_vdrs = paste(linked_vdrs_linkstatus, linked_sudors_linkstatus, sep = " | "),
    sudor_hmis = paste(linked_sudors_linkstatus, linked_hmis_linkstatus, sep = " | "),
    hmis_vdrs = paste(linked_hmis_linkstatus, linked_vdrs_linkstatus, sep = " | "),
    sudor_vdrs_hmis = paste(linked_hmis_linkstatus, linked_vdrs_linkstatus, linked_sudors_linkstatus, sep = " | ")
  )

PEH_data %>%
  select(sudor_vdrs, sudor_hmis, hmis_vdrs, sudor_vdrs_hmis) %>%
  tbl_summary(percent = "column") %>%
  bold_labels()%>%
  modify_caption("**Table 4. Concordance between HMIS VDRS and SUDORS**") 








#Cross-Sections ####
PEH_data %>% 
  tbl_summary(include = c(l_event_mech_fct))
