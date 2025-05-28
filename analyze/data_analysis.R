
# Loading in Data####
setwd("~/Desktop/Summer2025")
PEH_data <- read.csv("death_tbl_wlinkedids_exportable.csv")
#View(PEH_data)

#view unique values in County column
table(PEH_data$l_place_rcounty_fct)

library(ggplot2)

ggplot(PEH_data, aes(x = l_place_rcounty_fct)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Morbidity Per County", x = "County", y = "Number of Peopled") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



