library(tidyverse)
library(ggthemes)

rm(list=ls())

incidence <- read.table("Data/Inzidenz/Krebsdaten.csv", sep = ";", dec = ",", header = TRUE, na.strings = c("x"))
mortality <- read.csv("Data/Mortalität/Krebsdaten.csv", sep = ";", dec = ",", header = TRUE, na.strings = c("x"))
incidence$X85. <-  as.numeric(sub(",", ".", incidence$X85., fixed = TRUE))
summary(incidence)


colnames(incidence)[1:3] <- c("year", "sex", "cancer")
incidence_tidy <- incidence %>% gather(key = "age_group", value = "incidence", -year, -sex, -cancer) %>% group_by(year, sex, cancer) %>% summarise(incidence_total = sum(incidence, na.rm = TRUE)) %>% ungroup()

colnames(mortality)[1:3] <- c("year", "sex", "cancer")
mortality_tidy <- mortality %>% gather(key = "age_group", value = "mortality", -year, -sex, -cancer) %>% group_by(year, sex, cancer) %>% summarise(mortality_total = sum(mortality, na.rm = TRUE)) %>% ungroup()

incidence_plot <- incidence_tidy %>% filter(year == 2016) %>% arrange(desc(incidence_total))
incidence_plot[which(incidence_plot$sex != "weiblich"),"incidence_total"] <- incidence_plot[which(incidence_plot$sex != "weiblich"),"incidence_total"] * (-1)
incidence_plot$incidence_total_abs <- abs(incidence_plot$incidence_total)

incidence_plot <- incidence_plot %>% filter(str_detect(incidence_plot$cancer, c("Prostata|Darm|Lunge|Brust|Melanom|Harnblase|Bauchspeichel|Non-Hodgkin-Lymphom|Magen|Leuk")))
incidence_plot$cancer

gg_incidence <- ggplot(data = incidence_plot, aes(fill = sex, x = incidence_total, y = reorder(cancer, incidence_total_abs))) +
  geom_bar(stat = "identity")+
  scale_x_continuous(name = "Annual number of new cancer cases\nper 100,000 individuals in Germany", breaks = c(-2000, -1000, 0, 1000, 2000), labels = c(2000, 1000, 0, 1000, 2000)) +
  scale_y_discrete(name = "", labels = rev(c("Prostate", "Colon", "Lung", "Breast", "Melanoma", "Bladder", "Pankreas", "Non-Hodgkin\nLymphoma", "Gastric", "Leukemia"))) +
  scale_fill_discrete(name = "", labels = c("Male", "Female")) +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), legend.position = "top")
gg_incidence
ggsave(gg_incidence, filename = "gg_incidence.png", device = "png", width = 12, height = 15, units = "cm")


mortality_plot <- mortality_tidy %>% filter(year == 2016) %>% arrange(desc(mortality_total))
mortality_plot[which(mortality_plot$sex != "weiblich"),"mortality_total"] <- mortality_plot[which(mortality_plot$sex != "weiblich"),"mortality_total"] * (-1)
mortality_plot$mortality_total_abs <- abs(mortality_plot$mortality_total)

mortality_plot <- mortality_plot %>% filter(str_detect(mortality_plot$cancer, c("Prostata|Darm|Lunge|Brust|Harnblase|Bauchspeichel|Non-Hodgkin-Lymphom|Magen|Leuk|Leber")))

gg_mortality <- ggplot(data = mortality_plot, aes(fill = sex, x = mortality_total, y = reorder(cancer, mortality_total_abs))) +
  geom_bar(stat = "identity")+
  scale_x_continuous(name = "Annual number of cancer deaths\nper 100,000 individuals in Germany", breaks = c(-1000, -500, 0, 500, 1000), labels = c(1000, 500, 0, 500, 1000)) +
  scale_y_discrete(name = "", labels = rev(c("Lung", "Colon", "Prostate", "Pankreas", "Breast", "Gastric", "Leukemia", "Liver", "Bladder", "Non-Hodgkin\nLymphoma"))) +
  scale_fill_discrete(name = "", labels = c("Male", "Female")) +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), legend.position = "top")
gg_mortality

ggsave(gg_mortality, filename = "gg_mortality.png", device = "png", width = 12, height = 15, units = "cm")

