library(tidyverse)


incidence <- read.table("Data/Inzidenz/Krebsdaten.csv", sep = ";", dec = ",", header = TRUE, na.strings = c("x"))
mortality <- read.csv("Data/Mortalität/Krebsdaten.csv", sep = ";", dec = ",", header = TRUE, na.strings = c("x"))
incidence$X85. <-  as.numeric(sub(",", ".", incidence$X85., fixed = TRUE))
glimpse(incidence)


colnames(incidence)[1:3] <- c("year", "sex", "cancer")
incidence_tidy <- incidence %>% gather(key = "age_group", value = "incidence", -year, -sex, -cancer) %>% group_by(year, sex, cancer) %>% summarise(incidence_total = sum(incidence, na.rm = TRUE))

colnames(mortality)[1:3] <- c("year", "sex", "cancer")
mortality_tidy <- mortality %>% gather(key = "age_group", value = "mortality", -year, -sex, -cancer) %>% group_by(year, sex, cancer) %>% summarise(mortality_total = sum(mortality, na.rm = TRUE))


