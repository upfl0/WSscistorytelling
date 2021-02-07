library(tidyverse)
library(treemapify)

IN <- read.csv("Data/23211-0004.csv", sep = ";", skip = 10, stringsAsFactors = TRUE, header = FALSE, na.strings = c("-", ""))#, colClasses = c("numeric", "numeric", "factor", "factor", rep("numeric", 23)))
IN <-IN[-(81:84),]
IN <- IN[,-1]

IN$parent <- c("total", 
               rep("Infektionen", 5), 
               rep("Neoplasien", 25), 
               "Andere", 
               "Endokrin", 
               "Endokrin", 
               rep("Psychiatrisch", 3), 
               "Neurologisch", 
               "Infektionen", 
               rep("Kardiovaskulär", 9), 
               rep("Respiratorisch", 5), 
               rep("Gastroenterologisch", 2), 
               rep("Leber", 2),
               "Andere",
               rep("Muskuloskelletal", 2),
               rep("Urogenital", 2),
               "Schwangerschaft",
               rep("Kongenital", 4),
               "Andere",
               "Schwangerschaft",
               "Andere",
               rep("External", 10)
               )


IN <- IN %>% gather(key = "delete", value = "deaths", -V2, -parent) %>% group_by(V2) %>% summarise(deaths_total = sum(deaths, na.rm = TRUE), parent = first(parent)) %>% ungroup()

IN <- IN %>% filter(!str_detect(V2, "Insgesamt|Neubildungen|Krankheiten des Kreislaufsystems|Krankheiten des Atmungssystems|Fehlbildungen,Deformitäten|Äußere Ursachen von Morbidität und Mortalität|BN d. Larynx, d. Trachea, d. Bronchien u. d. Lunge|BN der Genital- und Harnorgane|BN der Leber, der Gallenwege und des Pankreas|Krankheiten der Niere|Alkoh.Leberkh."))

sum(IN$deaths_total)

treemap <- ggplot(data = IN, aes(area = deaths_total, fill = parent, subgroup = parent, label = V2)) + 
  geom_treemap(size = 1) +
  #geom_treemap_subgroup_border(colour = "black", alpha = 0.5, size = 2)+
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  geom_treemap_subgroup_text(place = "bottomleft", grow = T, alpha = 0.5, colour =
                               "black", min.size = 0, padding.x = grid::unit(5,"mm"), padding.y = grid::unit(5,"mm")) +
  guides(fill = FALSE)
treemap
ggsave(treemap, filename = "treemap.png", device = "png", width = 20, height = 12, units = "cm")
