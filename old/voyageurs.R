data_v <- read.csv("/home/silvio/Workspace/clean/data_voyageurs.csv")
library(ggplot2)


total_v <- subset(data_v, Ligne!="total" & Annee != 1936 & Ligne!="ben_thuy_dong_ha")

# total des voyageurs en milliers
ggplot(total_v, aes(x = Annee, y = Total_4/1000, color=Ligne)) + geom_line() + geom_point()

# parcours moyen 4e classe
ggplot(total_v, aes(x = Annee, y = KM_moyen_4, color=Ligne)) + geom_line() + geom_point()
