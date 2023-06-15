data_agri <- subset(data, Type2 == "Produits agricoles et bétail" & Mouvement == "Départ")
total_agri <- subset(Quantité ~ Année, data_agri, sum)
data_ctc <- subset(data_agri, ID==246 | ID == 249 | ID==281 | ID==293 | ID == 294 | ID == 295)

spd <- aggregate(Quantité ~ Annee, data_agri, sum)
spd2 <- aggregate(Quantité ~ Annee, subset(data_ctc, Type=="Caoutchouc" | Type == "Produits agricoles"), sum)
spd2$total <- spd$Quantité
spd2$prc <- spd2$Quantité * 100 / spd2$total

ggplot(spd2, aes(x = Annee, y = prc)) + geom_line() + geom_point() + theme_minimal() + ylab("Part du Caoutchouc dans le trafic agricole (%)")

spd2$qté_tot_ctc <- c(5700, 8000, 9600, 10300, 11200, 16800, 40800)
spd2$prc_ctc <- spd2$Quantité * 100 / spd2$qté_tot_ctc

ggplot(spd2, aes(x = Annee, y = prc_ctc, yend = 0)) + geom_line() + geom_point() + theme_minimal() + ylab("Part du Caoutchouc passé par le chemin de fer dans les export. totales (%)")

data_pétrole <- subset(data, Type2 == "Fossiles" & Mouvement == "Départ")
data_pétrole <- subset(data_pétrole, grepl("Pétrole|Essence", Type))
data_pétrole <- aggregate(Quantité ~ Annee, data_pétrole, sum)
data_pétrole$total <- c(53000, 57000, 71000, 93000, 75000, 71000, 68000)
data_pétrole$prc_total <- data_pétrole$Quantité * 100 / data_pétrole$total
ggplot(data_pétrole, aes(x = Annee, y = prc_total)) + geom_line() + geom_point() + theme_minimal()


# part des matériaux de construction dans le trafic en t et en valeur
data_matcons <- subset(data, Type2 == "Matériaux de construction" & Mouvement == "Départ" & Quantité != -1)

total_fret <- subset(data, Quantité != 1 & Mouvement == "Départ" & Type2 != "Voyageurs")
total_fret <- aggregate(Quantité ~ Annee, total_fret, sum)

data_matcons <- aggregate(Quantité ~ Annee, data_matcons, sum)
data_matcons$prc <- data_matcons$Quantité * 100 / total_fret$Quantité

ggplot(data_matcons, aes(x = Annee, y = prc)) + geom_line() + geom_point()

## petite fonction pour avoir le poucentage d'un type en fonction des années

evolution_pourcentage_type <- function (type) {
  data_type <- subset(data, Type2 == type & Mouvement == "Départ" & Quantité != -1)
  total_fret <- subset(data, Quantité != 1 & Mouvement == "Départ" & Type2 != "Voyageurs")
  total_fret <- aggregate(Quantité ~ Annee, total_fret, sum)
  
  data_type <- aggregate(Quantité ~ Annee, data_type, sum)
  data_type$prc <- data_type$Quantité * 100 / total_fret$Quantité
  return(data_type)
}

# on essaye de calculer la part des lignes dans ce calcul total
data_ligne_matcons <- aggregate(Quantité ~ Annee + ligne_clean, subset(data, Type2 == "Matériaux de construction" & Mouvement == "Départ" & Quantité != -1), sum)
data_matcons <- evolution_pourcentage_type("Matériaux de construction")
data_ligne_matcons <- full_join(data_ligne_matcons, data_matcons, by = "Annee")

data_ligne_matcons = data_ligne_matcons %>%
  mutate(prc_ligne = Quantité.x * 100 / Quantité.y)

ggplot(data_ligne_matcons, aes(x = Annee, y = prc_ligne, colour = ligne_clean)) + geom_line() + geom_point()


# 
data_fret <- subset(data, Quantité != 1 & Mouvement == "Départ" & Type2 != "Voyageurs")
data_top <- subset(data_fret, Type2 == "Matériaux de construction" | Type2 == "Bois et produits forestiers" | Type2 == "Produits agricoles et bétail" | Type2 == "Matériel de chemin de fer" | Type2 == "Services")
data_top <- aggregate(Quantité ~ Type2 + Annee, data_top, sum)

total_fret <- aggregate(Quantité ~ Annee, data_fret, sum)
data_top <- full_join(data_top, total_fret, by = "Annee")
data_top$prc <- data_top$Quantité.x * 100 / data_top$Quantité.y

ggplot(data_top, aes(x = Annee, y = prc, colour = Type2)) + geom_line() + geom_point()










