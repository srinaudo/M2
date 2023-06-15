library(ggplot2)
library(forcats)

data           <- read.csv("./data/data.csv")
data_fret   <- subset(data, Type2 != "Voyageurs")

aggr_types <- function(df) {
  df2 <- df
  df2$Type2 <- replace(df2$Type2, df2$Type2 == "Mines" | df2$Type2 == "Divers" | df2$Type2 == "Engrais" | df2$Type2 == "Fossiles", "Autre")
  df2$Type2 <- replace(df2$Type2, df2$Type2 ==  "Produits manufactures" | df2$Type2 == "Produits alimentaires", "Produit de consommation")
  return (df2)
}

#je ne sais pas quel serait le nb idéal de stations à comparer
nb_stations_conserver = 15

station <- aggregate(Quantite ~ ID, data_fret, sum)
grandes_stations <- head(arrange(station, desc(Quantite)), nb_stations_conserver)$ID

p_sta <- data_fret[sapply(data_fret$ID, function(id) id %in% grandes_stations),]

#deux visualisations que j'ai faites pour tester:

# marchandises au départ et marchandises à l'arrivée, en valeur absolue
ggplot(p_sta, aes(x=Quantite/1000, forcats::fct_reorder(Gare, Quantite), fill = Mouvement)) + 
  geom_col() +
  xlab("trafic total (milliers de tonnes)")+
  ylab("principales gares")
  
  
p_sta_dec <- aggr_types(p_sta)


ggplot(p_sta_dec, aes(x=Quantite/1000, y=Gare, fill=Type2)) + 
  geom_col() +
  xlab ("Trafic total (milliers de tonnes)") +
  ylab("principales gares")
  
  
  
  
  
