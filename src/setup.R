# Données -----------------------------------------------------------------

library(ggplot2)
library(GGally)
library(dplyr)
library(forcats)
library(reshape2)

data           <- read.csv("./data/data.csv")
data_gares     <- read.csv("./data/data_gares.csv")
data_voyageurs <- read.csv("./data/data_voyageurs.csv")

# données complémentaires
annees     <- c("1923", "1925", "1927", "1929", "1931", "1933", "1936")
nb_km      <- c(1216, 1216, 1216, 1516, 1516, 1536, 2115)
annees_num <- as.numeric(annees)

# unités kilométriques
voy_km    <- c(139850849, 137538358, 254514427, 316546955, 287160193, 253269171, 389615273)
tonnes_km <- c(35068993, 65325762, 67767217, 74795300, 72317236, 46053180, 68248862)

# subsets
data_fret   <- subset(data, Type2 != "Voyageurs")




# Fonctions ---------------------------------------------------------------


export_csv <- function(df, name) {
  # fonction qui attend un df associant des ID de gares à des valeurs
  
  #association des donnees de data gare 
  df <- left_join(df, data_gares, by = "ID")
  df[is.na(df)] <- 0
  #export
  write.csv(df, 
            paste("./csv/",name,".csv", sep=""),
            row.names = FALSE
  )
}


aggr_types <- function(df) {
  df2 <- df
  df2$Type2 <- replace(df2$Type2, df2$Type2 == "Mines" | df2$Type2 == "Divers" | df2$Type2 == "Engrais" | df2$Type2 == "Fossiles", "Autre")
  df2$Type2 <- replace(df2$Type2, df2$Type2 ==  "Produits manufactures" | df2$Type2 == "Produits alimentaires", "Produit de consommation")
  return (df2)
}



# Old ---------------------------------------------------------------------

#types = c("Bois et produits forestiers", "Divers", "Engrais", "Fossiles", "Matériaux de construction", "Matériel de chemin de fer", "Mines", "Produits agricoles et bétail", "Produits alimentaires", "Produits forestiers", "Produits manufacturés", "Services")

#somme_type <- function(df, type) {
#sum(subset(df, Type2==type)$Quantité)
#}

#visu_deficit_ligne <- function (df, ligne) {
#  df2 <- nettoyage(subset(df, ligne_clean==ligne & Type2 != "Voyageurs"))
#  df2 <- solde_depart_arrivee(df2)
#  v <- ggplot(data = df2, aes(x = Type, y = Solde)) + geom_col() + theme_minimal() + theme(axis.text.x=element_text(color="grey21", angle=45, hjust=1)) + facet_wrap(~ d$Annee)
#  return(v)
#}


