# Données -----------------------------------------------------------------

library(ggplot2)
library("ggpubr")
library(GGally)
library(dplyr)
library(forcats)
library(reshape2)
library(scales)
library(patchwork)

theme_set(theme_bw() + 
            theme(plot.caption = element_text(hjust=0.5, size=rel(0.8)))+
            theme(axis.title = element_text(size = rel(0.9)))
          )

data           <- read.csv("./data/data.csv")
data_gares     <- read.csv("./data/data_gares.csv")
data_voyageurs <- read.csv("./data/data_voyageurs.csv")
data_ports     <- read.csv("./data/data_ports.csv")

# données complémentaires
annees         <- c("1923", "1925", "1927", "1929", "1931", "1933", "1936")
nb_km          <- c(1216, 1216, 1216, 1516, 1516, 1536, 2115)
annees_num     <- as.numeric(annees)
nb_gares       <- rename(aggregate(ID ~ Annee, aggregate(Quantite ~ ID + Annee, data, sum), length), NB = ID)

# Commerce (Source : ASI)
export_hevea   <- c(5700, 8000, 9600, 10300, 11200, 16800, 40800)
import_petrole <- c(53000, 57000, 71000, 93000, 75000, 71000, 68000)
prc_rec_petr   <- c(0.018599402, 0.025609209, 0.036111101, 0.053115533, 0.053125183, 0.078065691, 0.095101077)

# unités kilométriques
voy_km         <- c(139850849, 137538358, 254514427, 316546955, 287160193, 253269171, 389615273)
tonnes_km      <- c(35068993, 65325762, 67767217, 74795300, 72317236, 46053180, 68248862)

# subsets
data_fret      <- subset(data, Type2 != "Voyageurs")




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

merge_quantiles <- function (x, n, name) {
  if (x < n) {
    return(name)
  }
  else {
    return(x)
  }
}

arrondi <- function (nombre, base) {
  return(base * round(nombre / base))
}
