library(ggplot2)
library(GGally)

#pour faire une visu du deficit d'un produit, selon une ligne
solde <- aggregate(Quantité ~  Mouvement + Annee, subset(data, ligne_clean == "hanoi_ben_thuy" & Type2 == "Voyageurs"), sum)
solde2 <- aggregate(Quantité ~ Annee, solde, sum)
solde2$Quantité <- solde2$Quantité/2
solde2$Diff <- subset(solde, Mouvement=="Départ")$Quantité - subset(solde, Mouvement=="Arrivée")$Quantité
visu_abs <- ggplot(solde2, aes(x = Annee, y = Diff)) + geom_col() 
visu_abs
solde2$prc <- solde2$Diff * 100 / solde2$Quantité
visu_prc <- ggplot(solde2, aes(x = Annee, y = prc)) + geom_line() + geom_point()
visu_prc


## test avec plusieurs lignes
sol <- aggregate(Quantité ~ Mouvement + Annee + ligne_clean, subset(data, Type2 == "Voyageurs"), sum)
sol2 <- aggregate(Quantité ~ Annee + ligne_clean, sol, sum)
sol2$Quantité <- sol2$Quantité/2
sol2$Diff <- subset(sol, Mouvement=="Départ")$Quantité - subset(sol, Mouvement=="Arrivée")$Quantité
sol2$prc <- sol2$Diff * 100 / sol2$Quantité
visu_tout_prc <- ggplot(sol2, aes(x = Annee, y = prc, colour = ligne_clean)) + geom_line() + geom_point()
visu_tout_prc

visu_abs <- ggplot(sol2, aes(x = Annee, y= Diff, colour = ligne_clean)) + geom_line() + geom_point()
visu_abs


###" fonctions #####

nettoyage <- function(df) {
  new_df <- data.frame(Annee = c(), Mouvement = c(), Type = c(), Quantité = c())
  for (y in annees) {
    for (type in types) {
      dep <- subset(df, Annee==y & Mouvement=="Départ")
      arr <- subset(df, Annee==y & Mouvement=="Arrivée")
      
      x1 <- somme_type(dep, type)
      
      x2 <- somme_type(arr, type)
      new_df <- rbind(new_df, data.frame(Annee = c(y, y), Mouvement = c("Départ", "Arrivée"), Type = c(type, type), Quantité = c(x1, x2)))
    }
  }
  return(new_df)
}

solde_depart_arrivee <- function(df) {
  dep <- subset(df, Mouvement=="Départ")
  arr <- subset(df, Mouvement=="Arrivée")
  x <- dep$Quantité - arr$Quantité
  return(data.frame(Annee = dep$Annee, Type = dep$Type, Solde = x))
}