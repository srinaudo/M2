

sel <-
  subset(data,
         Annee == 1936 &
           Mouvement == "Départ" & Type2 == "Bois et produits forestiers")
sel <- aggregate(Quantité ~ ID, sel, sum)

write.csv(
  sel,
  "/home/silvio/fac/memoire/GIS/csv_requetes/1927_depart_bois.csv",
  row.names = FALSE
)


for (y in annees) {
  heatmap_produit(data, y, "Départ", "Bois et produits forestiers", "dep_bois")
}

a = "1933"
m = "Arrivée"
t = "Bois et produits forestiers"
dd <-
  subset(data, Annee == a & Mouvement == m &
           Type2 == t & Quantité != "-1")
dd2 <- aggregate(Quantité ~ ID, dd, sum)
write.csv(dd2,
          "/home/silvio/fac/memoire/GIS/csv_requetes/1933_arr_bois.csv",
          row.names = FALSE)



heatmap_produit <- function(df, a, m, t, nom) {
  d <- subset(df, Annee == a &
                Mouvem ent == m & Type2 == t & Quantité != "-1")
  d2 <- aggregate(Quantité ~ ID, d, sum)
  path = paste("/home/silvio/fac/memoire/GIS/csv_requetes/",
               a,
               "_",
               nom,
               ".csv",
               sep = "")
  write.csv(d2, path, row.names = FALSE)
}

test <-
  read.csv(
    "/home/silvio/fac/memoire/GIS/csv_requetes/1936_Départ_Bois et produits forestiers.csv"
  )
