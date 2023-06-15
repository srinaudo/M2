# je vais tester de calculer le % d'augmentation en tonnes de la circulation, d'abord sur toutes les lignes, après on voit



# 0. dataset
sub_data <- subset(data, Mouvement=="Départ" & Type2 != "Voyageurs" & Quantité != -1)

# 1. % des produits par rapport au total
tableau <- aggregate(Quantité ~ Type2 + Annee, sub_data, sum)

tableau$Total <- ave(tableau$Quantité, tableau$Annee, FUN = sum)

tableau$prc_total <- tableau$Quantité / tableau$Total * 100

visu_prc <- ggplot(tableau, aes(x = Annee, y = prc_total, colour = Type2)) + geom_line()
visu_prc


# 2. voir % de l'augmentation du trafic total
tbl2 <- aggregate(Quantité ~ Annee, sub_data, sum)

gr = tbl2 %>%
  arrange(Annee) %>%
  mutate(diff_qté   = Quantité - lag(Quantité),
         prc        = diff_qté* 100 /lag(Quantité) )

ggplot(gr, aes(x = Annee, y = prc)) + geom_line() + geom_point()

# 3. on essaye l'augmentation par catégorie sur le trafic total

tbl3 <- aggregate(Quantité ~ Annee + Type2, sub_data, sum)
gr2 = tbl3 %>%
  group_by(Type2) %>%
  arrange(Annee, .by_group = TRUE) %>%
  mutate(diff_qté =  Quantité - lag(Quantité),
         prc        = diff_qté* 100 /lag(Quantité) )

gr2_visu <- subset(gr2, Type2 != "Engrais" & Type2 != "Mines")
gr2_visu[is.na(gr2_visu)] <- 0

ggplot(gr2_visu, aes(x = Annee, y = prc, colour = Type2)) + geom_line() + geom_point()

# 3.2 une petite fonction pour faciliter l'utilisation

visu_rapide_evol_produit <- function (p1, p2, p3) {
  sb <- subset(data, Mouvement=="Départ")
  sb <- subset(sb, Type2 == p1 | Type2 == p2 | Type2 == p3)
  tbl <- aggregate(Quantité ~ Annee + Type2, sb, sum)
  gg = tbl %>%
    group_by(Type2) %>%
    arrange(Annee, .by_group = TRUE) %>%
    mutate(diff_qté =  Quantité - lag(Quantité),
           prc        = diff_qté* 100 /lag(Quantité) )
  gg[is.na(gg)] <- 0
  return(ggplot(gg, aes(x = Annee, y = prc, colour = Type2)) + geom_line() + geom_point())
}

# 4. On prend le diff de chaque année, et on montre c quoi le pourcentage de chaque categorie dedans. 

#il ns faut la table comme au point d'avant
tbl4 <- aggregate(Quantité ~ Annee + Type2, sub_data, sum)

#+ la quantité totale transportée
tbl5 <- aggregate(Quantité ~ Annee, sub_data, sum)

gr3 = tbl5 %>%
  arrange(Annee) %>%
  mutate(diff_total   = Quantité - lag(Quantité),
         prc_total    = diff_total* 100 /lag(Quantité) )

gr3 = gr3 %>% rename(Quantité_total = Quantité)

# join les deux df avec Anne comme ID
gr4 <- full_join(tbl4, gr3, by="Annee")
gr5 = gr4 %>%
  group_by(Type2) %>%
  arrange(Annee, .by_group = TRUE) %>%
  mutate(diff_qté       = Quantité - lag(Quantité),
         diff_marg      = diff_qté*100/diff_total,
         accroissement  = diff_qté * 100/lag(Quantité))

# ici pour avoir la moyenne des diff_marg par type:
gr5$diff_marg <- abs(gr5$diff_marg)
gr_mean <- aggregate(diff_marg ~ Type2, gr5, mean)
gr_mean <- gr_mean[order(-gr_mean$diff_marg),]
ggplot(gr_mean, aes(y = reorder(Type2, diff_marg), x = diff_marg)) + geom_col() + theme_minimal()

gr_mean2 <- aggregate(accroissement ~ Type2, gr5, mean)


# 5. voir l'accroissement moyen par ligne

tbl6 <- aggregate(Quantité ~ Annee + ligne_clean, tbl6, sum)
tbl7 <- aggregate(Quantité ~ Annee + ligne_clean + Type2, sub_data, sum)

#chiffres absolus absolu
ggplot(tbl6, aes(y = Quantité, x = Annee, colour = ligne_clean)) + geom_line() + geom_point()

gr6 = tbl6 %>%
  group_by(ligne_clean) %>%
  arrange(Annee, .by_group=TRUE) %>%
  mutate(diff_qté       = Quantité - lag(Quantité),
         accroissement  = diff_qté * 100/lag(Quantité)
  )

ggplot(subset(gr6, ligne_clean != "ben_thuy_dong_ha"), aes(x = Annee, y = accroissement, colour = ligne_clean)) + geom_line() + geom_point()

# j'ai abandonné u peu pour l'instant


  

## 6. j'essaie de calculer l'accroissement moyen pour chaque station. 
ndec = 6
tbl_dec <- aggregate(Quantité ~ ID, sub_data, sum)
tbl_dec$decile <- ntile(tbl_dec$Quantité, ndec)
tbl_dec$Quantité <- NULL

tbl8 <- aggregate(Quantité ~ ID + Annee, sub_data, sum)
gr8 = tbl8 %>%
  #group_by(ID, Annee) %>%
  arrange(ID) %>%
  mutate(
    diff = case_when(
      ID == lag(ID)  & lag(Quantité) != 0 ~ (Quantité - lag(Quantité))/lag(Quantité)*100,
      TRUE ~ 0
    )
  )

limit = 200

tbl9 <- full_join(gr8, tbl_dec, by = "ID")
#tbl9$diff <- sapply(tbl9$diff, function(x) if(x > limit) {x = limit} else {x})

tbl9 <- aggregate(Quantité ~ decile + Annee, tbl9, mean)

ggplot(tbl9, aes(x = Annee, y = Quantité, colour = as.character(decile))) + geom_line()




