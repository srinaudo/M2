# 1. Première étude sommaire

# a. Présentation de la BDD -----------------------------------------------

# nombre de stations par kilomètre exploité par année
tmp <- aggregate(Quantite ~ ID + Annee, data, sum)
nb_stations <- aggregate(ID ~ Annee, tmp, length)
nb_stations$km_exp <- nb_km
nb_stations$station_par_km_exp <- nb_stations$ID / nb_stations$km_exp

ggplot(nb_stations, aes(x = Annee, y = station_par_km_exp)) + geom_line() + geom_point()


# b. Présentation trafic --------------------------------------------------

# évolution UK par années
unites_km            <- data.frame(annees_num, voy_km, tonnes_km)
unites_km$mvoy_km    <- unites_km$voy_km/1000000
unites_km$mtonnes_km <- unites_km$tonnes_km/1000000
unites_km$munites_km <- unites_km$mvoy_km + unites_km$mtonnes_km

ggplot(unites_km, aes(x=annees_num, y=munites_km)) + geom_line() + geom_point() + ylab("Millions d'unitées kilométriques")

# trafic  fret cumulé total par ligne
fret_ttl_cml_ligne      <- aggregate(Quantite ~ ligne_clean, data_fret, sum)
fret_ttl_cml_ligne$part <- fret_ttl_cml_ligne$Quantite * 100 / sum(fret_ttl_cml_ligne$Quantite)

# trafic fret par an par ligne
fret_ligne <- subset(data_fret, ligne_clean != "tourane_nha_trang" & ligne_clean != "ben_thuy_dong_ha")
fret_ligne  <- aggregate(Quantite ~ ligne_clean + Annee, fret_ligne, sum) 

ggplot(fret_ligne, aes(x = Annee, y = Quantite/1000, color = ligne_clean)) + geom_line() + geom_point() + ylab("tonnes (millers)")

# trafic voy cumulé par ligne
voy__ttl_cml_ligne <-      aggregate(Quantite ~ ligne_clean, subset(data, Type2 == "Voyageurs"), sum)
voy__ttl_cml_ligne$part <- voy__ttl_cml_ligne$Quantite * 100 / sum(voy__ttl_cml_ligne$Quantite)

# trafic voy par an par ligne
voy_ligne <- subset(data, ligne_clean != "tourane_nha_trang" & ligne_clean != "ben_thuy_dong_ha" & Type2 == "Voyageurs")
voy_ligne <- aggregate(Quantite ~ ligne_clean + Annee, voy_ligne, sum)

ggplot(voy_ligne, aes(x = Annee, y = Quantite/10000, color = ligne_clean)) + geom_line() + geom_point() + ylab("Voyageurs (Dizaine de milliers)")


# tonnes cumulées de marchandises
produits_cml <- aggregate(Quantite ~ Type2, data_fret, sum)

ggplot(produits_cml, aes(y = Type2, x = Quantite/10000)) + geom_col()


# repartition du type de trafic marchandise par année
prod <- subset(data_fret, Mouvement = "Depart")
prod$Type2 <- replace(prod$Type2, prod$Type2 == "Mines", "Autre")
prod$Type2 <- replace(prod$Type2, prod$Type2 == "Divers", "Autre")
prod$Type2 <- replace(prod$Type2, prod$Type2 == "Produits manufactures", "Produit de consommation")
prod$Type2 <- replace(prod$Type2, prod$Type2 == "Produits alimentaires", "Produit de consommation")

prod$Type2 <- replace(prod$Type2, prod$Type2 == "Materiel de chemin de fer", "Autre")
prod$Type2 <- replace(prod$Type2, prod$Type2 == "Engrais", "Autre")
prod$Type2 <- replace(prod$Type2, prod$Type2 == "Fossiles", "Autre")

             
ggplot(aggregate(Quantite ~ Annee + Type2, prod, sum), aes(y = Quantite/10000, x = as.character(Annee), fill=Type2)) + 
  geom_col() + 
  ylab("Tonnes transportées (dizaines de milliers") + 
  xlab("Années")

# transport de produits agricole et bétail par an
data_agri <- subset(aggregate(Quantite ~ Annee + Type2, prod, sum), Type2 == "Produits agricoles et betail")
ggplot(data_agri, aes(y = Quantite/1000, x = Annee)) + geom_line() + geom_point() + ylab("tonnes (milliers)")
#env 12%, passe à 18% en 32


#moyenne de tonnes transportées par an par ligne
prod2 <- aggregate(Quantite ~ Annee + ligne_clean, prod, sum)
prod2 <- aggregate(Quantite ~ ligne_clean, prod2, mean)
prod2 <- arrange(prod2, prod2$Quantite)
ggplot(prod2, aes(y = forcats::fct_reorder(ligne_clean, Quantite), x = Quantite/1000)) + geom_col() + ylab("Ligne") + xlab("Trafic marchandise moyen annuel (milliers de tonnes)")


#heatmap des départs
hm_dep <- subset(prod, Annee == 1936)
hm_dep <- aggregate(Quantite ~ ID, hm_dep, sum)
export_csv(hm_dep, "heatmap_depart_1936")

#heatmap des arrrivées
hm_arr <- subset(data_fret, Mouvement == "Arrivee" & Annee == 1936)
hm_arr <- aggregate(Quantite ~ ID, hm_arr, sum)
export_csv(hm_arr, "heatmap_arrivee_1936")








# c. présentation du réseau -----------------------------------------------

# principales stations
station <- aggregate(Quantite ~ ID, data_fret, sum)
station <- full_join(station, data_gares, by="ID")

grandes_stations <- head(arrange(station, desc(Quantite)), 15)$ID
p_sta <- data_fret[sapply(data_fret$ID, function(id) id %in% grandes_stations),]
p_sta <- left_join(p_sta, data_gares, by="ID")

#grandes gares départ vs arrivée
ggplot(p_sta, aes(x=Quantite/1000, forcats::fct_reorder(Gare, Quantite), fill = Mouvement)) + geom_bar(stat="sum")

p_sta_data <- aggregate(Quantite ~ Gare + Type2, p_sta, sum)
ggplot(p_sta_data, aes(x = Quantite/100000, y = forcats::fct_reorder(Gare, Quantite)), fill = Type2) + geom_bar(stat="sum") + xlab("trafic total (centaines de milliers de tonnes") + ylab("15 principales stations")

tmp2 <- aggr_types(p_sta_data)
tmp2 <- group_by(tmp2, Gare)
tmp2 <- arrange(tmp2, Quantite, .by_group=TRUE)

ggplot(tmp2, aes(x=Quantite/1000000, y=Gare, fill=Type2)) + geom_col()
 

# croissance moyenne
# sous-pop : prendre les stations qui existent déjà en 1923 et les comparer à leur état en 1936
data_1923 <- subset(data_fret, Annee == 1923)
data_1936 <- subset(data_fret, Annee ==  1936)

  #ne garder que les ID de stations qui existent dans les deux années
data_1936 <- data_1936[sapply(data_1936$ID, function(id) id %in% data_1923$ID),]
data_1923 <- data_1923[sapply(data_1923$ID, function(id) id %in% data_1936$ID),]

evol <- aggregate(Quantite ~ ID, data_1923, sum)
tmp <- aggregate(Quantite ~ ID, data_1936, sum)

evol$Quantite_1936 <- tmp$Quantite

evol <- filter(evol, Quantite > 0, Quantite_1936 > 0)

#evolution moyenne = -19%, soit - 1,42% par an, pour toutes les stations
evol_moyenne = (sum(evol$Quantite_1936) * 100 / sum(evol$Quantite) - 100)/13


# On regarde l'évolution moyenne des plus grandes stations, qui stagnent aussi. La moyenne est encore plus basse.
# En fait, la plupart perdent beaucoup, et certaines gagnent un peu de trafic. 

evol_psta         <- evol[sapply(evol$ID, function(id) id %in% grandes_stations),]
evol_psta$evol    <- evol_psta$Quantite_1936 * 100 /evol_psta$Quantite - 100
evol_moyenne_psta = (sum(evol_psta$Quantite_1936) * 100 / sum(evol_psta$Quantite) - 100)/13

# Si on fait une répartition du trafic en fonction du Type2, on se rend compte que les matériaux de constructions et matériel de chemin de
# fer constituent la majeure partie de la baisse. 

data_evol <- subset(data_fret, (Annee == 1923 | Annee == 1936) & Mouvement=="Depart")
data_evol <- aggr_types(data_evol)

# total du corpus de stations : 
ggplot(aggregate(Quantite ~ Type2 + Annee, data_evol, sum), 
       aes(x = Quantite/1000, y = forcats::fct_reorder(Type2,Quantite), fill = as.character(Annee))) + 
  geom_col() 

# Stations principales
data_evol_psta <- data_evol[sapply(data_evol$ID, function(id) id %in% evol_psta$ID),]

ggplot(aggregate(Quantite ~ Type2 + Annee, data_evol_psta, sum), 
       aes(x = Quantite/1000, y = forcats::fct_reorder(Type2,Quantite), fill = as.character(Annee))) + 
  geom_col() 

# On tente donc de calculre cette évolution sans prendre en compte 
# ces élements plus variables, liés à la construction du chemin de fer

data_stable <- subset(data_evol, Type2 != "Materiaux de construction" & Type2 != "Materiel de chemin de fer")

# corpus entier : 2,6% d'augmentation annuelle, soit 34% !!!
total1923                = aggregate(Quantite ~ Annee, data_stable, sum)[1,2]
total1936                = aggregate(Quantite ~ Annee, data_stable, sum)[2,2]
evol_moyenne_hors_constr = (total1936 * 100 / total1923 - 100)/13

# et pour les stations principales: 0.01%
data_stable_psta               <- data_stable[sapply(data_stable$ID, function(id) id %in% evol_psta$ID),]
total1923                     = aggregate(Quantite ~ Annee, data_stable_psta, sum)[1,2]
total1936                     = aggregate(Quantite ~ Annee, data_stable_psta, sum)[2,2]
evol_moyenne_hors_constr_psta = (total1936 * 100 / total1923 - 100)/13











# 2. Intégration économique


# a. corpus ---------------------------------------------------------------

# Definition d'une sous-population de centres urbains :
# Ce sont les chefs-lieux de provinces que traverse et atteint 
# le chemin de fer (Ha Tinh par exemple n'y figure pas)
#on rajoute ben thuy (id#76) qui est l eport de Vinh

urbains = c(6, 26, 33, 44, 45, 49, 57, 74, 76, 122, 124, 132, 147, 148, 179, 216, 246, 284, 300, 306, 320)

data_urbain <- subset(data, ID %in% urbains)
data_fret_urbain <- subset(data_urbain, Type2 != "Voyageurs")
data_voy_urbain  <- subset(data_urbain, Type2 == "Voyageurs")

data_hors_urbain <- subset(data, !(ID %in% urbains))

# premier test : % du trafic 
part_trafic_urbain             <- aggregate(Quantite ~ Annee, data_fret, sum)
part_trafic_urbain             <- rename(part_trafic_urbain, Fret_total = Quantite)
part_trafic_urbain$Voy_total   <- aggregate(Quantite ~ Annee, subset(data, Type2 == "Voyageurs"), sum)$Quantite

part_trafic_urbain$Fret_urbain <- aggregate(Quantite ~ Annee, data_fret_urbain, sum)$Quantite
part_trafic_urbain$Voy_urbain  <- aggregate(Quantite ~ Annee, subset(data_urbain, Type2 == "Voyageurs"), sum)$Quantite

part_trafic_urbain$part_fret   <- part_trafic_urbain$Fret_urbain * 100 / part_trafic_urbain$Fret_total
part_trafic_urbain$part_voy    <- part_trafic_urbain$Voy_urbain * 100 / part_trafic_urbain$Voy_total

part_trafic_urbain$Fret_total_depart   <- aggregate(Quantite ~ Annee, subset(data_fret, Mouvement=="Depart"), sum)$Quantite
part_trafic_urbain$Fret_total_arrivee  <- aggregate(Quantite ~ Annee, subset(data_fret, Mouvement=="Arrivee"), sum)$Quantite
part_trafic_urbain$Fret_urbain_depart  <- aggregate(Quantite ~ Annee, subset(data_fret_urbain, Mouvement=="Depart"), sum)$Quantite
part_trafic_urbain$Fret_urbain_arrivee <- aggregate(Quantite ~ Annee, subset(data_fret_urbain, Mouvement=="Arrivee"), sum)$Quantite

part_trafic_urbain$part_fret_depart    <- part_trafic_urbain$Fret_urbain_depart * 100 / part_trafic_urbain$Fret_total_depart
part_trafic_urbain$part_fret_arrivee   <- part_trafic_urbain$Fret_urbain_arrivee * 100 / part_trafic_urbain$Fret_total_arrivee

part_trafic_urbain$Voy_total_depart    <- aggregate(Quantite ~ Annee, subset(data, Mouvement=="Depart" & Type2 == "Voyageurs"), sum)$Quantite
part_trafic_urbain$Voy_total_arrivee   <- aggregate(Quantite ~ Annee, subset(data, Mouvement=="Arrivee" & Type2 == "Voyageurs"), sum)$Quantite
part_trafic_urbain$Voy_urbain_depart   <- aggregate(Quantite ~ Annee, subset(data_urbain, Mouvement=="Depart" & Type2 == "Voyageurs"), sum)$Quantite
part_trafic_urbain$Voy_urbain_arrivee  <- aggregate(Quantite ~ Annee, subset(data_urbain, Mouvement=="Arrivee" & Type2 == "Voyageurs"), sum)$Quantite

part_trafic_urbain$part_voy_depart     <- part_trafic_urbain$Voy_urbain_depart * 100 / part_trafic_urbain$Voy_total_depart
part_trafic_urbain$part_voy_arrivee    <- part_trafic_urbain$Voy_urbain_arrivee * 100 / part_trafic_urbain$Voy_total_arrivee

part_trafic_urbain$diff_voy            <- part_trafic_urbain$Voy_urbain_arrivee - part_trafic_urbain$Voy_urbain_depart
part_trafic_urbain$part_diff_voy       <- part_trafic_urbain$diff_voy * 100 / part_trafic_urbain$Voy_total
part_trafic_urbain$diff_voy_comp       <- part_trafic_urbain$Voy_urbain_arrivee * 100 / part_trafic_urbain$Voy_urbain_depart - 100


# En courbes
ggplot(part_trafic_urbain, aes(x = Annee)) +
  geom_line(aes(y = part_voy, col="red")) +
  geom_line(aes(y = part_fret)) +
  scale_y_continuous(limits=c(0,60))

# en histogrammes
tmp <- melt(part_trafic_urbain[,c('Annee','part_fret','part_voy', 'part_fret_depart', 'part_fret_arrivee', 'part_voy_depart', 'part_voy_arrivee')],id.vars = 1)
ggplot(tmp, aes(x = as.character(Annee), y= value)) + 
  geom_col(aes(fill = variable), position = "dodge")

# comme la variabilité est assez faible selon les années, un diagramme avec valeur moyenne
ggplot(aggregate(value ~ variable, tmp, mean), aes(x = variable, y= value)) + geom_col()

# on remarque un creusement dans la différence arrivé départ voyageurs
# en valeur absolue
ggplot(part_trafic_urbain, aes(x = Annee, y = diff_voy/1000)) + geom_line() + ylab("solde de voyageurs dans les centres urbains (milliers)") + geom_point()

# en poucentage
ggplot(part_trafic_urbain, aes(x = Annee, y = part_diff_voy)) + geom_line() + ylab("solde de voyageurs dans les centres urbains (en % du total de voyageurs)") + geom_point()
ggplot(part_trafic_urbain, aes(x = Annee, y = diff_voy_comp)) + geom_line() + ylab("Quantum de voyageurs arrivées vs partis (en %)") + geom_point()


# pour toutes les stations
diff_voy_urbain <- aggregate(Quantite ~ ID + Annee, subset(data_voy_urbain, Mouvement == "Depart"), sum)
diff_voy_urbain <- rename(diff_voy_urbain, VDepart = Quantite)
diff_voy_urbain$VArrivee <- aggregate(Quantite ~ ID + Annee, subset(data_voy_urbain, Mouvement == "Arrivee"), sum)$Quantite
diff_voy_urbain$diff <- diff_voy_urbain$VArrivee - diff_voy_urbain$VDepart

diff_voy_urbain <- filter(diff_voy_urbain, diff!=0)

diff_voy_urbain$diff_part <- diff_voy_urbain$VArrivee * 100 / diff_voy_urbain$VDepart - 100

# on voit le cas extrêmes : saigon qui reçoit les voyageurs du saigon mytho, 
# les marchés (nam dinh, tourane) peu ou pas ouvert aux voyageurs
# Ben Thuy qui est un port de débarquement. 
diff_voy_urbain <- left_join(diff_voy_urbain, data_gares, by= "ID")
ggplot(subset(diff_voy_urbain, Annee == 1936), aes(y = forcats::fct_reorder(NOM, diff_part), x = diff_part)) + geom_col()



#si on calcule la moyenne pour 1936 hors ces cas extrêmes on obtient une moyenne de 6% et médiane 7.8%
tmp <- subset(diff_voy_urbain, ID != 306 & ID != 74 & ID != 45 & Annee == 1936)
moy_qt_arr_dep_voy_urbain = mean(tmp$diff_part)

# pour le reste des stations, la moyenne est de -4% et la mediane de -9%
voy_hors_urbain <- aggregate(Quantite ~ ID, subset(data_hors_urbain, Annee == 1936 & Mouvement == "Arrivee" & Type2 == "Voyageurs"), sum)
voy_hors_urbain <- rename(voy_hors_urbain, arrivee=Quantite)
voy_hors_urbain$depart <- aggregate(Quantite ~ ID, subset(data_hors_urbain, Annee == 1936 & Mouvement == "Depart" & Type2 == "Voyageurs"), sum)$Quantite
voy_hors_urbain <- filter(voy_hors_urbain, arrivee!=0, depart!=0)

voy_hors_urbain$quantum <- voy_hors_urbain$arrivee * 100 / voy_hors_urbain$depart - 100

# % des importation des diverses marchandises par annee

# mauvais voie mais code intéressant
part_fret_urbain <- aggr_types(subset(data_fret_urbain, Mouvement == "Depart"))
part_fret_urbain <- aggregate(Quantite ~ ID + Type2, part_fret_urbain, sum)
part_fret_urbain <- dcast(part_fret_urbain, formula = ID~Type2, fun.aggregate = sum, value.var = "Quantite")

# deuxieme tentative
part_fret_urbain2 <- aggr_types(subset(data_fret_urbain, Mouvement == "Arrivee"))
part_fret_urbain2 <- aggregate(Quantite ~ Annee + Type2, part_fret_urbain2, sum)                   
part_fret_urbain2$Total <- aggregate(Quantite ~ Annee + Type2, aggr_types(subset(data_fret, Mouvement == "Arrivee")), sum)$Quantite     


part_fret_urbain2$depart <- aggregate(Quantite ~ Annee + Type2, aggr_types(subset(data_fret_urbain, Mouvement == "Depart")), sum)$Quantite
part_fret_urbain2$depart_total <- aggregate(Quantite ~ Annee + Type2, aggr_types(subset(data_fret, Mouvement == "Depart")), sum)$Quantite   


part_fret_urbain2$part <- part_fret_urbain2$Quantite * 100 / part_fret_urbain2$Total
part_fret_urbain2$part_depart <- part_fret_urbain2$depart * 100 / part_fret_urbain2$depart_total

part_fret_urbain2 <- filter(part_fret_urbain2, Type2 != "Materiel de chemin de fer", Type2 != "Autre")

ggplot(part_fret_urbain2, aes(x = as.character(Annee), y = part, fill = Type2)) + geom_col(position="dodge")
ggplot(part_fret_urbain2, aes(x = as.character(Annee), y = part_depart, fill = Type2)) + geom_col(position="dodge")

# quelque chose d'assez saisissant c'est que les centres urbains sont à la fois les 
# principaux exportateurs de produits agricoles, et importateurs également (60 vs 75%)
# est-ce que le gros des échanges ne se fait pas _entre_ centres urbains ?

data_agri_urbain <- subset(data_fret_urbain, Type2 == "Produits agricoles et betail")
data_agri_urbain <- aggregate(Quantite ~ Annee + Type + Mouvement, data_agri_urbain, sum)

# On voit qu'effectivement les centres urbains sont aussi des endroits très exportateurs de p. Agri 
# et que l'année 1931 marque même un moment d'exportation globale; 
ggplot(data_agri_urbain, aes(y = Type, x = Quantite/1000, fill = Mouvement)) + geom_col(position="dodge") + facet_wrap(~ Annee)

ggplot(aggregate(Quantite ~ Annee, subset(data_agri_urbain, Mouvement == "Arrivee"), sum), aes(x = Annee, y = Quantite/1000)) + 
  geom_line() +
  ylab("tonnes de produits agricoles importés dans les principaux centres urbains (milliers)")

contribution_urbain <- aggregate(Quantite ~ Annee + Mouvement, data_agri_urbain, sum)
contribution_urbain <- dcast(contribution_urbain, formula = Annee~Mouvement, fun.aggregate = sum, value.var = "Quantite")
contribution_urbain$part <- contribution_urbain$Depart * 100 / contribution_urbain$Arrivee

# en effet pratiquement toujours au dessus de 50% sauf en 1936. 
ggplot(contribution_urbain, aes(x = Annee, y = part)) + 
  geom_line() +
  scale_y_continuous(limits=c(0,120))

contrib_urbain_detail <- subset(data_fret_urbain, Type2 == "Produits agricoles et betail")
contrib_urbain_detail <- aggregate(Quantite ~ Gare + Mouvement, contrib_urbain_detail, sum)
ggplot(contrib_urbain_detail, aes(y = forcats::fct_reorder(Gare, Quantite), x = Quantite/10000, fill = Mouvement)) + 
  geom_col(position = "dodge") +
  ylab("centres urbains") +
  xlab("trafic total de produits agricoles (dizaine de milliers de tonnes")

contrib_urbain_detail2 <- subset(data_fret_urbain, Type2 == "Produits agricoles et betail")
contrib_urbain_detail2 <- aggregate(Quantite ~ Gare + Mouvement + Annee, contrib_urbain_detail2, sum)

contrib_urbain_detail2 <- aggregate(Quantite ~ Gare + Annee, subset(contrib_urbain_detail2, Mouvement == "Depart"), sum)
contrib_urbain_detail2 <- rename(contrib_urbain_detail2, Depart_local = Quantite)

contrib_urbain_detail2 <- full_join(contrib_urbain_detail2, contribution_urbain, by="Annee")
contrib_urbain_detail2$part_local <- contrib_urbain_detail2$Depart_local * 100 / contrib_urbain_detail2$Depart

n = 3
id_haute_contrib <- head(
  arrange(
    aggregate(part_local ~ Gare, contrib_urbain_detail2, mean),
    desc(part_local)), 
  n)$Gare

contrib_urbain_detail2 <- filter(contrib_urbain_detail2, Gare %in% id_haute_contrib)

ggplot(contrib_urbain_detail2, aes(y = as.character(Annee), x = part_local, fill = Gare)) + geom_col(position="dodge")












