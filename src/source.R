# 1. Première étude sommaire

# a. Présentation de la BDD -----------------------------------------------

# nombre de stations par kilomètre exploité par année


A1 <- ggplot(nb_gares, aes(x = Annee, y = NB)) + 
  geom_line() + 
  geom_point() + 
  labs(
    x = "",
    y = "",
    caption = "Nombre de gares en exploitation")


A2 <- ggplot(nb_gares, aes(x = Annee, y = NB/nb_km)) + 
  geom_line() + 
  geom_point() + 
  labs(
    x = "",
    y = "",
    caption = "Nombres de gares par km exploité")

A = A1 + A2
A
ggsave(file = "./plots/A_Presentation.svg", plot=A)




# b. Présentation trafic --------------------------------------------------

# évolution UK par années
unites_km            <- data.frame(annees_num, voy_km, tonnes_km)
unites_km$mvoy_km    <- unites_km$voy_km/1000000
unites_km$mtonnes_km <- unites_km$tonnes_km/1000000
unites_km$munites_km <- unites_km$mvoy_km + unites_km$mtonnes_km

B1 <-ggplot(unites_km, aes(x=annees_num, y=munites_km)) + 
  geom_line() + 
  geom_point() + 
  labs(
    y = "Unitées kilométriques (Millions)",
    x = "",
    caption = "Volume de trafic annuel")


# Trafic de fret par an et par ligne

fret_ligne <- subset(data_fret, ligne_clean != "tourane_nha_trang" & ligne_clean != "ben_thuy_dong_ha")
fret_ligne <- aggregate(Quantite ~ ligne_clean + Annee, fret_ligne, sum) 

B2 <- ggplot(fret_ligne, aes(x = Annee, y = Quantite/1000, color = ligne_clean)) + 
  geom_line() + 
  geom_point() + 
  theme(legend.title = element_text(size = rel(0.7)),
        legend.text  = element_text(size = rel(0.7))) +
  labs(
    x = "",
    y = "Tonnes (Milliers)",
    colour = "Lignes",
    caption = "Trafic marchandises par ligne") +
  scale_colour_manual(labels = c("Hue - Da Nang", "Ha Noi - Vinh", "Ha Noi - Lang Son", "Sai Gon - Nha trang", "Sai Gon - My Tho"), values = hue_pal()(5))


# Trafic voyageurs par an et par ligne

voy_ligne <- subset(data, ligne_clean != "tourane_nha_trang" & ligne_clean != "ben_thuy_dong_ha" & Type2 == "Voyageurs")
voy_ligne <- aggregate(Quantite ~ ligne_clean + Annee, voy_ligne, sum)

B3 <- ggplot(voy_ligne, aes(x = Annee, y = Quantite/1000000, color = ligne_clean)) + 
  geom_line() + 
  geom_point() + 
  theme(legend.title = element_text(size = rel(0.7)),
        legend.text  = element_text(size = rel(0.7))) +
  labs(
    x = "",
    y = "Voyageurs (Millions)",
    colour = "Lignes",
    caption = "Trafic voyageurs par ligne") +
  scale_colour_manual(labels = c("Hue - Da Nang", "Ha Noi - Vinh", "Ha Noi - Lang Son", "Sai Gon - Nha trang", "Sai Gon - My Tho"), values = hue_pal()(5))

# transport de produits agricole et bétail par an : de 12% à 18% en 1932.
data_agri <- subset(aggregate(Quantite ~ Annee + Type2 + Mouvement, prod, sum), Type2 == "Produits agricoles et betail" & Mouvement == "Depart")

B4 <- ggplot(data_agri, aes(y = Quantite/1000, x = Annee)) + 
  geom_line() + 
  geom_point() + 
  labs(x ="",
       y = "Milliers de tonnes",
       caption = "Expédition de produits agricoles")



# tonnes cumulées de marchandises totales
produits_cml <- aggregate(Quantite ~ Type2, subset(data_fret, Mouvement == "Depart"), sum)

B5 <- ggplot(produits_cml, aes(y = forcats::fct_reorder(Type2, Quantite), x = Quantite/1000)) + 
  geom_col() +
  labs(
    x = "Milliers de tonnes",
    y = "",
    caption = "Trafic marchandises cumulé (1923 - 1936)"
  )

# repartition du type de trafic marchandise par année
prod <- subset(data_fret, Mouvement = "Depart")
prod <- aggr_types(prod)

             
B6 <- ggplot(aggregate(Quantite ~ Annee + Type2, prod, sum), aes(x = Quantite/1000, y = as.character(Annee), fill=forcats::fct_reorder(Type2, Quantite))) + 
  geom_col() + 
  labs(
    y = "",
    x = "Milliers de tonnes",
    caption = "Repartition annuelle du trafic marchandises",
    fill = "Catégories"
  )

BPanel <- ggarrange(B2, B3, B1, B4, common.legend = TRUE, legend = "top", labels=c("A", "B", "C", "D", ncol = 2, nrow = 2))
BPanel
ggexport(BPanel, filename = "./plots/B_presentation1.svg")

BPanel2 <- ggarrange(B5, B6, nrow = 2, labels = c("A", "B"))
BPanel2
ggexport(BPanel2, filename = "./plots/B_presentation2.svg")


# Carte de chaleur des départs

hm_dep <- subset(prod, Annee == 1936)
hm_dep <- aggregate(Quantite ~ ID, hm_dep, sum)
export_csv(hm_dep, "heatmap_depart_1936")

# Carte de chaleur des arrivées

hm_arr <- subset(data_fret, Mouvement == "Arrivee" & Annee == 1936)
hm_arr <- aggregate(Quantite ~ ID, hm_arr, sum)
export_csv(hm_arr, "heatmap_arrivee_1936")




# c. présentation du réseau -----------------------------------------------

# principales stations

grandes_stations <- head(arrange(aggregate(Quantite ~ ID, data_fret, sum), desc(Quantite)), 15)$ID
p_sta <- subset(data_fret, ID %in% grandes_stations)

#grandes gares départ vs arrivée

C1 <- ggplot(aggregate(Quantite ~ Mouvement + Gare, p_sta, sum), aes(x=Quantite/1000, forcats::fct_reorder(Gare, Quantite), fill = Mouvement)) + 
  geom_col() +
  labs(
    x = "Milliers de tonnes",
    y ="",
    fill = "",
    caption = "Répartition du trafic des principales gares"
  )

p_sta_data <- aggregate(Quantite ~ Gare + Type2, p_sta, sum)
p_sta_data <- aggr_types(p_sta_data)

C2 <- ggplot(p_sta_data, aes(x=Quantite/1000, y=forcats::fct_reorder(Gare, Quantite), fill=forcats::fct_reorder(Type2, Quantite))) + 
  geom_col() + 
  labs(
    x = "Milliers de tonnes",
    y ="",
    fill = "",
    caption = "Répartition du trafic des principales gares"
  )
 

# croissance moyenne
# sous-pop : prendre les stations qui existent déjà en 1923 et les comparer à leur état en 1936
data_1923 <- subset(data_fret, Annee == 1923)
data_1936 <- subset(data_fret, Annee ==  1936)

  #ne garder que les ID de stations qui existent dans les deux années
data_1936          <- subset(data_1936,ID %in% data_1923$ID)
data_1923          <- subset(data_1923, ID %in% data_1936$ID)
evol               <- aggregate(Quantite ~ ID, data_1923, sum)
evol$Quantite_1936 <- aggregate(Quantite ~ ID, data_1936, sum)$Quantite
evol              <- filter(evol, Quantite > 0, Quantite_1936 > 0)

#evolution moyenne = -19%, soit - 1,42% par an, pour toutes les stations
evol_moyenne = (sum(evol$Quantite_1936) * 100 / sum(evol$Quantite) - 100)/13

# On regarde l'évolution moyenne des plus grandes stations, qui stagnent aussi. La moyenne est encore plus basse.
# En fait, la plupart perdent beaucoup, et certaines gagnent un peu de trafic. -2,18 % 

evol_psta         <- subset(evol, ID %in% grandes_stations)
evol_psta$evol    <- evol_psta$Quantite_1936 * 100 /evol_psta$Quantite - 100
evol_moyenne_psta = (sum(evol_psta$Quantite_1936) * 100 / sum(evol_psta$Quantite) - 100)/13

# Si on fait une répartition du trafic en fonction du Type2, on se rend compte que les matériaux de constructions et matériel de chemin de
# fer constituent la majeure partie de la baisse. 

data_evol <- subset(data_fret, (Annee == 1923 | Annee == 1936) & Mouvement=="Depart")
data_evol <- aggr_types(data_evol)

# total du corpus de stations : 
C3 <- ggplot(aggregate(Quantite ~ Type2 + Annee, data_evol, sum), 
       aes(x = Quantite/1000, y = forcats::fct_reorder(Type2,Quantite), fill = as.character(Annee))) + 
  geom_col() +
  labs(
    x = "Milliers de tonnes",
    y = "",
    fill = "Année",
    caption = "Repartition par produit (total)"
  )

# Stations principales
data_evol_psta <- subset(data_evol, ID %in% evol_psta$ID)

C4 <- ggplot(aggregate(Quantite ~ Type2 + Annee, data_evol_psta, sum), 
       aes(x = Quantite/1000, y = forcats::fct_reorder(Type2,Quantite), fill = as.character(Annee))) + 
  geom_col() +
  labs(
    x = "Milliers de tonnes",
    y = "",
    fill = "Année",
    caption = "Repartition par produit (principales gares)"
  )

# Calcul de l'évolution du trafic sans les élements variables 
# liés à la construction du chemin de fer en particulier. 

data_stable <- subset(data_evol, Type2 != "Materiaux de construction" & Type2 != "Materiel de chemin de fer")

# corpus entier : 2,6% d'augmentation annuelle, soit 34% d'évolution totale
total1923                     = aggregate(Quantite ~ Annee, data_stable, sum)[1,2]
total1936                     = aggregate(Quantite ~ Annee, data_stable, sum)[2,2]
evol_totale_hors_construction = (total1936 * 100 / total1923 - 100)
evol_moyenne_hors_constr      = evol_totale_hors_construction /13

# et pour les stations principales: 0.01%
data_stable_psta              <- data_stable[sapply(data_stable$ID, function(id) id %in% evol_psta$ID),]
total1923                     = aggregate(Quantite ~ Annee, data_stable_psta, sum)[1,2]
total1936                     = aggregate(Quantite ~ Annee, data_stable_psta, sum)[2,2]
evol_moyenne_hors_constr_psta = (total1936 * 100 / total1923 - 100)/13



CPanel <- ggarrange(C2, 
                    ggarrange(C1, legend = "right"),
                    ggarrange(C3, C4, ncol = 2, common.legend = TRUE, legend = "right"),
                    nrow = 3, legend = "right")
CPanel
ggexport(CPanel, filename = "./plots/C_Presentation_reseau.svg")



# 2. Intégration économique

# a. centres urbains  ---------------------------------------------------------------

# Definition d'une sous-population de centres urbains :
# Ce sont les chefs-lieux de provinces que traverse et atteint 
# le chemin de fer (Ha Tinh par exemple n'y figure pas)
# on rajoute ben thuy (id#76) qui est le port de Vinh

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

part_trafic_urbain$part_fret   <- part_trafic_urbain$Fret_urbain / part_trafic_urbain$Fret_total
part_trafic_urbain$part_voy    <- part_trafic_urbain$Voy_urbain / part_trafic_urbain$Voy_total

part_trafic_urbain$Fret_total_depart   <- aggregate(Quantite ~ Annee, subset(data_fret, Mouvement=="Depart"), sum)$Quantite
part_trafic_urbain$Fret_total_arrivee  <- aggregate(Quantite ~ Annee, subset(data_fret, Mouvement=="Arrivee"), sum)$Quantite
part_trafic_urbain$Fret_urbain_depart  <- aggregate(Quantite ~ Annee, subset(data_fret_urbain, Mouvement=="Depart"), sum)$Quantite
part_trafic_urbain$Fret_urbain_arrivee <- aggregate(Quantite ~ Annee, subset(data_fret_urbain, Mouvement=="Arrivee"), sum)$Quantite

part_trafic_urbain$part_fret_depart    <- part_trafic_urbain$Fret_urbain_depart / part_trafic_urbain$Fret_total_depart
part_trafic_urbain$part_fret_arrivee   <- part_trafic_urbain$Fret_urbain_arrivee / part_trafic_urbain$Fret_total_arrivee

part_trafic_urbain$Voy_total_depart    <- aggregate(Quantite ~ Annee, subset(data, Mouvement=="Depart" & Type2 == "Voyageurs"), sum)$Quantite
part_trafic_urbain$Voy_total_arrivee   <- aggregate(Quantite ~ Annee, subset(data, Mouvement=="Arrivee" & Type2 == "Voyageurs"), sum)$Quantite
part_trafic_urbain$Voy_urbain_depart   <- aggregate(Quantite ~ Annee, subset(data_urbain, Mouvement=="Depart" & Type2 == "Voyageurs"), sum)$Quantite
part_trafic_urbain$Voy_urbain_arrivee  <- aggregate(Quantite ~ Annee, subset(data_urbain, Mouvement=="Arrivee" & Type2 == "Voyageurs"), sum)$Quantite

part_trafic_urbain$part_voy_depart     <- part_trafic_urbain$Voy_urbain_depart / part_trafic_urbain$Voy_total_depart
part_trafic_urbain$part_voy_arrivee    <- part_trafic_urbain$Voy_urbain_arrivee / part_trafic_urbain$Voy_total_arrivee

part_trafic_urbain$diff_voy            <- part_trafic_urbain$Voy_urbain_arrivee - part_trafic_urbain$Voy_urbain_depart
part_trafic_urbain$part_diff_voy       <- part_trafic_urbain$diff_voy / part_trafic_urbain$Voy_total
part_trafic_urbain$diff_voy_comp       <- part_trafic_urbain$Voy_urbain_arrivee / part_trafic_urbain$Voy_urbain_depart - 1


# En courbes
part_urbain <- melt(part_trafic_urbain[,c('Annee','part_fret','part_voy')],id.vars=1)
D6 <- ggplot(part_urbain, aes(x = Annee, y = value, colour = variable, yend = 0)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    colour = "",
    caption = "Part du trafic des centres urbains"
  ) +
  scale_colour_manual(labels = c("Marchandises", "Voyageurs"), values = hue_pal()(2))


# Repartition annuelle 
part_urbain2 <- melt(part_trafic_urbain[,c('Annee','part_fret','part_voy', 'part_fret_depart', 'part_fret_arrivee', 'part_voy_depart', 'part_voy_arrivee')],id.vars = 1)
ggplot(part_urbain2, aes(x = as.character(Annee), y= value)) + 
  geom_col(aes(fill = variable), position = "dodge")

# Comme la variabilité est assez faible, diagramme avec valeur moyennes
part_urbain2 <- aggregate(value ~ variable, part_urbain2, mean)
D7 <- ggplot(part_urbain2, aes(y = variable, x = value)) + 
  geom_col() +
  scale_x_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part du trafic des centres urbains"
  )

# on remarque un creusement dans la différence arrivé départ voyageurs
# en valeur absolue
D8 <- ggplot(part_trafic_urbain, aes(x = Annee, y = diff_voy/1000)) + 
  geom_line() + 
  geom_point() +
  labs(
    x = "",
    y = "Milliers de voyageurs",
    caption = "Solde de voyageurs"
  )

# en poucentage = 2%
ggplot(part_trafic_urbain, aes(x = Annee, y = part_diff_voy)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "solde des centres urbains rapporté au nb total de voyageurs"
  )

D9 <- ggplot(part_trafic_urbain, aes(x = Annee, y = diff_voy_comp)) + 
  geom_line() + ylab("Quantum de voyageurs arrivées vs partis (en %)") +
  geom_point() +
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Moyenne annuelle des surplus d'arrivée dans les centres urbains"
  )


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



# i on calcule la moyenne pour 1936 hors ces cas extrêmes on obtient une moyenne de 6% et médiane 7.8%
tmp <- subset(diff_voy_urbain, ID != 306 & ID != 74 & ID != 45 & Annee == 1936)
moy_qt_arr_dep_voy_urbain = mean(tmp$diff_part)

# our le reste des stations, la moyenne est de -4% et la mediane de -9%
voy_hors_urbain <- aggregate(Quantite ~ ID, subset(data_hors_urbain, Annee == 1936 & Mouvement == "Arrivee" & Type2 == "Voyageurs"), sum)
voy_hors_urbain <- rename(voy_hors_urbain, arrivee=Quantite)
voy_hors_urbain$depart <- aggregate(Quantite ~ ID, subset(data_hors_urbain, Annee == 1936 & Mouvement == "Depart" & Type2 == "Voyageurs"), sum)$Quantite
voy_hors_urbain <- filter(voy_hors_urbain, arrivee!=0, depart!=0)
voy_hors_urbain$quantum <- voy_hors_urbain$arrivee * 100 / voy_hors_urbain$depart - 100



## % des importation des diverses marchandises par annee

part_fret_urbain2              <- aggr_types(subset(data_fret_urbain, Mouvement == "Arrivee"))
part_fret_urbain2              <- aggregate(Quantite ~ Annee + Type2, part_fret_urbain2, sum)                   
part_fret_urbain2$Total        <- aggregate(Quantite ~ Annee + Type2, aggr_types(subset(data_fret, Mouvement == "Arrivee")), sum)$Quantite     
part_fret_urbain2$depart       <- aggregate(Quantite ~ Annee + Type2, aggr_types(subset(data_fret_urbain, Mouvement == "Depart")), sum)$Quantite
part_fret_urbain2$depart_total <- aggregate(Quantite ~ Annee + Type2, aggr_types(subset(data_fret, Mouvement == "Depart")), sum)$Quantite   
part_fret_urbain2$part         <- part_fret_urbain2$Quantite * 100 / part_fret_urbain2$Total
part_fret_urbain2$part_depart  <- part_fret_urbain2$depart * 100 / part_fret_urbain2$depart_total
part_fret_urbain2              <- filter(part_fret_urbain2, Type2 != "Materiel de chemin de fer", Type2 != "Autre")

# Données exploratoires : annexes
ggplot(part_fret_urbain2, aes(x = as.character(Annee), y = part, fill = Type2)) + geom_col(position="dodge")
ggplot(part_fret_urbain2, aes(x = as.character(Annee), y = part_depart, fill = Type2)) + geom_col(position="dodge")

# Les centres urbains expédient 60% de la production agricole, 
# et sont le point d'arrivée également de 75% de cette production

data_agri_urbain <- subset(data_fret_urbain, Type2 == "Produits agricoles et betail" & Annee != 1927)
data_agri_urbain <- aggregate(Quantite ~ Annee + Type + Mouvement, data_agri_urbain, sum)

# On voit qu'effectivement les centres urbains sont aussi des endroits très exportateurs de p. Agri 
# et que l'année 1931 marque même un moment d'exportation globale; 

# Exploratoire
ggplot(data_agri_urbain, aes(y = Type, x = Quantite/1000, fill = Mouvement)) + geom_col(position="dodge") + facet_wrap(~ Annee)
ggplot(aggregate(Quantite ~ Annee, subset(data_agri_urbain, Mouvement == "Arrivee"), sum), aes(x = Annee, y = Quantite/1000)) + 
  geom_line() +
  ylab("tonnes de produits agricoles importés dans les principaux centres urbains (milliers)")



# en effet pratiquement toujours au dessus de 50% sauf en 1936. 
contribution_urbain      <- aggregate(Quantite ~ Annee + Mouvement, data_agri_urbain, sum)
contribution_urbain      <- dcast(contribution_urbain, formula = Annee~Mouvement, fun.aggregate = sum, value.var = "Quantite")
contribution_urbain$part <- contribution_urbain$Depart / contribution_urbain$Arrivee

D1 <- ggplot(contribution_urbain, aes(x = Annee, y = part, yend = 0)) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Expedition p.r. arrivage produits agricoles centres urbains"
  )

# nombre de stations qui exporte au moins une tonne de produits agri/betail
agri_station  <- aggregate(Quantite ~ Annee + ID, subset(data_fret, Mouvement == "Depart" &  Type2 == "Produits agricoles et betail"), sum)
agri_station <- filter(agri_station, Quantite > 0)
agri_station <- aggregate(ID ~ Annee, agri_station, length)
agri_station <- left_join(agri_station, nb_gares, by = "Annee")

D2 <- ggplot(agri_station, aes(x = Annee, y = ID / NB)) + 
  geom_line() + 
  geom_point() +
  scale_y_continuous(label = percent, limit = c(0, 1)) +
  labs(
    x = "",
    y = "",
    caption = "Part des gares expédiant des matières agricoles"
  )


# distribution des déciles de l'export de produits agricoles
# On voit que La tendance avant 1929 est vers l'augmentation de la part du 10e décile
# et elle s'inverse après = des stations moyennes qui participent de plus en plus

agri_station2 <- aggregate(Quantite ~ Annee + ID, subset(data_fret, Annee != 1927 & Mouvement == "Depart" &  Type2 == "Produits agricoles et betail"), sum)
agri_station2 <- filter(agri_station2, Quantite > 0)
agri_station2 <- agri_station2 %>%
  group_by(Annee) %>%
  mutate(decile = ntile(Quantite, 10))
agri_station2$decile <- sapply(agri_station2$decile, function(x) merge_quantiles(x, 7, "Reste"))

D3 <- ggplot(agri_station2) + 
  stat_summary(
    mapping = aes(x = as.character(Annee), y = Quantite),
    fun.min = function(z) { quantile(z,0.25) },
    fun.max = function(z) { quantile(z,0.75) },
    fun = median) +
  labs (
    x = "",
    y = "Tonnes",
    caption = "Médiane, 1er et 4e quartiles de l'expédition agricole"
  )

D4 <- ggplot(agri_station2, aes(x = as.character(Annee), y = Quantite/1000, fill = forcats::fct_reorder(decile, Quantite))) + 
  geom_col() + 
  theme(legend.title = element_text(size = rel(0.7)),
        legend.text  = element_text(size = rel(0.7)),
        legend.key.size = unit(0.5, 'cm')) +
  labs(
    x = "",
    y = "Milliers de tonnes",
    fill = "Décile",
    caption = "Répartition de l'expédition agricole par décile"
  )

agri_station3 <- aggregate(Quantite ~ Gare, subset(data_fret, ID %in% urbains & Annee != 1927 & Mouvement == "Depart" &  Type2 == "Produits agricoles et betail"), sum)
D5 <- ggplot(agri_station3, aes(x = Quantite/1000, y = forcats::fct_reorder(Gare, Quantite))) +
  geom_col() +
  labs(x = "Milliers de tonnes",
       y = "",
       caption = "Répartition de l'expédition agricole dans les centres urbains") 


DPanel <- ggarrange(D5, 
                    ggarrange(D1, D2, D3, D4, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2),
                    nrow = 2, labels = c("E")
                  )
DPanel
ggexport(DPanel, filename = "./plots/D_urbain_agri.svg")

DPanel2 <- ggarrange(D6, D7, D8, D9, ncol =2, nrow = 2, labels = c("A", "B", "C", "D"), legend = "top")
DPanel2
ggexport(DPanel2, filename = "./plots/D_urbain_trafic.svg")



# Engrais : pas de corrélation

data_engrais <- aggregate(Quantite ~ Annee + Type2, subset(data_fret, Annee != 1927 & Mouvement == "Arrivee" &  (Type2 == "Engrais" | Type2 == "Produits agricoles et betail")), sum)
ggplot(data_engrais, aes(x = Annee, y = Quantite/1000, colour = Type2)) +
  geom_line() +
  geom_point()

data_engrais <- dcast(data_engrais, formula = Annee~Type2, fun.aggregate = sum, value.var = "Quantite")
data_engrais <- rename(data_engrais, Agri = `Produits agricoles et betail`)
plot(Agri ~ Engrais, data_engrais)






# b. voyageurs ------------------------------------------------------------





# PANEL 1

# Evolution du nombre de voyageurs
data_voy <- melt(data_voyageurs[,c('Annee','Total_123','Total_4')],id.vars = 1)

Y1 <- ggplot(data_voy, aes(x = Annee, y = value/100000, colour = variable)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(trans = log10_trans()) +
  labs(
    x = "",
    y = "Voyageurs (Centaines de milliers)",
    colour = "Classe",
    caption = "Évolution du nombre de voyageurs"
  ) + 
  scale_colour_manual(labels = c("1, 2, 3", "4"), values = hue_pal()(2))


# km moyen 
data_voy2 <- melt(data_voyageurs[,c('Annee','KM_moyen_123','KM_moyen_4')],id.vars = 1)

Y2 <- ggplot(data_voy2, aes(x = Annee, y = value, colour = variable)) + 
  geom_line() +
  geom_point() +
  scale_y_continuous(trans = log10_trans()) +
  labs(
    x = "",
    y = "Kilomètres",
    colour = "Classe",
    caption = "Évolution de la moyenne de kilomètres parcourus"
  ) + 
  scale_colour_manual(labels = c("1, 2, 3", "4"), values = hue_pal()(2))


# part des voyageurs des classes 1, 2, 3

Y3 <- ggplot(data_voyageurs, aes(x = Annee, y = Total_123 / (Total_123 + Total_4), yend = 0)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part des classes 1, 2, 3 dans le total des voyageurs"
  )


# part des recettes des 3 premières classes classe

Y4 <- ggplot(data_voyageurs, aes(x = Annee, y = Recettes_123 / (Recettes_123 + Recettes_4), yend = 0)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part des classes 1, 2, 3 dans le total des recettes"
  )


YPanel <- ggarrange(Y1, Y2, Y3, Y4,  labels = c("A", "B", "C", "D"),
                    common.legend = TRUE, legend = "top",  ncol = 2, nrow = 2)
YPanel
ggexport(YPanel, filename = "./plots/Voyageurs1.svg")




# PANEL 2 : indice du prix des billets (IPB) au KM, Regressions

data_voy3 <- data_voyageurs
data_voy3$Recettes_Totales <- data_voy3$Recettes_123 + data_voy3$Recettes_4

data_voy3$IPB_KM_4   <- (data_voy3$Recettes_4/data_voy3$KM_total_4) / data_voy3$Recettes_Totales * 10000000000
data_voy3$IPB_KM_123 <- (data_voy3$Recettes_123/data_voy3$KM_total_123) / data_voy3$Recettes_Totales * 10000000000
data_voy3$IPB_V_4    <- (data_voy3$Recettes_4/data_voy3$Total_4) / data_voy3$Recettes_Totales * 100000000


# IPB par KM

data_voy4 <- melt(data_voy3[,c('Annee','IPB_KM_123','IPB_KM_4')],id.vars = 1)

X1 <- ggplot(data_voy4, aes(x = Annee, y = value, colour = variable)) +
  geom_point() + 
  geom_line() +
  labs(
    x = "",
    y = "IPB (par km)",
    colour = "Classe",
    caption = "Évolution de l'indice de prix des billets"
  ) + 
  scale_colour_manual(
    labels = c("1, 2, 3", "4"), 
    values = hue_pal()(2))

# IPB par voyageur de 4e

X2 <- ggplot(data_voy3, aes(x = Annee, y = IPB_V_4, yend = 0)) +
  geom_point(col="#00BFC4") + 
  geom_line(col="#00BFC4") +
  labs(
    x = "",
    y = "IPB (par voyageur)",
    colour = "Classe",
    caption = "Évolution de l'indice de prix des billets de 4e classe"
  )

# Correlation entre les IPB et le nombre de voyageurs de 4e ? 
# (on sait déjà que c'est pas le cas pour les classes au dessus)

X3 <- ggplot(data_voy3, aes(y=IPB_V_4, x=Total_4/1000000)) + 
  geom_line(col="#00BFC4") +  
  geom_point(col="#00BFC4") + 
  geom_smooth(method='lm') + 
  labs(
    x = "Voyageurs (millions)",
    y = "IPB (par voyageur)",
    caption = "Régression entre l'IPB et le nombre de voyageurs")


# ID. avec le nombre de kilomètres parcourus

X4 <- ggplot(data_voy3, aes(y=IPB_KM_4, x=Total_4/1000000)) + 
  geom_line(col="#00BFC4") + 
  geom_point(col="#00BFC4") +  
  geom_smooth(method='lm') + 
  labs(x = "Voyageurs (millions)", 
       y= "IPB (par KM)",
       caption = "Régression entre l'IPB et le nombre de voyageurs")

XPanel <- ggarrange(X1, X2, X3, X4,  labels = c("A", "B", "C", "D"), 
                    common.legend = TRUE, legend = "top",  ncol = 2, nrow = 2)
XPanel
ggexport(XPanel, filename = "./plots/Voyageurs2.svg")

# c. interdépendances -----------------------------------------------------

# est-ce que le raccordement progressif des lignes amène à une croissance des échanges entre régions ? 

data_fret2         <- aggr_types(subset(data_fret, Annee != 1927))
data_inter         <- aggregate(Quantite ~ ligne_clean + Type2 + Annee, subset(data_fret2, Mouvement == "Depart"), sum)
data_inter         <- rename(data_inter, depart=Quantite)
data_inter$arrivee <- aggregate(Quantite ~ ligne_clean + Type2 + Annee, subset(data_fret2, Mouvement == "Arrivee"), sum)$Quantite
data_inter$delta   <- data_inter$depart - data_inter$arrivee

# calculer la quantité des interdependances
data_inter$d_abs  <- abs(data_inter$delta)

inter_annuel      <- aggregate(d_abs ~ Annee, data_inter, sum)
inter_annuel      <- rename(inter_annuel, diff_annuelle = d_abs)
inter_annuel$diff_annuelle <- inter_annuel$diff_annuelle / 2

# marge d'erreur : erreur de saisie ou dans les statistiques, la somme des interdependances n'est pas nulle. 
inter_annuel$incoherences  <- aggregate(delta ~ Annee, data_inter, sum)$delta
inter_annuel$diff_annuelle_aj <- inter_annuel$diff_annuelle + inter_annuel$incoherences

ggplot(inter_annuel, aes(x = as.character(Annee), y = diff_annuelle/1000)) + 
  geom_col()

E1 <- ggplot(inter_annuel, aes(x = as.character(Annee), y = diff_annuelle_aj/1000)) + 
  geom_col() +
  labs(
    y = "Milliers de tonnes",
    x = "",
    caption ="Transit entre plusieurs lignes"
    )


# distribution des interdependances par ligne 
#exploratoire
ggplot(aggregate(delta ~ Annee + ligne_clean, data_inter, sum), aes(x = Annee, y = delta/1000, fill = ligne_clean)) + 
  geom_col(position="dodge")


# hanoi benthuy de loin la ligne qui concentre les arrivées: 
# en plotant les Type2 on se rend compte que c'est largement dû à l'importation du bois et de matériaux de construction. 
E2 <- ggplot(subset(data_inter, ligne_clean=="hanoi_ben_thuy"), aes(x=as.character(Annee), y=delta/1000, fill = Type2)) + 
  geom_col(position="dodge") +
  labs(
    x = "",
    y = "Milliers de tonnes",
    caption = "Détail du solde de la ligne Ha Noi - Vinh"
  )


# test sur l'année 1936 : distribution par type 2 Exploratoire
sub_data_inter <- subset(data_inter, Annee == 1936)
ggplot(sub_data_inter, aes(y = forcats::fct_reorder(Type2, desc(delta)), x = delta/1000, fill = ligne_clean)) + geom_col(position="dodge")


EPanel = ggarrange(E1, E2, labels = c("A", "B"))
EPanel
ggexport(EPanel, filename = "./plots/E_Interdependances1.svg")


## ports

# présentation générale des ports
F1 <- ggplot(data_ports, aes(x = Annee, y = Tonnage, color=Port)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(trans = log10_trans(), 
                     breaks = trans_breaks("log10", function(x) 10^x), 
                     labels = trans_format("log10", math_format(10^.x))) +
  labs(
    x = "",
    y = "Tonnes",
    caption = "Présentation du trafic des ports de la voie ferrée"
       )

# petits ports de cabotage sur la voie ferrée

# Ben Thuy
ggplot(subset(data_ports, Port != "Saigon"), aes(x = Annee, y = Tonnage/1000, color=Port)) + geom_line() + geom_point()

port_ben_thuy <- subset(data_fret, Gare == "Ben Thuy")
port_ben_thuy <- aggregate(Quantite ~ Annee, port_ben_thuy, sum)
port_ben_thuy <- left_join(port_ben_thuy, subset(data_ports, Port=="Ben Thuy") , "Annee")

port_ben_thuy <- melt(port_ben_thuy[,c('Annee','Quantite','Tonnage')],id.vars = 1)


F2 <- ggplot(port_ben_thuy, aes(x = Annee, y = value/1000, colour = variable)) + 
  geom_line() +
  geom_point() +
  labs(
    x = "",
    y = "Milliers de tonnes",
    colour = "",
    caption = "Ben Thuy"
  ) +
  scale_colour_manual(
    labels = c("Voie ferrée", "Port"), 
    values = hue_pal()(2))

# Tourane
port_tourane <- subset(data_fret, ID == 147 | ID == 148)
port_tourane <- aggregate(Quantite ~ Annee, port_tourane, sum)
port_tourane <- left_join(port_tourane, subset(data_ports, Port=="Tourane") , "Annee")

port_tourane <- melt(port_tourane[,c('Annee','Quantite','Tonnage')],id.vars = 1)

F3 <- ggplot(port_tourane, aes(x = Annee, y = value/1000, colour = variable)) + 
  geom_line() +
  geom_point() +
  labs(
    x = "",
    y = "Milliers de tonnes",
    colour = "",
    caption = "Da Nang"
  ) +
  scale_colour_manual(
    labels = c("Voie ferrée", "Port"), 
    values = hue_pal()(2))



# Phan tiet
port_phan_tiet <- subset(data_fret, ID == 284)
ggplot(port_phan_tiet, aes(x = Annee, y = Quantite, fill = Type2)) + geom_col()

port_phan_tiet <- aggregate(Quantite ~ Annee, port_phan_tiet, sum)
port_phan_tiet <- full_join(port_phan_tiet, subset(data_ports, Port=="Phantiet") , "Annee")
port_phan_tiet <- melt(port_phan_tiet[,c('Annee','Quantite','Tonnage')],id.vars = 1)

F4 <- ggplot(port_phan_tiet, aes(x = Annee, y = value/1000, colour = variable)) + 
  geom_line() +
  geom_point() +
  labs(
    x = "",
    y = "Milliers de tonnes",
    colour = "",
    caption = "Phan Tiet"
  ) +
  scale_colour_manual(
    labels = c("Voie ferrée", "Port"), 
    values = hue_pal()(2))

FPanel <- ggarrange(F2, F3, F4, labels = c("A", "B", "C"), ncol = 2, nrow = 2, common.legend = TRUE, legend = "top")
ggexport(FPanel, filename = "./plots/F_Ports2.svg")




# 3. Prédation environnementale

## hevea

# subset : stations dont on est sûr qu'elles expédient majoritairement de l'hévéa.
data_fret_agri   <- subset(data_fret, Type2 == "Produits agricoles et betail" & Mouvement == "Depart")
data_hevea       <- subset(data_fret_agri,  ID == 249 | ID==281 | ID==293 | ID == 294 | ID == 295)
total_agri       <- aggregate(Quantite ~ Annee, data_fret_agri, sum)

data_hevea       <- aggregate(Quantite ~ Annee, subset(data_hevea, Type=="Caoutchouc" | (Type == "Produits agricoles" & Annee != 1923 & Annee != 1925)), sum)
data_hevea$Total <- total_agri$Quantite
data_hevea$Part  <- data_hevea$Quantite / data_hevea$Total

# quantitée en valeur absolue
G1 <- ggplot(data_hevea, aes(x = Annee, y = Quantite/1000)) + 
  geom_line() + 
  geom_point() + 
  labs(
    x = "",
    y = "Milliers de tonnes",
    caption = "Quantité de caoutchouc expédiée"
  )

# comparaison avec le pourcentage de trafic agricole total
G2 <- ggplot(data_hevea, aes(x = Annee, y = Part)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(label = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part du Caoutchouc dans le trafic agricole"
  )

# Part du transport en chemin de fer par rapport à la qté totale exportée
data_hevea$Part_hevea <- data_hevea$Quantite / export_hevea

G3 <- ggplot(data_hevea, aes(x = Annee, y = Part_hevea, yend = 0)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(label = percent) +
  labs(x = "",
       y = "",
       caption = "Part de la voie ferrée dans les exportations de caoutchouc")


GPanel <- ggarrange(G1, G2, G3, labels = c("A", "B", "C"), ncol = 2, nrow = 2)
GPanel
ggexport(GPanel, filename = "./plots/G_Hevea1.svg")


# regarder quelle station contribue le plus

data_hevea2 <- subset(data_fret_agri, ID== 249 | ID==281 | ID==293 | ID == 294 | ID == 295)
data_hevea2 <- subset(data_hevea2, Type=="Caoutchouc" | (Type == "Produits agricoles" & Annee != 1923 & Annee != 1925))

G4 <- ggplot(data_hevea2, aes(y=as.character(Annee), x=Quantite/1000, fill=forcats::fct_reorder(Gare, Quantite))) + 
  geom_col() +
  labs(
    y = "",
    x = "Milliers de tonnes",
    fill = "Gares",
    caption = "Répartition de l'expédition de caoutchouc par gare"
  )

# si on garde les trois principales et qu'on regarde les importations etc
data_hevea3       <- subset(data_fret, ID==293 | ID == 294 | ID == 295)
data_hevea3       <- aggr_types(data_hevea3)
data_hevea3$Type2 <- replace(data_hevea3$Type2, data_hevea3$Type2 ==  "Materiel de chemin de fer", "Materiaux de construction")
data_hevea3       <- subset(data_hevea3, Type2 != "Autre")

# le fonctionnement de ces plantations necessite unle chemin de fer pour importer matériaux de construction, etc.
G5 <- ggplot(data_hevea3, aes(y = Mouvement, x = Quantite/1000, fill = forcats::fct_reorder(Type2, Quantite))) + 
  geom_col() + 
  facet_wrap(~Gare) + 
  labs(
    y = "",
    x = "Milliers de tonnes",
    fill="Catégorie",
    caption = "Trafic détaillé des principales gares exportatrices de caoutchouc"
  )

# test individuel sur Xuan Loc
G6 <- ggplot(subset(data_hevea3, ID == 293), aes(y = Mouvement, x = Quantite/1000, fill = forcats::fct_reorder(Type2, Quantite))) + 
  geom_col() + 
  facet_wrap(~Annee) + 
  labs(
    y = "",
    x = "Milliers de tonnes",
    fill="Catégorie",
    caption = "Décomposition du trafic annuel de Xuan Loc"
  )

GPanel2 <- ggarrange(G4, G5, labels = c("A", "B"), ncol = 1, nrow = 2)
GPanel2
ggexport(GPanel2, filename = "./plots/G_Hevea2.svg")

GPanel3 <- ggarrange(G6, legend = "top")
GPanel3


## Bois



data_bois <- subset(data_fret, Type2 == "Bois et produits forestiers" & Mouvement == "Depart" & Annee != 1927)

# overview
data_bois0 <- subset(data_bois, ligne_clean != "tourane_nha_trang" & ligne_clean != "ben_thuy_dong_ha" & ligne_clean != "saigon_my_tho")
H1 <- ggplot(aggregate(Quantite ~ Annee + ligne_clean, data_bois0, sum), aes(y = as.character(Annee), x = Quantite/1000, fill = forcats::fct_reorder(ligne_clean, Quantite))) +
  geom_col() + 
  scale_fill_manual(labels = c("Hue - Da Nang", "Ha Noi - Lang Son", "Ha Noi - Vinh", "Sai Gon - Nha trang"), values = hue_pal()(4)) +
  labs(
   x = "Milliers de tonnes",
   y = "",
   fill = "Lignes",
   caption = "Décomposition par ligne de l'expédition de bois"
  )
  

# nombre de stations qui exportent du bois selon l'année. 
data_bois2 <- aggregate(Quantite ~ Annee + ID, data_bois, sum)
data_bois2 <- filter(data_bois2, Quantite > 10)
data_bois2 <- aggregate(ID ~ Annee, data_bois2, length)
data_bois2 <- rename(data_bois2, NB_bois = ID)
data_bois2 <- left_join(data_bois2, nb_gares, by = "Annee")

# en valeur absolue
H2 <- ggplot(data_bois2, aes(x = Annee, y = NB_bois, yend=0)) + 
  geom_line() + 
  geom_point() +
  labs(
    y = "",
    x = "",
    caption = "Nombre de stations exportant au moins 10 tonnes de bois"
  )

# déciles 
data_bois3 <- aggregate(Quantite ~ Annee + ID, data_bois, sum)
data_bois3 <- data_bois3 %>%
  group_by(Annee) %>%
  mutate(decile = ntile(Quantite, 20))

data_bois3$decile <- sapply(data_bois3$decile, function(x) merge_quantiles(x, 18, "≤ 17"))
H3 <- ggplot(data_bois3, aes(x = as.character(Annee), y = Quantite/1000, fill = forcats::fct_reorder(decile, Quantite))) + 
  geom_col() + 
  theme_bw() +
  labs(
    y = "Milliers de tonnes",
    x = "",
    fill = "Quantile",
    caption = "Décomposition en quantile de l'exportation de bois"
  )



HPanel <- ggarrange(H1,
                    ggarrange(H2, H3, ncol = 2),
                    nrow = 2)
HPanel
ggexport(HPanel, "./plots/H_Bois1.svg")

# On s'intéresse région de Bien Hoa : 10 stations qui en expédient le plus
gares_bois <- head(arrange(aggregate(Quantite ~ ID, subset(data_bois, ligne_clean == "nha_trang_saigon"), sum), desc(Quantite)), 10)$ID
data_bois4 <- subset(data_bois, ID %in% gares_bois)

# 88 % total de la ligne
t = sum(data_bois4$Quantite) * 100 / sum(subset(data_bois, ligne_clean == "nha_trang_saigon")$Quantite)

# Répartition des gares
H4 <- ggplot(data_bois4, aes(y = as.character(Annee), x = Quantite/1000, fill = forcats::fct_reorder(Gare, Quantite))) + 
  geom_col() +
  labs(
    y = "",
    x = "Milliers de tonnes",
    fill = "Gares",
    caption = "Détail des 10 gares les plus exportatrices de bois de la région de Bien Hoa"
  )

# répartition dans le total
data_bois4 <- aggregate(Quantite ~ Annee, data_bois4, sum)
data_bois4$total <- aggregate(Quantite ~ Annee, data_bois, sum)$Quantite
 
H5 <- ggplot(data_bois4, aes(x = Annee, y = Quantite/ 1000, yend = 0)) +
  geom_line() +
  geom_point() +
  labs(x= "",
       y = "Milliers de tonnes",
       caption = "Expédition cumulée de bois de ces dix gares")


H6 <- ggplot(data_bois4, aes(x = Annee, y = Quantite / total, yend=0)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(label = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part de ces 10 gares dans le total des expéditions de bois"
  )

HPanel2 <- ggarrange(H4, 
          ggarrange(H5, H6, labels = c("B", "C"), ncol = 2),
          labels = c("A"), nrow = 2)
ggexport(HPanel2, filename = "./plots/H_Bois2.svg")


# carte de la région de Bien Hoa, une en 1923 et une en 1929 et une en 1936 :
data_bois5 <- subset(data_bois, ligne_clean == "nha_trang_saigon")
data_bois5 <- filter(data_bois5, Quantite > 10)
export_csv(subset(data_bois5, Annee == 1923), "Bien_Hoa_Bois_1923")
export_csv(subset(data_bois5, Annee == 1929), "Bien_Hoa_Bois_1929")
export_csv(subset(data_bois5, Annee == 1936), "Bien_Hoa_Bois_1936")

# Ligne Hanoie Ben Thuy évolution de l'expédition de bois
data_bois6 <- subset(data_bois, ligne_clean == "hanoi_ben_thuy")
data_bois6 <- filter(data_bois6, Quantite > 10)
data_bois6 <- filter(data_bois6, Annee != 1927)

# -> on observe la diminution de l'expédition, on voit
ggplot(data_bois6) + 
  theme_bw() +
  stat_summary(
    mapping = aes(x = Annee, y = Quantite),
    fun.min = function(z) { quantile(z,0.25) },
    fun.max = function(z) { quantile(z,0.75) },
    fun = median) +
  labs (
    x = "",
    y = "Bois (Tonnes)",
    caption = "Médiane, 1er et 4e quartiles de l'expédition de bois"
  )

# % de l'expédition à Ben Thuy vs. production totale. 
data_bois7 <- subset(data_bois, ligne_clean == "hanoi_ben_thuy" & Annee != 1927)
tmp        <- aggregate(Quantite ~ Annee, subset(data_bois7, Gare == "Ben Thuy"), sum)
data_bois7 <- aggregate(Quantite ~ Annee, data_bois7, sum)
data_bois7 <- rename(data_bois7, Total = Quantite)
data_bois7 <- left_join(data_bois7, tmp, by = "Annee")

ggplot(data_bois7, aes(x = Annee, y = Quantite/1000)) + 
  theme_bw() +
  labs(
    x = "",
    y = "Expédition totale de Bois (Milliers de tonnes)"
  ) +
  geom_line() +
  geom_point()


ggplot(data_bois7, aes(x = Annee)) + 
  theme_bw() +
  labs(
    x = "",
    y = "Expédition totale de Bois (Milliers de tonnes)",
    caption = "Exp. totale en rouge et de Ben Thuy en vert"
  ) +
  geom_line(aes(y = Quantite/1000), col="seagreen") +
  geom_point(aes(y = Quantite/1000), col="seagreen") + 
  geom_line(aes(y = Total/1000), col="red4") +
  geom_point(aes(y = Total/1000), col="red4")

# ligne hanoi nacham

# population 
data_bois8        <- subset(data_bois, ligne_clean == "na_cham_hanoi")
station_bois_hnnc <- head(arrange(aggregate(Quantite ~ ID, data_bois8, sum), desc(Quantite)), 8)$ID
tmp               <- data_bois8
data_bois8        <- subset(data_bois8, ID %in% station_bois_hnnc)

# Ces 8 stations représentent 94% du total des exp cumulées de bois de la ligne
t = sum(data_bois8$Quantite) * 100 / sum(tmp$Quantite)

# Carte
export_csv(aggregate(Quantite ~ ID, data_bois8, sum), "bois_ligne_nacham")

data_bois8 <- aggregate(Quantite ~ Annee + Gare, data_bois8, sum)
data_bois8 <- filter(data_bois8, Annee == 1923 | Annee == 1929 | Annee == 1936)
data_bois8$Quantite <- arrondi(data_bois8$Quantite, 50)

# tableau de l'évolution des expéditions, en 1923, 1929 et 1936, en milliers de tonnes
data_bois8 <- dcast(data_bois8, formula = Gare~Annee, fun.aggregate = sum, value.var = "Quantite")


# % des centres urbains dans les importations de bois. On voit que la baisse de l'arrivée de bois est principalement expliquée
# par les centres urbains. Les autres stations restent relativement stable, même si on observe un léger declin dû au ralentissement
# de la construction.

data_bois9  <- subset(data_fret, Type2 == "Bois et produits forestiers" & Mouvement == "Arrivee" & Annee != 1927)
urbain_bois <- aggregate(Quantite ~ Annee, subset(data_bois9, ID %in% urbains), sum)
data_bois9  <- aggregate(Quantite ~ Annee, data_bois9, sum)
urbain_bois$Total <- data_bois9$Quantite
urbain_bois$Reste <- urbain_bois$Total - urbain_bois$Quantite

urbain_bois <- melt(urbain_bois[,c('Annee','Quantite','Total', 'Reste')],id.vars = 1)

ggplot(urbain_bois, aes(x = Annee, y = value/1000, colour = variable)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  labs(
    x = "",
    y = "Bois à l'arrivée (Milliers de tonnes)",
    colour = "Distribution"
  )



##  Administration fossile ?  


# Services, croissance la plus élevée de toutes les catégories. On enlève 1923 car pas encore comptabilisé partout. 
# pincettes car arrive que ce ne soit pas comptabilisé. 

data_services <- subset(data_fret, Mouvement == "Depart" & Type2 == "Services" & Annee != 1923)

ggplot(data_services, aes(x = Quantite/1000, y = as.character(Annee), fill = forcats::fct_reorder(ligne_clean, Quantite))) + 
  geom_col() +
  labs(
    x = "Marchandises exportées en services (Milliers de tonnes)",
    y = "",
    fill = "Lignes"
  )

# taux de croissance moyen annuel : 6%, bien au delà des autres moyennes calculées. 

evol_moy_services = (sum(subset(data_services, Annee == 1936)$Quantite) * 100 / sum(subset(data_services, Annee == 1925)$Quantite) - 100)/11

# % de services en fonction de la ligne
data_services2 <- aggregate(Quantite ~ ligne_clean + Type2, data_fret, sum) %>%
  group_by(ligne_clean) %>%
  mutate(Part = Quantite * 100 /sum(Quantite) )
  
ggplot(subset(data_services2, Type2 == "Services"), aes(y = forcats::fct_reorder(ligne_clean, Part), x = Part)) +
  geom_col() + 
  labs(
    y = "Part des transports en services dans le trafic total (%)",
    x = "")


# infrastructure de soutien à la route : (Pétrole)

data_pétrole <- subset(data, Type2 == "Fossiles" & Mouvement == "Depart")
data_pétrole <- subset(data_pétrole, grepl("Petrole|Essence", Type))
data_pétrole <- aggregate(Quantite ~ Annee, data_pétrole, sum)


# Augmentation du transit de pérole, essence, etc. en valeur absolue :
Z1 <- ggplot(data_pétrole, aes(x = Annee, y = Quantite/1000)) + 
  geom_line() + 
  geom_point() + 
  labs(
    x = "",
    y = "Milliers de tonnes",
    caption = "Pétrole en transit sur le chemin de fer"
  )


# "pèse" peu dans le volume total de fret 

data_pétrole$total_fret <- aggregate(Quantite ~ Annee, subset(data_fret, Mouvement = "Depart"), sum)$Quantite
data_pétrole$prc_fret   <- data_pétrole$Quantite  / data_pétrole$total_fret

Z2 <- ggplot(data_pétrole, aes(x = Annee, y = prc_fret)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part du pétrole dans le trafic total de marchandises"
  )


# % du transit de pétrole entre centres urbains

data_pétrole2         <- subset(data_fret, Type2 == "Fossiles")
data_pétrole2         <- subset(data_pétrole2, grepl("Petrole|Essence", Type))
tmp                   <- aggregate(Quantite ~ Annee, subset(data_pétrole2, ID %in% urbains), sum)
data_pétrole2         <- aggregate(Quantite ~ Annee, data_pétrole2, sum)
data_pétrole2$urbains <- tmp$Quantite
data_pétrole2$prc_urb <- data_pétrole2$urbains / data_pétrole2$Quantite

# 66 % en moyenne. 
part_urbain_petrole = mean(data_pétrole2$prc_urb)


# part dans le total du pétrole importé augmente beaucoup : au début la consommation se fait beaucoup dans les ports
# au fur et à mesure que la route s'impose, le cdf soutient l'éparpillement de la demande de carburant. 

data_pétrole$total      <- import_petrole
data_pétrole$prc_total  <- data_pétrole$Quantite / data_pétrole$total

Z3 <- ggplot(data_pétrole, aes(x = Annee, y = prc_total)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part du pétrole importé en transit sur le chemin de fer"
  )


# part du pétrole dans les recettes

data_pétrole$prc_recettes <- prc_rec_petr

Z4 <- ggplot(data_pétrole, aes(x = Annee, y = prc_recettes)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(labels = percent) +
  labs(
    x = "",
    y = "",
    caption = "Part du pétrole dans les recettes d'exploitation"
  )
  
## Panel Pétrole :

ZPanel <- ggarrange(Z1, Z2, Z3, Z4, labels = c("A", "B", "C", "D"),ncol = 2, nrow = 2)
ZPanel
ggexport(ZPanel, filename = "./plots/petrole.svg")


# Carte pétrole
data_pétrole3        <- subset(data, Type2 == "Fossiles")
data_pétrole3        <- subset(data_pétrole3, grepl("Petrole|Essence", Type))
data_pétrole_depart  <- head(arrange(aggregate(Quantite ~ ID, subset(data_pétrole3, Mouvement == "Depart"), sum) ,desc(Quantite)), 10)
data_pétrole_arrivee <- head(arrange(aggregate(Quantite ~ ID, subset(data_pétrole3, Mouvement == "Arrivee"), sum) ,desc(Quantite)), 10)

export_csv(data_pétrole_depart, "petrole_depart")
export_csv(data_pétrole_arrivee, "petrole_arrivee")







