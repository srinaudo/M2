
path = "/home/silvio/fac/memoire/GIS/csv_requetes/"


qt <- subset(data, Type2 != "Voyageurs" & Quantité != -1 & Mouvement == "Départ" & Annee == 1936)

temp <- aggregate(Quantité ~ ID + Annee, qt, sum)
temp <- aggregate(Quantité ~ ID, qt, sum)


quantile(temp$Quantité, probs = seq(.1, .9, by = .1))


temp$decile <- ntile(temp$Quantité, 10)

write.csv(temp, paste(path, "test_deciles.csv"), row.names = FALSE)





val = 50
qt <-subset(data, Type2 != "Voyageurs" & Quantité != -1 & Annee == 1936)
qt$Type3 <- paste(qt$Mouvement, qt$Type2)


nt <- data.frame(ID = c(), N = c())
for (i in qt$ID) {
  df <- subset(qt, ID==i)
  l <- df[1, "ligne_clean"]
  df <- aggregate(Quantité ~ Type3, df, sum)
  df <- subset(df, Quantité > val)
  nt <- rbind(nt, data.frame(c(i), c(nrow(df)), c(l), as.vector(df$Type3)))
}



ggplot(nt, aes(x = nt$c.nrow.df..)) + geom_density()


 







ggplot(temp, aes(x = Quantité, y = ligne_clean, fill = ligne_clean)) + 
  geom_density_ridges() +
  theme_ridges() +                                 # No color on backgroud
  theme(legend.position = "none",                  # No show legend
        axis.title.x = element_text(hjust = 0.5),  # x axis title in the center
        axis.title.y = element_text(hjust = 0.5))  # y axis title in the center


data_voyageurs <- subset(data, Annee == 1936 & Type2 == "Voyageurs" & ID != 33 & ID != 306) # j'enleve hanoi et saigon qui sont aberrants
data_voyageurs <- aggregate(Quantité ~ ID + ligne_clean, data_voyageurs, sum)

ggplot(data_voyageurs, aes(x = Quantité, y = ligne_clean, fill = ligne_clean)) + 
  geom_density_ridges() +
  theme_ridges() +                                 # No color on backgroud
  theme(legend.position = "none",                  # No show legend
        axis.title.x = element_text(hjust = 0.5),  # x axis title in the center
        axis.title.y = element_text(hjust = 0.5))  # y axis title in the center

data_fret <- subset(data, Annee == 1936 & Type2 != "Voyageurs" & ID != 33 & ID != 306 & Quantité != -1)
data_fret <- aggregate(Quantité ~ ID + ligne_clean, data_fret, sum)
ggplot(data_fret, aes(x = Quantité, y = ligne_clean, fill = ligne_clean)) + 
  geom_density_ridges() +
  theme_ridges() +                                 # No color on backgroud
  theme(legend.position = "none",                  # No show legend
        axis.title.x = element_text(hjust = 0.5),  # x axis title in the center
        axis.title.y = element_text(hjust = 0.5))  # y axis title in the center

data_fret2 <- subset(data, Type2 == "Voyageurs")




