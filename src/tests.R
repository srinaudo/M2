data_v <- read.csv("./data/data_voyageurs.csv")
data2<-subset(data_v, Ligne == "total")
data2
plot(data2)

#diagramme de corélation
data3 <- read.csv("./data/data_voyageurs2.csv", encoding ="UTF-8", sep=";", dec=",")
library(corrplot)
corrplot(cor(data3))

#régression linéraire --> le résultat est intéressant
reg<-lm(formula = Recettes_123 ~ KM_total_123, data = data3)
plot(Recettes_123 ~ KM_total_123, data=data3)
abline(reg, col = "red")
#les recettes augmentent de manière proportionnelle au kilométrage. 

plot(Recettes_4 ~ KM_total_4, data=data3)
abline(lm(Recettes_4 ~ KM_total_4, data=data3))

plot(Total_123 ~ KM_moyen_123, data3)
abline(lm(Total_123 ~ KM_moyen_123, data=data3))



dlibrary(ggplot2)
#OH LUI IL EST ZINZIN
p1 <- ggplot(data=data3, aes(x=Annee, y=Total_123/1000)) +
  geom_line() +
  theme_bw()
p1

p2 <- ggplot(data=data3, aes(x=Annee, y=KM_total_123/1000000)) +
  geom_point() +
  theme_bw()
p2

p3 <- ggplot(data=data3, aes(x=Annee, y=Recettes_123)) +
  geom_line() +
  theme_bw()
p3

data3$part_recettes123 = data3$Recettes_123 * 100 / (data3$Recettes_123 + data3$Recettes_4)

#graph à recadrer sur 0
ggplot(data3, aes(x = Annee, y = part_recettes123)) + geom_line() + geom_point()

data3$part_voy_123 = data3$Total_123 * 100 / (data$Total_123 + data3$Total_4)
ggplot(data3, aes(x = Annee, y = part_voy_123)) + geom_line() + geom_point()



#indice de contribution au recette par km parcouru
data3$pcrkmp4 <- (data3$Recettes_4/data3$KM_total_4) *100 / (data3$Recettes_123 + data3$Recettes_4)
data3$pcrkmp123 <- (data3$Recettes_123/data3$KM_total_123) *100 / (data3$Recettes_123 + data3$Recettes_4)


ggplot(data3) + geom_line(aes(x=Annee, y=pcrkmp4*10000000)) + geom_line(aes(x=Annee, y=pcrkmp123*10000000), col="red")

plot(pcrkmp4 ~ Annee, data3)
abline(lm(pcrkmp4 ~ Annee, data3))


#indice de contrib aux recettes par voyageur
data3$pcrv4 <- (data3$Recettes_4/data3$Total_4) *100 / (data3$Recettes_123 + data3$Recettes_4)

ggplot(data3, aes(x=Annee, y=pcrv4*100000)) + geom_line()

#baisse effective du coût des billets ?
plot(pcrv4 ~ Annee, data3)
abline(lm(pcrv4 ~ Annee, data3))

plot(pcrv4 ~ Total_4, data3)
reg <- lm(pcrv4 ~ Total_4, data3)
abline(reg, col="red")

ggplot(data3, aes(y=pcrv4*1000000, x=Total_4/1000000)) + geom_line() +  geom_smooth(method='lm') + theme_minimal() + xlab("voyageurs (millions)") + ylab("indice de prix du billet")
ggplot(data3, aes(y=pcrkmp4*1000000, x=Total_4/1000000)) + geom_line() + geom_point() +  geom_smooth(method='lm') + theme_classic() + xlab("voyageurs (millions)") + ylab("indice de prix du billet")

