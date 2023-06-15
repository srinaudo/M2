qt <-subset(data, Type2 != "Voyageurs" & Quantité != -1 & Annee == 1936)
qt$Type3 <- paste(qt$Mouvement, qt$Type2)

detail <- aggregate(Quantité ~ ID + Type3, qt, sum)
total <- aggregate(Quantité ~ ID, qt, sum)

total$decile <- ntile(total$Quantité, 4)

test <- merge(detail, total, by = "ID")

prem_quar <- subset(test, decile==1)

ggplot(prem_quar, aes(x=Type3, y=Quantité.x)) + geom_bar(stat="sum")

ggplot(data = prem_quar, aes(x = Type3, by = Quantité.x)) +
  geom_bar(position = position_dodge(), stat = "prop")