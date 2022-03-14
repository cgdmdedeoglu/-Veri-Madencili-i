
# knn dpğruluk oranı içinkarar ağacına uyarlanmıştır.Sadece nümerik değerlerden oluşmaktadır.

veriler= read.table(file.choose(), header = T, sep = ";")
install.packages("caret")
library(caret)
install.packages("cluster")
library(cluster)
View(veriler)
summary(veriler)
str(veriler)
attributes(veriler)

#VERİLER nümerik ve faktör olarak tanımlanır
veriler$yas <- as.numeric(veriler$yas)
veriler$anemi <- as.factor(veriler$anemi)
veriler$kreatinin_.fosfokinaz <- as.numeric(veriler$kreatinin_.fosfokinaz)
veriler$diyabet <- as.factor(veriler$diyabet)
veriler$ejeksiyon_fraksiyonu <- as.numeric(veriler$ejeksiyon_fraksiyonu)
veriler$yüksek_kan_basıncı <- as.factor(veriler$yüksek_kan_basıncı)
veriler$trombositler <- as.numeric(veriler$trombositler)
veriler$serum_sodyum <- as.numeric(veriler$serum_sodyum)
veriler$cinsiyet <- as.factor(veriler$cinsiyet)
veriler$sigara <- as.factor(veriler$sigara)
veriler$zaman <- as.numeric(veriler$zaman)
veriler$ölüm_olayı <- as.factor(veriler$ölüm_olayı)

veriler$ölüm_olayı <- revalue(veriler$ölüm_olayı, c("0"="ölümyok","1"="ölümvar"))

#sadece nümerik  değerlerden oluşan alt küme oluşturuldu ve nümerik değerlere karşılık gelen
#verilerin değerleri sayısal olarak girildi.
n_veriler <- veriler [,c(1,3,5,7,8,11,12)]

#rastgele veri seçimi için set.seed kullanılır.
set.seed(1234)

ind <- sample(1:299,299)
veriler <- n_veriler[ind,]


modelC11 <- J48(ölüm_olayı~.,data = veriler)

#kurallari gorelim
print(modelC11)

summary(modelC11)

#grafigini cizelim
plot(modelC11)




