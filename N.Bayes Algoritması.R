
 
 #naviebayesss
#veri seti çağırılır
veriler = read.table(file.choose(),header = T,sep = ";")

#modelin çalışması için aşağıdaki veriler kurulur.
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


data(veriler)
View(veriler)
str(veriler)
summary(veriler)
attributes(veriler)

#veriler nümerik ve faktör olarak tanımlanır.
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

#revalue komutunun çalışması için plyr kullanılır.
install.packages("plyr")
library(plyr)


#anemi değişkeninde 1=anemivar,0=anemiyok şeklinde tanımlanır.
veriler$anemi <- revalue(veriler$anemi, c("1"="anemivar","0"="anemiyok"))

#cinsiyet değişkeni 0=erkek 1=kadın olarak tanımlanır.
veriler$cinsiyet <- revalue(veriler$cinsiyet, c("0"="erkek","1"="kadın"))

#diyabet değişkeni 1=yok 0= var şeklinde tanımlanır.
veriler$diyabet <- revalue(veriler$diyabet, c("1"="yok","0"="var"))

#yüksek kan basıncı değişkeni 0=yok 1=var
veriler$yüksek_kan_basıncı <- revalue(veriler$yüksek_kan_basıncı, c("0"="yok","1"="var"))

#sigara değişkeni 0=içiyor 1=içmiyor şeklinde tanımlanır.
veriler$sigara <- revalue(veriler$sigara, c("0"="içiyor","1"="içmiyor"))

#ölüm olayı değişkeni 0=ölümyok 1=ölümvar şeklinde tanımlanmıştır.
veriler$ölüm_olayı <- revalue(veriler$ölüm_olayı, c("0"="ölümyok","1"="ölümvar"))

set.seed(1)
verisetibolme <- createDataPartition(y=veriler$ölüm_olayı, p=0.6, list=FALSE)

#veri setini egitim ve test olarak rastgele ikiye ayiracagiz
egitim <- veriler[verisetibolme,]
test <- veriler[-verisetibolme,]

# Eğitim ve test veri setine tahmininde kullanılacak nitelik ve hedef nitelik(ölüm_olayı) atanır. 
#Ölüm_olayı 12. Sütunda olduğu için 12 kullanıldı.

testNitelikleri <- test[,-12]
testHedefNitelik <- test[[12]]
egitimNitelikleri <- egitim [,-12]
egitimHedefNitelik <- egitim [[12]]

# Naive bayes için e1071 paketi çağrıldı. Bu paketteki naiveBayes() fonksiyonu kullanıldı.
library(e1071)
naiveBayes_modeli_kuruldu <- naiveBayes(egitimNitelikleri, egitimHedefNitelik)
naiveBayes_modeli_kuruldu

#modelin tahminleri bulunur.
(tahminiSiniflar <- predict(naiveBayes_modeli_kuruldu, testNitelikleri))

#gerçek sınıflar ile tahmini sınıflar kıyaslanır.
(karisiklikmatrisi <- table(tahminiSiniflar, testHedefNitelik, dnn =c ("Tahmini Siniflar", "Gercek Siniflar")))

(TP <- karisiklikmatrisi [1])
(FP <- karisiklikmatrisi [3])
(FN <- karisiklikmatrisi [2])
(TN <- karisiklikmatrisi [4])

#performans değerlendirme ölçütleri hesaplanır.
paste0("Dogruluk = ",(Dogruluk <- (TP+TN)/sum(karisiklikmatrisi)))
paste0("Hata = ",(Hata <- 1-Dogruluk))
