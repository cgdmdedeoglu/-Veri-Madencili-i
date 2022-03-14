
#KNN UYGULAMASI

#önce veri seti çağırılır
veriler= read.table(file.choose(), header = T, sep = ";")

library(caret)
install.packages("cluster")
library(cluster)
View(veriler)
summary(veriler)
str(veriler)
attributes(veriler)

#VERİLER NUM. DÖNÜŞTÜRÜLDÜ
veriler$yas <- as.numeric(veriler$yas)
veriler$kreatinin_.fosfokinaz <- as.numeric(veriler$kreatinin_.fosfokinaz)
veriler$ejeksiyon_fraksiyonu <- as.numeric(veriler$ejeksiyon_fraksiyonu)
veriler$trombositler <- as.numeric(veriler$trombositler)
veriler$serum_sodyum <- as.numeric(veriler$serum_sodyum)
veriler$zaman <- as.numeric(veriler$zaman)
veriler$ölüm_olayı <- as.character(veriler$ölüm_olayı)

#sadece nüm. değerlerden oluşan alt küme
n_veriler <- veriler [,c(1,3,5,7,8,11,12)]

set.seed(1234)

ind <- sample(1:299,20)
veriler <- n_veriler[ind,]


model <- agnes(veriler,metric="manhattan", method="single") # manhattan uzakliga gore
modelo <- agnes (veriler, metric = "euclidien", method="single") #oklit uzakliga gore

#gorsellestirelim
pltree(model, main="en yakın komsu algoritması ile kumeleme")
pltree(modelo, main="en yakın komsu algoritması ile kumeleme")

#bu gorselde sayilar ile gosterim var, sinif etiketi seklinde gormek istersek

veriler$ölüm_olayı <- revalue(veriler$ölüm_olayı, c("0"="ölümyok","1"="ölümvar"))

pltree(model, main="en yakın komşu algoritması ile kümeleme", labels=veriler$ölüm_olayı)

bannerplot(agnes(veriler),main="Bannerplot Grafigi", labels = veriler$ölüm_olayı)

