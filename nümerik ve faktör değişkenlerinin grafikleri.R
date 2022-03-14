

veriler= read.table(file.choose(), header = T, sep = ";")

#nümerik değişkenlerin grafikleri çizilir.

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

#kategorik değişkenler bu şekilde tanımlanır.
veriler$anemi <- revalue(veriler$anemi, c("1"="anemivar","0"="anemiyok"))
veriler$cinsiyet <- revalue(veriler$cinsiyet, c("0"="erkek","1"="kadın"))
veriler$diyabet <- revalue(veriler$diyabet, c("1"="yok","0"="var"))
veriler$yüksek_kan_basıncı <- revalue(veriler$yüksek_kan_basıncı, c("0"="yok","1"="var"))
veriler$sigara <- revalue(veriler$sigara, c("0"="içiyor","1"="içmiyor"))
veriler$ölüm_olayı <- revalue(veriler$ölüm_olayı, c("0"="ölümyok","1"="ölümvar"))

str (veriler)

#nümerik değişkenler için Histogram ve kutu Grafikleri çizilir.
#Nümerik değişkenlerin grafikleri çizilir.

hist(veriler$yas, col="purple", main = "Yaş Histogram Grafiği")
hist(veriler$ kreatinin_.fosfokinaz, col="purple", main = "Kreatinin Fosfokinaz Histogram Grafiği")
hist(veriler$ ejeksiyon_fraksiyonu, col="purple", main = "Ejeksiyon Fraksiyonu Histogram Grafiği")
hist(veriler$ trombositler, col="purple", main = "Trombositler Histogram Grafiği")
hist(veriler$ serum_sodyum, col="purple", main = "Serum Sodyum Histogram Grafiği")
hist(veriler$  zaman, col="purple", main = "Zaman Histogram Grafiği")
 
 
 
 
 #Anemi için grafik çizimi>
frekansanemi <- table(veriler$anemi)
barplot(frekansanemi, col="purple" , main="Anemi Dağılımları Grafiği",xlab="Anemi Sayısı",ylab = "Anemi" , horiz = TRUE)
 #Cinsiyet için grafik çizimi>
frekanscinsiyet <- table(veriler$cinsiyet)
barplot(frekanscinsiyet, col="purple" , main="Cinsiyet Dağılımları Grafiği",xlab="Cinsiyet Sayısı",ylab = "Anemi" , horiz = TRUE)
 #Diyabet için grafik çizimi>
frekansdiyabet <- table(veriler$diyabet)
barplot(frekansdiyabet, col="purple" , main="Diyabet Dağılımları Grafiği",xlab="DiyabetSayısı",ylab = "Diyabet" , horiz = TRUE)
 #Yüksek_kan_basıncı için grafik çizimi>
Frekansyüksek_kan_basıncı <- table(veriler$yüksek_kan_basıncı)
barplot(frekansyüksek-kan_basıncı, col="purple" , main="Yüksek Kan Basıncı Dağılımları Grafiği",xlab="Yüksek Kan Basıncı Sayısı",ylab = "Yüksek Kan Basıncı" , horiz = TRUE)
 #Sıgara için grafik çizimi>
frekanssigara <- table(veriler$sigara)
barplot(frekanssigara, col="purple" , main="sigara Dağılımları Grafiği",xlab="Sıgara Kullanım Sayısı",ylab = "Sigara" , horiz = TRUE)
 #Ölüm Olayı için grafik çizimi>
 frekansÖlüm_olayı <- table(veriler$ölüm_olayı)
  barplot(frekansölüm_olayı, col="purple" , main="Ölüm Olayı Dağılımları Grafiği",xlab="Ölüm Sayısı",ylab = "Ölüm Olayı" , horiz = TRUE)
 
 #kutu grafikleri çizimi 
  boxplot(veriler$yas, col=" yellow ", main="Yas Kutu Grafiği") 
  boxplot(veriler$ kreatinin_.fosfokinaz, col=" yellow ", main="kreatinin Fosfokinaz Kutu Grafiği")
  boxplot(veriler$ejeksiyon_fraksiyonu, col=" yellow ", main="Ejeksiyon Fraksiyonu Kutu Grafiği") 
  boxplot(veriler$trombositler, col=" yellow ", main="Trombositler Kutu Grafiği")
  boxplot(veriler$serum_sodyum, col=" yellow ", main="Serum Sodyum Kutu Grafiği")
  boxplot(veriler$zaman, col="yellow", main="Zaman Kutu Grafiği") 
 
 #serpilme diyagramı çizimi 
 > pairs( ~ yas+ zaman+ serum_sodyum + trombositler + ejeksiyon_fraksiyonu + kreatinin_fosfokinaz , data= veriler, col=" dark blue", main= "Serpilme Diyagramları")
 
 
 
 

hist(veriler$trombositler, col="purple", main = "Trombositler Histogram Grafiği")

boxplot(veriler$trombositler, col="yellow", main="Trombositler Kutu Grafiği")


#kategorik değişkenler için dağılım grafikleri çizilir.
frekansölüm_olayı <- table(veriler$ölüm_olayı)
barplot(frekansölüm_olayı, col="purple" , main="Ölüm Olayı Dağılımları 
Grafiği",xlab="Ölüm Sayısı",ylab = ",ölüm olayı" , horiz = TRUE)

pairs( ~ trombositler+ zaman + serum_sodyum + ejeksiyon_fraksiyonu + kreatinin_.fosfokinaz + yas , data= veriler, 
       col=" dark blue", main= "Serpilme Diyagramları")


