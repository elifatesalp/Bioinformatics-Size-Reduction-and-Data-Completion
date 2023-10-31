#VERİ TAMAMLAMA VE BOYUT AZALTMA

#NCBI GDS 3233 – SERVİCAL CANCER DATA SET
#Veri okumaları ve veri özellikleri;

library(GEOquery)
gds=getGEO("GDS3233")#Veri kümesini geçici olarak tutmak için getGEO kullanılır.
eset=GDS2eSet(gds,do.log2 = TRUE)#Veri analiz için eset nesnesine gönderilir.
eset #Veriye ait tüm özellikler
dim(eset) #Verinin içeriğini yani kaç örnek olduğunu görüntülenir

pVeri=pData(eset) 
summary(pVeri)
veri[2:6,2:6]
kayip=nrow(which(is.na(exprs(eset)),arr.ind = TRUE))
show(kayip) 
veri[15,3090] #Veride herhangi bir satırı görüntüledik. 

library(impute)
yeni_veri<-impute::impute.knn(as.matrix(veri))
veri=yeni_veri$data
veri[15,3090] #Böylece az önce NA olan değerin KNN ile dolduğu görülmektedir.

#Veri Boyutu Azaltma 
veri1=(exprs(eset)) #Mevcut veriye matris’e dönüştürmek için exprs() kullanılır.
head(veri1)
veri1[1:4,1:4]
veri=t(exprs(eset)) #Verinin transposesi alınır
veri[1:4,1:4]

modelpca=prcomp(veri[1:4,1:3]) #PCA uygulanarak verinin boyutu azaltılır
summary(modelpca)
plot(modelpca,type="l", main="Temel bileşenler") #Plotta gösterimi





#########################################################################################################
#UCI – HEART DİSEASE DATA SET

url="https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.switzerland.data"
okunan=read.table(url, sep=",", na.strings = "?")
head(okunan)

library(VIM)#Kayıp verileri bulmak için VIM kütüphanesi kullanılır.
aggr(okunan, prop =F, numbers = T)

library(randomForest)
set.seed(222)#üretilecek sayı sabitlenmesi için 
okunan.imputed=rfImpute(V1~.,okunan)
head(okunan.imputed)

#PCA ile  veri boyutu azaltma
numerical_okunan <- okunan.imputed[,1:7] 
head(numerical_okunan)
normalized_okunan <- scale(numerical_okunan) #Data 0-1 arası normalize
head(normalized_okunan)
corr_matrix <- cor(normalized_okunan) #Korelasyon hesaplanır

okunan.pca <- princomp(corr_matrix) #PCA hesaplanır
summary(okunan.pca)
fviz_pca_var(okunan.pca, col.var = "cos2",
 gradient.cols = c("black", "orange", "green"),
 repel = TRUE)
