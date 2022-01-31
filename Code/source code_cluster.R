#Rayhan Kurnia Yusda 
#F1E119066

#Panggil package factoextra, dan cluster
library(factoextra)#Algoritma klastering dan visualisasi
library(cluster)#Algoritma klastering

#Input Data 
data=read.csv(file.choose(), header = TRUE)
View(data)#View data
summary(data)#Ringkasan data
str(data)#Liat struktur data

#Praproses data
data_norm<-data[,-1]
View(data_norm)
row.names(data_norm)=data[,1]
View(data_norm)

#Menghingkan Missing Value
data_norm$X2016 = ifelse(is.na(data_norm$X2016),
                          ave(data_norm$X2016, FUN = function(x) mean(x, na.rm = TRUE)),
                          data_norm$X2016)
summary(data_norm)#ringkasan data/untuk mencek data

#Membuat standarisasi data
data.stds<-scale(data_norm)
View(data.stds)
str(data.stds)

#Proses Algoritma K-means Clustering
#Mempertimbangkan nilai k atau jumlah kluster yang optimal
#Dengan Elbow, Sillhoute dan gap statistic

#Metode elbow k=2
fviz_nbclust(data.stds,kmeans, method = "wss")

#Metode silhouette k=3
fviz_nbclust(data.stds, kmeans, method = "silhouette")

#Metode gap statistic k=3
gapstat <- clusGap(data.stds, FUN = kmeans, K.max=10, B = 47)
gapstat
fviz_nbclust(data.stds, kmeans, method = "gap")

#clustering data dengan menggunakan algoritma kmeans didaaptkan dengan K=3
clustering = kmeans(data.stds,3, nstart = 25)
clustering

#visualisasi data
fviz_cluster(clustering, data = data.stds)+ggtitle("Clustering K=3")#Visualisasi
final=data.frame(data_norm,clustering$cluster)
View(final)#View data telah diklaster
final


#Hasil Klaster dijadikan excel
#Ekraks Jadi Excel
install.packages("writexl")
library("writexl")
df<-final
write_xlsx(df,"D:\\df.xlsx")
