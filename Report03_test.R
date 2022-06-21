rm(list = ls())
X<-read.csv("/Users/jesshsieh/Desktop/庭's stuff/R/dataReport03_測試集.csv" , header = TRUE ,check.names = F, sep = "," , stringsAsFactors = FALSE,fileEncoding = "BIG-5")
#View(X)
#資料量太大，以隨機抽樣解決電腦無法容納的問題
N <- nrow(X)
set.seed(4)
s <- sample(1:N,N*0.2)
X <- X[s,]
Y <- X[,1] #將酒名獨立出來
X <- X[,-1] #剩下是分群屬性變數

#歐式距離、華德法且分三群
Mdis<-dist(X, method="manhattan")
#plot(Mdis_w,main="曼哈頓距離、ward.D2")
Mdis_w1<-hclust(Mdis,method="ward.D2")
Mdis_w1_cut<-cutree(Mdis_w1, k=3)
matrix_Mdis_w1_cut<-table(Mdis_w1_cut,Y)
CR_Mdis_w1<-sum(diag(matrix_Mdis_w1_cut))/sum(matrix_Mdis_w1_cut)
CR_Mdis_w1

#曼哈頓距離、華德法且分三群
Edis<-dist(X, method="euclidean")
#plot(Mdis_w,main="曼哈頓距離、ward.D2")
Edis_w1<-hclust(Edis,method="ward.D2")
Edis_w1_cut<-cutree(Edis_w1, k=3)
matrix_Edis_w1_cut<-table(Edis_w1_cut,Y)
CR_Edis_w1<-sum(diag(matrix_Edis_w1_cut))/sum(matrix_Edis_w1_cut)
CR_Edis_w1

cat("歐式距離、華德法且分三群正確率為：",CR_Mdis_w1,"\n")
cat("曼哈頓距離、華德法且分三群正確率為：",CR_Edis_w1)
