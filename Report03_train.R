rm(list = ls())

####A.
X<-read.csv("/Users/jesshsieh/Desktop/庭's stuff/R/dataReport03_訓練集.csv" , header = TRUE ,check.names = F, sep = "," , stringsAsFactors = FALSE,fileEncoding = "BIG-5")
#View(X)
#資料量太大，以隨機抽樣解決電腦無法容納的問題
N <- nrow(X)
set.seed(1)
s <- sample(1:N,N*0.2)
X <- X[s,]
Y <- X[,1] #將酒名獨立出來
X <- X[,-1] #剩下是分群屬性變數

###歐式距離
## (1) 歐式距離、單一連結法 ----
Edis<-dist(X,method="euclidean") 
Edis_s<-hclust(Edis,method="single")   
#plot(Edis_s,main="歐式距離、single")

w<-1:10
CR_Edis_s <-rep(0,length(w))
names(CR_Edis_s)<-w
a<-0
for(j in w)
{a<-a+1
Edis_s_cut<-cutree(Edis_s, k=j)
matrix_Edis_s_cut<-table(Edis_s_cut,Y)
CR_Edis_s[a]<-sum(diag(matrix_Edis_s_cut))/sum(matrix_Edis_s_cut)}
CR_Edis_s


## (2) 歐式距離、完全連結法 ----
Edis_c<-hclust(Edis,method="complete")
#plot(Edis_c,main="歐式距離、complete")

w<-1:10
CR_Edis_c<-rep(0,length(w))
names(CR_Edis_c)<-w
a<-0
for(j in w)
{a<-a+1
Edis_c_cut<-cutree(Edis_c, k=j)
matrix_Edis_c_cut<-table(Edis_c_cut,Y)
CR_Edis_c[a]<-sum(diag(matrix_Edis_c_cut))/sum(matrix_Edis_c_cut)}
CR_Edis_c


## (3) 歐式距離、平均連結法 ----
Edis_a<-hclust(Edis,method="average") 
#plot(Edis_a,main="歐式距離、average")

w<-1:10
CR_Edis_a<-rep(0,length(w))
names(CR_Edis_a)<-w
a<-0
for(j in w)
{a<-a+1
Edis_a_cut<-cutree(Edis_a, k=j)
matrix_Edis_a_cut<-table(Edis_a_cut,Y)
CR_Edis_a[a]<-sum(diag(matrix_Edis_a_cut))/sum(matrix_Edis_a_cut)}
CR_Edis_a

## (4) 歐式距離、中心法 ----
Edis_cen<-hclust(Edis,method="centroid") 
#plot(Edis_cen,main="歐式距離、centroid")

w<-1:10
CR_Edis_cen<-rep(0,length(w))
names(CR_Edis_cen)<-w
a<-0
for(j in w)
{a<-a+1
Edis_cen_cut<-cutree(Edis_cen, k=j)
matrix_Edis_cen_cut<-table(Edis_cen_cut,Y)
CR_Edis_cen[a]<-sum(diag(matrix_Edis_cen_cut))/sum(matrix_Edis_cen_cut)}
CR_Edis_cen


## (5) 歐式距離、華德法 ----
Edis_w<-hclust(Edis,method="ward.D2") 
#plot(Edis_w,main="歐式距離、ward.D2")

w<-1:10
CR_Edis_w<-rep(0,length(w))
names(CR_Edis_w)<-w
a<-0
for(j in w)
{a<-a+1
Edis_w_cut<-cutree(Edis_w, k=j)
matrix_Edis_w_cut<-table(Edis_w_cut,Y)
CR_Edis_w[a]<-sum(diag(matrix_Edis_w_cut))/sum(matrix_Edis_w_cut)}
CR_Edis_w


###曼哈頓距離
## (1)曼哈頓距離、單一連結法----
Mdis<-dist(X, method="manhattan") 
Mdis_s<-hclust(Mdis,method="single")  
#plot(Mdis_s,main="曼哈頓距離、single")

## 與實際資料比較
w<-1:10
CR_Mdis_s<-rep(0,length(w))
names(CR_Mdis_s)<-w
a<-0
for(j in w)
{a<-a+1
Mdis_s_cut<-cutree(Mdis_s, k=j)
matrix_Mdis_s_cut<-table(Mdis_s_cut,Y)
CR_Mdis_s[a]<-sum(diag(matrix_Mdis_s_cut))/sum(matrix_Mdis_s_cut)}
CR_Mdis_s



## (2)曼哈頓距離、完全連結法----
Mdis_c<-hclust(Mdis,method="complete") 
#plot(Mdis_c,main="曼哈頓距離、complete")

w<-1:10
CR_Mdis_c<-rep(0,length(w))
names(CR_Mdis_c)<-w
a<-0
for(j in w)
{a<-a+1
Mdis_c_cut<-cutree(Mdis_c, k=j)
matrix_Mdis_c_cut<-table(Mdis_c_cut,Y)
CR_Mdis_c[a]<-sum(diag(matrix_Mdis_c_cut))/sum(matrix_Mdis_c_cut)}
CR_Mdis_c



## (3)曼哈頓距離、平均連結法----
Mdis_a<-hclust(Mdis,method="average") 
#plot(Mdis_a,main="曼哈頓距離、average")

w<-1:10
CR_Mdis_a<-rep(0,length(w))
names(CR_Mdis_a)<-w
a<-0
for(j in w)
{a<-a+1
Mdis_a_cut<-cutree(Mdis_a, k=j)
matrix_Mdis_a_cut<-table(Mdis_a_cut,Y)
CR_Mdis_a[a]<-sum(diag(matrix_Mdis_a_cut))/sum(matrix_Mdis_a_cut)}
CR_Mdis_a


## (4)曼哈頓距離、中心法----
Mdis_cen<-hclust(Mdis,method="centroid")
#plot(Mdis_cen,main="曼哈頓距離、centroid")

w<-1:10
CR_Mdis_cen<-rep(0,length(w))
names(CR_Mdis_cen)<-w
a<-0
for(j in w)
{a<-a+1
Mdis_cen_cut<-cutree(Mdis_cen, k=j)
matrix_Mdis_cen_cut<-table(Mdis_cen_cut,Y)
CR_Mdis_cen[a]<-sum(diag(matrix_Mdis_cen_cut))/sum(matrix_Mdis_cen_cut)}
CR_Mdis_cen


## (5)曼哈頓距離、華德法----
Mdis_w<-hclust(Mdis,method="ward.D2")  
#plot(Mdis_w,main="曼哈頓距離、ward.D2")

w<-1:10
CR_Mdis_w<-rep(0,length(w))
names(CR_Mdis_w)<-w
a<-0
for(j in w)
{a<-a+1
Mdis_w_cut<-cutree(Mdis_w, k=j)
matrix_Mdis_w_cut<-table(Mdis_w_cut,Y)
CR_Mdis_w[a]<-sum(diag(matrix_Mdis_w_cut))/sum(matrix_Mdis_w_cut)}
CR_Mdis_w



CR_table <- list()
CR_table[1] <- max(CR_Edis_s)
CR_table[2] <- max(CR_Edis_c)
CR_table[3] <- max(CR_Edis_a)
CR_table[4] <- max(CR_Edis_cen)
CR_table[5] <- max(CR_Edis_w)

CR_table[6] <- as.numeric(names(CR_Edis_s[CR_Edis_s == max(CR_Edis_s)]))
CR_table[7] <- as.numeric(names(CR_Edis_c[CR_Edis_c == max(CR_Edis_c)]))
CR_table[8] <- as.numeric(names(CR_Edis_a[CR_Edis_a == max(CR_Edis_a)]))
CR_table[9] <- as.numeric(names(CR_Edis_cen[CR_Edis_cen == max(CR_Edis_cen)]))
CR_table[10] <- as.numeric(names(CR_Edis_w[CR_Edis_w == max(CR_Edis_w)]))

CR_table[11] <- max(CR_Mdis_s)
CR_table[12] <- max(CR_Mdis_c)
CR_table[13] <- max(CR_Mdis_a)
CR_table[14] <- max(CR_Mdis_cen)
CR_table[15] <- max(CR_Mdis_w)

CR_table[16] <- as.numeric(names(CR_Mdis_s[CR_Mdis_s == max(CR_Mdis_s)]))
CR_table[17] <- as.numeric(names(CR_Mdis_c[CR_Mdis_c == max(CR_Mdis_c)]))
CR_table[18] <- as.numeric(names(CR_Mdis_a[CR_Mdis_a == max(CR_Mdis_a)]))
CR_table[19] <- as.numeric(names(CR_Mdis_cen[CR_Mdis_cen == max(CR_Mdis_cen)]))
CR_table[20] <- as.numeric(names(CR_Mdis_w[CR_Mdis_w == max(CR_Mdis_w)]))

CR_table <- matrix(CR_table,nrow = 4,byrow = TRUE)
colnames(CR_table) <- c("單一連結法","完全連結法","平均連結法","中心法","華德法")
rownames(CR_table) <- c("歐式距離正確率","歐式距離最佳分法","曼哈頓距離正確率","曼哈頓距離最佳分法")
CR_table

#從CR_table得出使用歐式距離或是曼哈頓距離，搭配華德法且分三群的正確率是最大的



#### B.導入測試集資料
X1<-read.csv("/Users/jesshsieh/Desktop/庭's stuff/R/dataReport03_測試集.csv" , header = TRUE ,check.names = F, sep = "," , stringsAsFactors = FALSE,fileEncoding = "BIG-5")
#View(X)
#資料量太大，以隨機抽樣解決電腦無法容納的問題
N1 <- nrow(X1)
set.seed(4)
s1 <- sample(1:N1,N1*0.2)
X1 <- X1[s1,]
Y1 <- X1[,1] #將酒名獨立出來
X1 <- X1[,-1] #剩下是分群屬性變數

#曼哈頓距離、華德法且分三群
Mdis<-dist(X, method="manhattan")
#plot(Mdis_w,main="曼哈頓距離、ward.D2")
Mdis_w1<-hclust(Mdis,method="ward.D2")
Mdis_w1_cut<-cutree(Mdis_w1, k=3)
matrix_Mdis_w1_cut<-table(Mdis_w1_cut,Y)
CR_Mdis_w1<-sum(diag(matrix_Mdis_w1_cut))/sum(matrix_Mdis_w1_cut)
CR_Mdis_w1

#歐式距離、華德法且分三群
Edis<-dist(X, method="euclidean")
#plot(Mdis_w,main="曼哈頓距離、ward.D2")
Edis_w1<-hclust(Edis,method="ward.D2")
Edis_w1_cut<-cutree(Edis_w1, k=3)
matrix_Edis_w1_cut<-table(Edis_w1_cut,Y)
CR_Edis_w1<-sum(diag(matrix_Edis_w1_cut))/sum(matrix_Edis_w1_cut)
CR_Edis_w1

cat("歐式距離、華德法且分三群正確率為：",CR_Mdis_w1,"\n")
cat("曼哈頓距離、華德法且分三群正確率為：",CR_Edis_w1)

