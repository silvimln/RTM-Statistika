mydata <- read_excel("F:/Kuliah/Semester 3/Statistika/penilaian.xlsx")

#sort
sort(mydata$Nilai) #mengurutkan data dari terkecil

n = length(mydata$Nilai) #banyak data
n

max(mydata$Nilai) #data paling besar
min(mydata$Nilai) #data paling kecil

#jangkauan
jangkauan<- max(mydata$Nilai)-min(mydata$Nilai)
jangkauan

#menentukan jumlah kelas
K = 1+(3.3*log(90, base=10))
K
k = round(K)
k

#interval
interval <- jangkauan/k
interval
interval <- round(interval)
interval

#Menghitung Frekuensi Setiap Kelas
frek<-function (x,maks,min)
{a=0
for (i in 1 : length(x)){
if (x[i]>=maks && x[i]<=min){
a<- a + 1}}
print(a)}
frek(mydata$Nilai,10,22)
frek(mydata$Nilai,23,35)
frek(mydata$Nilai,36,48)
frek(mydata$Nilai,49,61)
frek(mydata$Nilai,62,74)
frek(mydata$Nilai,75,87)
frek(mydata$Nilai,88,100)

#membuat tabel
Interval <- c("10 - 22", "23 - 35", "36 - 48", "49 - 61", "62 - 74", "75 - 87", "88 -100")
NilaiTengah <- c(16,29,42,55,68,81,94)
frekuensi <- c(5,5,1,1,3,44,149)
fixi <- NilaiTengah*frekuensi
tabel <- data.frame(Interval,NilaiTengah,frekuensi,fixi)
tabel 

#1.Mean
Mean <-sum(tabel$fixi)/sum(tabel$frekuensi)
Mean

#2.Median
kelasmedian<-length(mydata$Nilai)/2
kelasmedian

tbm<- 87.5  #tepi bawah kelas median
fkm<- 59  #frekuensi kumulatif kelas sebelum kelas median
fm<- 149 #frekuensi kelas median
median<-tbm+interval*((kelasmedian-fkm)/fm)
median

#3. Modus
tbm<-87.5
d1<-104
d2<-149
modus<-tbm+interval*(d1/(d1+d2))
modus

#4. Range
terakhir<- 94 #nilai tengah terakhir
pertama<- 16 #nilai tengah pertama
R <- terakhir - pertama
R

#5. Mean Deviasi
SR  <- 1/sum(frekuensi)*sum(frekuensi*abs(NilaiTengah-Mean))
SR

#6. Standar Deviasi
varian <- 1/sum(frekuensi)*sum(frekuensi*(NilaiTengah-Mean)^2)
varian
stdev<-sqrt(varian)
stdev
