# Data-Science-in-Marketing-Customer-Segmentation
Customer Segmentation dengan program R
#DATA PREPARATION
#1. Membaca data csv dan dimasukkan ke variable pelanggan
pelanggan <- read.csv("https://academy.dqlab.id/dataset/customer_segments.txt",sep="\t")

#2. Konversi data kategori teks menjadi numeric dengan data.matrix
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])

#3. Menggabungkan hasil konversi dengan variable asal agar tidak kehilangan referensi
pelanggan <- data.frame(pelanggan, pelanggan_matrix)

#4. Menyederhanakan nilai belanja dari jutaan menjadi puluhan
pelanggan$NilaiBelanjaSetahun = pelanggan$NilaiBelanjaSetahun/1000000

#5. Membuat data master dengan meringkas data kategori dan numeric ke dalam variable
Profesi <- unique(pelanggan[c("Profesi","Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Residen <- unique(pelanggan[c("Tipe.Residen","Tipe.Residen.1")])

#Buat variable field_yang_digunakan dengan isi berupa vector "Jenis.Kelamin", "Umur" dan "Profesi"
field_yang_digunakan = c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1","NilaiBelanjaSetahun")


#MENENTUKAN JUMLAH CLUSTER TERBAIK
library(ggplot2)
#1. Simulasi jumlah cluster dan sum of squares (SS)
set.seed(100)
sse <- sapply(1:10, function(param_k){kmeans(pelanggan[field_yang_digunakan], param_k, nstart=25)$tot.withinss})
#Keterangan
#1. sse : nama variable yang akan digunakan untuk menyimpan nilai tot.withinss dari tiap objek kmeans
#2. sapply : merupakan function yang digunakan untuk menghasilkan vector dari iterasi (looping) dari eksekusi fungsi tertentu (pada kasus ini: kmeans) dengan nilai range yang diberikan
#3. 1:10 :range jumlah cluster dari 1 sampai dengan 10
#4. param_k : parameter yang akan berisi nilai 1 sampai dengan 10, sesuai range di atas
#5.  kmeans(pelanggan[field_yang_digunakan], param_k, nstart=25) : Fungsi kmeans yang dipanggil sebanyak nilai range 1 sampai dengan 10 (param_k) dari dataset pelanggan
#6. $tot.withinss : total penjumlahan dari tiap SS dari withinss

#2. Membuat grafik elbow effect/elbow method
jumlah_cluster_max <- 10
ssdata = data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) +
                geom_line(color="red") + geom_point() +
                ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") +
                geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:jumlah_cluster_max))


#CLUSTERING DENGAN FUNGSI kmeans
#fungsi kmeans untuk membentuk 5 cluster dengan 25 skenario random dan simpan ke dalam variable segmentasi
segmentasi <- kmeans(x=pelanggan[field_yang_digunakan], centers=5, nstart=25)

#keterangan hasil fungsi kmeans dimulai dari bagian paling atas ke bawah
#1. Ukuran/jumlah titik data pada tiap Cluster
#2. Nilai rata-rata (centroid) dari tiap Cluster
#3. Pembagian cluster dari tiap elemen data berdasarkan porsinya
#4. Jumlah jarak kuadrat dari tiap titik ke centroidnya
#5. komponen informasi lain yang terkandung di dalam objek kmeans
  #cluster : Vector dari cluster untuk tiap titik data
  #centers : Merupakan informasi titik centroid dari tiap cluster, seperti pada bagian "Analisa Hasil Cluster Means"
  #totss : Total Sum of Squares (SS) untuk seluruh titik data
  #withinss : Total Sum of Squares per cluster
  #tot.withinss : Total penjumlahan dari tiap SS dari withinss
  #betweenss : Perbedaan nilai antara totss dan tot.withinss
  #size : Jumlah titik data pada tiap cluster
  #iter : Jumlah iterasi luar yang digunakan oleh kmeans
  #ifault : Nilai integer yang menunjukkan indikator masalah pada algoritma


#PENGELOMPOKAN MODEL KMEANS
#1. Menamakan Segmen (Urutan cluster hasil program R dapat berubah)
Segmen.Pelanggan <- data.frame(cluster=c(1,2,3,4,5), Nama.Segmen=c("Silver Youth Gals", "Diamond Senior Member", "Gold Young Professional", "Diamond Professional", "Silver Mid Professional"))
#keterangan
  #Cluster 1 : Silver Youth Gals: alasannya adalah karena umurnya rata-rata adalah 20, wanita semua, profesinya bercampur antar pelajar dan professional serta pembelanjaan sekitar 6 juta.
  #Cluster 2 : Diamond Senior Member: alasannya adalah karena umurnya rata-rata adalah 61 tahun dan pembelanjaan di atas 8 juta.
  #Cluster 3 : Gold Young Professional: alasannya adalah karena umurnya rata-rata adalah 31 tahun, professional dan pembelanjaan cukup besar.
  #Cluster 4 : Diamond Profesional: alasannya adalah karena umurnya rata-rata adalah 42 tahun, pembelanjaan paling tinggi dan semuanya professional.
  #Cluster 5 : Silver Mid Professional: alasannya adalah karena umurnya rata-rata adalah 52 tahun dan pembelanjaan sekitar 6 juta.

#2. #Menggabungkan seluruh aset ke dalam variable Identitas.Cluster
Identitas.Cluster <- list(Profesi=Profesi, Jenis.Kelamin=Jenis.Kelamin, Tipe.Residen=Tipe.Residen, Segmentasi=segmentasi, Segmen.Pelanggan=Segmen.Pelanggan, field_yang_digunakan=field_yang_digunakan)

#3. Menyimpan objek dalam bentuk file untuk mengalokasikan data baru ke segmen yang sesuai
saveRDS(Identitas.Cluster,"cluster.rds")


#MENGOPERASIKAN MODEL KMEANS PADA KASUS
#1. memasukkan data baru
databaru <- data.frame(Customer_ID="CUST-100", Nama.Pelanggan="Rudi Wilamar",Umur=32,Jenis.Kelamin="Wanita",Profesi="Pelajar",Tipe.Residen="Cluster",NilaiBelanjaSetahun=3.5)

#2. Membuat objek clustering dari file
Identitas.Cluster <- readRDS(file="cluster.rds")

#3. Merge databaru dengan data referensi
databaru <- merge(databaru, Identitas.Cluster$Profesi)
databaru <- merge(databaru, Identitas.Cluster$Jenis.Kelamin)
databaru <- merge(databaru, Identitas.Cluster$Tipe.Residen)

#4. Menentukan cluster databaru
Identitas.Cluster$Segmen.Pelanggan[which.min(sapply( 1:5, function( x ) sum( ( databaru[Identitas.Cluster$field_yang_digunakan] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) )),]

