# Mengimport data
library(readxl)
data <- read_excel("Analisis Faktor-Faktor yang Mempengaruhi Tingkat Stres Mahasiswa Matematika UIN Syarif Hidayatullah Jakarta dengan Pendekatan Regresi Logistik.xlsx")
View(data)

# Mengubah tipe data sesuai kebutuhan
data$angkatan <- factor(data$angkatan, ordered = TRUE, levels = c(2023, 2022, 2021))
data$jenis_kelamin <- factor(data$jenis_kelamin)
data$tempat_tinggal <- factor(data$tempat_tinggal)
data$penghasilan_orang_tua <- factor(data$penghasilan_orang_tua, ordered = TRUE, levels = c("< Rp1.000.000", "Rp1.000.000-Rp3.000.000", "Rp3.000.000-Rp5.000.000", "> Rp5.000.000"))
data$dukungan_orang_tua_dalam_perkuliahan <- as.numeric(data$dukungan_orang_tua_dalam_perkuliahan)
data$durasi_tidur <- as.numeric(data$durasi_tidur)
data$IPK <- as.numeric(data$IPK)
data$durasi_belajar_diluar_perkuliahan <- as.numeric(data$durasi_belajar_diluar_perkuliahan)
data$beban_tugas <- factor(data$beban_tugas, ordered = TRUE, levels = c("Sedikit", "Sedang", "Banyak"))
data$keterlibatan_kegiatan_non_akademik <- factor(data$keterlibatan_kegiatan_non_akademik)
data$bekerja_sambil_kuliah <- factor(data$bekerja_sambil_kuliah)
data$merasa_salah_jurusan <- factor(data$merasa_salah_jurusan)
data$memiliki_trauma <- factor(data$memiliki_trauma)
data$memiliki_masalah_kesehatan_mental <- factor(data$memiliki_masalah_kesehatan_mental)
data$tingkat_stres <- factor(data$tingkat_stres, ordered = TRUE, levels = c("Rendah", "Sedang", "Tinggi"))

# Statistik Deskriptif
summary(data)

# Membuat diagram batang untuk variabel bertipe data kategorik
library(ggplot2) 
ggplot(data, aes(x = angkatan, fill = angkatan)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Angkatan", x = "Angkatan", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = jenis_kelamin, fill = jenis_kelamin)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Jenis Kelamin", x = "Jenis Kelamin", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = tempat_tinggal, fill = tempat_tinggal)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Tempat Tinggal", x = "Tempat Tinggal", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = penghasilan_orang_tua, fill = penghasilan_orang_tua)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Penghasilan Orang Tua ", x = "Penghasilan Orang Tua", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = beban_tugas, fill = beban_tugas)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Beban Tugas", x = "Beban Tugas", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = keterlibatan_kegiatan_non_akademik, fill = keterlibatan_kegiatan_non_akademik)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Keterlibatan dalam Kegiatan Non-Akademik ", x = "Keterlibatan dalam Kegiatan Non-Akademik", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = bekerja_sambil_kuliah, fill = bekerja_sambil_kuliah)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Bekerja sambil Kuliah", x = "Bekerja sambil Kuliah", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = merasa_salah_jurusan, fill = merasa_salah_jurusan)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Merasa Salah Jurusan", x = "Merasa Salah Jurusan", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = memiliki_trauma, fill = memiliki_trauma)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Memilki Trauma", x = "Memilki Trauma", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = memiliki_masalah_kesehatan_mental, fill = memiliki_masalah_kesehatan_mental)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Memiliki Masalah Kesehatan Mental", x = "Memilki Masalah Kesehatan Mental", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

ggplot(data, aes(x = tingkat_stres, fill = tingkat_stres)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +  # Menambahkan teks frekuensi
  labs(title = "Diagram Batang Tingkat Stres", x = "Tingkat Stres", y = "Frekuensi") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Posisikan judul di tengah (hjust = 0.5)

# Membuat histogram untuk variabel bertipe data numerik
hist(data$usia, main = "Histogram Usia", 
     xlab = "Usia", col = "lightgreen", border = "black")

hist(data$tingkat_kenyaman_tempat_tinggal, main = "Histogram Tingkat Kenyamanan Tempat Tinggal", 
     xlab = "Tingkat Kenyamanan Tempat Tinggal", col = "lightgreen", border = "black")

hist(data$anak_ke, main = "Histogram Anak ke Berapa", 
     xlab = "Anak ke-", col = "lightgreen", border = "black")

hist(data$dukungan_orang_tua_dalam_perkuliahan, main = "Histogram Dukungan Orang Tua dalam Perkuliahan", 
     xlab = "Dukungan Orang Tua dalam Perkuliahan", col = "lightgreen", border = "black")

hist(data$durasi_tidur, main = "Histogram Durasi Tidur", 
     xlab = "Durasi Tidur", col = "lightgreen", border = "black")

hist(data$jumlah_teman_belajar, main = "Histogram Jumlah Teman Belajar", 
     xlab = "Jumlah Teman Belajar", col = "lightgreen", border = "black")

hist(data$IPK, main = "Histogram IPK", 
     xlab = "IPK", col = "lightgreen", border = "black")

hist(data$durasi_belajar_diluar_perkuliahan, main = "Histogram Durasi Belajar diluar Perkuliahan", 
     xlab = "Durasi Belajar diluar Perkuliahan", col = "lightgreen", border = "black")

# Membagi data menjadi data train dan test

# Random sampling 
samplesize = 0.60*nrow(data)
set.seed(100)
index = sample(seq_len(nrow(data)), size = samplesize)

# Membuat data train dan test 
datatrain = data[index,]
datatest = data[-index,]

# Membangun model regresi logistik ordinal
library(MASS)
model <- polr(tingkat_stres ~ ., data = datatrain, Hess = TRUE)
summary(model)

# Mengecek multikolinearitas pada model awal
library(car)
vif(model)

# Melakukan backward selection
model2 <- step(model, direction = "backward")

# Ringkasan model final
summary(model2)

# Mendapatkan koefisien dari model
coef_model <- coef(model2)

# Menghitung odds ratio
odds_ratio <- exp(coef_model)
data.frame(odds_ratio)

# Compute confusion table and misclassification error
predict_data <- predict(model2, datatest)
confusion_matrix <- table(datatest$tingkat_stres, predict_data)
confusion_matrix