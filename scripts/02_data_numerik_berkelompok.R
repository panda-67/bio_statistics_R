## =============================================================
## BIOSTATISTIKA UNTUK BIOLOGI
## Pertemuan 2 - Data Numerik Berkelompok
## Topik: Rancangan Acak Kelompok (RAK / RCBD)
## =============================================================
##
## TUJUAN PEMBELAJARAN:
## 1. Memahami perbedaan RAK (RCBD) dengan RAL (Pertemuan 1)
## 2. Membaca & mengeksplorasi data dengan struktur berblok
## 3. Menghitung statistik deskriptif per perlakuan
## 4. Memvisualisasikan data dengan mempertimbangkan blok
## 5. Menguji pengaruh perlakuan DAN blok dengan two-way ANOVA
##    tanpa interaksi (model aditif khas RAK)
## 6. Melakukan uji lanjut Tukey HSD khusus untuk faktor perlakuan
## 7. Memeriksa asumsi model
## =============================================================
##
## KONSEP KUNCI: RAK vs RAL
## Pada RAL (Pertemuan 1), unit percobaan dianggap homogen
## sehingga perlakuan diacak sepenuhnya. Pada RAK, unit percobaan
## memiliki sumber keragaman lain yang diketahui (misalnya
## perbedaan lokasi, waktu, atau kondisi lingkungan) -- sumber
## keragaman ini dikelompokkan menjadi "blok". Dengan memasukkan
## blok ke dalam model, keragaman akibat blok dipisahkan dari
## galat percobaan, sehingga uji terhadap perlakuan menjadi lebih
## sensitif (lebih berpeluang mendeteksi perbedaan yang sebenarnya
## ada).
## =============================================================

## -----------------------------------------------------------
## 0. PERSIAPAN: memuat paket yang dibutuhkan
## -----------------------------------------------------------
# tidyverse adalah kumpulan paket (readr, dplyr, ggplot2, dll)
# yang mempermudah proses baca data, manipulasi, dan visualisasi
# dalam satu alur kerja yang konsisten.
library(dplyr)
library(readr)
library(ggplot2)


## -----------------------------------------------------------
## 1. MEMBACA DATA
## -----------------------------------------------------------
data <- read_csv("data/02_data_numerik_berkelompok.csv")

# glimpse() membantu memastikan struktur data sudah benar:
# - "perlakuan" -> variabel perlakuan (faktor yang diuji)
# - "blok"      -> variabel pengelompokan (faktor kontrol)
# - "tinggi_cm" -> variabel respons numerik
glimpse(data)

# Pastikan "perlakuan" dan "blok" terbaca sebagai faktor
# (kategori), bukan teks/angka bebas. Ini penting karena ANOVA
# memperlakukan keduanya sebagai sumber keragaman kategorik,
# bukan variabel numerik kontinu.
data <- data |>
  mutate(
    perlakuan = factor(perlakuan),
    blok = factor(blok)
  )

levels(data$perlakuan)
levels(data$blok)


## -----------------------------------------------------------
## 2. STATISTIK DESKRIPTIF PER PERLAKUAN
## -----------------------------------------------------------
# Ringkasan ini menggambarkan rata-rata & keragaman tinggi
# tanaman pada tiap perlakuan, digabung lintas semua blok.
# Ini baru gambaran awal -- efek blok belum diperhitungkan di
# sini, dan baru akan terlihat lewat model ANOVA di bagian 4.
data |>
  group_by(perlakuan) |>
  summarise(
    n = n(),
    mean = mean(tinggi_cm, na.rm = TRUE),
    sd = sd(tinggi_cm, na.rm = TRUE),
    .groups = "drop"
  )

# Statistik deskriptif per blok juga berguna untuk melihat
# apakah blok memang menyumbang keragaman yang cukup besar
# (jika rata-rata antar blok jauh berbeda, artinya pengelompokan
# blok memang beralasan/bermanfaat).
data |>
  group_by(blok) |>
  summarise(
    n = n(),
    mean = mean(tinggi_cm, na.rm = TRUE),
    sd = sd(tinggi_cm, na.rm = TRUE),
    .groups = "drop"
  )


## -----------------------------------------------------------
## 3. VISUALISASI DATA
## -----------------------------------------------------------
# Boxplot per perlakuan tetap menjadi cara utama melihat sebaran
# respons antar perlakuan. Titik individual (geom_point + jitter)
# menampilkan tiap pengamatan, dan pewarnaan menurut blok
# membantu melihat apakah pola tinggi tanaman konsisten di
# setiap blok atau tidak.
ggplot(data, aes(x = perlakuan, y = tinggi_cm)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_point(
    aes(color = blok),
    position = position_jitter(width = 0.08),
    size = 2
  ) +
  labs(
    title = "Sebaran Tinggi Tanaman Menurut Perlakuan dan Blok",
    x = "Perlakuan",
    y = "Tinggi Tanaman (cm)",
    color = "Blok"
  ) +
  theme_minimal()

# Grafik tambahan: garis per blok untuk melihat pola/interaksi
# perlakuan-blok secara visual. Jika garis antar blok cenderung
# sejajar, ini mendukung asumsi model aditif (tanpa interaksi)
# yang digunakan pada RAK klasik di bagian 4.
ggplot(data, aes(x = perlakuan, y = tinggi_cm, group = blok, color = blok)) +
  geom_line(stat = "summary", fun = mean) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  labs(
    title = "Rata-rata Tinggi Tanaman per Blok Menurut Perlakuan",
    x = "Perlakuan",
    y = "Rata-rata Tinggi Tanaman (cm)",
    color = "Blok"
  ) +
  theme_minimal()


## -----------------------------------------------------------
## 4. MODEL RAK: TWO-WAY ANOVA TANPA INTERAKSI
## -----------------------------------------------------------
# Model: tinggi_cm ~ perlakuan + blok
# Notasi "+" (bukan "*") berarti model aditif: pengaruh
# perlakuan dan pengaruh blok terhadap tinggi_cm dianggap saling
# bebas/tidak berinteraksi. Ini adalah bentuk model klasik untuk
# RAK dengan satu ulangan per kombinasi perlakuan-blok.
#
# Urutan penulisan (perlakuan + blok) memengaruhi tabel Sum of
# Squares tipe I (default aov()): keragaman akibat perlakuan
# dihitung lebih dulu, baru sisanya dijelaskan oleh blok. Karena
# fokus penelitian ada pada perlakuan, urutan ini yang lazim
# digunakan.
model <- aov(tinggi_cm ~ perlakuan + blok, data = data)

# summary() menghasilkan tabel ANOVA dengan baris terpisah untuk
# "perlakuan" dan "blok":
# - Baris "perlakuan" -> menguji apakah rata-rata tinggi_cm
#   berbeda signifikan antar perlakuan (ini fokus utama analisis)
# - Baris "blok"      -> menguji apakah blok menyumbang keragaman
#   signifikan (menunjukkan apakah pengelompokan blok efektif)
summary(model)


## -----------------------------------------------------------
## 5. UJI LANJUT (POST-HOC): TUKEY HSD UNTUK PERLAKUAN
## -----------------------------------------------------------
# Karena yang ingin dibandingkan secara berpasangan adalah
# level-level "perlakuan" (bukan "blok"), argumen kedua pada
# TukeyHSD() diisi "perlakuan" agar hanya perbandingan antar
# perlakuan yang dihitung dan ditampilkan.
#
# Catatan: sama seperti Pertemuan 1, uji lanjut ini hanya
# relevan diinterpretasikan jika baris "perlakuan" pada tabel
# ANOVA di atas signifikan (Pr(>F) < 0.05).
tukey_result <- TukeyHSD(model, "perlakuan")
print(tukey_result)

# Selang kepercayaan yang tidak melewati angka 0 menunjukkan
# pasangan perlakuan tersebut berbeda signifikan.
plot(tukey_result)


## -----------------------------------------------------------
## 6. PEMERIKSAAN ASUMSI MODEL (DIAGNOSTIK)
## -----------------------------------------------------------
# Asumsi yang diperiksa sama seperti pada model RAL:
#   (1) Residual menyebar normal
#   (2) Ragam antar kelompok homogen
#   (3) Tidak ada pencilan yang terlalu berpengaruh
# Namun pada RAK, residual yang diperiksa sudah "bersih" dari
# pengaruh blok, karena blok sudah dimasukkan ke dalam model.
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1)) # kembalikan tampilan grafik ke normal


## -----------------------------------------------------------
## 7. (OPSIONAL) UJI FORMAL ASUMSI
## -----------------------------------------------------------
# Uji Shapiro-Wilk untuk normalitas residual, dan Bartlett untuk
# homogenitas ragam antar kelompok perlakuan. Sama seperti
# sebelumnya, kedua uji ini melengkapi -- bukan menggantikan --
# pemeriksaan visual pada bagian 6.
shapiro.test(residuals(model))
bartlett.test(tinggi_cm ~ perlakuan, data = data)

## =============================================================
## RINGKASAN ALUR ANALISIS:
## Baca data -> Eksplorasi -> Deskriptif (per perlakuan & blok)
## -> Visualisasi -> ANOVA dua arah (perlakuan + blok) ->
## Post-hoc Tukey khusus perlakuan -> Cek asumsi model
##
## PERBEDAAN UTAMA DENGAN PERTEMUAN 1 (RAL):
## - Model menambahkan "blok" sebagai sumber keragaman terkontrol
## - Tukey HSD dibatasi hanya untuk faktor "perlakuan"
## - Tujuannya: mengurangi galat percobaan dengan memisahkan
##   keragaman akibat blok, sehingga uji perlakuan lebih peka
## =============================================================
