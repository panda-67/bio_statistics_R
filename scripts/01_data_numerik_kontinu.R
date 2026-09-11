## =============================================================
## BIOSTATISTIKA UNTUK BIOLOGI
## Pertemuan 1 - Data Numerik Kontinu
## Topik: Rancangan Acak Lengkap (RAL) + One-Way ANOVA
## =============================================================
##
## TUJUAN PEMBELAJARAN:
## 1. Membaca dan mengeksplorasi data numerik kontinu
## 2. Menghitung statistik deskriptif per kelompok perlakuan
## 3. Memvisualisasikan sebaran data antar kelompok
## 4. Menguji perbedaan rata-rata antar perlakuan dengan ANOVA
## 5. Melakukan uji lanjut (post-hoc) jika ANOVA signifikan
## 6. Memeriksa asumsi model (normalitas & homogenitas ragam)
## =============================================================

## -----------------------------------------------------------
## 0. PERSIAPAN: memuat paket yang dibutuhkan
## -----------------------------------------------------------
# dplyr   -> untuk manipulasi data (group_by, summarise, dll)
# readr   -> untuk membaca file .csv dengan cepat & rapi
# ggplot2 -> untuk membuat visualisasi data
library(dplyr)
library(readr)
library(ggplot2)


## -----------------------------------------------------------
## 1. MEMBACA DATA
## -----------------------------------------------------------
# Pastikan file CSV berada di folder "data/" relatif terhadap
# working directory project ini (cek dengan getwd()).
data <- read_csv("data/01_data_numerik_kontinu.csv")

# glimpse() menampilkan struktur data: nama kolom, tipe data,
# dan beberapa contoh nilai -> berguna untuk memastikan data
# terbaca dengan benar (misalnya "perlakuan" harus berupa
# karakter/faktor, dan "tinggi_cm" harus numerik).
glimpse(data)

# summary() memberikan ringkasan statistik dasar (min, max,
# median, kuartil, dll) untuk setiap kolom.
summary(data)

# Pastikan kolom "perlakuan" terbaca sebagai faktor (kategori),
# bukan teks bebas, supaya ANOVA memperlakukannya sebagai
# kelompok/perlakuan, bukan variabel numerik/karakter biasa.
data <- data |>
  mutate(perlakuan = factor(perlakuan))

# Cek level/kategori perlakuan yang terbentuk
levels(data$perlakuan)


## -----------------------------------------------------------
## 2. STATISTIK DESKRIPTIF PER KELOMPOK
## -----------------------------------------------------------
# Sebelum uji statistik formal, kita perlu memahami data secara
# deskriptif: berapa jumlah sampel (n), rata-rata (mean),
# simpangan baku (sd), dan galat baku (se) pada setiap kelompok
# perlakuan. Galat baku (SE) menggambarkan presisi estimasi
# rata-rata dan berguna saat membuat error bar pada grafik.
ringkasan <- data |>
  group_by(perlakuan) |>
  summarise(
    n = n(),
    mean = mean(tinggi_cm, na.rm = TRUE),
    sd = sd(tinggi_cm, na.rm = TRUE),
    se = sd / sqrt(n),
    .groups = "drop" # menghindari pesan peringatan grouping
  )

print(ringkasan)


## -----------------------------------------------------------
## 3. VISUALISASI DATA
## -----------------------------------------------------------
# Boxplot menunjukkan sebaran (median, kuartil, outlier) tiap
# kelompok, sedangkan jitter menampilkan titik data individu
# agar kita bisa melihat jumlah & sebaran ulangan sebenarnya
# (bukan hanya ringkasannya).
ggplot(data, aes(x = perlakuan, y = tinggi_cm, fill = perlakuan)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.7, size = 2) +
  labs(
    title = "Sebaran Tinggi Tanaman Menurut Perlakuan",
    x = "Perlakuan",
    y = "Tinggi Tanaman (cm)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # legenda tidak perlu karena
# kategori sudah ada di sumbu-x

## -----------------------------------------------------------
## 4. MODEL RAL + ONE-WAY ANOVA
## -----------------------------------------------------------
# Rancangan Acak Lengkap (RAL) cocok digunakan ketika unit
# percobaan (misalnya pot tanaman) relatif homogen, sehingga
# perlakuan dapat diacak sepenuhnya tanpa pengelompokan
# (blocking) tambahan.
#
# Model: tinggi_cm ~ perlakuan
# artinya kita menguji apakah rata-rata tinggi_cm berbeda
# signifikan antar level perlakuan.
model <- aov(tinggi_cm ~ perlakuan, data = data)

# summary() pada objek aov menghasilkan tabel ANOVA:
# Df, Sum Sq, Mean Sq, F value, dan Pr(>F).
# Jika nilai Pr(>F) < 0.05, maka minimal ada satu pasang
# perlakuan yang rata-ratanya berbeda signifikan.
summary(model)


## -----------------------------------------------------------
## 5. UJI LANJUT (POST-HOC): TUKEY HSD
## -----------------------------------------------------------
# ANOVA hanya memberi tahu APAKAH ada perbedaan, bukan DI MANA
# perbedaan itu terjadi. Uji Tukey HSD (Honestly Significant
# Difference) membandingkan semua pasangan perlakuan sekaligus
# sambil mengontrol tingkat kesalahan (error rate) akibat
# perbandingan berganda.
#
# Catatan: uji ini hanya relevan diinterpretasikan jika ANOVA
# di atas menunjukkan hasil signifikan (Pr(>F) < 0.05).
tukey_result <- TukeyHSD(model)
print(tukey_result)

# Visualisasi selang kepercayaan perbedaan antar pasangan
# perlakuan; jika selang tidak melewati angka 0, maka pasangan
# tersebut berbeda signifikan.
plot(tukey_result)


## -----------------------------------------------------------
## 6. PEMERIKSAAN ASUMSI MODEL (DIAGNOSTIK)
## -----------------------------------------------------------
# ANOVA mengasumsikan:
#   (1) Residual (galat) menyebar normal
#   (2) Ragam (variance) antar kelompok homogen
#   (3) Pengamatan saling bebas (independen)
#
# par(mfrow = c(2,2)) mengatur tampilan menjadi 2x2 panel agar
# keempat grafik diagnostik dapat dilihat sekaligus:
#   - Residuals vs Fitted   -> mendeteksi pola non-linear/heteroskedastisitas
#   - Normal Q-Q            -> memeriksa normalitas residual
#   - Scale-Location         -> memeriksa homogenitas ragam
#   - Residuals vs Leverage  -> mendeteksi data pencilan berpengaruh
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1)) # mengembalikan tampilan grafik ke normal (1 panel)


## -----------------------------------------------------------
## 7. (OPSIONAL) UJI FORMAL ASUMSI
## -----------------------------------------------------------
# Selain inspeksi visual, asumsi juga bisa diuji secara formal:
#   - shapiro.test() untuk normalitas residual
#   - bartlett.test() atau car::leveneTest() untuk homogenitas ragam
# Uji formal berguna sebagai pelengkap, bukan pengganti, inspeksi
# visual di atas -- terutama karena uji formal sensitif terhadap
# ukuran sampel yang besar/kecil.

shapiro.test(residuals(model))
bartlett.test(tinggi_cm ~ perlakuan, data = data)

## =============================================================
## RINGKASAN ALUR ANALISIS:
## Baca data -> Eksplorasi -> Deskriptif per kelompok ->
## Visualisasi -> ANOVA -> Post-hoc (jika signifikan) ->
## Cek asumsi model
## =============================================================
