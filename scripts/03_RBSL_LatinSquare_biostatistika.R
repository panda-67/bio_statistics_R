## =============================================================
## BIOSTATISTIKA UNTUK BIOLOGI
## Pertemuan 3 - Data Numerik Dua Arah
## Topik: Rancangan Bujur Sangkar Latin (RBSL / Latin Square)
## =============================================================
##
## TUJUAN PEMBELAJARAN:
## 1. Memahami konsep RBSL sebagai pengembangan dari RAK dengan
##    DUA arah pengelompokan (baris & kolom) sekaligus
## 2. Membaca & mengeksplorasi data dengan struktur baris-kolom
## 3. Memvisualisasikan tata letak perlakuan dalam bentuk "peta"
##    baris x kolom
## 4. Menguji pengaruh perlakuan dengan mengontrol keragaman
##    akibat baris dan kolom secara bersamaan
## 5. Melakukan uji lanjut Tukey HSD khusus untuk faktor perlakuan
## 6. Memeriksa asumsi model
## =============================================================
##
## KONSEP KUNCI: RBSL vs RAK
## Pada RAK (Pertemuan 2), keragaman terkontrol hanya berasal
## dari SATU arah pengelompokan (blok). Pada RBSL, ada DUA arah
## pengelompokan sekaligus -- baris dan kolom -- yang masing-
## masing dianggap sebagai sumber keragaman terkontrol yang
## saling tegak lurus (independen satu sama lain). Setiap
## perlakuan muncul TEPAT SATU KALI pada setiap baris dan TEPAT
## SATU KALI pada setiap kolom, sehingga pengaruh baris dan
## kolom dapat dipisahkan dari pengaruh perlakuan yang
## sesungguhnya ingin diuji.
##
## Konsekuensinya, RBSL membutuhkan jumlah ulangan = jumlah
## perlakuan (tata letak berbentuk bujur sangkar n x n), dan
## TIDAK ADA replikasi lain di luar itu -- karena itu, model
## RBSL klasik tidak dapat menguji interaksi antar faktor.
## =============================================================

## -----------------------------------------------------------
## 0. PERSIAPAN: memuat paket yang dibutuhkan
## -----------------------------------------------------------
# Di sini paket dimuat satu per satu (bukan tidyverse) agar
# lebih jelas paket mana yang menyediakan fungsi apa:
#   readr   -> membaca file .csv (read_csv)
#   dplyr   -> manipulasi data (mutate, group_by, summarise)
#   ggplot2 -> visualisasi data
library(readr)
library(dplyr)
library(ggplot2)


## -----------------------------------------------------------
## 1. MEMBACA DATA
## -----------------------------------------------------------
data <- read_csv("data/03_data_numerik_dua_arah.csv")

# glimpse() memastikan struktur data sudah sesuai ekspektasi:
# - "baris"      -> faktor pengelompokan arah pertama
# - "kolom"      -> faktor pengelompokan arah kedua
# - "perlakuan"  -> faktor perlakuan yang ingin diuji
# - "biomassa_g" -> variabel respons numerik
glimpse(data)

# Konversi "baris", "kolom", dan "perlakuan" menjadi faktor.
# Ini penting terutama untuk "baris" dan "kolom": jika masih
# berupa angka, R akan memperlakukannya sebagai variabel numerik
# kontinu (bukan kategori), sehingga model ANOVA yang terbentuk
# akan salah secara konsep.
data <- data |>
  mutate(
    baris = factor(baris),
    kolom = factor(kolom),
    perlakuan = factor(perlakuan)
  )

levels(data$baris)
levels(data$kolom)
levels(data$perlakuan)

# Pemeriksaan tata letak RBSL: pastikan setiap perlakuan memang
# muncul tepat satu kali pada tiap baris dan tiap kolom. Jika
# tabel yang dihasilkan bukan berisi angka 1 semua, artinya tata
# letak bujur sangkar latin belum terpenuhi dan model di bagian
# 4 tidak akan valid.
table(data$baris, data$perlakuan)
table(data$kolom, data$perlakuan)


## -----------------------------------------------------------
## 2. STATISTIK DESKRIPTIF PER PERLAKUAN
## -----------------------------------------------------------
# Ringkasan ini digabung lintas semua baris & kolom -- baru
# gambaran awal. Efek baris dan kolom baru terlihat nanti lewat
# model ANOVA pada bagian 4.
data |>
  group_by(perlakuan) |>
  summarise(
    n = n(),
    mean = mean(biomassa_g, na.rm = TRUE),
    sd = sd(biomassa_g, na.rm = TRUE),
    .groups = "drop"
  )


## -----------------------------------------------------------
## 3. VISUALISASI TATA LETAK RBSL
## -----------------------------------------------------------
# geom_tile() menggambarkan "peta" petak percobaan: sumbu-x
# adalah kolom, sumbu-y adalah baris, warna petak menunjukkan
# nilai biomassa_g, dan label di dalam petak menunjukkan
# perlakuan apa yang ditempatkan di sana. Grafik ini sangat
# membantu memvisualisasikan apakah tata letak perlakuan sudah
# benar-benar seimbang (setiap perlakuan sekali per baris/kolom).
ggplot(data, aes(x = factor(kolom), y = factor(baris), fill = biomassa_g)) +
  geom_tile(color = "white") +
  geom_text(aes(label = perlakuan), color = "black", size = 4) +
  scale_fill_gradient(low = "#fff5eb", high = "#d94801") +
  labs(
    title = "Tata Letak Rancangan Bujur Sangkar Latin",
    subtitle = "Warna petak = biomassa_g, label = kode perlakuan",
    x = "Kolom",
    y = "Baris",
    fill = "Biomassa (g)"
  ) +
  theme_minimal()

# Boxplot tambahan untuk melihat sebaran biomassa per perlakuan
# secara langsung, mirip grafik pada Pertemuan 1 & 2.
ggplot(data, aes(x = perlakuan, y = biomassa_g)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2, alpha = 0.7) +
  labs(
    title = "Sebaran Biomassa Menurut Perlakuan",
    x = "Perlakuan",
    y = "Biomassa (g)"
  ) +
  theme_minimal()


## -----------------------------------------------------------
## 4. MODEL RBSL: ANOVA DENGAN DUA ARAH PENGELOMPOKAN
## -----------------------------------------------------------
# Model: biomassa_g ~ perlakuan + factor(baris) + factor(kolom)
# Ketiga suku dijumlahkan (model aditif) -- artinya pengaruh
# perlakuan, baris, dan kolom terhadap biomassa_g diasumsikan
# tidak saling berinteraksi. Ini adalah bentuk model klasik untuk
# RBSL, sesuai dengan keterbatasan derajat bebas pada rancangan
# ini (jumlah data yang terbatas tidak memungkinkan pendugaan
# suku interaksi).
#
# factor(baris) dan factor(kolom) ditulis eksplisit di dalam
# formula sebagai jaminan tambahan bahwa keduanya diperlakukan
# sebagai kategori, meskipun sudah dikonversi ke faktor di
# bagian 1.
model <- aov(
  biomassa_g ~ perlakuan + factor(baris) + factor(kolom),
  data = data
)

# summary() menghasilkan tabel ANOVA dengan tiga baris:
# - "perlakuan"     -> fokus utama: apakah rata-rata biomassa
#                      berbeda signifikan antar perlakuan
# - "factor(baris)" -> apakah arah baris menyumbang keragaman
#                      signifikan
# - "factor(kolom)" -> apakah arah kolom menyumbang keragaman
#                      signifikan
# Baris & kolom yang signifikan menunjukkan bahwa pengelompokan
# dua arah tersebut memang efektif mengurangi galat percobaan.
summary(model)


## -----------------------------------------------------------
## 5. UJI LANJUT (POST-HOC): TUKEY HSD UNTUK PERLAKUAN
## -----------------------------------------------------------
# Sama seperti RAK, uji lanjut hanya dilakukan untuk faktor
# "perlakuan" karena itulah faktor yang menjadi fokus penelitian
# (baris dan kolom hanya faktor pengontrol, bukan objek yang
# ingin dibandingkan levelnya).
#
# Catatan: interpretasikan hasil ini hanya jika baris
# "perlakuan" pada tabel ANOVA di atas signifikan (Pr(>F) < 0.05).
tukey_result <- TukeyHSD(model, "perlakuan")
print(tukey_result)

# Selang kepercayaan yang tidak melewati angka 0 menunjukkan
# pasangan perlakuan tersebut berbeda signifikan.
plot(tukey_result)


## -----------------------------------------------------------
## 6. PEMERIKSAAN ASUMSI MODEL (DIAGNOSTIK)
## -----------------------------------------------------------
# Asumsi yang diperiksa sama seperti model-model sebelumnya:
#   (1) Residual menyebar normal
#   (2) Ragam antar kelompok homogen
#   (3) Tidak ada pencilan yang terlalu berpengaruh
# Pada RBSL, residual yang diperiksa sudah "bersih" dari
# pengaruh baris maupun kolom, karena keduanya sudah dimasukkan
# ke dalam model.
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1)) # kembalikan tampilan grafik ke normal


## -----------------------------------------------------------
## 7. (OPSIONAL) UJI FORMAL ASUMSI
## -----------------------------------------------------------
shapiro.test(residuals(model))
bartlett.test(biomassa_g ~ perlakuan, data = data)

## =============================================================
## RINGKASAN ALUR ANALISIS:
## Baca data -> Cek tata letak baris/kolom -> Deskriptif ->
## Visualisasi peta petak -> ANOVA tiga arah (perlakuan + baris
## + kolom) -> Post-hoc Tukey khusus perlakuan -> Cek asumsi
##
## PERBEDAAN UTAMA DENGAN PERTEMUAN 2 (RAK):
## - Ada DUA arah pengelompokan (baris & kolom), bukan satu
## - Setiap perlakuan muncul tepat sekali per baris & per kolom
##   (tata letak bujur sangkar), sehingga jumlah ulangan =
##   jumlah perlakuan
## - Model tidak dapat menguji interaksi karena keterbatasan
##   derajat bebas pada rancangan ini
## =============================================================
