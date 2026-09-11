## =============================================================
## BIOSTATISTIKA UNTUK BIOLOGI
## Pertemuan 4 - Data Numerik Multifaktor
## Topik: Rancangan Acak Kelompok (RAK) Faktorial 2 Faktor
## =============================================================
##
## TUJUAN PEMBELAJARAN:
## 1. Memahami konsep rancangan FAKTORIAL: menguji dua faktor
##    perlakuan sekaligus (di sini: pupuk & cahaya) dalam satu
##    model, termasuk kemungkinan INTERAKSI antar keduanya
## 2. Membaca & mengeksplorasi data dengan dua faktor perlakuan
##    dan satu faktor blok
## 3. Menghitung statistik deskriptif untuk tiap kombinasi
##    perlakuan (pupuk x cahaya)
## 4. Memvisualisasikan pola interaksi dengan grafik garis
## 5. Menguji pengaruh utama pupuk, cahaya, serta interaksinya,
##    sambil tetap mengontrol keragaman akibat blok
## 6. Memeriksa asumsi model
## =============================================================
##
## KONSEP KUNCI: APA ITU INTERAKSI?
## Pada rancangan faktorial, kita tidak hanya menguji pengaruh
## masing-masing faktor secara terpisah (disebut PENGARUH UTAMA/
## main effect), tetapi juga apakah pengaruh satu faktor
## BERGANTUNG pada level faktor lainnya (disebut INTERAKSI).
## Contoh: jika pengaruh pupuk terhadap tinggi tanaman berbeda-
## beda tergantung kondisi cahaya (misalnya pupuk X sangat efektif
## di cahaya terang tetapi tidak berpengaruh di tempat teduh),
## maka ada interaksi antara pupuk dan cahaya. Interaksi inilah
## yang membuat rancangan faktorial lebih informatif dibanding
## menguji tiap faktor secara terpisah dalam percobaan berbeda.
## =============================================================

## -----------------------------------------------------------
## 0. PERSIAPAN: memuat paket yang dibutuhkan
## -----------------------------------------------------------
# Paket dimuat satu per satu (bukan tidyverse) agar jelas fungsi
# apa berasal dari paket mana:
#   readr   -> membaca file .csv (read_csv)
#   dplyr   -> manipulasi data (mutate, group_by, summarise)
#   ggplot2 -> visualisasi data
library(readr)
library(dplyr)
library(ggplot2)


## -----------------------------------------------------------
## 1. MEMBACA DATA
## -----------------------------------------------------------
data <- read_csv("data/04_data_numerik_multifaktor.csv")

# glimpse() memastikan struktur data sudah sesuai ekspektasi:
# - "blok"      -> faktor pengelompokan (dari rancangan RAK)
# - "pupuk"     -> faktor perlakuan pertama
# - "cahaya"    -> faktor perlakuan kedua
# - "tinggi_cm" -> variabel respons numerik
glimpse(data)

# Konversi "blok", "pupuk", dan "cahaya" menjadi faktor, supaya
# ANOVA memperlakukan ketiganya sebagai variabel kategorik, bukan
# numerik/karakter bebas.
data <- data |>
  mutate(
    blok = factor(blok),
    pupuk = factor(pupuk),
    cahaya = factor(cahaya)
  )

levels(data$blok)
levels(data$pupuk)
levels(data$cahaya)

# Pemeriksaan keseimbangan rancangan: idealnya setiap kombinasi
# pupuk x cahaya muncul dengan jumlah ulangan yang sama pada
# setiap blok. Tabel ini membantu mendeteksi data yang hilang
# atau tidak seimbang sebelum masuk ke tahap pemodelan.
table(data$pupuk, data$cahaya)


## -----------------------------------------------------------
## 2. STATISTIK DESKRIPTIF PER KOMBINASI PERLAKUAN
## -----------------------------------------------------------
# group_by(pupuk, cahaya) mengelompokkan data berdasarkan
# KOMBINASI kedua faktor, sehingga kita bisa melihat rata-rata
# dan keragaman tinggi_cm pada tiap kombinasi pupuk x cahaya --
# ini adalah langkah awal untuk "melihat" pola interaksi secara
# numerik sebelum divisualisasikan.
data |>
  group_by(pupuk, cahaya) |>
  summarise(
    n = n(),
    mean = mean(tinggi_cm, na.rm = TRUE),
    sd = sd(tinggi_cm, na.rm = TRUE),
    .groups = "drop"
  )


## -----------------------------------------------------------
## 3. VISUALISASI POLA INTERAKSI
## -----------------------------------------------------------
# Grafik interaksi (interaction plot) adalah cara paling umum
# untuk melihat pola interaksi secara visual:
# - sumbu-x -> level faktor pertama (cahaya)
# - garis terpisah (linetype/color) -> level faktor kedua (pupuk)
# - titik & garis -> rata-rata tinggi_cm pada tiap kombinasi
#
# Interpretasi pola garis:
# - Garis yang SEJAJAR  -> menunjukkan TIDAK ADA interaksi
#   (pengaruh cahaya terhadap tinggi_cm konsisten di semua level
#   pupuk)
# - Garis yang BERPOTONGAN atau kemiringannya berbeda jauh ->
#   mengindikasikan ADANYA interaksi antara pupuk dan cahaya
ggplot(
  data,
  aes(x = cahaya, y = tinggi_cm, group = pupuk, color = pupuk, linetype = pupuk)
) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Pola Interaksi Pupuk x Cahaya terhadap Tinggi Tanaman",
    subtitle = "Garis tidak sejajar mengindikasikan adanya interaksi",
    x = "Kondisi Cahaya",
    y = "Rata-rata Tinggi Tanaman (cm)",
    color = "Pupuk",
    linetype = "Pupuk"
  ) +
  theme_minimal()

# Boxplot tambahan menurut kombinasi perlakuan, untuk melihat
# sebaran data individual (tidak hanya rata-rata seperti grafik
# interaksi di atas).
ggplot(data, aes(x = cahaya, y = tinggi_cm, fill = pupuk)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Sebaran Tinggi Tanaman Menurut Kombinasi Pupuk dan Cahaya",
    x = "Kondisi Cahaya",
    y = "Tinggi Tanaman (cm)",
    fill = "Pupuk"
  ) +
  theme_minimal()


## -----------------------------------------------------------
## 4. MODEL RAK FAKTORIAL: ANOVA DUA FAKTOR + BLOK
## -----------------------------------------------------------
# Model: tinggi_cm ~ blok + pupuk * cahaya
# Notasi "pupuk * cahaya" adalah singkatan dari
# "pupuk + cahaya + pupuk:cahaya", yang berarti model menguji:
#   - pengaruh utama pupuk
#   - pengaruh utama cahaya
#   - pengaruh interaksi pupuk:cahaya
# sekaligus, sedangkan "blok" tetap dimasukkan lebih dulu di
# dalam formula untuk mengontrol/menghilangkan keragaman akibat
# pengelompokan blok (sama seperti pada RAK di Pertemuan 2),
# sebelum menguji pengaruh kedua faktor perlakuan.
model <- aov(
  tinggi_cm ~ blok + pupuk * cahaya,
  data = data
)

# summary() menghasilkan tabel ANOVA dengan baris:
# - "blok"          -> keragaman akibat pengelompokan blok
# - "pupuk"         -> pengaruh utama pupuk
# - "cahaya"        -> pengaruh utama cahaya
# - "pupuk:cahaya"  -> pengaruh interaksi antara pupuk dan cahaya
#
# CARA MEMBACA HASIL:
# - Jika "pupuk:cahaya" SIGNIFIKAN (Pr(>F) < 0.05), maka
#   interpretasi pengaruh utama pupuk atau cahaya secara terpisah
#   menjadi kurang bermakna -- fokuskan interpretasi pada pola
#   interaksi (lihat kembali grafik pada bagian 3), dan lakukan
#   uji lanjut per kombinasi bila diperlukan.
# - Jika interaksi TIDAK signifikan, maka pengaruh utama pupuk
#   dan cahaya masing-masing dapat diinterpretasikan secara
#   independen/terpisah.
summary(model)


## -----------------------------------------------------------
## 5. (OPSIONAL) UJI LANJUT
## -----------------------------------------------------------
# Jika interaksi TIDAK signifikan, uji lanjut cukup dilakukan
# untuk masing-masing pengaruh utama yang signifikan, contoh:
#   TukeyHSD(model, "pupuk")
#   TukeyHSD(model, "cahaya")
#
# Jika interaksi SIGNIFIKAN, sebaiknya bandingkan rata-rata
# antar kombinasi pupuk x cahaya, contoh:
#   TukeyHSD(model, "pupuk:cahaya")
# Interpretasikan hasil ini sesuai dengan pola pada grafik
# interaksi di bagian 3.

TukeyHSD(model, "pupuk:cahaya")


## -----------------------------------------------------------
## 6. PEMERIKSAAN ASUMSI MODEL (DIAGNOSTIK)
## -----------------------------------------------------------
# Asumsi yang diperiksa sama seperti model-model sebelumnya:
#   (1) Residual menyebar normal
#   (2) Ragam antar kelompok (kombinasi perlakuan) homogen
#   (3) Tidak ada pencilan yang terlalu berpengaruh
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1)) # kembalikan tampilan grafik ke normal


## -----------------------------------------------------------
## 7. (OPSIONAL) UJI FORMAL ASUMSI
## -----------------------------------------------------------
shapiro.test(residuals(model))
bartlett.test(tinggi_cm ~ interaction(pupuk, cahaya), data = data)

## =============================================================
## RINGKASAN ALUR ANALISIS:
## Baca data -> Cek keseimbangan kombinasi perlakuan ->
## Deskriptif per kombinasi -> Visualisasi pola interaksi ->
## ANOVA (blok + pupuk * cahaya) -> Cek signifikansi interaksi
## -> Uji lanjut sesuai hasil -> Cek asumsi model
##
## PERBEDAAN UTAMA DENGAN PERTEMUAN SEBELUMNYA:
## - Ada DUA faktor perlakuan sekaligus (pupuk & cahaya), bukan
##   satu, sehingga model dapat menguji INTERAKSI antar faktor
## - Notasi "*" dalam formula ANOVA menghasilkan pengaruh utama
##   kedua faktor DITAMBAH interaksi keduanya
## - Interpretasi hasil selalu dimulai dari suku interaksi
##   sebelum menafsirkan pengaruh utama masing-masing faktor
## =============================================================
