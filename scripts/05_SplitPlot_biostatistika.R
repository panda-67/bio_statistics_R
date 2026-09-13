## =============================================================
## BIOSTATISTIKA UNTUK BIOLOGI
## Pertemuan 5 - Data Numerik Bertingkat
## Topik: Rancangan Petak Terbagi (Split Plot Design)
## =============================================================
##
## TUJUAN PEMBELAJARAN:
## 1. Memahami konsep rancangan BERTINGKAT: ada dua "unit
##    percobaan" dengan ukuran berbeda dalam satu percobaan yang
##    sama (petak utama/main plot dan anak petak/subplot)
## 2. Memahami mengapa split plot memerlukan LEBIH DARI SATU
##    strata galat (error strata), berbeda dari rancangan
##    sebelumnya yang hanya punya satu sumber galat
## 3. Membaca & mengeksplorasi data dengan struktur bertingkat
## 4. Memvisualisasikan pola interaksi antara faktor petak utama
##    dan faktor anak petak
## 5. Menyusun model ANOVA dengan Error() sesuai struktur galat
##    berjenjang
## 6. Mengenal arah pengembangan lanjutan: model mixed-effects
## =============================================================
##
## KONSEP KUNCI: MENGAPA SPLIT PLOT BUTUH DUA STRATA GALAT?
## Pada rancangan split plot, satu faktor (misalnya irigasi)
## diterapkan pada PETAK UTAMA (main plot) yang berukuran besar
## dan sulit/tidak praktis diacak sesering faktor lain -- misalnya
## karena keterbatasan teknis pengairan. Faktor kedua (misalnya
## pupuk) kemudian diterapkan pada ANAK PETAK (subplot) yang
## merupakan pembagian lebih kecil di dalam tiap petak utama.
##
## Konsekuensinya:
## - Faktor petak utama (irigasi) diuji dengan galat petak utama
##   (main plot error), yang lebih besar karena unit
##   percobaannya lebih besar dan replikasinya lebih terbatas
## - Faktor anak petak (pupuk) dan interaksinya dengan faktor
##   petak utama diuji dengan galat anak petak (subplot error),
##   yang biasanya lebih kecil/lebih presisi
## Karena itu, split plot TIDAK BISA dianalisis dengan aov()
## satu strata galat seperti rancangan sebelumnya -- diperlukan
## Error() untuk memisahkan kedua sumber galat tersebut.
## =============================================================

## -----------------------------------------------------------
## 0. PERSIAPAN: memuat paket yang dibutuhkan
## -----------------------------------------------------------
# Paket dimuat satu per satu agar jelas fungsi apa berasal dari
# paket mana:
#   readr   -> membaca file .csv (read_csv)
#   dplyr   -> manipulasi data (mutate, group_by, summarise)
#   ggplot2 -> visualisasi data
library(readr)
library(dplyr)
library(ggplot2)


## -----------------------------------------------------------
## 1. MEMBACA DATA
## -----------------------------------------------------------
data <- read_csv("data/05_data_numerik_bertingkat.csv")

# glimpse() memastikan struktur data sudah sesuai ekspektasi:
# - "blok"      -> faktor pengelompokan/ulangan
# - "irigasi"   -> faktor PETAK UTAMA (main plot factor)
# - "pupuk"     -> faktor ANAK PETAK (subplot factor)
# - "tinggi_cm" -> variabel respons numerik
glimpse(data)

# Konversi "blok", "irigasi", dan "pupuk" menjadi faktor, supaya
# model memperlakukan ketiganya sebagai variabel kategorik.
data <- data |>
  mutate(
    blok = factor(blok),
    irigasi = factor(irigasi),
    pupuk = factor(pupuk)
  )

levels(data$blok)
levels(data$irigasi)
levels(data$pupuk)

# Pemeriksaan struktur bertingkat: setiap kombinasi blok x
# irigasi seharusnya memuat SEMUA level pupuk (karena pupuk
# "tersarang" di dalam tiap petak irigasi). Tabel ini membantu
# memverifikasi bahwa struktur data sesuai dengan rancangan
# split plot yang dimaksud.
table(data$blok, data$irigasi)
with(data, table(interaction(blok, irigasi), pupuk))


## -----------------------------------------------------------
## 2. STATISTIK DESKRIPTIF PER KOMBINASI PERLAKUAN
## -----------------------------------------------------------
# Seperti pada rancangan faktorial (Pertemuan 4), kita melihat
# rata-rata & keragaman tinggi_cm pada tiap kombinasi irigasi x
# pupuk sebagai gambaran awal pola data, termasuk kemungkinan
# interaksi antar keduanya.
data |>
  group_by(irigasi, pupuk) |>
  summarise(
    n = n(),
    mean = mean(tinggi_cm, na.rm = TRUE),
    sd = sd(tinggi_cm, na.rm = TRUE),
    .groups = "drop"
  )


## -----------------------------------------------------------
## 3. VISUALISASI POLA INTERAKSI
## -----------------------------------------------------------
# Grafik interaksi: sumbu-x adalah faktor anak petak (pupuk),
# garis terpisah menurut faktor petak utama (irigasi). Sama
# seperti Pertemuan 4, garis yang tidak sejajar mengindikasikan
# adanya interaksi antara irigasi dan pupuk.
ggplot(
  data,
  aes(
    x = pupuk,
    y = tinggi_cm,
    group = irigasi,
    color = irigasi,
    linetype = irigasi
  )
) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Pola Interaksi Irigasi x Pupuk terhadap Tinggi Tanaman",
    subtitle = "Garis tidak sejajar mengindikasikan adanya interaksi",
    x = "Pupuk (faktor anak petak)",
    y = "Rata-rata Tinggi Tanaman (cm)",
    color = "Irigasi (faktor petak utama)",
    linetype = "Irigasi (faktor petak utama)"
  ) +
  theme_minimal()

# Boxplot tambahan untuk melihat sebaran data individual per
# kombinasi irigasi x pupuk.
ggplot(data, aes(x = pupuk, y = tinggi_cm, fill = irigasi)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Sebaran Tinggi Tanaman Menurut Irigasi dan Pupuk",
    x = "Pupuk",
    y = "Tinggi Tanaman (cm)",
    fill = "Irigasi"
  ) +
  theme_minimal()


## -----------------------------------------------------------
## 4. MODEL SPLIT PLOT: ANOVA DENGAN DUA STRATA GALAT
## -----------------------------------------------------------
# Model: tinggi_cm ~ irigasi * pupuk + Error(blok/irigasi)
#
# Penjelasan tiap bagian formula:
# - "irigasi * pupuk" -> menguji pengaruh utama irigasi, pengaruh
#   utama pupuk, dan interaksi irigasi:pupuk (sama seperti
#   notasi "*" pada Pertemuan 4)
# - "Error(blok/irigasi)" -> bagian INILAH yang membedakan split
#   plot dari faktorial biasa. Notasi ini memberi tahu R bahwa:
#     a. "blok" adalah strata paling luar (ulangan)
#     b. "irigasi" tersarang (nested) di dalam blok, membentuk
#        strata petak utama (blok:irigasi) sebagai galat untuk
#        menguji pengaruh irigasi
#     c. Sisa keragaman di luar kedua strata ini otomatis
#        menjadi strata galat anak petak (Within), yang
#        digunakan untuk menguji pupuk dan interaksi
#        irigasi:pupuk
model <- aov(
  tinggi_cm ~ irigasi * pupuk + Error(blok / irigasi),
  data = data
)

# summary() pada model Error() menghasilkan BEBERAPA tabel
# ANOVA terpisah, satu untuk tiap strata galat:
# - Strata "blok:irigasi" -> berisi baris "irigasi", diuji
#   dengan galat petak utama (main plot error)
# - Strata "Within"       -> berisi baris "pupuk" dan
#   "irigasi:pupuk", diuji dengan galat anak petak (subplot
#   error, biasanya lebih presisi)
#
# PENTING: jangan membandingkan nilai F "irigasi" dengan
# "pupuk" secara langsung -- keduanya diuji terhadap galat yang
# berbeda strata, sehingga tidak bisa disamakan begitu saja.
summary(model)


## -----------------------------------------------------------
## 5. CATATAN UJI LANJUT PADA SPLIT PLOT
## -----------------------------------------------------------
# TukeyHSD() TIDAK dapat langsung digunakan pada objek aov()
# yang memuat Error(), karena TukeyHSD() mengasumsikan model
# dengan satu strata galat saja.
#
# Untuk uji lanjut yang benar pada split plot, mahasiswa perlu
# menghitung galat baku (standard error) pembanding secara
# manual sesuai strata yang relevan (main plot error untuk
# irigasi, subplot error untuk pupuk dan interaksinya), atau
# menggunakan paket khusus seperti "agricolae" (fungsi
# LSD.test()/duncan.test() dengan spesifikasi derajat bebas dan
# kuadrat tengah galat yang sesuai strata). Cara yang lebih
# praktis dan modern ditunjukkan pada bagian 9 dengan paket
# "emmeans", setelah model mixed-effects dibangun pada bagian 7.
##
## Contoh kerangka pemikiran (bukan kode siap pakai):
##   - Ekstrak KTGalat & db galat strata "blok:irigasi" untuk
##     pembanding rata-rata antar level irigasi
##   - Ekstrak KTGalat & db galat strata "Within" untuk
##     pembanding rata-rata antar level pupuk / kombinasi
##     irigasi:pupuk

## -----------------------------------------------------------
## 6. PEMERIKSAAN ASUMSI MODEL
## -----------------------------------------------------------
# Diagnostik plot() standar (seperti pada Pertemuan 1-4) TIDAK
# tersedia secara langsung untuk objek aov() dengan Error(),
# karena residual tersebar di beberapa strata galat sekaligus.
# Pemeriksaan asumsi untuk split plot dilakukan setelah beralih
# ke pendekatan model mixed-effects (lihat bagian 7 & 8), yang
# menyediakan residual gabungan yang lebih mudah didiagnosis.

## -----------------------------------------------------------
## 7. PENDEKATAN MODERN: MODEL MIXED-EFFECTS (lme4)
## -----------------------------------------------------------
# Pendekatan Error() pada aov() (bagian 4) bersifat klasik dan
# hanya valid untuk struktur galat berjenjang yang SEIMBANG
# (balanced) -- setiap kombinasi blok x irigasi x pupuk harus
# lengkap tanpa data hilang. Ketika struktur data lebih kompleks
# atau tidak seimbang, pendekatan yang lebih fleksibel adalah
# MODEL LINEAR CAMPURAN (linear mixed-effects model).
#
# Dalam model mixed-effects:
# - "irigasi", "pupuk", dan interaksinya tetap menjadi EFEK
#   TETAP (fixed effect) -- inilah fokus pengujian, sama seperti
#   pada model aov() di bagian 4
# - "blok" dan petak utama "blok:irigasi" diperlakukan sebagai
#   EFEK ACAK (random effect), yang secara eksplisit
#   merepresentasikan strata galat berjenjang -- menggantikan
#   peran Error(blok/irigasi)
#
# install.packages("lme4")       # jalankan sekali saja jika belum terpasang
# install.packages("lmerTest")   # menambahkan nilai p pada model lme4
library(lme4)
library(lmerTest) # membungkus lmer() agar summary() turut menampilkan nilai p

# "(1 | blok/irigasi)" merepresentasikan struktur galat
# berjenjang yang sama seperti Error(blok/irigasi) pada aov():
# artinya ada efek acak "blok", dan efek acak "irigasi" yang
# tersarang (nested) di dalam tiap blok.
model_lmer <- lmer(
  tinggi_cm ~ irigasi * pupuk + (1 | blok / irigasi),
  data = data
)

# summary() menampilkan dua bagian utama:
# - "Random effects" -> variansi yang disumbangkan oleh blok,
#   oleh petak utama (blok:irigasi), dan oleh residual/anak
#   petak. Semakin besar variansi suatu strata, semakin besar
#   pula sumbangan keragamannya terhadap data.
# - "Fixed effects" -> dengan bantuan lmerTest, tabel ini turut
#   menampilkan nilai p untuk tiap koefisien, memakai
#   pendekatan derajat bebas Satterthwaite secara default.
summary(model_lmer)

# anova() pada model lmerTest menghasilkan tabel uji-F untuk
# tiap pengaruh utama & interaksi (irigasi, pupuk,
# irigasi:pupuk) sekaligus -- bentuknya lebih ringkas untuk
# dibandingkan langsung dengan tabel ANOVA klasik di bagian 4.
anova(model_lmer)


## -----------------------------------------------------------
## 8. DIAGNOSTIK MODEL MIXED-EFFECTS
## -----------------------------------------------------------
# Berbeda dengan model aov() + Error() pada bagian 4-6, model
# lmer() memiliki residual gabungan yang jelas sehingga
# diagnostik asumsi dapat dilakukan langsung, mirip seperti
# Pertemuan 1-4.

# (1) Normalitas residual: sebaran titik pada QQ-plot sebaiknya
# mengikuti garis diagonal.
qqnorm(residuals(model_lmer), main = "QQ-Plot Residual (model_lmer)")
qqline(residuals(model_lmer), col = "red")

# (2) Homogenitas ragam: plot nilai residual terhadap nilai
# dugaan (fitted). Sebaran titik idealnya tidak membentuk pola
# corong (semakin lebar/menyempit secara sistematis).
plot(
  fitted(model_lmer),
  residuals(model_lmer),
  xlab = "Nilai Dugaan (Fitted)",
  ylab = "Residual",
  main = "Residual vs Fitted (model_lmer)"
)
abline(h = 0, col = "red", lty = 2)

# (3) Uji normalitas formal sebagai pelengkap inspeksi visual
shapiro.test(residuals(model_lmer))


## -----------------------------------------------------------
## 9. UJI LANJUT PADA MODEL MIXED-EFFECTS
## -----------------------------------------------------------
# Paket "emmeans" menghitung rata-rata marjinal (estimated
# marginal means) beserta pembandingan berpasangan yang sudah
# otomatis menyesuaikan strata galat yang relevan -- inilah
# jawaban modern atas keterbatasan TukeyHSD() pada bagian 5.
#
# install.packages("emmeans")   # jalankan sekali saja jika belum terpasang
library(emmeans)

# Jika interaksi irigasi:pupuk SIGNIFIKAN (lihat hasil anova()
# di bagian 7), bandingkan rata-rata antar kombinasi:
emm_kombinasi <- emmeans(model_lmer, ~ irigasi * pupuk)
pairs(emm_kombinasi, adjust = "tukey")

# Jika interaksi TIDAK signifikan, cukup bandingkan pengaruh
# utama masing-masing faktor secara terpisah:
emm_irigasi <- emmeans(model_lmer, ~irigasi)
pairs(emm_irigasi, adjust = "tukey")

emm_pupuk <- emmeans(model_lmer, ~pupuk)
pairs(emm_pupuk, adjust = "tukey")

## =============================================================
## RINGKASAN ALUR ANALISIS:
## Baca data -> Cek struktur bertingkat (petak utama & anak
## petak) -> Deskriptif per kombinasi -> Visualisasi pola
## interaksi -> ANOVA klasik dengan Error(blok/irigasi) -> Baca
## hasil per strata galat -> Bangun model mixed-effects (lmer)
## sebagai pendekatan modern -> Diagnostik residual gabungan ->
## Uji lanjut dengan emmeans()
##
## PERBEDAAN UTAMA DENGAN PERTEMUAN SEBELUMNYA:
## - Ada DUA UKURAN unit percobaan berbeda (petak utama & anak
##   petak), bukan satu unit percobaan seragam
## - Model klasik (aov + Error()) memisahkan galat petak utama
##   dan galat anak petak secara implisit, tetapi terbatas pada
##   struktur data yang seimbang dan sulit diberi uji lanjut
## - Model mixed-effects (lmer) merepresentasikan strata galat
##   yang sama secara eksplisit lewat efek acak, sekaligus
##   membuka jalan untuk diagnostik residual & uji lanjut
##   (emmeans) yang lebih langsung -- inilah pendekatan "split
##   plot modern" yang lebih banyak dipakai di luar konteks
##   pengajaran dasar
## =============================================================
