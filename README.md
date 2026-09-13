# Biostatistika untuk Biologi — Analisis Data Eksperimental dengan R

Proyek RStudio ini menyertai **Buku Ajar Biostatistika untuk Biologi: Rancangan
Percobaan dan Analisis Ragam**. Berisi lima skrip R yang saling berkaitan,
mengikuti urutan bab pada buku ajar, beserta data contoh (sintetis) agar
setiap skrip dapat langsung dijalankan.

## Workflow

Import → Inspect → Explore → Visualize → Model → Diagnose →
Test/Post-hoc → Interpret → Report

## Prinsip interpretasi

Output statistik tidak boleh dibaca terpisah dari desain penelitian.
Mahasiswa harus selalu menghubungkan:

pertanyaan biologis → unit percobaan → desain → hipotesis → model →
output → kesimpulan biologis.

## R

Gunakan R versi relatif baru dan RStudio/Posit IDE bila diinginkan.
Package: dplyr, readr, ggplot2, lme4, lmerTest.

## Cara Memulai

1. Buka file **`BiostatistikaBiologi.Rproj`** dengan RStudio (klik dua kali,
   atau _File > Open Project_). RStudio akan otomatis mengatur _working
   directory_ ke folder proyek ini, sehingga seluruh path relatif pada skrip
   (`data/...`) berfungsi tanpa perlu diubah.
2. Buka skrip yang diinginkan di folder `scripts/` dan jalankan baris demi
   baris (Ctrl+Enter / Cmd+Enter), atau jalankan seluruhnya dengan tombol
   **Source**.
3. Paket R yang dibutuhkan (jalankan sekali saja jika belum terpasang):

   ```r
   install.packages(c("readr", "dplyr", "ggplot2", "lme4", "lmerTest", "emmeans"))
   ```

## Struktur Folder

```
BiostatistikaBiologi/
├── BiostatistikaBiologi.Rproj   <- buka file ini di RStudio
├── README.md                    <- file ini
├── scripts/                     <- lima skrip R, satu per bab
│   ├── 01_RAL_ANOVA_biostatistika.R
│   ├── 02_RAK_RCBD_biostatistika.R
│   ├── 03_RBSL_LatinSquare_biostatistika.R
│   ├── 04_RAK_Faktorial_biostatistika.R
│   └── 05_SplitPlot_biostatistika.R
├── data/                        <- data contoh (CSV) untuk tiap skrip
│   ├── 01_data_numerik_kontinu.csv
│   ├── 02_data_numerik_berkelompok.csv
│   ├── 03_data_numerik_dua_arah.csv
│   ├── 04_data_numerik_multifaktor.csv
│   └── 05_data_numerik_bertingkat.csv
└── output/                      <- kosong; simpan grafik/hasil ekspor di sini
```

## Padanan Skrip dan Bab Buku Ajar

| Skrip                                 | Bab   | Rancangan                            |
| ------------------------------------- | ----- | ------------------------------------ |
| `01_RAL_ANOVA_biostatistika.R`        | Bab 1 | Rancangan Acak Lengkap (RAL)         |
| `02_RAK_RCBD_biostatistika.R`         | Bab 2 | Rancangan Acak Kelompok (RAK)        |
| `03_RBSL_LatinSquare_biostatistika.R` | Bab 3 | Rancangan Bujur Sangkar Latin (RBSL) |
| `04_RAK_Faktorial_biostatistika.R`    | Bab 4 | RAK Faktorial (2 Faktor)             |
| `05_SplitPlot_biostatistika.R`        | Bab 5 | Rancangan Petak Terbagi (Split Plot) |

## Catatan tentang Data

Data pada folder `data/` bersifat **sintetis** (dibuat secara acak mengikuti
struktur rancangan masing-masing) dan hanya dimaksudkan agar kelima skrip
dapat langsung dijalankan sebagai demonstrasi. Untuk penggunaan nyata,
gantikan file CSV tersebut dengan data hasil pengamatan sendiri, dengan
mempertahankan nama kolom yang sama:

- **01**: `perlakuan`, `tinggi_cm`
- **02**: `perlakuan`, `blok`, `tinggi_cm`
- **03**: `baris`, `kolom`, `perlakuan`, `biomassa_g`
- **04**: `blok`, `pupuk`, `cahaya`, `tinggi_cm`
- **05**: `blok`, `irigasi`, `pupuk`, `tinggi_cm`
