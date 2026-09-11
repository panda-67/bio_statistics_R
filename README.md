# Biostatistika untuk Biologi — Analisis Data Eksperimental dengan R

Repository pendamping buku ajar Biostatistika untuk Biologi.

## Struktur pembelajaran

1. Data Numerik Kontinu — RAL dan one-way ANOVA
2. Data Numerik Berkelompok — RAK/RCBD
3. Data Numerik Dua Arah — RBLS/Latin Square
4. Data Numerik Multifaktor — RAK 2 faktorial dan interaksi
5. Data Numerik Bertingkat — Rancangan Petak Terbagi

## Workflow

Import → Inspect → Explore → Visualize → Model → Diagnose →
Test/Post-hoc → Interpret → Report

## Data

Semua dataset dalam repository adalah dataset sintetis untuk
pembelajaran. Jangan menganggap angka di dalamnya sebagai
hasil penelitian nyata.

## R

Gunakan R versi relatif baru dan RStudio/Posit IDE bila diinginkan.
Package: dplyr, readr, ggplot2, lme4, lmerTest.

## Cara menjalankan

Dari root repository:

```r
install.packages(c("readr", "dplyr", "ggplot2", "lme4", "lmerTest"))
```

Kemudian jalankan script pada folder `scripts/` sesuai pertemuan.

## Prinsip interpretasi

Output statistik tidak boleh dibaca terpisah dari desain penelitian. Mahasiswa harus selalu menghubungkan:

pertanyaan biologis → unit percobaan → desain → hipotesis → model → output → kesimpulan biologis.
