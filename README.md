# 🌏 Climate Change Dashboard - Kelompok 12

**Dashboard Perubahan Iklim: Analisis Emisi Berdasarkan Sektor di Indonesia**

Dashboard ini dikembangkan oleh Kelompok 12 (Nur Na’imah Ma’ruf, Nuzul Athaillah, Rifa Fairuz) dalam rangka proyek mata kuliah **Komputasi Statistik** di Politeknik Statistika STIS.

Dashboard ini menyajikan data emisi gas rumah kaca (GRK) di Indonesia berdasarkan sektor dan provinsi, serta menyediakan fitur analisis inferensia regresi untuk eksplorasi mendalam.

---

## 🔗 Tautan Penting

- 📊 **Akses Dashboard**:  
  [https://ufnaimah.shinyapps.io/ClimateChangeDashboard_Kelompok12](https://ufnaimah.shinyapps.io/ClimateChangeDashboard_Kelompok12)

- 📄 **Proposal Proyek**:  
  [Link Proposal Google Drive](https://drive.google.com/...) *(ganti dengan link proposalmu)*

- ▶️ **Video Tutorial Penggunaan**:  
  [Tonton di YouTube](https://youtu.be/DDpdxGWcOVg)

- 📄 **Panduan PDF (User Guide)**  
  Tersedia tombol unduhan di dalam dashboard bagian *Tata Cara Penggunaan*

---

## 📌 Fitur Dashboard

- **Overview**: Gambaran umum emisi global dan Asia Tenggara
- **Emisi Provinsi**: Peta, tren, dan peringkat emisi di tiap provinsi
- **Emisi Sektor**: Top 3 sektor penyumbang emisi dan proporsinya
- **Inferensia Statistik**: Uji asumsi dan model regresi suhu vs emisi
- **Metadata**: Penjelasan sumber dan struktur data
- **Tentang Kami**: Informasi tim pengembang

---

## 📦 Package yang Digunakan

| Package       | Fungsi Utama |
|---------------|--------------|
| **shiny**         | Framework utama dashboard interaktif |
| **leaflet**       | Visualisasi peta choropleth |
| **ggplot2**       | Grafik tren dan distribusi |
| **plotly**        | Visualisasi interaktif (opsional) |
| **readxl / readr**| Membaca file Excel dan CSV |
| **dplyr / tidyr** | Manipulasi data |
| **forcats**       | Mengatur urutan kategori (faktor) |
| **scales**        | Format angka di grafik |
| **lmtest / car / MASS / sandwich** | Uji asumsi dan regresi |
| **sf**            | Data spasial (GeoJSON) |
| **corrplot**      | Korelasi variabel |
| **DT**            | Tabel interaktif |
| **janitor**       | Pembersihan nama kolom |
| **stringr / stringi** | Pemrosesan teks dan label |
| **shinyjs**       | Kontrol JavaScript (opsional interaktif) |

---

## 🗂️ Struktur Folder
Projek RShiny/
│
├── app.R # File utama dashboard
├── www/
│ ├── Panduan_Climate_Change_Dashboard.pdf
│ └── custom CSS / assets lainnya
├── data/
│ ├── [File Excel dan CSV]
│ └── [GeoJSON]
├── README.md
├── .gitignore

👩‍💻 Tim Pengembang
Nur Na’imah Ma’ruf
Nuzul Athaillah
Rifa Fairuz

Politeknik Statistika STIS - 2025
