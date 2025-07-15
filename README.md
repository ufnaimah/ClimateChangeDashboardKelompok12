# 🌏 Climate Change Dashboard - Kelompok 12

**Dashboard Interaktif Emisi Gas Rumah Kaca (GRK) di Indonesia berdasarkan Sektor dan Provinsi**

Dashboard ini dikembangkan dalam rangka tugas akhir mata kuliah **Komputasi Statistik** oleh mahasiswa Politeknik Statistika STIS. Aplikasi ini menyajikan informasi visual dan analisis statistik terkait emisi GRK nasional.

---

## 🔗 Tautan Penting

- 📊 **Akses Dashboard (shinyapps.io)**  
  [https://ufnaimah.shinyapps.io/ClimateChangeDashboard_Kelompok12](https://kmfotw-nur-na0imah0ma0ruf.shinyapps.io/ClimateChangeDashboard_Kelompok12/)

- 📄 **Proposal Proyek**  
  [Link Proposal](https://drive.google.com/drive/folders/1_-xC9ZlX7lSLX7i9R6yrXEK7-WlrftyT?usp=sharing)

- ▶️ **Video Tutorial Penggunaan Dashboard**  
  [Tonton Video di YouTube](https://youtu.be/DDpdxGWcOVg)

- 📘 **Panduan Penggunaan (PDF)**  
  [Link Panduan Penggunaan](https://drive.google.com/drive/folders/1_-xC9ZlX7lSLX7i9R6yrXEK7-WlrftyT?usp=sharing)

---

## 📌 Fitur Utama

- **Overview Global & ASEAN**: Perbandingan emisi Indonesia dengan negara-negara ASEAN
- **Provinsi**: 
  - Top 3 provinsi emisi tertinggi
  - Tren emisi per provinsi
  - Peta choropleth berdasarkan total emisi per tahun
- **Sektor**:
  - Top 3 sektor penyumbang emisi
  - Distribusi emisi tiap sektor dalam pie chart
- **Inferensia Statistik**:
  - Uji asumsi regresi
  - Perbandingan model linier dan log-lin
- **Metadata**: Penjelasan sumber data
- **Tentang Kami**: Informasi tim pengembang

---

## 🗂️ Struktur Folder
<pre> Projek RShiny-FInal/
│
├── appPage012345.R # File utama dashboard R Shiny
├── README.md # Dokumentasi proyek
├── .gitignore # Daftar file/folder yang diabaikan Git
│
├── www/ # Folder untuk file statis
│ ├── Panduan_Climate_Change_Dashboard.pdf # File panduan PDF
│ ├── custompage1.css # File CSS kustom
│ ├── anggota1.jpg
│ ├── anggota2.jpg
│ ├── anggota3.jpg
│ ├── foto_kelompok.jpg
│ ├── background.jpg
| ├── Data Regresi.xlsx
| ├── Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023.xlsx
│ ├── Emisi Perkapita Negara Asia Tenggara.xlsx
│ ├── Emisi Tahunan Global, Asia, dan Indonesia.xlsx
│ ├── Suhu Rata-rata Permukaan Global, Asia, dan Indonesia.xlsx
│ ├── Tren Emisi Per Provinsi 2000-2023.xlsx
│ └── gadm41_IDN_1.json 
│
├── data/ # Folder data
| ├── Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023.xlsx
│ ├── Emisi Perkapita Negara Asia Tenggara.xlsx
│ ├── Emisi Tahunan Global, Asia, dan Indonesia.xlsx
│ ├── Suhu Rata-rata Permukaan Global, Asia, dan Indonesia.xlsx
│ ├── Tren Emisi Per Provinsi 2000-2023.xlsx
│ └── gadm41_IDN_1.json  
└── rsconnect </pre>
---

## 📦 Package yang Digunakan

| Package       | Fungsi Utama |
|---------------|--------------|
| **shiny**         | Framework utama dashboard interaktif |
| **leaflet**       | Visualisasi peta choropleth |
| **ggplot2**       | Grafik tren dan distribusi |
| **plotly**        | Visualisasi interaktif |
| **readxl / readr**| Membaca file Excel dan CSV |
| **dplyr / tidyr** | Manipulasi dan transformasi data |
| **forcats**       | Mengatur urutan kategori (faktor) |
| **scales**        | Format label angka dan sumbu |
| **lmtest / car / MASS / sandwich** | Uji asumsi dan regresi |
| **sf**            | Baca dan olah data spasial (GeoJSON) |
| **DT**            | Tabel interaktif |
| **janitor**       | Pembersihan nama kolom |
| **stringr / stringi** | Pemrosesan string |
| **shinyjs**       | Tambahan interaktivitas di UI |
| **corrplot**      | Visualisasi matriks korelasi |

---

## 👩‍💻 Tim Pengembang
<pre>
 - Nur Na’imah Ma’ruf
 - Nuzul Athaillah
 - Rifa Fairuz
</pre>

## Politeknik Statistika STIS – 2025

