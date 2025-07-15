

library(shiny)
library(leaflet)
library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(DT)
library(forcats)
library(scales)
library(ggplot2)
library(lmtest)
library(car)
library(MASS)
library(sandwich)
library(plotly)
library(sf)
library(forcats)
library(shinyjs)
library(dplyr)
library(corrplot)
library(janitor)
library(stringr)
library(stringi)

# Fix fungsi yang tabrakan
select <- dplyr::select
recode <- dplyr::recode
mutate <- dplyr::mutate

# ===== DATA METADATA (SEBELUM UI) =====
metadata_raw <- data.frame(
  Jenis_Data = c("Data Emisi GRK", "Data Emisi GRK", "Data Emisi GRK", "Data Emisi GRK", "Data Suhu", "Data Tambahan"),
  Nama_Data = c(
    "Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023",
    "Emisi Perkapita Negara Asia Tenggara",
    "Emisi Tahunan Global, Asia, dan Indonesia",
    "Tren Emisi Per Provinsi 2000-2023",
    "Suhu Rata-rata Permukaan Global, Asia, dan Indonesia",
    "GADM Indonesia (GeoJSON Provinsi)"
  ),
  Penyedia = c("KLHK", "Our World in Data", "Our World in Data", "KLHK", "Our World in Data", "GADM"),
  Tahun = c("2000–2023", "2000–2023", "2000–2023", "2000–2023", "2000–2024", "-"),
  Link = c(
    "https://signsmart.menlhk.go.id/v2.1/app/chart/emisi_m",
    "https://ourworldindata.org/co2-and-greenhouse-gas-emissions",
    "https://ourworldindata.org/co2-and-greenhouse-gas-emissions",
    "https://signsmart.menlhk.go.id/v2.1/app/chart/emisi_m",
    "https://ourworldindata.org/grapher/average-monthly-surface-temperature",
    "https://gadm.org/download_country.html"
  ),
  File = c(
    "Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023.xlsx",
    "Emisi Perkapita Negara Asia Tenggara.xlsx",
    "Emisi Tahunan Global, Asia, dan Indonesia.xlsx",
    "Tren Emisi Per Provinsi 2000-2023.xlsx",
    "Suhu Rata-rata Permukaan Global, Asia, dan Indonesia.xlsx",
    "gadm41_IDN_1.json"
  ),
  stringsAsFactors = FALSE
)

detail_texts <- list(
  "Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023" = list(
    desc = HTML("<b>Deskripsi:</b><br/>Dataset ini memuat jumlah emisi gas rumah kaca berdasarkan sektor di Indonesia untuk periode tahun 2000–2023. Data disajikan dalam satuan Gigagram CO₂ ekuivalen (GGCO₂e).
                Data ini juga terdapat pada Laporan Inventarisasi Gas Rumah Kaca (Grk) Dan Monitoring, Pelaporan, Verifikasi (Mpv) 2024 yang dengan tahun 2000-2019 telah dipublikasikan oleh BPS.
                </br>"),
    table = data.frame(
      `Variable Name` = c("Tahun", "Sektor", "Emisi"),
      `Description` = c("Tahun pengukuran", "Kategori sektor", "Jumlah emisi dalam GGCO₂e"),
      `Type` = c("Numeric", "String", "Numeric"),
      `Scale` = c("Scale", "Nominal", "Scale")
    )
  ),
  "Emisi Perkapita Negara Asia Tenggara" = list(
    desc = HTML("<b>Deskripsi:</b><br/>Dataset ini mencakup 11 negara ASEAN dan menyajikan data emisi gas rumah kaca per kapita."),
    table = data.frame(
      `Variable Name` = c("Tahun", "Negara", "Emisi"),
      `Description` = c("Tahun pengukuran", "Nama negara", "Emisi per kapita (ton CO₂e)"),
      `Type` = c("Numeric", "String", "Numeric"),
      `Scale` = c("Scale", "Nominal", "Scale")
    )
  ),
  "Emisi Tahunan Global, Asia, dan Indonesia" = list(
    desc = HTML("<b>Deskripsi:</b><br/>Dataset ini menyajikan data emisi gas rumah kaca dari Global, Asia, dan Indonesia."),
    table = data.frame(
      `Variable Name` = c("Tahun", "Wilayah", "Emisi"),
      `Description` = c("Tahun pengukuran", "Wilayah geografis", "Emisi per kapita (ton CO₂e)"),
      `Type` = c("Numeric", "String", "Numeric"),
      `Scale` = c("Scale", "Nominal", "Scale")
    )
  ),
  "Tren Emisi Per Provinsi 2000-2023" = list(
    desc = HTML("<b>Deskripsi:</b><br/>Dataset ini berisi tren emisi gas rumah kaca di tingkat provinsi di Indonesia."),
    table = data.frame(
      `Variable Name` = c("Tahun", "Provinsi", "Emisi"),
      `Description` = c("Tahun pengukuran", "Nama provinsi", "Jumlah emisi dalam GGCO₂e"),
      `Type` = c("Numeric", "String", "Numeric"),
      `Scale` = c("Scale", "Nominal", "Scale")
    )
  ),
  "Suhu Rata-rata Permukaan Global, Asia, dan Indonesia" = list(
    desc = HTML("<b>Deskripsi:</b><br/>Dataset ini menyajikan suhu rata-rata permukaan udara Global, Asia, dan Indonesia."),
    table = data.frame(
      `Variable Name` = c("Tahun", "Wilayah", "Suhu"),
      `Description` = c("Tahun pengamatan", "Wilayah geografis", "Suhu (°C)"),
      `Type` = c("Numeric", "String", "Numeric"),
      `Scale` = c("Scale", "Nominal", "Scale")
    )
  ),
  "GADM Indonesia (GeoJSON Provinsi)" = list(
    desc = HTML("<b>Deskripsi:</b><br/>GeoJSON batas provinsi di Indonesia untuk visualisasi spasial di dashboard."),
    table = data.frame(
      `Variable Name` = c("GID_1", "NAME_1", "geometry"),
      `Description` = c("ID Provinsi", "Nama Provinsi", "Batas wilayah"),
      `Type` = c("String", "String", "Geometry"),
      `Scale` = c("Nominal", "Nominal", "Spasial")
    )
  )
)


# FUNGSI BANTUAN UNTUK MEMBUAT INFO BOX MODERN
infoBoxR <- function(value, title, icon_name, bg_color = "#eee") {
  div(
    class = "info-box",
    style = paste0("padding: 20px; border-radius: 15px; color: white; background: ", bg_color, ";"),
    tags$i(class = paste0("fa fa-", icon_name), style = "font-size: 30px;"),
    h4(title),
    h2(value, style = "font-weight: bold;")
  )
}


#=======DATA=======#
metadata_raw <- data.frame(
  Jenis_Data = c("Data Emisi GRK", "Data Emisi GRK", "Data Emisi GRK", "Data Emisi GRK", "Data Suhu", "Data Tambahan"),
  Nama_Data = c(
    "Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023",
    "Emisi Perkapita Negara Asia Tenggara",
    "Emisi Tahunan Global, Asia, dan Indonesia",
    "Tren Emisi Per Provinsi 2000-2023",
    "Suhu Rata-rata Permukaan Global, Asia, dan Indonesia",
    "GADM Indonesia (GeoJSON Provinsi)"
  ),
  Penyedia = c("KLHK", "Our World in Data", "Our World in Data", "KLHK", "Our World in Data", "GADM"),
  Tahun = c("2000–2023", "2000–2023", "2000–2023", "2000–2023", "2000–2024", "-"),
  Link = c(
    "https://signsmart.menlhk.go.id/v2.1/app/chart/emisi_m",
    "https://ourworldindata.org/co2-and-greenhouse-gas-emissions",
    "https://ourworldindata.org/co2-and-greenhouse-gas-emissions",
    "https://signsmart.menlhk.go.id/v2.1/app/chart/emisi_m",
    "https://ourworldindata.org/grapher/average-monthly-surface-temperature",
    "https://gadm.org/download_country.html"
  ),
  File = c(
    "Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023.xlsx",
    "Emisi Perkapita Negara Asia Tenggara.xlsx",
    "Emisi Tahunan Global, Asia, dan Indonesia.xlsx",
    "Tren Emisi Per Provinsi 2000-2023.xlsx",
    "Suhu Rata-rata Permukaan Global, Asia, dan Indonesia.xlsx",
    "gadm41_IDN_1.json"
  ),
  stringsAsFactors = FALSE
)

#============================== ******** USER INTERFACE ******** ==============================#
ui <- navbarPage(title = div(class = "navbar-title-container",
                             span("Dashboard Perubahan Iklim", class = "main-title"),
                             span("Analisis Emisi Berdasarkan Sektor di Indonesia", class = "sub-title")
),                
                 # ==========================
                 # TAB: OVERVIEW
                 # ==========================
                 tabPanel("Overview",
                          fluidPage(
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "custompage1.css")
                            ),
                            
                            # === JUDUL UTAMA ===
                            tags$h2(
                              "Overview Perubahan Iklim dan Emisi",
                              style = "text-align: center; font-size: 32px; font-weight: bold; margin-top: 20px; margin-bottom: 30px;"
                            ),
                            
                            # === BAGIAN 1: VALUE BOX & FILTER TAHUN ===
                            wellPanel(
                              div(style = "margin-bottom: 25px;",
                                  selectInput("year_overview", "Pilih Tahun (Overview):", choices = NULL)
                              ),
                              
                              fluidRow(
                                column(4,
                                       infoBoxR(
                                         value = textOutput("shareEmissionValue"),
                                         title = "Share Emisi Global",
                                         icon_name = "percent",
                                         bg_color = "linear-gradient(135deg, #6f42c1, #a98eda)"
                                       )
                                ),
                                column(4,
                                       infoBoxR(
                                         value = textOutput("indonesiaEmissionValue"),
                                         title = "Emisi Indonesia (miliar ton CO₂eq)",
                                         icon_name = "smog",
                                         bg_color = "linear-gradient(135deg, #fd7e14, #ffb14e)"
                                       )
                                ),
                                column(4,
                                       infoBoxR(
                                         value = textOutput("globalEmissionValue"),
                                         title = "Emisi Global (miliar ton CO₂eq)",
                                         icon_name = "globe-asia",
                                         bg_color = "linear-gradient(135deg, #20c997, #59e0b8)"
                                       )
                                )
                              )
                            ),
                            br(),
     
                            # === BAGIAN 2: PENJELASAN & TABEL ASEAN ===
                            wellPanel(
                              fluidRow(
                                # KIRI: Penjelasan
                                column(6,
                                       h3("Penjelasan Singkat"),
                                       h4("Apa itu Perubahan Iklim?"),
                                       p("Perubahan iklim adalah pergeseran jangka panjang suhu dan cuaca Bumi, terutama akibat aktivitas manusia seperti pembakaran bahan bakar fosil dan deforestasi.erubahan iklim tidak hanya memengaruhi lingkungan, tetapi juga kehidupan sosial, ekonomi, dan ekosistem global, menjadikannya tantangan kritis yang membutuhkan perhatian dan tindakan segera dari seluruh lapisan masyarakat."),
                                       
                                       h4("Penyebab Perubahan Iklim"),
                                       p("Pemicu utama perubahan iklim adalah meningkatnya konsentrasi gas rumah kaca (GRK) di atmosfer, seperti karbon dioksida (CO₂), metana (CH4), dan dinitrogen oksida (N2O). Gas-gas ini dihasilkan dari berbagai aktivitas manusia."),
                                       
                                       h4("Dampak Perubahan Iklim"),
                                       p("Perubahan iklim membawa konsekuensi yang luas dan komplek. Kenaikan suhu global memicu peristiwa cuaca ekstrem. Naiknya permukaan air laut akibat mencairnya es di kutub dan glasier mengancam wilayah pesisir dan pulau-pulau kecil. Ekosistem alami menghadapi ancaman kepunahan spesies, sementara ketahanan pangan global terancam oleh menurunnya hasil pertanian dan perikanan. Dampak ini juga memengaruhi kesehatan manusia, memperburuk penyakit terkait panas dan memperluas penyebaran penyakit menular, serta memicu migrasi massal akibat bencana lingkungan."),
                                       
                                       h4("Solusi Perubahan Iklim"),
                                       p("Gunakan energi terbarukan, kurangi emisi gas rumah kaca, dan lindungi ekosistem alami untuk masa depan yang berkelanjutan.")
                                ),
                                
                                # KANAN: Tabel Peringkat ASEAN
                                column(6,
                                       h4("Peringkat Emisi Gas Rumah Kaca Per Kapita di Asia Tenggara"),
                                       selectInput("tahun", "Pilih Tahun:", choices = NULL),
                                       DTOutput("rankingTable")
                                )
                              )
                            ),
                            br(), # Memberi jarak
                            
                            # === BAGIAN 3: VIDEO NASA ===
                            wellPanel(
                              h2("Tonton Video Perubahan Iklim dari NASA", style="text-align:center;"),
                              div(class = "video-container",
                                  HTML('
                                    <iframe width="700" height="394" 
                                      src="https://www.youtube.com/embed/YfWCUYX2_U0?si=oN13ycjx01wuB17M" 
                                      frameborder="0" 
                                      allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
                                      allowfullscreen>
                                    </iframe>
                                  ')
                                )
                              ),
                            br(), # Memberi jarak
                            
                            # === BAGIAN 4: GRAFIK TREN ===
                            wellPanel(
                              # --- Judul untuk bagian grafik ---
                              div(style = "text-align: center; margin-bottom: 20px;",
                                  h3("Bagaimana kondisi suhu dan emisi Indonesia dibandingkan dengan negara-negara Asia dan Global?"),
                                  p("Grafik di bawah ini memberikan gambaran tren terkini.", style = "color: #6c757d;")
                              ),
                              
                              fluidRow(
                                column(6,
                                       wellPanel(
                                         h4("Tren Suhu Rata-Rata Permukaan"),
                                         checkboxGroupInput("filterSuhu", "Tampilkan Suhu Wilayah:",
                                                            choices = c("Global", "Asia", "Indonesia"),
                                                            selected = c("Global", "Asia", "Indonesia"),
                                                            inline = TRUE),
                                         plotOutput("suhuChart", height = "300px")
                                       )
                                ),
                                column(6,
                                       wellPanel(
                                         h4("Tren Emisi Indonesia vs Asia vs Global"),
                                         checkboxGroupInput("filterEmisi", "Tampilkan Tren Emisi:",
                                                            choices = c("Indonesia", "Asia", "Global"),
                                                            selected = c("Indonesia", "Asia", "Global"),
                                                            inline = TRUE),
                                         plotOutput("gabunganEmisiChart", height = "300px")
                                       )
                                )
                              )
                            ),
                            br(), # Memberi jarak
                            
                            # === BAGIAN 5: VIDEO TATA CARA PENGGUNAAN ===
                            div(style = "max-width: 800px; margin: 0 auto;",
                                wellPanel(  
                                  h3("Tata Cara Penggunaan Dashboard", style = "text-align:center;"),
                                  
                                  div(style = "text-align:center; margin-bottom: 15px;",
                                      downloadButton("downloadGuide", "Unduh Panduan PDF")
                                  ),
                                  
                                  div(class = "video-container",
                                      HTML('
                                        <iframe width="100%" height="400" 
                                          src="https://www.youtube.com/embed/DDpdxGWcOVg"
                                          frameborder="0" 
                                          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
                                          allowfullscreen>
                                        </iframe>
                                      ')
                                    )
                                )
                            ),
                            
                            br(), # Memberi jarak
                            
                            # === BAGIAN 6: QUOTE MOTIVASI ===
                            div(
                              style = "text-align: center; margin: 40px 20px;",
                              div(
                                style = "border-top: 2px solid #fff; width: 100%; margin: 0 auto; padding-top: 20px;"
                              ),
                              p(
                                '"Bumi adalah apa yang kita semua miliki bersama."',
                                style = "font-size: 1.5em; text-align: center; font-style: italic; color: #f8f9fa; text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5); margin-top: 15px; margin-bottom: 5px; cursor: pointer; transition: transform 0.2s ease-in-out;",
                                onmouseover = "this.style.transform = 'scale(1.05)';",
                                onmouseout = "this.style.transform = 'scale(1.0)';"
                              ),
                              p(
                                "– Wendell Berry",
                                style = "font-size: 1.1em; text-align: center; color: #ddd; text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.5); margin-top: 10px;"
                              ),
                              div(
                                style = "border-bottom: 2px solid #fff; width: 100%; margin: 20px auto; padding-bottom: 20px;"
                              )
                            ),
                            br()
                          )
                 ),
                 
                 
                 # HALAMAN 2
                 navbarMenu("EMISI",
                            
                            # ----------- SUB-TAB 1: PROVINSI -----------
                            tabPanel("Provinsi",
                                     fluidPage(
                                       tags$head(
                                         tags$link(rel = "stylesheet", type = "text/css", href = "custompage1.css")
                                       ),
                                       
                                       fluidRow(
                                         column(6,
                                                wellPanel(
                                                  h4("Top 3 Provinsi dengan Emisi Tertinggi"),
                                                  selectInput("tahunTop3", "Pilih Tahun:", choices = NULL),
                                                  plotOutput("barChartProvinsi", height = "250px"),
                                                  br(),
                                                  uiOutput("interpretasiBarProvinsi")
                                                )
                                         ),
                                         column(6,
                                                wellPanel(
                                                  h4("Tren Emisi per Provinsi"),
                                                  selectInput("provinsiTren", "Pilih Provinsi:", choices = NULL),
                                                  plotOutput("lineChartProvinsi", height = "250px"),
                                                  br(),
                                                  uiOutput("interpretasiLineProvinsi")
                                                )
                                         )
                                       ),
                                       fluidRow(
                                         column(12,
                                                wellPanel(
                                                  h4("Peta Emisi Provinsi Indonesia"),
                                                  selectInput("tahunPeta", "Pilih Tahun:", choices = NULL),
                                                  leafletOutput("mapChart", height = "450px"),
                                                  br(),
                                                  uiOutput("interpretasiPetaProvinsi")
                                                  
                                                )
                                         )
                                       )
                                     )
                            ),
                            
                            # ----------- SUB-TAB 2: SEKTOR -----------
                            tabPanel("Sektor",
                                     fluidPage(
                                       fluidRow(
                                         column(12,
                                                wellPanel(
                                                  h4("Penjelasan Sektor IPPC dan BPS"),
                                                  p("Bagian ini menyajikan data emisi gas rumah kaca (GRK) berdasarkan klasifikasi sektor yang digunakan dalam inventarisasi nasional Indonesia, mengacu pada pedoman IPCC (Intergovernmental Panel on Climate Change)."),
                                                  p("Sektor-sektor yang ditampilkan meliputi: Energi, Proses Industri dan Penggunaan Produk (IPPU), Pertanian, Kehutanan dan Penggunaan Lahan (FOLU), serta Limbah."),
                                                  p("Pendekatan ini mencerminkan total emisi dari seluruh aktivitas utama penyumbang GRK, baik dari penggunaan energi maupun non-energi. Visualisasi ini berguna untuk memahami kontribusi masing-masing sektor terhadap total emisi nasional.")
                                                ),
                                                
                                                fluidRow(
                                                  column(6,
                                                         wellPanel(
                                                           h3("Top 3 Sektor Penyumbang Emisi"),
                                                           selectInput("tahunSektorBar", "Pilih Tahun untuk Top Sektor:", choices = NULL), 
                                                           plotOutput("barChartSektorIPPU", height = 350),
                                                           br(),
                                                           uiOutput("interpretasiBarSektor")
                                                         )
                                                  ),
                                                  column(6,
                                                         wellPanel(
                                                           h3("Persentase Emisi per Sektor"),
                                                           selectInput("tahunSektorPie", "Pilih Tahun untuk Persentase Sektor:", choices = NULL), 
                                                           plotOutput("pieChartSektorIPPU", height = 350),
                                                           br(),
                                                           uiOutput("interpretasiPieSektor")
                                                         )
                                                  )
                                                )
                                         )
                                       )
                                     )
                            ) # <-- Akhir dari tabPanel Sektor (tidak perlu koma jika ini argumen terakhir)
                            
                 ), # <-- Akhir dari navbarMenu
                 
                 # HALAMAN 4 (INFERENSIA)
                 tabPanel("Inferensia",
                          fluidPage(
                            # Menambahkan tag untuk menyertakan CSS
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "custompage1.css")
                            ),
                            
                            h2("Analisis Inferensia: Pengaruh Suhu berdasarkan Sektor terhadap Climate Change", align = "center"),
                            div(
                              style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; margin-bottom: 15px;",
                              p("Catatan: Variabel 'limbah' tidak dimasukkan dalam model regresi karena memiliki nilai VIF (Variance Inflation Factor) di atas 10, yang menunjukkan adanya multikolinearitas kuat dengan variabel independen lain. Penghapusan ini dilakukan untuk menjaga kestabilan dan interpretabilitas model.", style = "color: #555; font-style: italic; text-align:justify;")
                            ),
                            fluidRow(
                              column(7,  # Kiri: Pengujian Asumsi
                                     tabsetPanel(
                                       tabPanel("Linearitas",
                                                # MENAMBAHKAN CLASS 'custom-subtitle' DI SINI
                                                h5("Visualisasi Linearitas", class = "custom-subtitle"),
                                                plotOutput("plotLinearitas"),
                                                verbatimTextOutput("interpretasiLinearitas")
                                       ),
                                       tabPanel("Korelasi",
                                                h5("Matriks Korelasi Antar Variabel", class = "custom-subtitle"),
                                                plotOutput("plotKorelasi"),
                                                uiOutput("interpretasiKorelasi")
                                       ),
                                       # (Tambahkan class = "custom-subtitle" juga pada h5 di tab lainnya jika perlu)
                                       tabPanel("Normalitas",
                                                h5("Q-Q Plot", class = "custom-subtitle"),
                                                plotOutput("qqplotResidual"),
                                                h5("Shapiro-Wilk Test", class = "custom-subtitle"),
                                                verbatimTextOutput("shapiroTest")
                                       ),
                                       tabPanel("Homoskedastisitas",
                                                h5("Uji Breusch–Pagan", class = "custom-subtitle"),
                                                verbatimTextOutput("ujiBP")
                                       ),
                                       tabPanel("Autokorelasi",
                                                h5("Durbin–Watson Test", class = "custom-subtitle"),
                                                verbatimTextOutput("dwtestResult")
                                       ),
                                       tabPanel("Outlier",
                                                h5("Studentized Residual", class = "custom-subtitle"),
                                                verbatimTextOutput("ujiOutlier"),
                                                h5("Cook's Distance", class = "custom-subtitle"),
                                                plotOutput("plotCooksDistance")
                                       ),
                                       tabPanel("Multikolinearitas",
                                                h5("VIF (Variance Inflation Factor)", class = "custom-subtitle"),
                                                tableOutput("tabelVIF"),
                                                verbatimTextOutput("interpretasiVIF")
                                       )
                                     )
                              ),
                              column(5,  # Kanan: Regresi
                                     tabsetPanel(
                                       tabPanel("Model Regresi",
                                                verbatimTextOutput("summaryRegresi"),
                                                # MENAMBAHKAN CLASS 'custom-subtitle' DI SINI
                                                h4("Interpretasi Hasil Regresi", class = "custom-subtitle"),
                                                uiOutput("interpretasiRegresi"),
                                                br(), br()
                                       )
                                     )
                              )
                            )
                          )
                 ),
                 
                 # HALAMAN 5 (METADATA)
                 tabPanel("Metadata",
                          fluidPage(
                            h2("Metadata Dataset Emisi dan Suhu"),
                            br(),
                            DTOutput("metadataTable")
                          )
                 ),
                 
                 # TENTANG KAMI
                 tabPanel("Tentang Kami",
                          fluidPage(
                            # ==== BAGIAN DESKRIPSI ====
                            h2("Tentang Kami", class = "about-us-title"),
                            div(
                              class = "about-us-container",
                              wellPanel(
                                p("     Kami adalah mahasiswa Politeknik Statistika STIS yang mengembangkan dashboard interaktif berbasis R Shiny sebagai bagian dari Praktikum Komputasi Statistik. Proyek ini bertujuan untuk menyajikan analisis emisi gas rumah kaca (GRK) dari berbagai sektor di Indonesia yang berkontribusi terhadap perubahan iklim."),
                                p("     Dashboard ini dikembangkan melalui proses yang panjang dan kolaboratif, mulai dari pengumpulan dan integrasi data dari berbagai sumber terpercaya, pembersihan data, validasi, hingga penentuan indikator utama yang relevan. Visualisasi yang disajikan mencakup tren emisi dari tahun 2000 hingga 2023, serta sebagian data tahun 2024 yang telah tersedia meskipun belum lengkap untuk seluruh sektor dan wilayah. Fitur-fitur utama meliputi analisis kontribusi sektor, distribusi spasial emisi per provinsi, dan visualisasi komparatif antartahun yang dapat dieksplorasi secara interaktif oleh pengguna."),
                                p("     Seluruh data yang digunakan telah melalui proses transformasi dan konsolidasi agar dapat dianalisis secara andal. Informasi ditampilkan dalam berbagai bentuk visual seperti grafik time series, pie chart kontribusi sektor, peta choropleth interaktif, dan ringkasan insight yang dirancang untuk meningkatkan pemahaman pengguna. Dashboard ini juga menyediakan fitur metadata agar pengguna dapat menelusuri detail struktur dan sumber data yang digunakan."),
                                p("     Dengan penyajian yang intuitif dan fleksibel, dashboard ini diharapkan menjadi kontribusi kecil namun bermakna dalam mendorong kesadaran kolektif terhadap isu perubahan iklim, serta memperkuat penggunaan data sebagai dasar pengambilan keputusan dan kebijakan di masa depan.")
                              ),
                            ),
                            
                            # ==== BAGIAN FOTO KELOMPOK ====
                            h2("Kelompok 12", class = "about-us-title"),
                            div(
                              class = "team-photo-container",
                              img(
                                src = "foto_kelompok.jpg",
                                class = "team-photo-img"
                              )
                            ),
                            
                            # ==== BAGIAN ANGGOTA TIM ====
                            h2("Anggota Tim", class = "about-us-title"),
                            div(
                              class = "team-members-container",
                              # -- Kartu Anggota --
                              div(class = "member-card",
                                  img(src = "anggota1.jpg", class = "member-img"),
                                  div(class = "member-info",
                                      h4("Nur Na'imah Ma'ruf", class = "member-name")
                                  )
                              ),
                              div(class = "member-card",
                                  img(src = "anggota2.jpg", class = "member-img"),
                                  div(class = "member-info",
                                      h4("Nuzul Athaillah", class = "member-name")
                                  )
                              ),
                              div(class = "member-card",
                                  img(src = "anggota3.jpg", class = "member-img"),
                                  div(class = "member-info",
                                      h4("Rifa Fairuz", class = "member-name")
                                  )
                              )
                            ),
                            
                            # ==== BAGIAN DESKRIPSI KOLABORASI ====
                            div(
                              class = "collaboration-container",
                              p("Dalam mengembangkan dashboard ini, setiap anggota tim berperan aktif sesuai dengan keahliannya. Kami saling mendukung dalam proses pengumpulan dan pembersihan data, eksplorasi visualisasi, analisis statistik, serta pengembangan fitur interaktif dengan R Shiny.")
                            ),
                            
                            # ==== BAGIAN QUOTE VINCENT VAN GOGH ====
                            div(
                              class = "quote-container-van-gogh",
                              div(
                                class = "quote-box",
                                HTML('"Great things are done by a series of small things brought together." <br>– <i>Vincent Van Gogh</i>')
                              )
                            )
                          )
                 ), 
                 
                 # === BAGIAN 7: FOOTER ===
                 footer = tags$footer(
                   style = "background-color: #f5f5f5; border-top: 1px solid #ddd; padding: 20px; text-align: center; color: #555; margin-top: 40px; width: 100%; position: relative; left: 0; bottom: 0;",
                   strong("Dashboard Climate Change Emission"),
                   tags$div("Kelompok 12 - Praktikum Komputasi Statistik")
                 )
)
#============================== ************ SERVER ************ ==============================#

    server <- function(input, output, session) {
      # === BACA DATA OVERVIEW ===
      data_overview <- reactive({
        df <- read_excel("data/Emisi Tahunan Global, Asia, dan Indonesia.xlsx")
        names(df) <- c("Negara", "Tahun", "Emisi")  # <=== PENTING: Rename kolom biar konsisten
        df
      })
      
      
      # DEBUG: LIHAT KOLOM ASLINYA
      observe({
        cat("DEBUG - Kolom data_overview:\n")
        print(names(data_overview()))
      })
      
      
      # === DIBENERIN DI SINI: PASTIKAN KOLOMNYA BENER DAN CASE-SENSITIVE ===
      observe({
        tahun_tersedia <- sort(unique(data_overview()$Tahun), decreasing = TRUE)
        updateSelectInput(session, "year_overview", choices = tahun_tersedia, selected = tahun_tersedia[1])
      })
      
      
      # === FILTER DATA SESUAI TAHUN ===
      # Amanin overview_filtered
      overview_filtered <- reactive({
        req(input$year_overview)
        df <- data_overview()
        
        req("Tahun" %in% colnames(df), "Negara" %in% colnames(df), "Emisi" %in% colnames(df))
        
        df %>% filter(Tahun == input$year_overview)
      })
      
      
      
      # === HITUNG RINGKASAN EMISI ===
      emission_summary <- reactive({
        df <- overview_filtered()
        indo <- df$Emisi[df$Negara == "Indonesia"]
        world <- df$Emisi[df$Negara == "World"]
        
        list(
          share = round((indo / world) * 100, 2),
          indo = indo,
          global = world
        )
      })
      
      
      # === OUTPUT VALUE BOX ===
      
      output$shareEmissionValue <- renderText({
        req(input$year_overview)  # ← penting
        paste0(emission_summary()$share, "%")
      })
      
      
      output$indonesiaEmissionValue <- renderText({
        req(input$year_overview)
        format(emission_summary()$indo, big.mark = ",", scientific = FALSE)
      })
      
      
      output$globalEmissionValue <- renderText({
        req(input$year_overview)
        format(emission_summary()$global, big.mark = ",")
      })
      
  
  data_emisi <- reactive({
    read_excel("data/Emisi Perkapita Negara Asia Tenggara.xlsx")
  })
  
  data_asean_long <- reactive({
    read_excel("data/Emisi Perkapita Negara Asia Tenggara.xlsx") %>%
      pivot_longer(cols = -Year, names_to = "Negara", values_to = "Emisi_Total") %>%
      rename(Tahun = Year)
  })
  
  # Isi dropdown tahun
  observe({
    data <- data_asean_long()
    tahun_choices <- sort(unique(data$Tahun), decreasing = TRUE)
    updateSelectInput(session, "tahun", 
                      choices = tahun_choices,
                      selected = tahun_choices[1])
  })
  
  # Data berdasarkan tahun
  data_tahun <- reactive({
    req(data_asean_long(), input$tahun)
    data_asean_long() %>%
      filter(Tahun == input$tahun) %>%
      arrange(desc(Emisi_Total)) %>%
      mutate(Peringkat = row_number()) %>%
      select(Peringkat, Negara, Emisi_Total) %>%
      mutate(Emisi_Total = round(Emisi_Total, 4))
  })
  
  # Render tabel
  output$rankingTable <- renderDT({
    datatable(data_tahun(),
              extensions = 'Buttons',
              options = list(
                paging = FALSE, # <-- Disables the 'Next' button and page navigation
                dom = 'Bfrt',   # <-- Removes the information summary ('i') and pagination ('p')
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ),
              rownames = FALSE,
              colnames = c('Peringkat', 'Negara', 'Emisi Total (ton CO₂e)'))
  })
  
  output$suhuChart <- renderPlot({
    req(input$filterSuhu)
    
    suhu_data <- read_excel("data/Suhu Rata-rata Permukaan Global, Asia, dan Indonesia.xlsx")
    
    suhu_long <- suhu_data %>%
      pivot_longer(cols = -Tahun, names_to = "Wilayah", values_to = "Suhu") %>%
      filter(Wilayah %in% input$filterSuhu)
    
    ggplot(suhu_long, aes(x = Tahun, y = Suhu, color = Wilayah)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Tren Rata-rata Suhu Permukaan (2000–2024)",
        x = "Tahun",
        y = "Suhu (°C)",
        color = "Wilayah"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = c(
        "Global" = "red",
        "Asia" = "steelblue",
        "Indonesia" = "darkgreen"
      )) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1))
  })
  
  output$gabunganEmisiChart <- renderPlot({
    req(input$filterEmisi)
    
    emisi_data <- read_excel("data/Emisi Tahunan Global, Asia, dan Indonesia.xlsx")
    
    emisi_long <- emisi_data %>%
      rename(Negara = 1, Tahun = 2, Emisi = 3) %>%
      mutate(
        Tahun = as.integer(Tahun),
        Emisi = as.numeric(Emisi),
        Wilayah = case_when(
          Negara == "World" ~ "Global",
          TRUE ~ Negara
        )
      ) %>%
      filter(Wilayah %in% input$filterEmisi)
    
    ggplot(emisi_long, aes(x = Tahun, y = Emisi, color = Wilayah)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Tren Emisi Gas Rumah Kaca: Global, Asia, dan Indonesia",
        x = "Tahun",
        y = "Emisi (juta ton CO₂)",
        color = "Wilayah"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = c(
        "Global" = "red",
        "Asia" = "steelblue",
        "Indonesia" = "darkgreen"
      )) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$downloadGuide <- downloadHandler(
    filename = function() {
      "Panduan_Climate_Change_Dashboard.pdf"
    },
    content = function(file) {
      file.copy("www/Panduan_Climate_Change_Dashboard.pdf", file)
    }
  )
  
  
  # HALAMAN 2
  
  # === BACA DATA PROVINSI (FORMAT WIDE → LONG) ===
  data_provinsi <- reactive({
    path <- "data/Tren Emisi Per Provinsi 2000-2023.xlsx"
    df_wide <- read_excel(path)
    names(df_wide)[1] <- "Tahun"
    
    df_long <- df_wide %>%
      pivot_longer(cols = -Tahun, names_to = "Provinsi", values_to = "Total Emisi") %>%
      mutate(
        Tahun = as.numeric(Tahun),
        Provinsi = stringi::stri_trans_general(Provinsi, "Latin-ASCII"),
        Provinsi = str_squish(Provinsi),  # Hanya rapikan, tidak hapus spasi
        Provinsi = toupper(Provinsi),
        `Total Emisi` = as.numeric(`Total Emisi`)
      ) %>%
      filter(!is.na(Tahun) & !is.na(`Total Emisi`))
    
    return(df_long)
  })
  
  
  
  # === UPDATE DROPDOWN BERDASARKAN DATA ===
  observe({
    df <- data_provinsi()
    tahun_choices <- sort(unique(df$Tahun))
    provinsi_choices <- unique(df$Provinsi)
    
    updateSelectInput(session, "tahunTop3", choices = tahun_choices, selected = max(tahun_choices))
    updateSelectInput(session, "tahunPeta", choices = tahun_choices, selected = max(tahun_choices))
    updateSelectInput(session, "provinsiTren", choices = provinsi_choices, selected = provinsi_choices[1])
  })
  
  # === TOP 3 BAR CHART ===
  output$barChartProvinsi <- renderPlot({
    req(data_provinsi(), input$tahunTop3)
    top_provinsi <- data_provinsi() %>%
      filter(Tahun == input$tahunTop3, Provinsi != "NASIONAL") %>%
      arrange(desc(`Total Emisi`)) %>%
      slice_head(n = 3)
    
    ggplot(top_provinsi, aes(x = reorder(Provinsi, `Total Emisi`), y = `Total Emisi`)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = format(`Total Emisi`, big.mark = ",")), hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(title = paste("Top 3 Provinsi Emisi Tahun", input$tahunTop3), x = "Provinsi", y = "Total Emisi (ton)") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  output$interpretasiBarProvinsi <- renderUI({
    req(input$tahunTop3)
    top <- data_provinsi() %>%
      filter(Tahun == input$tahunTop3, Provinsi != "NASIONAL") %>%
      arrange(desc(`Total Emisi`)) %>%
      slice_head(n = 3)
    
    if (nrow(top) == 0) return(NULL)
    
    HTML(paste0(
      "<p><b>Interpretasi:</b> Pada tahun ", input$tahunTop3, 
      ", tiga provinsi dengan emisi gas rumah kaca tertinggi adalah ",
      paste(top$Provinsi, collapse = ", "), 
      ". Provinsi ", top$Provinsi[1], 
      " memiliki emisi tertinggi sebesar ", 
      format(top$`Total Emisi`[1], big.mark = ".", decimal.mark = ","), " ton.</p>"
    ))
  })
  
  
  # === LINE CHART TREN ===
  output$lineChartProvinsi <- renderPlot({
    req(data_provinsi(), input$provinsiTren)
    data_tren <- data_provinsi() %>%
      filter(Provinsi == input$provinsiTren) %>%
      arrange(Tahun)
    
    if (nrow(data_tren) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("Tidak ada data untuk", input$provinsiTren), size = 5) +
        theme_void()
    } else {
      ggplot(data_tren, aes(x = Tahun, y = `Total Emisi`)) +
        geom_line(color = "darkgreen", size = 1.2) +
        geom_point(size = 3, color = "darkgreen") +
        labs(title = paste("Tren Emisi", input$provinsiTren), x = "Tahun", y = "Total Emisi (ton)") +
        scale_y_continuous(labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
        theme_minimal()
    }
  })
  
  output$interpretasiLineProvinsi <- renderUI({
    req(input$provinsiTren)
    tren <- data_provinsi() %>% filter(Provinsi == input$provinsiTren)
    
    if (nrow(tren) < 2) return(NULL)
    
    pertama <- tren$`Total Emisi`[1]
    terakhir <- tren$`Total Emisi`[nrow(tren)]
    arah <- if (terakhir > pertama) "meningkat" else if (terakhir < pertama) "menurun" else "stabil"
    
    HTML(paste0(
      "<p><b>Interpretasi:</b> Tren emisi di provinsi ", input$provinsiTren, 
      " dari tahun ", min(tren$Tahun), " hingga ", max(tren$Tahun), 
      " menunjukkan pola yang ", arah, 
      ". Emisi berubah dari ", 
      format(pertama, big.mark = ".", decimal.mark = ","), " ton menjadi ", 
      format(terakhir, big.mark = ".", decimal.mark = ","), " ton.</p>"
    ))
  })
  
  
  # === LOAD GEOJSON ===
  # === BACA DAN BERSIHKAN GEOJSON ===
  geo_prov <- st_read("data/gadm41_IDN_1.json") %>%
    mutate(NAME_1 = recode(NAME_1,
                           "SulawesiUtara" = "SULAWESI UTARA",
                           "SulawesiBarat" = "SULAWESI BARAT",
                           "SulawesiTengah" = "SULAWESI TENGAH",
                           "SulawesiSelatan" = "SULAWESI SELATAN",
                           "SulawesiTenggara" = "SULAWESI TENGGARA",
                           "BangkaBelitung" = "KEP. BANGKA BELITUNG",
                           "SumateraUtara" = "SUMATERA UTARA",
                           "SumateraBarat" = "SUMATERA BARAT",
                           "SumateraSelatan" = "SUMATERA SELATAN",
                           "JakartaRaya" = "DKI JAKARTA",
                           "Yogyakarta" = "DI YOGYAKARTA",
                           "KepulauanRiau" = "KEP. RIAU",
                           "JawaBarat" = "JAWA BARAT",
                           "JawaTengah" = "JAWA TENGAH",
                           "JawaTimur" = "JAWA TIMUR",
                           "KalimantanBarat" = "KALIMANTAN BARAT",
                           "KalimantanTengah" = "KALIMANTAN TENGAH",
                           "KalimantanSelatan" = "KALIMANTAN SELATAN",
                           "KalimantanTimur" = "KALIMANTAN TIMUR",
                           "KalimantanUtara" = "KALIMANTAN UTARA",
                           "MalukuUtara" = "MALUKU UTARA",
                           "PapuaBarat" = "PAPUA BARAT",
                           "DKIJakarta" = "DKI JAKARTA",
                           "DI Yogyakarta" = "DI YOGYAKARTA",
                           "NusaTenggaraBarat" = "NUSA TENGGARA BARAT",
                           "NusaTenggaraTimur" = "NUSA TENGGARA TIMUR",
                           "PAPUA" = "PAPUA"
    )) %>%
    mutate(NAME_1 = stringi::stri_trans_general(NAME_1, "Latin-ASCII"),
           NAME_1 = str_squish(NAME_1),
           NAME_1 = toupper(NAME_1))
  
  
  # === DATA UNTUK CHOROPLETH MAP ===
  data_choropleth <- reactive({
    df <- data_provinsi() %>%
      filter(Tahun == input$tahunPeta, Provinsi != "NASIONAL") %>%
      group_by(Provinsi) %>%
      summarise(`Total Emisi` = sum(`Total Emisi`, na.rm = TRUE))
    
    geo_merged <- geo_prov %>%
      left_join(df, by = c("NAME_1" = "Provinsi"))
    
    return(geo_merged)
  })
  
  # === CHOROPLETH MAP ===
  output$mapChart <- renderLeaflet({
    shp <- data_choropleth()
    req(shp)
    
    if (all(is.na(shp$`Total Emisi`))) {
      return(leaflet() %>%
               addTiles() %>%
               addPopups(lng = 118, lat = -2.5, popup = "Data emisi tidak ditemukan untuk tahun tersebut."))
    }
    
    pal <- colorNumeric(palette = "YlOrRd", domain = shp$`Total Emisi`, na.color = "gray")
    
    leaflet(shp) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(`Total Emisi`),
        color = "white",  # biar jelas batasnya
        weight = 1,
        fillOpacity = 1,  # biar jelas
        popup = ~paste0("<strong>", NAME_1, "</strong><br>",
                        "Emisi: ", format(`Total Emisi`, big.mark = ".", decimal.mark = ","), " ton")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~`Total Emisi`,
        title = "Total Emisi",
        labFormat = labelFormat(suffix = " ton")
      )
    
  })
  
  output$interpretasiPetaProvinsi <- renderUI({
    req(data_choropleth())
    shp <- data_choropleth()
    
    if (all(is.na(shp$`Total Emisi`))) {
      return(HTML("<p><b>Interpretasi:</b> Data emisi tidak tersedia untuk tahun ini.</p>"))
    }
    
    top <- shp %>% filter(!is.na(`Total Emisi`)) %>% arrange(desc(`Total Emisi`)) %>% slice(1)
    bottom <- shp %>% filter(!is.na(`Total Emisi`)) %>% arrange(`Total Emisi`) %>% slice(1)
    rata2 <- mean(shp$`Total Emisi`, na.rm = TRUE)
    jumlah_prov <- nrow(filter(shp, !is.na(`Total Emisi`)))
    
    HTML(paste0(
      "<p><b>Interpretasi:</b> Pada tahun ", input$tahunPeta, 
      ", provinsi dengan emisi tertinggi adalah <b>", top$NAME_1, "</b> (", 
      format(top$`Total Emisi`, big.mark = ".", decimal.mark = ","), " ton), ",
      "sedangkan yang terendah adalah <b>", bottom$NAME_1, "</b> (", 
      format(bottom$`Total Emisi`, big.mark = ".", decimal.mark = ","), " ton). ",
      "Rata-rata emisi antar provinsi sebesar ", 
      format(rata2, big.mark = ".", decimal.mark = ","), 
      " ton, dari total ", jumlah_prov, " provinsi yang memiliki data.</p>"
    ))
  })
  
  
  observe({
    print("======= Provinsi dari Excel (data_provinsi) =======")
    print(unique(data_provinsi()$Provinsi))
    
    print("======= Provinsi dari GeoJSON (geo_prov) =======")
    print(unique(geo_prov$NAME_1))
    
    # Cek perbedaan
    print("======= Yang ada di Geo tapi ga ada di Excel =======")
    print(setdiff(geo_prov$NAME_1, data_provinsi()$Provinsi))
    
    print("======= Yang ada di Excel tapi ga ada di Geo =======")
    print(setdiff(data_provinsi()$Provinsi, geo_prov$NAME_1))
  })
  
  
  # HALAMAN 3 - SERVER
  #====================================================================
  
  # A. MEMUAT DATA SEKTOR (REACTIVE)
  # -----------------------------------------------------------------
  data_sektor <- reactive({
    # PRAKTIK TERBAIK: Gunakan path relatif.
    # Buat folder "data" di dalam folder aplikasi Anda dan letakkan file Excel di sana.
    file_path <- "data/Emisi Gas Rumah Kaca Menurut Sektor di Indonesia 2000-2023.xlsx"
    
    validate(
      need(file.exists(file_path), 
           paste("File tidak ditemukan. Pastikan file Excel ada di dalam folder 'data'. Path yang dicari:", file_path))
    )
    
    data <- read_excel(file_path, sheet = 1) %>%
      rename_with(~ trimws(toupper(.x)))
    
    validate(
      need("TAHUN" %in% colnames(data), "Kolom 'TAHUN' tidak ditemukan."),
      need(any(c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH") %in% colnames(data)), 
           "Tidak ada kolom sektor relevan (ENERGI, IPPU, dll.).")
    )
    
    data <- data %>%
      mutate(TAHUN = readr::parse_number(as.character(TAHUN)))
    
    return(data)
  })
  
  # B. MEMPERBARUI INPUT DROPDOWN (OBSERVE)
  # -----------------------------------------------------------------
  observe({
    req(data_sektor())
    tahun_choices <- sort(unique(data_sektor()$TAHUN), decreasing = TRUE)
    
    validate(
      need(length(tahun_choices) > 0, "Tidak ada data tahun yang valid.")
    )
    
    updateSelectInput(session, "tahunSektorBar",
                      choices = tahun_choices,
                      selected = max(tahun_choices))
    
    updateSelectInput(session, "tahunSektorPie",
                      choices = tahun_choices,
                      selected = max(tahun_choices))
  })
  
  # Palet warna aman untuk colorblind
  cbPalette <- c(
    "ENERGI" = "#0072B2",     # biru
    "IPPU" = "#E69F00",       # oranye
    "PERTANIAN" = "#009E73",  # hijau
    "KEHUTANAN" = "#D55E00",  # merah bata
    "LIMBAH" = "#CC79A7"      # magenta gelap
  )
  
  
  # C. MEMBUAT PLOT (RENDERPLOT)
  # -----------------------------------------------------------------
  
  # Grafik Kiri: Bar Chart Top 3 Sektor
  output$barChartSektorIPPU <- renderPlot({
    req(data_sektor(), input$tahunSektorBar)
    
    sektor_relevan <- c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH")
    
    df <- data_sektor() %>%
      filter(TAHUN == input$tahunSektorBar) %>%
      select(TAHUN, any_of(sektor_relevan)) %>%
      pivot_longer(cols = -TAHUN, names_to = "Sektor", values_to = "Emisi") %>%
      mutate(Emisi = as.numeric(gsub(",", ".", Emisi))) %>%
      arrange(desc(Emisi)) %>%
      slice_head(n = 3)
    
    validate(
      need(nrow(df) > 0, "Tidak ada data untuk tahun yang dipilih.")
    )
    
    ggplot(df, aes(x = fct_reorder(Sektor, Emisi), y = Emisi, fill = Sektor)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = paste("Top 3 Sektor Emisi GRK pada Tahun", input$tahunSektorBar),
        x = "Sektor",
        y = "Emisi (GG CO₂)"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = cbPalette) +  # Tambahkan palet
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$interpretasiBarSektor <- renderUI({
    req(data_sektor(), input$tahunSektorBar)
    sektor_relevan <- c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH")
    
    df <- data_sektor() %>%
      filter(TAHUN == input$tahunSektorBar) %>%
      select(TAHUN, any_of(sektor_relevan)) %>%
      pivot_longer(cols = -TAHUN, names_to = "Sektor", values_to = "Emisi") %>%
      mutate(Emisi = as.numeric(gsub(",", ".", Emisi))) %>%
      arrange(desc(Emisi)) %>%
      slice_head(n = 3)
    
    if (nrow(df) == 0) {
      return(HTML("<p><b>Interpretasi:</b> Tidak ada data tersedia untuk tahun tersebut.</p>"))
    }
    
    sektor_terbesar <- df$Sektor[1]
    nilai_terbesar <- df$Emisi[1]
    
    HTML(paste0(
      "<p><b>Interpretasi:</b> Pada tahun ", input$tahunSektorBar, 
      ", sektor penyumbang emisi terbesar adalah <b>", sektor_terbesar, "</b> dengan total emisi sebesar ", 
      format(round(nilai_terbesar), big.mark = ".", decimal.mark = ","), " GG CO₂.</p>"
    ))
  })
  
  
  # Grafik Kanan: Pie Chart Semua Sektor
  output$pieChartSektorIPPU <- renderPlot({
    req(data_sektor(), input$tahunSektorPie)
    
    sektor_relevan <- c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH")
    
    data_tahun <- data_sektor() %>%
      filter(TAHUN == input$tahunSektorPie) %>%
      select(any_of(sektor_relevan)) %>%
      pivot_longer(cols = everything(), names_to = "Sektor", values_to = "Emisi") %>%
      mutate(Emisi = as.numeric(gsub(",", ".", Emisi)),
             Persentase = Emisi / sum(Emisi, na.rm = TRUE) * 100,
             Label = paste0(round(Persentase, 1), "%")) %>%
      filter(!is.na(Emisi))
    
    validate(
      need(nrow(data_tahun) > 0, "Tidak ada data untuk tahun yang dipilih.")
    )
    
    ggplot(data_tahun, aes(x = "", y = Persentase, fill = Sektor)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = Label),
                position = position_stack(vjust = 0.5),
                color = "white",
                size = 4,
                fontface = "bold") +
      scale_fill_manual(values = cbPalette) +  # Tambahkan palet
      labs(
        title = paste0("Distribusi Emisi GRK per Sektor pada Tahun ", input$tahunSektorPie),
        fill = "Sektor"
      ) +
      theme_void(base_size = 14)
  })
  
  
  output$interpretasiPieSektor <- renderUI({
    req(data_sektor(), input$tahunSektorPie)
    sektor_relevan <- c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH")
    
    data_tahun <- data_sektor() %>%
      filter(TAHUN == input$tahunSektorPie) %>%
      select(any_of(sektor_relevan)) %>%
      pivot_longer(cols = everything(), names_to = "Sektor", values_to = "Emisi") %>%
      mutate(Emisi = as.numeric(gsub(",", ".", Emisi))) %>%
      filter(!is.na(Emisi)) %>%
      mutate(Persentase = Emisi / sum(Emisi, na.rm = TRUE) * 100)
    
    if (nrow(data_tahun) == 0) {
      return(HTML("<p><b>Interpretasi:</b> Tidak ada data tersedia untuk tahun tersebut.</p>"))
    }
    
    top <- data_tahun %>% arrange(desc(Persentase)) %>% slice(1)
    
    HTML(paste0(
      "<p><b>Interpretasi:</b> Pada tahun ", input$tahunSektorPie, 
      ", sektor <b>", top$Sektor, "</b> memberikan kontribusi terbesar terhadap total emisi, yaitu sebesar ", 
      round(top$Persentase, 1), "% dari total emisi nasional.</p>"
    ))
  })
  
  # HALAMAN 5 (INFERENSIA)
  data_inferensia <- reactive({
    df <- read_excel("data/Data Regresi.xlsx") %>%
      janitor::clean_names() %>%
      mutate(
        log_suhu = log(suhu)
      )
    df
  })
  
  model_inf <- reactive({
    lm(suhu ~ energi + ippu + pertanian + kehutanan, data = data_inferensia())
  })
  
  output$plotLinearitas <- renderPlot({
    df <- data_inferensia()
    pairs(df[, c("suhu", "energi", "ippu", "pertanian", "kehutanan")],
          panel = function(x, y, ...) {
            points(x, y, ...)
            abline(lm(y ~ x), col = "blue")
          })
  })
  
  output$interpretasiLinearitas <- renderPrint({
    cat("Jika titik-titik mengikuti garis biru pada setiap scatter plot, maka hubungan dapat dianggap linear.")
  })
  
  output$plotKorelasi <- renderPlot({
    M <- cor(data_inferensia()[, c("energi", "ippu", "pertanian", "kehutanan", "suhu")], use = "complete.obs")
    corrplot(M, method = "color", addCoef.col = "black", number.cex = 0.7)
  })
  
  output$interpretasiKorelasi <- renderUI({
    helpText(class="text-interpretasi","Nilai korelasi mendekati 1 atau -1 menunjukkan hubungan kuat antar variabel. Multikolinearitas bisa terjadi jika antar variabel independen saling berkorelasi tinggi.")
  })
  
  output$qqplotResidual <- renderPlot({
    res <- residuals(model_inf())
    qqnorm(res); qqline(res, col = "red")
  })
  
  output$shapiroTest <- renderPrint({
    res <- shapiro.test(residuals(model_inf()))
    cat("a. Hipotesis:\nH0: Residual terdistribusi normal\nH1: Tidak normal\n")
    cat("b. Tingkat signifikansi: 0.05\n")
    cat("c. Tolak H0 jika p-value < 0.05\n")
    cat("d. Statistik uji:\n"); print(res)
    cat("e. Keputusan: ", ifelse(res$p.value < 0.05, "Tolak H0\n", "Gagal tolak H0\n"))
    cat("Kesimpulan: ", ifelse(res$p.value < 0.05, "Residual tidak terdistribusi normal.", "Residual terdistribusi normal."))
  })
  
  output$ujiBP <- renderPrint({
    res <- bptest(model_inf())
    cat("a. Hipotesis:\nH0: Homoskedastisitas\nH1: Heteroskedastisitas\n")
    cat("b. Tingkat signifikansi: 0.05\n")
    cat("c. Tolak H0 jika p-value < 0.05\n")
    cat("d. Statistik uji:\n"); print(res)
    cat("e. Keputusan: ", ifelse(res$p.value < 0.05, "Tolak H0\n", "Gagal tolak H0\n"))
    cat("Kesimpulan: ", ifelse(res$p.value < 0.05, "Model mengalami heteroskedastisitas.", "Model tidak mengalami heteroskedastisitas."))
  })
  
  output$dwtestResult <- renderPrint({
    res <- dwtest(model_inf())
    cat("a. Hipotesis:\nH0: Tidak ada autokorelasi\nH1: Ada autokorelasi\n")
    cat("b. Tingkat signifikansi: 0.05\n")
    cat("c. Tolak H0 jika p-value < 0.05\n")
    cat("d. Statistik uji:\n"); print(res)
    cat("e. Keputusan: ", ifelse(res$p.value < 0.05, "Tolak H0\n", "Gagal tolak H0\n"))
    cat("Kesimpulan: ", ifelse(res$p.value < 0.05, "Ada autokorelasi dalam residual.", "Tidak ada autokorelasi dalam residual."))
  })
  
  output$ujiOutlier <- renderPrint({
    stud.res <- rstudent(model_inf())
    out.idx <- which(abs(stud.res) > 2)
    cat("a. Deteksi outlier berdasarkan Studentized Residual > 2\n")
    if (length(out.idx) > 0) {
      cat("b. Outlier terdeteksi:\n")
      print(data.frame(Index = out.idx, Studentized = round(stud.res[out.idx], 3)))
    } else {
      cat("b. Tidak ada outlier yang signifikan.")
    }
  })
  
  output$plotCooksDistance <- renderPlot({
    cooks <- cooks.distance(model_inf())
    plot(cooks, type = "h", ylab = "Cook's distance")
    abline(h = 4 / length(cooks), col = "red", lty = 2)
  })
  
  output$tabelVIF <- renderTable({
    vif_vals <- vif(model_inf())
    data.frame(Variable = names(vif_vals), VIF = round(as.numeric(vif_vals), 2))
  }, rownames = FALSE)
  
  output$interpretasiVIF <- renderPrint({
    vif_vals <- vif(model_inf())
    cat("Interpretasi Multikolinearitas:\n")
    if (any(vif_vals > 10)) {
      cat("Beberapa variabel memiliki nilai VIF > 10, yang menunjukkan adanya multikolinearitas kuat. Variabel dengan VIF tinggi dapat dipertimbangkan untuk dihapus dari model jika tidak memiliki dasar teoritis yang kuat.")
    } else {
      cat("Semua nilai VIF < 10, tidak ditemukan indikasi multikolinearitas yang mengganggu.")
    }
  })
  
  output$summaryRegresi <- renderPrint({
    summary(model_inf())
  })
  
  output$interpretasiRegresi <- renderUI({
    s <- summary(model_inf())
    
    HTML(paste0(
      "<div style='border: 1px solid #ccc; padding: 12px; background-color: #f9f9f9; border-radius: 5px;'>",
      
      "<b>R-squared:</b> ", round(s$r.squared, 4), 
      " → Model menjelaskan sekitar ", round(100 * s$r.squared, 2), "% variasi suhu yang diamati.<br>",
      
      "<b>Adjusted R-squared:</b> ", round(s$adj.r.squared, 4), 
      " → Mengoreksi pengaruh jumlah variabel bebas dalam model.<br><br>",
      
      "<b>Uji F (simultan):</b><br>",
      "H<sub>0</sub>: Semua koefisien regresi (selain intercept) = 0<br>",
      "H<sub>1</sub>: Setidaknya satu koefisien regresi ≠ 0<br>",
      "F = ", round(s$fstatistic[1], 2), 
      ", df = (", s$fstatistic[2], ", ", s$fstatistic[3], ")<br>",
      "p-value = ", round(pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE), 5), "<br>",
      if (pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE) < 0.05) {
        "<span style='color:green;'>→ Terdapat setidaknya satu variabel independen yang berpengaruh signifikan secara simultan terhadap suhu.</span><br><br>"
      } else {
        "<span style='color:red;'>→ Tidak ada variabel independen yang signifikan secara simultan.</span><br><br>"
      },
      
      "<b>Uji t (parsial):</b><br>",
      "<i>H<sub>0</sub>: Tidak ada pengaruh variabel independen terhadap suhu</i><br>",
      "<i>H<sub>1</sub>: Ada pengaruh variabel independen terhadap suhu</i><br><br>",
      
      "<b>Interpretasi Koefisien:</b><br>",
      paste(
        sapply(2:nrow(s$coefficients), function(i) {
          nama <- rownames(s$coefficients)[i]
          est <- round(s$coefficients[i, 1], 7)
          pval <- round(s$coefficients[i, 4], 4)
          signif <- ifelse(pval < 0.05,
                           "<span style='color:green; font-weight:bold;'>Signifikan</span>",
                           "<span style='color:red;'>Tidak signifikan</span>")
          paste0("<i>", nama, "</i>: Koefisien = ", est, ", p-value = ", pval, " → ", signif)
        }),
        collapse = "<br>"
      ),
      
      "<br><br><b>Kesimpulan Akhir:</b><br>",
      if (any(s$coefficients[2:5, 4] < 0.05)) {
        "Setidaknya satu variabel independen berpengaruh signifikan terhadap suhu. Model regresi dapat digunakan untuk menjelaskan hubungan antara sektor dan perubahan suhu."
      } else {
        "Tidak ada variabel yang signifikan pada tingkat signifikansi 5%. Model kurang dapat menjelaskan variasi suhu."
      },
      
      "</div>"
    ))
  })
  
  # HALAMAN 6 (METADATA)
  # === METADATA TABLE DAN MODAL ===
  metadata_df <- metadata_raw %>%
    mutate(
      Link = paste0("<a href='", Link, "' target='_blank'>Lihat Link</a>"),
      Detail = paste0("<button id='detail_", row_number(), "' class='btn btn-info btn-sm action-button'>Lihat Detail</button>"),
      Unduh = paste0("<a href='", File, "' download target='_blank'>Unduh Data</a>")
    )
  
  output$metadataTable <- renderDT({
    datatable(
      metadata_df[, c("Jenis_Data", "Nama_Data", "Penyedia", "Tahun", "Link", "Unduh", "Detail")],
      escape = FALSE,
      selection = 'none',
      options = list(pageLength = 10),
      callback = JS("table.on('click', 'button', function() {
                    Shiny.setInputValue('detail_clicked', this.id, {priority: 'event'});
                  });")
    )
  })
  
  observeEvent(input$detail_clicked, {
    row_index <- as.numeric(gsub("detail_", "", input$detail_clicked))
    selected_name <- metadata_raw$Nama_Data[row_index]
    detail <- detail_texts[[selected_name]]
    showModal(modalDialog(
      title = paste("Detail Metadata:", selected_name),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Tutup"),
      tagList(
        p(detail$desc),
        br(),
        h5("Struktur Variabel:"),
        DTOutput("detailVariable")
      )
    ))
    
    output$detailVariable <- renderDT({
      datatable(detail$table, options = list(dom = 't'), rownames = FALSE)
    })
  })
  
  
  # HALAMAN 7 (TENTANG KAMI)
}

#============================== *********** .RUN APP *********** ==============================#
shinyApp(ui = ui, server = server)