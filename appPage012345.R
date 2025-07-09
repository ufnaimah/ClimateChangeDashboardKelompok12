
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
library(rsconnect)

# Fix fungsi yang tabrakan
select <- dplyr::select
recode <- dplyr::recode
mutate <- dplyr::mutate

#============================== ******** USER INTERFACE ******** ==============================#
ui <- navbarPage("CLIMATE CHANGE - Emission by Energy,Transportation, and Electricity Sector",
                 id = "mainTabs",
                 
                 # OVERVIEW
                 tabPanel("Overview",
                          fluidPage(
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "custompage1.css")
                            ),
                            
                            br(),
                            # Membuat judul agar membentang dari kiri ke kanan dengan penataan CSS
                            tags$h2(
                              "Overview Perubahan Iklim dan Emisi",
                              style = "text-align: center; width: 100%; font-size: 32px; font-weight: bold; margin-bottom: 20px;"
                            ),
                            
                            # Membagi halaman menjadi dua kolom (kiri untuk teks, kanan untuk grafik)
                            fluidRow(
                              # Kolom kiri - Penjelasan
                              column(6,
                                     wellPanel(
                                       h3("Apa itu Perubahan Iklim?"),
                                       p("Perubahan iklim adalah perubahan jangka panjang pada suhu dan cuaca di Bumi, terutama akibat aktivitas manusia seperti pembakaran bahan bakar fosil dan deforestasi.")
                                     ),
                                     
                                     wellPanel(
                                       h3("Penyebab Perubahan Iklim"),
                                       p("Penyebab utama adalah emisi gas rumah kaca (GRK), seperti CO2 dan metana, dari pembakaran bahan bakar fosil, deforestasi, pertanian, dan industri.")
                                     ),
                                     
                                     wellPanel(
                                       h3("Dampak Perubahan Iklim"),
                                       p("Dampak termasuk peningkatan suhu, cuaca ekstrem, naiknya permukaan laut, dan ancaman terhadap ekosistem dan ketahanan pangan.")
                                     ),
                                     
                                     wellPanel(
                                       h3("Solusi untuk Mengatasi Perubahan Iklim"),
                                       p("Solusi mencakup penggunaan energi terbarukan, pengurangan emisi gas rumah kaca, dan perlindungan ekosistem alami.")
                                     )
                              ),
                              
                              # Kolom kanan - Grafik atau visualisasi
                              column(6,
                                     wellPanel(
                                       h3("Grafik Perubahan Iklim dan Emisi"),

                                       fluidRow(
                                         column(4,
                                                div(style = "text-align: center;",
                                                    img(src = "Grafik1.png",
                                                        style = "height: 200px; width: 200px; cursor: pointer; box-shadow: 0px 2px 10px rgba(0,0,0,0.2);",
                                                        onclick = "Shiny.setInputValue('goToTab', 'Emisi Global')"),
                                                    br(),
                                                    strong("Emisi Global")
                                                )
                                         ),
                                         column(4,
                                                div(style = "text-align: center;",
                                                    img(src = "Grafik2.png",
                                                        style = "height: 200px; width: 200px; cursor: pointer; box-shadow: 0px 2px 10px rgba(0,0,0,0.2);",
                                                        onclick = "Shiny.setInputValue('goToTab', 'Emisi Nasional')"),
                                                    br(),
                                                    strong("Emisi Nasional")
                                                )
                                         ),
                                         column(4,
                                                div(style = "text-align: center;",
                                                    img(src = "Grafik3.png",
                                                        style = "height: 200px; width: 200px; cursor: pointer; box-shadow: 0px 2px 10px rgba(0,0,0,0.2);",
                                                        onclick = "Shiny.setInputValue('goToTab', 'Emisi Sektor')"),
                                                    br(),
                                                    strong("Emisi Sektor")
                                                )
                                         )
                                       ),
                                           br(), br()
                                       )
                                     )
                              )
                              
                            ),
                            
                            # Galeri Foto Satelit
                            wellPanel(
                              h3("Galeri Foto Satelit Bumi Sepanjang Waktu"),
                              div(class = "gallery-row",
                                  div(class = "gallery-image",
                                      img(src = "https://www.nasa.gov/wp-content/uploads/2023/03/as17-148-22727_lrg.jpg", style = "width: 100%; height: 400px; object-fit: cover;", 
                                          onclick = "Shiny.setInputValue('goToClimateChange', true)"),  # Tambahkan onclick pada gambar
                                      p("Bumi dari luar angkasa, 1972")
                                  ),
                                  div(class = "gallery-image",
                                      img(src = "https://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73580/world.topo.bathy.200401.3x5400x2700.png", style = "width: 100%; height: 400px; object-fit: cover;"),
                                      p("Topografi Bumi, 2004")
                                  ),
                                  div(class = "gallery-image",
                                      img(src = "https://eoimages.gsfc.nasa.gov/images/imagerecords/78000/78349/arctic_vir_2012147_tn.jpg", style = "width: 100%; height: 400px; object-fit: cover;"),
                                      p("Bumi dari NASA Blue Marble, 2012")
                                  )
                              ),
                              br()
                            ),
                            
                            # Video YouTube 1
                            wellPanel(
                              h3("Tonton Video Perubahan Iklim dari NASA"),
                              HTML('
                                <div style="text-align:center; max-width: 100%; margin: 0 auto;">
                                  <iframe width="100%" height="500" 
                                          src="https://www.youtube.com/embed/YfWCUYX2_U0?si=oN13ycjx01wuB17M" 
                                          title="YouTube video player" 
                                          frameborder="0" 
                                          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
                                          allowfullscreen>
                                  </iframe>
                                </div>
                              '),
                              br(),
                              br()
                            ),
                            
                            # Video YouTube 2
                            wellPanel(
                              h3("Tata Cara Penggunaan Dashboard"),
                              HTML('
                                <div style="text-align:center; max-width: 100%; margin: 0 auto;">
                                  <iframe width="100%" height="500" 
                                          src="https://www.youtube.com/embed/gE8PP-dNYqQ" 
                                          title="YouTube video player" 
                                          frameborder="0" 
                                          allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" 
                                          allowfullscreen>
                                  </iframe>
                                </div>
                              '),
                              br(),
                              br()
                            ),
                            
                            # Quote
                            div(style = "text-align: center; font-size: 18px; font-style: italic; color: #2b2b2b; margin-top: 40px;",
                                '"Bumi adalah apa yang kita semua miliki bersama." – Wendell Berry'),
                          br(),
                          br()
                          ),
                          
                 
                          
                 # HALAMAN 1
                 tabPanel("Climate Change dan Emisi Global",
                          fluidPage(
                            tags$head(
                              tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap"),
                              tags$link(rel = "stylesheet", type = "text/css", href = "custompage1.css")
                            ),
                            
                            titlePanel("Climate Change dan Emisi Global"),
                            
                            fluidRow(
                              # Kolom tengah - Grafik utama (Suhu dan Tren Emisi)
                              column(7,
                                     wellPanel(
                                       h4("Tren Suhu Rata-Rata Permukaan"),
                                       
                                       checkboxGroupInput("filterSuhu", "Tampilkan Suhu Wilayah:",
                                                          choices = c("Global", "Indonesia", "Asia"),
                                                          selected = c("Global", "Indonesia", "Asia"),
                                                          inline = TRUE),
                                       
                                       plotOutput("suhuChart", height = "300px"),
                                       
                                       h4("Tren Emisi"),
                                       
                                       checkboxGroupInput("filterEmisi", "Tampilkan Tren Emisi:",
                                                          choices = c("Indonesia", "ASEAN"),
                                                          selected = c("Indonesia", "ASEAN"),
                                                          inline = TRUE),
                                       
                                       plotOutput("gabunganEmisiChart", height = "330px")
                                     )
                              ),
                              
                              
                              # Kolom kanan - hanya tabel ranking
                              column(5,
                                     wellPanel(
                                       h4("Peringkat Emisi Gas Rumah Kaca di Asia Tenggara"),
                                       selectInput("tahun", "Pilih Tahun:", 
                                                   choices = NULL, 
                                                   selected = NULL),
                                       div(style = "height: 600px; overflow-y: auto;",
                                           DTOutput("rankingTable")
                                       )
                                     )
                              )
                            )
                          )
                 ),
                 
                 # HALAMAN 2
                 tabPanel("Emisi Nasional",
                          
                          fluidPage(
                            tags$head(
                              tags$link(rel = "stylesheet", type = "text/css", href = "custompage1.css")
                            ),
                            
                            fluidRow(
                              column(6,
                                     wellPanel(
                                       h4("Top 3 Provinsi dengan Emisi Tertinggi"),
                                       selectInput("tahunTop3", "Pilih Tahun:", choices = NULL),
                                       plotOutput("barChartProvinsi", height = "250px")
                                     )
                              ),
                              column(6,
                                     wellPanel(
                                       h4("Tren Emisi per Provinsi"),
                                       selectInput("provinsiTren", "Pilih Provinsi:", choices = NULL),
                                       plotOutput("lineChartProvinsi", height = "250px")
                                     )
                              )
                            ),
                            fluidRow(
                              column(12,
                                     wellPanel(
                                       h4("Peta Emisi Provinsi Indonesia"),
                                       selectInput("tahunPeta", "Pilih Tahun:", choices = NULL),
                                       leafletOutput("mapChart", height = "450px")
                                     )
                              )
                            )
                          )
                 ),
                 # HALAMAN 3
                 tabPanel("Emisi sektor", 
                          fluidPage(
                            fluidRow(
                              column(12,
                                     tabsetPanel(
                                       tags$head(
                                         includeCSS("www/custompage1.css")
                                       ),
                                       
                                       # Tab 1: Sektor berdasarkan IPCC dan BPS
                                       tabPanel("BPS (IPCC-based)",
                                                wellPanel(
                                                  # Penjelasan dengan Lorem Ipsum sebelum grafik
                                                  h4("Penjelasan Sektor IPPC dan BPS"),
                                                  p("Bagian ini menyajikan data emisi gas rumah kaca (GRK) berdasarkan klasifikasi sektor yang digunakan dalam inventarisasi nasional Indonesia, mengacu pada pedoman IPCC (Intergovernmental Panel on Climate Change)."),
                                                  p("Sektor-sektor yang ditampilkan meliputi: Energi, Proses Industri dan Penggunaan Produk (IPPU), Pertanian, Kehutanan dan Penggunaan Lahan (FOLU), serta Limbah."),
                                                  p("Pendekatan ini mencerminkan total emisi dari seluruh aktivitas utama penyumbang GRK, baik dari penggunaan energi maupun non-energi. Visualisasi ini berguna untuk memahami kontribusi masing-masing sektor terhadap total emisi nasional.")
                                              
                                                  ),
                                                fluidRow(
                                                  # Grafik kiri: Bar Chart Top 3 Sektor
                                                  column(6,
                                                         h3("Top 3 Sektor Penyumbang Emisi"),
                                                         selectInput("tahunSektorBarIPPU", "Pilih Tahun untuk Top Sektor:", choices = NULL),
                                                         plotOutput("barChartSektorIPPU", height = 350)
                                                  ),
                                                  # Grafik kanan: Pie Chart Semua Sektor
                                                  column(6,
                                                         h3("Persentase Emisi per Sektor"),
                                                         selectInput("tahunSektorIPPU", "Pilih Tahun untuk Persentase Sektor:", choices = NULL),
                                                         plotOutput("pieChartSektorIPPU", height = 350)
                                                  )
                                                )
                                       ),
                                       # Tab 2: Sektor berdasarkan Fokus Studi (Dikosongkan)
                                       tabPanel("IEA",
                                                wellPanel(
                                                  h4("Sektor Fokus Studi"),
                                                  p("Bagian ini menyajikan data emisi berdasarkan sektor-sektor spesifik yang menjadi fokus studi, seperti Listrik, Transportasi, dan Industri."),
                                                  p("Visualisasi ini bertujuan untuk memberikan gambaran tren dan distribusi emisi gas rumah kaca secara lebih rinci."),
                                                
                                                  tags$hr(),
                                                  tags$b("⚠️ Catatan Penting:"),
                                                  p("Sektor 'Listrik', 'Transportasi', dan 'Industri' dalam versi IEA sebenarnya merupakan bagian dari sektor 'Energi' dalam klasifikasi IPCC/BPS."),
                                                  p("Oleh karena itu, total emisi dan cakupan datanya tidak sama dan tidak dapat dibandingkan langsung antara versi IPCC/BPS dan versi IEA.")
                                                  ),
                                                
                                                # ==== OVERVIEW 3 VALUE BOX ====
                                                
                                                fluidRow(
                                                  column(12,
                                                         selectInput("year_overview", "Pilih Tahun (Overview):", choices = NULL)
                                                  )
                                                ),
                                                
                                                fluidRow(
                                                  column(4,
                                                         wellPanel(
                                                           h4(textOutput("shareEmissionValue")),
                                                           p("Share emisi global oleh Indonesia")
                                                         )),
                                                  column(4,
                                                         wellPanel(
                                                           h4(textOutput("indonesiaEmissionValue")),
                                                           p("Emisi di Indonesia (ton CO2)")
                                                         )),
                                                  column(4,
                                                         wellPanel(
                                                           h4(textOutput("globalEmissionValue")),
                                                           p("Emisi Global (juta ton CO2)")
                                                         ))
                                                ),
                                                
                                                
                                                # ==== TREND CHART ====
                                                fluidRow(
                                                  column(9,
                                                         h4("Tren Emisi berdasarkan Sektor"),
                                                         plotOutput("trendEmissionChart", height = "300px")
                                                  ),
                                                  column(3,
                                                         h5("Pilih Sektor:"),
                                                         checkboxGroupInput("sector", label = NULL,
                                                                            choices = c("Listrik dan Produsen Panas" = "Electricity and heat producers",
                                                                                        "Transportasi" = "Transport",
                                                                                        "Industri" = "Industry",
                                                                                        "Perumahan" = "Residential",
                                                                                        "Layanan Publik dan Komersial" = "Commercial and Public Services",
                                                                                        "Pertanian/Kehutanan" = "Agriculture/Forestry",
                                                                                        "Perikanan" = "Fishing",
                                                                                        "Lain-lain (Non-spesifik)" = "Non specified-other"),
                                                                            selected = "Electricity and heat producers"
                                                         ),
                                                         sliderInput("year_trend", "Pilih Tahun Maksimal:", 
                                                                     min = 2000, max = 2023, value = 2023, step = 1, sep = "")
                                                  )
                                                ),
                                                
                                                # === OVERVIEW EMISI TIAP SEKTOR (DENGAN FILTER TAHUN) ===
                                                fluidRow(
                                                  column(12,
                                                         selectInput("year_sector_overview", "Pilih Tahun (Sektor Overview):", choices = NULL)
                                                  )
                                                ),
                                                fluidRow(
                                                  column(4,
                                                         wellPanel(
                                                           h4(textOutput("listrikOverview")),
                                                           p("Emisi Sektor Listrik (Mt CO2)")
                                                         )),
                                                  column(4,
                                                         wellPanel(
                                                           h4(textOutput("industriOverview")),
                                                           p("Emisi Sektor Industri (Mt CO2)")
                                                         )),
                                                  column(4,
                                                         wellPanel(
                                                           h4(textOutput("transportOverview")),
                                                           p("Emisi Sektor Transportasi (Mt CO2)")
                                                         ))
                                                ),
                                                
                                                
                                                
                                                br(),
                                                
                                                fluidRow(
                                                  column(4,
                                                         h4("Emisi Sektor Listrik"),
                                                         selectInput("provinsi_listrik", "Pilih Provinsi:",
                                                                     choices = NULL, selected = NULL),
                                                         plotOutput("trendListrikProvinsi")
                                                         
                                                  ),
                                                  column(4,
                                                         h4("Emisi Sektor Industri"),
                                                         plotlyOutput("industriPlot", height = "400px")
                                                  ),
                                                  column(4,
                                                         h4("Emisi Sektor Transportasi"),
                                                         selectInput("tahun_filter_kendaraan", "Pilih Tahun (Transportasi):", choices = NULL),
                                                         plotlyOutput("transportPlot", height = "380px")
                                                  ) 
                                                ) 
                                       ) 
                                     ) 
                              )          
                            )            
                          )              
                 ),                     

                
                 # HALAMAN 4 (INFERENSIA)
                 tabPanel("Inferensia",
                          fluidPage(
                            h3("Analisis Inferensia: Pengaruh Emisi terhadap Suhu", align = "center"),
                            br(),
                            
                            fluidRow(
                              # KIRI: TABEL
                              column(5,
                                     radioButtons("filterData", "Pilih Model:",
                                                  choices = c(
                                                    "Linear (Suhu ~ Emisi)" = "original",
                                                    "1/Emisi (Suhu ~ 1/Emisi)" = "inv",
                                                    "1/Emisi² (Suhu ~ 1/Emisi²)" = "inv2",
                                                    "1/sqrt(Emisi) (Suhu ~ 1/√Emisi)" = "invsqrt"
                                                  ),
                                                  selected = "original"
                                     ),
                                     DTOutput("tabelRegresi")
                              ),
                              
                              # KANAN: PENGUJIAN REGRESI
                              column(7,
                                     tabsetPanel(
                                       
                                       tabPanel("Linearitas",
                                                h5("Visualisasi Linearitas"),
                                                plotOutput("plotLinearitas")
                                       ),
                                       
                                       tabPanel("Normalitas",
                                                fluidRow(
                                                  column(6, h5("Q-Q Plot"), plotOutput("qqplotResidual")),
                                                  column(6, h5("Shapiro-Wilk Test"), verbatimTextOutput("shapiroTest"))
                                                )
                                       ),
                                       
                                       tabPanel("Homoskedastisitas",
                                                h5("Uji Breusch-Pagan"),
                                                verbatimTextOutput("ujiBP"),
                                       ),
                                       
                                       
                                       tabPanel("Autokorelasi",
                                                h5("Durbin-Watson Test"),
                                                verbatimTextOutput("dwtestResult")
                                       ),
                                       
                                       tabPanel("Outlier",
                                                h5("Uji Outlier dengan Studentized Residual"),
                                                verbatimTextOutput("ujiOutlier"),
                                                br(),
                                                h5("Cook's Distance"),
                                                plotOutput("plotCooksDistance")
                                       ),
                                       
                                       tabPanel("Hasil Regresi",
                                                verbatimTextOutput("regresiSummary")
                                       ),
                                       
                                       tabPanel("Robust Regresi",
                                                verbatimTextOutput("regresiRobust")
                                       ),
                                       tabPanel("Kesimpulan",
                                                verbatimTextOutput("kesimpulanRegresi")
                                       )
                                       
                                     )
                              )
                            )
                          )
                 ),
                 
                 
                 # HALAMAN 5 (METADATA)
                 tabPanel("Metadata",
                          # Menggunakan shinyjs untuk interaksi pop-up
                          shinyjs::useShinyjs(), 
                          
                          fluidPage(
                            titlePanel("Metadata Sumber Data"),
                            p("Halaman ini berisi daftar dan penjelasan mengenai sumber data yang digunakan dalam aplikasi. Klik 'Lihat Detail' untuk informasi lebih lanjut dan tautan unduhan."),
                            hr(),
                            
                            # Tabel akan ditampilkan di sini
                            DT::dataTableOutput("metadata_table")
                          )
                 ),
                 
                 # TENTANG KAMI
                 tabPanel("Tentang Kami",
                          fluidPage(
                            br(),
                            
                            # ==== DESKRIPSI ====
                            div(style = "padding-left: 90px; padding-right: 90px; text-align: center;",
                                h2("Tentang Kami"),
                                wellPanel(
                                  p("Kami adalah sekelompok mahasiswa Politeknik Statistika STIS yang sedang belajar mengenai analisis data menggunakan R. Proyek ini merupakan bagian dari Praktikum Mata Kuliah Komputasi Statistik di STIS. Proyek pembuatan dashboard interaktif berbasis RShiny dikembangkan sebagai upaya untuk menyediakan sistem analisis emisi gas rumah kaca (GRK) dari berbagai sektor di Indonesia yang memengaruhi perubahan iklim. Dashboard ini dibuat melalui proses yang panjang sehingga dapat menyajikan informasi yang komprehensif.")
                                ),
                                
                                wellPanel(
                                  p("Dashboard interaktif berbasis R Shiny ini dikembangkan sebagai upaya untuk menyediakan sistem analisis emisi gas rumah kaca (GRK) dari berbagai sektor di Indonesia yang berkontribusi terhadap perubahan iklim. Pengembangan dilakukan melalui proses yang panjang dan kolaboratif agar dapat menyajikan informasi yang komprehensif dan mudah dipahami. Dashboard ini dirancang untuk memvisualisasikan tren emisi dari tahun 2000 hingga 2023, menampilkan kontribusi setiap sektor, distribusi spasial per provinsi, serta menyediakan fitur analisis dan komparasi secara dinamis.")
                                ),
                                
                                wellPanel(
                                  p("Kami berharap karya ini dapat menjadi kontribusi kecil yang bermakna dalam meningkatkan kesadaran dan upaya bersama untuk mengatasi perubahan iklim. Dalam proses pengembangannya, dilakukan beberapa tahapan penting yang mencakup pengumpulan dan integrasi data dari berbagai sumber, validasi dan penentuan objek data, serta pembangunan sistem business intelligence yang memungkinkan pengguna untuk mengeksplorasi informasi dengan fleksibel. Seluruh data yang digunakan telah melalui proses pembersihan dan konsolidasi agar siap dianalisis, serta disajikan dalam bentuk grafik time series, pie chart kontribusi sektor, peta interaktif, dan ringkasan insight yang mendukung pemahaman data.")
                                )
                            ),
                            br(),
                            
                            # ==== FOTO KELOMPOK ====
                            div(style = "text-align: center;",
                                br(),
                                h2("Kelompok 5"),
                                img(src = "foto_kelompok.jpg", height = "350px", width = "600px",
                                    style = "border-radius: 10px; box-shadow: 0px 2px 10px rgba(0,0,0,0.2);"),
                                br(), br()
                            ),
                            br(),
                            
                            # ==== FOTO PRIBADI (3 ORANG) ====
                            div(style = "text-align: center;",
                                h3("Anggota"),
                                # Foto-foto dalam inline-block
                                div(style = "display: inline-block; margin: 20px;",
                                    img(src = "anggota1.jpg", style = "height: 200px; width: 200px; box-shadow: 0px 2px 10px rgba(0,0,0,0.2);"),
                                    br(),
                                    br(),
                                    strong("Emisi Global")
                                ),
                                
                                div(style = "display: inline-block; margin: 20px;",
                                    img(src = "anggota2.jpg", style = "height: 200px; width: 200px; box-shadow: 0px 2px 10px rgba(0,0,0,0.2);"),
                                    br(),
                                    br(),
                                    strong("Emisi Nasional")
                                ),
                                
                                div(style = "display: inline-block; margin: 20px;",
                                    img(src = "anggota3.jpg", style = "height: 200px; width: 200px; box-shadow: 0px 2px 10px rgba(0,0,0,0.2);"),
                                    br(),
                                    br(),
                                    strong("Emisi Sektor")
                                )
                            ),
                            
                            
                            # ==== DESKRIPSI KOLABORASI ====
                            div(style = "padding-left: 90px; padding-right: 90px; text-align: center;",
                                p("Dalam mengembangkan dashboard ini, setiap anggota tim berperan aktif sesuai dengan keahliannya. Kami saling mendukung dalam proses pengumpulan dan pembersihan data, eksplorasi visualisasi, analisis statistik, serta pengembangan fitur interaktif dengan R Shiny."),
                            ),
                            
                            # ==== QUOTE ====
                            div(class = "quote-container",
                                div(class = "quote-box",
                                    HTML('"Great things are done by a series of small things brought together." <br>– <i>Vincent Van Gogh</i>')
                                )
                            )
                          )
                 )
                
)

#============================== ************ SERVER ************ ==============================#

server <- function(input, output, session) {
  
  # OVERVIEW
  server <- function(input, output, session) {
    observeEvent(input$goToTab, {
      print(paste("Go to tab:", input$goToTab))
      updateTabsetPanel(session, inputId = "mainTabs", selected = input$goToTab)
    })
  }
  
  
  # HALAMAN 1
  
  # Data Emisi Asia Tenggara
  data_emisi <- reactive({
    read_excel("data/(3) Annual CO2 Emissions.xlsx")
  })
  
  # Data ASEAN (dalam bentuk long)
  data_asean_long <- reactive({
    data_wide <- read_excel("data/(3) Annual CO2 Emissions.xlsx")
    
    pivot_longer(
      data_wide,
      cols = -Year,
      names_to = "Negara",
      values_to = "Emisi_Total"
    ) %>%
      rename(Tahun = Year)
  })
  
  # Grafik suhu asli dari file excel
  output$suhuChart <- renderPlot({
    req(input$filterSuhu)
    
    suhu_data <- read_excel("data/average surface temperature global, asean, and indonesia.xlsx")
    
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
  
  # Grafik gabungan tren emisi Indonesia dan ASEAN
  output$gabunganEmisiChart <- renderPlot({
    req(data_emisi(), data_asean_long(), input$filterEmisi)
    
    df_indonesia <- data_emisi() %>%
      select(Year, Indonesia) %>%
      rename(Tahun = Year, Emisi = Indonesia) %>%
      mutate(Wilayah = "Indonesia")
    
    df_asean <- data_asean_long() %>%
      group_by(Tahun) %>%
      summarise(Emisi = sum(Emisi_Total, na.rm = TRUE)) %>%
      mutate(Wilayah = "ASEAN")
    
    df_gabungan <- bind_rows(df_indonesia, df_asean)
    
    df_filtered <- df_gabungan %>%
      filter(Wilayah %in% input$filterEmisi)
    
    ggplot(df_filtered, aes(x = Tahun, y = Emisi, color = Wilayah)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Tren Emisi Gas Rumah Kaca",
        x = "Tahun",
        y = "Emisi (juta ton CO2e)",
        color = "Wilayah"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_color_manual(values = c("Indonesia" = "darkgreen", "ASEAN" = "steelblue")) +
      scale_y_continuous(labels = scales::comma)
  })
  
  
  # Pilihan tahun untuk dropdown
  observe({
    data <- data_asean_long()
    tahun_choices <- sort(unique(data$Tahun), decreasing = TRUE)
    updateSelectInput(session, "tahun", 
                      choices = tahun_choices,
                      selected = tahun_choices[1])
  })
  
  # Data peringkat berdasarkan tahun yang dipilih
  data_tahun <- reactive({
    req(data_asean_long(), input$tahun)
    
    data_asean_long() %>%
      filter(Tahun == input$tahun) %>%
      arrange(desc(Emisi_Total)) %>%
      mutate(Peringkat = row_number()) %>%
      select(Peringkat, Negara, Emisi_Total) %>%
      mutate(Emisi_Total = round(Emisi_Total, 4))
  })
  
  # Tabel peringkat ditampilkan di UI
  output$rankingTable <- renderDT({
    datatable(data_tahun(),
              extensions = 'Buttons',
              options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                pageLength = 11
              ),
              rownames = FALSE,
              colnames = c('Peringkat', 'Negara', 'Emisi Total (juta ton CO2e)'))
  })

# HALAMAN 2
    data_provinsi <- reactive({
      read_excel("data/Emission by province in Indonesia.xlsx", sheet = 1)
    })
    
    data_long_emisi <- reactive({
      path_emisi <- "data/Tren Emisi Per Provinsi 2000-2024.xlsx"
      read_excel(path_emisi, col_types = "text") %>%
        pivot_longer(cols = -1, names_to = "Provinsi", values_to = "Value_char") %>%
        mutate(
          Tahun = as.numeric(.[[1]]),
          `Total Emisi` = as.numeric(gsub(",", ".", Value_char, fixed = TRUE))
        ) %>%
        filter(!is.na(Tahun) & !is.na(`Total Emisi`))
    })
    
    observe({
      req(data_provinsi())
      tahun_choices <- sort(unique(data_provinsi()$Tahun))
      provinsi_choices <- unique(data_provinsi()$Provinsi)
      updateSelectInput(session, "tahunTop3", choices = tahun_choices, selected = max(tahun_choices))
      updateSelectInput(session, "tahunPeta", choices = tahun_choices, selected = max(tahun_choices))
      updateSelectInput(session, "provinsiTren", choices = provinsi_choices, selected = provinsi_choices[1])
    })
    
    output$barChartProvinsi <- renderPlot({
      req(data_provinsi(), input$tahunTop3)
      top_provinsi <- data_provinsi() %>%
        filter(Tahun == input$tahunTop3, toupper(Provinsi) != "NASIONAL") %>%
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
    
    output$lineChartProvinsi <- renderPlot({
      req(input$provinsiTren)
      data_tren <- data_long_emisi() %>%
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
          geom_text(aes(label = format(`Total Emisi`, big.mark = ".", decimal.mark = ",")), vjust = -0.8, size = 3) +
          labs(title = paste("Tren Emisi", input$provinsiTren), x = "Tahun", y = "Total Emisi (ton)") +
          scale_y_continuous(labels = ~format(.x, big.mark = ".", decimal.mark = ",")) +
          theme_minimal()
      }
    })
    
    geo_prov <- st_read("data/gadm41_IDN_1.json")
    
    data_choropleth <- reactive({
      req(data_provinsi(), input$tahunPeta)
      
      df <- data_provinsi() %>%
        filter(Tahun == input$tahunPeta & toupper(Provinsi) != "NASIONAL") %>%
        group_by(Provinsi) %>%
        summarise(`Total Emisi` = sum(`Total Emisi`, na.rm = TRUE)) %>%
        mutate(Provinsi = toupper(Provinsi))
      
      geo_fix <- geo_prov %>%
        mutate(NAME_1 = toupper(NAME_1)) %>%
        mutate(NAME_1 = recode(NAME_1,
                               "SUMATERAUTARA" = "SUMATERA UTARA",
                               "SUMATERABARAT" = "SUMATERA BARAT",
                               "BANGKABELITUNG" = "KEP. BANGKA BELITUNG",
                               "SUMATERASELATAN" = "SUMATERA SELATAN",
                               "JAWABARAT" = "JAWA BARAT",
                               "JAWATENGAH" = "JAWA TENGAH",
                               "JAWATIMUR" = "JAWA TIMUR",
                               "NUSATENGGARABARAT" = "NUSA TENGGARA BARAT",
                               "NUSATENGGARATIMUR" = "NUSA TENGGARA TIMUR",
                               "KALIMANTANBARAT" = "KALIMANTAN BARAT",
                               "KALIMANTANTENGAH" = "KALIMANTAN TENGAH",
                               "KALIMANTANSELATAN" = "KALIMANTAN SELATAN",
                               "KALIMANTANTIMUR" = "KALIMANTAN TIMUR",
                               "KALIMANTANUTARA" = "KALIMANTAN UTARA",
                               "SULAWESITENGAH" = "SULAWESI TENGAH",
                               "SULAWESIBARAT" = "SULAWESI BARAT",
                               "SULAWESISELATAN" = "SULAWESI SELATAN",
                               "SULAWESITENGGARA" = "SULAWESI TENGGARA",
                               "SULAWESIUTARA" = "SULAWESI UTARA",
                               "MALUKUUTARA" = "MALUKU UTARA",
                               "PAPUABARAT" = "PAPUA BARAT",
                               "PAPUA" = "PAPUA"
        ))
      
      geo_merged <- geo_fix %>%
        left_join(df, by = c("NAME_1" = "Provinsi"))
      
      return(geo_merged)
    })
    
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
          color = "white",
          weight = 1,
          fillOpacity = 0.8,
          smoothFactor = 0.2,
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

# HALAMAN 3
    # Memuat data sektor dengan penanganan kesalahan
    data_sektor <- reactive({
      file_path <- "data/(2) Emisi Gas Rumah Kaca Menurut Sektor 2000-2023.xlsx"
      validate(
        need(file.exists(file_path), paste("File tidak ditemukan di:", file_path, ". Silakan periksa path file."))
      )
      data <- read_excel(file_path, sheet = 1)
      validate(
        need("TAHUN" %in% colnames(data), "Kolom 'TAHUN' tidak ditemukan dalam file Excel."),
        need(any(c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH") %in% colnames(data)), 
             "Tidak ada kolom sektor relevan (ENERGI, IPPU, PERTANIAN, KEHUTANAN, LIMBAH) dalam file Excel.")
      )
      data
    })
    
    # Observasi untuk memperbarui input dropdown
    observe({
      req(data_sektor())
      tahun_choices <- sort(unique(data_sektor()$TAHUN), decreasing = TRUE)
      validate(
        need(length(tahun_choices) > 0, "Tidak ada data tahun yang valid dalam kolom TAHUN.")
      )
      updateSelectInput(session, "tahunSektorBarIPPU",
                        choices = tahun_choices,
                        selected = max(tahun_choices))
      updateSelectInput(session, "tahunSektorIPPU",
                        choices = tahun_choices,
                        selected = max(tahun_choices))
    })
    
    # Bar Chart Top 3 Sektor (Grafik Kiri)
    output$barChartSektorIPPU <- renderPlot({
      req(data_sektor(), input$tahunSektorBarIPPU)
      
      # Semua sektor relevan
      sektor_relevan <- c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH")
      
      df <- data_sektor() %>%
        filter(TAHUN == input$tahunSektorBarIPPU) %>%
        select(TAHUN, any_of(sektor_relevan)) %>%
        pivot_longer(cols = -TAHUN, names_to = "Sektor", values_to = "Emisi") %>%
        mutate(Emisi = as.numeric(gsub(",", ".", Emisi))) %>% # Mengganti koma dengan titik
        arrange(desc(Emisi)) %>%
        slice_head(n = 3) # Ambil top 3 sektor
      
      validate(
        need(nrow(df) > 0, "Tidak ada data untuk tahun yang dipilih atau sektor relevan.")
      )
      
      ggplot(df, aes(x = fct_reorder(Sektor, Emisi), y = Emisi, fill = Sektor)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = paste("Top 3 Sektor Emisi GRK pada Tahun", input$tahunSektorBarIPPU),
          x = "Sektor",
          y = "Emisi (ribu ton CO2e)"
        ) +
        scale_y_continuous(labels = comma) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
    })
    
    # Pie Chart Semua Sektor (Grafik Kanan)
    output$pieChartSektorIPPU <- renderPlot({
      req(data_sektor(), input$tahunSektorIPPU)
      
      # Semua sektor relevan
      sektor_relevan <- c("ENERGI", "IPPU", "PERTANIAN", "KEHUTANAN", "LIMBAH")
      
      data_tahun <- data_sektor() %>%
        filter(TAHUN == input$tahunSektorIPPU) %>%
        select(any_of(sektor_relevan)) %>%
        pivot_longer(cols = everything(), names_to = "Sektor", values_to = "Emisi") %>%
        mutate(Emisi = as.numeric(gsub(",", ".", Emisi)),
               Persentase = Emisi / sum(Emisi, na.rm = TRUE) * 100,
               Label = paste0(round(Persentase, 1), "%")) %>%
        filter(!is.na(Emisi))
      
      validate(
        need(nrow(data_tahun) > 0, "Tidak ada data untuk tahun yang dipilih atau sektor relevan.")
      )
      
      ggplot(data_tahun, aes(x = "", y = Persentase, fill = Sektor)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = Label), 
                  position = position_stack(vjust = 0.5), 
                  color = "white", 
                  size = 4, 
                  fontface = "bold") +
        labs(
          title = paste0("Distribusi Emisi GRK per Sektor pada Tahun ", input$tahunSektorIPPU),
          fill = "Sektor"
        ) +
        theme_void(base_size = 14)
    })

# HALAMAN 4
    # OVERVIEW DATA - Annual CO2 emissions per country
    data_overview <- reactive({
      df <- read_excel("data/Annual co2 emissions per country.xlsx")
      return(df)
    })
    
    # TREND DATA - Evolution of CO2 emissions by sector
    data_trend <- reactive({
      df <- read_csv("data/Evolution of CO2 emissions by sector in Indonesia since 2000.csv", 
                     show_col_types = FALSE)
      return(df)
    })
    
    # Update pilihan tahun untuk overview
    observe({
      tahun_overview <- sort(unique(data_overview()$Year), decreasing = TRUE)
      updateSelectInput(session, "year_overview", 
                        choices = tahun_overview, 
                        selected = tahun_overview[1])
    })
    
    # Data yang difilter berdasarkan tahun
    overview_filtered <- reactive({
      req(input$year_overview)
      data_overview() %>% filter(Year == input$year_overview)
    })
    
    # Perhitungan summary emission
    emission_summary <- reactive({
      df <- overview_filtered()
      indonesia <- df %>% filter(Entity == "Indonesia") %>% pull(`Annual CO2 emissions`)
      world <- df %>% filter(Entity == "World") %>% pull(`Annual CO2 emissions`)
      
      list(
        share = round((indonesia / world) * 100, 2),
        indo = indonesia,
        global = world / 1000000
      )
    })
    
    # Output value boxes
    output$shareEmissionValue <- renderText({
      paste0(emission_summary()$share, "%")
    })
    
    output$indonesiaEmissionValue <- renderText({
      format(emission_summary()$indo, big.mark = ",", scientific = FALSE)
    })
    
    output$globalEmissionValue <- renderText({
      format(round(emission_summary()$global, 1), big.mark = ",")
    })
    
    # TREND EMISSION CHART
    data_trend_filtered <- reactive({
      req(input$sector, input$year_trend)
      data_trend() %>%
        filter(`CO2 emissions by sector in Indonesia` %in% input$sector) %>%
        filter(Year <= input$year_trend) %>%
        mutate(Value = as.numeric(Value), Year = as.numeric(Year)) %>%
        filter(!is.na(Value))
    })
    
    output$trendEmissionChart <- renderPlot({
      ggplot(data_trend_filtered(), aes(x = Year, y = Value, color = `CO2 emissions by sector in Indonesia`)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        labs(x = "Tahun", y = "Emisi (Mt CO2)", 
             title = "Tren Emisi berdasarkan Sektor") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          legend.title = element_blank()
        )
    })
    
    
    #SEKTOR
    # === OBSERVER: Dropdown Tahun Overview Sektor ===
    observe({
      tahun_trend <- sort(unique(data_trend()$Year), decreasing = TRUE)
      updateSelectInput(session, "year_sector_overview", 
                        choices = tahun_trend, 
                        selected = max(tahun_trend, na.rm = TRUE))
    })
    
    
    # === REACTIVE: Filter berdasarkan Tahun untuk Overview Sektor ===
    sector_overview_data <- reactive({
      req(input$year_sector_overview)
      data_trend() %>%
        filter(Year == input$year_sector_overview)
    })
    
    # === OUTPUT: Text Overview per Sektor ===
    output$listrikOverview <- renderText({
      df <- sector_overview_data()
      val <- df %>% filter(`CO2 emissions by sector in Indonesia` == "Electricity and heat producers") %>% pull(Value)
      if (length(val) == 0) return("-")
      format(round(val, 2), big.mark = ",")
    })
    
    output$industriOverview <- renderText({
      df <- sector_overview_data()
      val <- df %>% filter(`CO2 emissions by sector in Indonesia` == "Industry Sector") %>% pull(Value)
      if (length(val) == 0) return("-")
      format(round(val, 2), big.mark = ",")
    })
    
    output$transportOverview <- renderText({
      df <- sector_overview_data()
      val <- df %>% filter(`CO2 emissions by sector in Indonesia` == "Transport Sector") %>% pull(Value)
      if (length(val) == 0) return("-")
      format(round(val, 2), big.mark = ",")
    })
    
    # 1. Load dan pivot data
    listrik_provinsi_data <- reactive({
      raw_data <- read_excel("data/Tenaga Listrik yang Dibangkitkan Menurut Provinsi 2011-2023.xlsx")
      
      cleaned_data <- raw_data %>%
        rename(Tahun = TAHUN) %>%
        # Perbaikan Disini
        mutate(
          Tahun = as.numeric(Tahun), 
          across(-Tahun, ~as.numeric(gsub(",", ".", gsub("-", NA, .))))
        )
      
      cleaned_data %>%
        pivot_longer(
          cols = -Tahun,
          names_to = "Provinsi",
          values_to = "Value"
        ) %>%
        filter(!is.na(Value))
    })
    
    # 2. Update dropdown provinsi
    observe({
      provinsi_choices <- unique(listrik_provinsi_data()$Provinsi)
      updateSelectInput(session, "provinsi_listrik", 
                        choices = provinsi_choices, 
                        selected = provinsi_choices[1])
    })
    
    # 3. Render grafik
    output$trendListrikProvinsi <- renderPlot({
      data <- listrik_provinsi_data()
      selected_prov <- input$provinsi_listrik  
      
      if (!is.null(selected_prov)) {
        prov_data <- data %>%
          filter(Provinsi == selected_prov) %>%
          arrange(Tahun)
        
        print(head(prov_data))
        print(nrow(prov_data))
        
        
        ggplot(prov_data, aes(x = Tahun, y = Value)) +
          geom_line(color = "green", size = 1.2) +
          geom_point(color = "darkgreen", size = 2) +
          labs(
            title = paste("Tren Emisi Listrik di Provinsi", selected_prov),
            y = "Energi (MWh)",
            x = "Tahun"
          ) +
          theme_minimal()
      }
    })
    
    # --- INDUSTRI: CO2 per Orang (2000–2023)
    data_emisi_industri <- reactive({
      read_excel("data/Jumlah Emisi CO2 yang dihasilkan oleh rata2 satu orang.xlsx") %>%
        mutate(
          Tahun = as.integer(Tahun),
          Global = as.numeric(Global),
          Indonesia = as.numeric(Indonesia),
          Asia = as.numeric(Asia)
        )
    })
    
    output$industriPlot <- renderPlotly({
      df <- data_emisi_industri() %>% filter(Tahun >= 2000 & Tahun <= 2023)
      if (nrow(df) == 0) return(NULL)
      p <- ggplot(df, aes(x = Tahun)) +
        geom_line(aes(y = Global, color = "Global"), size = 0.5) +
        geom_line(aes(y = Indonesia, color = "Indonesia"), size = 0.5) +
        geom_line(aes(y = Asia, color = "Asia"), size = 0.5) +
        geom_point(aes(y = Global, color = "Global"), size = 1) +
        geom_point(aes(y = Indonesia, color = "Indonesia"), size = 1) +
        geom_point(aes(y = Asia, color = "Asia"), size = 1) +
        scale_color_manual(values = c("Global" = "blue", "Indonesia" = "orange", "Asia" = "green")) +
        labs(title = "Emisi CO2 per Orang: Global, Asia, Indonesia",
             x = "Tahun", y = "Ton CO2 per Orang", color = "Wilayah") +
        theme_minimal(base_size = 12)
      ggplotly(p, tooltip = c("x", "y", "color"))
    })
    
    # --- TRANSPORTASI: Kendaraan Bermotor
    data_kendaraan <- reactive({
      df <- read_excel("data/Jumlah Kendaraan Bermotor Menurut Jenis (Unit).xlsx")
      
      df %>%
        mutate(across(c(MobilPenumpang, MobilBis, MobilBarang, SepedaMotor), 
                      ~ as.numeric(gsub("[^0-9.,]", "", gsub(",", ".", .))))) %>%
        mutate(across(c(MobilPenumpang, MobilBis, MobilBarang, SepedaMotor), ~ . / 1e6))
    })
    
    observe({
      tahun_choices <- unique(data_kendaraan()$Tahun)
      updateSelectInput(session, "tahun_filter_kendaraan", choices = tahun_choices, selected = max(tahun_choices))
    })
    
    output$transportPlot <- renderPlotly({
      req(input$tahun_filter_kendaraan)
      df <- data_kendaraan() %>% filter(Tahun == input$tahun_filter_kendaraan)
      if (nrow(df) == 0) return(NULL)
      df <- df %>% pivot_longer(cols = c(MobilPenumpang, MobilBis, MobilBarang, SepedaMotor), 
                                names_to = "Jenis", values_to = "Jumlah") %>% 
        filter(!is.na(Jumlah)) %>% mutate(Jenis = fct_reorder(Jenis, Jumlah))
      p <- ggplot(df, aes(x = Jenis, y = Jumlah, fill = Jenis)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = scales::comma(Jumlah, accuracy = 0.01)), hjust = -0.1, size = 4) +
        coord_flip() +
        labs(title = paste("Jumlah Kendaraan Tahun", input$tahun_filter_kendaraan),
             x = "Jenis Kendaraan", y = "Jumlah (juta)") +
        scale_fill_brewer(palette = "Set2") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      ggplotly(p, tooltip = c("x","y"))
    })
    
# HALAMAN 5 (INFERENSIA)
    # === LOAD DATA REGRESI ===
    data_regresi_full <- reactive({
      df <- read_excel("data/Data regresi.xlsx")
      colnames(df) <- c("Tahun", "Emisi", "Suhu")
      df <- df %>% mutate(logEmisi = log(Emisi))
      return(df)
    })
    
    # Tampilkan 10 baris di tabel
    output$tabelRegresi <- renderDT({
      df <- data_regresi_full()
      
      if (input$filterData == "original") {
        datatable(head(df[, c("Tahun", "Emisi", "Suhu")], 25))
        
      } else if (input$filterData == "inv") {
        df <- df %>% mutate(invEmisi = 1 / Emisi)
        datatable(head(df[, c("Tahun", "invEmisi", "Suhu")], 25))
        
      } else if (input$filterData == "inv2") {
        df <- df %>% mutate(invEmisi2 = 1 / (Emisi^2))
        datatable(head(df[, c("Tahun", "invEmisi2", "Suhu")], 25))
        
      } else if (input$filterData == "invsqrt") {
        df <- df %>% mutate(invSqrtEmisi = 1 / sqrt(Emisi))
        datatable(head(df[, c("Tahun", "invSqrtEmisi", "Suhu")], 25))
      }
    })
    
    
    
    # === MODEL  REGRESI ===
    modelData <- reactive({
      df <- data_regresi_full()
      
      model <- switch(input$filterData,
                      "original" = lm(Suhu ~ Emisi, data = df),
                      "inv" = lm(Suhu ~ I(1 / Emisi), data = df),
                      "inv2" = lm(Suhu ~ I(1 / (Emisi^2)), data = df),
                      "invsqrt" = lm(Suhu ~ I(1 / sqrt(Emisi)), data = df),
                      NULL
      )
      
      return(model)
    })
    
    
    # === PLOT LINEARITAS ===
    output$plotLinearitas <- renderPlot({
      plot(modelData()$fitted.values, modelData()$residuals,
           xlab = "Fitted Values", ylab = "Residuals",
           main = "Plot Linearitas")
      abline(h = 0, col = "red")
    })
    
    # === PLOT Q-Q (NORMALITAS) +SHAPIRO ===
    output$qqplotResidual <- renderPlot({
      qqnorm(resid(modelData()))
      qqline(resid(modelData()), col = "red")
    })
    
    output$shapiroTest <- renderPrint({
      shapiro.test(resid(modelData()))
    })
    
    # === PLOT HOMOSKEDASTISITAS ===
    output$plotHomoskedastisitas <- renderPlot({
      plot(modelData()$fitted.values, abs(resid(modelData())),
           xlab = "Fitted Values", ylab = "|Residual|",
           main = "Plot Homoskedastisitas")
    })
    
    # === BREUSCH-PAGAN TEST ===
    output$ujiBP <- renderPrint({
      req(modelData())
      bp <- bptest(modelData())
      
      cat("Uji Breusch-Pagan untuk Heteroskedastisitas\n")
      cat("Hipotesis:\n")
      cat("H0: Tidak ada heteroskedastisitas (residual memiliki varian konstan)\n")
      cat("H1: Ada heteroskedastisitas (residual memiliki varian tidak konstan)\n\n")
      
      print(bp)
      
      cat("\nDengan α = 0.05\n")
      if (bp$p.value > 0.05) {
        cat("Kesimpulan: Gagal tolak H0. Tidak ada bukti cukup adanya heteroskedastisitas.\n")
      } else {
        cat("Kesimpulan: Tolak H0. Ada bukti heteroskedastisitas.\n")
      }
    })
    
    output$boxcoxPlot <- renderPlot({
      df <- data_regresi_full()
      
      # Pastikan input dan data valid
      validate(
        need(nrow(df) > 0, "Data kosong atau belum dimuat.")
      )
      
    })
    
    
    # === DURBIN-WATSON TEST ===
    output$dwtestResult <- renderPrint({
      dw <- dwtest(modelData())
      cat("Uji Durbin-Watson untuk Autokorelasi Residual\n")
      cat("Hipotesis:\n")
      cat("H0: Tidak ada autokorelasi (residual independen)\n")
      cat("H1: Ada autokorelasi (residual tidak independen)\n\n")
      print(dw)
      cat("\nDengan α = 0.05\n")
      if (dw$p.value > 0.05) {
        cat("Kesimpulan: Gagal tolak H0. Tidak ada cukup bukti autokorelasi.\n")
      } else {
        cat("Kesimpulan: Tolak H0. Terdapat indikasi autokorelasi pada residual.\n")
      }
    })
    
    
    # === COOK'S DISTANCE ===
    output$plotCooksDistance <- renderPlot({
      cooks <- cooks.distance(modelData())
      plot(cooks, type = "h", main = "Cook's Distance")
      abline(h = 4/length(cooks), col = "red", lty = 2)
    })
    
    output$ujiOutlier <- renderPrint({
    req(modelData(), data_regresi_full())
    
    # Hitung studentized residual
    student_resid <- rstudent(modelData())
    cooks_d <- cooks.distance(modelData())
    
    outlier_threshold <- 2  # Aturan umum: |studentized residual| > 2
    outliers <- which(abs(student_resid) > outlier_threshold)
  
    cat("Uji Outlier dengan Studentized Residual\n")
    cat("Hipotesis:\n")
    cat("H0: Tidak ada outlier signifikan\n")
    cat("H1: Ada outlier signifikan\n\n")
    
    if (length(outliers) == 0) {
      cat("Tidak ditemukan outlier signifikan berdasarkan studentized residual (|residual| > 2).\n")
      cat("Kesimpulan: Gagal tolak H0.\n")
    } else {
      cat("Ditemukan outlier signifikan pada observasi:\n")
      print(outliers)
      cat("Kesimpulan: Tolak H0. Ada indikasi data outlier.\n")
    }
    
    cat("\nNilai Cook's Distance terbesar:\n")
    max_cook <- max(cooks_d)
    cat("Cook's D max: ", round(max_cook, 4), "\n")
    if (max_cook > 1) {
      cat("Indikasi adanya pengaruh kuat (influential point).\n")
    } else {
      cat("Tidak ada data yang terlalu berpengaruh (influential).\n")
    }
  })

    modelRobust <- reactive({
      df <- data_regresi_full()
      
      model <- switch(input$filterData,
                      "original" = rlm(Suhu ~ Emisi, data = df),
                      "inv" = rlm(Suhu ~ I(1 / Emisi), data = df),
                      "inv2" = rlm(Suhu ~ I(1 / (Emisi^2)), data = df),
                      "invsqrt" = rlm(Suhu ~ I(1 / sqrt(Emisi)), data = df),
                      NULL
      )
      
      return(model)
    })

    # === RINGKASAN REGRESI ===
    output$regresiSummary <- renderPrint({
      summary(modelData())
    })
    
    # === KESIMPULAN ===
    output$kesimpulanRegresi <- renderPrint({
      model <- modelData()
      summary_text <- summary(model)
      r_squared <- summary_text$r.squared
      p_value <- summary_text$coefficients[2, 4]
      
      cat("Model:", ifelse(input$filterData == "original", "Suhu ~ Emisi", "Suhu ~ log(Emisi)"), "\n")
      cat("R-squared:", round(r_squared, 3), "\n")
      cat("P-value koefisien emisi:", signif(p_value, 3), "\n\n")
      
      if (p_value < 0.05) {
        cat("Dengan α = 5%, terdapat cukup bukti untuk menyimpulkan bahwa emisi berpengaruh signifikan terhadap suhu.")
      } else {
        cat("Dengan α = 5%, tidak terdapat cukup bukti untuk menyimpulkan bahwa emisi berpengaruh signifikan terhadap suhu.")
      }
    })
    
    output$regresiRobust <- renderPrint({
      model <- modelData()
      
      # Estimasi robust SE
      robust_se <- coeftest(model, vcov = sandwich)
      
      cat("=== Regresi dengan Robust Standard Errors ===\n\n")
      print(robust_se)
      
      cat("\nInterpretasi tetap bisa dilakukan pada koefisien.\n")
      cat("Standar error dan p-value sekarang telah dikoreksi terhadap heteroskedastisitas.\n")
    })
    
    generateKesimpulan <- function(model, model_type) {
      summary_text <- summary(model)
      r_squared <- round(summary_text$r.squared, 3)
      p_value <- signif(summary_text$coefficients[2, 4], 3)
      
      interpretation <- switch(model_type,
                               "original" = "Setiap peningkatan 1 satuan emisi diperkirakan meningkatkan suhu sebesar β1 derajat.",
                               "inv" = "Penurunan emisi diasosiasikan dengan perubahan suhu secara non-linear (1/X).",
                               "inv2" = "Penurunan tajam emisi memiliki dampak non-linear (1/X²) terhadap suhu.",
                               "invsqrt" = "Penurunan emisi diasosiasikan dengan perubahan suhu (1/√X).",
                               "Model tidak dikenali."
      )
      
      result <- paste0(
        "Model yang digunakan: ", switch(model_type,
                                         "original" = "Suhu ~ Emisi",
                                         "inv" = "Suhu ~ 1/Emisi",
                                         "inv2" = "Suhu ~ 1/Emisi^2",
                                         "invsqrt" = "Suhu ~ 1/sqrt(Emisi)"
        ), "\n\n",
        
        "Interpretasi:\n", interpretation, "\n\n",
        "Nilai R²: ", r_squared, "\n",
        "P-value koefisien: ", p_value, "\n",
        "Tingkat signifikansi: α = 0.05\n\n",
        if (p_value < 0.05) {
          "Kesimpulan: Terdapat cukup bukti bahwa emisi berpengaruh signifikan terhadap suhu."
        } else {
          "Kesimpulan: Tidak terdapat bukti cukup bahwa emisi berpengaruh signifikan terhadap suhu."
        }
      )
      
      return(result)
    }
    
    
    generateKesimpulanRobust <- function(model, model_type) {
      coef_val <- round(coef(model)[2], 4)
      
      interpretation <- switch(model_type,
                               "original" = "Setiap peningkatan 1 satuan emisi diperkirakan meningkatkan suhu sebesar β1 derajat (model robust).",
                               "inv" = "Penurunan emisi diasosiasikan dengan perubahan suhu secara non-linear (1/X) dalam model robust.",
                               "inv2" = "Penurunan tajam emisi memiliki dampak non-linear (1/X²) terhadap suhu dalam model robust.",
                               "invsqrt" = "Penurunan emisi diasosiasikan dengan perubahan suhu (1/√X) dalam model robust.",
                               "Model tidak dikenali."
      )
      
      result <- paste0(
        "Model Robust yang digunakan: ", switch(model_type,
                                                "original" = "Suhu ~ Emisi",
                                                "inv" = "Suhu ~ 1/Emisi",
                                                "inv2" = "Suhu ~ 1/Emisi^2",
                                                "invsqrt" = "Suhu ~ 1/sqrt(Emisi)"
        ), "\n\n",
        
        "Interpretasi:\n", interpretation, "\n\n",
        "Koefisien emisi (β1) dari model robust: ", coef_val, "\n",
        "Catatan: Uji signifikansi tidak tersedia langsung dari `rlm`, namun model robust lebih tahan terhadap outlier dan heteroskedastisitas.\n"
      )
      
      return(result)
    }
    
    output$kesimpulanRegresi <- renderPrint({
      cat("=== MODEL REGRESI BIASA ===\n\n")
      cat(generateKesimpulan(modelData(), input$filterData))
      
      cat("\n\n\n=== MODEL REGRESI ROBUST ===\n\n")
      cat(generateKesimpulanRobust(modelRobust(), input$filterData))
    })
    
# HALAMAN 6 (METADATA)
 
    # HALAMAN 6 (METADATA)
    # 1. Buat data frame yang berisi informasi metadata
    metadata_df <- tibble(
      ID = 1:12,
      NamaDataset = c(
        "Emisi Gas Rumah Kaca Menurut Sektor",
        "Emisi CO2 Tahunan Per Negara",
        "Emisi CO2 Tahunan Antara Indonesia dan Dunia",
        "Suhu Permukaan Rata-rata",
        "Emisi per Provinsi di Indonesia",
        "Jumlah Kendaraan Bermotor Menurut Jenis",
        "Tenaga Listrik Dibangkitkan per Provinsi",
        "Batas Administrasi Provinsi Indonesia",
        "Data untuk Analisis Regresi", 
        "Evolusi Emisi CO2 per Sektor di Indonesia", 
        "Emisi CO2 per Kapita", 
        "Tren Emisi per Provinsi"
      ),
      Deskripsi = c(
        "Dataset ini merinci emisi gas rumah kaca (GRK) di Indonesia dari tahun 2000-2023, dikelompokkan berdasarkan sektor utama sesuai standar IPCC.",
        "Data emisi CO2 tahunan yang berguna untuk melihat perbandingan negara-negara di Asia Tenggara",
        "Data emisi CO2 tahunan yang berguna untuk melihat perbandingan Indonesia dengan Dunia.",
        "Berisi data anomali suhu permukaan rata-rata tahunan untuk Global, ASEAN, dan Indonesia.",
        "Rincian emisi yang diatribusikan ke masing-masing provinsi di Indonesia, memungkinkan analisis regional.",
        "Jumlah kendaraan bermotor yang terdaftar di Indonesia, dipisahkan berdasarkan jenis: Mobil Penumpang, Bis, Barang, dan Sepeda Motor.",
        "Total energi listrik yang dibangkitkan (dalam GWh atau MWh) per provinsi dari tahun 2011-2023.",
        "Data geospasial dalam format GeoJSON yang berisi poligon batas wilayah administrasi level 1 (provinsi) untuk Indonesia.",
        "Dataset yang telah diproses untuk analisis regresi, menghubungkan emisi dengan faktor lain.", 
        "Data historis perkembangan emisi CO2 per sektor di Indonesia sejak tahun 2000 untuk analisis tren.", 
        "Data emisi CO2 per kapita (per orang) yang dihitung dari total emisi nasional dibagi jumlah penduduk.", 
        "Menunjukkan tren atau kecenderungan perubahan emisi di tingkat provinsi selama periode 2006-2024."
      ),
      Sumber = c("KLHK/BPS (disadur)", "Our World in Data/IEA", "Our World in Data/IEA", "NASA GISS/Berkeley Earth", "Kompilasi Data Regional", "BPS", "BPS/PLN", "GADM","Olah Data Mandiri", "Our World in Data/IEA", 
                 "Kalkulasi/Olah Data", "Kompilasi Data Regional"),
      TipeFile = c("Excel", "Excel", "Excel", "Excel", "Excel", "Excel", "Excel", "GeoJSON", "Excel", "CSV", "Excel", "Excel"),
      PathFile = c(
        "(2) Emisi Gas Rumah Kaca Menurut Sektor 2000-2023.xlsx",
        "(3) Annual CO2 Emissions.xlsx",
        "Annual co2 emissions per country.xlsx",
        "average surface temperature global, asean, and indonesia.xlsx",
        "Emission by province in Indonesia.xlsx",
        "Jumlah Kendaraan Bermotor Menurut Jenis (Unit).xlsx",
        "Tenaga Listrik yang Dibangkitkan Menurut Provinsi 2011-2023.xlsx",
        "gadm41_IDN_1.json",
        "Data regresi.xlsx", 
        "Evolution of CO2 emissions by sector in indonesia since 2000.csv", 
        "Jumlah emisi CO2 yang dihasilkan oleh rata2 satu orang.xlsx", 
        "Tren Emisi Per Provinsi 2006-2024.xlsx"  
      )
    )
    
    # 2. Render Tabel Interaktif
    output$metadata_table <- DT::renderDataTable({
      df_display <- metadata_df %>%
        mutate(Aksi = paste0(
          '<button class="btn btn-info btn-sm" onclick="Shiny.setInputValue(\'view_meta_id\', ', ID, ', {priority: \'event\'})">Lihat Detail</button>'
        )) %>%
        select(NamaDataset, Deskripsi, Sumber, Aksi)
      
      datatable(
        df_display,
        escape = FALSE,
        rownames = FALSE,
        options = list(
          paging = TRUE,      
          pageLength = 6,     
          info = TRUE,        
          lengthChange = FALSE, 
          columnDefs = list(list(className = 'dt-center', targets = 3))
        ),
        class = "display compact"
      )
    })
    
    # 3. Observer untuk menangani klik tombol "Lihat Detail"
    observeEvent(input$view_meta_id, {
      req(input$view_meta_id)
      
      # Ambil data untuk item yang dipilih
      selected_data <- metadata_df %>% filter(ID == input$view_meta_id)
      
      # Tampilkan modal pop-up
      showModal(modalDialog(
        title = selected_data$NamaDataset,
        p(strong("Deskripsi: "), selected_data$Deskripsi),
        p(strong("Sumber: "), selected_data$Sumber),
        p(strong("Tipe File: "), selected_data$TipeFile),
        hr(),
        h4("Unduh File", style = "text-align:center;"),
        # Tombol Unduhan
        div(style="display:flex; justify-content:center; gap:15px;",
            downloadButton("download_metadata_file", "Unduh Metadata (.txt)"),
            downloadButton("download_raw_file", "Unduh Data Asli")
        ),
        easyClose = TRUE,
        footer = modalButton("Tutup")
      ))
    })
    
    # 4. Reactive value untuk menyimpan ID item yang aktif di modal
    active_metadata_id <- eventReactive(input$view_meta_id, {
      input$view_meta_id
    })
    
    # 5. Download Handler untuk file METADATA
    output$download_metadata_file <- downloadHandler(
      filename = function() {
        id <- active_metadata_id()
        item_name <- metadata_df$NamaDataset[metadata_df$ID == id]
        paste0("metadata-", gsub(" ", "_", tolower(item_name)), ".txt")
      },
      content = function(file) {
        id <- active_metadata_id()
        item <- metadata_df[metadata_df$ID == id, ]
        
        # Tulis konten ke file .txt
        writeLines(
          c(
            paste("Nama Dataset:", item$NamaDataset),
            paste("Deskripsi:", item$Deskripsi),
            paste("Sumber:", item$Sumber),
            paste("Tipe File:", item$TipeFile)
          ),
          file
        )
      }
    )
    
    # 6. Download Handler untuk file DATA ASLI
    output$download_raw_file <- downloadHandler(
      filename = function() {
        id <- active_metadata_id()
        basename(metadata_df$PathFile[metadata_df$ID == id])
      },
      content = function(file) {
        id <- active_metadata_id()
        file.copy(metadata_df$PathFile[metadata_df$ID == id], file)
      }
    )
    
# HALAMAN 7 (TENTANG KAMI)
}

#============================== *********** .RUN APP *********** ==============================#
shinyApp(ui = ui, server = server)