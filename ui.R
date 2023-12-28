library(shinydashboard)
library(scales)

# Menambahkan resource path

dashboardPage(
  # ----- HEADER -----
  dashboardHeader(title = "Menu"),
  
  # ----- SIDEBAR -----
  dashboardSidebar(
    # menambahkan beberapa menu pada sidebar
    sidebarMenu(
      menuItem(" Ringkasan", tabName = "page1", icon = icon("clipboard")),
      menuItem(" Overview", tabName = "page2", icon = icon("clipboard")),
      menuItem(" Map", tabName = "page3", icon = icon("map-location-dot")),
      menuItem(" LRFM", tabName = "page4", icon = icon("table")),
      menuItem(" Customers", tabName = "page5", icon = icon("users")),
      menuItem(" Profil", tabName = "page6", icon = icon("list")),
      inverse = T
    )
  ),
  
  # ----- BODY -----
  dashboardBody(
    # ----- Membuat Judul ------

    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Overview Olist E-Commerce Performance in Brazil </span>\');
      })
     ')),
    
    tags$head(
      tags$style(HTML("
    .small-box .info-box-content {
      border-radius: 10px;
    }
  "))
    ),
  
  
  # tags$style(
  #   HTML("
  #            .project-description {
  #              max-width: 100%;
  #              width: 100%;
  #            }
  #          ")),
  
  
    # tags$head(
    #   tags$style(HTML("
    #   .tab-content {
    #     width: 80vw !important;
    #   }
    # "))
    # ),
    # ----- TAB (karena ada lebih dari satu tab) -----
    tabItems(
      # ----- TAB1 -----
      tabItem(
        tabName = "page1", # tabname itu identitas
        mainPanel(width =12,
          tabsetPanel(
            tabPanel(title = "Flow",
                     fluidRow(
                       column(
                         width = 12,
                         tags$img(
                           src = "Images/judul.png",
                           width = "100%",
                           height = "100%",
                           align = "center"
                         )
                       )
                     )
            ),
            tabPanel("Latar Belakang",
                     br(),
                     hr(),
                       h4(strong("Project Description")),
                     fluidRow(
                       box(width =12,
                       style = "text-align: center;",
                       column(
                         width = 12,
                         tags$img(
                           src = "Images/mckensey.png",
                           width = "60%",
                           height = "auto",
                           style = "margin: 0 auto;",
                           align = "center"
                         )
                       )
                     )
                     ),
                     box(width =12,
                     br(),
                       HTML("Dalam lingkungan bisnis yang sangat kompetitif, penting bagi perusahaan untuk memahami pelanggannya dengan lebih baik untuk meningkatkan retensi pelanggan, loyalitas, dan profitabilitas. Mengutip pernyataan dari McKinsey’s John Forsyth di laman Harvard Business Review, banyak bisnis kesulitan mengidentifikasi pelanggan berharga mereka dan mengembangkan strategi pemasaran yang tepat untuk mengoptimalkan keterlibatan pelanggan dan meningkatkan pertumbuhan penjualan. Oleh karena itu, diperlukan pemanfaatan wawasan yang didorong data untuk membagi segmen pelanggan dan mengembangkan kampanye pemasaran yang dipersonalisasi."),
                       br(),
                       br(),
                       HTML("Menurut Thalkar di jurnalnya yang berjudul “Customer Segmentation Using Machine Learning”, customer segmentation atau segmentasi pelanggan penting dalam bisnis karena memungkinkan perusahaan untuk mengetahui preferensi dan kebutuhan pelanggan mereka secara lebih baik, sehingga mereka dapat mengkomunikasikan produk dan layanan mereka secara lebih efektif dan meningkatkan penjualan. Melalui teknik ini, perusahaan juga dapat mengalokasikan anggaran pemasaran mereka dengan lebih baik dan mengidentifikasi peluang pasar baru. Selain itu, customer segmentation juga dapat membantu perusahaan membuat keputusan bisnis yang lebih baik dengan menganalisis preferensi dan kebiasaan pembelian pelanggan mereka."),
                       br(),
                       br(),
                       HTML("Metode customer segmentation yang umum digunakan adalah RFM (Recency, Frequency, Monetary Value), yang mengukur tiga faktor utama, yaitu waktu pembelian terakhir, frekuensi pembelian, dan nilai pembelian. Namun, metode RFM memiliki kelemahan, yaitu tidak mempertimbangkan faktor-faktor seperti durasi hubungan pelanggan dengan perusahaan dan jenis produk yang dibeli."),
                       br(),
                       br(),
                       HTML("Oleh karena itu, dikembangkanlah metode customer segmentation yang lebih canggih, yaitu LRFM (Length of Relationship, Recency, Frequency, Monetary Value), yang juga mempertimbangkan faktor durasi hubungan pelanggan dengan perusahaan. Berdasarkan penelitian oleh (Ali n Ahmad, 2018) dalam jurnalnya “A New Approach for Customer Clustering by Integrating the LRFM Model and Fuzzy Inference System” Metode LRFM ini telah terbukti memberikan pemahaman yang lebih baik tentang karakteristik pelanggan, termasuk faktor-faktor yang tidak dipertimbangkan oleh metode RFM.")
                     ,
                     hr()
                     )
            ),
            tabPanel("Data",
                     fluidRow(
                       box(width =12,
                       style = "text-align: center;",
                       column(
                         width = 12,
                         tags$img(
                           src = "Images/Olist.png",
                           width = "60%",
                           height = "auto",
                           style = "margin: 0 auto;",
                           align = "center"
                         )
                       )
                     )),
                     br(),
                     box(width =12,
                     h4(strong("Informasi Singkat")),
                     HTML("Dataset Brazilian E-Commerce Public Dataset by Olist adalah kumpulan data yang berisi informasi tentang transaksi e-commerce di Brasil oleh Olist, department store terbesar di pasar Brasil dari tahun 2016 hingga 2018. Dataset ini mencakup informasi tentang lebih dari 100.000 pesanan yang dilakukan oleh pelanggan Olist, termasuk informasi tentang produk yang dibeli, harga, tanggal pembelian, metode pembayaran, dan lokasi pengiriman.

Selain itu, dataset ini juga berisi informasi tentang pelanggan Olist, termasuk nama, alamat email, nomor telepon, dan informasi demografis seperti jenis kelamin, usia, dan lokasi geografis.

Dataset ini sangat berguna untuk membangun model RFM (Recency, Frequency, Monetary) yang dapat membantu perusahaan e-commerce dalam strategi pemasaran mereka."),
                     hr()),
                     box(width =12,
                     h4(strong("Sumber Data")),
                     HTML("Dataset bersumber dari kaggle, dengan link [kaggle.com](https://www.kaggle.com/datasets/olistbr/brazilian-ecommerce?datasetId=55151&sortBy=voteCount&searchQuery=rfm).

Ini adalah data komersial yang nyata, telah dianonimkan, dan referensi ke perusahaan dan mitra dalam teks ulasan telah diganti dengan nama-nama rumah besar Game of Thrones. Teknik pengumpulannya menggunakan random sampling."),
                     hr())
            ),
            tabPanel(title = "Flow",
                     fluidRow(
                       column(
                         width = 12,
                         tags$img(
                           src = "Images/flow.png",
                           width = "100%",
                           height = "100%",
                           align = "center"
                         )
                       )
                     )

            
            )
            ),
        )
      ),
      # ----- TAB2 -----
      tabItem(tabName = "page2", # tabname itu identitas
              fluidRow( # Membagi bagian dalam halaman (secara row) dengan menggunakan parameter infoBox
                (valueBoxOutput(width = 3,
                                outputId = "amount_valuebox")
                ),
                (valueBoxOutput(width = 3,
                                outputId = "transaction_valuebox")
                ),
                (valueBoxOutput(width = 3,
                                outputId = "customer_valuebox")
                ),
                (valueBoxOutput(width = 3,
                                outputId = "rentang_valuebox")
                 )
              ),
              
              # ----- Plot1 -----
              tabsetPanel(
                
                # ----- TAB1 -----
              tabPanel("Overview",
              fluidRow( #Kotak bayangan untuk menaruh kertas/box
                tags$head(tags$style("
                     #plot1{height:300px !important;}
                     ")),
                box(title = "Distribusi Order Status", status = "primary", solidHeader = T, collapsible = T,#Kertas/alas untuk menaruh plot1
                  width = 6,
                  plotlyOutput("plot1")
                ),
              
              # ----- Plot2 -----
                box(title = "Tipe Pembayaran", status = "primary", solidHeader = T, collapsible = T,#Kotak bayangan untuk menaruh kertas/box
                tags$head(tags$style("
                     #plot2{height:300px !important;}
                     ")),
                  width = 6,
                  plotlyOutput("plot2")
                )
              ),
              
              # ----- Plot6 -----
              fluidRow( #Kotak bayangan untuk menaruh kertas/box
                tags$head(tags$style("
                     #plot6{height:300px !important;}
                     ")),
                box(title = "Total Pelanggan berdasarkan Repeat Order", status = "primary", solidHeader = T, collapsible = T, #Kertas/alas untuk menaruh plot1
                  width = 6,
                  plotlyOutput("plot6")
                ),
                # ----- Plot9 -----
                box(title = "Distribusi Kategori Produk", status = "primary", solidHeader = T, collapsible = T, #Kotak bayangan untuk menaruh kertas/box
                  tags$head(tags$style("
                     #plot9{height:300px !important;}
                     ")),
                  width = 6,
                  plotlyOutput("plot9")
                )
              )
              ),
              
              # ----- TAB2 -----
              tabPanel("Transaksi",
                box(width = 12,
                    #uiOutput("select_city")
                    selectInput(inputId = "selected_year",
                                label = "Pilih Tahun",
                                choices = c("Semua","2016", "2017", "2018"),
                                selected = "Semua")),       
                # ----- Plot3 -----
                box( #Kotak bayangan untuk menaruh kertas/box
                  tags$head(tags$style("
                       #plot3{height:300px !important;}
                       ")),
                  width = 12,
                  plotlyOutput("plot3")
                ),
              
              # ----- Plot4 -----
              fluidRow( #Kotak bayangan untuk menaruh kertas/box
                tags$head(tags$style("
                     #plot4{height:300px !important;}
                     ")),
                box( #Kertas/alas untuk menaruh plot1
                  width = 6,
                  plotlyOutput("plot4")
                ),
                
              #----- Plot5 -----
                box( #Kotak bayangan untuk menaruh kertas/box
                  tags$head(tags$style("
                     #plot5{height:300px !important;}
                     ")),
                  width = 6,
                  plotlyOutput("plot5")
                )
              ),
              ),
              )
              
      ),
      
      # ----- TAB3 -----
      tabItem(tabName = "page3",
              #----- SelectBox -----
              fluidRow(
                box(width = 12,
                            #uiOutput("select_city")
                            pickerInput(inputId = "select_city",
                                        label = "Pilih Kota",
                                        choices = unique(database$customer_state_name),
                                        options = list(`actions-box` = T), multiple = T, selected = unique(database$customer_state_name))
                )),
              
              fluidRow(
                # ----- Plot2 -----
                box(width = 12,
                    leafletOutput("map")
                )
              ),
              
              fluidRow(
              # ----- Plot7 -----
              box( #Kotak bayangan untuk menaruh kertas/box
                tags$head(tags$style("
                                     #plot7{height:500px !important;}
                                     ")),
                width = 6,
                plotlyOutput("plot7")
              ),
              # ----- Plot8 -----
              box( #Kotak bayangan untuk menaruh kertas/box
                tags$head(tags$style("
                                     #plot8{height:500px !important;}
                                     ")),
                width = 6,
                plotlyOutput("plot8")
              )
            ),
              
      ),
      
      # ----- TAB4 -----
      
      tabItem(tabName = "page4",
              fluidRow( # Membagi bagian dalam halaman (secara row) dengan menggunakan parameter infoBox
                (valueBoxOutput(width = 3,
                                outputId = "amount_lrfm")
                ),
                (valueBoxOutput(width = 3,
                                outputId = "transaction_lrfm")
                ),
                (valueBoxOutput(width = 3,
                                outputId = "customer_lrfm")
                ),
                (valueBoxOutput(width = 3,
                                outputId = "rentang_lrfm")
                )
              ),
              #----- SelectBox -----
              

              fluidRow(
                box(width = 4,
                    #uiOutput("select_city")
                    pickerInput(inputId = "select_lrfm",
                                label = "Pilih Metrik",
                                choices = unique(cluster_lrfm_gathered$features),
                                options = list(`actions-box` = T), multiple = T, selected = unique(cluster_lrfm_gathered$features))
                ),
                box(width = 8,
                    #uiOutput("select_city")
                    pickerInput(inputId = "select_cluster",
                                label = "Pilih Cluster",
                                choices = unique(cluster_lrfm_gathered$fm_segment),
                                options = list(`actions-box` = T), multiple = T, selected = unique(cluster_lrfm_gathered$fm_segment))
                ),


              ),
              # ----- Plot1 -----
              tabsetPanel(
                
                # ----- TAB1 -----
                tabPanel("Overview",
              fluidRow(
                
                # ----- cluster -----
                box( #Kotak bayangan untuk menaruh kertas/box
                  tags$head(tags$style("
                                     #cluster{height:500px !important;}
                                     ")),
                  width = 4,
                  plotlyOutput("lrfm")
                ),
                # ----- lrfm -----
                box( #Kotak bayangan untuk menaruh kertas/box
                  tags$head(tags$style("
                                     #lrfm{height:500px !important;}
                                     ")),
                  width = 8,
                  plotlyOutput("cluster")
                )
                )
              ),
              
              tabPanel("Profil",
                       fluidRow( #Kotak bayangan untuk menaruh kertas/box
                         
                     # ----- Plot10 -----
                         tags$head(tags$style("
                     #plot1{height:300px !important;}
                     ")),
                     box(title = "Distribusi Order Status", status = "primary", solidHeader = T, collapsible = T,#Kertas/alas untuk menaruh plot1
                         width = 6,
                         plotlyOutput("plot10")
                     ),
                     # ----- Plot11 -----
                     box(title = "Distribusi Kategori Produk", status = "primary", solidHeader = T, collapsible = T, #Kotak bayangan untuk menaruh kertas/box
                         tags$head(tags$style("
                     #plot9{height:300px !important;}
                     ")),
                     width = 6,
                     plotlyOutput("plot11")
                     )
                    
              ),
              fluidRow( #Kotak bayangan untuk menaruh kertas/box
                
                # ----- Plot10 -----
                tags$head(tags$style("
                     #plot1{height:300px !important;}
                     ")),
                box(title = "Distribusi Order State", status = "primary", solidHeader = T, collapsible = T,#Kertas/alas untuk menaruh plot1
                    width = 12,
                    plotlyOutput("plot12")
              )
              )
              ),
              tabPanel("Map",
              fluidRow(
                # ----- Plot2 -----
                box(width = 12,
                    leafletOutput("map2")
                )
              )
              ),
              )
    ),

# ----- TAB5 -----
tabItem(tabName = "page5",
        #----- SelectBox -----
        fluidRow(
          box(width = 6,
              #uiOutput("select_city")
              pickerInput(inputId = "select_segment",
                          label = "Pilih Segmentasi",
                          choices = unique(data$fm_segment),
                          options = list(`actions-box` = T), multiple = T, selected = unique(data$fm_segment))
          ),
          
          box(width = 6,
              #uiOutput("select_city")
              pickerInput(inputId = "select_kolom",
                          label = "Pilih kolom yang akan ditampilkan",
                          choices = names(data),
                          options = list(`actions-box` = T), multiple = T, selected = names(data))
          )
          ),
        
        fluidRow(
          # ----- Plot2 -----
          box(width = 12,
              dataTableOutput("customer_s")
          )
          
          
        ),
        
),


# ----- TAB6 -----
tabItem(tabName = "page6",
        #----- SelectBox -----
        fluidRow(
          box(width =12,
              style = "text-align: center;",
              column(
                width = 12,
                tags$img(
                  src = "Images/img_tri.jpg",
                  width = "10%",
                  height = "auto",
                  style = "margin: 0 auto;",
                  align = "center"
                )
              ),
                 h3("Tri Saputra Ungko S.Si"),
                 "Sebagai lulusan matematika yang mahir dalam Python, R, dan SQL, saya memiliki minat yang kuat dalam teknologi, Machine Learning, dan AI. Saya dikenal sebagai individu yang rajin dan mampu bekerja dengan baik di bawah tekanan, baik secara mandiri maupun dalam tim. Saya memiliki keinginan untuk terus belajar dan mengembangkan keterampilan untuk menghadapi tantangan di masa depan. Dengan kombinasi keterampilan teknis dan dorongan untuk terus belajar, saya yakin dengan kemampuan saya untuk memberikan kontribusi yang signifikan bagi organisasi dan lingkungan kerja saya nantinya."
          )
          ),
        fluidRow(
          box(width = 12,
              "Contact Person :",
              tags$div(style = "display: flex; justify-content: center; align-items: center; height: 5vh;",
          tags$li(class = "dropdown",
                  style = "display: flex; align-items: center; list-style: none;",
                  tags$a(href = "https://www.linkedin.com/in/tri-ungko/", target = "_blank",
                         style = "margin-right: 100px;",
                         icon("linkedin"), "LinkedIn"),
                  tags$a(href = "https://rpubs.com/trisaputrau", target = "_blank",
                         style = "margin-right: 100px;",
                         icon("r-project"), "RPubs"),
                  tags$a(href = "mailto:triungko@gmail.com", target = "_blank",
                         style = "margin-right: 100px;",
                         icon("envelope"), "Email"),
                  tags$a(href = "https://github.com/trisaputrau", target = "_blank",
                         style = "margin-right: 100px;",
                         icon("github"), "GitHub"),
                  tags$a(href = "https://www.instagram.com/trisaputrau", target = "_blank",
                         style = "margin-right: 100px;",
                         icon("instagram"), "Instagram")
          )
          ))
        ),
        
        
        
)

  )
)
)



