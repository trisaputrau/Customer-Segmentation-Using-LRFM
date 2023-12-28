# Define server logic required to draw a histogram
function(input, output, session) {

  # ------------------------------------------------------ TAB 1 ------------------------------------------------------------
 
  # ------ VALUEBOX (REPEAT) ------ 

  output$rentang_valuebox <- renderValueBox({
    
    if (input$selected_year == "Semua") {
      year_filter <- database
    } else {
      year_filter <- database %>%
        filter(format(order_purchase_year_month_day, "%Y") == input$selected_year)
    }

    # Menghitung nilai avg transaction
    avg_transaction <- (length(year_filter$order_id) / length(unique(year_filter$customer_unique_id)))
    # Mengubah angka desimal menjadi rentang
    rentang <- paste(floor(avg_transaction), ceiling(avg_transaction), sep = "-")
    # Menampilkan Value Box dengan rentang sebagai nilai value
    
    valueBox(
      value = rentang,
      subtitle = "Avg Transaction",
      icon = icon("repeat"),
      color = "aqua"
    )
  })
  
  # ------ VALUEBOX (Amount) ------ 
  output$amount_valuebox <- renderValueBox({
    
    if (input$selected_year == "Semua") {
      year_filter <- database
    } else {
      year_filter <- database %>%
        filter(format(order_purchase_year_month_day, "%Y") == input$selected_year)
    }

    # Menghitung nilai amount
    amount <- sum(year_filter$price + year_filter$freight_value)
    
    valueBox(
      subtitle = "Amount",
      value = label_number_si(accuracy = 0.1, prefix = "R$ ")(round(amount, 2)),
      icon = icon("money-bill"),
      width = 3,
      color = "aqua"
    )
  })
  
  # ------ VALUEBOX (total_transaction ) ------ 
  output$transaction_valuebox <- renderValueBox({
    
    if (input$selected_year == "Semua") {
      year_filter <- database
    } else {
      year_filter <- database %>%
        filter(format(order_purchase_year_month_day, "%Y") == input$selected_year)
    }
    # Menghitung nilai total_transaction 
    total_transaction <- length(year_filter$order_id)
    
    valueBox(
      value = format(total_transaction, big.mark = "."),
      subtitle = "Total Transaction",
      width = 3,
      icon = icon("cart-shopping"),
      color = "aqua"
    )
  })
  
  # ------ VALUEBOX (total_customer) ------ 
  output$customer_valuebox <- renderValueBox({
    
    if (input$selected_year == "Semua") {
      year_filter <- database
    } else {
      year_filter <- database %>%
        filter(format(order_purchase_year_month_day, "%Y") == input$selected_year)
    }
    # Menghitung nilai total_customer 
    total_customer <- length(unique(year_filter$customer_unique_id))
    
    valueBox(
      value = format(total_customer, big.mark = "."),
      subtitle = "Total Customer",
      width = 3,
      icon = icon("users"),
      color = "aqua"
    )
  })

  
  # ------ PLOT1 (Order Status) ------
  output$plot1 <- renderPlotly({
  # table count of order_status
  status_count <- df_order %>% 
    count(order_status) %>% 
    mutate(pct = n/sum(n)) %>% 
    mutate(popup=glue("Status : {order_status}
                    Frekuensi : {n}"))

  # plot the count table
  order_status_plt <- ggplot(status_count, aes(n, reorder(order_status, n))) +
    geom_bar(stat="identity", fill = "skyblue", aes(text=popup)) +
    labs(title= "Distribusi Order Status", x="Frekuensi", y="Order Status") +
    theme_minimal() +
    geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3.5, hjust = 1000) +
    scale_x_continuous(labels = scales::comma) + 
    theme(plot.title = element_text(size = 13)) +
    theme(panel.background = element_rect(fill='white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggplotly(order_status_plt, tooltip = "text") %>% 
    layout(title = list(text = paste0('Olist E-Commerce September 2016 - Oktober 2018')))
  
  })
  
  # ------ PLOT2 (Payment Type) ------
  output$plot2 <- renderPlotly({
    # table count of payment_type
    
    # Menggunakan gsub untuk mengganti "_" menjadi " " di kolom "payment_type"
    database$payment_type <- gsub("_", " ", database$payment_type)
    
    status_count <- database %>% 
      filter((order_status %in% c("delivered"))) %>%
      count(payment_type) %>% 
      mutate(pct = n/sum(n)) %>% 
      mutate(popup=glue("Tipe Pembayaran : {payment_type}
                    Frekuensi : {n}"))
    
    # plot the count table
    payment_type_plt <- ggplot(status_count, aes(n, reorder(payment_type, n))) +
      geom_bar(stat="identity", fill = "skyblue", aes(text=popup)) +
      labs(title= "Distribution of Order Status", x="Frekuensi", y="Tipe Pembayaran") +
      theme_minimal() +
      geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3.5, hjust = 1000) +
      scale_x_continuous(labels = scales::comma) +
    theme(plot.title = element_text(size = 13)) +
      theme(panel.background = element_rect(fill='white'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggplotly(payment_type_plt, tooltip = "text") %>% 
      layout(title = list(text = paste0('Olist E-Commerce September 2016 - Oktober 2018')))
    
  })
  
  # ------ PLOT6 (Repeat Order) ------ 
  output$plot6 <- renderPlotly({  
    cust_freq_order <- database %>% 
      filter((order_status %in% c("delivered"))) %>%
      select(customer_unique_id,order_id) %>% 
      distinct() %>% 
      group_by(customer_unique_id) %>% 
      summarise(freq = n()) %>% 
      ungroup() %>% 
      group_by(freq) %>% 
      summarise(
        count_cust = n()
      ) %>% 
      ungroup() %>% 
      mutate(
        normalisasi = (count_cust-min(count_cust))/(max(count_cust)-min(count_cust))+0.1,
        popup = glue("Repeat Order: {freq}
                  Banyaknya customer: {count_cust}")
      )
    
    plot_cust_freq_order <- ggplot(cust_freq_order,aes(freq,normalisasi))+
      geom_bar(aes(fill=freq,text=popup), stat = "identity", position = position_dodge2(padding = 2), show.legend = F)+
      geom_text(aes(label=count_cust, y=normalisasi+0.02),size=3, color="black")+
      labs(
        title="Total Customer based on Repeat Order",
        x = "Frequency of Repeat Order",
        y=""
      )+
      theme_minimal() +
      scale_x_continuous(limits = c(0,(max(cust_freq_order$freq)+2)),breaks = c(0:(max(cust_freq_order$freq)+2)))+
      scale_fill_gradient(low="skyblue",high="skyblue")+
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      )
    
    ggplotly(plot_cust_freq_order, tooltip = "text") %>%
      layout(title = list(text = paste0('Olist E-Commerce September 2016 - Oktober 2018')))
    
    
  })
  
  # ------ PLOT9 (Category Type) ------
  output$plot9 <- renderPlotly({
    # table count of payment_type
    # Menggunakan gsub untuk mengganti "_" menjadi " " di kolom "product_category_name_english"
    database$product_category_name_english <- gsub("_", " ", database$product_category_name_english)
    
    status_count <- database %>% 
      filter((order_status %in% c("delivered"))) %>%
      count(product_category_name_english) %>% 
      mutate(pct = n/sum(n)) %>% 
      mutate(popup=glue("Kategori Produk : {product_category_name_english}
                    Frekuensi : {n}")) %>% 
      arrange(-n) %>% 
      head(10)
    
    # plot the count table
    product_type_plt <- ggplot(status_count, aes(n, reorder(product_category_name_english, n))) +
      geom_bar(stat="identity", fill = "skyblue", aes(text=popup)) +
      labs(title= "Distribution of Product Category", x="Frekuensi", y="Kategori Produk") +
      theme_minimal() +
      geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3.5, hjust = 1000) +
      scale_x_continuous(labels = scales::comma) +
      theme(plot.title = element_text(size = 13)) +
      theme(panel.background = element_rect(fill='white'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggplotly(product_type_plt, tooltip = "text") %>% 
      layout(title = list(text = paste0('Olist E-Commerce September 2016 - Oktober 2018')))
    
  })
  
  
  # ------------------------------------------------------ TAB 2 ------------------------------------------------------------
  
  
  # ------ PLOT3 (Year-Month) ------  
  
  database$order_purchase_year_month_day <- as.Date(database$order_purchase_year_month_day)
  output$plot3 <- renderPlotly({  
    
    selected_year <- input$selected_year
    
    if (selected_year == "Semua") {
      filtered_data <- database
      plot_title <- "Pertumbuhan Frekuensi Orders per Bulan<br><sup>Frekuensi order bulanan Tahun 2016 - 2018</sup>"
    } else {
      filtered_data <- database %>%
        filter(format(order_purchase_year_month_day, "%Y") == selected_year)
      plot_title <- glue::glue("Pertumbuhan Frekuensi Orders per Bulan<br><sup>Frekuensi order bulanan Tahun {selected_year}</sup>")
    }
    
    Evolution_plot <- filtered_data %>%
      group_by(order_purchase_year_month_day) %>%
      summarise(freq = n()) %>%
      ungroup() %>%
      mutate(popup = glue("Tanggal : {format(order_purchase_year_month_day, '%d/%m/%Y')} | Frekuensi : {freq}")) %>%
      ggplot(aes(order_purchase_year_month_day, freq, group = 1)) +
      geom_line(color = "skyblue", size = 1) +
      geom_point(color = "black", size = 0.3, alpha = 0.9, aes(text = popup)) +
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Evolution of Total Orders in Brazilian E-Commerce",
           x = "Tanggal", y = "Total Orders") +
      theme_minimal() +
      theme(plot.title = element_text(size = 13),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_text(size = 8)) +
      theme(panel.background = element_rect(fill = 'white'),
            panel.grid.minor = element_blank())
    
    Evolution_plot <- Evolution_plot +
      scale_x_date(labels = date_format("%m/%Y"), 
                   date_breaks = "1 month") 
    
    ggplotly(Evolution_plot, tooltip = "text") %>%
      layout(title = list(text = plot_title))

  
  })
  
  # ------ PLOT4 (day of week) ------ 
  output$plot4 <- renderPlotly({   
    selected_year <- input$selected_year
    
    if (selected_year == "Semua") {
      filtered_data <- database
      plot_title <- "Total Orders by Day of Week<br><sup>Frekuensi order bulanan Tahun 2016 - 2018</sup>"
    } else {
      filtered_data <- database %>%
        filter(format(order_purchase_year_month_day, "%Y") == selected_year)
      plot_title <- glue::glue("Total Orders by Day of Week<br><sup>Frekuensi order bulanan Tahun {selected_year}</sup>")
    }
  dayofweak_plot <- filtered_data %>% 
    filter((order_status %in% c("delivered"))) %>%
    group_by(order_purchase_day0fweek) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(pct = n/sum(n)) %>% 
    mutate(popup=glue("Day of Week : {order_purchase_day0fweek}
                    Frequency : {n}")) %>% 
    ggplot(aes(order_purchase_day0fweek, n, fill = order_purchase_day0fweek)) +
    geom_col(aes(text=popup), show.legend = FALSE)+
    scale_y_continuous(labels = scales::comma)+
    labs(title = "Total Orders by Day of Week",
         x = "Day of Week", y = "Total Orders") +
    theme_minimal() +
    theme(panel.background = element_rect(fill='white'))+
    theme(panel.background = element_rect(fill='white'),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3, color = "Black") +
    scale_fill_brewer(palette = 1) +
    theme(legend.position="none")
  
  ggplotly(dayofweak_plot, tooltip = "text") %>%
    layout(title = list(text = plot_title))
  
 })
  
  # ------ PLOT5 (day) ------ 
  output$plot5 <- renderPlotly({  
    
    selected_year <- input$selected_year
    
    if (selected_year == "Semua") {
      filtered_data <- database
      plot_title <- "Total Orders by Dayk<br><sup>Frekuensi order bulanan Tahun 2016 - 2018</sup>"
    } else {
      filtered_data <- database %>%
        filter(format(order_purchase_year_month_day, "%Y") == selected_year)
      plot_title <- glue::glue("Total Orders by Day<br><sup>Frekuensi order bulanan Tahun {selected_year}</sup>")
    }
    
    timeday_plot <- filtered_data %>% 
      filter((order_status %in% c("delivered"))) %>%
      group_by(order_purchase_time_day) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(pct = n/sum(n)) %>% 
      mutate(popup=glue("Day of Week : {order_purchase_time_day}
                    Frequency : {n}")) %>% 
      ggplot(aes(order_purchase_time_day, n, fill = order_purchase_time_day)) +
      geom_col(show.legend = F, aes(text=popup))+
      scale_y_continuous(labels = scales::comma)+
      labs(title = "Total Orders by Day", 
           x = "Time of Day", y = "Total Orders") +
      theme_minimal() +
      theme(plot.title = element_text(size = 13), 
            axis.title=element_text(size=8)) +
      theme(panel.background = element_rect(fill='white'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())+
      geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3, vjust = 1.3, color = "black")+ 
      scale_fill_brewer(palette = 1) +
      theme(legend.position="none")

    ggplotly(timeday_plot, tooltip = "text") %>%
      layout(title = list(text = plot_title))
    
  
  })
  

  
  # ------ PLOT7 (customer state) ------
  output$plot7 <- renderPlotly({
    plot_total_customer <-  database %>% 
      select(customer_unique_id,customer_state_name) %>% 
      filter(customer_state_name %in% input$select_city) %>% 
      distinct() %>% 
      group_by(customer_state_name) %>% 
      summarise(
        freq = n()
      ) %>% 
      ungroup() %>% 
      mutate(
        customer_state_n = reorder(customer_state_name,freq),
        popup = glue("State: {customer_state_name}
      Total Customer : {freq}")
      ) %>% 
      ggplot(aes(customer_state_n,(freq/sum(freq))*100, text=popup)) +
      geom_bar(aes(fill=customer_state_n), stat = "identity", show.legend = FALSE)+
      geom_text(aes(label=paste0(round((freq/sum(freq))*100,2),"%"), 
                    y=(freq/sum(freq))*100+2), hjust = 0.2, size=2.5, color="black") +
      labs(
        title = "Total Customer by State",
        x="",
        y="Total Customer (%)",
        fill="Year"
      )+
      coord_flip()
    
    
    ggplotly(plot_total_customer, tooltip="text") %>%
      layout(showlegend=FALSE) %>% 
      layout(title = list(text = paste0('Total Customer by State',
                                        '<br>',
                                        '<sup>',
                                        'Olist E-Commerce dari 2016-09-04 - 2018-10-17',
                                        '</sup>')))
    
  })
  
  # ------ PLOT8 (customer state) ------
  output$plot8 <- renderPlotly({
  plot_best_state <-  database %>% 
    filter(customer_state_name %in% input$select_city) %>% 
    group_by(customer_state_name) %>% 
    summarise(
      total = sum(price+freight_value)
    ) %>% 
    ungroup() %>% 
    mutate(
      customer_state_n = reorder(customer_state_name,total),
      popup = glue("State: {customer_state_name}
      Total Monetary : R$ {label_number_si(accuracy=0.1)(round(total,2))}")
    ) %>% 
    ggplot(aes(customer_state_n,(total/sum(total))*100, text=popup)) +
    geom_bar(aes(fill=customer_state_n), stat = "identity", show.legend = FALSE)+
    geom_text(aes(label=paste0(round((total/sum(total))*100,1),"%"), 
                  y=(total/sum(total))*100+1.5), vjust = -0.5, size=2.5, color="black") +
    labs(
      title = "Total Order Amount by State in Brazil",
      x="",
      y="Total Order Amount (%)",
      fill="Year"
    )+
    coord_flip()
  
  
  ggplotly(plot_best_state, tooltip="text") %>%
    layout(showlegend=FALSE) %>% 
    layout(title = list(text = paste0('Total Order Amount by State',
                                      '<br>',
                                      '<sup>',
                                      'Olist E-Commerce dari 2016-09-04 - 2018-10-17',
                                      '</sup>')))
  })
  
  
  # ------ Map ------
  output$map <- renderLeaflet({
  # Zipping locations
  lats <- database  %>% 
    filter(customer_state_name %in% input$select_city) %>% 
    filter(!is.na(geolocation_lat) & !is.na(geolocation_lng)) %>% 
    filter(!(geolocation_lat > 20 )) %>% 
    select(geolocation_lat)
  longs <- database  %>% 
    filter(customer_state_name %in% input$select_city) %>% 
    filter(!is.na(geolocation_lat) & !is.na(geolocation_lng)) %>% 
    filter(!(geolocation_lat > 20 )) %>% 
    select(geolocation_lng)
  locations <- cbind(lats, longs)
  
  # Creating a map using leaflet
  leaflet(data = locations) %>% 
    addTiles() %>% 
    addMarkers(~geolocation_lng, ~geolocation_lat, clusterOptions = markerClusterOptions())
  })
  
  # ------ cluster ------
  output$cluster <- renderPlotly({
  # cust_kmeans <- kmeans(lrfm_scale %>% select(frequency, monetary), centers = 3)
  # fviz_cluster(cust_kmeans, lrfm_scale %>% select(frequency, monetary), palette = "Set2", geom = "point")+
  #   labs(x = "Frequency", y = "Monetary")+
  #   theme_minimal()
  
    # Mengubah data menjadi format yang sesuai
    cluster_lrfm_gathered <- cluster_lrfm %>%
      filter(fm_segment %in% input$select_cluster) %>% 
      gather("features", "values",  input$select_lrfm) %>% 
      mutate(
        popup = glue("Jumlah Customer: unique{customer}"))
    
    # Menentukan palet warna untuk setiap fm_segment
    segment_colors <- brewer.pal(length(unique(cluster_lrfm$fm_segment)), "Accent")
    
    # Membuat data frame untuk mark tulisan
    customer_count <- cluster_lrfm_gathered %>%
      group_by(fm_segment) %>%
      summarise(total_customers = unique(customer)) %>% 
      mutate(label = paste("Jumlah Customers:", total_customers))
    
    # Menghitung nilai mean dari kolom L, R, F, M
    mean_L <- mean(cluster_lrfm$L)
    mean_R <- mean(cluster_lrfm$R)
    mean_F <- mean(cluster_lrfm$F)
    mean_M <- mean(cluster_lrfm$M)
    
    # Membuat data frame untuk mean
    mean_data <- data.frame(
      features = c(input$select_lrfm, input$select_lrfm, input$select_lrfm, input$select_lrfm),
      values = c(mean_L, mean_R, mean_F, mean_M)
    )
    
    # Membuat plot untuk setiap fitur berdasarkan fm_segment
    plot_cluster <- ggplot(cluster_lrfm_gathered, aes(x = values, y = reorder(features, values), fill = fm_segment)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(values, 1)), size = 2, color = "black", hjust = -2) +
      stat_summary(
        fun = "mean", 
        geom = "point", 
        shape = 18, 
        size = 1, 
        fill = "red", 
        color = "red", 
        alpha = 0.5,  # Tingkat transparansi
        data = mean_data) + # Menambahkan titik mean
      labs(
        title = "K-Means Clustering for 4 Segment",
        x = "Features",
        y = "Feature Value"
      ) +
      # Menambahkan mark tulisan di dalam plot
      geom_label(
        data = customer_count,
        aes(x = Inf, y = -Inf, label = label),
        hjust = 1.1, vjust = -0.3,
        fill = "white", color = "black",
        size = 3, label.padding = unit(0.4, "lines"),
        label.r = unit(0.2, "lines"), label.size = 0.25,
        show.legend = FALSE
      ) +
      scale_fill_manual(values = segment_colors) +
      theme(
        text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.position = "none",
        axis.text.x = element_text(hjust = 1),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 10)
      ) +
      facet_wrap(~ fm_segment, nrow = 2, ncol = 2) 
    
    
    # Menampilkan plot dengan 2x2 layout
    plot_cluster

  })


  # ------ lrfm ------
  output$lrfm <- renderPlotly({

    cluster_lrfm %>%
      gather("features","values",input$select_lrfm) %>%
      mutate(
        popup = glue("Jumlah Customer: {customer}")
      ) %>% 
      ggplot(aes(x = fm_segment, y = features)) +
      #scale_x_continuous(breaks = seq(min(cluster), max(cluster), by = 1)) +
      geom_tile(aes(fill = values), show.legend = FALSE) +
      geom_text(aes(label=round(values,1)), size=2, color="black")+
      coord_equal() +
      labs(
        title = "K-Means Clustering for 4 Segment",
        x = "Cluster"
      )+
      scale_fill_gradient(low="skyblue" ,na.value = "#C0C0C0", high="blue")+
      theme(text = element_text(size=11),
            plot.title =element_text(size=12),
            legend.position = "bottom",
            axis.text.y = element_text(size=9),
            axis.title.x = element_text(size=9),
            axis.text.x = element_text(angle = 45, hjust = 1)) -> plot_cluster


    grid.arrange(plot_cluster, nrow = 1)
    ggplotly(plot_cluster, tooltip="text")

  })
  
  
  # ------ VALUEBOX LRFM ---------

  # ------ VALUEBOX (Amount) ------
  output$amount_lrfm <- renderValueBox({

    # Menghitung nilai amount
    amount <- cluster_lrfm_full %>% 
      filter(fm_segment %in% input$select_cluster) %>% 
      summarise(sum(monetary))
    
    amount_value <- as.numeric(amount$`sum(monetary)`)
    
    total_amount_value_all <- cluster_lrfm_full %>% 
      summarise(sum(monetary))
    
    total_amount_value_all <- as.numeric(total_amount_value_all)
    
    persentase <- scales::percent(amount_value / total_amount_value_all)

    valueBox(
      subtitle = tags$div(
        style = "text-align: left;",
        "Total Amount",
        tags$span(
          style = "font-size: 12px; display: block;",
          paste0(persentase, " dari total Amount")
        )
      ),
      value = label_number_si(accuracy = 0.1, prefix = "R$ ")(amount_value),
      icon = icon("money-bill"),
      width = 3,
      color = "aqua"
    )
  })

  # ------ VALUEBOX (total_transaction ) ------
  output$transaction_lrfm <- renderValueBox({

    # Menghitung nilai total_transaction
    total_transaction <- cluster_lrfm_full %>% 
      filter(fm_segment %in% input$select_cluster) %>% 
      summarise(sum(frequency))
    
    total_transaction_all <- cluster_lrfm_full %>% 
      summarise(sum(frequency))
    
    total_transaction <- as.numeric(total_transaction)
    total_transaction_all <- as.numeric(total_transaction_all)
    
    persentase <- scales::percent(total_transaction / total_transaction_all)
    valueBox(
      value = format(total_transaction, big.mark = "."),
      subtitle = tags$div(
        style = "text-align: left;",
        "Total Transaksi",
        tags$span(
          style = "font-size: 12px; display: block;",
          paste0(persentase, " dari total transaksi")
        )
      ),
      width = 3,
      icon = icon("cart-shopping"),
      color = "aqua"
    )
  })

  # ------ VALUEBOX (total_customer) ------
  output$customer_lrfm <- renderValueBox({

    # Menghitung nilai total_customer
    total_customer <- cluster_lrfm_full %>% 
      filter(fm_segment %in% input$select_cluster) %>% 
      summarise(length(customer_unique_id_new))
    
    total_customer_all <- cluster_lrfm_full %>% 
      summarise(length(customer_unique_id_new))
    
    total_customer <- as.numeric(total_customer)
    total_customer_all <- as.numeric(total_customer_all)

    persentase <- scales::percent(total_customer / total_customer_all)
      valueBox(
        value = format(total_customer, big.mark = "."),
        subtitle = tags$div(
          style = "text-align: left;",
          "Total Customer",
          tags$span(
            style = "font-size: 12px; display: block;",
            paste0(persentase, " dari total pelanggan")
          )
        ),
        width = 3,
        icon = icon("users"),
        color = "aqua"
      )
    
    
  })
  
  # ------ VALUEBOX (REPEAT) ------ 
  
  output$rentang_lrfm <- renderValueBox({
    
    # Menghitung nilai avg transaction
    avg_transaction <- cluster_lrfm_full %>% 
      filter(fm_segment %in% input$select_cluster) %>% 
      summarise(mean(frequency))
    # Mengubah angka desimal menjadi rentang
    rentang <- paste(floor(avg_transaction), ceiling(avg_transaction), sep = "-")
    # Menampilkan Value Box dengan rentang sebagai nilai value
    
    min_f <- cluster_lrfm_full %>% 
      filter(fm_segment %in% input$select_cluster) %>% 
      summarise(min(frequency))
    
    max_f <- cluster_lrfm_full %>% 
      filter(fm_segment %in% input$select_cluster) %>% 
      summarise(max(frequency))
    
    valueBox(
      value = rentang,
      subtitle = tags$div(
        style = "text-align: left;",
        "Avg Transaction",
        tags$span(
          style = "font-size: 12px; display: block;",
          paste0("Min: ", min_f, ",  Max: ", max_f)
        )),
      icon = icon("repeat"),
      color = "aqua"
    )
  })
  
  
  
  # ------ PLOT10 (Payment Type LRFM) ------
  output$plot10 <- renderPlotly({
    # table count of payment_type
    
    # Menggunakan gsub untuk mengganti "_" menjadi " " di kolom "payment_type"
    data$payment_type <- gsub("_", " ", data$payment_type)
    
    status_count <- data %>% 
      filter((fm_segment %in% input$select_cluster)) %>%
      count(payment_type) %>% 
      mutate(pct = n/sum(n)) %>% 
      mutate(popup=glue("Tipe Pembayaran : {payment_type}
                    Frekuensi : {n}"))
    
    # plot the count table
    payment_type_lrfm <- ggplot(status_count, aes(n, reorder(payment_type, n))) +
      geom_bar(stat="identity", fill = "skyblue", aes(text=popup)) +
      labs(title= "Distribution of Order Status", x="Frekuensi", y="Tipe Pembayaran") +
      theme_minimal() +
      geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3.5, hjust = 1000) +
      scale_x_continuous(labels = scales::comma) +
      theme(plot.title = element_text(size = 13)) +
      theme(panel.background = element_rect(fill='white'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggplotly(payment_type_lrfm, tooltip = "text") %>% 
      layout(title = list(text = paste0('Profil ', input$select_cluster)))
    
  })
  
  # ------ PLOT11 (Category Type) ------
  output$plot11 <- renderPlotly({
    # table count of payment_type
    
    # Menggunakan gsub untuk mengganti "_" menjadi " " di kolom "product_category_name_english"
    data$product_category_name_english <- gsub("_", " ", data$product_category_name_english)
    
    status_count <- data %>% 
      filter((fm_segment %in% input$select_cluster)) %>%
      count(product_category_name_english) %>% 
      mutate(pct = n/sum(n)) %>% 
      mutate(popup=glue("Kategori Produk : {product_category_name_english}
                    Frekuensi : {n}")) %>% 
      arrange(-n) %>% 
      head(10)
    
    # plot the count table
    product_type_lrfm <- ggplot(status_count, aes(n, reorder(product_category_name_english, n))) +
      geom_bar(stat="identity", fill = "skyblue", aes(text=popup)) +
      labs(title= "Distribution of Product Category", x="Frekuensi", y="Kategori Produk") +
      theme_minimal() +
      geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3.5, hjust = 1000) +
      scale_x_continuous(labels = scales::comma) +
      theme(plot.title = element_text(size = 13)) +
      theme(panel.background = element_rect(fill='white'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggplotly(product_type_lrfm, tooltip = "text") %>% 
      layout(title = list(text = paste0('Profil ', input$select_cluster)))
    
  })
  
  
  # ------ PLOT12 (City) ------
  output$plot12 <- renderPlotly({
    # table count of payment_type
    status_count <- data %>% 
      filter((fm_segment %in% input$select_cluster)) %>%
      count(customer_state_name.x) %>% 
      mutate(pct = n/sum(n)) %>% 
      mutate(popup=glue("Kota : {customer_state_name.x}
                    Frekuensi : {n}")) %>% 
      arrange(-n) %>% 
      head(10)
    
    # plot the count table
    product_type_lrfm <- ggplot(status_count, aes(n, reorder(customer_state_name.x, n))) +
      geom_bar(stat="identity", fill = "skyblue", aes(text=popup)) +
      labs(title= "Distribution of Product Category", x="Frekuensi", y="Kategori Produk") +
      theme_minimal() +
      geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3.5, hjust = 1000) +
      scale_x_continuous(labels = scales::comma) +
      theme(plot.title = element_text(size = 13)) +
      theme(panel.background = element_rect(fill='white'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
    ggplotly(product_type_lrfm, tooltip = "text") %>% 
      layout(title = list(text = paste0('Profil ', input$select_cluster)))
    
  })

  
  
  # ------ Map ------
  output$map2 <- renderLeaflet({  
  colFac <- colorFactor( 
    palette = "Accent",
    domain = unique(unique(data$fm_segment)))
  
  labels <- sprintf(
    "ID: %s <br/>State Name: %s <br>Length: %g <br>Recency: %g <br>Frequency: %g <br/>Monetary: R$ %g",
    data$customer_unique_id_new, 
    data$customer_state_name.x,
    data$length,
    data$recency,
    data$frequency,
    data$monetary
  ) %>% lapply(htmltools::HTML)
  
  data %>% 
    filter(customer_state_name.x %in% c("São Paulo","Rio de Janeiro","Minas Gerais","Rio Grande do Sul","Paraná")) %>% 
    filter(fm_segment %in% input$select_cluster) %>% 
    leaflet() %>%
    setView(-54.69142,-23.95,5) %>%
    addTiles() %>%
    clearShapes() %>%
    addCircles( lng =  ~ geolocation_lng,
                lat =  ~ geolocation_lat,
                label = labels,
                popup = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"),
                #weight = 3,
                #radius=40,
                color= ~colFac(fm_segment), stroke = TRUE, fillOpacity = 0.5) %>%
    addLegend("bottomright", pal = colFac, values = ~fm_segment,
              title = "Cluster",
              opacity = 1
    )
  
  })
  
  
  output$customer_s <- renderDataTable({
    data1 <- data %>% 
      select(input$select_kolom) %>% 
      filter((fm_segment %in% input$select_segment))
    datatable(data1,
              options = list(scrollX= TRUE,
                        scrollY= '600px',
                        scrollCollapse= TRUE,
                        paging= TRUE,
                        border = TRUE))
    
  })
  
  
  output$info_paragraph <- renderText({
    "Dataset Brazilian E-Commerce Public Dataset by Olist adalah kumpulan data yang berisi informasi tentang transaksi e-commerce di Brasil oleh Olist, department store terbesar di pasar Brasil dari tahun 2016 hingga 2018. Dataset ini mencakup informasi tentang lebih dari 100.000 pesanan yang dilakukan oleh pelanggan Olist, termasuk informasi tentang produk yang dibeli, harga, tanggal pembelian, metode pembayaran, dan lokasi pengiriman.

Selain itu, dataset ini juga berisi informasi tentang pelanggan Olist, termasuk nama, alamat email, nomor telepon, dan informasi demografis seperti jenis kelamin, usia, dan lokasi geografis.

Dataset ini sangat berguna untuk membangun model RFM (Recency, Frequency, Monetary) yang dapat membantu perusahaan e-commerce dalam strategi pemasaran mereka."
  })
  
  output$source_paragraph <- renderText({
    "Dataset bersumber dari kaggle, dengan link [kaggle.com](https://www.kaggle.com/datasets/olistbr/brazilian-ecommerce?datasetId=55151&sortBy=voteCount&searchQuery=rfm).

Ini adalah data komersial yang nyata, telah dianonimkan, dan referensi ke perusahaan dan mitra dalam teks ulasan telah diganti dengan nama-nama rumah besar Game of Thrones. Teknik pengumpulannya menggunakan random sampling."
  })
}