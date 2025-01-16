library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(knitr)
library(tidyverse)
library(DT)
library(lubridate)
library(padr)
library(plotly)
library(likert)
library(Hmisc)
library(corrplot)

ui <- shinyUI(
  dashboardPage(
    skin = "black-light",
    title = "UAS Kelompok 10",
    dashboardHeader(
      title = "R VS PYTHON",
      titleWidth = 300
    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "Home", icon = icon("home")),
        menuItem("Data", tabName = "Data", icon = icon("file")),
        menuItem("Pre Processing", tabName = "PreProcessing", icon = icon("cogs")),
        menuItem("Visualisasi", tabName = "Visualisasi", icon = icon("bar-chart")),
        menuItem("Analisis", tabName = "Analisis", icon = icon("chart-line"))
      )
    ),
    
    dashboardBody(
      tags$head(
        tags$style(HTML("
          .main-header .navbar .navbar-brand {
            display: block !important; 
            visibility: visible !important; 
          }
          .sidebar-collapse .main-header .navbar .navbar-brand {
            visibility: visible !important; 
          }
        "))
      ),
      
      tabItems(
        tabItem(
          tabName = "Home",
          tabsetPanel(
            tabPanel(
              "About",
              fluidRow(
                box(
                  title = "Judul Penelitian",
                  h1("Eksplorasi Preferensi Mahasiswa Dalam Bahasa Pemrograman R dan Python: Tinjauan Pada Penggunaan Dalam Praktikum, Tugas, dan Proyek Pribadi"),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12
                ),
                box(
                  title = "Latar Belakang",
                  p(HTML("Bahasa pemrograman adalah alat yang digunakan untuk mengelola data mengembangkan kemampuan analisis data, pemrograman, dan ilmu komputer. Dengan menggunakan bahasa pemrograman, mahasiswa dapat membuat aplikasi, website, game, atau sistem yang bermanfaat bagi diri sendiri maupun masyarakat. Namun, tidak semua mahasiswa memiliki minat atau kemampuan yang sama dalam mempelajari bahasa pemrograman. Oleh karena itu, perlu dilakukan penelitian untuk mengetahui preferensi mahasiswa dalam memilih bahasa pemrograman yang sesuai dengan kebutuhan dan tujuan mereka.<br/><br/>
Salah satu bahasa pemrograman yang populer digunakan oleh mahasiswa adalah R dan Python. Kedua bahasa ini memiliki kelebihan dan kekurangan masing-masing, serta digunakan untuk berbagai bidang keperluan. R adalah bahasa pemrograman open source yang khusus digunakan untuk analisis statistik dan data science. Python adalah bahasa pemrograman open source yang lebih universal dan mudah dipelajari. Kedua bahasa ini juga memiliki banyak library atau pustaka yang dapat membantu proses analisis data.<br/><br/>
Namun, tidak semua mahasiswa mengetahui atau mengenal kedua bahasa ini dengan baik. Beberapa faktor yang dapat mempengaruhi preferensi mahasiswa dalam memilih R atau Python antara lain adalah latar belakang pendidikan, minat akademik, sumber belajar, lingkungan sosial, dan motivasi belajar. Oleh karena itu, penelitian ini bertujuan untuk mengeksplorasi preferensi mahasiswa dalam menggunakan R atau Python dalam praktikum, tugas, dan proyek pribadi.")),
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12
                )
              )
            ),
            tabPanel(
              "Profile",
              fluidRow(
                box(
                  title = "Anggota Kelompok:",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 4,
                  tableOutput("groupMembers")
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "Data",
          fluidRow(
            box(
              title = "Dataset Primer Melalui Kuesioner",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              DT::dataTableOutput("tabel")
            )
          )
        ), 
        tabItem(
          tabName = "Visualisasi",
          tabsetPanel(
            tabPanel(
              "Distro",
              fluidRow(
                selectInput("var_selected", "Pilih Variabel", 
                            choices = colnames(data)),
                plotlyOutput("histplot")
              )
            ),
            
            tabPanel(
              "Stepped line Graph",
              fluidRow(
                box(
                  title = "Penguasaan R",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("ecdFP10lot1")
                ),
                box(
                  title = "Penguasaan Python",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("ecdFP10lot2")
                )
              )
            ),  
            
            tabPanel(
              "Likert Chart",
              fluidRow(
                box(
                  title = "Bagaimana Penguasaan dan Kenyamanan dalam Bahasa Pemrograman Python dan R?",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("likertChart1")
                ),
                box(
                  title = "Bagaimana Frekuensi Penggunaan dan Kesulitan dalam Bahasa Pemrograman Python dan R?",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("likertChart2")
                ),
                box(
                  title = "Penguasaan dan Kenyamanan Ditinjau dari Pilihan Bahasa Pemrograman Favorit",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotOutput("likertChart3")
                ),
                box(
                  title = "Frekuensi Penggunaan dan Kesulitan Ditinjau dari Jenis Kelamin",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotOutput("likertChart4")
                )
              )
            ),  
            
            tabPanel(
              "Box Plot",
              fluidRow(
                box(
                  title = "Boxplot Variabel Selain Ordinal",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("boxPlot1")
                ),
                box(
                  title = "Boxplot Variabel Selain Ordinal Ditinjau Berdasarkan Gender",
                  status = "primary",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  width = 6,
                  plotOutput("boxPlot2")
                )
              )
            ), 
            
            tabPanel(
              "Scatter plot",
              div(
                fluidPage(
                  h2(tags$b(textOutput("captionscat")), align="center"),
                  box(plotOutput("scatter")),
                  box(
                    selectInput(
                      "variabel3", "variabel x :",
                      c(
                        "R atau Python" = "RorPython"
                      )
                    ),
                    selectInput(
                      "variabel4", "variabel y :",
                      c(
                        "Library Python" = "library_Python",
                        "Library R" = "library_R",
                        "Praktikum Python" = "praktikum_Python",
                        "Praktikum R" = "praktikum_R",
                        "Proyek menggunakan Python" = "proyek_Python",
                        "Proyek menggunakan R" ="proyek_R"
                      )
                    )
                  )
                )
              )
            ),  
            
            tabPanel(
              "Stacked Bar Chart", 
              value = "stackedBarChart",
              fluidRow(
                div(
                  class = "mx-auto",
                  box(
                    title = "",
                    status = "primary",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    width = 12,
                    fluidPage(
                      div(
                        class = "row",
                        div(class = "col-sm-6",
                            selectInput(
                              "variable5", "Variable-1:",
                              c("R atau Python" = "RorPython",
                                "Jenis Kelamin" = "jenis_kelamin")
                            ),
                            selectInput(
                              "variable6", "Variable-2:",
                              c(
                                "Platform Favorit" = "platform_favorite",
                                "Praktikum Python" = "praktikum_Python",
                                "Praktikum R" = "praktikum_R",
                                "Jenis Kelamin" = "jenis_kelamin",
                                "R atau Python" = "RorPython"
                              )
                            )
                        ),
                        div(class = "col-sm-6", plotOutput("stackedBarChart"))
                      )
                    )
                  )
                )
              )
            )  
          )  
        ),  
        
        
        
        tabItem(
          tabName = "PreProcessing",
          h2("Cek Missing Value"),
          fluidRow(
            box(
              title = "Histogram Proporsi Missing Value Tiap Variabel",
              status = "primary",  
              solidHeader = TRUE,  
              collapsible = TRUE,  
              width = 6,
              plotOutput("missingvalue")
            ),
            box(
              title = "Frekuensi Missing Value Tiap Variabel",
              status = "primary", 
              solidHeader = TRUE,
              collapsible = TRUE,  
              width = 6,
              DT::dataTableOutput("missingValuesTable")
            )
          ),
          h2("Cek Outlier"),
          fluidRow(
            box(
              title = "Boxplot Cek Outlier Variabel Numerik",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,  # Adjusted the width to 12
              plotOutput("Outlier")
            )
          )
        ),
        
        tabItem(
          tabName = "Analisis",
          fluidRow(
            box(
              title = "Analisis Statistika Deskriptif",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,  
              column(
                4,
                selectInput("varSelect", "Pilih Variabel:", 
                            choices = c("praktikum_R", "proyek_R", "library_R", "praktikum_Python", "proyek_Python", "library_Python"))
              ),
              column(
                8,
                verbatimTextOutput("descStats")
              )
            )
          ),
          fluidRow(
            box(
              title = "Korelasi dengan Tabel kontingensi",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,
              sidebarLayout(
                sidebarPanel(
                  selectInput(
                    "var7", "Variable-1:",
                    c(
                      "Platform Favorit" = "platform_favorite",
                      "Jenis Kelamin" = "jenis_kelamin",
                      "R atau Python" = "RorPython",
                      "Pentingnya Pemrograman" = "pentingnya_pemrograman",
                      "Frekuensi R" = "frekuensi_R",
                      "Kenyamanan R" = "kenyamanan_R",
                      "Kesulitan R" = "kesulitan_R",
                      "Penguasaan R" = "penguasaan_R",
                      "Visualisasi R" = "visualisasi_R",
                      "Frekuensi Python" = "frekuensi_Python",
                      "Kenyamanan Python" = "kenyamanan_Python",
                      "Kesulitan Python" = "kesulitan_Python",
                      "Penguasaan Python" = "penguasaan_Python",
                      "Visualisasi Python" = "visualisasi_Python")
                  ),
                  selectInput(
                    "var8", "Variable-2:",
                    c(
                      "Platform Favorit" = "platform_favorite",
                      "Jenis Kelamin" = "jenis_kelamin",
                      "R atau Python" = "RorPython",
                      "Pentingnya Pemrograman" = "pentingnya_pemrograman",
                      "Frekuensi R" = "frekuensi_R",
                      "Kenyamanan R" = "kenyamanan_R",
                      "Kesulitan R" = "kesulitan_R",
                      "Penguasaan R" = "penguasaan_R",
                      "Visualisasi R" = "visualisasi_R",
                      "Frekuensi Python" = "frekuensi_Python",
                      "Kenyamanan Python" = "kenyamanan_Python",
                      "Kesulitan Python" = "kesulitan_Python",
                      "Penguasaan Python" = "penguasaan_Python",
                      "Visualisasi Python" = "visualisasi_Python")
                  )
                ),
                mainPanel(
                  verbatimTextOutput("contingency_table"),
                  verbatimTextOutput("chi_square_test")
                )
              )
            )
          ),
          fluidRow(
            box(
              title = "Uji Reliabilitas",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 12,  # Adjust the width as needed
              sidebarLayout(
                sidebarPanel(
                  selectInput("Bahasa", "Pilih Bahasa Pemrograman", choices = c("R", "Python"))
                ),
                mainPanel(
                  DTOutput("reliability")
                )
              )
            ),
            fluidRow(
              box(
                title = "Uji Validitas",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,  # Adjust the width as needed
                sidebarLayout(
                  sidebarPanel(
                    selectInput("Bahasa1", "Pilih Bahasa Pemrograman", choices = c("R", "Python"))
                  ),
                  mainPanel(
                    DTOutput("validity")
                  )
                )
              )
            ),
            fluidRow(
              box(
                title = "Heatmap Korelasi Spearman",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                plotOutput("correlationPlot")
              )
            ),
            fluidRow(
              box(
                title = "Man Whitney",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                verbatimTextOutput("mannWhitneyTest")
              )
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$tabel <- DT::renderDataTable(
    tabel <- read.csv("data_bersih.csv"),
    extensions = "Buttons", 
    option = list(dom = "Bfrtip", buttons = list('copy','csv','excel'))
  )
  
  output$groupMembers <- renderTable({
    data.frame(
      Nama = c("Brahmantya Fikri S.P.", "Zoen Yokhanan S.", "Nafisahika Putri H.", "Fradinka Amelia E.", "Verdyan Farrel B."),
      NIM = c("164221034", "164221038", "164221039", "164221045", "164221076")
    )
  }, rownames = TRUE)
  
  output$missingvalue <- renderPlot({
    library("VIM")
    df <- read.csv("data_bersih.csv")
    # Mengganti nilai "kurang tau, 3 sepertinya" dengan 3
    df$praktikum_R <- ifelse(df$praktikum_R == "kurang tau, 3 sepertinya", 3, df$praktikum_R)
    # Mengganti nilai "m10,m11" dengan 2
    df$praktikum_R  <- ifelse(df$praktikum_R == "m10,m11", 2, df$praktikum_R )
    # Mengganti nilai "3 sepertinya" dengan 3
    df$proyek_R <- ifelse(df$proyek_R  == "3 sepertinya", 3, df$proyek_R )
    # Mengubah input non-numerik pada kolom numerik menjadi NAN
    kolom_untuk_dibersihkan <- c("pentingnya_pemrograman", "frekuensi_R", "kenyamanan_R", "kesulitan_R", "penguasaan_R", "visualisasi_R", "praktikum_R", "proyek_R", "library_R", "frekuensi_Python", "kenyamanan_Python", "kesulitan_Python", "penguasaan_Python", "visualisasi_Python", "praktikum_Python", "proyek_Python", "library_Python")
    for (kolom in kolom_untuk_dibersihkan) {
      if (kolom %in% names(df)) {
        df[[kolom]] <- as.numeric(gsub("[^0-9\\.]", "", df[[kolom]]))
      }
    }
    aggr_plot <- aggr(df, col=c('skyblue','red'), numbers=TRUE, 
                      sortVars=TRUE, labels=names(df), cex.axis=.5, gap=1, ylab=c("","Pattern"))
    print(aggr_plot)
  })
  
  preprocess_data <- reactive({
    read.csv("data_kotor.csv")
    # You can replace the above line with the path to your new dataset
  })
  
  output$tabel <- DT::renderDataTable({
    preprocess_data()
  })
  
  output$missingvalue <- renderPlot({
    library("VIM")
    df <- preprocess_data()
    # The rest of your missing value handling code
    aggr_plot <- aggr(df, col = c('skyblue', 'red'), numbers = TRUE, 
                      sortVars = TRUE, labels = names(df), cex.axis = 0.5, gap = 1, ylab = c("", "Pattern"))
    print(aggr_plot)
  })
  output$missingValuesTable <- DT::renderDataTable({
    df <- read.csv("data_bersih.csv")
    missing_values_all <- sapply(df, function(x) sum(is.na(x)))
    missing_values_df <- data.frame(
      nama_variabel = names(missing_values_all),
      jumlah_missing_value = as.numeric(missing_values_all)
    )
  })
  
  
  
  output$groupMembers <- renderTable({
    data.frame(
      Nama = c("Brahmantya Fikri S.P.", "Zoen Yokhanan S.", "Nafisahika Putri H.", "Fradinka Amelia E.", "Verdyan Farrel B."),
      NIM = c("164221034", "164221038", "164221039", "164221045", "164221076")
    )
  }, rownames = TRUE)
  
  
  output$Outlier <- renderPlot({
    library(ggplot2)
    library(gridExtra)
    df <- read.csv("data_bersih.csv")
    kolom_boxplot <- subset(df, select = c("praktikum_R", "proyek_R", "library_R", "praktikum_Python", "proyek_Python", "library_Python"))
    df$praktikum_R <- ifelse(df$praktikum_R == "kurang tau, 3 sepertinya", 3, df$praktikum_R)
    df$praktikum_R  <- ifelse(df$praktikum_R == "m10,m11", 2, df$praktikum_R )
    df$proyek_R <- ifelse(df$proyek_R  == "3 sepertinya", 3, df$proyek_R )
    kolom_untuk_dibersihkan <- c("pentingnya_pemrograman", "frekuensi_R", "kenyamanan_R", "kesulitan_R", "penguasaan_R", "visualisasi_R", "praktikum_R", "proyek_R", "library_R", "frekuensi_Python", "kenyamanan_Python", "kesulitan_Python", "penguasaan_Python", "visualisasi_Python", "praktikum_Python", "proyek_Python", "library_Python")
    for (kolom in kolom_untuk_dibersihkan) {
      if (kolom %in% names(df)) {
        df[[kolom]] <- as.numeric(gsub("[^0-9\\.]", "", df[[kolom]]))
      }
    }
    
    plot_list <- lapply(names(kolom_boxplot), function(x_var) {
      boxplot <- ggplot(df, aes_string(y = x_var)) +
        geom_boxplot() +
        labs(title = paste("Box Plot", x_var), x="",y="" )
      boxplot <- boxplot + theme(
        plot.title = element_text(size = 6.5)
      )
      return(boxplot)
    })
    
    grid.arrange(grobs = plot_list, ncol = 3)
  })
  
  observe({
    data <- read.csv("data_bersih.csv")
    var_choices <- names(data)[sapply(data, is.numeric)] # Filter variabel numerik saja
    updateSelectInput(session, "varSelect", choices = var_choices, selected = var_choices[1])
  })
  
  output$descStats <- renderPrint({
    req(input$varSelect)
    data <- read.csv("data_bersih.csv")
    desc_stats <- data %>% 
      summarise(
        Mean = mean(.data[[input$varSelect]], na.rm = TRUE),
        SD = sd(.data[[input$varSelect]], na.rm = TRUE),
        Min = min(.data[[input$varSelect]], na.rm = TRUE),
        Median = median(.data[[input$varSelect]], na.rm = TRUE),
        Max = max(.data[[input$varSelect]], na.rm = TRUE)
      )
    print(desc_stats)
  })
  
  visduadata <- data[, c("RorPython", "jenis_kelamin", "kenyamanan_R", "penguasaan_R", "kenyamanan_Python", "penguasaan_Python")]
  
  # Format Columns
  colnames(visduadata)[3:ncol(visduadata)] <- paste0("Q", str_pad(1:4, 2, "left", "0"), ": ", colnames(visduadata)[3:ncol(visduadata)], "?") %>%
    stringr::str_replace_all("\\.", " ") %>%
    stringr::str_squish()
  
  # Encode
  lbs <- c("Tidak", "Kurang", "Ya", "Sangat")
  vis2 <- visduadata %>%
    dplyr::mutate_if(is.character, factor) %>%
    dplyr::mutate_if(is.numeric, factor, levels = 1:4, labels = lbs) %>%
    drop_na() %>%
    as.data.frame()
  
  output$likertChart1 <- renderPlot({
    likert_data_vis2 <- likert(vis2[, 3:6])
    plot(likert_data_vis2, ordered = FALSE, wrap = 10)
  })
  
  # Likert Chart 3
  output$likertChart3 <- renderPlot({
    likert_data_vis2 <- likert(vis2[, 3:6], grouping = vis2[,1])
    plot(likert_data_vis2, ordered = FALSE, wrap = 10)
  })
  
  # Encode
  lbs <- c("Tidak Pernah", "Jarang", "Sering",  "Sangat Sering")
  vis21 <- vis21data %>%
    dplyr::mutate_if(is.character, factor) %>%
    dplyr::mutate_if(is.numeric, factor, levels = 1:4, labels = lbs) %>%
    drop_na() %>%
    as.data.frame()
  
  # LIkert Chart 2
  output$likertChart2 <- renderPlot({
    likert_data_vis21 <- likert(vis21[, 2:5])
    plot(likert_data_vis21, ordered = FALSE, wrap = 50)
  })
  
  # Likert Chart 4
  output$likertChart4 <- renderPlot({
    likert_data_vis21 <- likert(vis21[, 2:5], grouping = vis21[,1])
    plot(likert_data_vis21, ordered = FALSE, wrap = 10)
  })
  
  output$boxPlot1 <- renderPlot({
    visbox <- c("praktikum_Python", "proyek_Python", "library_Python", "praktikum_R", "proyek_R", "library_R")
    
    boxplot <- data %>%
      gather(key = "activity", value = "score", visbox) %>%
      ggplot(aes(x = activity, y = score)) +
      geom_boxplot() +
      facet_wrap(~ activity, scales = "free_x", ncol = 6) +
      labs(x = "Activity", y = "Score") +
      theme_minimal()
    
    print(boxplot)
  })
  
  output$boxPlot2 <- renderPlot({
    visbox <- c("praktikum_Python", "proyek_Python", "library_Python", "praktikum_R", "proyek_R", "library_R")
    
    boxplot <- data %>%
      gather(key = "activity", value = "score", visbox) %>%
      ggplot(aes(x = activity, y = score, fill = jenis_kelamin)) +
      geom_boxplot() +
      facet_wrap(~ activity, scales = "free_x", ncol = 6) +
      labs(x = "Activity", y = "Score") +
      theme_minimal()
    
    print(boxplot)
  })
  
  
  
  output$scatter <- renderPlot({
    formulaText <- reactive({
      paste("Scatterplot ", input$variabel3, "- ", input$variabel4)
    })
    
    output$captionscat <- renderText({
      formulaText()
    })
    
    datascat1 <- eval(parse(text = paste("data$", input$variabel3)))
    datascat2 <- eval(parse(text = paste("data$", input$variabel4)))
    
    # Menggunakan ggplot2 untuk scatter plot yang lebih fleksibel
    scatter_plot <- ggplot(data = data, aes(x = datascat1, y = datascat2, color = jenis_kelamin)) +
      geom_point() +
      labs(title = formulaText(), x = input$variabel3, y = input$variabel4, color = "Jenis Kelamin") +
      theme_minimal()
    
    print(scatter_plot)
  })
  
  
  
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    install.packages("RColorBrewer")
  }
  library(RColorBrewer)
  
  output$stackedBarChart <- renderPlot({
    viscateg <- data[, c(input$variable5, input$variable6)]
    
    unique_levels <- unique(viscateg[[input$variable6]])
    
    
    if (is.null(input$colorstack) || length(input$colorstack) == 0) {
      colors <- setNames(brewer.pal(length(unique_levels), "Set1"), unique_levels)
    } else {
      colors <- setNames(input$colorstack[1:length(unique_levels)], unique_levels)
    }
    
    stacked_bar_chart <- ggplot(viscateg, aes(x = .data[[input$variable5]], fill = .data[[input$variable6]])) +
      geom_bar() +
      scale_fill_manual(values = colors) +
      labs(x = input$variable5, y = "Count") +
      theme_minimal()
    
    print(stacked_bar_chart)
  })
  
  output$ecdFP1010lot1 <- renderPlot({
    vis1data <- data[, c("penguasaan_Python", "jenis_kelamin", "penguasaan_R")]
    
    # Color
    clrs3 <- c("firebrick4", "gray70", "darkblue")
    clrs5 <- c("firebrick4", "firebrick1", "gray70", "blue", "darkblue")
    
    library(ggplot2)
    
    plot1 <- vis1data %>%
      ggplot(aes(x = penguasaan_R, color = jenis_kelamin)) +
      geom_step(aes(y = ..y..), stat = "ecdf", direction = "vh") +
      labs(y = "Cumulative Density", x = "Penguasaan R") +
      scale_color_manual(values = clrs5) +
      theme_bw() +
      scale_x_discrete(limits = c("1","2","3","4"), 
                       breaks = c(1,2,3,4),
                       labels=c("Tidak Menguasai", "Kurang Menguasai", 
                                "Menguasai", "Sangat Menguasai"))
    print(plot1)
  })
  
  output$ecdFP10lot2 <- renderPlot({
    vis1data <- data[, c("penguasaan_Python", "jenis_kelamin", "penguasaan_R")]
    
    # Color
    clrs3 <- c("firebrick4", "gray70", "darkblue")
    clrs5 <- c("firebrick4", "firebrick1", "gray70", "blue", "darkblue")
    
    library(ggplot2)
    
    plot2 <- vis1data %>%
      ggplot(aes(x = penguasaan_Python, color = jenis_kelamin)) +
      geom_step(aes(y = ..y..), stat = "ecdf", direction = "hv") +
      labs(y = "Cumulative Density", x = "Penguasaan Python") +
      scale_color_manual(values = clrs5) +
      theme_bw() +
      scale_x_discrete(limits = c("1","2","3","4"), 
                       breaks = c(1,2,3,4),
                       labels=c("Tidak Menguasai", "Kurang Menguasai", 
                                "Menguasai", "Sangat Menguasai"))
    print(plot2)
  })
  
  
  observe({
    updateSelectInput(session, "var1", choices = c(
      "Jenis Kelamin" = "jenis_kelamin",
      "Penguasaan Python" = "penguasaan_Python",
      names(data())
    ))
  })
  
  data <- read.csv("data_bersih.csv")
  
  # Define callback for histogram plot
  output$histplot <- renderPlotly({
    # Assuming 'data' is a reactive or global variable
    selected_var <- input$var_selected
    
    if (is.null(selected_var)) {
      return(NULL)
    }
    
    x_data <- data[[selected_var]]
    
    if (is.null(x_data) || !is.numeric(x_data)) {
      return(NULL)
    }
    
    # Plot histogram
    p <- plot_ly(x = ~x_data, type = "histogram") %>% 
      layout(xaxis = list(title = selected_var),
             yaxis = list(title = "Frequency"))
    
    return(p)
  })
  
  
  
  observe({
    # Check if data is available
    if (exists("data")) {
      
      # Create a contingency table
      create_contingency_table <- function(data, var1, var2) {
        contingency_table <- table(data[[var1]], data[[var2]])
        chi_square_test <- chisq.test(contingency_table)
        
        result <- list(
          contingency_table = contingency_table,
          chi_square_test = chi_square_test
        )
        
        return(result)
      }
      
      # Get selected variables
      var1 <- input$var7
      var2 <- input$var8
      
      # Create the contingency table and chi-square test
      result <- create_contingency_table(data, var1, var2)
      
      # Print the contingency table in the UI
      output$contingency_table <- renderPrint({
        print(result$contingency_table)
      })
      
      # Print the chi-square test result in the UI
      output$chi_square_test <- renderPrint({
        cat("\nChi-square Test Result:\n")
        print(result$chi_square_test)
      })
    }
  })
  
  observe({
    tryCatch({
      df <- read.csv("data_bersih.csv")
      Bahasa <- input$Bahasa
      Bahasa1 <- input$Bahasa1
      
      # Filter data berdasarkan pilihan pengguna
      if (Bahasa == "R") {
        df_selected <- df[, c(5, 6, 7, 8, 10)]
      } else if (Bahasa == "Python") {
        df_selected <- df[, c(14, 15, 16, 17, 19)]
      }
      
      # Include column 11 in df_selected1
      if (Bahasa1 == "R") {
        df_selected1 <- df[, c(5, 6, 7, 8, 9, 10, 11)]
      } else if (Bahasa1 == "Python") {
        df_selected1 <- df[, c(14, 15, 16, 17, 18, 19, 20)]
      }
      
      reliability_result <- reactive({
        reliability <- psych::alpha(df_selected)
        reliability_data <- as.data.frame(reliability$total)
        reliability_data
      })
      
      validity_result <- reactive({
        validity <- Hmisc::rcorr(as.matrix(df_selected1), type = "pearson")
        validity_data <- as.data.frame(validity$r)
        validity_data
      })
      
      output$reliability <- renderDT({
        reliability_data <- reliability_result()
        DT::datatable(
          reliability_data,
          options = list(
            paging = FALSE, 
            fixedHeader = TRUE, 
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1)
          ),
          caption = "Hasil Uji Relalibilitas"
        )
      })
      
      output$validity <- renderDT({
        validity_data <- validity_result()
        DT::datatable(
          validity_data,
          options = list(
            paging = FALSE, 
            fixedHeader = TRUE, 
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 1)
          ),
          caption = "Hasil Uji Validitas"
        )
      })
    }, error = function(e) {
      # Print the error message
      print(paste("Error:", e))
    })
  })
  
  
  # Directly read the data from the given path
  df <- read.csv("data_bersih.csv")
  
  # Mann-Whitney U Test
  output$mannWhitneyTest <- renderPrint({
    # Ensure the columns exist
    req(colnames(df) %in% c("TOTAL_R", "TOTAL_Python"))
    
    # Convert to numeric and omit NA
    df$TOTAL_R <- as.numeric(df$TOTAL_R)
    df$TOTAL_Python <- as.numeric(df$TOTAL_Python)
    data_R <- na.omit(df$TOTAL_R)
    data_Python <- na.omit(df$TOTAL_Python)
    
    # Perform the test
    result <- wilcox.test(data_R, data_Python)
    print(result)
    
    # Interpret the result
    alpha <- 0.05
    if (result$p.value < alpha) {
      cat("Terdapat perbedaan signifikan antara mahasiswa terhadap kenyamanan, kesulitan, penguasaan, maupun frekuensi penggunaan bahasa pemrograman R atau Python.")
    } else {
      cat("Tidak terdapat perbedaan signifikan antara mahasiswa terhadap kenyamanan, kesulitan, penguasaan, maupun frekuensi penggunaan bahasa pemrograman R atau Python.")
    }
  })
  
  # Spearman Correlation Heatmap
  output$correlationPlot <- renderPlot({
    # Ensure the selected variables exist
    selected_variables <- c("praktikum_R", "proyek_R", "library_R",
                            "praktikum_Python", "proyek_Python", "library_Python")
    req(all(selected_variables %in% colnames(df)))
    
    selected_data <- df[, selected_variables, drop = FALSE]  # Use drop = FALSE to keep it as a data frame
    numeric_data <- sapply(selected_data, as.numeric)
    
    cor_matrix <- cor(numeric_data, method = "spearman")
    corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.7)
  })
  
}
shinyApp(ui,server)