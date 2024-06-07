library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(lubridate)
library(padr)
library(RColorBrewer)
library(wesanderson)
library(readxl)
library(ggplot2)
library(ggridges)

#Set Directory
Data <- read_xlsx("proyek shiny.xlsx")

#Subset Data 
Screen <- Data$`Screen Time`  
ShopeePay <- Data$ShopeePay
GoPay <- Data$GoPay
Ovo <- Data$Ovo
Dana <- Data$Dana
Lainnya <- Data$Lainnya
eMoney <- Data$eMoney
Pengeluaran <- Data$Pengeluaran
Makan <- Data$Makan
Transportasi <- Data$Transportasi
Wajib <- Data$`Belanja (kebutuhan wajib)`
Tersier <- Data$`Gaya hidup (kebutuhan tersier)`
Pemasukan <- Data$Pemasukan
Hangout <- Data$`Frekuensi Hangout`
Jarak <- Data$`Jarak ke kampus`

PropShopeePay <- "90%"
Propall <- "100%"
Propmakan <- "95%"

ui <- shinyUI(
  dashboardPage(
    skin = "black-light",
    dashboardHeader(title = "Dashboard Kelompok 5", titleWidth = 243),
    dashboardSidebar( 
      sidebarMenu(
        menuItem("Data", tabName="page1",badgeLabel = "new",badgeColor = "green", icon=icon("folder-open"), selected = TRUE),
        menuItem("Visualisasi", tabName="page2", icon=icon("chart-simple"),
                 menuSubItem("Univariate Visualization",
                             tabName ="vizmenu1",
                             icon=icon("pie-chart")),
                 menuSubItem("Bivariate Visualization",
                             tabName ="vizmenu2",
                             icon=icon("bar-chart"))),
        menuItem("Kesimpulan dan Saran", tabName="page3", icon=icon("panorama"))
      ), width = 243),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "page1",
          
          fluidRow(
            box(
              title = "Overview Data",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              infoBoxOutput(
                outputId = "propA",
                width = 4
              ),
              infoBoxOutput(
                outputId = "propS",
                width = 4
              ),
              infoBoxOutput(
                outputId = "propM",
                width = 4
              )
            ),
            
            fluidRow(
              box(
                title = "Data Penggunaan e-Money TSD 21",
                status = "primary",
                solidHeader = FALSE,
                width = 12,
                dataTableOutput(
                  outputId = "data"
                )
              ))
          )
        ),
        
        tabItem(
          tabName = "vizmenu1",
          h1("Univariate Visualization"),
          tabBox(id="t2", width=10),
          fluidRow(
            box(
              title = "Select Variable",
              status = "primary",
              solidHeader = FALSE,
              width = 3,
              selectInput(
                inputId = "sel1",
                label = "Select Variable for Graph",
                choices =  c("Screen Time", "eMoney", "Pengeluaran", "Alokasi Pengeluaran", "Pemasukan", "Frekuensi Hangout", "Jarak ke Kampus"),
                selected = "Sportwear"
              ),
              
              sliderInput(
                inputId = "range", 
                label = "Number of Bins : ",
                min = 0, max = 20, value = 6
              )), 
            box(
              title = "Pilih Visualisasi",
              status = "primary",
              solidHeader = TRUE,
              width = 3,
              conditionalPanel(condition = "input.sel1 == 'Screen Time'", 
                               radioButtons("Screen_shape",
                                            label = "Visualisasi:",
                                            choices = c("Histogram 1", "Kernel Density 1"))),
              conditionalPanel(condition = "input.sel1 == 'eMoney'", 
                               radioButtons("eMoney_shape",
                                            label = "Visualisasi:",
                                            choices = c("Bar Chart 1", "Pie Chart 1"))),
              conditionalPanel(condition = "input.sel1 == 'Pengeluaran'", 
                               radioButtons("Pengeluaran_shape",
                                            label = "Visualisasi:",
                                            choices = c("Histogram 2", "Kernel Density 2"))),
              conditionalPanel(condition = "input.sel1 == 'Alokasi Pengeluaran'", 
                               radioButtons("Alokasi_shape",
                                            label = "Visualisasi:",
                                            choices = c("Bar Chart 2", "Pie Chart 2"))),
              conditionalPanel(condition = "input.sel1 == 'Pemasukan'", 
                               radioButtons("Pemasukan_shape",
                                            label = "Visualisasi:",
                                            choices = c("Histogram 3", "Kernel Density 3"))),
              conditionalPanel(condition = "input.sel1 == 'Frekuensi Hangout'", 
                               radioButtons("Frekuensi_shape",
                                            label = "Visualisasi:",
                                            choices = c("Histogram 4", "Kernel Density 4"))),
              conditionalPanel(condition = "input.sel1 == 'Jarak ke Kampus'", 
                               radioButtons("Jarak_shape",
                                            label = "Visualisasi:",
                                            choices = c("Histogram 5", "Kernel Density 5")))
              
            ),
            
            box(
              title = "Descriptive Statistics",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              verbatimTextOutput(
                outputId = "ds"
              )
              
            ),
            
            fluidRow(
              box(
                title = "Graph",
                closable = TRUE,
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                
                plotOutput(
                  outputId = "graph1"
                )
              ),
              box(
                title = "Interpretasi",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                htmlOutput(
                  outputId = "int1"
                )
                
              )
            ))
        ),
        tabItem(tabName = "vizmenu2",
                h1("Bivariate Visualization"),
                fluidRow(
                  box(
                    title = "Select Variable",
                    status = "primary",
                    solidHeader = FALSE,
                    width = 8,
                    selectInput(
                      inputId = "sel2",
                      label = "Select Variable for Graph",
                      choices =  c("Screen Time vs Pengeluaran", "Screen Time vs Pemasukan","Screen Time vs Frekuensi Hangout",
                                   "Screen Time vs Jarak ke Kampus", "Pengeluaran vs Pemasukan", "Pengeluaran vs Frekuensi Hangout",
                                   "Pengeluaran vs Jarak ke Kampus", "Pemasukan vs Frekuensi Hangout", "Pemasukan vs Jarak ke Kampus",
                                   "Frekuensi Hangout vs Jarak ke Kampus", "eMoney vs Alokasi Pengeluaran", "Screen Time vs eMoney",
                                   "Screen Time vs Alokasi Pengeluaran", "Pengeluaran vs eMoney", "Pengeluaran vs Alokasi Pengeluaran",
                                   "Pemasukan vs eMoney", "Pemasukan vs Alokasi Pengeluaran", "Frekuensi Hangout vs eMoney","Jarak ke Kampus vs eMoney"),
                      selected = "Sportwear"
                    )),
                  
                  fluidRow(
                    box(
                      title = "Graph",
                      closable = TRUE,
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      
                      plotOutput(
                        outputId = "graph2"
                      ))
                  ),
                  fluidRow(
                    box(
                      title = "Interpretasi",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      htmlOutput(
                        outputId = "int2"
                      )
                      
                    ))
                  
                )
        ),
        tabItem(
          tabName = "page3",
          
          fluidRow(
            box(
              title = "Kesimpulan dari Visualisasi Data",
              closable = TRUE,
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              htmlOutput(
                outputId = "int"
              ))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$propA <- renderInfoBox({
    infoBox(
      title = "E-Money",
      value = Propall,
      subtitle = "Persentasi Responden Pengguna E-Money",
      col = "red",
      icon = icon("pie-chart"),
      fill = TRUE
    )
  })
  
  output$propS <- renderInfoBox({
    infoBox(
      title = "ShopeePay",
      value = PropShopeePay,
      subtitle = "Persentasi Responden Pengguna ShopeePay",
      col = "purple",
      icon = icon("pie-chart"),
      fill = TRUE
    )
  })
  
  output$propM <- renderInfoBox({
    infoBox(
      title = "Konsumsi",
      value = Propmakan,
      subtitle = "Alokasi Pengeluaran Responden di Pangan",
      col = "blue",
      icon = icon("pie-chart"),
      fill = TRUE
    )
  })
  
  output$data <- renderDataTable({
    datatable(Data, colnames = c("Screen", "ShopeePay", "GoPay", "Ovo", "Dana", "Lainnya", "eMoney", "Pengeluaran", "Makan",  "Transportasi", "Wajib", "Tersier", "Pemasukan", "Hangout", "Jarak"))
  })
  
  output$graph1 <- renderPlot({
    if (input$sel1 == "Screen Time"){
      varname <- input$Screen_shape
      if (varname == "Histogram 1"){
        bins <- seq(min(Screen), max(Screen), length.out = (input$range + 1))
        hist(Screen, breaks = bins, col = brewer.pal(n = 6, "Reds"), main = "Histogram Screen Time", xlab = "Screen Time (jam)", ylab =  "Frekuensi Responden")
      }
      if (varname == "Kernel Density 1"){
        return(ggplot(data = Data, aes(x = Screen)) + 
                 ylab("Density") +
                 xlab("Screen Time (jam)") +
                 geom_density(fill = "#FF9999", col = "black") + 
                 ggtitle("Density Plot Screen Time"))
      }
    }
    else if (input$sel1 =="eMoney"){
      varname <- input$eMoney_shape
      if (varname == "Bar Chart 1"){
        ##Bar-Chart (sering)
        Jenis_EMoney <- c("ShopeePay", "Gopay", "Ovo", "Dana", "Lainnya")
        Frekuensi1 <- c(56, 55, 36, 28, 4)
        return (ggplot()+
                  geom_col(aes(x=Jenis_EMoney, y = Frekuensi1), fill= brewer.pal(n = 5, name = "Pastel1"), 
                           width=0.4, col = "black") +
                  coord_flip() +
                  ggtitle("Diagram Batang E-Money yang Digunakan") +
                  xlab("Jenis E-Money") +
                  ylab("Frekuensi Pengguna") +
                  theme(plot.title = element_text(hjust = 0.5)))
      }
      if (varname == "Pie Chart 1"){
        Jenis_EMoney <- c("ShopeePay", "Gopay", "Ovo", "Dana", "Lainnya")
        Frekuensi_Pengguna <- c(24, 24, 6, 4, 4)
        sering <- data.frame(Jenis_EMoney, Frekuensi_Pengguna)
        
        return(ggplot(sering, aes(x="", y=Frekuensi_Pengguna, fill=Jenis_EMoney)) +
                 geom_bar(stat="identity", width=1, color = "black") +
                 coord_polar("y", start=0) +
                 ggtitle("Pie Chart Jenis E-Money yang Paling Sering Digunakan") +
                 scale_fill_brewer(palette="Set2"))
        
      }
    }
    else if (input$sel1 =="Pengeluaran"){
      varname <- input$Pengeluaran_shape
      if (varname == "Histogram 2"){
        bins <- seq(min(Pengeluaran), max(Pengeluaran), length.out = input$range + 1)
        hist(Pengeluaran, breaks = bins, col = brewer.pal(n = 6, "Purples"), main = "Histogram Pengeluaran Responden", xlab = "Pengeluaran (Rp)", ylab =  "Frekuensi Responden")
      }
      if (varname == "Kernel Density 2"){
        return(ggplot(data = Data, aes(x = Pengeluaran)) + 
                 ylab("Density") +
                 xlab("Pengeluaran (Rp)") +
                 geom_density(fill = "#CCCCFF", col = "black") + 
                 ggtitle("Density Plot Pengeluaran"))
      }
    }
    else if (input$sel1 =="Alokasi Pengeluaran"){
      varname <- input$Alokasi_shape
      if (varname == "Bar Chart 2"){
        Jenis_Alokasi <- c("Makan", "Transportasi", "Belanja", "Gaya Hidup")
        Frekuensi_Pengguna <- c(59, 25, 30, 22)
        return(ggplot()+
                 geom_col(aes(x=Jenis_Alokasi, y = Frekuensi_Pengguna), fill= brewer.pal(n = 4, name = "Pastel2"), 
                          width=0.4, col = "black") +
                 coord_flip() +
                 ggtitle("Diagram Batang Alokasi Pengeluaran Pengguna E-Money") +
                 xlab("Jenis Alokasi") +
                 ylab("Frekuensi Pengguna") +
                 theme(plot.title = element_text(hjust = 0.5)))
      }
      if (varname == "Pie Chart 2"){
        Jenis_Alokasi <- c("Makan", "Transportasi", "Belanja", "Gaya Hidup")
        Frekuensi_Pengguna <- c(59, 25, 30, 22)
        sering <- data.frame(Jenis_Alokasi, Frekuensi_Pengguna)
        return(ggplot(sering, aes(x="", y=Frekuensi_Pengguna, fill=Jenis_Alokasi)) +
                 geom_bar(stat="identity", width=1, color = "black") +
                 coord_polar("y", start=0) +
                 ggtitle("Pie Chart Alokasi Pengeluaran Pengguna E-Money") +
                 scale_fill_brewer(palette="Set2"))
      }
    }
    else if (input$sel1 =="Pemasukan"){
      varname <- input$Pemasukan_shape
      if (varname == "Histogram 3"){
        bins <- seq(min(Pemasukan), max(Pemasukan), length.out = input$range + 1)
        hist(Pemasukan, breaks = bins, col = brewer.pal(n = 6, "Greens"), main = "Histogram Pemasukan", xlab = "Pemasukan (Rp)", ylab =  "Frekuensi Responden")
      }
      if (varname == "Kernel Density 3"){
        return(ggplot(data = Data, aes(x = Pemasukan)) + 
                 ylab("Density") +
                 xlab("Pemasukan (Rp)") +
                 geom_density(fill = "#99FFCC", col = "black") + 
                 ggtitle("Density Plot Pemasukan"))
      }
    }
    else if (input$sel1 =="Frekuensi Hangout"){
      varname <- input$Frekuensi_shape
      if (varname == "Histogram 4"){
        bins <- seq(min(Hangout), max(Hangout), length.out = input$range + 1)
        hist(Hangout, breaks = bins, col = brewer.pal(n = 6, "Blues"), main = "Histogram Frekuensi Hangout Responden", xlab = "Frekuensi Hangout", ylab =  "Frekuensi Responden")
      }
      if (varname == "Kernel Density 4"){
        return(ggplot(data = Data, aes(x = Hangout)) + 
                 ylab("Density") +
                 xlab("Frekuensi Hangout") +
                 geom_density(fill = "#0066CC", col = "black") + 
                 ggtitle("Density Plot Hangout"))
      }
    }
    else if (input$sel1 =="Jarak ke Kampus"){
      varname <- input$Jarak_shape
      if (varname == "Histogram 5"){
        bins <- seq(min(Jarak), max(Jarak), length.out = input$range + 1)
        hist(Jarak, breaks = bins, col = brewer.pal(n = 6, "Oranges"), main = "Histogram Jarak ke Kampus", xlab = "Jarak (km)", ylab =  "Frekuensi Responden")
      }
      if (varname == "Kernel Density 5"){
        return(ggplot(Data, aes(x = Jarak)) + 
                 ylab("Density") +
                 xlab("Jarak ke Kampus (km)") +
                 geom_density(fill = "#FF9966", col = "black") + 
                 ggtitle("Density Plot Jarak ke Kampus"))
      }
    }
    
  })
  
  output$ds <- renderPrint({
    varname <- input$sel1
    if (varname =="Screen Time"){
      return(summary(Screen))
    }
    if (varname =="Pengeluaran"){
      return(summary(Pengeluaran))
    }
    if (varname =="Pemasukan"){
      return(summary(Pemasukan))
    }
    if (varname =="Frekuensi Hangout"){
      return(summary(Hangout))
    }
    if (varname =="Jarak ke Kampus"){
      return(summary(Jarak))
    }
  })
  
  
  output$int1 <- renderUI({
    if (input$sel1 == "Screen Time"){
      varname <- input$Screen_shape
      if (varname == "Histogram 1"){
        return (HTML(paste("Dapat diketahui, bahwa waktu screen time Mahasiswa TSD Angkatan 2021 berada pada rentang antara 5 - 10 jam per hari serta memiliki outlier yakni lebih dari 15 jam per harinya. ")))
      }
      if (varname == "Kernel Density 1"){
        return (HTML(paste("Dapat diketahui, bahwa waktu screen time Mahasiswa TSD Angkatan 2021 berada pada rentang antara 5 - 10 jam per hari serta memiliki outlier yakni lebih dari 15 jam per harinya. ")))
      }
    }
    else if (input$sel1 =="eMoney"){
      varname <- input$eMoney_shape
      if (varname == "Bar Chart 1"){
        return (HTML(paste("Bar Chart e-money yang digunakan: Dapat diketahui, dari 62 responden (responden dapat memilih lebih dari satu), ShopeePay merupakan jenis e-money yang paling banyak digunakan Mahasiswa Teknologi Sains Data Angkatan 2021 dengan jumlah 56, kemudian disusul oleh GoPay sebanyak 55, dan Ovo sebanyak 36. Kemudian, paling sedikit adalah Lainnya, yakni berisi e-money lain seperti M-banking (Livin dan Brimo) sebanyak 5.")))
      }
      if (varname == "Pie Chart 1"){
        return (HTML(paste("dapat terlihat mayoritas menggunakan ShopeePay, dengan persentase 31.11%, lalu GoPay dengan persentase 30.56%. Kemudian, persentase paling sedikit adalah Lainnya (yakni M-banking Livin dan Brimo), sebesar 2.78%.")))
      }
    }
    else if (input$sel1 =="Pengeluaran"){
      varname <- input$Pengeluaran_shape
      if (varname == "Histogram 2"){
        return (HTML(paste("Untuk variabel pengeluaran memiliki rentang 0 - 10 (dalam juta rupiah) per bulan, sehingga dapat dikatakan bahwa pengeluaran Mahasiswa TSD Angkatan 2021 persebarannya pada 0 - 10 (dalam juta rupiah) per bulannya. Dari histogram juga dapat terlihat bahwa data memiliki outlier pada pengeluaran lebih dari 2 (dalam juta rupiah) per bulannya.")))
      }
      if (varname == "Kernel Density 2"){
        return (HTML(paste("Untuk variabel pengeluaran memiliki rentang 0 - 10 (dalam juta rupiah) per bulan, sehingga dapat dikatakan bahwa pengeluaran Mahasiswa TSD Angkatan 2021 persebarannya pada 0 - 10 (dalam juta rupiah) per bulannya. Dari Density Plot juga dapat terlihat bahwa data memiliki outlier pada pengeluaran lebih dari 2 (dalam juta rupiah) per bulannya.")))
      }
    }
    else if (input$sel1 =="Alokasi Pengeluaran"){
      varname <- input$Alokasi_shape
      if (varname == "Bar Chart 2"){
        return (HTML(paste("persentase kegunaan e-money oleh Mahasiswa TSD Angkatan 2021 adalah sebagian besar menggunakan e-money untuk keperluan makan, dengan persentase sebesar 41.4%, dilanjutkan dengan belanja kebutuhan wajib sebesar 21.4%, dan persentase paling sedikit adalah lainnya (hal-hal selain yang disebutkan), sebesar 2.9%.")))
      }
      if (varname == "Pie Chart 2"){
        return (HTML(paste("Kegunaan e-money paling banyak (responden boleh memilih lebih dari satu) adalah untuk makan sebanyak 58, kemudian belanja kebutuhan wajib sebanyak 30, dan untuk transportasi sebesar 25.")))
      }
    }
    else if (input$sel1 =="Pemasukan"){
      varname <- input$Pemasukan_shape
      if (varname == "Histogram 3"){
        return (HTML(paste("Persebaran pemasukan dari Mahasiswa TSD Angkatan 2021 adalah pada 1 - 2 juta rupiah per bulannya, serta pada data dapat terlihat bahwa data memiliki outlier pada pemasukan sebesar 5 juta per bulannya.")))
      }
      if (varname == "Kernel Density 3"){
        return (HTML(paste("Persebaran pemasukan dari Mahasiswa TSD Angkatan 2021 adalah pada 1 - 2 juta rupiah per bulannya, serta pada data dapat terlihat bahwa data memiliki outlier pada pemasukan sebesar 5 juta per bulannya.")))
      }
    }
    else if (input$sel1 =="Frekuensi Hangout"){
      varname <- input$Frekuensi_shape
      if (varname == "Histogram 4"){
        return (HTML(paste("Untuk frekuensi keluar rumah (di luar jam kuliah) Mahasiswa TSD Angkatan 2021 memiliki persebaran sebesar 0 - 20 kali per bulannya. Lalu, pada data dapat terlihat bahwa memiliki outlier pada nilai lebih dari 40 kali per bulannya.")))
      }
      if (varname == "Kernel Density 4"){
        return (HTML(paste("Untuk frekuensi keluar rumah (di luar jam kuliah) Mahasiswa TSD Angkatan 2021 memiliki persebaran sebesar 0 - 20 kali per bulannya. Lalu, pada data dapat terlihat bahwa memiliki outlier pada nilai lebih dari 40 kali per bulannya.")))
      }
    }
    else if (input$sel1 =="Jarak ke Kampus"){
      varname <- input$Jarak_shape
      if (varname == "Histogram 5"){
        return (HTML(paste("Jarak ke kampus mahasiswa TSD Angkatan 2021 memiliki persebaran data 0 - 10 (dalam km). Lalu, pada data dapat terlihat bahwa data memiliki outlier pada nilai lebih dari 25 (dalam km).")))
      }
      if (varname == "Kernel Density 5"){
        return (HTML(paste("Jarak ke kampus mahasiswa TSD Angkatan 2021 memiliki persebaran data 0 - 10 (dalam km). Lalu, pada data dapat terlihat bahwa data memiliki outlier pada nilai lebih dari 25 (dalam km).")))
      }
    }
  })
  
  
  
  output$graph2 <- renderPlot({
    if (input$sel2 == "Screen Time vs Pengeluaran"){
      return(ggplot(Data, aes(x = Screen, y = Pengeluaran)) + 
               geom_point(color = "cornflowerblue") +
               ggtitle("Scatter Plot Screen Time vs Pengeluaran") +
               theme(plot.title = element_text(hjust = 0.5))+
               xlab("Screen Time (jam)") +
               ylab("Pengeluaran (Rp)")
      )
    }
    else if (input$sel2 =="Screen Time vs Pemasukan"){
      return( ggplot(Data, aes(x = Screen, y = Pemasukan)) + 
                geom_point(color = "#CC0066") +
                ggtitle("Scatter Plot Screen Time vs Pemasukan") +
                theme(plot.title = element_text(hjust = 0.5))+ 
                xlab("Screen Time (jam)") +
                ylab("Pemasukan (Rp)"))
    }
    else if (input$sel2 =="Screen Time vs Frekuensi Hangout"){
      return(ggplot(Data, aes(x = Screen, y = Hangout)) + 
               geom_point(color = "#FF0099") +
               ggtitle("Scatter Plot Screen Time vs Pengeluaran") +
               theme(plot.title = element_text(hjust = 0.5)) + 
               xlab("Screen Time (jam)") +
               ylab("Frekuensi Hangout "))
    }
    else if (input$sel2 =="Screen Time vs Jarak ke Kampus"){
      return(ggplot(Data, aes(x = Screen, y = Jarak)) + 
               geom_point(color = "#CC3300") +
               ggtitle("Scatter Plot Screen Time vs Jarak ke Kampus") +
               theme(plot.title = element_text(hjust = 0.5))+
               xlab("Screen Time (jam)") +
               ylab("Jarak ke Kampus (km)"))
    }
    else if (input$sel2 =="Pengeluaran vs Pemasukan"){
      return(ggplot(Data, aes(x = Pengeluaran, y = Pemasukan)) + 
               geom_point(color = "#CC66FF") +
               ggtitle("Scatter Plot Pengeluaran vs Pemasukan") +
               theme(plot.title = element_text(hjust = 0.5)) +
               xlab("Pengeluaran (Rp)") +
               ylab("Pemasukan (Rp)"))
    }
    else if (input$sel2 =="Pengeluaran vs Frekuensi Hangout"){
      return(ggplot(Data, aes(x = Pengeluaran, y = Hangout)) + 
               geom_point(color = "Red")+
               ggtitle("Scatter Plot Pengeluaran vs Frekuensi Hangout") +
               theme(plot.title = element_text(hjust = 0.5)) +
               xlab("Pengeluaran (Rp)") +
               ylab("Frekuensi Hangout"))
    }
    else if (input$sel2 =="Pengeluaran vs Jarak ke Kampus"){
      return(ggplot(Data, aes(x = Pengeluaran, y = Jarak)) + 
               geom_point(color = "#3366CC")+
               ggtitle("Scatter Plot Pengeluaran vs Jarak ke Kampus") +
               theme(plot.title = element_text(hjust = 0.5)) +
               xlab("Pengeluaran (Rp)") +
               ylab("Jarak ke Kampus (km)"))
    }
    else if (input$sel2 =="Pemasukan vs Frekuensi Hangout"){
      return(ggplot(Data, aes(x = Pemasukan, y = Hangout)) + 
               geom_point(color = "#3366FF")+
               ggtitle("Scatter Plot Pemasukan vs Frekuensi Hangout") +
               theme(plot.title = element_text(hjust = 0.5)) +
               xlab("Pemasukan (Rp)") +
               ylab("Frekuensi Hangout"))
    }
    else if (input$sel2 =="Pemasukan vs Jarak ke Kampus"){
      return(ggplot(Data, aes(x = Pemasukan, y = Jarak)) + 
               geom_point(color = "#6633CC")+
               ggtitle("Scatter Plot Pemasukan vs Jarak ke Kampus") +
               theme(plot.title = element_text(hjust = 0.5)) +
               xlab("Pemasukan (Rp)") +
               ylab("Jarak ke Kampus (km)"))
    }
    else if (input$sel2 =="Frekuensi Hangout vs Jarak ke Kampus"){
      return(ggplot(Data, aes(x = Hangout, y = Jarak)) + 
               geom_point(color = "#9900CC")+
               ggtitle("Scatter Plot Frekuensi Hangout vs Jarak ke Kampus") +
               theme(plot.title = element_text(hjust = 0.5)) +
               xlab("Frekuensi Hangout") +
               ylab("Jarak ke Kampus"))
    }
    
    else if (input$sel2 =="eMoney vs Alokasi Pengeluaran"){
      jmlh_alokasi <- c(3,4,2,2,2,1,3,2,1,2,2,1,3,2,2,4,1,3,2,1,4,2,1,1,3,1,2,3,3,2,1,3,2,2,2,2,2,2,3,1,2,2,2,2,3,3,2,4,1,3,4,1,3,3,0,2,1,2,1,4,2,4)
      return(ggplot(Data, aes(x = eMoney, fill = factor(jmlh_alokasi))) +
               geom_bar(position = "fill")+
               labs(y = "Proportion")+
               ggtitle("Proposi Jumlah Alokasi Pengeluaran Berdasarkan Jenis E-Money")+
               scale_fill_brewer(palette="Set2"))+
        theme(plot.title = element_text(hjust = 0.5))
      
    }
    
    else if (input$sel2 =="Screen Time vs eMoney"){
      return(ggplot(Data, aes(x = eMoney, y = Screen)) + 
               geom_boxplot(notch = FALSE, fill = brewer.pal(n = 5, name = "Pastel1"), alpha = .7) +
               xlab("Jenis E-Money") +
               ylab("Screen Time")+
               ggtitle("Screen Time Pengguna Berdasarkan Jenis E-Money") +
               theme(plot.title = element_text(hjust = 0.5)))
    }
    else if (input$sel2 =="Screen Time vs Alokasi Pengeluaran"){
      jumlah_alokasi <- c(1,2,3,4,0)
      rata2screen2 <- c(8.34, 7.55, 8.44, 6.11, 4)
      data7 <- data.frame(jumlah_alokasi, rata2screen2)
      return(ggplot(data7, aes(x = jumlah_alokasi, y = rata2screen2)) + 
               geom_bar(stat = "identity", fill = brewer.pal(n = 5, name = "Pastel1")) +
               xlab("Jumlah Alokasi Pengeluaran") +
               ylab("Rata-Rata Screen Time (jam)")+
               ggtitle("Rata-Rata Screen Time Berdasarkan Jumlah Alokasi Pengeluaran") +
               theme(plot.title = element_text(hjust = 0.5)))
    }
    else if (input$sel2 =="Pengeluaran vs eMoney"){
      return(ggplot(Data, aes(x = Pengeluaran, y = eMoney, fill = eMoney)) +
               geom_density_ridges() +
               theme_ridges() +
               xlab("Pengeluaran") +
               ylab("Jenis E-Money") +
               ggtitle("Persebaran Pengeluaran Pengguna berdasarkan Jenis E-Money") +
               theme(plot.title = element_text(hjust = 0.5))+
               theme(legend.position = "none"))
    }
    else if (input$sel2 =="Pengeluaran vs Alokasi Pengeluaran"){
      jumlah_alokasi <- c(1,2,3,4,0)
      rata2peng2 <- c(1035714, 707885, 516429, 290000, 100000)
      data8 <- data.frame(jumlah_alokasi, rata2peng2)
      return(ggplot(data8, aes(x = jumlah_alokasi, y = rata2peng2)) + 
               geom_bar(stat = "identity", fill = brewer.pal(n = 5, name = "Pastel1")) +
               xlab("Jumlah Alokasi Pengeluaran") +
               ylab("Rata-Rata Pengeluaran (Rp)")+
               ggtitle("Rata-Rata Pengeluaran Berdasarkan Jumlah Alokasi Pengeluaran") +
               theme(plot.title = element_text(hjust = 0.5)))
    }
    else if (input$sel2 =="Pemasukan vs eMoney"){
      return(ggplot(Data, aes(x = eMoney, y = Pemasukan)) + 
               geom_boxplot(notch = FALSE, fill = brewer.pal(n = 5, name = "Pastel1"), alpha = .7) +
               xlab("Jenis E-Money") +
               ylab("Pemasukan")+
               ggtitle("Pemasukan Pengguna Berdasarkan Jenis E-Money") +
               theme(plot.title = element_text(hjust = 0.5)))
    }
    else if (input$sel2 =="Pemasukan vs Alokasi Pengeluaran"){
      jumlah_alokasi <- c(1,2,3,4,0)
      rata2pem2 <- c(1892857, 1659615, 1578571, 685714, 2000000)
      data9 <- data.frame(jumlah_alokasi, rata2pem2)
      return(ggplot(data9, aes(x = jumlah_alokasi, y = rata2pem2)) + 
               geom_bar(stat = "identity", fill = brewer.pal(n = 5, name = "Pastel1")) +
               xlab("Jumlah Alokasi Pengeluaran") +
               ylab("Rata-Rata Pemasukan (Rp)")+
               ggtitle("Rata-Rata Pemasukan Berdasarkan Jumlah Alokasi Pengeluaran") +
               theme(plot.title = element_text(hjust = 0.5)))
    }
    else if (input$sel2 =="Frekuensi Hangout vs eMoney"){
      return(ggplot(Data, aes(x = Hangout, y = eMoney, fill = eMoney)) +
               geom_density_ridges() +
               theme_ridges() +
               xlab("Frekuensi Hangout") +
               ylab("Jenis E-Money") +
               ggtitle("Persebaran Frekuensi Hangout Pengguna berdasarkan Jenis E-Money") +
               theme(plot.title = element_text(hjust = 0.5))+
               theme(legend.position = "none"))
    }
    else if (input$sel2 =="Jarak ke Kampus vs eMoney"){
      return(ggplot(Data, aes(x = eMoney, y = Jarak)) + 
               geom_boxplot(notch = FALSE, fill = brewer.pal(n = 5, name = "Pastel1"), alpha = .7) +
               xlab("Jenis E-Money") +
               ylab("Jarak (km)")+
               ggtitle("Jarak Tempat Tinggal Pengguna ke Kampus Berdasarkan Jenis E-Money") +
               theme(plot.title = element_text(hjust = 0.5)))
    }
    
  })
  
  output$int2 <- renderUI({
    return(HTML(paste(strong("Interpretasi Scatterplot :"),
    "Scatter variabel pengeluaran dan hangout: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar -0.01117393, yang artinya kedua variabel tersebut berkorelasi negatif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter pengeluaran dan jarak kampus:  Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar -0.166454, yang artinya kedua variabel tersebut berkorelasi negatif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter pemasukan dan hangout: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar 0.1382148, yang artinya kedua variabel tersebut berkorelasi positif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter pemasukan dan jarak ke kampus: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar -0.2936217, yang artinya kedua variabel tersebut berkorelasi negatif dan memiliki hubungan rendah/lemah.",
    "",
    "- Scatter frekuensi hangout dan jarak ke kampus: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar -0.08062157, yang artinya kedua variabel tersebut berkorelasi negatif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter screen time dan pengeluaran: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar -0.1862319, yang artinya kedua variabel tersebut berkorelasi negatif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter screen time dan pemasukan:  Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar 0.04946649, yang artinya kedua variabel tersebut berkorelasi positif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter screen time dan hangout: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar 0.1471118, yang artinya kedua variabel tersebut berkorelasi positif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter screen time dan jarak ke kampus: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar 0.02079218, yang artinya kedua variabel tersebut berkorelasi positif dan memiliki hubungan sangat rendah/lemah.",
    "",
    "- Scatter pengeluaran dan pemasukan: Dari hubungan kedua variabel tersebut, didapatkan pula nilai korelasinya sebesar 0.6656749, yang artinya kedua variabel tersebut berkorelasi positif dan memiliki hubungan kuat.",
    "",
    "",
    strong("Interpretasi Segmented BarChart E-Money :"),
    "Berdasarkan visualisasi segmented tersebut didapatkan bahwa pada kategori e-money Dana, jumlah alokasi di dominasi sebesar 2, diikuti dengan jumlah alokasi 0 dan 3 dengan sama banyak. Kemudian untuk kategori GoPay, jumlah alokasi di dominasi sebanyak 2, diikuti dengan jumlah alokasi 1 dan 3 dengan sama banyak, dan yang terkecil adalah jumlah alokasi sebesar 4. Untuk kategori Lainnya, jumlah alokasi di dominasi sebesar 1 dan diikuti dengan jumlah alokasi sebesar 4. Sedangkan OVO, di dominasi oleh jumlah alokasi sebesar 2, kemudian diikuti oleh jumlah alokasi sebesar 1 sebagai nomor 2 terbanyak, dan yang terakhir adalah jumlah alokasi sebesar 3. Yang terakhir adalah kategori ShopeePay, dengan di dominasi oleh jumlah alokasi terbanyak sebesar 1, 2, dan 3 dengan nilai yang sama besar. Dan diikuti dengan jumlah alokasi sebesar 4 yang memiliki nilai terkecil.",
    "",
    strong("Interpretasi Boxplot Screen Time Berdasarkan E-Money yang digunakan :"),
    "Dapat diketahui dari visualisasi Boxplot antara e-money dan screen time pengguna, untuk Lainnya dan Ovo tidak memiliki nilai outlier  , kemudian untuk e-money Dana, memiliki outlier sebesar 15 (dalam jam), kemudian e-money GoPay memiliki outlier sebesar lebih dari 15 (dalam jam), dan ShopeePay memiliki 3 nilai outlier yang memiliki nilai lebih dari 10 (dalam jam)",
    "",
    "",
    strong("Interpretasi BarChart Rata-Rata Screen time Berdasarkan Jumlah Alokasi Pengeluaran"),
    "Untuk visualisasi rata-rata screen time dengan jumlah alokasi pengeluaran, dapat dilihat jika jumlah alokasi pengeluarannya sebesar 0 (ShopeePay), maka rata-rata screen timenya adalah 8.34, kemudian jika jumlah alokasi sebesar 1 (GoPay), maka rata-rata screen time adalah sebesar 7.55, lalu jika alokasi pengeluarannya 2 (Ovo), maka rata-rata screen time mencapai 8.44, kemudian jika jumlah alokasi pengeluaran 3 (Dana), maka rata-rata screen time 6.11, dan terakhir jika jumlah alokasi pengeluaran 4 (Lainnya), maka rata-rata screen time adalah 4",
    "",
    "",
    strong("Interpretasi Ridgeline Plot Pengeluaran Berdasarkan e-Money yang digunakan"),
    "- Ridgeline Plot Pengeluaran dengan ShopeePay : Pengeluaran mahasiswa Teknologi Sains Data Angkatan 2021 menggunakan ShopeePay berada pada rentang ratusan ribu hingga 1 juta rupiah.",
    "",
    "- Ridgeline Plot Pengeluaran dengan Ovo : Untuk pengeluaran mahasiswa Teknologi Sains Data Angkatan 2021 pada penggunaan Ovo yakni berada pada rentang ribuan hingga ratusan ribu rupiah.",
    "",
    "- Ridgeline Plot Pengeluaran dengan E-money Lainnya : Untuk pengeluaran mahasiswa Teknologi Sains Data Angkatan 2021 pada E-money lainnya (seperti M-banking Livin dan Brimo) yakni sebesar lebih dari 2 juta rupiah, serta terdapat outlier dengan nilai 0 (tidak ada pengeluaran)",
    "",
    "- Ridgeline Plot Pengeluaran dengan Gopay : Untuk penggunaan Gopay pada mahasiswa Teknologi Sains Data Angkatan 2021, pengeluarannya berada pada rentang ratusan ribu hingga 2 juta rupiah.",
    "",
    "- Ridgeline Plot Pengeluaran dengan Dana : Pada penggunaan Dana oleh mahasiswa Teknologi Sains Data Angkatan 2021, pengeluarannya berada pada rentang ratusan ribu rupiah, namun terdapat outlier dengan nominal sekitar 4,5 juta rupiah.",
    "",
    "",
    strong("Interpretasi BarChart Rata-Rata Pengeluaran Berdasarkan Jumlah Alokasi Pengeluaran :"),
    "Untuk visualisasi rata-rata pengeluaran dengan jumlah alokasi pengeluaran, didapatkan jika jumlah alokasi pengeluarannya sebesar 0 maka rata-rata pengeluarannya adalah Rp100.000. Jika jumlah alokasi sebesar 1, maka rata-rata pengeluaran sebesar Rp1.035.714. Kemudian untuk jumlah alokasi pengeluaran sebesar 2, maka rata-rata pengeluaran sebesar Rp707.885. Kemudian jika jumlah alokasi pengeluaran 3, maka rata-rata pengeluarannya adalah sebanyak Rp516.429. Yang terakhir yaitu jika jumlah alokasi pengeluaran 4, maka rata-rata pengeluaran adalah Rp290.000.",
    "",
    "",
    strong("Interpretasi Boxplot Pemasukan Berdasarkan e-money yang digunakan :"),
    "Untuk Boxplot hubungan pemasukan berdasarkan penggunaan e-money, dapat dikatakan bahwa pengguna Dana, GoPay, dan ShopeePay, tidak memiliki nilai outlier pada pemasukan per bulannya. Sedangkan, pengguna Ovo dan Lainnya memiliki nilai masing-masing satu outlier .  Untuk e-money Ovo, outlierr yang dimiliki adalah Rp 2.500.000 per bulan, lalu untuk Lainnya memiliki nilai outlier pemasukan sekitar mendekati Rp 500.000 per bulannya.",
    "",
    "",
    strong("Interpretasi BarChart Rata-Rata Pemasukan berdasarkan Jumlah Alokasi Pengeluaran :"),
    "Untuk visualisasi rata-rata pemasukan dengan jumlah alokasi pengeluaran, didapatkan jika jumlah alokasi pengeluarannya sebesar 0 maka rata-rata pemasukannya adalah Rp2.000.000. Jika jumlah alokasi sebesar 1, maka rata-rata pemasukannya sebesar Rp1.892.857. Kemudian untuk jumlah alokasi pengeluaran sebesar 2, maka rata-rata pemasukannya sebesar Rp1.659.615. Kemudian jika jumlah alokasi pengeluaran 3, maka rata-rata pemasukan adalah sebanyak Rp1.578.571. Yang terakhir yaitu jika jumlah alokasi pengeluaran 4, maka rata-rata pemasukan adalah Rp685.714.",
    "",
    "",
    strong("Ridgeline Plot Frekuensi Hangout Responden Berdasarkan e-Money yang digunakan"),
    "- Ridgeline Plot Frekuensi Hangout dengan ShopeePay : Frekuensi Hangout mahasiswa Teknologi Sains Data Angkatan 2021 yang menggunakan ShopeePay berada pada rentang 0 (tidak hangout) hingga 30 kali perbulan, namun terdapat outlier yakni 50 dan 60 kali perbulan.",
    "",
    "- Ridgeline Plot Frekuensi Hangout dengan Ovo : Untuk Frekuensi Hangout mahasiswa Teknologi Sains Data Angkatan 2021 yang menggunakan Ovo yakni berada pada rentang 4 hingga 30 kali perbulan.",
    "",
    "- Ridgeline Plot Frekuensi Hangout dengan E-money Lainnya : Untuk Frekuensi Hangout mahasiswa Teknologi Sains Data Angkatan 2021 yang menggunakan E-money lainnya (seperti M-banking Livin dan Brimo) berada pada rentang 0 (tidak hangout) hingga 15 kali perbulan.",
    "",
    "- Ridgeline Plot Frekuensi Hangout dengan Gopay : Untuk Frekuensi Hangout pada mahasiswa Teknologi Sains Data Angkatan 2021 yang menggunakan Gopay berada pada rentang 2 hingga 45 kali perbulan.",
    "",
    "- Ridgeline Plot Frekuensi Hangout dengan Dana : Untuk Frekuensi Hangout pada mahasiswa Teknologi Sains Data Angkatan 2021 yang menggunakan Dana berada pada rentang 11 hingga 30 kali, namun terdapat outlier yaitu 60 kali perbulan.",
    "",
    "",
    strong("Interpretasi Boxplot Jarak ke Kampus Berdasarkan e-Money :"),
    "Dari visualisasi Boxplot tersebut, dapat diketahui bahwa pengguna e-money Dana memiliki jarak antara tempat tinggal dan kampus dengan median mendekati 10 km, serta tidak memiliki outlier . Kemudian, untuk pengguna GoPay, juga dapat dilihat data tidak memiliki nilai outlier_ dan median kurang dari 5 km. Setelah itu, untuk pengguna e-money Lainnya, memiliki nilai outlier sebesar lebih dari 25 km, untuk Ovo memiliki dua nilai outlier , yakni kurang dari 5 km dan lebih dari 15 km, serta pengguna ShopeePay jarak antar tempat tinggal dan kampus memiliki 3 nilai outlier , yakni ketiganya lebih dari 15 km, dengan salah satunya adalah berjarak 20 km.",
    "",
    sep = "<br/>")))
      })
  
  output$int <- renderUI({
    return (HTML(paste(strong("Kesimpulan"),
                       "Berdasarkan data penggunaan e-Money yang dikumpulkan, dapat dilihat bahwa hampir seluruh mahasiswa teknologi sains data angkatan 2021 menggunakan e-Money berupa shopeePay. Namun, jika dilihat dari summary datanya terlihat bahwa mahasiswa yang tidak memiliki e-Money berupa shopeePay telah memiliki e-Money lain. Hal ini menunjukkan bahwa semua mahasiswa teknologi sains data angkatan 2021 telah mempunyai dan menggunakan e-Money. E-Money tersebut paling banyak digunakan untuk kebutuhan pembelian makanan. Rata-rata pengeluaran mahasiswa  teknologi sains data angkatan 2021 dalam menggunakan e-Money adalah sekitar 600000 (rupiah), namun terdapat nilai pencilan sebesar 2300000 (rupiah), 2500000 (rupiah), dan 4500000 (rupiah). Setelah dilakukan analisis korelasi apa saja yang mempengaruhi pengeluaran tersebut, dihasilkan bahwa hanya variabel pemasukan yang memiliki hubungan yang cukup kuat. ",
                       "",
                       "",
                       strong("Saran untuk Penelitian Selanjutnya"),
                       "Saran untuk penelitian selanjutnya adalah akan lebih baik jika lebih banyak variabel yang ditambahkan untuk melihat kemungkinan korelasinya dengan pengeluaran penggunaan e-Money tersebut. Kemudian pertanyaan yang diajukan juga seharusnya bisa lebih spesifik, seperti perkiraan pengeluaran untuk tiap bagiannya seperti apa (makan, transportasi, dll).",
                       "",
                       "",
                       strong("Saran  untuk Mahasiswa TSD 2021"),
                       " Dari hasil analisis yang dilakukan, didapatkan bahwa pengeluaran terbesar adalah dalam pembelian makanan sehingga saran untuk mahasiswa teknologi sains data angkatan 2021 ketika menghadapi situasi dimana harus menghemat pengeluaran adalah dengan mengurangi pembelian makanan. Alternatifnya adalah bisa dengan membuat masakan secara mandiri sehingga bisa meminimalisir pengeluaran.",
                       "",
                       "",
                       sep = "<br/>")))
  })
}

shinyApp(ui, server)


