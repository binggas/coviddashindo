library(shiny)
library("dplyr")
library("ggplot2")
library("tidyr")
library("plotly")




ma <- function(x, n = 7){stats::filter(x, rep(1 / n, n), sides = 1)}


## BUBBLE PLOT
df_prov <- read.csv("data/provinsi.csv")
df_prov$date <- as.Date(df_prov$date, format = "%Y-%m-%d")

max_date <- max(df_prov$date)

df_pop <- read.csv("data/population_2019.csv")



# df_prov_latest <- df_prov[df_prov$date >= max(df_prov$date),]
# 
# df_prov_latest$cfr <- round(df_prov_latest$death_cumulative / df_prov_latest$case_cumulative * 100,2)
# 
# df_prov_v2 <- left_join(df_prov_latest, df_pop, by=c("province"="province")) %>%  filter(province != "Lain")
# 
# df_prov_v2$case_rate_100k <- round(df_prov_v2$case_cumulative/df_prov_v2$n_pop * 100000, 2)
# 
# cfr_mean <- mean(df_prov_v2$cfr)
  

## PROVINCE LEVEL

dfx <- df_prov %>% 
  select(date, province, case_cumulative) %>% 
  group_by(province) %>%
  mutate(case_daily = case_cumulative - lag(case_cumulative, default = 0)) %>% 
  mutate(case_daily = replace(case_daily, case_daily < 0, 0)) %>% 
  select(date, province, case_daily) %>% 
  spread(province, case_daily)


df_death <- df_prov %>% 
  select(date, province, death_cumulative) %>% 
  spread(province, death_cumulative)


df_case_daily <- dfx %>% 
  gather("province", "case_daily", -date) %>% 
  group_by(province) %>% 
  mutate(moving_avg = ma(case_daily))


# Shiny App

ui <- fluidPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #dec4de;
        }

        body, label, input, button, select { 
          font-family: "Times";
        }')
  )),
  
  
  
  titlePanel("Kasus Covid-19 Indonesia"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Pilih tanggal"),

      dateRangeInput("dates",
                       "Tanggal",
                       start = "2020-04-01",
                       # end = as.character(Sys.Date()-1))
                       end = max_date
      )
      , br()
      , textOutput("selected_var")
      ,br()
      ,br()
    ),
    
    
    mainPanel(
       tabsetPanel(
        tabPanel("CFR vs Kasus"
                 , h3("Perbandingan CFR terhadap Total Kasus tiap Provinsi") 
                 , p("Some text here")
                 , br()
                 , div( style = "position:relative",
                          plotOutput("plot2", hover = hoverOpts("plot_hover", delay = 250, delayType = "debounce"))
                          , uiOutput("hover_info")))
        ,tabPanel("Tren Provinsi"
                  , h3("Tren Kasus Harian tiap Provinsi dengan Rerata 7 Hari")
                  , p("Some text here")
                  , br()
                  , plotOutput("plot5"))
      )
      # , div( style = "position:relative",
      #         plotOutput("plot2", hover = hoverOpts("plot_hover", delay = 250, delayType = "debounce"))
      #         , uiOutput("hover_info")
      
              # , plotOutput("plot2")
              
             , br()
             , br()

             , selectInput("province_var", 
                          label = "Provinsi",
                          choices = c("Indonesia","Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Jambi", "Bangka Belitung", "Bengkulu",
                                      "Kepulauan Riau", "Sumatera Selatan", "Lampung",
                                      "Banten", "Jawa Tengah", "Jawa Timur", "Jawa Barat", "DKI Jakarta", "DI Yogyakarta",
                                      "Kalimantan Barat", "Kalimantan Tengah", "Kalimantan Selatan", "Kalimantan Timur", "Kalimantan Utara",
                                      "Sulawesi Utara", "Sulawesi Tengah", "Sulawesi Tenggara", "Gorontalo", "Sulawesi Selatan", "Sulawesi Barat",
                                      "Bali", "NTB", "NTT",
                                      "Maluku", "Maluku Utara", "Papua",  "Papua Barat"),
                          selected = "Indonesia")
      
             ,br()
           #  , h3("Kasus Harian")
           #  , br()
           #  , p("Some text here")
           #  , br()
           #  , plotOutput("plot3")
           #  , br()
           # , h3("Angka Kematian Total")
           #  , br()
           #  ,p("Some text here")
           # , br()
           #  , plotOutput("plot4")
              # ,tableOutput("data")
      
      # , tabsetPanel(
        , tabPanel("Plot",
                 fluidRow(
                   column(6, plotOutput("plot3")),
                   column(6, plotOutput("plot4"))
                 ))
        # )
      
      # , width = 7
              )
  )
)

# Server logic
server <- function(input, output) {
  
  subData <- reactive({
    df %>%
      filter(
        as.Date(date) >= as.Date(input$dates[1]),
        as.Date(date) <= as.Date(input$dates[2])
      )
  })
  
  subData2 <- reactive({
    df_prov %>%
      filter(
        as.Date(date) >= as.Date(input$dates[1]),
        as.Date(date) <= as.Date(input$dates[2])
      )
  })
  
  
  subData3 <- reactive({
    dfx %>%
      filter(
        as.Date(date) >= as.Date(input$dates[1]),
        as.Date(date) <= as.Date(input$dates[2])
      )
  })

  subData4 <- reactive({
    df_death %>%
      filter(
        as.Date(date) >= as.Date(input$dates[1]),
        as.Date(date) <= as.Date(input$dates[2])
      )
  })
  
  
  subData5 <- reactive({
    df_case_daily %>%
      filter(
        as.Date(date) >= as.Date(input$dates[1]),
        as.Date(date) <= as.Date(input$dates[2])
      )
  })
  
  
  output$selected_var <- renderText({ 
    paste("Grafik menunjukkan data dari tanggal", format(as.Date(input$dates[1]) , "%d %b"), "hingga", format(as.Date(input$dates[2]) , "%d %b %Y"))
  })
  
  
  output$plot2 <- renderPlot({
  
    df_prov_latest <- subData2()[subData2()$date >= max(subData2()$date),]
    df_prov_latest$cfr <- round(df_prov_latest$death_cumulative / df_prov_latest$case_cumulative * 100,2)
    df_prov_v2 <- left_join(df_prov_latest, df_pop, by=c("province"="province")) %>%  filter(province != "Lain")
    df_prov_v2$case_rate_100k <- round(df_prov_v2$case_cumulative/df_prov_v2$n_pop * 100000, 2)
    cfr_mean <- mean(df_prov_v2$cfr)
    
    
    ggplot(df_prov_v2, aes(x=case_rate_100k, y=cfr)) +
      geom_point(aes(size=n_pop, color=island), alpha=0.7) +
      geom_text(aes(label=ifelse(cfr > 6.5 | case_rate_100k >=50, province,"")), size=3, hjust=1, vjust=-0.5) +
      scale_colour_brewer(name  ="", palette = "Set2") +
      scale_size(name="Pop", range = c(1,20)) +
      scale_y_continuous(name = "CFR (%)") +
      scale_x_continuous(name= "Kasus per 100ribu Penduduk (log)") +
      coord_trans(x = "log10") +

      geom_hline(yintercept=cfr_mean, linetype="dashed", color="red") +
      annotate("text", x = 3 , y = cfr_mean - 0.45, label = paste("Rerata CFR",round(cfr_mean,1), "%"), size=3, color="blue") +

      theme_minimal() +
      theme(#legend.title=element_blank(),
        legend.position = 'bottom', legend.text = element_text(size = 6, face = "bold"), legend.key.size=unit(6,"point"))

  })


  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df_prov_v2, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left  #+ (0.1 * left_pct * (hover$range$right - hover$range$left))
    top_px <- hover$range$top  #+ (0.1 * top_pct * (hover$range$bottom - hover$range$top))
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px, "px; top:", top_px, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Provinsi: </b>", point$province, "<br/>",
                    "<b> CFR: </b>", point$cfr, "%", "<br/>",
                    "<b> Kasus Kumulatif: </b>", point$case_cumulative, "<br/>",
                    "<b> Angka Kematian Kumulatif: </b>", point$death_cumulative, "<br/>"
                    )))
    )
  })  
  
  
  

  
  output$plot3 <- renderPlot({

    req(input$province_var)
    df3 <- subData3() %>%
      select(date, .data[[input$province_var]])  %>%
      mutate(moving_avg = ma(.data[[input$province_var]]))
    
    df3 %>% 
      ggplot(aes(x=date, y=.data[[input$province_var]])) +
      geom_bar(stat="identity", fill="#999999", alpha=0.4) +
      geom_line(aes(y=moving_avg),stat="identity", color="red", show.legend=TRUE) +
      annotate("segment", x = input$dates[1]+8, xend = input$dates[1]+6, y = df3$moving_avg[8], yend = df3$moving_avg[8]+3.5, colour = "black") +
      annotate("text", x = input$dates[1]+8, y =df3$moving_avg[8]+4, label = "Rerata 7 hari", size=3, color="black", fill="white") +
      scale_x_date(name = "") +
      # scale_y_continuous(name= paste("Kasus Harian", input$province_var)) +
      scale_y_continuous(name = "") +
      ggtitle(paste("Kasus Harian di", input$province_var)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            text=element_text(family="Times"))
    
})
  

  output$plot4 <- renderPlot({
    
    req(input$province_var)
    subData4() %>%
      select(date, .data[[input$province_var]])  %>%
      ggplot(aes(x=date, y=.data[[input$province_var]])) +
      geom_line(stat="identity", color="orange", show.legend=TRUE, size=1) +
      scale_x_date(name = "") +
      # scale_y_continuous(name= paste("Angka Kematian", input$province_var)) +
      scale_y_continuous(name = "") +
      ggtitle(paste("Angka Kematian Total di", input$province_var)) +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            text=element_text(family="Times"))
  })
  
  
  output$plot5 <- renderPlot({
    
    req(input$province_var)
    subData5() %>% 
      filter(province != "Lain") %>% 
      ggplot(aes(x=date, y=moving_avg)) + 
      geom_line(color="red") +
      facet_wrap(.~ province,  scales = "free_y", ncol = 5) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size=7),
        strip.background = element_rect(fill="#ddf3f5"),
        
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        
        # plot.background = element_rect(fill = "#EAEAEA")
      ) 
    
    
  })
  
  
  
}
# Run the app
shinyApp(ui, server)

