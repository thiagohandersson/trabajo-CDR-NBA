
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyr)

df <- read.csv("nba_stats.csv")

df_filtrado <- df |> 
  filter(MP >= 500) |>                                     
    mutate(Pos_simple = sapply(strsplit(Pos, "-"), function(x) x[1]),  # Pasamos los jugadores que tienen varias posiciones a solo 1
    PTS_pp = PTS / G,
    AST_pp = AST / G,
    TRB_pp = TRB / G,
    STL_pp = STL / G,
    BLK_pp = BLK / G,
    X3PA_pp = X3PA / G)

variables_disponibles <- c("Puntos por partido"      = "PTS_pp",
                           "Asistencias por partido" = "AST_pp",
                           "Rebotes por partido"     = "TRB_pp",
                           "Robos por partido"       = "STL_pp",
                           "Bloqueos por partido"    = "BLK_pp",
                           "Triples intentados/partido" = "X3PA_pp",
                           "Eficiencia de tiro (TS%)"   = "TS.",
                           "PER"                     = "PER",
                           "Win Shares(WS)"= "WS")

jugadores <- sort(unique(df_filtrado$Player))
lista_posiciones <- sort(unique(df_filtrado$Pos_simple))

ui <- fluidPage(
  titlePanel("Datos NBA (1990-2017)"),
  tabsetPanel(
    tabPanel(
      "Evolución de la NBA", #primer pestaña
      sidebarLayout(
        sidebarPanel(
          selectInput("var_evolucion", "Variable a graficar:",
                      choices = variables_disponibles),
          sliderInput("rango_anios", "Rango de años:",
                      min = min(df_filtrado$Year), max = max(df_filtrado$Year),
                      value = c(min(df_filtrado$Year), max(df_filtrado$Year)),
                      sep = "")
        ),
        mainPanel(
          plotOutput("grafico_evolucion")
        )
      )
    ),
    tabPanel(
      "Comparar por posición", #segunda pestaña
      sidebarLayout(
        sidebarPanel(
          selectInput("var_posicion", "Variable a comparar:",
                      choices = variables_disponibles),
          checkboxGroupInput("posiciones_elegidas", "Posiciones:",
                             choices = lista_posiciones,
                             selected = lista_posiciones)
        ),
        mainPanel(
          plotOutput("grafico_posicion")
        )
      )
    ),
    tabPanel("Comparar jugadores", #3er pestaña
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("jugador1", "Jugador 1:", choices = jugadores,
                                selected = jugadores[1],
                                options = list(maxOptions = length(jugadores))),
                 selectizeInput("jugador2", "Jugador 2:", choices = jugadores,
                                selected = jugadores[2],
                                options = list(maxOptions = length(jugadores)))
               ),
               mainPanel(
                 plotOutput("grafico_comparacion"),
                 br(),
                 DTOutput("tabla_comparacion")
               )
             )
    ),
    tabPanel("Top jugadores por metrica", # pestaña 4
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("metricatop", "Metrica de Jugadores",
                 choices = variables_disponibles)
               ),
               mainPanel(
                 plotOutput("grafico_metricatop"),
               )
             ))
  )
 )


server <- function(input, output) {
  
  
  #Reactive de la pestaña 1
  datos_evolucion <- reactive({
    df_filtrado |> 
      filter(Year >= input$rango_anios[1], Year <= input$rango_anios[2]) |> 
      group_by(Year) |> 
      summarise(promedio = mean(.data[[input$var_evolucion]]))
  })
  
  output$grafico_evolucion <- renderPlot({
    ggplot(datos_evolucion(), aes(x = Year, y = promedio)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue") +
      labs(x = "Año", y = names(variables_disponibles)[variables_disponibles == input$var_evolucion],
           title = "Evolución del promedio de la liga") +
      theme_minimal()
  })
  #reactive pestaña2
  datos_posicion <- reactive({
    req(input$posiciones_elegidas)
    df_filtrado |>  filter(Pos_simple %in% input$posiciones_elegidas)
  })
  
  output$grafico_posicion <- renderPlot({
    ggplot(datos_posicion(), aes(x = Pos_simple, y = .data[[input$var_posicion]], fill = Pos_simple)) +
      geom_boxplot() +
      labs(x = "Posición", y = names(variables_disponibles)[variables_disponibles == input$var_posicion],
           title = "Distribución por posición") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  #reactive pestaña3
  comparacion_jugadores <- reactive({
    req(input$jugador1, input$jugador2)
    
    df_filtrado |> #medias de todas las metricas de los partidos
      filter(Player %in% c(input$jugador1, input$jugador2)) %>%
      group_by(Player)  |> 
      summarise(
        `Puntos/partido`      = round(mean(PTS_pp), 1),
        `Asistencias/partido` = round(mean(AST_pp), 1),
        `Rebotes/partido`     = round(mean(TRB_pp), 1),
        `Robos/partido`       = round(mean(STL_pp), 1),
        `Bloqueos/partido`    = round(mean(BLK_pp), 1),
        `TS.`                 = round(mean(TS.), 3),
        `Temporadas en la base` = n()
      )
  })
  
  output$grafico_comparacion <- renderPlot({ #separar para luego realizar la tabla y grafico de la metrica y su media
    datos_largos <- comparacion_jugadores() |> 
      select(Player, `Puntos/partido`, `Asistencias/partido`, `Rebotes/partido`,
             `Robos/partido`, `Bloqueos/partido`) |> 
      pivot_longer(cols = -Player, names_to = "metric", values_to = "mediametric")
    
    ggplot(datos_largos, aes(x = metric, y = mediametric, fill = Player)) +
      geom_col(position = "dodge") +
      labs(x = NULL, y = "Promedio por partido", title = "Comparación de jugadores",
           fill = "Jugador") +
      theme_minimal()
  })
  
  output$tabla_comparacion <- renderDT({
    comparacion_jugadores()
  }, options = list(dom = 't')) #para que no aparezcan cosas default que no nos interesaban de la tabla
  # reactive pestaña 4
  
  metrica_top <- reactive({
    req(input$metricatop)
    df_filtrado |> 
      group_by(Player) |> 
      summarise(promediotop = mean(.data[[input$metricatop]])) |> 
      arrange(desc(promediotop)) |> 
      slice_head(n = 10) |> 
      mutate(Player = factor(Player, levels = Player)) 
  })
  output$grafico_metricatop <- renderPlot(
    ggplot(metrica_top(), aes(x = Player, y = promediotop)) + 
             geom_col(color = "black", fill = "yellow")+ 
             theme_minimal()+
             labs(x = "Jugador", y = "Promedio de Metrica", title = "Top 10 jugadores por métrica")
  )
  
}
shinyApp(ui, server)
  
  
  
  
  
  
  
  
  
                                

