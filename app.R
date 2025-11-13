library(shiny)
library(dplyr)

# Cargar datos
data <- read.csv("clusters_ciudades.csv")

# UI ----
ui <- fluidPage(
  titlePanel("Consulta de Riesgo por Ciudad y Distrito"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Selecciona la Ciudad:", choices = unique(data$city_name)),
      uiOutput("ward_ui"),
      actionButton("consultar", "Consultar Riesgo"),
      br(), br()
    ),
    
    mainPanel(
      h4("Resultado de la consulta:"),
      textOutput("resultado"),
      br(),
      textOutput("comentario")
    )
  )
)

# SERVER ----
server <- function(input, output, session) {
  
  # Actualizar wards según la ciudad seleccionada
  output$ward_ui <- renderUI({
    req(input$city)
    wards <- data %>% filter(city_name == input$city) %>% pull(admin_ward)
    selectInput("ward", "Selecciona el Distrito (admin_ward):", choices = unique(wards))
  })
  
  # Acción del botón
  observeEvent(input$consultar, {
    req(input$city, input$ward)
    
    result <- data %>%
      filter(city_name == input$city, admin_ward == input$ward) %>%
      select(cluster, risk_level) %>%
      slice(1)
    
    # Mensajes personalizados según riesgo
    comentario <- case_when(
      result$risk_level == "Alto" ~ "Esta zona presenta condiciones críticas que requieren atención prioritaria.",
      result$risk_level == "Medio" ~ "La zona tiene un riesgo moderado, es recomendable un monitoreo constante.",
      result$risk_level == "Bajo" ~ "Zona estable con condiciones favorables.",
      TRUE ~ "No hay información de riesgo disponible."
    )
    
    output$resultado <- renderText({
      paste0("Ciudad: ", input$city,
             " | Distrito: ", input$ward,
             " | Cluster: ", result$cluster,
             " | Nivel de riesgo: ", result$risk_level)
    })
    
    output$comentario <- renderText(comentario)
  })
}

# Ejecutar la app ----
shinyApp(ui, server)
