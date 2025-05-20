#–– Assicurati di avere installati questi pacchetti:
# install.packages(c("shiny","readxl","dplyr","tidyr","plotly","janitor","stringr","bslib"))

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(janitor)
library(stringr)

# 1) Caricamento e pulizia dati
df_raw <- read_excel(
  path  = "pip_rendimenti_fine2023.xlsx",
  sheet = "PIP - Elenco rendimenti"
)

df <- df_raw %>%
  clean_names() %>%
  fill(societa) %>% 
  mutate(across(c(x1anno, x3anni, x5anni, x10anni, x20anni),
                as.character)) %>%
  select(
    societa  = societa,
    pip      = pip,
    linea    = linea,
    `1anno`  = x1anno,
    `3anni`  = x3anni,
    `5anni`  = x5anni,
    `10anni` = x10anni,
    `20anni` = x20anni
  ) %>%
  pivot_longer(
    cols      = `1anno`:`20anni`,
    names_to  = "orizzonte",
    values_to = "rendimento"
  ) %>%
  mutate(
    orizzonte = factor(
      orizzonte,
      levels = c("1anno","3anni","5anni","10anni","20anni")
    ),
    rendimento = as.numeric(str_replace_all(rendimento, ",", "."))
  )

# 2) Definisci un tema responsive
my_theme <- bs_theme(
  version           = 4,
  bootswatch        = "flatly",
  bg                = "#f8f9fa",
  fg                = "#212529",
  primary           = "#2c7fb8",
  base_font_size    = "14px",
  heading_font_scale= 1.1
)

# 3) UI
ui <- fluidPage(
  theme = my_theme,
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1")
  ),
  titlePanel("Esplora performance PIP"),
  
  # Controlli in colonna unica
  fluidRow(
    column(
      width = 12,
      uiOutput("controls"),
      hr()
    )
  ),
  
  # Grafici in colonna unica con altezza ridotta per mobile
  fluidRow(
    column(
      width = 12,
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        tabPanel("Distribuzione",
                 plotlyOutput("boxDist", height = "300px"),
                 helpText("Boxplot dei rendimenti per l'orizzonte selezionato. Passa il mouse sui puntini per vedere la società.")
        ),
        tabPanel("Rendimento medio",
                 plotlyOutput("meanLine", height = "300px"),
                 helpText("Rendimento medio annuo per ciascun orizzonte.")
        ),
        tabPanel("Top N linee",
                 plotlyOutput("barTop", height = "300px"),
                 helpText("Barre orizzontali con le prime N linee ordinate per rendimento.")
        ),
        tabPanel("Confronto Top 5",
                 plotlyOutput("lineTop5", height = "300px"),
                 helpText("Andamento delle prime 5 linee su tutti gli orizzonti.")
        )
      )
    )
  )
)

# 4) Server
server <- function(input, output, session) {
  
  # Controlli dinamici
  output$controls <- renderUI({
    tagList(
      selectInput(
        inputId = "horiz",
        label   = "Orizzonte temporale:",
        choices = levels(df$orizzonte),
        selected= "10anni"
      ),
      sliderInput(
        inputId = "topN",
        label   = "Numero di linee da visualizzare:",
        min     = 3,
        max     = 20,
        value   = 10,
        width   = "100%"
      ),
      p(class="small text-muted",
        "Con lo slider scegli quante delle prime linee, ordinate per rendimento decrescente,",
        "vuoi evidenziare nei grafici 'Top N'."
      )
    )
  })
  
  # 4.1) Distribuzione con hover sulla società
  output$boxDist <- renderPlotly({
    df_sub <- df %>% filter(orizzonte == input$horiz)
    plot_ly(
      df_sub,
      x = ~rendimento,
      type = "box",
      boxpoints = "all",
      jitter    = 0.3,
      marker    = list(color = '#2c7fb8'),
      line      = list(color = '#2c7fb8'),
      # Costruiamo direttamente il text con tutti i campi desiderati
      text      = ~paste0(
        "Società: ", societa, "<br>",
        "PIP: ",     pip,     "<br>",
        "Linea: ",   linea
      ),
      hoverinfo = "text+x"
    ) %>%
      layout(
        title = paste("Distribuzione rendimenti a", input$horiz),
        xaxis = list(title = "Rendimento (%)"),
        yaxis = list(showticklabels = FALSE)
      )
  })
  
  # 4.2) Rendimento medio annuo
  output$meanLine <- renderPlotly({
    df_mean <- df %>%
      group_by(orizzonte) %>%
      summarise(rend_med = mean(rendimento, na.rm = TRUE), .groups="drop")
    
    plot_ly(
      df_mean,
      x    = ~orizzonte,
      y    = ~rend_med,
      type = 'scatter',
      mode = 'lines+markers',
      line   = list(width = 2),
      marker = list(size = 8)
    ) %>%
      layout(
        title = "Rendimento medio annuo per orizzonte",
        xaxis = list(title = "Orizzonte"),
        yaxis = list(title = "Rendimento medio (%)")
      )
  })
  
  # 4.3) Top N linee
  # 4.3) Top N linee (ora etichette “PIP – LINEA”)
  output$barTop <- renderPlotly({
    df %>%
      filter(orizzonte == input$horiz) %>%
      arrange(desc(rendimento)) %>%
      slice_head(n = input$topN) %>%
      # creiamo una colonna temporanea per l’etichetta
      mutate(label = paste0(pip, " – ", linea)) %>%
      plot_ly(
        x           = ~rendimento,
        y           = ~reorder(label, rendimento),
        type        = 'bar',
        orientation = 'h',
        marker      = list(
          color = 'rgba(44,160,44,0.7)',
          line  = list(color = 'rgba(44,160,44,1.0)', width = 1)
        ),
        text        = ~paste(
          "PIP: ", pip, "<br>",
          "Linea: ", linea, "<br>",
          "Società: ", societa
        ),
        hoverinfo   = "text+x"
      ) %>%
      layout(
        title = paste0("Top ", input$topN, " linee a ", input$horiz),
        xaxis = list(title = "Rendimento (%)"),
        yaxis = list(title = "", automargin = TRUE)
      )
  })
  
  # 4.4) Confronto Top 5
  output$lineTop5 <- renderPlotly({
    top5 <- df %>%
      filter(orizzonte == input$horiz) %>%
      arrange(desc(rendimento)) %>%
      slice_head(n = 5) %>%
      pull(linea)
    
    df %>%
      filter(linea %in% top5) %>%
      plot_ly(
        x     = ~orizzonte,
        y     = ~rendimento,
        color = ~linea,
        colors= "Set2",
        type  = 'scatter',
        mode  = 'lines+markers',
        text  = ~paste("Società:", societa),
        hoverinfo = "text+y"
      ) %>%
      layout(
        title = paste("Andamento Top 5 linee a", input$horiz),
        xaxis = list(title = "Orizzonte"),
        yaxis = list(title = "Rendimento (%)")
      )
  })
  
}

# 5) Avvia l’app
shinyApp(ui, server)
