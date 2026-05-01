# app.R

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(DT)

# ------------------------------------------------------------
# Funkcja wyznaczająca cenę i greki dla europejskiej opcji
# Model Blacka-Scholesa z ciągłą stopą dywidendy q
# ------------------------------------------------------------

bs_greeks <- function(S, K, T, r, sigma, q = 0, type = "call") {
  
  S <- pmax(S, 1e-8)
  K <- pmax(K, 1e-8)
  T <- pmax(T, 1e-8)
  sigma <- pmax(sigma, 1e-8)
  
  d1 <- (log(S / K) + (r - q + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  discount_r <- exp(-r * T)
  discount_q <- exp(-q * T)
  
  if (type == "call") {
    
    price <- S * discount_q * pnorm(d1) - K * discount_r * pnorm(d2)
    delta <- discount_q * pnorm(d1)
    theta <- -S * discount_q * dnorm(d1) * sigma / (2 * sqrt(T)) -
      r * K * discount_r * pnorm(d2) +
      q * S * discount_q * pnorm(d1)
    rho <- K * T * discount_r * pnorm(d2)
    
  } else {
    
    price <- K * discount_r * pnorm(-d2) - S * discount_q * pnorm(-d1)
    delta <- discount_q * (pnorm(d1) - 1)
    theta <- -S * discount_q * dnorm(d1) * sigma / (2 * sqrt(T)) +
      r * K * discount_r * pnorm(-d2) -
      q * S * discount_q * pnorm(-d1)
    rho <- -K * T * discount_r * pnorm(-d2)
  }
  
  gamma <- discount_q * dnorm(d1) / (S * sigma * sqrt(T))
  kappa <- S * discount_q * dnorm(d1) * sqrt(T)
  
  data.frame(
    S = S,
    K = K,
    T = T,
    r = r,
    sigma = sigma,
    q = q,
    type = type,
    price = price,
    delta = delta,
    gamma = gamma,
    kappa = kappa,
    theta = theta,
    rho = rho,
    kappa_1pp = kappa / 100,
    rho_1pp = rho / 100,
    theta_daily = theta / 365
  )
}

# ------------------------------------------------------------
# Interfejs użytkownika
# ------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Greki opcji europejskiej"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Panel główny", tabName = "main", icon = icon("chart-line")),
      menuItem("Tabela", tabName = "table", icon = icon("table"))
    ),
    
    hr(),
    
    selectInput(
      inputId = "type",
      label = "Typ opcji:",
      choices = c("Call" = "call", "Put" = "put"),
      selected = "call"
    ),
    
    numericInput(
      inputId = "S",
      label = "Cena instrumentu bazowego S:",
      value = 100,
      min = 0.01,
      step = 1
    ),
    
    numericInput(
      inputId = "K",
      label = "Cena wykonania K:",
      value = 100,
      min = 0.01,
      step = 1
    ),
    
    sliderInput(
      inputId = "T",
      label = "Czas do wygaśnięcia T, w latach:",
      min = 0.01,
      max = 5,
      value = 1,
      step = 0.01
    ),
    
    sliderInput(
      inputId = "r",
      label = "Stopa wolna od ryzyka r:",
      min = -5,
      max = 20,
      value = 5,
      step = 0.1,
      post = "%"
    ),
    
    sliderInput(
      inputId = "sigma",
      label = "Zmienność sigma:",
      min = 1,
      max = 100,
      value = 20,
      step = 1,
      post = "%"
    ),
    
    sliderInput(
      inputId = "q",
      label = "Stopa dywidendy q:",
      min = 0,
      max = 20,
      value = 0,
      step = 0.1,
      post = "%"
    ),
    
    selectInput(
      inputId = "heat_greek",
      label = "Greka na mapie ciepła:",
      choices = c(
        "Delta" = "delta",
        "Gamma" = "gamma",
        "kappa dla 1 p.p." = "kappa_1pp",
        "Theta dzienna" = "theta_daily",
        "Rho dla 1 p.p." = "rho_1pp"
      ),
      selected = "delta"
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .small-box { border-radius: 12px; }
        .box { border-radius: 12px; }
      "))
    ),
    
    tabItems(
      
      tabItem(
        tabName = "main",
        
        fluidRow(
          valueBoxOutput("box_price"),
          valueBoxOutput("box_delta"),
          valueBoxOutput("box_gamma")
        ),
        
        fluidRow(
          valueBoxOutput("box_kappa"),
          valueBoxOutput("box_theta"),
          valueBoxOutput("box_rho")
        ),
        
        fluidRow(
          box(
            title = "Cena opcji i wypłata w dniu wygaśnięcia",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_price", height = 360)
          ),
          
          box(
            title = "Greki jako funkcje ceny instrumentu bazowego",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_greeks_spot", height = 360)
          )
        ),
        
        fluidRow(
          box(
            title = "Mapa ciepła wybranej greki względem ceny S i zmienności",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_heatmap", height = 450)
          )
        )
      ),
      
      tabItem(
        tabName = "table",
        
        fluidRow(
          box(
            title = "Bieżące wartości parametrów i współczynników greckich",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            DTOutput("table_greeks")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# Logika serwera
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  current_values <- reactive({
    bs_greeks(
      S = input$S,
      K = input$K,
      T = input$T,
      r = input$r / 100,
      sigma = input$sigma / 100,
      q = input$q / 100,
      type = input$type
    )
  })
  
  # ----------------------------------------------------------
  # Value boxes
  # ----------------------------------------------------------
  
  output$box_price <- renderValueBox({
    x <- current_values()
    valueBox(
      value = round(x$price, 4),
      subtitle = "Cena opcji",
      icon = icon("coins"),
      color = "blue"
    )
  })
  
  output$box_delta <- renderValueBox({
    x <- current_values()
    valueBox(
      value = round(x$delta, 4),
      subtitle = "Delta",
      icon = icon("arrow-right"),
      color = "green"
    )
  })
  
  output$box_gamma <- renderValueBox({
    x <- current_values()
    valueBox(
      value = round(x$gamma, 6),
      subtitle = "Gamma",
      icon = icon("project-diagram"),
      color = "yellow"
    )
  })
  
  output$box_kappa <- renderValueBox({
    x <- current_values()
    valueBox(
      value = round(x$kappa_1pp, 4),
      subtitle = "kappa dla zmiany zmienności o 1 p.p.",
      icon = icon("wave-square"),
      color = "purple"
    )
  })
  
  output$box_theta <- renderValueBox({
    x <- current_values()
    valueBox(
      value = round(x$theta_daily, 4),
      subtitle = "Theta dzienna",
      icon = icon("clock"),
      color = "red"
    )
  })
  
  output$box_rho <- renderValueBox({
    x <- current_values()
    valueBox(
      value = round(x$rho_1pp, 4),
      subtitle = "Rho dla zmiany stopy o 1 p.p.",
      icon = icon("percent"),
      color = "aqua"
    )
  })
  
  # ----------------------------------------------------------
  # Wykres ceny opcji
  # ----------------------------------------------------------
  
  output$plot_price <- renderPlot({
    
    S_grid <- seq(
      from = max(0.01, min(input$S, input$K) * 0.3),
      to = max(input$S, input$K) * 2.2,
      length.out = 300
    )
    
    df_price <- bs_greeks(
      S = S_grid,
      K = input$K,
      T = input$T,
      r = input$r / 100,
      sigma = input$sigma / 100,
      q = input$q / 100,
      type = input$type
    )
    
    if (input$type == "call") {
      payoff <- pmax(S_grid - input$K, 0)
    } else {
      payoff <- pmax(input$K - S_grid, 0)
    }
    
    df_plot <- data.frame(
      S = S_grid,
      `Cena opcji` = df_price$price,
      `Wypłata w terminie wygaśnięcia` = payoff
    ) |>
      pivot_longer(
        cols = -S,
        names_to = "Seria",
        values_to = "Wartość"
      )
    
    ggplot(df_plot, aes(x = S, y = Wartość, linetype = Seria)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$S, linetype = "dashed") +
      geom_vline(xintercept = input$K, linetype = "dotted") +
      labs(
        x = "Cena instrumentu bazowego S",
        y = "Wartość",
        linetype = NULL
      ) +
      theme_minimal()
  })
  
  # ----------------------------------------------------------
  # Wykres greków względem ceny instrumentu bazowego
  # ----------------------------------------------------------
  
  output$plot_greeks_spot <- renderPlot({
    
    S_grid <- seq(
      from = max(0.01, min(input$S, input$K) * 0.3),
      to = max(input$S, input$K) * 2.2,
      length.out = 300
    )
    
    df <- bs_greeks(
      S = S_grid,
      K = input$K,
      T = input$T,
      r = input$r / 100,
      sigma = input$sigma / 100,
      q = input$q / 100,
      type = input$type
    )
    
    df_long <- df |>
      select(
        S,
        Delta = delta,
        Gamma = gamma,
        `kappa, 1 p.p.` = kappa_1pp,
        `Theta dzienna` = theta_daily,
        `Rho, 1 p.p.` = rho_1pp
      ) |>
      pivot_longer(
        cols = -S,
        names_to = "Greka",
        values_to = "Wartość"
      )
    
    ggplot(df_long, aes(x = S, y = Wartość)) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = input$S, linetype = "dashed") +
      facet_wrap(~ Greka, scales = "free_y", ncol = 2) +
      labs(
        x = "Cena instrumentu bazowego S",
        y = NULL
      ) +
      theme_minimal()
  })
  
  # ----------------------------------------------------------
  # Mapa ciepła: wybrana greka względem S oraz sigma
  # ----------------------------------------------------------
  
  output$plot_heatmap <- renderPlot({
    
    S_grid <- seq(
      from = max(0.01, min(input$S, input$K) * 0.4),
      to = max(input$S, input$K) * 2.0,
      length.out = 120
    )
    
    sigma_grid <- seq(
      from = 0.01,
      to = 1.00,
      length.out = 100
    )
    
    df_grid <- expand.grid(
      S = S_grid,
      sigma = sigma_grid
    )
    
    df_calc <- bs_greeks(
      S = df_grid$S,
      K = input$K,
      T = input$T,
      r = input$r / 100,
      sigma = df_grid$sigma,
      q = input$q / 100,
      type = input$type
    )
    
    df_grid$value <- df_calc[[input$heat_greek]]
    
    greek_label <- switch(
      input$heat_greek,
      "delta" = "Delta",
      "gamma" = "Gamma",
      "kappa_1pp" = "kappa dla 1 p.p.",
      "theta_daily" = "Theta dzienna",
      "rho_1pp" = "Rho dla 1 p.p."
    )
    
    ggplot(df_grid, aes(x = S, y = sigma, fill = value)) +
      geom_tile() +
      geom_vline(xintercept = input$S, linetype = "dashed") +
      geom_vline(xintercept = input$K, linetype = "dotted") +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      scale_fill_viridis_c() +
      labs(
        x = "Cena instrumentu bazowego S",
        y = "Zmienność sigma",
        fill = greek_label
      ) +
      theme_minimal()
  })
  
  # ----------------------------------------------------------
  # Tabela
  # ----------------------------------------------------------
  
  output$table_greeks <- renderDT({
    
    x <- current_values()
    
    df_table <- data.frame(
      Miara = c(
        "Typ opcji",
        "Cena instrumentu bazowego S",
        "Cena wykonania K",
        "Czas do wygaśnięcia T",
        "Stopa wolna od ryzyka r",
        "Zmienność sigma",
        "Stopa dywidendy q",
        "Cena opcji",
        "Delta",
        "Gamma",
        "kappa",
        "kappa dla zmiany zmienności o 1 p.p.",
        "Theta roczna",
        "Theta dzienna",
        "Rho",
        "Rho dla zmiany stopy o 1 p.p."
      ),
      Wartość = c(
        ifelse(input$type == "call", "Call", "Put"),
        round(x$S, 6),
        round(x$K, 6),
        round(x$T, 6),
        paste0(round(100 * x$r, 4), "%"),
        paste0(round(100 * x$sigma, 4), "%"),
        paste0(round(100 * x$q, 4), "%"),
        round(x$price, 6),
        round(x$delta, 6),
        round(x$gamma, 6),
        round(x$kappa, 6),
        round(x$kappa_1pp, 6),
        round(x$theta, 6),
        round(x$theta_daily, 6),
        round(x$rho, 6),
        round(x$rho_1pp, 6)
      )
    )
    
    datatable(
      df_table,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        dom = "t"
      )
    )
  })
}

# ------------------------------------------------------------
# Uruchomienie aplikacji
# ------------------------------------------------------------

shinyApp(ui = ui, server = server)
