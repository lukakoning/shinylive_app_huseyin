# To export: shinylive::export("./", "site")

#### 0 Libraries ####

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

#### 1 Data laden ####

# Voorberwerking data; omzetten naar base64 str
if (FALSE) {
  data <- read.csv("../Werkgelegenheid_Twente_Nieuw.csv", sep = ";")

  data$Peildatum <- as.Date(data$Peildatum, format = "%d-%m-%Y")
  data$Jaar <- as.numeric(format(data$Peildatum, "%Y"))
  data$Gebied <- iconv(data$Gebied, from = "latin1", to = "UTF-8")
  data$Type <- iconv(data$Type, from = "latin1", to = "UTF-8")

  # Alleen de relevante kolommen behouden
  data <- data %>%
    select(
      Jaar,
      Gebied,
      Type,
      Werkzame_personen_totaal,
      Werkzame_personen_fulltime,
      Werkzame_personen_parttime
    )

  saveRDS(data, "data.RDS")
}

# Voorbewerkte data inlezen
if (TRUE) {
  data <- readRDS("data.RDS")

  # Get unique and sorted years for the year select input
  years <- sort(unique(data$Jaar))
}

#### 2 UI #####

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5),

  tagList(
    tags$head(
      # Ensure dataTables is loaded in time (otherwise issues with initial page load)
      tags$script(
        src = "https://cdn.datatables.net/1.13.4/js/jquery.dataTables.min.js"
      ),

      # Ensure card contents do not clip the dropdown
      tags$style('.card { overflow: visible !important; }'),
      tags$style('.card-body { overflow: visible !important; }'),

      # Limit select input size inside the card
      tags$style(
        '.selectize-input { 
            white-space: nowrap; 
            overflow: hidden; 
            text-overflow: ellipsis; 
            max-width: 100%; 
            width: 100%; 
          }'
      ),

      # Allow dropdown menu to overflow the card
      tags$style(
        '.selectize-dropdown { 
            position: absolute !important; 
            z-index: 1050 !important; 
            max-width: none !important; 
          }'
      ),

      # Set minimal card height
      tags$style(
        '.card {
            min-height: 150px; /* Adjust the height as needed */
          }'
      ),

      # Style the data tables
      tags$style(
        HTML(
          "
            table.dataTable {
              table-layout: auto !important; /* Use auto layout so that columns can expand */
              width: 100% !important;
            }
    
            table.dataTable th,
            table.dataTable td {
              text-align: center;
              vertical-align: middle;
              border: 1px solid #ddd !important;
              white-space: nowrap; /* Prevent text from wrapping */
              padding: 8px;
            }
    
            table.dataTable tbody tr:nth-child(even) {
              background-color: #f9f9f9;
            }
          "
        )
      ),

      # JavaScript to dynamically adjust overflow based on table width
      tags$script(
        HTML(
          "
            function adjustDataTableOverflow() {
              $('.dataTables_wrapper').each(function() {
                var containerWidth = $(this).outerWidth();
                var tableWidth = $(this).find('table').outerWidth();
                if (tableWidth > containerWidth) {
                  $(this).css('overflow-x', 'auto');
                } else {
                  $(this).css('overflow-x', 'visible');
                }
              });
            }
        
            // Ensure the function runs after tables are rendered
            $(document).on('shiny:connected', function() {
              setTimeout(adjustDataTableOverflow, 500);
            });
        
            $(document).on('shiny:value', function() {
              setTimeout(adjustDataTableOverflow, 500);
            });
        
            // Also adjust on window resize
            $(window).on('resize', function() {
              adjustDataTableOverflow();
            });
        
            // Observe table changes and apply fix dynamically
            const observer = new MutationObserver(() => adjustDataTableOverflow());
            observer.observe(document.body, { childList: true, subtree: true });
        "
        )
      )
    ),

    fluidRow(
      column(
        3,
        bslib::card(
          sliderInput(
            "year",
            "Jaar",
            min = min(years),
            max = max(years),
            value = c(min(years), max(years)),
            step = 1,
            sep = ""
          )
        )
      ),
      column(
        3,
        bslib::card(
          selectInput(
            "area",
            "Gebied",
            choices = unique(data$Gebied),
            selected = "Almelo",
            multiple = TRUE
          )
        )
      ),
      column(
        3,
        bslib::card(
          selectInput(
            "sector",
            "Sector",
            choices = unique(data$Type),
            selected = "Totaal"
          )
        )
      ),
      column(
        3,
        bslib::card(
          selectInput(
            "dienstverband",
            "Dienstverband",
            choices = c("Fulltime", "Parttime", "Alle"),
            selected = "Alle"
          )
        )
      )
    ),

    # Dynamic plot title
    h3(textOutput("plot_title")),
    uiOutput("plot"),

    # Table
    uiOutput("table_ui")
  )
)

#### 3 Server ####

server <- function(input, output, session) {
  # Reactive value for display in titles
  dienstverband_title_text <- reactiveVal("werkzame personen")
  observeEvent(input$dienstverband, {
    dienstverband_title_text(
      switch(
        input$dienstverband,
        "Fulltime" = "fulltimers",
        "Parttime" = "parttimers",
        "Alle" = "werkzame personen"
      )
    )
  })

  # Reactive value for selecting the y_col in the data
  y_col <- reactiveVal("Werkzame_personen_totaal")
  observeEvent(input$dienstverband, {
    y_col(
      switch(
        input$dienstverband,
        "Fulltime" = "Werkzame_personen_fulltime",
        "Parttime" = "Werkzame_personen_parttime",
        "Alle" = "Werkzame_personen_totaal"
      )
    )
  })

  # Reactive expression for filtered data based on input values.
  filtered_data <- reactive({
    req(input$year, input$area, input$sector)
    data %>%
      filter(
        Jaar >= input$year[1] & Jaar <= input$year[2],
        Gebied %in% input$area,
        Type %in% input$sector
      )
  })

  # Reactive expression: compute the indexed data based on the filtered data.
  indexed_data <- reactive({
    filtered_data() %>%
      arrange(Gebied, Jaar) %>%
      group_by(Gebied) %>%
      mutate(
        first_value = first(.data[[y_col()]][!is.na(.data[[y_col()]])]),
        indexed_value = ifelse(
          is.na(first_value) | first_value == 0,
          NA,
          ((.data[[y_col()]] - first_value) / abs(first_value)) * 100 + 100
        )
      ) %>%
      ungroup()
  })

  output$plot_title <- renderText({
    paste0(
      "Ontwikkeling aantal ",
      dienstverband_title_text(),
      " (",
      min(input$year),
      " - ",
      max(input$year),
      ")"
    )
  })

  output$plot <- renderUI({
    if (length(input$area) == 0) {
      return(p("Selecteer minimaal één gebied."))
    }

    renderPlotly({
      p <- ggplot(
        indexed_data(),
        aes(
          x = Jaar,
          y = indexed_value,
          color = Gebied,
          group = Gebied,
          text = paste0(
            "Jaar: ",
            Jaar,
            "<br>Index: ",
            round(indexed_value, 1),
            "<br>Aantal: ",
            .data[[y_col()]]
          )
        )
      ) +
        geom_line() +
        geom_point() +
        labs(y = "Indexcijfer") +
        theme_minimal()

      ggplotly(p, tooltip = "text")
    })
  })

  output$table_ui <- renderUI({
    if (length(input$area) == 0) {
      return(NULL)
    }

    req(indexed_data())
    req(filtered_data())

    list(
      h4(textOutput("table_title")),
      DT::dataTableOutput("main_table"),
      h4(textOutput("index_table_title")),
      DT::dataTableOutput("index_table")
    )
  })

  output$table_title <- renderText({
    paste0("Aantal ", dienstverband_title_text())
  })

  output$index_table_title <- renderText({
    paste0("Aantal ", dienstverband_title_text(), " (index)")
  })

  output$main_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data() %>%
        group_by(Jaar, Gebied, Type) %>%
        rename(Sector = Type) %>%
        summarise(value = first(.data[[y_col()]]), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = Jaar, values_from = value) %>%
        mutate(across(where(is.numeric), ~format(., decimal.mark = ","))) %>%
        arrange(Gebied, Sector),
      options = list(
        dom = 't', # Table only
        paging = FALSE,
        searching = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })

  output$index_table <- DT::renderDataTable({
    DT::datatable(
      indexed_data() %>%
        select(Jaar, Gebied, Type, indexed_value) %>%
        rename(Sector = Type) %>%
        mutate(
          indexed_value = format(round(indexed_value, 2), decimal.mark = ",")
        ) %>%
        tidyr::pivot_wider(names_from = Jaar, values_from = indexed_value) %>%
        arrange(Gebied, Sector),
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
}

#### 4 Run app ####

shinyApp(ui, server)
