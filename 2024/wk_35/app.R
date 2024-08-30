# Load libraries
pacman::p_load(tidyverse, shiny, reactable, shiny.semantic, semantic.dashboard, rsconnect)

# load data
tuesdata <- tidytuesdayR::tt_load(2024, week = 35)

power_rangers_episodes <- tuesdata$power_rangers_episodes
power_rangers_seasons <- tuesdata$power_rangers_seasons

# Data Wrangling
season_num <- power_rangers_seasons |>
  select(season_title, season_num)

# Join the data sets
pr_episodes_df <- power_rangers_episodes |>
  left_join(season_num, by = "season_title") |>
  mutate(Abbreviation = paste0("SN", season_num, ":EP", episode_num),
         season_title = str_remove(season_title, "\\s*\\([^)]*\\)")) |>
  select(-all_of(c("season_num", "episode_num")))

# Extract unique years for drop down
years <- unique(format(as.Date(pr_episodes_df$air_date), "%Y"))

# Helper functions
generate_top_rated <- function(abbr, season_title, episode_title, date) {
  paste0(
    '<div style="position: relative; text-align: left; font-size: 14px;">',
    '<div style="position: absolute; top: 0; right: 0; color: #D1D1D1;">', date, '</div>',
    '<div style="font-weight: bold; margin-bottom: 2px;">',
    '<span style="display: inline-block; margin-right: 0.25rem; padding: 0 0.25rem; border: 1px solid hsl(0, 0%, 75%); border-radius: 2px;">', abbr, '</span>',
    '</div>',
    '<div style="color: blue; margin-bottom: 2px;font-size: 17px;">', season_title, '</div>',
    '<div style="color: grey; font-style: italic; margin-left: 5px;font-size: 15x;">', episode_title, '</div>',
    '</div>'
  )
}

generate_stars <- function(IMDB_rating, total_votes) {
  total_stars <- 10
  full_stars <- floor(IMDB_rating)
  partial_star_ratio <- IMDB_rating - full_stars
  
  stars <- sapply(1:total_stars, function(i) {
    if (i <= full_stars) {
      return('<svg width="20" height="20" viewBox="0 0 24 24"><path d="M12 2l2.09 6.26L21 9l-5 4.47L17.18 20 12 16.12 6.82 20 7 13.47 2 9l6.91-.74L12 2z" fill="gold"/></svg>')
    } else if (i == full_stars + 1 && partial_star_ratio > 0) {
      return(paste0(
        '<svg width="16" height="16" viewBox="0 0 24 24"><defs><linearGradient id="grad', i, '" x1="0%" y1="0%" x2="100%" y2="0%"><stop offset="0%" stop-color="gold"/><stop offset="', 
        partial_star_ratio * 100, '%" stop-color="gold"/><stop offset="',
        partial_star_ratio * 100, '%" stop-color="lightgray"/><stop offset="100%" stop-color="lightgray"/></linearGradient></defs><path d="M12 2l2.09 6.26L21 9l-5 4.47L17.18 20 12 16.12 6.82 20 7 13.47 2 9l6.91-.74L12 2z" fill="url(#grad', i, ')"/></svg>'
      ))
    } else {
      return('<svg width="20" height="20" viewBox="0 0 24 24"><path d="M12 2l2.09 6.26L21 9l-5 4.47L17.18 20 12 16.12 6.82 20 7 13.47 2 9l6.91-.74L12 2z" fill="lightgray"/></svg>')
    }
  })
  
  paste0(
    '<div style="text-align: center;">',
    sprintf('<div style="font-size: 20px; font-weight: bold; margin-bottom: 4px;">%.1f</div>', IMDB_rating),
    paste(stars, collapse = ""),
    sprintf('<div style="font-size: 11px; font-weight: bold; margin-top: 6px;">%d votes</div>', total_votes),
    '</div>'
  )
}

pr_episodes_df$Top_Rated <- mapply(generate_top_rated, pr_episodes_df$Abbreviation, pr_episodes_df$season_title, 
                                   pr_episodes_df$episode_title, pr_episodes_df$air_date, SIMPLIFY = FALSE)
pr_episodes_df$Rating_Display <- mapply(generate_stars, pr_episodes_df$IMDB_rating, pr_episodes_df$total_votes, SIMPLIFY = FALSE)

# UI
ui <- dashboardPage(
  dashboardHeader(
    title = div(
      style = "text-align: center; width: 100%; font-weight: bold; font-size: 22px;",
      "Mighty Morphin Power Rangers"
    ),
    tags$p(
      style = "text-align: center; font-size: 14px; color: #14279B;",
      "Graphic: Manasseh Oduor"
    ),
    titleWidth = "wide"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("IMDb Top-Rated Episodes (Latest Rankings)", icon = icon("star"), 
               href = "https://www.imdb.com/title/tt0106064/episodes/?topRated=DESC")
    ),
    width = 250
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @media (max-width: 768px) {
          .main-header .navbar {
            margin-left: 0 !important;
          }
          .main-sidebar {
            width: 50px !important;
          }
          .main-sidebar .sidebar-menu > li > a > .fa {
            font-size: 18px;
            margin-right: 0;
          }
          .main-sidebar .sidebar-menu > li > a > span {
            display: none;
          }
        }
        
        /* Custom styles for the reactable table */
        .custom-table .rt-thead .rt-th {
          /* Increase header font size */
          font-size: 15px; 
        }
        
        .custom-table .rt-tbody .rt-td {
          /* Increase content font size */
          font-size: 14px;
        }
      "))
    ),
    tags$footer(
      tags$div(
        style = "text-align: center; padding: 10px; font-size: 12px; color: grey;",
        "Data source: Kaggle's Power Rangers Dataset! provided by Tinashe M. Tapera  #TidyTuesday Week:35, 2024"
      )
    ),
    tabItems(
      tabItem(tabName = "Filter_Ops",
              fluidRow(
                box(width = 12,
                    title = "Filter Options",
                    div(class = "ui grid",
                        div(class = "two wide column",
                            selectInput("year", "YEAR", choices = c("All", sort(years, decreasing = TRUE)), selected = "All")
                        ),
                        div(class = "four wide column",
                            textInput("search_titles", "SEARCH TITLE")
                        ),
                        div(class = "six wide column no-ticks",
                            sliderInput("IMDB_rating", "RATING", min = 0, max = 10, value = 5, step = 0.1, ticks = FALSE)
                        ),
                        div(class = "four wide column",
                            uiOutput("rating_label")
                        )
                    )
                ),
                box(width = 12,
                    reactableOutput("table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Filter based on the input filters
  filtered_data <- reactive({
    data_filtered <- pr_episodes_df
    
    # Filter by year if selected
    if (input$year != "All") {
      data_filtered <- data_filtered[format(as.Date(data_filtered$air_date), "%Y") == input$year, ]
    }
    
    # Filter by search term in Season and Episode titles
    if (input$search_titles != "") {
      data_filtered <- data_filtered[
        grepl(input$search_titles, data_filtered$season_title, ignore.case = TRUE) | 
          grepl(input$search_titles, data_filtered$episode_title, ignore.case = TRUE), 
      ]
    }
    
    # Filter by rating
    data_filtered <- data_filtered[data_filtered$IMDB_rating >= input$IMDB_rating, ]
    
    data_filtered
  })
  
  reordered_data <- reactive({
    df <- filtered_data()
    df <- df %>%
      select(Top_Rated, desc, Rating_Display, everything())
    df
  })
  
  output$table <- renderReactable({
    reactable(
      reordered_data(),
      columns = list(
        Top_Rated = colDef(name = "TOP RATED", html = TRUE),
        desc = colDef(name = "DESCRIPTION"),
        Rating_Display = colDef(html = TRUE, name = "RATING"),
        Abbreviation = colDef(show = FALSE),
        season_title = colDef(show = FALSE),
        episode_title = colDef(show = FALSE),
        air_date = colDef(show = FALSE),
        IMDB_rating = colDef(show = FALSE),
        total_votes = colDef(show = FALSE)
      ),
      compact = TRUE,
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      defaultSorted = list(Rating_Display = "desc"),
      highlight = TRUE,
      bordered = TRUE,
      class = "custom-table"
    )
  })
  
  output$rating_label <- renderUI({
    tagList(
      div(
        style = "margin-top: 11px; font-weight: bold;",
        HTML(paste(
          "<span style='color: #6EACDA;font-size: 16px;'>Rating: </span>",
          "<span style='color: blue; font-size: 20px;'>", input$IMDB_rating, "</span>",
          "<span style='color: #6EACDA;font-size: 14px;'> & Above</span>"
        ))
      )
    )
  })
  
}

shinyApp(ui, server)
