
# Preliminaries -----------------------------------------------------------


library(shiny); library(dplyr); library(tidyr); library(engsoccerdata);
library(DT); library(plotly); library(shinydashboard)
# odd dependency
# library(nlme)

teamnames = teamnames %>%
  mutate(country=forcats::fct_recode(country, Spain='spain', `United States`='MLS'),
         country = as.character(country)) %>%
  arrange(country)

mls$tier = 1  # only dataset that doesn't have tier for some reason

load('data/all_leagues.RData')

main_page_text = "This dashboard creates tables for a given country and year selected. A few countries will have multiple tiers available.  Beyond that, one can get a specific teams' historical first tier finishing position (assuming they were ever in the first tier), or league games for a specific season."

# UI ----------------------------------------------------------------------



# Define UI for application that draws a histogram
ui <- dashboardPage(skin='red',

  dashboardHeader(
    title="Historical Football Data",
    titleWidth='15%'
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "main", icon = icon("futbol-o")),
      menuItem("League Tables", tabName = "league-tables", icon = icon("trophy")),
      menuItem("Team History", tabName = "team-history", icon = icon("line-chart")),
      menuItem("Game History", tabName = "game-history", icon = icon("calendar"))
    ),
    width='15%'
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "standard_html.css")
    ),
    tabItems(


#• main tab ----------------------------------------------------------------


      tabItem(tabName = "main",
              box(
                p(main_page_text),
                br(),
                br(),
                p('The data are provided by the engsoccerdatapackage.'),
                br(),
                br(),
                div(style='text-align:center',
                    img(src='img/Borussia_Dortmund.svg', width='10%'),
                    img(src='img/Sheffield_Wednesday.png', width='8%'),
                    img(src='img/Portland_Timbers_logo.svg.png', width='10%'),
                    img(src='img/Everton.svg', width='10%'),
                    img(src='img/FC_St._Pauli.svg', width='10%')))),


#• league-history ----------------------------------------------------------


      tabItem(tabName = "league-tables",
              fluidRow(
                sidebarPanel("Get League Tables",
                             selectInput("country",
                                         "Choose a country:",
                                         choices=unique(teamnames$country),
                                         selected='England'),
                             uiOutput('Year'),
                             uiOutput('Tier'), width=2
                ),
                column(
                  dataTableOutput('league'),
                  width=5
                )

              )
      ),


#• team-history ------------------------------------------------------------


      tabItem(tabName = "team-history",
              h3("Historical First Tier Finishing"),
              fluidRow(
                sidebarPanel(
                  selectInput("country2",
                              "Choose a country:",
                              choices=unique(teamnames$country),
                              selected='England'),
                  uiOutput('Team'), width=2
                ),
                column(
                  plotlyOutput('team-history'),
                  width=5
                )
              )
      ),


#• game-history ------------------------------------------------------------

      tabItem(tabName = "game-history",
              h3("Find a game."),
              fluidRow(
                sidebarPanel(
                  selectInput("team2",
                              "Choose a team:",
                              choices=sort(unique(teamnames$name)),
                              selected='Everton'),
                  uiOutput('gamesUI'), width=2
                ),
                column(
                  dataTableOutput('games'),
                  p('Date: Date of match', br(),
                    'Season: Season of match - refers to starting year', br(),
                    'home: Home team', br(),
                    'visitor: Visiting team',
                    'FT: Full-time result', br(),
                    'hgoal: Goals scored by home team', br(),
                    'vgoal: Goals scored by visiting team', br(),
                    'division: Division: 1,2,3,4 or 3a (Old 3-North) or 3b (Old 3-South)', br(),
                    'tier: Tier of football pyramid: 1,2,3,4', br(),
                    'totgoal: Total goals in game', br(),
                    'goaldif: Goal difference in game home goals - visitor goals', br(),
                    'result: Result: H-Home Win, A-Away Win, D-Draw', br(),
                    'hgoal: Goals scored by home team', br(),
                    'vgoal: Goals scored by visiting team', br(),
                    'hconf: Conference of home team', br(),
                    'vconf: Conference of visiting team', br(),
                    'totgoal: Total goals in game', br(),
                    'round: Regular Season or Playoff round', br(),
                    'leg: leg of Playoff game', br(),
                    'hgoalaet: Goals scored by home team after extra time', br(),
                    'vgoalaet: Goals scored by visiting team after extra time', br(),
                    'hpen: Penalties scored by home team in shootout', br(),
                    'vpen: Penalties scored by visiting team in shootout'),
                  width=5
                )
              )
      )

    )
)
)


# Server ------------------------------------------------------------------



# Define server logic required to draw a histogram
server <- function(input, output) {


# League Tables ----------------------------------------------------------


  df_init = reactive({
    c0 = tolower(input$country)
    if (c0 == 'united states') c0 = 'mls'
    get(c0)
  })

  output$league = renderDataTable({
    df = df_init()
    maketable_all(df = df, Season=input$year, tier=input$tier) %>%
      mutate(Pos = strtoi(Pos)) %>%
      datatable(extensions='Buttons',
                rownames=F,
                options=list(dom='Bt',
                             pageLength=nrow(.),
                             buttons = c('copy', 'csv')))
  })

  output$Year <- renderUI({
    df = df_init()
    selectInput("year", "Season:", rev(sort(unique(as.integer(df$Season)))))
  })

  output$Tier <- renderUI({
    df = df_init()
    tier = unique(df$tier)
    if (is.null(tier)) tier = 1
    selectInput("tier", "Tier (if applicable):", tier)
  })



  # Team history ------------------------------------------------------------


  df_init2 = reactive({
    c0 = tolower(input$country2)
    if (c0 == 'united states') c0 = 'mls'
    get(c0)
  })


  output$Team <- renderUI({
    df = df_init2()
    teams = sort(intersect(df$home, df$visitor))
    selectInput("team", "Select a team's history:", c('', teams), selected=NULL)
  })

  observeEvent(input$country2, {
    shinyjs::reset('team')      # when changing country, this will avoid an error unlike validate and various other non-working approaches
  })


  observeEvent(input$team, {
    if (input$team != '') {
      withProgress(message = 'Generating data', detail = 'Processing...', value = 0, {
        incProgress(.1)

        all_seasons = df_init2() %>%
          group_by(Season) %>%
          do(tab=maketable_all(., tier=1))

        incProgress(.7)

        team = all_seasons %>%
          do(team = filter(.$tab, team==input$team),
             Season = .$Season) %>%
          unnest(Season) %>%
          unnest %>%
          mutate(Pos = as.integer(Pos))

        incProgress(.2)

      })

      output$team-history = renderPlotly({
        validate(
          need(try(nrow(team) != 0), "Sorry, your team sucks and was never in the first tier, so who cares what they did.")
        )
        team %>%
          plot_ly() %>%
          add_lines(x=~Season, y=~Pos, color=I('#ff5500')) %>%
          add_markers(x=~Season, y=~Pos,
                      color=~gd>0, size=~-Pos, text=~paste('GD:',gd),
                      showlegend=F, colors='Viridis') %>%
          layout(title = input$team,
                 yaxis = list(range=c(23,.5))) %>%
          layout(xaxis = list(zeroline=F,
                              showgrid=F),
                 yaxis = list(zeroline=F,
                              showgrid=F),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent') %>%
          config(showLink = F,
                 displaylogo = FALSE,
                 modeBarButtonsToRemove = list('pan2d'))
      })
    }
  })


# Game results ------------------------------------------------------------

  output$gamesUI <- renderUI({
    games = all_leagues %>%
      filter(home==input$team2 | visitor==input$team2)
    selectInput("season", "Select a season:", c('', games$Season), selected=NULL)
  })


  output$games = renderDataTable({
    all_leagues %>%
      filter(home==input$team2 | visitor==input$team2) %>%
      filter(Season == input$season) %>%
      datatable(extensions='Buttons',
                rownames=F,
                options=list(dom='Bt',
                             pageLength=nrow(.),
                             buttons = c('copy', 'csv')))
  })


}

# Run the application
shinyApp(ui = ui, server = server)

