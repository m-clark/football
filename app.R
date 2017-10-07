
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

data_list = p(
strong("England"),":	Premiership & Divs 1,2,3 & Conference", br(),
strong("Scotland"),":	Premiership & Divs 1,2 & 3", br(),
strong("Germany"),":	Bundesligas 1 & 2", br(),
strong("Italy"),":	Serie A & B", br(),
strong("Spain"),":	La Liga (Premera & Segunda)", br(),
strong("France"),":	Le Championnat & Division 2", br(),
strong("Netherlands"),":	KPN Eredivisie", br(),
strong("Belgium"),":	Jupiler League", br(),
strong("Portugal"),":	Liga I", br(),
strong("Turkey"),":	Ligi 1", br(),
strong("Greece"),":	Ethniki Katigoria"
)
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
      menuItem("Game History", tabName = "game-history", icon = icon("calendar")),
      menuItem("All Time Tables", tabName = "all-time-tables", icon = icon("clock-o"))
    ),
    width='10%'
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
                p("The data are provided by the", span(class='pack', 'engsoccerdata'), "package and include the following from", a("http://www.football-data.co.uk/")),
                data_list,
                p("More details can be found at the author's ", a(href='https://github.com/jalapic/engsoccerdata', 'github page'), '.'),
                br(),
                br(),
                div(style='text-align:center',
                    img(src='img/Borussia_Dortmund.svg', width='15%'),
                    img(src='img/Sheffield_Wednesday.png', width='12%'),
                    img(src='img/Portland_Timbers_logo.svg.png', width='15%'),
                    img(src='img/Everton.svg', width='15%'),
                    img(src='img/FC_St._Pauli.svg', width='15%')))),


#• league-history ----------------------------------------------------------


      tabItem(tabName = "league-tables",
              fluidRow(
                sidebarPanel("Get League Tables",
                             selectInput("league_country",
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
                  selectInput("team_country",
                              "Choose a country:",
                              choices=unique(teamnames$country),
                              selected='England'),
                  uiOutput('Team'),
                  width=2
                ),
                column(
                  plotlyOutput('team_history'),
                  width=5
                )
              )
      ),


#• game-history ------------------------------------------------------------

      tabItem(tabName = "game-history",
              h3("Find a game."),
              fluidRow(
                sidebarPanel(
                  selectInput("game_team",
                              "Choose a team:",
                              choices=sort(unique(teamnames$name)),
                              selected='Everton'),
                  uiOutput('gamesUI'), width=2
                ),
                column(
                  DT::dataTableOutput('games'),
                  br(),
                  br(),
                  p(strong('Date'),': Date of match', br(),
                    strong('Season'),': Season of match - refers to starting year', br(),
                    strong('home'),': Home team', br(),
                    strong('visitor'),': Visiting team',
                    strong('FT'),': Full-time result', br(),
                    strong('hgoal'),': Goals scored by home team', br(),
                    strong('vgoal'),': Goals scored by visiting team', br(),
                    strong('division'),': Division: 1,2,3,4 or 3a (Old 3-North) or 3b (Old 3-South)', br(),
                    strong('tier'),': Tier of football pyramid: 1,2,3,4', br(),
                    strong('totgoal'),': Total goals in game', br(),
                    strong('goaldif'),': Goal difference in game home goals - visitor goals', br(),
                    strong('result'),': Result: H-Home Win, A-Away Win, D-Draw', br(),
                    strong('hgoal'),': Goals scored by home team', br(),
                    strong('vgoal'),': Goals scored by visiting team', br(),
                    strong('hconf'),': Conference of home team', br(),
                    strong('vconf'),': Conference of visiting team', br(),
                    strong('totgoal'),': Total goals in game', br(),
                    strong('round'),': Regular Season or Playoff round', br(),
                    strong('leg'),': leg of Playoff game', br(),
                    strong('hgoalaet'),': Goals scored by home team after extra time', br(),
                    strong('vgoalaet'),': Goals scored by visiting team after extra time', br(),
                    strong('hpen'),': Penalties scored by home team in shootout', br(),
                    strong('vpen'),': Penalties scored by visiting team in shootout'),
                  width=10
                )
              )
      ),


#• all-time tables ---------------------------------------------------------

      tabItem(tabName = "all-time-tables",
              h3("View all-time records for every team"),
              fluidRow(
                sidebarPanel(
                  selectInput("all_time_league",
                              "Choose a country/league:",
                              choices=sort(unique(all_leagues_all_time_records$league)),
                              selected='England'),
                  width=2
                ),
                column(
                  DT::dataTableOutput('all_time_tables', width='86.5%'),
                  width = 7
                )
              ))

    )
)
)


# Server ------------------------------------------------------------------



# Define server logic required to draw a histogram
server <- function(input, output) {


# League Tables ----------------------------------------------------------


  df_init = reactive({
    c0 = tolower(input$league_country)
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
    c0 = tolower(input$team_country)
    if (c0 == 'united states') c0 = 'mls'
    get(c0)
  })


  output$Team <- renderUI({
    df = df_init2()
    teams = sort(intersect(df$home, df$visitor))
    selectInput("team", "Select a team's history:", c('', teams), selected=NULL)
  })

  observeEvent(input$team_country, {
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

      output$team_history = renderPlotly({
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
      filter(home==input$game_team | visitor==input$game_team)
    selectInput("season", "Select a season:", c('', games$Season), selected=NULL)
  })

  observeEvent(input$season, {

    output$games = DT::renderDataTable({
      all_leagues %>%
        filter(home==input$game_team | visitor==input$game_team) %>%
        filter(Season == input$season) %>%
        arrange(desc(Date)) %>%
        datatable(extensions='Buttons',
                  rownames=F,
                  options=list(dom='Bt',
                               pageLength=nrow(.),
                               buttons = c('copy', 'csv'),
                               scrollX=T,
                               autoWidth = TRUE,
                               columnDefs = list(list(width = '75px', targets = c(1)),
                                                 list(width = '150px', targets = c(3,4)))
                  ),
                  width=1200
        )
  })
  })


# All time records --------------------------------------------------------
  # observeEvent(input$all_time_league, {

    output$all_time_tables = DT::renderDataTable({
      all_leagues_all_time_records %>%
        filter(league==input$all_time_league) %>%
        unnest %>%
        datatable(extensions='Buttons',
                  rownames=F,
                  options=list(dom='Bft',
                               pageLength=nrow(.),
                               buttons = c('copy', 'csv'),
                               scrollX=T,
                               autoWidth = TRUE,
                               columnDefs = list(list(width = '75px', targets = c(0)),
                                                 list(width = '200px', targets = c(1)))
                  )
                  )
    })

  # })


}

# Run the application
shinyApp(ui = ui, server = server)

