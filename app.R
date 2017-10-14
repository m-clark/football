
# Preliminaries -----------------------------------------------------------

# devtools::install_github('jalapic/engsoccerdata')  # will not do anything if not necessary
library(shiny); library(shinydashboard); library(dplyr); library(tidyr);
library(engsoccerdata);
library(DT); library(plotly);
# odd dependency
# library(nlme)

teamnames = teamnames %>%
  mutate(country=forcats::fct_recode(country, Spain='spain', `United States`='MLS'),
         country = as.character(country)) %>%
  arrange(country)

mls$tier = 1  # only dataset that doesn't have tier for some reason

load('data/all_leagues.RData')


main_page_text = p("This site offers ", em('historical'), " data for a variety of football leagues, including the major European leagues and MLS.  One can create tables for a given country/league and year selected, with some leagues having multiple tiers available, and stretching back many decades (origin year in parenthesis).  Beyond that, one can get a specific teams' historical first tier finishing position (assuming they were ever in the first tier), league games for a specific season, all-time tables, and all-time head-to-head results (within a league).")

data_list = p(
  strong("Belgium"), ":	Jupiler League (1995).", br(),
  strong("England"), ":	Premiership & Divs 1,2,3 & Conference (1888)", br(),
  strong("France"), ":	Le Championnat & Division 2 (1932)", br(),
  strong("Germany"), ":	Bundesligas 1 & 2 (1963)", br(),
  strong("Greece"), ":	Ethniki Katigoria (1994)", br(),
  strong("Italy"), ":	Serie A & B (1929)", br(),
  strong("Netherlands"), ":	KPN Eredivisie (1956)", br(),
  strong("Portugal"), ":	Liga I (1994)", br(),
  strong("Scotland"), ":	Premiership & Divs 1,2 & 3 (1994)", br(),
  strong("Spain"), ":	La Liga Premera & Segunda (1928)", br(),
  strong("Turkey"), ":	Ligi 1 (1994)", br(),
  strong("United States"), ":	MLS (1996)"
)
# UI ----------------------------------------------------------------------



# Define UI for application that draws a histogram
ui <- dashboardPage(skin='red',

  dashboardHeader(
    title="Historical Football Data",
    titleWidth='250px'
    ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Main", tabName = "main", icon = icon("futbol-o")),
      menuItem("League Tables", tabName = "league-tables", icon = icon("trophy")),
      menuItem("Team History", tabName = "team-history", icon = icon("line-chart")),
      menuItem("Game History", tabName = "game-history", icon = icon("calendar")),
      menuItem("All Time Tables", tabName = "all-time-tables", icon = icon("clock-o")),
      menuItem("All Time Head-to-Head", tabName = "all-time-h2h", icon = icon("adjust"))
    ),
    width='250px' # if set to %, will not collapse properly
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
                p("The data are provided by the", span(class='pack', 'engsoccerdata'), "\npackage and include the following from", a(href="http://www.football-data.co.uk/", 'http://www.football-data.co.uk/')),
                data_list,
                p("More details can be found at the author's ", a(href='https://github.com/jalapic/engsoccerdata', 'GitHub page.'), 'For more and other types of data from the same data source, see the really nice shiny app', a(href='https://github.com/LyzandeR/FootballeR', 'here.')),
                br(),
                br(),
                div(style='text-align:center',
                    img(src='img/Borussia_Dortmund.svg', width='15%'),
                    img(src='img/Sheffield_Wednesday.png', width='12%'),
                    img(src='img/Portland_Timbers_logo.svg.png', width='15%'),
                    img(src='img/Everton.svg', width='15%'),
                    img(src='img/FC_St._Pauli.svg', width='15%')),
                style = "font-size:125%")
              ),


#• league-history ----------------------------------------------------------


      tabItem(tabName = "league-tables",
              h3('Historical League Tables'),
              br(),
              fluidRow(
                sidebarPanel(p('Start with a country, then select from the available seasons. Season year is the beginning year, e.g. 2013 is the 2013/2014 season.'),
                             br(),
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
              br(),
              fluidRow(
                sidebarPanel(
                  p('Select a country/team to see their first tier finishing position, for the seasons they were in the first tier.', strong('It will take a few seconds to process the data.')),
                  br(),
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
              h3("Find a game"),
              br(),
              fluidRow(
                sidebarPanel(
                  p('Select a season to view game results for any team.', br(), 'Leagues will have different seasons available.'),
                  selectInput("game_team",
                              "Choose a team:",
                              choices=sort(unique(teamnames$name)),
                              selected='Everton'),
                  uiOutput('gamesUI'),
                  width=2
                ),
                column(
                  DT::dataTableOutput('games', width='100%'),
                  br(),
                  br(),
                  p(strong('Date'),': Date of match', br(),
                    strong('Season'),': Season of match - refers to starting year', br(),
                    strong('home'),': Home team', br(),
                    strong('visitor'),': Visiting team',
                    strong('FT'),': Full-time result', br(),
                    strong('result'),': Result: H-Home Win, A-Away Win, D-Draw', br(),
                    strong('round'),': Regular Season or Playoff round', br(),
                    strong('division'),': Division: 1,2,3,4 or 3a (Old 3-North) or 3b (Old 3-South)', br()),

                  style = "font-size: 90%;", width=5
                )
              )
      ),

      # removed mostly useless columns
      # strong('hgoal'),': Goals scored by home team', br(),
      # strong('vgoal'),': Goals scored by visiting team', br(),
      # strong('tier'),': Tier of football pyramid: 1,2,3,4', br(),
      # strong('totgoal'),': Total goals in game', br(),
      # strong('goaldif'),': Goal difference in game home goals - visitor goals', br(),
      # strong('hgoal'),': Goals scored by home team', br(),
      # strong('vgoal'),': Goals scored by visiting team', br(),
      # strong('hconf'),': Conference of home team', br(),
      # strong('vconf'),': Conference of visiting team', br(),
      # strong('totgoal'),': Total goals in game', br(),
      # strong('leg'),': leg of Playoff game', br(),
      # strong('hgoalaet'),': Goals scored by home team after extra time', br(),
      # strong('vgoalaet'),': Goals scored by visiting team after extra time', br(),
      # strong('hpen'),': Penalties scored by home team in shootout', br(),
      # strong('vpen'),': Penalties scored by visiting team in shootout'),

#• all-time tables ---------------------------------------------------------

      tabItem(tabName = "all-time-tables",
              h3("View all-time records for every team"),
              br(),
              fluidRow(
                sidebarPanel(
                  selectInput("all_time_league",
                              "Choose a country/league:",
                              choices=sort(unique(all_leagues_all_time_records$league)),
                              selected='England'),
                  width=2
                ),
                column(
                  DT::dataTableOutput('all_time_tables'),
                  width = 5
                )
              )
              ),


#• H2H tables ---------------------------------------------------------

      tabItem(tabName = "all-time-h2h",
              h3("View all-time head to head matchups"),
              br(),
              fluidRow(
                sidebarPanel(
                  selectInput("h2h_country",
                              "Choose a country/league:",
                              choices=sort(unique(all_leagues_all_time_records$league)),
                              selected='England'),
                  uiOutput('h2h_team1'),
                  uiOutput('h2h_team2'),
                  width=2
                ),
                column(
                  DT::dataTableOutput('h2h_result'),
                  width = 4
                ),

                box(
                  h4('Head-to-head summary'),
                # column(
                  DT::dataTableOutput('h2h_summary', width='95%'),
                  # tableOutput('h2h_summary'),
                  # width = 4
                # ),
                width=5)

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
    c0 = tolower(input$league_country)
    if (c0 == 'united states') c0 = 'mls'
    get(c0)
  })

  output$league = renderDataTable({
    df = df_init()

    reds = RColorBrewer::brewer.pal(5, 'Reds')[2:4]
    blues = rev(RColorBrewer::brewer.pal(5, 'Blues'))[2:4]  # don't want darkest

    maketable_all(df = df, Season=input$year, tier=input$tier) %>%
      mutate(Pos = strtoi(Pos)) %>%
      mutate(color = NA,
             color = ifelse(Pos <= 3, 'top', color),
             color = ifelse(Pos >= nrow(.)-2, 'bot', color),
             color = paste0(color, c(1:3, rep(NA, nrow(.)-6), 1:3))
             ) %>%  # because styleEqual is dumber than styleInterval
      datatable(extensions='Buttons',
                rownames=F,
                options=list(dom='Bt',
                             pageLength=nrow(.),
                             buttons = c('copy', 'csv'),
                             columnDefs = list(list(visible=FALSE, targets=10)))) %>%
      formatStyle(
        target = 'row',
        columns = 'color',
        backgroundColor = styleEqual(c(paste0('top', 1:3), paste0('bot', 1:3)), c(blues, reds)),
        color = styleEqual(c(paste0('top', 1:3), paste0('bot', 1:3)), rep('white', 6)))

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

        if (input$team_country == 'England') {
          all_seasons = enland_season_tier_tables
        } else {
          all_seasons = df_init2() %>%
            group_by(Season, tier) %>%
            do(tab=maketable_all(.))
        }

        incProgress(.7)

        # note: Plotly will not respect integer/year and will put in inaccurate
        # decimal points; Note: tried lubridate same issue.; unbelievable
        # https://github.com/plotly/plotly.js/issues/135
        team = all_seasons %>%
          do(team = filter(.$tab, team==input$team),
             Season = .$Season,
             tier = .$tier) %>%
          unnest(Season, tier) %>%
          unnest %>%
          mutate_at(vars(Season, Pts, Pos), as.integer) %>%
          mutate(tier=factor(tier, levels=1:4))

        incProgress(.2)

      })

      output$team_history = renderPlotly({
        # validate(
        #   need(try(nrow(team) != 0), "Sorry, your team sucks and was never in the first tier, so who cares what they did.")
        # )
        # first of min ifelse fixes the 'bournemouth' problem, i.e. plotly year
        # formatting problem. among other arbitrary decisions plotly devs have
        # made,one is to not do stupid formatting if there is not at least five
        # units displayed
        minYear_xaxis = ifelse(nrow(team) < 3,
                               min(team$Season)-3,
                               min(team$Season)-1)
        maxYear_xaxis = ifelse(max(team$Season)>=lubridate::year(Sys.Date())-2,
                               max(team$Season)+1,
                               max(team$Season)+3)
        team %>%
          plot_ly() %>%
          add_lines(x=~Season, y=~Pos,
                    color=I('#ff5500'), text=NA, showlegend=F) %>%
          add_markers(x=~Season, y=~Pos,
                      color=~tier, size=~-Pos, text=~paste('GD:',gd),
                      showlegend=T, colors='Viridis', name='Tier') %>%
          layout(title = input$team,
                 xaxis = list(zeroline=F,
                              showgrid=F,
                              range=c(minYear_xaxis, maxYear_xaxis),
                              tick0 = minYear_xaxis,
                              # dtick = 1,
                              tickformat='####'),
                 yaxis = list(zeroline=F,
                              showgrid=F,
                              range=c(25,.5),
                              autorange = "reversed",
                              dtick=2,
                              tick0 = 1),
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

    select_yr = as.integer(lubridate::year(Sys.Date())-1)

    # this recursion seems to work best
    selectInput("season", "Select a season:", unique(games$Season), selected=input$season)
  })


  # currently shiny will ignore widths on first load
  observeEvent(input$season, {

    output$games = DT::renderDataTable({
      all_leagues %>%
        select(Date, Season, home, visitor, FT, result, round, division) %>%
        filter(home==input$game_team | visitor==input$game_team) %>%
        filter(Season == input$season) %>%
        select(-Season) %>%
        arrange(desc(Date)) %>%
        datatable(extensions='Buttons',
                  rownames=F,
                  options=list(dom='Bt',
                               pageLength=nrow(.),
                               buttons = c('copy', 'csv'),
                               scrollX=T,
                               autoWidth = F,
                               columnDefs = list(list(width = '15%', targets = 0),
                                                 list(width = '22.5%', targets = 1:2),
                                                 list(width = '10%', targets = 3:6),
                                                 list(class='dt-center', targets=c(0,3:6)))
                               )
        )
  })
  })


# All time records --------------------------------------------------------


    output$all_time_tables = DT::renderDataTable({
      all_leagues_all_time_records %>%
        filter(league==input$all_time_league) %>%
        select(-league) %>%
        unnest %>%
        datatable(extensions='Buttons',
                  rownames=F,
                  options=list(dom='Bft',
                               pageLength=nrow(.),
                               buttons = c('copy', 'csv'),
                               scrollX=T,
                               autoWidth = F,
                               columnDefs = list(list(width = '10%', targets = 1))
                  )
                  )
    })


# All time H2H ------------------------------------------------------------


  observeEvent(input$h2h_country, {
    df_init_h2h = reactive({
      c0 = tolower(input$h2h_country)
      if (c0 == 'united states') c0 = 'mls'
      get(c0)
    })

    df = df_init_h2h()
    nams = intersect(df$home, df$visitor)

    # set popular teams as default
    pop_teams = all_leagues_all_time_records %>%
      filter(league==input$h2h_country) %>%
      unnest %>%
      slice(1:2) %>%
      pull(team)

    output$h2h_team1 <- renderUI({
      selectInput("h2h_team1", "Team 1:",
                  choices=sort(nams),
                  selected=pop_teams[1])
    })

    output$h2h_team2 <- renderUI({
      selectInput("h2h_team2", "Team 2:",
                  choices=sort(nams),
                  selected=pop_teams[2])
  })

  # all results
  output$h2h_result = renderDataTable({
    games_between(df, teamname1=input$h2h_team1, teamname2=input$h2h_team2) %>%
      select(-Season) %>%
      arrange(Date) %>%
      datatable(extensions='Buttons',
                rownames=F,
                options=list(dom='Bt',
                             pageLength=nrow(.),
                             buttons = c('copy', 'csv'),
                             scrollX=T,
                             autoWidth = F,
                             columnDefs = list(list(width = '75px', targets = 0),
                                               list(width = '150px', targets = c(1,2)),
                                               list(width = '10px', targets = c(3,4)),
                                               list(className = 'dt-center', targets = c(0,3,4)))))
  })

  # summary
  observeEvent(input$h2h_team2, {
  output$h2h_summary = renderDataTable({
    games_between_sum(df, teamname1=input$h2h_team1, teamname2=input$h2h_team2) %>%
      select(-venue) %>%
      # pander::pander()
      filter(GD >= 0 | W == max(W)) %>%    # only show result in terms of the winner
      datatable(extensions='Buttons',
                rownames=F,
                options=list(dom='t',
                             ordering=F,
                             pageLength=nrow(.),
                             buttons = c('copy', 'csv'),
                             scrollX=T,
                             autoWidth = F,
                             columnDefs = list(list(width = '125px', targets = 0:1),
                                               list(width = '5px', targets = c(2:7)),
                                               list(className = 'dt-left', targets = c(0:1)),
                                               list(className = 'dt-center', targets = c(2:7))
                                               )
                             )) %>%
      formatStyle(
        target = 'row',
        columns = 0:7,
        backgroundColor = 'transparent')
  })

  })
  })


}

# Run the application
shinyApp(ui = ui, server = server)

