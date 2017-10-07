
library(shiny); library(dplyr); library(tidyr); library(engsoccerdata);
library(DT); library(plotly)
# odd dependency
# library(nlme)

teamnames = teamnames %>%
  mutate(country=forcats::fct_recode(country, Spain='spain', `United States`='MLS'),
         country = as.character(country)) %>%
  arrange(country)

mls$tier = 1  # only dataset that doesn't have tier for some reason


# Define UI
ui <- fluidPage(

  includeCSS("standard_html.css"),

   # Application title
   titlePanel("Soccer Data"),
   headerPanel(h5("The following creates tables for a given country and year selected. A few countries will have multiple tiers available.",
               br(),br(),
               'The data are provided by the engsoccerdatapackage.',br(),br())),

   # Sidebar
   fluidRow(
      sidebarPanel("Get League Tables",
         selectInput("country",
                     "Choose a country:",
                     choices=unique(teamnames$country),
                     selected='England'),
         uiOutput('Year'),
         uiOutput('Tier'),
         uiOutput('Team'), width=2
      ),

      column(
        dataTableOutput('data'),
        width=5
        ),
      column(
        plotlyOutput('team_history'),
        width=5
      )
   )
)


















# Define server logic
server <- function(input, output) {

  df_init = reactive({
    c0 = tolower(input$country)
    if (c0 == 'united states') c0 = 'mls'
    get(c0)
  })

  output$data = renderDataTable({
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

  output$Team <- renderUI({
    df = df_init()
    teams = sort(intersect(df$home, df$visitor))
    selectInput("team", "Select a team's history:", c('', teams), selected=NULL)
  })

  observeEvent(input$country, {
    shinyjs::reset('team')      # when changing country, this will avoid an error unlike validate and various other non-working approaches
  })


  observeEvent(input$team, {
    if (input$team != '') {
      withProgress(message = 'Generating data', detail = 'Processing...', value = 0, {
        incProgress(.1)

        all_seasons = df_init() %>%
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



}

# Run the application
shinyApp(ui = ui, server = server)

