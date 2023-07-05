library(tidyverse)
library(nflfastR)
library(reactable)
library(reactablefmtr)
library(shiny)
library(shiny)
library(shiny)
library(ggridges)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(reactable)
library(reactablefmtr)
library(tidyverse)
library(dplyr)
library(gtExtras)
library(rsconnect)
library(gtable)
library(gt)
library(magick)
library(ggplot2)
library(ggimage)

draftavg <- readr::read_csv("https://raw.githubusercontent.com/arjunmenon10/DraftThresholds/main/draftavgall.csv")

draftavg <- left_join(draftavg, teams_colors_logos, by = c('team' = 'team_abbr'))

draftGM <- readr::read_csv("https://raw.githubusercontent.com/arjunmenon10/DraftThresholds/main/draftavgGM.csv")

draftGM <- left_join(draftGM, teams_colors_logos, by = c('team' = 'team_abbr')) |> 
  mutate(GM = ifelse(GM == 'Mike Brown', 'Duke Tobin', GM))

draft3rd <- readr::read_csv("https://raw.githubusercontent.com/arjunmenon10/DraftThresholds/main/draftavg3rd.csv")

draft3rd <- left_join(draft3rd, teams_colors_logos, by = c('team' = 'team_abbr'))

teams <- unique(draft3rd$team)

titlelogo <- tags$a("By Arjun Menon", tags$img(src = "PFFlogo.png", height = 40))

ui <- fluidPage(
  theme = shinytheme("flatly"),

    titlePanel("NFL Draft Athletic Testing Team Averages"),

    # Sidebar with a slider input for number of bins 
    mainPanel(
      navbarPage(title = titlelogo,
                 tabPanel("By Position (2011-2022)",
                          fluidRow(
                            column(7, align = "center",
                                   selectInput(
                                     inputId =  "Position",
                                     label = "Position:",
                                     choices = c('QB', 'HB', 'WR', 'TE',
                                                 'T', 'G', 'C', 'DI', 'ED', 'LB',
                                                 'CB', 'S'),
                                     selected = 'WR'
                                   ),
                            ),
                            mainPanel(
                            reactableOutput("position_table"), width = 12)
                          )
                 ),
                 tabPanel('By GM Specific',
                          fluidRow(
                            column(7, align = "center",
                                   selectInput(
                                     inputId =  "PositionGM",
                                     label = "Position:",
                                     choices = c('QB', 'HB', 'WR', 'TE',
                                                 'T', 'G', 'C', 'DI', 'ED', 'LB',
                                                 'CB', 'S'),
                                     selected = 'WR'
                                   ),
                            ),
                            mainPanel(
                            reactableOutput("GM_table"), width = 12)
                          )
                 ),
                 tabPanel('Top 3 Rounds Only',
                          fluidRow(
                            column(7, align = "center",
                                   selectInput(
                                     inputId =  "Position3rd",
                                     label = "Position:",
                                     choices = c('QB', 'HB', 'WR', 'TE',
                                                 'T', 'G', 'C', 'DI', 'ED', 'LB',
                                                 'CB', 'S'),
                                     selected = 'WR'
                                   ),
                            ),
                            mainPanel(
                              reactableOutput("Top3_table"), width = 12)
                          )
                 ),
                 tabPanel('By Team',
                          fluidRow(
                            br(),
                            tags$p("Note: GMs with no completed drafts will have no table show up for them."),
                            column(7, align = "center",
                                   selectInput(
                                     inputId =  "Team",
                                     label = "Team:",
                                     choices = teams,
                                     selected = 'LAC'
                                   ),
                            ),
                            column(7, align = 'center',
                            radioButtons(
                              inputId = "GM",
                              label = "GM Specific",
                              choices = c('Yes', 'No'),
                              selected = 'No',
                            ),),
                            mainPanel(
                              reactableOutput("Teams_table"), width = 12)
                 ),
                 
      ),
      tabPanel('About',
               fluidRow(
                 tags$p("All of the averages on each tab are weighted by the Fitzgerald-Spielberger draft value of the pick where the 
                        player was taken. Players who were taken high in the draft have their athletic scores matter more than later in
                         the draft. If there is anything that looks off numbers wise, contact me @arjunmenon100 on Twitter, and I can 
                        fix it. For players testing data, combine data was used first, and if there were missing combine testing values, 
                        those were filled in using a player's pro day data. Finally, every tab but the GM specific tab and team specific tab 
                        (where GM specific button = yes) uses all years from 2011-2022, unless specified otherwise.")
               ))
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  position_data <- reactive({
    draftavg %>%
      filter(projected_position == input$Position) %>%
      select(team, team_logo_espn, projected_position, avgheight,
             avgweight, avgarm, avgbroad, avgfourty, avgten, avgthree, avgshuttle,
             avgvert)
  })

  output$position_table <- renderReactable({
    
    position_tbl <- position_data()
    
    reactable(position_tbl,
              compact = FALSE,
              pagination = FALSE,
              columns = 
                list(
                  team = colDef(name = "Team",
                                maxWidth = 60,
                                align = "left", sticky = 'left'),
                  team_logo_espn = colDef(name = "",
                                          maxWidth = 35,
                                          cell = embed_img(),
                                          align = "center", sticky = 'left'),
                   projected_position = colDef(name = "Position", maxWidth = 75, align = "center"),
                   avgheight = colDef(name = "Height (in)", maxWidth = 75, align = "center",
                                      cell = color_tiles(position_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                   avgweight = colDef(name = "Weight (lbs)", maxWidth = 80, align = "center",
                                      cell = color_tiles(position_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                   avgarm = colDef(name = "Arm Length", maxWidth = 75, align = "center",
                                   cell = color_tiles(position_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                   avgbroad = colDef(name = "Broad Jump", maxWidth = 75, align = "center",
                                     cell = color_tiles(position_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                   avgfourty = colDef(name = "40 Yard Dash", maxWidth = 75, align = "center",
                                      cell = color_tiles(position_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                   avgten = colDef(name = "10 Yard Split", maxWidth = 75, align = "center",
                                   cell = color_tiles(position_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                   avgthree = colDef(name = "Three Cone", maxWidth = 75, align = "center",
                                     cell = color_tiles(position_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                   avgshuttle = colDef(name = "Shuttle", maxWidth = 75, align = "center",
                                       cell = color_tiles(position_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                   avgvert = colDef(name = "Vertical", maxWidth = 75, align = "center",
                                   cell = color_tiles(position_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f")))
                ), fullWidth = TRUE)
  })
  
  GM_data <- reactive({
    draftGM %>%
      filter(projected_position == input$PositionGM) %>%
      select(team, GM, projected_position, avgheight,
             avgweight, avgarm, avgbroad, avgfourty, avgten, avgthree, avgshuttle,
             avgvert)
  })
  
  output$GM_table <- renderReactable({
    
    GM_tbl <- GM_data()
    
    reactable(GM_tbl,
              compact = FALSE,
              pagination = FALSE,
              columns = 
                list(
                  team = colDef(name = "Team",
                                maxWidth = 60,
                                align = "left", sticky = 'left'),
                  GM = colDef(name = "GM",
                                maxWidth = 100,
                                align = "left", sticky = 'left'),
                  projected_position = colDef(name = "Position", maxWidth = 75, align = "center"),
                  avgheight = colDef(name = "Height (in)", maxWidth = 75, align = "center",
                                     cell = color_tiles(GM_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgweight = colDef(name = "Weight (lbs)", maxWidth = 75, align = "center",
                                     cell = color_tiles(GM_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgarm = colDef(name = "Arm Length", maxWidth = 75, align = "center",
                                  cell = color_tiles(GM_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgbroad = colDef(name = "Broad Jump", maxWidth = 75, align = "center",
                                    cell = color_tiles(GM_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgfourty = colDef(name = "40 Yard Dash", maxWidth = 75, align = "center",
                                     cell = color_tiles(GM_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgten = colDef(name = "10 Yard Split", maxWidth = 75, align = "center",
                                  cell = color_tiles(GM_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgthree = colDef(name = "Three Cone", maxWidth = 75, align = "center",
                                    cell = color_tiles(GM_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgshuttle = colDef(name = "Shuttle", maxWidth = 75, align = "center",
                                      cell = color_tiles(GM_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgvert = colDef(name = "Vertical", maxWidth = 75, align = "center",
                                   cell = color_tiles(GM_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f")))
                ), fullWidth = TRUE)
  })
  
  
  Top3_data <- reactive({
    draft3rd %>%
      filter(projected_position == input$Position3rd) %>%
      select(team, team_logo_espn, projected_position, avgheight,
             avgweight, avgarm, avgbroad, avgfourty, avgten, avgthree, avgshuttle,
             avgvert)
  })
  
  output$Top3_table <- renderReactable({
    
    third_tbl <- Top3_data()
    
    reactable(third_tbl,
              compact = FALSE,
              pagination = FALSE,
              columns = 
                list(
                  team = colDef(name = "Team",
                                maxWidth = 60,
                                align = "left", sticky = 'left'),
                  team_logo_espn = colDef(name = "",
                                          maxWidth = 35,
                                          cell = embed_img(),
                                          align = "center", sticky = 'left'),
                  projected_position = colDef(name = "Position", maxWidth = 75, align = "center"),
                  avgheight = colDef(name = "Height (in)", maxWidth = 75, align = "center",
                                     cell = color_tiles(third_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgweight = colDef(name = "Weight (lbs)", maxWidth = 75, align = "center",
                                     cell = color_tiles(third_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgarm = colDef(name = "Arm Length", maxWidth = 75, align = "center",
                                  cell = color_tiles(third_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgbroad = colDef(name = "Broad Jump", maxWidth = 75, align = "center",
                                    cell = color_tiles(third_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                  avgfourty = colDef(name = "40 Yard Dash", maxWidth = 75, align = "center",
                                     cell = color_tiles(third_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgten = colDef(name = "10 Yard Split", maxWidth = 75, align = "center",
                                  cell = color_tiles(third_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgthree = colDef(name = "Three Cone", maxWidth = 75, align = "center",
                                    cell = color_tiles(third_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgshuttle = colDef(name = "Shuttle", maxWidth = 75, align = "center",
                                      cell = color_tiles(third_tbl, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                  avgvert = colDef(name = "Vertical", maxWidth = 75, align = "center",
                                   cell = color_tiles(third_tbl, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f")))
                ), fullWidth = TRUE)
  })
  
  Teams_data <- reactive({
    
    if(input$GM == "Yes"){
      draftGM %>%
        filter(team == input$Team) %>%
        select(team, GM, projected_position, avgheight,
               avgweight, avgarm, avgbroad, avgfourty, avgten, avgthree, avgshuttle,
               avgvert)
    }
    else{
      draftavg %>%
        filter(team == input$Team) %>%
        select(team, projected_position, avgheight,
               avgweight, avgarm, avgbroad, avgfourty, avgten, avgthree, avgshuttle,
               avgvert)
    }
    
  })
  
  output$Teams_table <- renderReactable({
    
    teams_tbl <- Teams_data()
    
    reactable(teams_tbl,
              compact = FALSE,
              pagination = FALSE,
              columns = 
                list(
                  team = colDef(name = "Team",
                                maxWidth = 60,
                                align = "left", sticky = 'left'),
                  projected_position = colDef(name = "Position", maxWidth = 75, align = "center"),
                  avgheight = colDef(name = "Height (in)", maxWidth = 75, align = "center"),
                  avgweight = colDef(name = "Weight (lbs)", maxWidth = 75, align = "center"),
                  avgarm = colDef(name = "Arm Length", maxWidth = 75, align = "center"),
                  avgbroad = colDef(name = "Broad Jump", maxWidth = 75, align = "center"),
                  avgfourty = colDef(name = "40 Yard Dash", maxWidth = 75, align = "center"),
                  avgten = colDef(name = "10 Yard Split", maxWidth = 75, align = "center"),
                  avgthree = colDef(name = "Three Cone", maxWidth = 75, align = "center"),
                  avgshuttle = colDef(name = "Shuttle", maxWidth = 75, align = "center"),
                  avgvert = colDef(name = "Vertical", maxWidth = 75, align = "center")
                ), fullWidth = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
