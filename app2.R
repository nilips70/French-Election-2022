library(leaflet)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(plotly)
library(shinyBS)
library(stringr)
library(shinythemes)
library(shinycssloaders)
library(purrr)

######################## reading datasets #######################

df_merged <- readRDS("df_merged.rds")
deps <- readRDS("departments.rds")
coords <- readRDS("geographic_information.rds") %>% rename(nom = DEP_NAME)

######################## Data preparation #######################

# Creating the votes dataset
votes = df_merged %>% 
  ungroup() %>% 
  select(nom:`DUPONT-AIGNAN`) %>% 
  pivot_longer(names_to = 'candidate', values_to = 'vote', -nom) %>% 
  group_by(nom) %>% 
  mutate(perc_vote = floor(vote/sum(vote)*100),
         winner = candidate[which.max(vote)],
         winner_vote = max(perc_vote)) %>% 
  ungroup()

# Creating departments socio-economic data
dep_features = df_merged %>% select(nom, white_collar_rate:geometry,life_expectancy,im_rate)

vote_dep_features = left_join(votes %>% filter(candidate == "MACRON"), dep_features) %>% #votes %>% filter(candidate == "MACRON")
  mutate(across(c(white_collar_rate, unemployment_rate, higher_education_rate), ~as.numeric(.))) %>% 
  st_as_sf(.)


###########################starting the ui section ######################
ui <- fluidPage(
  
#aesthetic 
theme = shinytheme("flatly"),

navbarPage("Exploring French Election Data 2022", theme = shinytheme("superhero"),
             tabPanel("Departments",
                      sidebarLayout(
                        column(width = 4,
                               fluidRow(selectInput(
                                   "candidate", "Choose a candidate:",
                                   c("Marine LE PEN"  = "LE PEN" ,
                                     "Jean-Luc MÉLENCHON" = "MÉLENCHON"   ,
                                     "Emmanuel MACRON"   = "MACRON"   ,
                                     "Nicolas DUPONT-AIGNAN" = "DUPONT-AIGNAN",
                                     "Jean LASSALLE" = "LASSALLE"     ,
                                     "Philippe POUTOU" =  "POUTOU",
                                     "Éric Zemmour" = "ZEMMOUR",
                                     "Valérie Pécresse" = "PÉCRESSE",
                                     "Yannick Jadot" = "JADOT",
                                     "Fabien Roussel" = "ROUSSEL",
                                     "Anne Hidalgo" = "HIDALGO",
                                     "Nathalie Arthaud" = "ARTHAUD")),
                                 plotlyOutput("view2")
                               ),
                        ),
                        mainPanel(
                          leafletOutput("view", height = 500) %>% withSpinner(color = "#1E90FF")
                        )
                      )
             ),
            tabPanel("Scatter Plots",
                      sidebarLayout(
                        column(width = 2,
                               fluidRow(selectInput(
                                   "candidate_scatter_plot", "Choose a candidate:",
                                   c("Marine LE PEN"  = "LE PEN" ,
                                     "Jean-Luc MÉLENCHON" = "MÉLENCHON"   ,
                                     "Emmanuel MACRON"   = "MACRON"   ,
                                     "Nicolas DUPONT-AIGNAN" = "DUPONT-AIGNAN",
                                     "Jean LASSALLE" = "LASSALLE"     ,
                                     "Philippe POUTOU" =  "POUTOU",
                                     "Éric Zemmour" = "ZEMMOUR",
                                     "Valérie Pécresse" = "PÉCRESSE",
                                     "Yannick Jadot" = "JADOT",
                                     "Fabien Roussel" = "ROUSSEL",
                                     "Anne Hidalgo" = "HIDALGO",
                                     "Nathalie Arthaud" = "ARTHAUD")
                                 ),
                                 
                               ),
                        ),
                        mainPanel(
                          column(width = 6,
                                 plotlyOutput("view_scatter_unemployment") %>% withSpinner(color = "#1E90FF"),
                                 hr()
                          ),
                         
                          column(width = 6,
                                 plotlyOutput("view_scatter_immigrants") %>% withSpinner(color = "#1E90FF"),
                                 hr()
                          ),
                          
                          column(width = 6,
                                 plotlyOutput("view_scatter_poverty") %>% withSpinner(color = "#1E90FF"),
                                 hr()
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_life") %>% withSpinner(color = "#1E90FF"),
                                 hr()
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_whitecollar") %>% withSpinner(color = "#1E90FF")
                                 
                          ),
                          column(width = 6,
                                 plotlyOutput("view_scatter_education") %>% withSpinner(color = "#1E90FF")
                          )
                        )
                       )
              )
   )
   
   
 )


######################### starting the server section #########################
server <- function(input, output) {
  

   ################starting reactive part department map ################
   output$view <- renderLeaflet({ #pay attention to what is the type of plot (main plot)
       
   #merging and mapping
   df_color <- vote_dep_features %>% mutate(percent_vote = floor(perc_vote * 100)) #fixing the color palette for each candidate
   df_map <- vote_dep_features %>% filter(candidate == input$candidate) %>%  #user's input for example: "MACRON" instead of input$candidate
         mutate(nom = as.factor(nom))
       
       
   df_map$percent_vote <- floor(df_map$perc_vote * 100) #rounding the percents to be shown on map
       
       
   #defining pallettes
   pal_votes <- colorBin("magma", vote_dep_features$perc_vote, 8, pretty = T)
       
   pal_winner <- colorFactor(palette = c("#A80500", "#E5002c", "#180067" ,"#0087cc", "#e32759",
                                             "#76b424", "#0b4e99", "#27456d", "#FFD600", "#e33c40", "#bd0d1f", '#0443b1'), 
                                 levels = c("ARTHAUD", "ROUSSEL", "ZEMMOUR", "DUPONT-AIGNAN", "HIDALGO", 
                                            "JADOT", "LASSALLE", "LE PEN", "MACRON", "MÉLENCHON", "POUTOU", 'PÉCRESSE'))
       
   pal_unemp <- colorBin("inferno", vote_dep_features$unemployment_rate, 4, pretty = T)
   pal_imm <- colorBin("inferno", vote_dep_features$im_rate, 8, pretty = T)
   pal_pov <- colorBin("inferno", vote_dep_features$poverty_rate, 8, pretty = T)
   pal_life <- colorBin("inferno", vote_dep_features$life_expectancy, 3, pretty = T)
   pal_edu <- colorBin("inferno", vote_dep_features$higher_education_rate, 4, pretty = T)
   pal_white <- colorBin("inferno", vote_dep_features$white_collar_rate, 4, pretty = T)
   pal_pop <- colorBin("viridis", domain = vote_dep_features$population,10, pretty = T)
       
       
       
   #making popups on the map (paste0 can get both variables and words)
   df4 <- vote_dep_features %>% 
      mutate(pop1 = paste0(vote_dep_features$perc_vote,"% at ", vote_dep_features$nom),
             pop2 = paste0(vote_dep_features$nom," population: ", vote_dep_features$population),
             pop3= paste0(vote_dep_features$winner, " is the first selected candidate at ", vote_dep_features$nom),
             #pop4 = paste0(round(df_merged$firstcand_dep * 100, 1), "% at ", df_merged$nom),
             pop5 = paste0("Unemployment Rate at ", vote_dep_features$nom, ": ", vote_dep_features$unemployment_rate),
             pop6 = paste0("Immigration Rate at " ,vote_dep_features$nom, ": ", vote_dep_features$im_rate),
             pop8 = paste0("Poverty Rate at ", vote_dep_features$nom, ": ", vote_dep_features$poverty_rate),
             pop9= paste0("Avg. Life Expectancy at ", vote_dep_features$nom, ": ", vote_dep_features$life_expectancy),
             pop10 = paste0("Higher Education Rate at ", vote_dep_features$nom, ": ", vote_dep_features$higher_education_rate),
             pop11 = paste0("White Colar Rate at ", vote_dep_features$nom, ": ", vote_dep_features$white_collar_rate))
    
    
popup_sb <- df4$pop1

leaflet() %>%
      addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
      addPolygons(data = vote_dep_features, fillColor = ~pal_votes(vote_dep_features$perc_vote), layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Votes",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_winner(vote_dep_features$winner), #layerId= ~nom2,
                  fillOpacity = 0.7,
                  group = "First Candidate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_unemp(vote_dep_features$unemployment_rate) , #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Unemployment Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop5,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_imm(vote_dep_features$im_rate), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Immigration Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop6,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_pov(vote_dep_features$poverty_rate), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Poverty Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop8,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_life(vote_dep_features$life_expectancy), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Avg. Life Expectancy",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop9,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_edu(vote_dep_features$higher_education_rate), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "Higher Education Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop10,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_white(vote_dep_features$white_collar_rate), #layerId= ~nom,
                  fillOpacity = 0.7,
                  group = "White Collar Rate",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop11,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
addPolygons(data = vote_dep_features, fillColor = ~pal_pop(vote_dep_features$population), #layerId= ~nom2,
                  fillOpacity = 0.7,
                  group = "Population",
                  weight = 0.2,
                  smoothFactor = 0.2,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.2,
                    bringToFront = TRUE),
                  label=df4$pop2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))  %>% 
addLegend(pal = pal_votes, group = "Votes" , values = vote_dep_features$perc_vote, title = "Vote %", opacity = 0.7,
          labFormat = labelFormat(suffix = " %")) %>%
addLegend(pal = pal_unemp, group = "Unemployment Rate" , values = vote_dep_features$unemployment_rate, title = "Unemployment Rate %", opacity = 0.7,
          labFormat = labelFormat(suffix = " %")) %>%
addLegend(pal = pal_winner, group = "First Candidate" , values = vote_dep_features$winner, title = "Candidate Name" ,opacity = 0.7) %>%
addLegend(pal = pal_imm, group = "Immigration Rate" , values = vote_dep_features$im_rate, title = "Immigration Rate" ,opacity = 0.7) %>%
addLegend(pal = pal_pov, group = "Poverty Rate" , values = vote_dep_features$poverty_rate, title = "Poverty Rate %" ,opacity = 0.7, labFormat = labelFormat(suffix = " %")) %>%
addLegend(pal = pal_life, group = "Avg. Life Expectancy" , values = vote_dep_features$life_expectancy, title = "Avg. Life Expectancy" ,opacity = 0.7) %>%
addLegend(pal = pal_edu, group = "Higher Education Rate" , values = vote_dep_features$higher_education_rate, title = "Higher Education Rate %", opacity = 0.7,
          labFormat = labelFormat(suffix = " %")) %>%
addLegend(pal = pal_white, group = "White Collar Rate" , values = vote_dep_features$white_collar_rate, title = "White Collar Rate %" ,opacity = 0.7, labFormat = labelFormat(suffix = " %")) %>% 
addLayersControl(
        overlayGroups = c("Votes", "First Candidate", "Unemployment Rate", "Immigration Rate", "Poverty Rate", "Avg. Life Expectancy",
                          "Higher Education Rate", "White Collar Rate", "Population"),
        options = layersControlOptions(collapsed = T),
        position = "bottomleft"
      ) %>% hideGroup(c("First Candidate", "Unemployment Rate", "Immigration Rate", "Poverty Rate", "Avg. Life Expectancy", 
                        "Higher Education Rate", "White Collar Rate", "Population")) 
    
})
##################end of reactive part 1 
  
################starting reactive part 2  

#bar chart for dep 1
output$view2 <- renderPlotly({

    
    req(!is.null(input$view_shape_click$id))
    location <- input$view_shape_click$id #yani jayi ke karbar rush click mikone tu naghshe
    
    
    plot1 <- votes %>% filter(nom == location) %>% 
      ggplot(aes(x = reorder(candidate, -perc_vote), y = perc_vote, fill = candidate, 
                 text = perc_vote)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = c("#A80500", "#E5002c", "#180067" ,"#0087cc", "#e32759",
                                   "#76b424", "#0b4e99", "#27456d", "#FFD600", "#e33c40", "#bd0d1f", '#0443b1'))+
      theme_minimal() +
      ggtitle(paste0("Candidate Preference in ", location))+
      theme(axis.text.x = element_text(size = 7, face = "bold", angle = 45, margin = margin(t = 6)),
            legend.position = "none") +
      labs(x="Candidate", y="Vote %")
    
    ggplotly(plot1, tooltip = "text")
    
  }) %>% bindEvent(input$view_shape_click)
  
##########################
df_merged <- reactive({ 
    

    
df_map <- vote_dep_features %>% filter(candidate == input$candidate_scatter_plot) %>%  #user's input for example: "MACRON" instead of input$candidate
      mutate(nom = as.factor(nom))
    
    
    df_merged <- vote_dep_features
    
    return(df_merged)
    
  })
    
  
  
  
output$view_scatter_unemployment <- renderPlotly({ #Vote vs unemployment
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=unemployment_rate, y=perc_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Unemployment Rate") ) +
      labs(x="Unemployment Rate %", y="Vote %")
    
    ggplotly(plot1, tooltip = "text")
    
  }) 
  
  
  
  output$view_scatter_immigrants <- renderPlotly({ #Vote vs immigrants
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=im_rate, y=perc_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Immigration Rate") ) +
      labs(x="Immigration Rate %", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
    
  }) 
    
  output$view_scatter_poverty <- renderPlotly({ #Vote vs poverty
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=poverty_rate, y=perc_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Poverty Rate %") ) +
      labs(x="Poverty Rate %", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
    
    
    
  })  
  
  output$view_scatter_life <- renderPlotly({ #Vote vs life expectancy
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=life_expectancy, y=perc_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Life Expectancy") ) +
      labs(x="Life Expectancy", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
  })
  
  output$view_scatter_education <- renderPlotly({ #Vote vs Higher Education
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=higher_education_rate, y=perc_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - Higher Education Rate") ) +
      labs(x="Higher Education %", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
  })  
  output$view_scatter_whitecollar <- renderPlotly({ #Vote vs White Collar
    
    plot1 <- df_merged() %>% 
      ggplot(aes(x=white_collar_rate, y=perc_vote)) + geom_point(aes(text = nom)) + geom_smooth(method = "lm")+
      theme_minimal() +
      ggtitle(paste0("Vote - White Collar Rate") ) +
      labs(x="White Collar%", y="Vote %")
    
    
    ggplotly(plot1, tooltip = "text")
  })   
}#end of server

# Run the application
shinyApp(ui = ui, server = server)
    
    
    
    
    
    
    
    