library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(leaflet)
library(data.table)
library(fmsb)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = HTML('<strong style="color:maroon;">CALCULATOR</strong>'),
                  titleWidth = 300,
                  dropdownMenu(type = "notifications", 
                               headerText = strong("HELP"), 
                               icon = icon("question"), 
                               badgeStatus = NULL,
                               notificationItem(
                                 text = ("Please be patient with the dashboard"),
                                 icon = icon("spinner")
                               ),
                               notificationItem(
                                 text = ("Student Name: Hitanshu Jain"),
                                 icon = icon("user")
                               ),
                               notificationItem(
                                 text = ("Student ID: 31337406"),
                                 icon = icon("user-circle")
                               ),
                               notificationItem(
                                 text = ("Last modified date: 22-06-2020"),
                                 icon = icon("clock")
                               )
                              
                  ),
                  tags$li(
                    a(
                      strong("OPEN IMDB.COM"),
                      height = 45,
                      href = "https://www.imdb.com/?ref_=nv_home",
                      title = "",
                      target = "_blank"
                    ),
                    class = "dropdown"
                  )
                  
  ),
  
  sidebar <- dashboardSidebar( width = 300,
                               sidebarUserPanel("USER NAME",
                                                subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
                               ),
                               sidebarMenu(
                                 # Setting id makes input$tabs give the tabName of currently-selected tab
                                 id = "tabs",
                                 menuItem("DASHBOARD", tabName = "dashboard", icon = icon("dashboard"),
                                          selectInput("main_genre", "Main Genre of the Movie",
                                                      c("Action", "Adventure", "Animation", "Biography", "Comedy",
                                                        "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
                                                        "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                                                        "Short", "Sport", "Thriller", "War", "Western")),
                                          sliderInput("year",strong("Year"),1916,2016,2010,animate = T),
                                          #uiOutput("director"),
                                          #uiOutput("actor")
                                          selectInput("director","Choose Director's Name",c("All")),
                                          selectInput("actor","Choose Main Actor's Name",c("All"))
                                 ),
                                 
                                 
                                 menuItem("GENRES", tabName = "subitem1",icon = icon("film")),
                                 menuItem("DIRECTORS/ACTORS", tabName = "subitem2",icon = icon("bar-chart-o")),
                                 menuItem("GLOBAL MAP", tabName = "subitem3",icon = icon("globe"))
                                 
                               )
  ),
  
  
  
  dashboardBody(
    tabItems(tabItem("subitem1",
                     fluidRow(
                       withSpinner(infoBoxOutput("total_movies",width = 4),color = "orange"),
                       withSpinner(infoBoxOutput("hit_movies",width = 4),color = "orange"),
                       infoBox(strong(" "),"Click for top 250 movies",icon = icon("star"),color = "purple",href = "https://www.imdb.com/chart/top",width = 4),
                       #bsButton("design2",label = "NEW DESIGN",size = "large",icon = icon("thumbs-o-up"),style = "success"),
                       column(12,
                              box(withSpinner(plotlyOutput("plot1"),color = "orange"),title = "GENRE-WISE MOVIE DISTRIBUTION (Use Mouse to identify the genre)",footer = strong("Radius of bubbles indicates number of movies"),solidHeader=T,status = "warning",width = 8,height = 500),
                              box(HTML('<ol style="color:Tomato;"><strong><li>Select the Main Genre from the dashboard</li>
                      <li>Slide to select the year from the dashboard</li>
                      <li>feel free to play with the play animation in year slider</li>
                      <li>Total and hit movies made in selected year and main genre can be viewed in above 1st and 2nd infobox</li>
                      <li>Click the 3rd infoBox to find the top 250 Movieson IMDB website</li>
                      <li>Click on notification box to open imdb website directly</li>
                      <li>Top 10 movies of selected year and Main genre is displayed in below datatable</li></strong></ol>'),
                                  title="INSTRUCTIONS",width=4,status = "warning",solidHeader = T)),
                       column(12,
                              box(footer = strong("Datatable containing selected main genre movies in selected year"),
                                withSpinner(dataTableOutput("plot2"),color = "orange"),status="warning",solidHeader=T,title="TOP 10 MOVIES BY IMDB RATING",width = 12))
                       #column(6,
                       #hpackedbubbleOutput("plot3"))
                     )
    ),
    tabItem("subitem2",
            fluidRow(
              box(width = 8,footer = strong("Scale of attributes are altered for spider chart, Please check below table for exact values"),
                     tabBox(id="bubble",
                            tabPanel(strong("DIRECTORS"),withSpinner(plotlyOutput("dirplot"),color = "orange")),
                            tabPanel(strong("ACTORS"),withSpinner(plotlyOutput("actplot"),color = "orange")),
                            width = 12),status = "warning",solidHeader = T),
                     box(HTML('<ol style="color:Tomato;"><strong><li>Select the Director name from the dashboard</li>
                      <li>Select the Actor name from the dashboard</li>
                      <li>switch between Director and Actor tab for both charts</li>
                      <li>Find original values of both average director and selected director in below table</li>
                      <li>Find original values of both average actor and selected actor in below table</li>
                      </strong></ol>'),
                         title="INSTRUCTIONS",width=4,status = "warning",solidHeader = T),
              box(Title="Tabular Data(Director)",withSpinner(dataTableOutput("DirMovie_stat"),color = "orange"),solidHeader=T,status = "warning"),
              box(Title="Tabular Data(Actor)",withSpinner(dataTableOutput("ActMovie_stat"),color = "orange"),solidHeader=T,status = "warning")
              
              )
            
    ),
    tabItem("subitem3",
            fluidRow(
              column(width = 12,
                     box(withSpinner(plotlyOutput("mapsplot"),color = "orange"),title = "GLOBAL IMDB AVERAGE(Use Mouse to identify the IMDB Score)",solidHeader=T,status = "warning",width = 12,height = 500)
              ))
    )
    
    
    )
    
  )
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  database<-read.csv("movie_metadata_original.csv")
  observe({
    updateSelectInput(session,"director",
                      label = "Choose Director's Name",
                      choices = database$director_name)
    updateSelectInput(session,"actor",
                      label = "Choose Main Actor's Name",
                      choices = database$actor_1_name)
  })
  
  output$plot2 = renderDataTable({
    Main_Genre<-input$main_genre
    database_main<-filter(database,database$Main_Genres==Main_Genre)
    year_distr<-input$year
    database_main_genre<-filter(database_main,database_main$title_year==year_distr)
    Sub_Genre<-database_main_genre$Sub_Genres
    IMDB_Rating<-database_main_genre$imdb_score
    Movie_title<-database_main_genre$movie_title
    Director<-database_main_genre$director_name
    Selected_Year<-database_main_genre$title_year
    data_upd<-data.frame(Movie_title,IMDB_Rating,Selected_Year,Main_Genre,Sub_Genre,Director)
    data_upd2<-data_upd[order(-IMDB_Rating),]
    data_upd3<-data_upd2%>%
      head(n=10)
    data_upd3
  })
  
  output$plot1<-renderPlotly({
    maingenres<-input$main_genre
    database_main_genre2<-filter(database,database$Main_Genres==maingenres)
    year_distr<-input$year
    database_main_genre<-filter(database_main_genre2,database_main_genre2$title_year==year_distr)
    data_sub<-database_main_genre$Sub_Genres
    data_IMDB<-database_main_genre$imdb_score
    data_title<-database_main_genre$movie_title
    data_revenue<-database_main_genre$gross
    data_upd<-data.frame(data_sub,data_IMDB,data_title,data_revenue)
    data_upd2<-data_upd %>%
      group_by(data_sub)%>%
      summarise(averageimdb = mean(data_IMDB),moviesnumber=n(),averagerevenue=mean(data_revenue))
    plot1<-plot_ly(data_upd2,y=data_upd2$moviesnumber,x=round(data_upd2$averageimdb,digits=2),
                   z=round(data_upd2$averagerevenue,digits=2),type = 'scatter3d',
                   color = data_upd2$data_sub,size = data_upd2$moviesnumber,
                   marker = list(symbol = 'circle', sizemode = 'diameter'),
                   text = ~paste('No.of movies:', data_upd2$moviesnumber,
                                 '<br>Average IMDB rating:', round(data_upd2$averageimdb,digits=2),
                                 '<br>Average Movie revenue:', round(data_upd2$averagerevenue,digits=2)),frame = T)
    
    plot1<-plot1%>%
      layout(title="IMDB rating VS Movie revenue VS No. of movies",
             scene = list(
               xaxis = list(title = 'Average IMDB rating'),
               yaxis = list(title = 'No. of Movies'),
               zaxis = list(title = 'Movie Revenue',
                            type = 'log'
               )))
    
  })
  output$total_movies<-renderInfoBox({
    maingenres<-input$main_genre
    database_main_genre2<-filter(database,database$Main_Genres==maingenres)
    year_distr<-input$year
    database_main_genre<-filter(database_main_genre2,database_main_genre2$title_year==year_distr)
    tot_data_IMDB<-database_main_genre$imdb_score
    tot_data_title<-database_main_genre$movie_title
    data_upd3<-data.frame(tot_data_IMDB,tot_data_title)
    data_upd4<-data_upd3 %>%
      summarise(averageimdb = mean(tot_data_IMDB),totmoviesnumber=n())
    #infoBox(strong(paste0("Total ",maingenres," Movies in ",year_distr,"-",as.numeric((year_distr)+1)," is ",data_upd4$totmoviesnumber)))
    infoBox(strong(" "),paste0("Total ",maingenres," movie made in ",year_distr,"-",as.numeric(year_distr+1)," is ",data_upd4$totmoviesnumber))
    #infoBox(strong(paste0("Total ",maingenres," Movie "),paste0("made in ",year_distr,"-",as.numeric(year_distr+1)," is ",data_upd4$totmoviesnumber)))  
    #infoBox(strong("Total ",maingenres," Movie "),HTML('<p>made in </p>'),year_distr,"-",as.numeric(year_distr+1)," is ",HTML('<p></p>'),data_upd4$totmoviesnumber)
    })
  
  output$hit_movies<-renderInfoBox({
    maingenres<-input$main_genre
    database_main_genre2<-filter(database,database$Main_Genres==maingenres)
    year_distr<-input$year
    database_main_genre<-filter(database_main_genre2,database_main_genre2$title_year==year_distr)
    tot_data_IMDB<-database_main_genre$imdb_score
    tot_data_title<-database_main_genre$movie_title
    data_upd3<-data.frame(tot_data_IMDB,tot_data_title)
    data_upd4<-data_upd3 %>%
      summarise(totmoviesnumber=n())
    
    data_upd5<-filter(database_main_genre,database_main_genre$imdb_score>=7)
    data_upd5<-data_upd5 %>%
      summarise(number=n())
    hit_number = (data_upd5$number/data_upd4$totmoviesnumber)*100
    
    infoBox(strong(" "),paste0("% of Hit ",maingenres," movies (IMDB score > 7) in ",year_distr,"-",as.numeric(year_distr+1)," is ",round(hit_number,digits = 2),"%"),icon = icon("thumbs-up"),color = "green")
  })
  
  output$dirplot<-renderUI({
    database<-read.csv("movie_metadata_original.csv")
    Average_IMDB<-database$imdb_score
    Revenue<-database$gross
    Budget<-database$budget
    Duration<-database$duration
    Director<-database$director_name
    Movie_Title<-database$movie_title
    database_director<-data.frame(Director,Movie_Title,Average_IMDB,Duration,Revenue,Budget)
    database_director<-unique(database_director)
    
  })
  
  output$dirplot<-renderPlotly({
    database<-read.csv("movie_metadata_original.csv")
    director_in<-input$director
    database_main_genre<-filter(database,database$director_name==director_in)
    director_movies<-count(database_main_genre,vars="movie_title")
    director_no_movies<-mean(director_movies$n)
    director_IMDB<-mean(database_main_genre$imdb_score)
    director_revenue<-mean(database_main_genre$gross)
    director_budget<-mean(database_main_genre$budget)
    director_duration<-mean(database_main_genre$duration)
    
    avg_dir<-database$director_name
    Movie_Name<-database$movie_title
    IMDB_Average<-mean(database$imdb_score)
    movie_revenue<-mean(database$gross)
    movie_budget<-mean(database$budget)
    movie_duration<-mean(database$duration)
    data_castd<-data.frame(avg_dir,Movie_Name)
    data_castdir<-data_castd%>%
      group_by(avg_dir)%>%
      summarise(Movie_count=n())
    avg_count<-mean(data_castdir$Movie_count)
    
    
    avg_trace <- list(
      fill = "toself", 
      name = "Average", 
      r = c(as.numeric((avg_count*4)),as.numeric((IMDB_Average*10)),as.numeric((movie_revenue*(100/760505847))),as.numeric((movie_budget*(100/2500000000))),as.numeric((movie_duration*(100/330))),as.numeric((avg_count*4))), 
      type = "scatterpolar", 
      theta = c('No. of Movies','IMDB_Rating','Revenue', 'Budget', 'Movies Duration', 'No. of Movies')
    )
    dir_trace <- list(
      fill = "toself", 
      name = paste0(director_in), 
      r = c(as.numeric((director_no_movies*4)),as.numeric((director_IMDB*10)),as.numeric((director_revenue*(100/760505847))),as.numeric((director_budget*(100/2500000000))),as.numeric((director_duration*(100/330))),as.numeric((director_no_movies*4))), 
      type = "scatterpolar", 
      theta = c('No. of Movies','IMDB_Rating','Revenue', 'Budget', 'Movies Duration', 'No. of Movies')
    )
    data <- list(dir_trace, avg_trace)
    layout <- list(
      polar = list(radialaxis = list(
        range = c(0, 100), 
        visible = TRUE
      )), 
      showlegend = T
    )
    p <- plot_ly()%>%
      add_trace(p, fill=dir_trace$fill, name=dir_trace$name, r=dir_trace$r, type=dir_trace$type, theta=dir_trace$theta)%>%
      add_trace(p, fill=avg_trace$fill, name=avg_trace$name, r=avg_trace$r, type=avg_trace$type, theta=avg_trace$theta)%>%
      layout(p, polar=layout$polar, showlegend=layout$showlegend)
    p
    
    
  })
  
  output$actplot<-renderPlotly({
    database<-read.csv("movie_metadata_original.csv")
    actor_in<-input$actor
    database_main_genre<-filter(database,database$actor_1_name==actor_in)
    actor_movies<-count(database_main_genre,vars="movie_title")
    actor_no_movies<-mean(actor_movies$n)
    actor_IMDB<-mean(database_main_genre$imdb_score)
    actor_revenue<-mean(database_main_genre$gross)
    actor_budget<-mean(database_main_genre$budget)
    actor_duration<-mean(database_main_genre$duration)
    
    avg_dir<-database$actor_1_name
    Movie_Name<-database$movie_title
    IMDB_Average<-mean(database$imdb_score)
    movie_revenue<-mean(database$gross)
    movie_budget<-mean(database$budget)
    movie_duration<-mean(database$duration)
    data_castd<-data.frame(avg_dir,Movie_Name)
    data_castdir<-data_castd%>%
      group_by(avg_dir)%>%
      summarise(Movie_count=n())
    avg_count<-mean(data_castdir$Movie_count)
    
    
    avg_trace <- list(
      fill = "toself", 
      name = "Average", 
      r = c(as.numeric((avg_count*(100/42))),as.numeric((IMDB_Average*10)),as.numeric((movie_revenue*(100/760505847))),as.numeric((movie_budget*(100/2500000000))),as.numeric((movie_duration*(100/330))),as.numeric((avg_count*4))), 
      type = "scatterpolar", 
      theta = c('No. of Movies','IMDB_Rating','Revenue', 'Budget', 'Movies Duration', 'No. of Movies')
    )
    dir_trace <- list(
      fill = "toself", 
      name = paste0(actor_in), 
      r = c(as.numeric((actor_no_movies*(100/42))),as.numeric((actor_IMDB*10)),as.numeric((actor_revenue*(100/760505847))),as.numeric((actor_budget*(100/2500000000))),as.numeric((actor_duration*(100/330))),as.numeric((actor_no_movies*4))), 
      type = "scatterpolar", 
      theta = c('No. of Movies','IMDB_Rating','Revenue', 'Budget', 'Movies Duration', 'No. of Movies')
    )
    data <- list(dir_trace, avg_trace)
    layout <- list(
      polar = list(radialaxis = list(
        range = c(0, 100), 
        visible = TRUE
      )), 
      showlegend = T
    )
    p <- plot_ly()%>%
      add_trace(p, fill=dir_trace$fill, name=dir_trace$name, r=dir_trace$r, type=dir_trace$type, theta=dir_trace$theta)%>%
      add_trace(p, fill=avg_trace$fill, name=avg_trace$name, r=avg_trace$r, type=avg_trace$type, theta=avg_trace$theta)%>%
      layout(p, polar=layout$polar, showlegend=layout$showlegend)
    p
  })
  
  
  output$mapsplot<-renderPlotly({
    
    df <-read.csv("movie_metadata_map.csv")
    l <- list(color = toRGB("#d1d1d1"), width = 0.5)
    #Specify map projection and options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'orthographic'),
      resolution = '100',
      showcountries = TRUE,
      countrycolor = '737474',
      showocean = TRUE,
      oceancolor = '69CCFC',
      showlakes = TRUE,
      lakecolor = 'navy',
      showrivers = TRUE,
      rivercolor = 'navy')
    p <- plot_geo(df) %>%
      add_trace(z = ~IMDB, color = ~IMDB, colors = 'YlOrRd',
                text = ~COUNTRY, locations = ~CODE, marker = list(line = l)) %>%
      colorbar(title = 'IMDB Rating') %>%
      layout(title = '', geo = g)
    p
    
  })
  
  output$DirMovie_stat<-renderDataTable({
    database<-read.csv("movie_metadata_original.csv")
    Director<-input$director
    #Director<-"James Cameron"
    avg_dir<-database$director_name
    Movie_Name<-database$movie_title
    IMDB_Average<-mean(database$imdb_score)
    movie_revenue<-mean(database$gross)
    movie_budget<-mean(database$budget)
    movie_duration<-mean(database$duration)
    data_castd<-data.frame(avg_dir,Movie_Name)
    data_castdir<-data_castd%>%
      group_by(avg_dir)%>%
      summarise(Movie_count=n())
    avg_count<-mean(data_castdir$Movie_count)
    
    database_main_genre<-filter(database,database$director_name==Director)
    director_movies<-count(database_main_genre,vars="movie_title")
    Movies_Count<-mean(director_movies$n)
    IMDB_Rating<-mean(database_main_genre$imdb_score)
    Revenue<-mean(database_main_genre$gross)
    Budget<-mean(database_main_genre$budget)
    Duration<-mean(database_main_genre$duration)
    DirData<-data.frame("Director"=c(Director,"'Average'"),"Movies_Count"=c(Movies_Count,avg_count),"IMDB_Rating"=c(IMDB_Rating,IMDB_Average),"Revenue"=c(Revenue,movie_revenue),"Budget"=c(Budget,movie_budget),"Duration"=c(Duration,movie_duration))
    t(DirData)
    
    
  })
  
  output$ActMovie_stat<-renderDataTable({
    database<-read.csv("movie_metadata_original.csv")
    Actor<-input$actor
    
    avg_dir<-database$actor_1_name
    Movie_Name<-database$movie_title
    IMDB_Average<-mean(database$imdb_score)
    movie_revenue<-mean(database$gross)
    movie_budget<-mean(database$budget)
    movie_duration<-mean(database$duration)
    data_castd<-data.frame(avg_dir,Movie_Name)
    data_castdir<-data_castd%>%
      group_by(avg_dir)%>%
      summarise(Movie_count=n())
    avg_count<-mean(data_castdir$Movie_count)
    
    database_main_genre<-filter(database,database$actor_1_name==Actor)
    director_movies<-count(database_main_genre,vars="movie_title")
    Movies_Count<-mean(director_movies$n)
    IMDB_Rating<-mean(database_main_genre$imdb_score)
    Revenue<-mean(database_main_genre$gross)
    Budget<-mean(database_main_genre$budget)
    Duration<-mean(database_main_genre$duration)
    ActData<-data.frame("Actor"=c(Actor,"'Average'"),"Movies_Count"=c(Movies_Count,avg_count),"IMDB_Rating"=c(IMDB_Rating,IMDB_Average),"Revenue"=c(Revenue,movie_revenue),"Budget"=c(Budget,movie_budget),"Duration"=c(Duration,movie_duration))
    
    t(ActData)
    
    
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
