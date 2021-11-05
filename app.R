library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(readxl)
library(tidyverse)
library(sf)
library(plotly)
library(scales)
library(ggiraph)
library(RColorBrewer)
library(htmltools)
library(ggridges)
library(data.table)
library(grid)
library(treemapify)
library(sjPlot)
library(dashboardthemes)



## remove scientific notation
options(scipen = 999)

## load data
load("dataNeeded.RData")
load("lmNeeded.RData")

states.count <- states %>%
  group_by(state) %>%
  summarise(count = round(sum(rate), 0)) %>%
  pull(count, name = state)







## UI -------------------------------------------------------


ui <- dashboardPage(

## header -------------------------------------------------------
dashboardHeader(title = "Offences in Australia"),

## sidebar -------------------------------------------------------
dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Australia", tabName = "ausViz", icon = icon("th")),
	  menuItem("Victoria", tabName = "vicViz", icon = icon("th"))
    )
),

## body -------------------------------------------------------
dashboardBody(

  shinyDashboardThemes(
    theme = "blue_gradient"
  ),

## intro tab ---------------------------------------------
tabItems(
tabItem(tabName = "intro",

        fluidRow(
          img(src = "https://justiceaction.org.au/wp-content/uploads/2020/08/859674-75199-axhywvjxyg-1511960362.jpg",
              height = 400,
              width = "100%"),
          tags$br(),
          tags$p("Source: justiceaction.org.au"),
          tags$br(),

          box(title = "Background Information",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              tags$p("This narrative visualisation focuses on 3 main topics, offences in each states,
             offences in different age groups and genders and offences in LGAs in Victoria. You can explore the below questions
             through the interactive graphs.",
             style = htmltools::css(color = "grey",
                                    fontWeight="bold",
                                    font.size = "14px"),
             tags$br(),
             tags$li("How do the numbers of the recorded crimes vary in each state from 2008 - 2020?
                     Which state has the highest recorded offences and what is the most recorded offence in this state?",
                     style = htmltools::css(color = "grey",
                                            fontWeight="bold",
                                            font.size = "14px")),
             tags$br(),
             tags$li("Which age group and gender committed the most offences in Australia?
                     How do the numbers of offences vary in each age group and gender from 2008 - 2020?",
                     style = htmltools::css(color = "grey",
                                            fontWeight="bold",
                                            font.size = "14px")),
             tags$br(),
             tags$li("Which are the top crime LGAs in Victoria? What is the most common type of crime in this area?
                     Does it affect the liveability score for this area?",
                     style = htmltools::css(color = "grey",
                                            fontWeight="bold",
                                            font.size = "14px"))),
             tags$br(),
             tags$p("Please select the tabs on sidebar to start exploring.",
             style = htmltools::css(color = "grey",
                                    fontWeight="bold",
                                    font.size = "14px")
             )),
        ),

        fluidRow(
          box(title = "Disclaimer",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              tags$p("This narrative visualisation dashboard is for information and communication purposes only.
                     While care is taken to ensure accuracy, we cannot guarantee that information on this dashboard
                     is correct and recommends that users exercise care with its use.",
             style = htmltools::css(color = "grey",
                                    fontWeight="bold",
                                    font.size = "14px")
              ))
        )


        ),






## Australia tab ---------------------------------------------
tabItem(tabName = "ausViz",

## display box for total count of offences in eash state
fluidRow(
valueBox(tags$p(names(states.count)[1], style = "font-size: 50%;"), tags$p(states.count[1]), width = 1, color = "teal"),
valueBox(tags$p(names(states.count)[2], style = "font-size: 50%;"), tags$p(states.count[2]), width = 1, color = "aqua"),
valueBox(tags$p(names(states.count)[3], style = "font-size: 50%;"), tags$p(states.count[3]), width = 1, color = "teal"),
valueBox(tags$p(names(states.count)[4], style = "font-size: 50%;"), tags$p(states.count[4]), width = 1, color = "aqua"),
valueBox(tags$p(names(states.count)[5], style = "font-size: 50%;"), tags$p(states.count[5]), width = 1, color = "teal"),
valueBox(tags$p(names(states.count)[6], style = "font-size: 50%;"), tags$p(states.count[6]), width = 1, color = "aqua"),
valueBox(tags$p(names(states.count)[7], style = "font-size: 50%;"), tags$p(states.count[7]), width = 1, color = "teal"),
valueBox(tags$p(names(states.count)[8], style = "font-size: 50%;"), tags$p(states.count[8]), width = 1, color = "aqua")
),


tabsetPanel(

## first tab -------------------------------------------------
tabPanel("States",

## choropleth map of Aus
fluidRow(
  shinydashboard::box(title = "Recorded offences per 100,000 population of each state",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 10,
                      height = 700,
                      withSpinner(girafeOutput("ausmap")),
                      tags$p("Data souce: Australian Bureau of Statistics",
                             style = htmltools::css(color = "lightblue",
                                                    fontWeight="bold",
                                                    font.size = "12px"))
                      ),
## tex box
  shinydashboard::box(title = "Manual",
                      status = "success",
                      solidHeader = TRUE,
                      width = 2,
                      tags$p(tags$li("Hover over on a graph to see the tooltips for more information.",
                                     style = htmltools::css(color = "grey",
                                                            font.size = "14px")),
                             tags$li("Selected one or more state on the map or use the lasso selector on top right corner
                                     to see yearly offences trend in line chart.",
                                     style = htmltools::css(color = "grey",
                                                            font.size = "14px")),
                             tags$li("To deselect a state, click again on the map.",
                                     style = htmltools::css(color = "grey",
                                                            font.size = "14px")),
                             tags$li("Move slider bar to change the year on sunburst chart.",
                                     style = htmltools::css(color = "grey",
                                                            font.size = "14px")),
                             tags$li("Click a state on the sunburst chart to see the different type of
                                     offences committed in this state and click again to return back to default view.",
                                     style = htmltools::css(color = "grey",
                                                            font.size = "14px")),
                             tags$li("Click 'Age & Gender' tab to switch to a different dashboard.",
                                     style = htmltools::css(color = "grey",
                                                            font.size = "14px"))
)),





  ),


## line chart for trend
fluidRow(

  shinydashboard::box(title = "Recorded offences by state and year",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 5,
                      height = 550,
                      tags$p("Data souce: Australian Bureau of Statistics",
                             style = htmltools::css(color = "lightblue",
                                                    fontWeight="bold",
                                                    font.size = "12px")),
                      withSpinner(plotlyOutput("stateTrend"))

),
## sunburst chart for offences types
  shinydashboard::box(title = "Different type of offences committed in each state",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 5,
                      height = 550,
                      tags$p("Data souce: Australian Bureau of Statistics",
                             style = htmltools::css(color = "lightblue",
                                                    fontWeight="bold",
                                                    font.size = "12px")),
                      # year slider bar
                      sliderInput("stateYearSelected",
                                  "Year",
                                  min = min(as.integer(states$year)),
                                  max = max(as.integer(states$year)),
                                  value = 2008,
                                  step = 1),
                      withSpinner(plotlyOutput("stateSun"))

),



  ),

fluidRow(



  shinydashboard::box(title = "Summary",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 10,
                      tags$p(tags$li("NT had the highest offences count per 100,000 population and ACT had the least
                                     offence numbers over the period of 2008-2019, but crimes in Australia seem to
                                     be under control.",
                             style = htmltools::css(color = "grey",
                                                    fontWeight = "bold",
                                                    font.size = "14px")),
                             tags$li("All the states are not showing an increasing trend in the recorded crimes since 2015.",
                                     style = htmltools::css(color = "grey",
                                                            fontWeight = "bold",
                                                            font.size = "14px")),
                             tags$li("Theft, Acts intended to cause injury, drug offences are the top offences in most of the states.",
                                     style = htmltools::css(color = "grey",
                                                            fontWeight = "bold",
                                                            font.size = "14px"))))

)


),




## second tab ---------------------------------------------------------
tabPanel("Age & Gender",

fluidRow(

      ## ridgeline plot
       box(title = "Distribution of recorded offences by age",
           status = "primary",
           solidHeader = TRUE,
           width = 7,
           height = 500,
           selectInput("ageGroup",
                       NULL,
                       choices = c("All", unique(age_p$age)),
                       selected = "All", multiple = TRUE),
           withSpinner(girafeOutput("AgeRidge")),
           tags$p("Data souce: Australian Bureau of Statistics",
                  style = htmltools::css(color = "lightblue",
                                         fontWeight="bold",
                                         font.size = "12px"))),

       ## line chart for gender
       box(title = "Recorded offences by gender and year",
           status = "primary",
           solidHeader = TRUE,
           width = 5,
           height = 500,
           tags$p("Data souce: Australian Bureau of Statistics",
                  style = htmltools::css(color = "lightblue",
                                         fontWeight="bold",
                                         font.size = "12px")),
           withSpinner(plotlyOutput("GenderDiff"))
  ),

  fluidRow(


      ## text box
       box(title = "Manual",
           status = "success",
           solidHeader = TRUE,
           width = 7,
           tags$p(tags$li("Hover over on a graph to see the tooltips for more information.",
                          style = htmltools::css(color = "grey",
                                                 font.size = "14px")),
                  tags$li("Selected one or more age groups in the drop down box to see the offences committed by different gender in
                          this age group, when no groups selected, error will be display.",
                          style = htmltools::css(color = "grey",
                                                 font.size = "14px")),
                  tags$li("Default view of line chart displays data for all age groups.",
                          style = htmltools::css(color = "grey",
                                                 font.size = "14px")))),

       box(title = "Summary",
           status = "primary",
           solidHeader = TRUE,
           width = 5,
           tags$p("Youth aged 15-24 years committed the most crimes among all age groups and males were the predominant offender
                             across all age groups. However, there are no incerasing trends in offences committed in both genders.",
                  style = htmltools::css(color = "grey",
                                         fontWeight = "bold",
                                         font.size = "14px"))),


  )


)

)
)),


## victoria dashboard --------------------------------------------
tabItem(tabName = "vicViz",

fluidRow(

  ## text
  box(title = "Manual",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      tags$p(tags$li("Hover over on a graph to see the tooltips for more information.",
                     style = htmltools::css(color = "grey",
                                            font.size = "14px")),
             tags$li("Selected a LGA on the map to see offence types in this area in bar chart.",
                     style = htmltools::css(color = "grey",
                                            font.size = "14px")),
             tags$li("To deselect a state, click again on the map.",
                     style = htmltools::css(color = "grey",
                                            font.size = "14px")),
             tags$li("To zoom in, click the blue magnifier  icon to active the zoom in function,
                   double click on the map to zoom in and click the pan icon to zoom out.",
                   style = htmltools::css(color = "grey",
                                          font.size = "14px")),
             tags$li("Move slider bar to change the year of the choropleth map.",
                     style = htmltools::css(color = "grey",
                                            font.size = "14px")),
             tags$li("When a year is selected on the slider bar, border of the LGAs with an increase of more than 20% in offences per 100,000 population
                   will be highlighted in red.",
                   style = htmltools::css(color = "grey",
                                          font.size = "14px")),
             tags$li("Number displayed on the top right corner on the map is the total count of offences in Victoria in a given year.",
                     style = htmltools::css(color = "grey",
                                            font.size = "14px")),
             tags$li("Choose a plot type of your choice in the selection panel.",
                     style = htmltools::css(color = "grey",
                                            font.size = "14px")),
             tags$li("Click 'Let's find out' button to display graph and statistical outputs.",
                     style = htmltools::css(color = "grey",
                                            font.size = "14px"))
      ))

),





fluidRow(

## choropleth map for Vic
shinydashboard::box(title = "Offences in Victoria by LGA",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 7,
                    height = 650,
                    withSpinner(girafeOutput("vicmap")),
                    sliderInput("lgaYear","Year",
                                min = min(vic_crime$Year),
                                max = max(vic_crime$Year),
                                value = min(vic_crime$Year),
                                step=1,
                                width="150%"),
                    tags$p("Data Source:Victoria state government crime statistics agency",
                           style = htmltools::css(color = "lightblue",
                                                  fontWeight="bold",
                                                  font.size = "12px"))),
## bar chart
shinydashboard::box(title = "Recorded offence by gender and year",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 5,
                    height = 650,
                    withSpinner(plotlyOutput("lgaSummary")),
		        radioButtons("lgaSummaryType",
		                     "Plot Type",
		                     choices = c("bar chart","pie chart","treemap"),
		                     selected = "bar chart",
		                     inline = T),
		        tags$p("Data Source:Victoria state government crime statistics agency & Domain.com.au",
		               style = htmltools::css(color = "lightblue",
		                                      fontWeight="bold",
		                                      font.size = "12px")))

),


fluidRow(
  box(title = "Offences against liveability scores of a suburb",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tags$p("Do you think crimes affect the liveability scores of a suburb? 🤔",
             style = htmltools::css(color = "#277F8EFF",
                                    fontWeight = "bold",
                                    font.size = "20px")),
      actionButton("lmactionB",
                   "Let's find out!"),
      uiOutput("lagLMplot")
)
),

fluidRow(
  box(title = "Summary",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      tags$p(tags$li("Melbourne CBD in Victoria had the highest crimes per 100,00 population in each year
                     followed by some northwest suburbs,
                     and what’s more, property and deception was the main crime in almost every LGA each year
                     between 2012 - 2021.",
                     style = htmltools::css(color = "grey",
                                            fontWeight = "bold",
                                            font.size = "14px")),
             tags$li("Click 'Let's find out' button to see the summary of offences agains liveability scores of a suburb.",
                     style = htmltools::css(color = "grey",
                                            fontWeight = "bold",
                                            font.size = "14px"))))



)



)
)
)
)








## SERVER ----------------------------------------------------

server <- function(input, output,session)
{


  observeEvent("", {
    showModal(modalDialog(
      title = "Important message",
      "Please do not close the R studio shiny popup window when opening the appilcation in a
      web browser, otherwise the application will go greyed out!",
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "message", label = "Dismiss")
      )
    ))
  })


  observeEvent(input$message,{
    removeModal()
  })

###ausmap
output$ausmap <- renderGirafe({
#####################aus map filled with count per 100,000 population

stateTotalCount <- states %>%
                   group_by(state) %>%
                   summarise(total_count = sum(rate)) %>%
                   ungroup()

aus_map2 <- aus_map1_simple %>%
  left_join(stateTotalCount, by = "state")

aus_map2 <- aus_map2 %>%
  mutate(label = paste0(STATE_NAME,'(',state,')<br/>Total Count: ', comma(total_count)))

##create interactive choropleth map
aus_map_plot <-  ggplot(aus_map2,
                        aes(geometry = geometry,
                            fill = total_count,
                            data_id = state,
                            tooltip = label)) +
  geom_sf_interactive(colour = "white",
                      size = 0.2) +
  scale_fill_viridis_c(direction = -1) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0)) +
  labs(title = "Year: 2008 -2019",
       fill = "Offences \nper 100,000 population")

#girafe(ggobj=aus_map_plot, width_svg=8,height_svg=5.7,options = list(
girafe(ggobj = aus_map_plot,
       width_svg = 12,
       height_svg = 8.6,
       options = list(
         opts_sizing(rescale = FALSE),
         opts_tooltip(use_fill= TRUE),
         opts_hover_inv(css = "opacity:0.5;"),
         opts_hover(css = "stroke:#1279BF;cursor:pointer;"),
         opts_toolbar(saveaspng=FALSE),
	#opts_selection(css = "stroke:#1279BF;cursor:pointer;",type='multiple')
	      opts_selection(type = 'multiple')
  ))
})

###stateTrend
output$stateTrend <- renderPlotly({


aus_line.data <- states %>%
  group_by(state, year) %>%
  summarise(total_count = sum(rate)) %>%
  ungroup()

if(isTruthy(input$ausmap_selected)) aus_line.data <- aus_line.data %>% filter(state %in% input$ausmap_selected)

aus_line <- ggplot(aus_line.data,
                   aes(x = year,
                       y = total_count,
                       colour = state,
                       group = state)) +
  geom_line(size = 0.8) +
  geom_point(size = 0.8) +
  theme_bw(base_size = 6) +
  theme(plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0),
        axis.text.x = element_text(hjust = 1,vjust = 1,angle = 30, size = 8),
        panel.grid.major.x = element_blank()
		) +
  scale_colour_viridis_d() +
  labs(x = "",
       y = "Offences per 100,000",
       colour = "")

ggplotly(aus_line,
         tooltip = c("x","y","colour")) %>%
  layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5,
                       y = -0.1,
                       font = list(size = 12))) %>%
  config(displayModeBar = FALSE)


})

###################stateSun
output$stateSun <- renderPlotly({

copy(states) -> states.burst
setDT(states.burst)
stateYear <- input$stateYearSelected

## create data frame
states.burst[year == stateYear] -> inter.states.burst
inter.states.burst[,.(count = sum(rate)), by = state] -> inter.states.burst1
inter.states.burst1[,.(labels = state, parent = stateYear, value = count)] -> inter.states.burst1
inter.states.burst[,.(count = sum(rate)), by=.(state, offence)] -> inter.states.burst2
##inter.states.burst2[,.SD[order(-count)][1:5],by=state]->inter.states.burst2
inter.states.burst2[,offence:=paste0(offence,'(',state,')')]
# inter.states.burst2[,offence:=paste0(offence,'(',state,')')]
inter.states.burst2[,.(labels = offence, parent = state, value = count)] -> inter.states.burst2
sunburst.data <- rbind(data.table(labels = stateYear,
                                  parent = "",
                                  value = inter.states.burst[,sum(rate)]),
                       inter.states.burst1,
                       inter.states.burst2)

## plot sunburst chart
plot_ly(
  height = 340,
  labels = sunburst.data[,labels],
  parents = sunburst.data[,parent],
  values = sunburst.data[,value],
  type = "sunburst",
  branchvalues = "total"
) %>%
  layout(
    colorway = c(
      "#440154FF","#46337EFF","#365C8DFF","#277F8EFF","#1FA187FF",
      "#4AC16DFF", "#9FDA3AFF","#FDE725FF")
    )
})


### vicmap

output$vicmap <- renderGirafe({

## filter data for highlighted borders
vicCrimeCount <- vic_crime %>%
  group_by(lga, Year) %>%
  summarise(total_count = sum(rate)) %>%
  mutate(pct_change = round((total_count/lag(total_count) -1) * 100, 1)) %>%
  ungroup() %>%
  filter(Year == input$lgaYear) %>%
  mutate(strokeV = case_when(is.na(pct_change)~0,
                             !is.na(pct_change)~pct_change))


q3_map <- vic_map_simple %>%
  left_join(vicCrimeCount,
            by = "lga")

q3_map <- q3_map %>%
  mutate(label = paste0(lga,"<br/>Total Count: ",
                        gsub("\\.0$","", comma(q3_map$total_count))))


countInfo <- vic_crime %>%
  filter(Year == input$lgaYear) %>%
  summarise(sum(rate)) %>%
  pull(1)

countInfo <- gTree(children = gList(
textGrob(x = unit(1,"npc") - unit(3,"mm"),
         y = unit(1,"npc") - unit(3,"mm"),
         label = paste0("Total offence count in Vic in ", input$lgaYear," is"),
         just = c("right","top"),
         gp = gpar(fontface = 1, cex = 1.5)),

textGrob(x = unit(1,"npc") - unit(3,"mm"),
         y = unit(1,"npc") - unit(5,"mm") - unit(1,"lines"),
         label = scales::comma(countInfo),
         just = c("right","top"),
         gp = gpar(fontface = 4, cex = 2))
))



vic_plot <- ggplot(q3_map,
                   aes(geometry = geometry,
                       fill = total_count,
                       data_id = lga,
                       tooltip = label)) +
  geom_sf_interactive(colour = "white",
                      size = 0.2) +
  geom_sf_interactive(data = q3_map %>%
                      filter(strokeV >= 20),
                      colour = "#EE4B2B",
                      lwd = 0.7) +
  scale_fill_viridis_c(direction = -1,
                       labels = comma,
                       breaks = seq(0, 30000, 5000)) +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(size = 16),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  labs(fill = "Offences per 100,000 population",
       title = paste("Year:",input$lgaYear)) +
  guides(fill = guide_colorbar(barwidth = unit(6,"cm"))) +
  annotation_custom(countInfo)

girafe(ggobj = vic_plot,
       width_svg = 6.5,
       height_svg = 5.8,
       options = list(
         opts_sizing(rescale = FALSE),
         opts_tooltip(use_fill = TRUE),
         opts_hover_inv(css = "opacity:0.5;"),
         opts_hover(css = "stroke:#1279BF;cursor:pointer;"),
         opts_toolbar(saveaspng = FALSE),
	#opts_selection(css = "stroke:#1279BF;cursor:pointer;",type="single")
	       opts_selection(type ="single"),
	      opts_zoom(min = .7, max = 15)
  ))
})

###lgaSummary
output$lgaSummary <- renderPlotly({

if(isTruthy(input$vicmap_selected))
{
vic_crime_plot.data <- vic_crime %>%
  filter(Year == input$lgaYear,
         lga == input$vicmap_selected) %>%
  group_by(lga, offence) %>%
  summarise(total_count = sum(rate)) %>%
  mutate(offence = fct_reorder(offence, total_count))
} else {
vic_crime_plot.data <- vic_crime %>%
  filter(Year == input$lgaYear) %>%
  group_by(offence) %>%
  summarise(total_count = sum(rate)) %>%
  mutate(offence = fct_reorder(offence, total_count))
}

Vicwhere <- paste(input$lgaYear, ifelse(isTruthy(input$vicmap_selected), input$vicmap_selected, "Whole Victoria"))
vic.plot.title <- Vicwhere

# paste0("Number of different offences committed(",Vicwhere,")")


if(input$lgaSummaryType == "bar chart"){
vic_crime_plot<-ggplot(vic_crime_plot.data,
                       aes(x = offence,
                           y = total_count,
                           fill = offence)) +
  geom_col() +
  # coord_flip() +
  scale_fill_viridis_d(direction = -1) +
  theme_light(base_size = 6) +
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 6, angle = 70),
        axis.title.x = element_text(size = 8),
        plot.title = element_text(size = 12, face = "bold"),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  labs(x = "Offences",
       y = "Offences per 100,000",
       title = vic.plot.title
	   )

vic_crime_plot_plotly <- ggplotly(vic_crime_plot +
                                    labs(x = "") +
                                    scale_y_continuous(expand = expansion(c(0,0.05))),
          tooltip = c("x","y")) %>%
  config(displayModeBar = FALSE)

}

if(input$lgaSummaryType == "pie chart")
{
vic_crime_plot_plotly <- plot_ly(vic_crime_plot.data %>%
                                   arrange(offence),
         labels = ~offence, values = ~total_count, type = "pie",
         marker = list(colors = viridis_pal(direction = -1)(6),
                      line = list(color = "#FFFFFF", width = 1)),
         showlegend = FALSE) %>%
  layout(title = list(text = vic.plot.title, font = list(size = 12)),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

}


if(input$lgaSummaryType == "treemap")
{
inter.data2 <- treemapify(vic_crime_plot.data,
                          area ="total_count") %>%
            left_join(vic_crime_plot.data,
                      by="offence") %>%
            mutate(label = paste0(offence,"<br><b><i>", comma(total_count),"</i></b>"))



inter.data2<-inter.data2 %>% mutate(
             xcen:=0.5*(xmin+xmax),
             ycen = 0.5*(ymin+ymax),
             textlabel = gsub(".*?(<.*>)","\\1",label),
             textsize = scales::rescale(sqrt(total_count),
                                        to = c(0.3, 5))
             )

ppTreemap<-ggplot(inter.data2,
                  aes(xmin = xmin,
                      xmax = xmax,
                      ymin = ymin,
                      ymax = ymax,
                      fill = offence,
                      text = label)) +
  geom_rect(color ="white",
            lwd = 0.2)+
  geom_text(mapping = aes(x = xcen,
                          y = ycen,
                          label = label,
                          text = label,
                          size = textsize),
            colour = "white",
            inherit.aes = F,
            fontface = "bold")+
  scale_size_identity()+
  scale_fill_viridis_d(direction = -1)+
  # scale_fill_brewer(palette = "Set3") +
	scale_x_continuous(expand = expansion(0))+
	scale_y_continuous(expand = expansion(0))+
	theme_void()



vic_crime_plot_plotly <- ggplotly(ppTreemap,
                                  tooltip="text") %>%
                       hide_legend()  %>%
                       layout(title = list(text = vic.plot.title,
                                           font = list(size = 11)))


}


vic_crime_plot_plotly
})


#### AgeRidge
output$AgeRidge <- renderGirafe({

ageplot0.data <-if("All" %in% input$ageGroup) age_p else age_p %>% filter(age %in% input$ageGroup)

ageplot0 <- ggplot(ageplot0.data,
                   aes(x = rate,
                       y = age,
                       fill = age)) +
  geom_density_ridges(alpha = 0.8)

ageplot0.data <- ggplot_build(ageplot0)$data[[1]] %>%
  mutate(age = unique(ageplot0.data$age)[as.integer(y)])

age_ridge <- ggplot(ageplot0.data,
                    aes(x = x,
                        ymin = ymin,
                        ymax = ymax,
                        fill = fill,
                        data_id = age,
                        tooltip = age)) +
geom_line(aes(x = x,
              y = ymax,
              group = ymin),
          inherit.aes = FALSE) +
  geom_ribbon_interactive(alpha = 0.6,
                          show.legend = F) +
  scale_fill_identity() +
  theme_ridges() +
  scale_fill_viridis_d() +
  labs(x = "Recorded Offence per 100,000",
       y = "Age Group",
       fill = "Age",
       title = "Year: 2008 - 2019") +
  theme(axis.text.y = element_text(size = 10),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0))+
  scale_y_continuous(breaks = 1:length(unique(ageplot0.data$age)),
                     label = unique(ageplot0.data$age))


girafe(ggobj = age_ridge,
       width_svg = 8,
       height_svg = 5,
       options = list(
         opts_sizing(rescale = FALSE),
         opts_tooltip(use_fill= TRUE),
         opts_hover_inv(css = "opacity:0.5;"),
         opts_hover(css = "stroke:#1279BF;cursor:pointer;")
  ))
})



#### GenderDiff
output$GenderDiff<-renderPlotly({

sex_plot.data <- if("All" %in% input$ageGroup) age_sex else age_sex %>% filter(age %in% input$ageGroup)
title.info <- if("All" %in% input$ageGroup) "All ages" else paste(input$ageGroup,collapse="& ")


sex_plot.data <- sex_plot.data %>%
  group_by(year, sex) %>%
  summarise(total_count = sum(rate)) %>%
  ungroup()


sex_plot <- ggplot(sex_plot.data,
                   aes(x = year,
                       y = total_count,
                       colour = sex,
                       group = sex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#22A884FF", "#440154FF")) +
  # scale_colour_viridis_d(direction = -1) +
  theme_light(base_size = 6) +
  labs(x = "",
       y = " Offences per 100,000",
       colour = "",
       title = title.info) +
  theme(axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 75, hjust = 1),
        plot.title = element_text(size = 12, face="bold"),
        legend.text=element_text(size = 10),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank())

ggplotly(sex_plot,
         height = 400,
         tooltip = c("x","y","colour")) %>%
  layout(legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5,
                       y = -0.2,
                       font = list(size = 12))) %>%
  config(displayModeBar = FALSE)
})

### suburb linear regression
output$lagLMplot<-renderUI({

req(input$lmactionB)

linear_plot <- q3_2 %>%
  ggplot(aes(x = log(total),
             y = rank)) +
  geom_smooth(method = "lm",
              colour = "#7AD151FF",
              se = FALSE) +
    geom_point(colour = "#440154FF") +
  theme_light(base_size=6) +
  labs(x = "Number of Crimes(log)",
       y = "Liveability Scores") +
        theme(axis.text.y = element_text(size = 6),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0))


fit <- lm(rank ~ log(total), data = q3_2)
fit.table <- tab_model(fit)$knitr
fit.table <- gsub('border:none;">', 'border:none;margin:auto">', fit.table)
lmt <- tags$div(HTML(fit.table))


tagList(tags$br(),
        tags$br(),
        tags$p("The graph below shows no correlation between recorded offences and
               liveability ranking for a suburb, the points do not closely lie along the line. R square value in the table output also suggests
               very week relationship between crimes and liveability scores, only 3% of the liveability scores can be explained by
               the offence numbers. Therefore, there is no strong relationship between recorded offences and liveability scores for a suburb. There are many
               other factors thatcan affect the liveability score of a suburb, such as public transport, schools, parks, shopping precinct,
               etc.",
               tags$br(),
               tags$br(),
               "Did you get it right?",
               style = htmltools::css(color = "#277F8EFF",
                                      fontWeight = "bold",
                                      font.size = "14px")),
        ggplotly(linear_plot),
        tags$br(),
        tags$br(),
        lmt
)


})




}




shinyApp(ui, server)



