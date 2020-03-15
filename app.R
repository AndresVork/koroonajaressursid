library(shiny)
library(shinyMatrix)
library(dplyr)
library(diagram)
library(heemod)

library(jsonlite)

#Väliste andmete laadimine
data <- fromJSON("https://raw.githubusercontent.com/okestonia/koroonakaart/master/public/EstonianData.json")
confirmed <- data$confirmed %>%  as.data.frame()
recovered <- data$recovered %>%  as.data.frame()
deaths <- data$recovered %>%  as.data.frame()
confirmed <- confirmed %>% mutate(date = as.Date(date)) %>% 
    #drop the day of the analysis, usually data are missing
    dplyr::filter(date<Sys.Date())

#daily cases
confirmeddaily <- confirmed %>%  group_by(date) %>% 
    summarise(newcases = n())  %>% 
    right_join(data.frame(date=seq.Date(from=min(confirmed$date), to = Sys.Date()-1, 
                                        by = "day"))) %>%
    mutate(newcases = ifelse(is.na(newcases), 0, newcases))

#cumulative cases
confirmedcumul <- confirmeddaily %>% 
    mutate(cumulcases = cumsum(newcases)) 

#Olekunimed
state_names = c("homecare", "stationarycare", "intensivecare", "recovered", "deaths")
#Igas seisundis saame määrata tervishoiusüsteemi kulud, nt vajalike töötajate arvu, terviseseisundite summa QALY jmt
#Need saab teha soovi korral sisendparameetriteks
State_homecare<- define_state(cost =0,utility =0.85)
State_stationarycare<- define_state(cost = 100, utility =0.6)
State_intensivecare<- define_state(cost =1000, utility =0.2)
State_recovered<- define_state(cost =0,utility =1)
State_deaths<- define_state(cost =0,utility =0)



ui <- fluidPage(

    # Application title
    titlePanel("Simulatsioonimudel"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            "Sisesta üleminekumaatriks",
            matrixInput("matrix1", 
                        value = matrix(
                            c(0.85, 0.03, 0.02, 0.10, 0.00,  #homecare
                            0.05, 0.90, 0.03, 0.01, 0.01, #stationary care
                            0.02, 0.50, 0.45, 0.00, 0.03, #intensive care
                            0, 0, 0, 1, 0,  #recovered stay recovered
                            0, 0, 0, 0, 1), 5, 5, byrow = TRUE,
                        dimnames = list(c("homecare", "stationarycare", "intensivecare", "recovered", "deaths"),
                                        c("homecare", "stationarycare", "intensivecare", "recovered", "deaths"))
                            ),
                        rows = list(names = TRUE,editableNames = FALSE),
                        cols = list(names =TRUE, editableNames = FALSE),
                        class = "numeric"),
          
              actionButton("button_calculate", "Uuenda simulatsioon",  
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("transitionmatrix"),
           br(),
           h3("Üleminekumaatriks"),
           plotOutput("matrixplot"),
           br(),
           h3("Uued juhud"),
           plotOutput("confirmednewcasesplot"),
           br(),
           h3("Kumulatiivsed juhud"),
           plotOutput("cumulativenewcasesplot"),
           
           h3("Simuleeritud tulemus"),
           plotOutput("simulatedcasesplot")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    ## markoviparameetrid -------------
    output$transitionmatrix <- renderTable(rownames = TRUE, {
        input$matrix1
    })

    mat_trans <- eventReactive(input$button_calculate, {

        lis <- as.list(t(input$matrix1))
        lis$state_names = colnames(input$matrix1)
        mat_trans <- do.call(define_transition, lis)
        mat_trans
    })
    
    output$matrixplot <- renderPlot({
        #plot(mat_trans())
        
        plot(mat_trans(), box.cex = 0.6, cex = 0.7, box.type= "ellipse", relsize = 0.75,
             pos = c(2,2,1))
    })
    
    
    ## aegrida -------------
    #Uued juhud
    output$confirmednewcasesplot <- renderPlot({
        confirmeddaily %>% 
            ggplot(aes(x=date, y = newcases)) +
            geom_col()
    })
    
    #Kumulatiivsed juhud
    output$cumulativenewcasesplot <- renderPlot({
        confirmedcumul %>% 
            ggplot(aes(x=date, y = cumulcases)) +
            geom_col()
    })
    
    #Mudeli simulatsioon ----------
    
    #sisendmaatriks on reactive; muud on hetkel määratud ülal enne ui osa.

    
    output$simulatedcasesplot <- renderPlot({
        #strateegia uuendamine
        modelstrategy <- define_strategy(
            transition = mat_trans(), #mat_trans on reactive
            homecare = State_homecare,
            stationarycare = State_stationarycare,
            intensivecare = State_intensivecare,
            recovered = State_recovered,
            deaths = State_deaths
        )
        
        #Kui iga päev alates tänasest tuleks peale 100 patsienti
        #Siia tuleb kindlasti teha nüüd prognoosimudel, mis sõltub sisendparameetritest
        inflow = define_inflow(
            homecare=100,
            stationarycare=0,
            intensivecare = 0,
            recovered =0,
            deaths=0
        )
        
        #Kõik senised uued juhtumid tulevad läbi homecare
        pt0 <- c(1, 0, 0, 0, 0.00)
        
        
        #mudeli simulatsioon
        res_mod <- run_model(
            modelstrategy,
            init = pt0*as.numeric(confirmeddaily[as.Date(confirmeddaily$date)==Sys.Date()-1, "newcases"]),
            inflow = inflow,
            cycles= 200,
            method = "end",
            effect = utility,
            cost = cost
        )
        #joonis
        plot(res_mod, panels = "by_state", free_y = TRUE)
        })

}

# Run the application 
shinyApp(ui = ui, server = server)
