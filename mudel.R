#Markovi mudel ressursside planeerimiseks

#TODO! Prognoosida uute juhtude arvu

#logi
#14.03.2020 - Andres

library(dplyr)

#Sisendparameetrid - input parameters
#seisundid
statenames <-  c("homecare", "stationarycare", "intensivecare", "recovered", "deaths")
#initial distribution, it should be possible to be updated  
#all new cases enter via homecare
pt0 <- c(1, 0, 0, 0, 0.00)

library(heemod)
#transition matrix between states - need information from health specialists
mat_trans  <- define_transition(
  0.85, 0.03, 0.02, 0.10, 0.00,  #homecare
  0.05, 0.90, 0.03, 0.01, 0.01, #stationary care
  0.02, 0.50, 0.45, 0.00, 0.03, #intensive care
  0, 0, 0, 1, 0,  #recovered stay recovered
  0, 0, 0, 0, 1, 
  state_names = c("homecare", "stationarycare", "intensivecare", "recovered", "deaths")
)

#transition matrix
library(diagram)
#plot(mat_trans)
#plot(mat_trans, relsize = 0.5, box.cex = 0.6, cex = 0.7)
plot(mat_trans, box.cex = 0.6, cex = 0.7, box.type= "ellipse", relsize = 0.75,
     pos = c(2,2,1))

#Input data
library(jsonlite)
data <- fromJSON("https://raw.githubusercontent.com/okestonia/koroonakaart/master/public/EstonianData.json")
confirmed <- data$confirmed %>%  as.data.frame()
recovered <- data$recovered %>%  as.data.frame()
deaths <- data$recovered %>%  as.data.frame()
confirmed <- confirmed %>% mutate(date = as.Date(date)) %>% 
  #drop the day of the analysis, usually date are missing
    dplyr::filter(date<Sys.Date())

#check
confirmed %>%  group_by(date) %>% 
  summarise(newcases = n()) %>% 
  ggplot(aes(x=date, y = newcases)) +
  geom_col()

confirmeddaily <- confirmed %>%  group_by(date) %>% 
  summarise(newcases = n())  %>% 
  right_join(data.frame(date=seq.Date(from=min(confirmed$date), to = Sys.Date()-1, 
                                      by = "day"))) %>%
  mutate(newcases = ifelse(is.na(newcases), 0, newcases))
         
#cumulative cases
confirmedcumul <- confirmeddaily %>% 
  mutate(cumulcases = cumsum(newcases)) 
#check
confirmedcumul %>% 
  ggplot(aes(x=date, y = cumulcases)) +
  geom_col()

#Define strategy for Markov chain simulation
state_names = c("homecare", "stationarycare", "intensivecare", "recovered", "deaths")
#State consists of cost to health care and health utility
State_homecare<- define_state(cost =0,utility =0.85)
State_stationarycare<- define_state(cost = 100, utility =0.6)
State_intensivecare<- define_state(cost =1000, utility =0.2)
State_recovered<- define_state(cost =0,utility =1)
State_deaths<- define_state(cost =0,utility =0)

mod1 <- define_strategy(
  transition = mat_trans,
  homecare = State_homecare,
  stationarycare = State_stationarycare,
  intensivecare = State_intensivecare,
  recovered = State_recovered,
  deaths = State_deaths
)

#model - one single inflow from 13 March 2020
res_mod <- run_model(
  mod1,
  init = pt0*as.numeric(confirmeddaily[confirmeddaily$date=="2020-03-13", "newcases"]),
  cycles= 14,
  method = "end",
  effect = utility,
  cost = cost
)

summary(res_mod)
plot(res_mod)


#Continuous inflow
#pt0 %*% t(confirmeddaily$newcases)

#TODO:! Tuua sisse aegrida mineviku haigestumisest ja prognoos

#Kui iga päev alates tänasest tuleks peale 100 patsienti
inflow = define_inflow(
  homecare=100,
  stationarycare=0,
  intensivecare = 0,
  recovered =0,
  deaths=0
  )

res_mod1 <- run_model(
  mod1,
  init = pt0*as.numeric(confirmeddaily[confirmeddaily$date=="2020-03-13", "newcases"]),
  inflow = inflow,
  cycles= 200,
  method = "end",
  effect = utility,
  cost = cost
)
#summary(res_mod1)
plot(res_mod1)
plot(res_mod1, panels = "by_state", free_y = TRUE)

#vaja läheks ca 60 intensiivravivoodit nende eelduste korral
