#Exercises
# Use the help function to explore what the series gafa_stock, PBS, vic_elec and pelt represent.

library(fpp3)
library(GGally)



help("gafa_stock")
help("PBS")
help("vic_elec")
help("pelt")
#------------------------------------

gafa_stock
View(gafa_stock)

gafa_stock %>%
  filter(year(Date)==2014) %>%
  autoplot(High) +
  labs(title = "High Trading Stock Prices in 2014", y = "Prices in $USD")

gafa_stock %>%
  filter(year(Date) == 2014) %>%
  gg_season(High) +
  labs(y= "Prices in $USD", title = "High trading stock prices in 2014")


help(PBS)

exerc <-PBS %>%
  filter(ATC2 == "A01") %>%
  select(Type, Month, Concession, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6)

autoplot(exerc, Cost) +
  labs(y = "$ Millions", title = "Australian Drug Sales")

help(vic_elec)
vic_elec %>%
  gg_season(Demand, period = "day") +
  labs(title = "Electricity Demand", y = "MWh")


vic_elec %>%
  gg_season(Temperature) +
  labs(title= "Temperature Demand in MW")

help(pelt)

pelt %>%
  ggplot(aes(Hare, Lynx)) +
  geom_point()+
  labs(title = "Pelt trading records")

#Use filter() to find what days corresponded to the peak closing price for 
#each of the four stocks in gafa_stockk


gafa_stock %>%
  filter(year(Date) == 2014) %>%
  gg_season(Close, period = "year") +
  labs(y= "Prices in $USD", title = "High trading stock prices in 2014")

tute <- readr::read_csv("tute1.csv")
tute

#converting tute to a timeseries
mytimeseries <- tute %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)

mytimeseries  

#plotting a graph
mytimeseries %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(x= Quarter, y= value, colour = name)) +
  geom_line()+
  facet_grid(~., scales = "free_y")

#install.packages("USgas")
library(USgas)

us_total
View(us_total)
# create a tsibble with us_total with year as index and state as a key

natural_gas <- us_total %>%
  as_tsibble(key = "state", index = "year") ; natural_gas


# Plot the annual natural gas consumption by state for the New England area (comprising the states of Maine, Vermont, New Hampshire, Massachusetts, Connecticut and Rhode Island).

UK_natgas <- natural_gas %>%
  filter(state %in% c("Maine", "Vermont", "New Hampshire", "Massachusetts", "Connecticut", "Rhode Island"))

View(UK_natgas)  

UK_natgas %>%
  mutate(y_abb = y/1e3) %>%
  autoplot(y_abb, period = "year") +
  labs(y = "Natural Gas consumption ('000) ", title = "Natural Gas Consumption in the New England")


UK_natgas %>%
  mutate(y_abb = y/1e3) %>%
  autoplot(y_abb) +
  facet_grid(vars(state), scales = "free_y") +
  labs(y = "Natural Gas consumption ('000) ", title = "Natural Gas Consumption in New England")



touris = readxl::read_excel("tourism.xlsx"); touris

tourim <- touris %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(key = c("Region", "State", "Purpose"), index = Quarter)

select(touris, Region, Purpose, Trips) %>%
  group_by(Region, Purpose) %>%
  summarise(Trips = max(Trips))


tot_trips <- tourim %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)); tot_trips

#ctrl 3 - the help pane

#Create time plots of the following four time series: 
#Bricks from aus_production, Lynx from pelt, 
#Close from gafa_stock, Demand from vic_elec.


aus_production

aus_production %>%
  autoplot(Bricks) +
  labs(y = "Clay Brick production in Millions", title = "Quarterly Production of Bricks in Australia")


# OR
aus_production %>%
  ggplot(aes(x=Quarter, y= Bricks)) +
  geom_line() +
  labs(y= "Clay Brick production in Millions", title = "Quarterly Production of Bricks in Australia")


View(pelt)

#check your phone for the answer
#pelt %>% summarise(Lynx) %>% mutate(Lynx = Lynx/1e3) %>% autoplot('Lynx')

pelt

pelt %>%
  mutate(Lynx=Lynx/1e3) %>%
  ggplot(aes(Year, Lynx)) +
  geom_line()+
  labs(y = "Lynx ('000)",title = "Lynx trading records")


gafa_stock

gafa_stock %>%
  autoplot(Close)

help(vic_elec)

vic_elec %>%
  autoplot(Demand) +
  labs(y = "Total Electricity Demand in MW", title = "Half-hourly Electricity Demand for Victoria, Australia")


View(aus_arrivals)

aus_arrivals %>%
  distinct(Origin)

aus_arrivals %>%
  mutate(Arrivals = Arrivals/1e3) %>%
  autoplot(Arrivals) +
  facet_grid(vars(Origin), scales = "free_y")
  labs(y= "Arrivals in ('000)", title = "International Arrivals to Australia")



aus_arrivals %>%
  mutate(Arrivals = Arrivals/1e3) %>%
  gg_season(Arrivals, period = "year") +
  labs(y= "Arrivals in ('000)", title = "International Arrivals to Australia")


aus_arrivals %>%
  mutate(Arrivals = Arrivals/1e3) %>%
  gg_subseries(Arrivals) +
  labs(y= "Arrivals in ('000)", title = "International Arrivals to Australia")


# NZ had a steady increase of arrivals from 1995
# there was a low arrival from UK during the begining to the end of 2nd quarter for all years represented
#Japan had the highest arrival in 1995 in the q3
# Uk arrivalalways had a spike from the third quater of the year

aus_retail

aus_retail %>%
  distinct(`Series ID`)


set.seed(50)
myseries <-aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1));myseries

myseries %>%
  autoplot(Turnover)



myseries %>%
  gg_season(Turnover, period= "year")



myseries %>%
  gg_subseries(Turnover, period= "year")



myseries %>%
  gg_lag(Turnover, period= "year", geom = "point")


myseries %>%
  ACF(Turnover) %>% 
  autoplot()


#--------------------------------------------
help(us_employment)
us_employment

us_employment %>%
  filter(Title == "Total Private") %>%
  mutate(Employed = Employed/1e3) %>%
  autoplot(Employed) +
  labs(y = "Total Private Employed ('000)", title = "US monthly employment data")

us_employment %>%
  filter(Title == "Total Private") %>%
  mutate(Employed = Employed/1e3) %>%
  gg_season(Employed, period = "year") +
  labs(y = "Total Private Employed ('000)", title = "US monthly employment data")


us_employment %>%
  filter(Title == "Total Private") %>%
  mutate(Employed = Employed/1e3) %>%
  gg_subseries(Employed) +
  labs(y = "Total Private Employed ('000)", title = "US monthly employment data")

us_employment %>%
  filter(Title == "Total Private") %>%
  mutate(Employed = Employed/1e3) %>%
  gg_lag(Employed)

us_employment %>%
  filter(Title == "Total Private") %>%
  ACF(Employed) %>%
  autoplot()
#----------------------------------------------------------------------
aus_production

aus_production %>%
  autoplot(Bricks)
  
aus_production %>%
  gg_season(Bricks)

aus_production %>%
  gg_subseries(Bricks)

aus_production %>%
  gg_lag(Bricks, geom = "point")

aus_production %>%
  ACF(Bricks)%>%
  autoplot()

#-----------------------------------------------------
pelt

pelt %>%
  autoplot(Hare)

pelt %>%
  ACF(Hare) %>%
  autoplot()


pelt %>%
  gg_subseries(Hare)

#------------------------------------------------------ 

help(PBS)

PBS %>%
  mutate(Cost = Cost/1e6) %>%
  ggplot(aes(Month, Cost)) +
  geom_line() + labs(y= "Cost of scripts in $ million AUD", title = "Monthly Medicare Australia Prescription  data")
  

PBS %>%
  summarise(TCost = sum(Cost)) %>%
  mutate(TCost = TCost/1e6) %>%
  gg_season(TCost, period = "year")


PBS %>%
  summarise(TCost = sum(Cost)) %>%
  mutate(TCost = TCost/1e6) %>%
  gg_subseries(TCost, period = "year")


PBS %>%
  summarise(TCost = sum(Cost)) %>%
  mutate(TCost = TCost/1e6) %>%
  ACF(TCost) %>%
  autoplot()


PBS %>%
  summarise(TCost = sum(Cost)) %>%
  mutate(TCost = TCost/1e6) %>%
  gg_lag(TCost, geom = "point", period = "year")

#facet_grid(~., scales = "free_y")
us_gasoline
us_gasoline%>%
  autoplot(Barrels)

us_gasoline %>%
  summarise(Barrels = sum(Barrels)) %>%
  gg_season(Barrels, period = "year") +
  facet_grid(~., scales = "free_y")
#--------------------------------------------------

aus_livestock
aus_livestock %>%
  distinct(Animal)

aus_livestock %>%
  filter(Animal == "Pigs", year(Month) == c(1992:1997)) %>%
  ggplot(aes(Month,Animal)) +
  geom_line()

gafa_stock
dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))
dgoog
help("difference")


#----------------------------------------------------------
#Exercise on Decomposition

global_economy%>%
  distinct(Country)

global_economy

global_economy %>%
  filter(Country == "United States")%>%
  autoplot(GDP/Population) +
  labs(y = "$US", title = "GDP per capital in United States")


aus_livestock %>%
  distinct(State)

aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers")%>%
  mutate(Count = Count/1e3) %>%
  autoplot(Count) +
  theme(legend.position = "none") +
  labs(y = "Bulls, bullocks and steers ('000)", title = "Victorian Slaughter")


vic_elec %>%
  autoplot(Demand)

aus_production %>%
  autoplot(Electricity)

canadian_gas


aus_tobacco <- aus_production %>%
  model(
    X_11 = X_13ARIMA_SEATS(Tobacco ~ x11())) %>%
  components(aus_tobacco)

  autoplot(aus_tobacco) +
  labs(title = "Decomposition of Australian Tobacco using X11")
  

  
gas <- tail(aus_production, 5*4) %>% select(Gas);gas

gas

