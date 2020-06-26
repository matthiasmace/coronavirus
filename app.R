library(shiny)
#library(shinyjs)
library(curl)
library(data.table)
library(ggplot2)
library(shinythemes)
library(ggiraph)
#
library(wbstats)
library(countrycode)
#library(R0)
library(EpiEstim)
library(rworldmap)
library(countrycode)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(corrgram)
library(ggrepel)
#
#library(oceanis)
#library(leaflet)
#library(sf)
#library(classInt)
#library(leaflet.extras)
#library(stringr)
#
library(shinyBS)


#
options(shiny.sanitize.errors = TRUE)
#
covdat <- fread('https://covid.ourworldindata.org/data/ecdc/full_data.csv')
covdat[is.na(covdat)] <- 0
covdat$date <- as.Date(covdat$date, format = "%Y-%m-%d")
#add iso2 countrycodes
covdat$iso2c <- countrycode(covdat$location, origin = 'country.name', destination ='iso2c')
#get continents
covdat$continent <- countrycode(covdat$location, origin = 'country.name', destination ='continent')
#get population_2018
pop_data <- wb(country = c(unique(covdat$iso2c)), indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)
covdat <- merge(covdat, pop_data[,c('iso2c', 'value')], by='iso2c')
names(covdat)[names(covdat) == "value"] <- "population"

#get values corrected for population
covdat$new_cases_percapita <- covdat$new_cases / covdat$population
covdat$new_deaths_percapita <- covdat$new_deaths / covdat$population
covdat$total_cases_percapita <- covdat$total_cases / covdat$population
covdat$total_deaths_percapita <- covdat$total_deaths / covdat$population

countries_max_cases <- aggregate(covdat$total_cases, by=list(Category=covdat$location), FUN=max)

# map data

#map.df <- pop_data[, c("iso2c", "value")]
#map.df <- dplyr::inner_join(map.df, aggregate(covdat[, c("iso2c", "total_cases")], by = list(Category = covdat$location), FUN = max))[, c("iso2c", "value", "total_cases")]
#map.df <- dplyr::inner_join(map.df, aggregate(covdat[, c("iso2c", "total_deaths")], by = list(Category = covdat$location), FUN = max))[, c("iso2c", "value", "total_cases", "total_deaths")]
#names(map.df) <- c("iso2c", "population", "cases", "deaths")
#map.df$cases_percapita <- map.df$cases/map.df$population
#map.df$deaths_percapita <- map.df$deaths/map.df$population
load("map.df.2.RData")

# select only countries with 100 or more cases
countries <- countries_max_cases[countries_max_cases$x>=1,]$Category

modifdate <- max(covdat$date)

mindate <- min(covdat$date)
maxdate <- max(covdat$date)

# by continent
continents.df <- aggregate(covdat[, c("new_cases", "new_deaths", "total_cases", "total_deaths", "total_cases_percapita", "total_deaths_percapita")], by=list(location = covdat$continent, date = covdat$date), FUN = sum)

# whole world
world.df <- aggregate(covdat[, c("new_cases", "new_deaths", "total_cases", "total_deaths", "total_cases_percapita", "total_deaths_percapita")], by=list(date = covdat$date), FUN = sum)
world.df$location <- "World"

###   TESTS DATA
tests.df <- as.data.frame(read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv"))
tests.df$Date <- as.Date(tests.df$Date)
TTT <- inner_join(covdat, tests.df, by = c("iso2c"= "ISO.code", "date" = "Date"))


################    FRANCE Data
##### departements
##  Hospit data
france.df <- as.data.frame(read.csv("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header = T, sep =";"))
#france.df <- as.data.frame(read.csv("63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header = T, sep =";"))
france.df$jour <- as.Date(france.df$jour)
#france.df$dep <- as.character(as.numeric(france.df$dep))
france.regions <- as.data.frame(read.csv("https://www.data.gouv.fr/en/datasets/r/987227fb-dcb2-429e-96af-8979f97c9c84", stringsAsFactors = FALSE))
#france.regions <- as.data.frame(rbind(france.regions, c("75", "Paris", "11", "\303\216le-de-France")))
#Encoding(france.regions$region_name) <- "Latin-ASCII"
#france.regions$region_name <- iconv(france.regions$region_name,from="Latin",to="ASCII//TRANSLIT")
france.df <- inner_join(france.df, france.regions, by = c("dep"= "num_dep"))
france.df$region_name <- gsub("La R\303\251union", "La Reunion", france.df$region_name)
france.df$region_name <- gsub("Auvergne-Rh\303\264ne-Alpes", "Auvergne-Rhone-Alpes", france.df$region_name)
france.df$region_name <- gsub("Provence-Alpes-C\303\264te d'Azur", "Provence-Alpes-Cote d'Azur", france.df$region_name)
france.df$region_name <- gsub("Bourgogne-Franche-Comt\303\251", "Bourgogne-Franche-Comte", france.df$region_name)
france.df$region_name <- gsub("\303\216le-de-France", "Ile-de-France", france.df$region_name)

#
pop.dep <- as.data.frame(read.csv("./data_france/ensemble/Departements.csv", header = T, sep =";"))
france.df <- inner_join(france.df, pop.dep, by = c("dep"= "CODDEP"))[, c("dep","DEP", "region_name","sexe","jour", "hosp","rea","rad","dc","PTOT")]
#
lits.dep <- as.data.frame(read.csv("./data_france/Lits_2013_2018.csv", header = T, sep =";"))
france.df <- inner_join(france.df, lits.dep, by = c("dep"= "dep"))
#
##### regions
regions.df <- aggregate(list(france.df[, -c(1:5)]), by=list(Sexe = france.df$sexe, Region = france.df$region_name, jour = france.df$jour), FUN=sum)
#pop.reg <- as.data.frame(read.csv("./data_france/ensemble/Regions.csv", header = T, sep =";"))

########################################################################################################
# Define UI for app that draws a histogram ----
#ui <-
#  ,fluidPage(title = "COVID-19 Pandemics for the P"

ui <- fluidPage(title = "COVID-19 Pandemics for the People"
, navbarPage("COVID-19"
      , tabPanel("World Data"
      #     useShinyjs()  # Set up shinyjs
          #theme = shinytheme("flatly")
      #    tags$style("[type = 'number'] {font-size:50px;height:50px;}")
      #    , tags$style("#myNumericInput {font-size:50px;height:50px;}"),
   , fluidRow(
    column(12,
      h1("Covid-19 for People", align="center")
      , h2("SARS-CoV-2 pandemics data display & analysis Webpage for the people", align="center")
      , p("Data from",
              a("Our World in Data",
                href="https://ourworldindata.org/coronavirus")
                  , "| Link to the dataset (last updated "
                  , modifdate
                  , "):"
                  , a("https://covid.ourworldindata.org/data/ecdc/full_data.csv"
                  ,  href = "https://covid.ourworldindata.org/data/ecdc/full_data.csv")
          , "&"
              , a("WorldBank"
                , href="https://data.worldbank.org")
          , "| Shiny app by Tomasz Suchan & Matthias Mace"
              , a("@tomaszsuchan",
                href="https://twitter.com/tomaszsuchan")
              , a("| Matthias FB",
                href="https://www.facebook.com/matthias.mace.5"),
              align = "center"
         )
       )
     )
  , fluidRow(
    sidebarLayout(
      sidebarPanel(width = 3
                  , radioButtons(inputId = "echelle_world"
                                , label = "Analysis scale:"
                                , choices = c("Country" = "country"
                                , "Continent" = "continent"
                                , "World" = "world"
                                )
                                , selected = "country"
                                , inline = TRUE
                                )
                  , radioButtons(inputId = "data_column"
                                , label = "Data to show:"
                                , choices = c("Total cases" = "total_cases"
                                , "New cases" = "new_cases"
                                , "Total deaths" = "total_deaths"
                                , "New deaths" = "new_deaths"
                                )
                                , selected = "total_cases"
                                , inline = TRUE
                                )
                    , conditionalPanel(
                                  condition = "input.echelle_world == 'continent'"
                                  , selectInput(inputId = "continents_sel"
                                        , label = "Continents:"
                                        , list('Europe'
                                               , 'Africa'
                                               , 'Americas'
                                               , 'Asia'
                                               , 'Oceania'
                                               )
                                          , selected = c("Africa", "Americas", "Asia", "Europe", "Oceania"
                                                   )
                                          , multiple = TRUE
                                                  )
                                                  )
                    , conditionalPanel(
                                condition = "input.echelle_world == 'country'"
                                , selectInput(inputId = "countries_sel"
                                  , label = "Countries (with at least 1 case):"
                                    , list('Europe' = unique(covdat[covdat$continent == 'Europe',]$location)
                                          , 'Africa' = unique(covdat[covdat$continent == 'Africa',]$location)
                                          , 'Americas' = unique(covdat[covdat$continent == 'Americas',]$location)
                                          , 'Asia' = unique(covdat[covdat$continent == 'Asia',]$location)
                                          , 'Oceania' = unique(covdat[covdat$continent == 'Oceania',]$location)
                                          )
                                    , selected = c("France", "Italy", "Germany", "Spain", "Poland", "South Korea", "United Kingdom", "United States"
                                                  )
                                    , multiple = TRUE
                                          )
                                          )
                   , strong("Plot options:")
                   , em("For curves (multiple selections allowed)")
                               , checkboxInput(inputId="log"
                                 , label = "Plot y axis on log scale", value = FALSE)
                               , checkboxInput(inputId="percapita",
                                 label = "Correct for population size", value = FALSE)
                  , checkboxInput(inputId="dailyscale",
                                 label = "Plot daily breaks on x axis", value = FALSE)
                  , checkboxInput(inputId="sync",
                                 label = "Synchronize national epidemics (minimal cases/deaths to begin with)", value = FALSE)
                  , numericInput(inputId = "num.min"
                                , label = ""
                                , value = 10
                                )
                   , hr(style="border-color: black")
                   , checkboxInput(inputId="R0",
                                 label = "Sliding R0 computation (select 'new_cases' or 'new_deaths')
                                 \n Please be patient, it takes up to several minutes !
                                 \n (remove South Korea & China before if performing on death toll)
                                 \n (if it does not work, increase from 10 to at least 100 the minimal number above)"
                                 #(choose the computing window in days)
                                 , value = FALSE)
                   , column(5
                        , numericInput(inputId = "SI.min"
                                 , label = "Serial Interval Min"
                                 , value = 4
                                 )
                            )
                   , column(5
                        , numericInput(inputId = "SI.max"
                                 , label = "Serial Interval Max"
                                 , value = 8
                                 )
                            )
                   , numericInput(inputId = "window.R0"
                                 , label = ""
                                 , value = 3
                                 )
                   , hr(style="border-color: black")
                   , strong("Select Socio-Economic Variable to Compare")
                   , selectizeInput(inputId = "socialvar"
                                 , label = "Select variable"
                                 , choices = c("NONE",
                                 names(map.df.2)[-c(1:3)]
                                 )
                                 , selected = c("NONE")
                                 )
                    , checkboxInput(inputId="map"
                                 , label = "World Map (select one WorldBank data)"
                                 , value = FALSE)
                  , checkboxInput(inputId="xyplot"
                                 , label = "XY-plot (select one WorldBank data)"
                                , value = FALSE)
                  , checkboxInput(inputId="corrmap",
                                  label = "Cross-Correlations (all WorldBank data)", value = FALSE)
                  ),
      mainPanel(width = 9,
                fluidRow(
                  plotOutput(outputId = "worldplot", width="100%", height=750)
                ),
                fluidRow(
                  sliderInput(inputId="dates",
                               label="Dates:",
                               min = mindate,
                               max = maxdate,
                               value = c(as.Date("2020-02-15", format = "%Y-%m-%d"),maxdate),
                               timeFormat = "%F",
                               width="100%")
                               )
                          )
                  )
            )
#,fluidPage(title = "Component 2")
  )
      , tabPanel("Donnees Francaises"
        , fluidRow(
        column(12,
          h1("Covid-19 pour le Tou.te.s", align="center")
          , h2("Page de Visualisation et d'Analyse de donnees de la pandemie due au SARS-CoV-2", align="center")
          , p("Donnees Source",
                  a("Sante Publique France / data.gouv.fr",
                    href="https://www.data.gouv.fr/fr/organizations/sante-publique-france/")
            #      , "| Link to the dataset (last updated "
            #      , modifdate
            #      , "):"
            #      , a("https://covid.ourworldindata.org/data/ecdc/full_data.csv"
            #      ,  href = "https://covid.ourworldindata.org/data/ecdc/full_data.csv")
            #  , "&"
            #      , a("WorldBank"
            #        , href="https://data.worldbank.org")
              , "| Shiny app by Tomasz Suchan & Matthias Mace"
                  , a("@tomaszsuchan",
                    href="https://twitter.com/tomaszsuchan")
                  , a("| Matthias FB",
                    href="https://www.facebook.com/matthias.mace.5"),
                  align = "center"
             )
           )
         )
         , fluidRow(
         sidebarLayout(
           sidebarPanel(width = 3
                        , radioButtons(inputId = "echelle"
                                    , label = "Echelle d'analyse:"
                                    , choices = c("Region" = "region"
                                    , "Departement" = "departement"
                                    )
                                    , selected = "departement"
                                    , inline = TRUE
                                    )
                        , radioButtons(inputId = "data_column_fr"
                                     , label = "Donnees a montrer :"
                                     , choices = c("Hospitalises" = "hosp"
                                     , "Saturation Hospitalisation" = 'hosp_sat'
                                     , "Reanimation" = "rea"
                                     , "Saturation Reanimation" = 'rea_sat'
                                     , "Sorties" = "rad"
                                     , "Decedes" = "dc"
                                     , "Grouper les 4 categories (histogramme)" = "all"
                                     )
                                     , selected = "hosp"
                                     , inline = TRUE
                                     )
                        , radioButtons(inputId = "sexe"
                                     , label = "Sexe"
                                     , choices = c("Tous" = 0
                                              , "Femmes" = 2
                                              , "Hommes" = 1
                                              )
                                    , selected = 0
                                    , inline = TRUE
                                    )
                        , conditionalPanel(
                              condition = "input.echelle == 'departement'"
                              , selectInput(inputId = "dep_sel"
                                    , label = "Departements (with at least 1 case):"
                                        , list(
                                            'Auvergne-Rhone-Alpes'	= unique(france.df[france.df$region_name == 'Auvergne-Rhone-Alpes',]$dep)
                                            , 'Bourgogne-Franche-Comte'	= unique(france.df[france.df$region_name == 'Bourgogne-Franche-Comte',]$dep)
                                            , 'Bretagne'	= unique(france.df[france.df$region_name == 'Bretagne',]$dep)
                                            , 'Centre-Val de Loire'	= unique(france.df[france.df$region_name == 'Centre-Val de Loire',]$dep)
                                            , 'Corse'	= unique(france.df[france.df$region_name == 'Corse',]$dep)
                                            , 'Grand Est'	= unique(france.df[france.df$region_name == 'Grand Est',]$dep)
                                            , 'Guadeloupe'	= unique(france.df[france.df$region_name == 'Guadeloupe',]$dep)
                                            , 'Guyane'	= unique(france.df[france.df$region_name == 'Guyane',]$dep)
                                            , 'Hauts-de-France'	= unique(france.df[france.df$region_name == 'Hauts-de-France',]$dep)
                                            , 'Ile-de-France'	= unique(france.df[france.df$region_name == 'Ile-de-France',]$dep)
                                            , 'La Reunion'	= unique(france.df[france.df$region_name == 'La Reunion',]$dep)
                                            , 'Martinique'	= unique(france.df[france.df$region_name == 'Martinique',]$dep)
                                            , 'Normandie'	= unique(france.df[france.df$region_name == 'Normandie',]$dep)
                                            , 'Nouvelle-Aquitaine'	= unique(france.df[france.df$region_name == 'Nouvelle-Aquitaine',]$dep)
                                            , 'Occitanie'	= unique(france.df[france.df$region_name == 'Occitanie',]$dep)
                                            , 'Pays de la Loire'	= unique(france.df[france.df$region_name == 'Pays de la Loire',]$dep)
                                            , "Provence-Alpes-Cote d'Azur"	= unique(france.df[france.df$region_name == "Provence-Alpes-Cote d'Azur",]$dep)
                                            )
                                    , selected = c(66, 31, 47, 11, 75, 67, 68
                                        )
                                    , multiple = TRUE
                                    )
                                    )
                                    ,     conditionalPanel(
                                          condition = "input.echelle == 'region'"
                                          , selectInput(inputId = "region_sel"
                                                , label = "Regions (with at least 1 case):"
                                                    , list(
                                                    "Auvergne-Rhone-Alpes"
                                                    ,"Bourgogne-Franche-Comte"
                                                    ,"Bretagne"
                                                    ,"Centre-Val de Loire"
                                                    ,"Corse"
                                                    ,"Grand Est"
                                                    ,"Guadeloupe"
                                                    ,"Guyane"
                                                    ,"Hauts-de-France"
                                                    ,"Ile-de-France"
                                                    ,"La Reunion"
                                                    ,"Martinique"
                                                    ,"Normandie"
                                                    ,"Nouvelle-Aquitaine"
                                                    ,"Occitanie"
                                                    ,"Pays de la Loire"
                                                    ,"Provence-Alpes-Cote d'Azur"
                                                        )
                                                , selected = c("Grand Est", "Ile-de-France", "Nouvelle-Aquitaine", "Occitanie"
                                                    )
                                                , multiple = TRUE
                                                )
                                                )
                        , strong("Plot options:")
                        , em("For curves (multiple selections allowed)")
                                    , checkboxInput(inputId="log_fr"
                                      , label = "Plot y axis on log scale", value = FALSE)
                                    , checkboxInput(inputId="percapita_fr",
                                      label = "Correct for population size", value = FALSE)
                       , checkboxInput(inputId="dailyscale_fr",
                                      label = "Plot daily breaks on x axis", value = FALSE)
                       , checkboxInput(inputId="sync_fr",
                                      label = "Synchroniser les epidemies departementales/regionales (nombre minimal de cas/morts pour definir le debut)", value = FALSE)
                       , numericInput(inputId = "num.min.fr"
                                     , label = ""
                                     , value = 10
                                     )
                       , checkboxInput(inputId="xyplot_fr"
                                     , label = "XY-plot (e.g. regions en tension)"
                                     , value = FALSE
                                     )
                        , hr(style="border-color: black")
                        , checkboxInput(inputId="R0_fr",
                                      label = "Calcul du R0 en fenetres glissantes (soyez patients svp, cela prend jusqu'à quelques minutes)"
                                      #(choose the computing window in days)
                                      , value = FALSE)
                        , column(5
                             , numericInput(inputId = "SI.min.fr"
                                      , label = "SI minimal"
                                      , value = 4
                                      )
                                 )
                        , column(5
                             , numericInput(inputId = "SI.max.fr"
                                      , label = "SI maximal"
                                      , value = 8
                                      )
                                 )
                        , numericInput(inputId = "window.R0"
                                      , label = ""
                                      , value = 3
                                      )
                        , hr(style="border-color: black")
                  #      , strong("Select Socio-Economic Variable to Compare")
                  #      , selectizeInput(inputId = "socialvar"
                  #                    , label = "Select variable"
                  #                    , choices = c("NONE",
                  #                    names(map.df.2)[-c(1:3)]
                  #                    )
                  #                    , selected = c("NONE")
                  #                    )
                         , checkboxInput(inputId="map.fr"
                                      , label = "Affichage des donnees sur une carte"
                                      , value = FALSE
                                      )
                  #     , checkboxInput(inputId="corrmap",
                  #                     label = "Cross-Correlations (all WorldBank data)", value = FALSE
                  #                    )
                       )
           , mainPanel(width = 9
                     , fluidRow(
          #            girafeOutput(outputId = "franceplot", width="100%", height=750
          #                        )
                     plotOutput(outputId = "franceplot", width="100%", height=750
                                , click = "plot_click"
                           )
                              #    , verbatimTextOutput("info")
                     )
                     , fluidRow(
                       sliderInput(inputId="dates",
                                    label="Dates:",
                                    min = mindate,
                                    max = maxdate,
                                    value = c(as.Date("2020-02-15", format = "%Y-%m-%d"),maxdate),
                                    timeFormat = "%F",
                                    width="100%")
                                    )
                #      ,     conditionalPanel(
                #          condition = "input.map.fr"
                #          , leafletOutput("mymap")
                #                )


, bsModal("modalExample", "Your plot", "go", size = "large",plotOutput("plot"),downloadButton('downloadPlot', 'Download'))



                               )
                       )
                 )



                               )
                        )
                  )



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
          #  onevent("mouseover", "R0", alert("aide R0"))
          #  onevent("mouseleave", "R0", alert("aide R0"))

          ############################### world
          # 1. It is "reactive" and therefore should be automatically
          #    re-executed when inputs (input$countries) change
          # 2. Its output type is a plot
          ###############################




  output$worldplot <- renderPlot({

#      dates_range <- seq(input$dates[1], input$dates[2], by = "days")
              if(input$echelle_world == "continent")
                  {
                  data_selected <- continents.df[(continents.df$location %in% input$continents_sel),]
                  }
                  else if(input$echelle_world == "country")
                  {
                  data_selected <- covdat[(covdat$location %in% input$countries_sel),]
                  }
                  else
                  {
                  data_selected <- world.df
                  }


      if(input$map | input$xyplot){
        sPDF <- joinCountryData2Map(map.df.2
        , joinCode = "ISO2"
        , nameJoinColumn = "iso2c")
        # creating a user defined colour palette
        # op <- palette(c("green", "yellow", "orange", "red"))
        #
        if(input$socialvar == "NONE"){
            var = input$data_column
            sPDF@data[["data_to_plot"]] <- sPDF@data[[var]]
            # sPDF@data[["data_to_plot"]] <- cut(sPDF@data[[var]]
            #    , breaks = 4
            #    , cutVector
            #    , include.lowest=TRUE
            #  )
            # levels(sPDF@data[["data_to_plot"]]) <- c("low","med", "high", "vhigh")
            } else {
            var <- input$data_column
            social.var <- input$socialvar
            sPDF@data[["data_to_plot"]] <- sPDF@data[[var]]/sPDF@data[[social.var]]
            # sPDF@data[["data_to_plot"]] <- cut(sPDF@data[[var]]/sPDF@data[[social.var]]
            #    , breaks = 4
            #    , cutVector
            #    , include.lowest=TRUE
            #  )
            #  levels(sPDF@data[["data_to_plot"]]) <- c("low","med", "high", "vhigh")

            }
          }

      if(input$corrmap){

            }

    if(input$R0){
      #  COLS <- c(which(names(covdat) %in% c("date", "location", input$data_column)))
      #  DAT.0 = covdat[covdat$location %in% input$countries_sel, COLS]
        #DAT.0 = covdat[covdat$location %in% input$countries_sel, c("date", "location", "new_deaths")]


        eval(parse(text = paste("DAT.0 = data_selected[, c('date', 'location', '", input$data_column, "')]", sep = "")))


  #      if(input$data_column == "new_cases"){
  #          DAT.0 = data_selected[, c(2, 3, 4)]
  #      } else if(input$data_column == "new_deaths") {
  #          DAT.0 = data_selected[, c(2, 3, 5)]
  #      } else {stop(safeError(("Incompatible Data to show / plot option combination")))}
        #
        names(DAT.0) <- c("date", "location", "data")
        RES <- list()
        #
        config <- make_config(list(mean_si = (mean(c(input$SI.min, input$SI.max))), std_mean_si = 1,
                                   min_mean_si = input$SI.min, max_mean_si = input$SI.max,
                                   std_si = 1.5, std_std_si = 0.5,
                                   min_std_si = 0.5, max_std_si = 2.5))
        #
        #window = input$window.R0
        #
        for(c in unique(DAT.0$location)){
          DAT.1 <- DAT.0[DAT.0$location == c  & (DAT.0$data >= input$num.min), ]
          rownames(DAT.1) <- DAT.1$date
          DAT.2 <- DAT.1[, -c(1, 2)]
          es_uncertain_si <- estimate_R(DAT.2,
                                         method = "uncertain_si",
                                         config = config)
          #
          max.length <- max(table(DAT.0[DAT.0$data > input$num.min, c("location")]))
          df <- rbind(do.call("rbind"
                                          , replicate(n = (max.length - dim(DAT.1)[1])
                                                      , rep(c(NA), times = dim(es_uncertain_si$R)[2])
                                          , simplify = FALSE)
                                          )
                                    , as.matrix(es_uncertain_si$R)
                                    )

          RES[[c]] <- data.frame("J" <- seq(dim(df)[1])
          										, "BEGIN" = df[, "t_start"]
          										, "END" = df[, "t_end"]
                              , "R0_point" = df[, "Median(R)"]
                              , "R0_low" = df[, "Quantile.0.05(R)"]
                              , "R0_high" = df[, "Quantile.0.95(R)"]
                              )
                  #rownames(RES[[c]]) <- sort(unique(DAT.0$date))
                            }

    for(c in names(RES)){
      RES[[c]]$location <- c
    }
    RES <- do.call("rbind", RES)
    names(RES)[1] <- "J"
    RES$J <- RES$J - length(unique(RES$J))  ##  reverse timescale
  }
###############################

    if(input$sync){
  #        before <- which(data_selected$total_cases == 0)
          before <- which(data_selected$total_cases < input$num.min)
          data_selected.sync <- data_selected[-before, ]
          data_selected.sync$J <- 0
          for (c in unique(data_selected.sync$location)){
          				L <- dim(data_selected.sync[data_selected.sync$location == c, ])[1]
          				data_selected.sync[data_selected.sync$location == c, "J"] <- seq(length = L)
                  }
          dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          covdat_selected <- data_selected.sync[data_selected.sync$date %in% dates_range, ]
          } else {
          dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          covdat_selected <- data_selected[data_selected$date %in% dates_range, ]
          }

    ######
    myplot <- ggplot(covdat_selected) +
          #scale_color_brewer(palette="Paired", name = "Country")
          scale_color_discrete(name = "Locations:") +
          theme_linedraw(base_size = 15)
    ######
    #    dates_range <- seq(input$dates[1], input$dates[2], by = "days")
    #    covdat_selected <- data_selected[(data_selected$location %in% input$countries_sel) & (data_selected$date %in% dates_range),]



    if(input$sync){
    if(input$percapita){
    myplot <- myplot + labs(x = "Date", y = "Number per capita")
    if(input$data_column == "total_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_cases_percapita, colour = location), size=1)+
                        ggtitle("Number of Confirmed Cases Per Capita throughout time"
                                , subtitle = "the raw number is divided by the country population")
                                        }
    else if(input$data_column == "new_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_cases_percapita, colour = location), size=1)+
                        ggtitle("Number of Daily New Confirmed Cases Per Capita"
                                , subtitle = "the raw number is divided by the country population")}
    else if(input$data_column == "total_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_deaths_percapita, colour = location), size=1)+
                        ggtitle("Number of Deaths Per Capita throughout time"
                                , subtitle = "the raw number is divided by the country population")}
    else if(input$data_column == "new_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_deaths_percapita, colour = location), size=1)+
                        ggtitle("Number of Daily New Deaths Per Capita"
                                , subtitle = "the raw number is divided by the country population")}
    }  else {
    myplot <- myplot + labs(x = "Date", y = "Raw Number")
    if(input$data_column == "total_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_cases, colour = location), size=1)+
                        ggtitle("Number of Confirmed Cases throughout time")
                        }
    else if(input$data_column == "new_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_cases, colour = location), size=1)+
                        ggtitle("Number of Daily New Confirmed Cases")
                        }
    else if(input$data_column == "total_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_deaths, colour = location), size=1)+
                        ggtitle("Number of Deaths throughout time")
                        }
    else if(input$data_column == "new_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_deaths, colour = location), size=1)+
                        ggtitle("Number of Daily New Deaths")
                        }
    }
    } else if(input$R0){
        myplot <- ggplot(data = RES, aes(x = J, y = R0_point, colour = location)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin=R0_low, ymax=R0_high, colour = location), linetype=2, alpha=0.1)+
        xlim(0, NA)+
        ylim(-1, NA)+
        geom_hline(
              yintercept = 1, linetype = 5, colour = "black",
              )+
        geom_text(aes(min(J), 1, label = "R0 = 1", vjust = -1), colour = "black")+
        labs(x = "Time in days (from past to present)", y = "Basic Reproduction Number (R) estimates")+
        xlim(-length(unique(RES$J)), 0)+
        theme_minimal()
      } else {
    if(input$percapita){
      myplot <- myplot + labs(x = "Date", y = "Number per capita")
      if(input$data_column == "total_cases"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = total_cases_percapita, colour = location), size=1)+
                          ggtitle("Number of Confirmed Cases Per Capita throughout time"
                          , subtitle = "the raw number is divided by the country population")
                          }
      else if(input$data_column == "new_cases"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = new_cases_percapita, colour = location), size=1)+
                          ggtitle("Number of Daily New Confirmed Cases Per Capita"
                                  , subtitle = "the raw number is divided by the country population")
                                  }
      else if(input$data_column == "total_deaths"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = total_deaths_percapita, colour = location), size=1)+
                          ggtitle("Number of Deaths Per Capita throughout time"
                                  , subtitle = "the raw number is divided by the country population")
                                  }
      else if(input$data_column == "new_deaths"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = new_deaths_percapita, colour = location), size=1)+
                          ggtitle("Number of Daily New Deaths Per Capita"
                                  , subtitle = "the raw number is divided by the country population")
                                  }
        }
        else{
          myplot <- myplot + labs(x = "Date", y = "Raw Number")
          if(input$data_column == "total_cases"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = total_cases, colour = location), size=1)+
                              ggtitle("Number of Confirmed Cases throughout time")
                              }
          else if(input$data_column == "new_cases"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = new_cases, colour = location), size=1)+
                              ggtitle("Number of Daily New Confirmed Cases")
                              }
          else if(input$data_column == "total_deaths"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = total_deaths, colour = location), size=1)+
                              ggtitle("Number of Deaths throughout time")
                              }
          else if(input$data_column == "new_deaths"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = new_deaths, colour = location), size=1)+
                              ggtitle("Number of Daily New Deaths")
                              }
        }
      }

if(input$map){
  if(input$data_column == "total_cases" | input$data_column == "total_deaths"){
      colourPalette <- rev(brewer.pal(10,'RdYlGn'))
      if(input$socialvar == "NONE"){
        myplot <- mapCountryData( sPDF
        , nameColumnToPlot="data_to_plot"
#            , catMethod="quantile"
        , catMethod="logFixedWidth"
        , mapTitle = var  #"Deaths Toll"
        , colourPalette = colourPalette
        , oceanCol="lightblue"
        , missingCountryCol="white"
        )
      } else {
        myplot <- mapCountryData( sPDF
        , nameColumnToPlot="data_to_plot"
#                 , catMethod="quantile"
        , catMethod="logFixedWidth"
        , mapTitle = paste(var, "/", social.var)#"Deaths Toll"
        , colourPalette = colourPalette
        , oceanCol="lightblue"
        , missingCountryCol="white"
        )
      }
    } else {stop(safeError(("Incompatible Data to show / plot option combination")))}
}

  if(input$xyplot){
    if(input$data_column == "total_cases" | input$data_column == "total_deaths"){
      #var="total_cases"; socialvar = "Population in the largest city (% of urban population)" ;
      xy.df <- map.df.2[, names(map.df.2) %in% c(var, social.var, "location", "continent")]
      names(xy.df) <- c("continent", "location", "y", "x")
      xy.df$fcontinent <- as.factor(xy.df$continent)
        myplot <- ggplot(xy.df, aes(x = x, y = y, colour = continent))+
                    geom_point()+
                    geom_label_repel(aes(label = location)
                    #, box.padding   = 0.35
                    , point.padding = 0.5
                    , segment.color = 'grey50'
                    , size = 2)+
                    labs(x = social.var, y = var)+
                    theme_minimal()
    } else {stop(safeError(("Incompatible Data to show / plot option combination")))}
}
    if(input$corrmap){
      labs = names(map.df.2[, -c(1:3)])
        myplot <- corrgram(map.df.2[, -c(1:3)]
          #          , lower.panel = panel.shade
                    , lower.panel = NULL
                    , upper.panel = panel.pie
          #          , diag.panel = panel.minmax
                    , text.panel = panel.txt
                    , outer.labels = list(bottom=list(labels=labs,cex = 0.5, srt = 15),
                                left=list(labels=labs,cex = 0.5, srt = -75))
                    )
      }

      if(input$log){
        myplot <- myplot + scale_y_log10()
      }
      if(input$dailyscale){
        myplot <- myplot + scale_x_date(date_minor_breaks = "1 day")
    }
      return(myplot)
  }
)

###############################   France    ###############################

output$franceplot <- renderPlot({
#output$franceplot <- renderGirafe({

      if(input$echelle == "departement"){
  #      dates_range <- seq(input$dates[1], input$dates[2], by = "days")
        data_selected <- france.df[(france.df$dep %in% input$dep_sel) & (france.df$sexe == input$sexe),]
        data_selected$location <- data_selected$dep
      } else {
        data_selected <- regions.df[(regions.df$Region %in% input$region_sel) & (regions.df$Sexe == input$sexe),]
        data_selected$location <- data_selected$Region
      }

      if(input$R0_fr){
        #  COLS <- c(which(names(covdat) %in% c("date", "location", input$data_column)))
        #  DAT.0 = covdat[covdat$location %in% input$countries_sel, COLS]
          #DAT.0 = covdat[covdat$location %in% input$countries_sel, c("date", "location", "new_deaths")]

          DAT.0 <- data_selected[, c("jour", "location", input$data_column_fr)]
          names(DAT.0) <- c("date", "location", "data")
          RES <- list()
          #
          config <- make_config(list(mean_si = (mean(c(input$SI.min.fr, input$SI.max.fr))), std_mean_si = 1,
                                     min_mean_si = input$SI.min.fr, max_mean_si = input$SI.max.fr,
                                     std_si = 1.5, std_std_si = 0.5,
                                     min_std_si = 0.5, max_std_si = 2.5))
          #
          #window = input$window.R0_fr
          #
          for(c in unique(DAT.0$location)){
            DAT.1 <- DAT.0[DAT.0$location == c  & (DAT.0$data >= input$num.min.fr), ]
            rownames(DAT.1) <- DAT.1$date
            DAT.1 <- DAT.1[, -c(1, 2)]
            es_uncertain_si <- estimate_R(DAT.1,
                                           method = "uncertain_si",
                                           config = config)
            #
            max.length <- max(table(DAT.0[DAT.0$data > input$num.min.fr, c("location")]))
            df <- rbind(do.call("rbind"
                                            , replicate(n = (max.length - length(DAT.1))
                                                        , rep(c(NA), times = dim(es_uncertain_si$R)[2])
                                            , simplify = FALSE)
                                            )
                                      , as.matrix(es_uncertain_si$R)
                                      )

            RES[[c]] <- data.frame("J" <- seq(dim(df)[1])
            										, "BEGIN" = df[, "t_start"]
            										, "END" = df[, "t_end"]
                                , "R0_point" = df[, "Median(R)"]
                                , "R0_low" = df[, "Quantile.0.05(R)"]
                                , "R0_high" = df[, "Quantile.0.95(R)"]
                                )
                    #rownames(RES[[c]]) <- sort(unique(DAT.0$date))
                              }

      for(c in names(RES)){
        RES[[c]]$location <- c
      }
      RES <- do.call("rbind", RES)
      names(RES)[1] <- "J"
      RES$J <- RES$J - length(unique(RES$J))  ##  reverse timescale
    }





      if(input$sync_fr){

          before <- which(data_selected$hosp < input$num.min.fr)
          data_selected.sync <- data_selected[-before, ]
          data_selected.sync$J <- 0
          for (c in unique(data_selected.sync$location)){
              L <- dim(data_selected.sync[data_selected.sync$location == c, ])[1]
              data_selected.sync[data_selected.sync$location == c, "J"] <- seq(length = L)
              }
      #        dates_range <- seq(input$dates[1], input$dates[2], by = "days")
      #        data_selected <- data_selected.sync[(data_selected.sync$location %in% input$countries_sel) & (data_selected.sync$date %in% dates_range),]
            data_selected.sync$jour <- data_selected.sync$J
            data_selected <- data_selected.sync
            } #else {
      #      if(input$echelle == "departement"){
      #          dates_range <- seq(input$dates[1], input$dates[2], by = "days")
      #          data_selected <- france.df[(france.df$dep %in% input$dep_sel) & (france.df$sexe == input$sexe),]
      #          data_selected$location <- data_selected$dep
      #        } else {
      #        }
      #      }


      ######
    #  myplot <- ggplot(data_selected, aes(tooltip = location, data_id = location,)) +
    myplot <- ggplot(data_selected) +
            #scale_color_brewer(palette="Paired", name = "Country")
            scale_color_discrete(name = paste(input$echelle, ":")) +
            theme_linedraw(base_size = 15)
      ######
      #    dates_range <- seq(input$dates[1], input$dates[2], by = "days")
      #    covdat_selected <- covdat[(covdat$location %in% input$countries_sel) & (covdat$date %in% dates_range),]


      if(input$data_column_fr =="all"){
        data <- melt(data = data_selected, id.vars = c("location", "jour"), measure.vars = c("hosp", "rea", "rad", "dc"))
        myplot <- ggplot(data, aes(fill = variable, y = value, x = jour))+
                  geom_bar_interactive(position = "stack", stat = "identity")
  #    if(input$sync){
  #    if(input$percapita){
      } else if(input$R0_fr){
        myplot <- ggplot(data = RES, aes(x = J, y = R0_point, colour = location)) +
        geom_line(size = 1)+
        geom_ribbon(aes(ymin = R0_low, ymax = R0_high, colour = location), linetype = 2, alpha = 0.1)+
        xlim(0, NA)+
        ylim(-1, NA)+
        geom_hline(
              yintercept = 1, linetype = 5, colour = "black",
              )+
        geom_text(aes(min(J), 1, label = "R0 = 1", vjust = -1), colour = "black")+
              labs(x = "Temps (du passe a aujourd'hui)", y = "Estimation du Nombre  Basique de Reproduction (R0)")+
              xlim(-length(unique(RES$J)), 0)+
              theme_minimal()


        } else {
      if(input$percapita_fr){
        #myplot <- myplot + labs(x = "Date", y = "Number of cases")
        if(input$data_column_fr == "hosp"){
          myplot <- myplot+ geom_line_interactive(mapping = aes(x = jour, y = (hosp*10e5/PTOT), colour = location
                          #    , tooltip = location, data_id = location
                              ), size=1)+
                            labs(x = "Date", y = "Patiens hospitalises / 100.000 habitants")
                            }
        else if(input$data_column_fr == "rea"){
          myplot <- myplot + geom_line(mapping = aes(x = jour, y = rea*10e5/PTOT, colour = location), size=1)+
                            labs(x = "Date", y = "Patiens en reanimation / 100.000 habitants")
                            }
        else if(input$data_column_fr == "rad"){
          myplot <- myplot + geom_line(mapping = aes(x = jour, y = rad*10e5/PTOT, colour = location), size=1)+
                            labs(x = "Date", y = "Patiens sortis / 100.000 habitants")
                            }
        else if(input$data_column_fr == "dc"){
          myplot <- myplot + geom_line(mapping = aes(x = jour, y = dc*10e5/PTOT, colour = location), size=1)+
                            labs(x = "Date", y = "Patiens decedes / 100.000 habitants")
                            }
          } else {
            #myplot <- myplot + labs(x = "Date", y = "Number of cases")
            if(input$data_column_fr == "hosp"){
              myplot <- myplot+ geom_line(mapping = aes(x = jour, y = hosp, colour = location), size=1)+
                                labs(x = "Date", y = "Patiens hospitalises")
                        }
            else if(input$data_column_fr == "hosp_sat"){
            df_poly <- data.frame(x = as.Date(c(0, 0, Inf, Inf), origin = min(data_selected$jour))
                                , y = c(1, Inf, Inf, 1)
                                )
              myplot <- myplot +
                geom_line(mapping = aes(x = jour, y = hosp/(SC_CHR_2018 + SC_AUTRES_2018), colour = location), size=1)+
                                labs(x = "Date", y = "Saturation en Hospitalisation")+
                                geom_polygon(data = df_poly, aes(x, y), fill="red", alpha=0.2)+
                                labs(title = "Saturation en Lits par les patients COVID"
                                    , subtitle = "Zone Rouge : Departement/Region sature(e) (>1 = >100%)"
                                    )
                        }
            else if(input$data_column_fr == "rea"){
              myplot <- myplot + geom_line(mapping = aes(x = jour, y = rea, colour = location), size=1)+
                                 labs(x = "Date", y = "Patiens en reanimation")
                         }
            else if(input$data_column_fr == "rea_sat"){
            df_poly <- data.frame(x = as.Date(c(0, 0, Inf, Inf), origin = min(data_selected$jour))
                                , y = c(1, Inf, Inf, 1)
                                )
              myplot <- myplot + geom_line(mapping = aes(x = jour, y = rea/(Rea_CHR_2018 + Rea_AUTRES_2018), colour = location), size=1)+
                                  labs(x = "Date", y = "Saturation en reanimation")+
                                  geom_polygon(data = df_poly, aes(x, y), fill="red", alpha=0.2)+
                                  labs(title = "Saturation en Lits par les patients COVID"
                                      , subtitle = "Zone Rouge : Departement/Region sature(e) (>1 = >100%)"
                                      )
                         }
            else if(input$data_column_fr == "rad"){
              myplot <- myplot + geom_line(mapping = aes(x = jour, y = rad, colour = location), size=1)+
                                 labs(x = "Date", y = "Patiens sortis")
                         }
            else if(input$data_column_fr == "dc"){
              myplot <- myplot + geom_line(mapping = aes(x = jour, y = dc, colour = location), size=1)+
                                 labs(x = "Date", y = "Patiens decedes")
                         }
                  }
            }
            if(input$map.fr){
                    donnees_monoloc <- rio::import(file = system.file("data/donnees_monoloc.rda", package = "oceanis"))
                    depm <- sf::st_read(dsn = system.file("extdata",
                                                          "dep_francemetro_2018.shp",
                                                          package = "oceanis"),
                                        quiet = TRUE,
                                        stringsAsFactors = FALSE)
                    # import du fond des regions
                    regm <- sf::st_read(dsn = system.file("extdata",
                                                          "reg_francemetro_2018.shp",
                                                          package = "oceanis"),
                                        quiet = TRUE,
                                        stringsAsFactors = FALSE)
                    # import du fond de France metropolitaine
                    fram <- sf::st_read(dsn = system.file("extdata",
                                                          "francemetro_2018.shp",
                                                          package = "oceanis"),
                                        quiet = TRUE,
                                        stringsAsFactors = FALSE)
                    #
                    map.data.fr <-inner_join(donnees_monoloc, france.df, by = c("COD_DEP"= "dep"))
                    #
                    donnees_monoloc.2 <- map.data.fr[map.data.fr$jour  == as.Date(format(Sys.time()-86400, "%Y-%m-%d")) & map.data.fr$sexe  == 0, ]
                    #
                    if(input$echelle == "region"){
                      data.map <- aggregate(list("Hospitalisation" = donnees_monoloc.2$hosp, "Reanimation" = donnees_monoloc.2$rea, "Retour_a_domicile" = donnees_monoloc.2$rad, "Deces" = donnees_monoloc.2$dc, "POP" = donnees_monoloc.2$PTOT), by = list(Region = donnees_monoloc.2$code_region), FUN = sum)
                      #
                      myplot <-      leaflet_ronds_classes(data.map
                      #            , fondMaille = depm[depm$reg %in% c("75","76"),]
                                  , fondMaille = regm
                                          # depm[depm$reg %in% c("93","94"),]
                      #            , fondMailleElargi = regm
                      #            , fondContour = fram
                                  , fondSuppl = regm
                                  , idData = "Region"
                                  , varVolume = "Deces"
                                  , varRatio = "Hospitalisation"
                                  )



                    } else if(input$echelle == "departement"){
                      data.map <- aggregate(list("Hospitalisation" = donnees_monoloc.2$hosp, "Reanimation" = donnees_monoloc.2$rea, "Retour_a_domicile" = donnees_monoloc.2$rad, "Deces" = donnees_monoloc.2$dc), by = list("Departement" = donnees_monoloc.2$COD_DEP, "POP" = donnees_monoloc.2$PTOT), FUN = sum)
                      #
                      myplot <-      leaflet_ronds_classes(data.map
                      #            , fondMaille = depm[depm$reg %in% c("75","76"),]
                                  , fondMaille = depm
                                          # depm[depm$reg %in% c("93","94"),]
                                  , fondMailleElargi = depm
                      #            , fondContour = fram
                      #            , fondSuppl = regm
                                  , idData = "Departement"
                                  , varVolume = "Deces"
                                  , varRatio = "Hospitalisation"
                                  )
                    }
                }

                if(input$xyplot_fr){
                    if(input$echelle == "departement"){
                      DF <- aggregate(france.df[france.df$sexe == input$sexe, c(5:9)]
                                      , by = as.list(france.df[france.df$sexe == input$sexe, c(1, 3, 10, 11:22)])
                                      , FUN = last
                                      )
                      df_poly <- data.frame(x = c(-Inf, Inf, Inf)
                                          , y = c(-Inf, Inf, -Inf)
                                          )
                      MAX <- max(DF$rea, DF$Rea_CHR_2018+DF$Rea_AUTRES_2018)
                      #
                      myplot <- ggplot(DF, aes(rea, Rea_CHR_2018+Rea_AUTRES_2018))+
                                        #geom_abline(slope = mean(DF$ratio)) +
                                        #geom_abline(slope = 1) +
                                        coord_fixed()+
                                        geom_polygon(data=df_poly, aes(x, y), fill="red", alpha=0.2) +
                                        xlim(0, MAX)+ ylim(0, MAX)+
                                        xlab("Patients COVID")+ ylab("Capacite Totale en Lits")+
                                        labs(title = "Saturation en Lits par les patients COVID"
                                            , subtitle = "Zone Rouge : Departements satures"
                                            )+
                                        geom_point(aes(size = rea, colour = dep), show.legend = FALSE)+
                                        #geom_text_repel(aes(label = paste(Region, "(", ratio.pct, "%)")), size=3)+
                                        geom_text_repel(aes(label = dep), size=3)+
                                        theme_minimal()
                    } else {
                      DF <- aggregate(france.df[france.df$sexe == input$sexe, c(5:9)]
                                      , by = as.list(france.df[france.df$sexe == input$sexe, c(1, 3, 10, 11:22)])
                                      , FUN = last
                                      )
                      DF <- aggregate(DF[, -c(1:2)], by = list(Region = DF$region_name), FUN = sum)
                      df_poly <- data.frame(x = c(-Inf, Inf, Inf)
                                          , y = c(-Inf, Inf, -Inf)
                                          )
                      MAX <- max(DF$rea, DF$Rea_CHR_2018+DF$Rea_AUTRES_2018)
                      #
                      myplot <- ggplot(DF, aes(rea, Rea_CHR_2018+Rea_AUTRES_2018))+
                                      #geom_abline(slope = mean(DF$ratio)) +
                                      #geom_abline(slope = 1) +
                                      coord_fixed()+
                                      geom_polygon(data=df_poly, aes(x, y), fill="red", alpha=0.2) +
                                      xlim(0, MAX)+ ylim(0, MAX)+
                                      xlab("Patients COVID")+ ylab("Capacite Totale")+
                                      labs(title = "Saturation en Lits par les patients COVID"
                                          , subtitle = "Zone Rouge : Regions saturees"
                                          )+
                                      geom_point(aes(size = rea, colour = Region), show.legend = FALSE)+
                                      #geom_text_repel(aes(label = paste(Region, "(", ratio.pct, "%)")), size=3)+
                                      geom_text_repel(aes(label = Region), size=3)+
                                      theme_minimal()
                    }
                }

                if(input$log_fr){
                myplot <- myplot + scale_y_log10()
                }
                if(input$dailyscale_fr){
                myplot <- myplot + scale_x_date(date_minor_breaks = "1 day")
                }
                return(
            #      girafe(ggobj = myplot
            #          , options = list(opts_selection(type = "single", only_shiny = FALSE)
            #                          )
            #          )
                myplot + labs(caption = "Donnees Sante Publique France et DREES")
                )
              }
              )
          #    output$info <- renderText({
          #      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
          #    })

    }

shinyApp(ui, server)
