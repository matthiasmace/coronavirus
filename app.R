library(shiny)
#library(shinyjs)
library(curl)
library(data.table)
library(ggplot2)
library(shinythemes)
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
pop_data <- wb(country = unique(covdat$iso2c), indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)
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

################    FRANCE Data
##### departements
france.df <- as.data.frame(read.csv("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header = T, sep =";"))
#france.df <- as.data.frame(read.csv("63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header = T, sep =";"))
france.df$jour <- as.Date(france.df$jour)
#france.df$dep <- as.character(as.numeric(france.df$dep))
france.regions <- as.matrix(read.csv("https://www.data.gouv.fr/en/datasets/r/1c31f420-829e-489e-a19d-36cf3ef57e4a", stringsAsFactors = FALSE))
france.regions <- as.data.frame(rbind(france.regions, c("75", "Paris", "11", "\303\216le-de-France")))
#Encoding(france.regions$nom_region) <- "Latin-ASCII"
#france.regions$nom_region <- iconv(france.regions$nom_region,from="Latin",to="ASCII//TRANSLIT")
france.df <- inner_join(france.df, france.regions, by = c("dep"= "code_departement"))
france.df$nom_region <- gsub("La R\303\251union", "La Reunion", france.df$nom_region)
france.df$nom_region <- gsub("Auvergne-Rh\303\264ne-Alpes", "Auvergne-Rhone-Alpes", france.df$nom_region)
france.df$nom_region <- gsub("Provence-Alpes-C\303\264te d'Azur", "Provence-Alpes-Cote d'Azur", france.df$nom_region)
france.df$nom_region <- gsub("Bourgogne-Franche-Comt\303\251", "Bourgogne-Franche-Comte", france.df$nom_region)
france.df$nom_region <- gsub("\303\216le-de-France", "Ile-de-France", france.df$nom_region)

#
pop.dep <- as.data.frame(read.csv("./data_france/ensemble/Departements.csv", header = T, sep =";"))
france.df <- inner_join(france.df, pop.dep, by = c("dep"= "CODDEP"))[, c("dep","sexe","jour","hosp","rea","rad","dc","nom_departement","code_region","nom_region","PTOT")]
#
##### regions
regions.df <- aggregate(list(france.df$hosp, france.df$rea, france.df$rad, france.df$dc, france.df$PTOT), by=list(Sexe = france.df$sexe, Region = france.df$nom_region, Day = france.df$jour), FUN=sum)
names(regions.df) <- c("Sexe", "Region", "jour", "hosp", "rea", "rad", "dc", "PTOT")

#pop.reg <- as.data.frame(read.csv("./data_france/ensemble/Regions.csv", header = T, sep =";"))


# Define UI for app that draws a histogram ----
#ui <-
#  ,fluidPage(title = "World Data"

ui <- fluidPage(title = "World Data"
, navbarPage("Pandemics for the People"
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
                   , radioButtons(inputId = "data_column"
                                , label = "Data to show:"
                                , choices = c("Total cases" = "total_cases"
                                , "New cases" = "new_cases"
                                , "Total deaths" = "total_deaths"
                                , "New deaths" = "new_deaths"
                                )
                                , selected = "total_cases"
                                )
                   , selectInput(inputId = "countries_sel"
                               , label = "Countries (with at least 1 case):"
                               , list('Europe' = unique(covdat[covdat$continent == 'Europe',]$location)
                                      , 'Africa' = unique(covdat[covdat$continent == 'Africa',]$location)
                                      , 'Americas' = unique(covdat[covdat$continent == 'Americas',]$location)
                                      , 'Asia' = unique(covdat[covdat$continent == 'Asia',]$location)
                                      , 'Oceania' = unique(covdat[covdat$continent == 'Oceania',]$location)
                                    )
                               , selected = c("France", "Italy", "Germany", "Spain", "Poland", "South Korea", "United Kingdom", "United States")
                               , multiple = TRUE
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
                                 label = "Sliding R0 computation (select 'new_cases' or 'new_deaths') \n (remove South Korea & China before)"
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
          h1("Covid-19 pour le peuple", align="center")
          , h2("Page de Visualisation et d'Analyse de données de la pandémie due au SARS-CoV-2", align="center")
          , p("Données Source",
                  a("Santé Publique France / data.gouv.fr",
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
                                    )
                        , radioButtons(inputId = "data_column_fr"
                                     , label = "Data to show:"
                                     , choices = c("Hospitalises" = "hosp"
                                     , "Réenimation" = "rea"
                                     , "Sorties" = "rad"
                                     , "Decedes" = "dc"
                                     , "Grouper les 4 categories (histogramme)" = "all"
                                     )
                                     , selected = "hosp"
                                     )
                        , radioButtons(inputId = "sexe"
                                     , label = "Sexe"
                                     , choices = c("Tous" = 0
                                              , "Femmes" = 2
                                              , "Hommes" = 1
                                              )
                                    , selected = 0
                                    )
                        ,     conditionalPanel(
                              condition = "input.echelle == 'departement'"
                              , selectInput(inputId = "dep_sel"
                                    , label = "Départements (with at least 1 case):"
                                        , list(
                                            'Auvergne-Rhône-Alpes'	= unique(france.df[france.df$nom_region == 'Auvergne-Rhone-Alpes',]$dep)
                                            , 'Bourgogne-Franche-Comté'	= unique(france.df[france.df$nom_region == 'Bourgogne-Franche-Comté',]$dep)
                                            , 'Bretagne'	= unique(france.df[france.df$nom_region == 'Bretagne',]$dep)
                                            , 'Centre-Val de Loire'	= unique(france.df[france.df$nom_region == 'Centre-Val de Loire',]$dep)
                                            , 'Corse'	= unique(france.df[france.df$nom_region == 'Corse',]$dep)
                                            , 'Grand Est'	= unique(france.df[france.df$nom_region == 'Grand Est',]$dep)
                                            , 'Guadeloupe'	= unique(france.df[france.df$nom_region == 'Guadeloupe',]$dep)
                                            , 'Guyane'	= unique(france.df[france.df$nom_region == 'Guyane',]$dep)
                                            , 'Hauts-de-France'	= unique(france.df[france.df$nom_region == 'Hauts-de-France',]$dep)
                                            , 'Île-de-France'	= unique(france.df[france.df$nom_region == 'Ile-de-France',]$dep)
                                            , 'La Réunion'	= unique(france.df[france.df$nom_region == 'La Reunion',]$dep)
                                            , 'Martinique'	= unique(france.df[france.df$nom_region == 'Martinique',]$dep)
                                            , 'Normandie'	= unique(france.df[france.df$nom_region == 'Normandie',]$dep)
                                            , 'Nouvelle-Aquitaine'	= unique(france.df[france.df$nom_region == 'Nouvelle-Aquitaine',]$dep)
                                            , 'Occitanie'	= unique(france.df[france.df$nom_region == 'Occitanie',]$dep)
                                            , 'Pays de la Loire'	= unique(france.df[france.df$nom_region == 'Pays de la Loire',]$dep)
                                            , "Provence-Alpes-Côte d'Azur"	= unique(france.df[france.df$nom_region == "Provence-Alpes-Cote d'Azur",]$dep)
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
                                      label = "Synchroniser les épidémies départementales/régionales (nombre minimal de cas/morts pour définir le début)", value = FALSE)
                       , numericInput(inputId = "num.min.fr"
                                     , label = ""
                                     , value = 10
                                     )
                        , hr(style="border-color: black")
                        , checkboxInput(inputId="R0",
                                      label = "Sliding R0 computation (select 'new_cases' or 'new_deaths') \n (remove South Korea & China before)"
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
                       plotOutput(outputId = "franceplot", width="100%", height=750)
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
  output$worldplot <- renderPlot({

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
        if(input$data_column == "new_cases"){
            DAT.0 = covdat[covdat$location %in% input$countries_sel, c(2, 3, 4)]
        } else if(input$data_column == "new_deaths") {
            DAT.0 = covdat[covdat$location %in% input$countries_sel, c(2, 3, 5)]
        } else {stop(safeError(("Incompatible Data to show / plot option combination")))}
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
          DAT.1 <- DAT.1[, -c(1, 2)]
          es_uncertain_si <- estimate_R(DAT.1,
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
  #        before <- which(covdat$total_cases == 0)
          before <- which(covdat$total_cases < input$num.min)
          covdat.sync <- covdat[-before, ]
          covdat.sync$J <- 0
          for (c in unique(covdat.sync$location)){
          				L <- dim(covdat.sync[covdat.sync$location == c, ])[1]
          				covdat.sync[covdat.sync$location == c, "J"] <- seq(length = L)
                  }
          dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          covdat_selected <- covdat.sync[(covdat.sync$location %in% input$countries_sel) & (covdat.sync$date %in% dates_range),]
          } else {
          dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          covdat_selected <- covdat[(covdat$location %in% input$countries_sel),]
          }

    ######
    myplot <- ggplot(covdat_selected) +
          #scale_color_brewer(palette="Paired", name = "Country")
          scale_color_discrete(name = "Countries:") +
          theme_linedraw(base_size = 15)
    ######
    #    dates_range <- seq(input$dates[1], input$dates[2], by = "days")
    #    covdat_selected <- covdat[(covdat$location %in% input$countries_sel) & (covdat$date %in% dates_range),]



    if(input$sync){
    if(input$percapita){
    myplot <- myplot + labs(x = "Date", y = "Number of cases per capita")
    if(input$data_column == "total_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_cases_percapita, colour = location), size=1)}
    else if(input$data_column == "new_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_cases_percapita, colour = location), size=1)}
    else if(input$data_column == "total_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_deaths_percapita, colour = location), size=1)}
    else if(input$data_column == "new_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_deaths_percapita, colour = location), size=1)}
    }  else {
    myplot <- myplot + labs(x = "Date", y = "Number of cases")
    if(input$data_column == "total_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_cases, colour = location), size=1)}
    else if(input$data_column == "new_cases"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_cases, colour = location), size=1)}
    else if(input$data_column == "total_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = total_deaths, colour = location), size=1)}
    else if(input$data_column == "new_deaths"){
      myplot <- myplot + geom_line(mapping = aes(x = J, y = new_deaths, colour = location), size=1)}
    }
    } else if(input$R0){
        myplot <- ggplot(data = RES, aes(x = J, y = R0_point, colour = location)) +
        geom_line(size = 3)+
        geom_ribbon(aes(ymin=R0_low, ymax=R0_high, colour = location), linetype=2, alpha=0.2)+
        xlim(0, NA)+
        geom_hline(
              yintercept = 1,
              )+
        labs(x = "Time in days", y = "Basic Reproduction Number (R) estimates")+
        xlim(-length(unique(RES$J)), 0)+
        theme_minimal()
      } else {
    if(input$percapita){
      myplot <- myplot + labs(x = "Date", y = "Number of cases per capita")
      if(input$data_column == "total_cases"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = total_cases_percapita, colour = location), size=1)}
      else if(input$data_column == "new_cases"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = new_cases_percapita, colour = location), size=1)}
      else if(input$data_column == "total_deaths"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = total_deaths_percapita, colour = location), size=1)}
      else if(input$data_column == "new_deaths"){
        myplot <- myplot + geom_line(mapping = aes(x = date, y = new_deaths_percapita, colour = location), size=1)}
        }
        else{
          myplot <- myplot + labs(x = "Date", y = "Number of cases")
          if(input$data_column == "total_cases"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = total_cases, colour = location), size=1)}
          else if(input$data_column == "new_cases"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = new_cases, colour = location), size=1)}
          else if(input$data_column == "total_deaths"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = total_deaths, colour = location), size=1)}
          else if(input$data_column == "new_deaths"){
            myplot <- myplot + geom_line(mapping = aes(x = date, y = new_deaths, colour = location), size=1)}
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

    if(input$log)
      myplot <- myplot + scale_y_log10()
    if(input$dailyscale)
      myplot <- myplot + scale_x_date(date_minor_breaks = "1 day")
    return(myplot)
  })

############################### France

output$franceplot <- renderPlot({

      if(input$echelle == "departement"){
  #      dates_range <- seq(input$dates[1], input$dates[2], by = "days")
        data_selected <- france.df[(france.df$dep %in% input$dep_sel) & (france.df$sexe == input$sexe),]
        data_selected$location <- data_selected$dep
      } else {
        data_selected <- regions.df[(regions.df$Region %in% input$region_sel) & (regions.df$Sexe == input$sexe),]
        data_selected$location <- data_selected$Region
      }




      if(input$map | input$xyplot){}




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
                  geom_bar(position = "stack", stat = "identity")
      } else {
  #    if(input$sync){
  #    if(input$percapita){
  #    }  else {
  #    }
  #    } else if(input$R0){
  #      } else {
      if(input$percapita_fr){
        #myplot <- myplot + labs(x = "Date", y = "Number of cases")
        if(input$data_column_fr == "hosp"){
          myplot <- myplot+ geom_line(mapping = aes(x = jour, y = (hosp*10e5/PTOT), colour = location), size=1)+
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
            else if(input$data_column_fr == "rea"){
              myplot <- myplot + geom_line(mapping = aes(x = jour, y = rea, colour = location), size=1)+
                                 labs(x = "Date", y = "Patiens en reanimation")
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

    if(input$log_fr)
      myplot <- myplot + scale_y_log10()
    if(input$dailyscale_fr)
      myplot <- myplot + scale_x_date(date_minor_breaks = "1 day")
    return(myplot)
}
)

}

shinyApp(ui, server)
