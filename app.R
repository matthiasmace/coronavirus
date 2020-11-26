library(shiny)
library(shinyWidgets)
library(BBmisc)
library(shinydashboard)
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
pop_data <- wb(country = c(unique(covdat$iso2c)), indicator = "SP.POP.TOTL", startdate = 2019, enddate = 2019)
#covdat <- merge(covdat, pop_data[,c('iso2c', 'value')], by='iso2c')
covdat <- dplyr::inner_join(covdat, pop_data[,c('iso2c', 'value')], by = c("iso2c"= "iso2c"))

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
##  Tests data
tests.fr.df <- as.data.frame(read.csv("https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675", header = T, sep =";"))
tests.fr.df$incid <- (tests.fr.df$P*100)/tests.fr.df$T
tests.fr.df$neg <- tests.fr.df$T - tests.fr.df$P
names(tests.fr.df) <- c("dep", "jour", "pos", "n_tests", "cl_age90", "incid", "neg")

################################################################
##    ON A DES DONNEES PAR CLASSE D AGE QU ON N UTILISE PAS   ##
################################################################

############  Hospit data
france.df <- as.data.frame(read.csv("https://www.data.gouv.fr/fr/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header = T, sep =";"))
#france.df <- as.data.frame(read.csv("63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header = T, sep =";"))


######  donnees EHPAD / non dispo par departement......
##### verifier fiabilite de https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv
france.ehpad <- as.data.frame(read.csv("https://github.com/opencovid19-fr/data/raw/master/dist/chiffres-cles.csv", header = T, sep =","))
#france.ehpad$maille_code <- gsub("DEP-", "", france.ehpad$maille_code)
#france.ehpad$date <- as.Date(france.ehpad$date)
#france.ehpad <- france.ehpad[france.ehpad$granularite == "departement", c("date", "maille_code", "deces_ehpad")]
#france.ehpad[which(!is.na(france.ehpad$deces_ehpad))]

##### une reciprocal cumsum serait interessante pour plotter le nombre de morts quotidiens

## fusion avec TESTS
france.df <- left_join(france.df, tests.fr.df[tests.fr.df$cl_age90 == 0, ], by = c("dep" = "dep", "jour" = "jour"))

france.df$jour <- as.Date(france.df$jour)

##  fusion avec regions
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
france.df <- inner_join(france.df, pop.dep, by = c("dep"= "CODDEP"))[, c("dep","DEP", "region_name","sexe","jour", "hosp","rea","rad","dc", "pos", "n_tests", "incid", "neg", "PTOT")]
#
lits.dep <- as.data.frame(read.csv("./data_france/Lits_2013_2018.csv", header = T, sep =";"))
france.df <- inner_join(france.df, lits.dep, by = c("dep"= "dep"))
#
##### regions
regions.df <- aggregate(list(france.df[, -c(1:5)]), by=list(Sexe = france.df$sexe, Region = france.df$region_name, jour = france.df$jour), FUN=sum)
#pop.reg <- as.data.frame(read.csv("./data_france/ensemble/Regions.csv", header = T, sep =";"))
## attention !!!! l'incidence est sommée > recalculer
regions.df$incid <- (regions.df$pos*100)/regions.df$n_tests

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
#                                , dropdownButton(
#                                                #          tags$h3("List of Input")
#                                                 #          , selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris))
#                                                            selectInput(inputId = 'data_column', label = 'Data to plot'
#                                                                     , choices = c("Total cases" = "total_cases"
#                                                                       , "New cases" = "new_cases"
#                                                                       , "Total deaths" = "total_deaths"
#                                                                       , "New deaths" = "new_deaths"
#                                                                       )
#                                                                       , selected = "total_cases"
#                                                                       )
#                                                    #            , sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9)
#                                                             , circle = TRUE, status = "danger", icon = icon("gear"), width = "300px"
#                                                             , tooltip = tooltipOptions(title = "Click to see inputs !")
#                                                               )
                   , strong("Plot options:")
                   , em("For curves (multiple selections allowed)")
                              , numericInput(inputId = "linewidth"
                                  , label = "Width of plot line"
                                  , value = 3
                                  )
                               , checkboxInput(inputId="smoothed"
                                  , label = "Smoothed curve", value = FALSE)
                               , checkboxInput(inputId="log"
                                 , label = "Plot y axis on log scale", value = FALSE)
                               , checkboxInput(inputId="percapita",
                                 label = "Correct for population size", value = FALSE)
                  , checkboxInput(inputId="dailyscale",
                                 label = "Plot daily breaks on x axis", value = FALSE)
                  , checkboxInput(inputId="sync",
                                 label = "Synchronize national epidemics (minimal cases/deaths to begin with)
                                 \n (if it does not work, decrease from 10 to at least 100 the minimal number below)"
                                 , value = FALSE)
                  , numericInput(inputId = "num.min"
                                , label = ""
                                , value = 10
                                )
                   , hr(style="border-color: black")
                   ,     HTML(
                          paste(
                                h4("Sliding R0 computation (select 'new_cases' or 'new_deaths')")
                          #      ,'<br/>'
                                , h5("MAKE SURE to tick graphic options BEFORE !")
                          #      ,'<br/>'
                                , h6("Please be patient, it takes up to several minutes !")
                          #      ,'<br/>'
                                , h6("(if it does not work for death toll, decrease from 10 for little states)")
                          #      ,'<br/>'
                                , h6("(remove South Korea & China before if performing on death toll)")
                                #,h6("(choose the computing window in days)")
                                )
                              )
                   , column(6
                        , numericInput(inputId = "SI.min"
                                 , label = "Serial Interval Min"
                                 , value = 4
                                 )
                            )
                   , column(6
                        , numericInput(inputId = "SI.max"
                                 , label = "Serial Interval Max"
                                 , value = 8
                                 )
                            )
    #               , numericInput(inputId = "window.R0"
    #                             , label = ""
    #                             , value = 3
    #                             )
                    , checkboxInput(inputId="R0"
                                    , label = "COMPUTE !"
                                    , value = FALSE
                                    )
                   , hr(style="border-color: black")
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
          h1("Covid-19 pour Tou.te.s", align="center")
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
          , h4(paste("En France, au moins/environ"
                                      , max(na.omit(france.ehpad[(france.ehpad$maille_code == "FRA" & france.ehpad$source_type == "ministere-sante"), "deces"]))
                                      , "(source Ministère de la Sante) /"
                                      , max(na.omit(france.ehpad[(france.ehpad$maille_code == "FRA" & france.ehpad$source_type == "opencovid19-fr"), "deces"]))
                                      , "(source opencovid19-fr)"
                                      , "morts en centre hospitalier et"
                                      , max(na.omit(france.ehpad[(france.ehpad$maille_code == "FRA" & france.ehpad$source_type == "ministere-sante"), "deces_ehpad"]))
                                      , "morts en EHPAD depuis le debut de l'epidemie (M. Sante)"
                                      )
                        )
          , h4(paste("Au moins"
                , (sum(france.df[france.df$jour == max(france.df$jour) & france.df$sexe == 0, "dc"])-sum(france.df[france.df$jour == (max(france.df$jour)-1) & france.df$sexe == 0, "dc"]))
                , "morts dans les dernieres 24h en centre hospitalier")
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

           , conditionalPanel(
                 condition = "input.echelle == 'departement'"
                 , selectInput(inputId = "dep_sel"
                       , label = "Departements :"
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
                       , selected = c(66, 31, 33, 47, 75, 68, 13, 59, 69
                           )
                       , multiple = TRUE
                       )
                       )
              , conditionalPanel(
                   condition = "input.echelle == 'region'"
                   , selectInput(inputId = "region_sel"
                         , label = "Regions :"
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
                         , radioButtons(inputId = "sexe"
                                      , label = "Sexe"
                                      , choices = c("Tous" = 0
                                               , "Femmes" = 2
                                               , "Hommes" = 1
                                               )
                                     , selected = 0
                                     , inline = TRUE
                                     )
                        , hr(style="border-color: black")
                         , switchInput(
                                inputId = "type_analyse"
                                , label = "Type d'analyse"
                                , labelWidth = "50px"
                                , offLabel = "Visualisation"
                                , onLabel = "Calculs"
                                )
                      , column(5
                         , dropdownButton(
                           tags$h3("Visualisation")
                           , selectInput(inputId = 'type_viz'
                                          , label = 'Type de representation de donnees brutes'
                                          , choices = c("Courbe" = "courbe_fr"
                                          , "XYplot" = "xyplot_fr"
                                          , "heatmap" = "heatmap_fr"
                                          , "carte" = "carte_fr"
                                          )
                                          , selected = "courbe_fr"
                                          )
                           , conditionalPanel(
                                         condition = "input.type_viz == 'courbe_fr'"
                                                    , selectInput(inputId = 'courbe_fr_data', label = 'Donnee a representer'
                                                                   , choices = c(
                                                                      "Tests positifs" = "pos"
                                                                      , "Tests negatifs" = "neg"
                                                                     , "Incidence (tests positifs/total)" = "incid"
                                                                      , "Hospitalises" = "hosp"
                                                                      , "Saturation Hospitalisation" = 'hosp_sat'
                                                                      , "Reanimation" = "rea"
                                                                      , "Saturation Reanimation" = 'rea_sat'
                                                                      , "Sorties" = "rad"
                                                                      , "Decedes" = "dc"
                                                                      , "Grouper les 4 categories (histogramme)" = "all"
                                                                   )
                                                                   , selected = "rea"
                                                                   )
                                                    )
                           , conditionalPanel(
                                         condition = "input.type_viz == 'xyplot_fr'"
                                                    , selectInput(inputId = 'xyplot_fr_data_x', label = 'Donnee en x'
                                                                   , choices = c(
                                                                      "Tests positifs" = "pos"
                                                                  #    , "Tests negatifs" = "neg"
                                                                      , "Incidence (tests positifs/total)" = "incid"
                                                                      , "Incidence a J-7 (moyenne une semaine)" = "J-7"
                                                                      , "Incidence a J-14 (moyenne une semaine)" = "J-14"
                                                                      , "Hospitalises" = "hosp"
                                                                  #    , "Saturation Hospitalisation" = 'hosp_sat'
                                                                      , "Reanimation" = "rea"
                                                                  #    , "Saturation Reanimation" = 'rea_sat'
                                                                      , "Sorties" = "rad"
                                                                      , "Decedes" = "dc"
                                                                  #    , "Lits d'hostpitalisation" = "sc"
                                                                  #    , "Lits de soins instensifs" = "si"
                                                                  #    , "Lits de reanimation" = "sr"
                                                                  #     , "Grouper les 4 categories (histogramme)" = "all"
                                                                   )
                                                                   , selected = "J-14"
                                                                   )
                                                    , selectInput(inputId = 'xyplot_fr_data_y', label = 'Donnee en y'
                                                                   , choices = c(
                                                                   "Tests positifs" = "pos"
                                                               #    , "Tests negatifs" = "neg"
                                                                   , "Incidence (tests positifs/total)" = "incid"
                                                                   , "Incidence a J-7 (moyenne une semaine)" = "J-7"
                                                                   , "Incidence a J-14 (moyenne une semaine)" = "J-14"
                                                                   , "Hospitalises" = "hosp"
                                                               #    , "Saturation Hospitalisation" = 'hosp_sat'
                                                                   , "Reanimation" = "rea"
                                                               #    , "Saturation Reanimation" = 'rea_sat'
                                                                   , "Sorties" = "rad"
                                                                   , "Decedes" = "dc"
                                                               #    , "Lits d'hostpitalisation" = "sc"
                                                               #    , "Lits de soins instensifs" = "si"
                                                               #    , "Lits de reanimation" = "sr"
                                                               #     , "Grouper les 4 categories (histogramme)" = "all"
                                                                   )
                                                                   , selected = "rea"
                                                                   )

                                                    , selectInput(inputId = 'xyplot_fr_data_z', label = 'Taille des points'
                                                                                  , choices = c(
                                                                                  #    , "Tests negatifs" = "neg"
                                                                                       "Incidence (tests positifs/total)" = "incid"
                                                                                      , "Incidence a J-7 (moyenne une semaine)" = "J-7"
                                                                                      , "Incidence a J-14 (moyenne une semaine)" = "J-14"
                                                                                      , "Hospitalises" = "hosp"
                                                                                  #    , "Saturation Hospitalisation" = 'hosp_sat'
                                                                                      , "Reanimation" = "rea"
                                                                                  #    , "Saturation Reanimation" = 'rea_sat'
                                                                                      , "Sorties" = "rad"
                                                                                      , "Decedes" = "dc"
                                                                                      , "Lits d'hostpitalisation" = "sc"
                                                                                      , "Lits de soins instensifs" = "si"
                                                                                      , "Lits de reanimation" = "sr"
                                                                                  #     , "Grouper les 4 categories (histogramme)" = "all"
                                                                                  )
                                                                                  , selected = "sr"
                                                                                  )

                                                    )
                           , circle = TRUE, status = "danger", icon = icon("drafting-compass"), width = "300px",
                           tooltip = tooltipOptions(title = "Visualisation")
                         )
                         )
                      , column(5
                         , dropdownButton(
                           tags$h3("Calculs")
                           , selectInput(inputId = 'type_comp'
                                          , label = 'Type de calculs'
                                          , choices = c("Selectionnez un type de calcul" = "blabla"
                                          , "Calcul du R0 (par fenetres glissantes)" = "R0_fr"
                                          , "Calculs des donnees " = "xyplot_comp"
                                          , "carte" = "carte_fr"
                                          )
                                          , selected = "blabla"
                                          )
                           , conditionalPanel(
                                         condition = "input.type_comp == 'R0_fr'"
                                                    , selectInput(inputId = 'xcol', label = "Variable utilisee (pour plus de visibilite, cochez l'echelle logarithmique AVANT)"
                                                                   , choices = c(
                                                                      "Tests positifs" = "pos"
                                #                                      , "Tests negatifs" = "neg"
                                #                                      , "Incidence (tests positifs/total)" = "incid"
                                                                      , "Hospitalises" = "hosp"
                                #                                      , "Saturation Hospitalisation" = 'hosp_sat'
                                                                      , "Reanimation" = "rea"
                                #                                      , "Saturation Reanimation" = 'rea_sat'
                                #                                      , "Sorties" = "rad"
                                                                      , "Decedes" = "dc"
                                #                                      , "Grouper les 4 categories (histogramme)" = "all"
                                                                   )
                                                                   , selected = "rea"
                                                                   )
                                                    )
                           , conditionalPanel(
                                         condition = "input.type_comp == 'xyplot_comp'"
                                                    , selectInput(inputId = 'ycol', label = 'Variable en Y'
                                                                   , choices = c(
                                                                      "Tests positifs" = "pos"
                                    #                                  , "Tests negatifs" = "neg"
                                                                      , "Incidence (tests positifs/total)" = "incid"
                                                                      , "Hospitalises" = "hosp"
                                                                      , "Saturation Hospitalisation" = 'hosp_sat'
                                                                      , "Reanimation" = "rea"
                                                                      , "Saturation Reanimation" = 'rea_sat'
                                                                      , "Sorties" = "rad"
                                                                      , "Decedes" = "dc"
                                                                      , "Grouper les 4 categories (histogramme)" = "all"
                                                                   )
                                                                   , selected = "rea"
                                                                   )
                                                    , selectInput(inputId = 'xcol', label = 'Variable en x'
                                                                   , choices = c(
                                                                      "Tests positifs" = "pos"
                                                                      , "Tests negatifs" = "neg"
                                                                      , "Incidence (tests positifs/total)" = "incid"
                                                                      , "Hospitalises" = "hosp"
                                                                      , "Saturation Hospitalisation" = 'hosp_sat'
                                                                      , "Reanimation" = "rea"
                                                                      , "Saturation Reanimation" = 'rea_sat'
                                                                      , "Sorties" = "rad"
                                                                      , "Decedes" = "dc"
                                                                      , "Grouper les 4 categories (histogramme)" = "all"
                                                                   )
                                                                   , selected = "rea"
                                                                   )

                                                    )


                           , circle = TRUE, status = "danger", icon = icon("square-root-alt"), width = "300px",
                           tooltip = tooltipOptions(title = "Calculs")
                           )
                         )

#                        , radioButtons(inputId = "type"
#                                    , label = "Type de graphe"
#                                    , choices = c("Courbe" = "courbe_fr"
#                                    , "XYplot" = "xyplot_fr"
#                                    )
#                                    , selected = "courbe_fr"
#                                    , inline = TRUE
#                                    )
#                        , conditionalPanel(
#                                    condition = "input.type == 'courbe_fr'"
#                                    , radioButtons(inputId = "data_column_fr"
#                                     , label = "Donnees a montrer :"
#                                     , choices = c(
#                                        "Tests positifs" = "pos"
#                                        , "Tests negatifs" = "neg"
#                                        , "Incidence (tests positifs/total)" = "incid"
#                                        , "Hospitalises" = "hosp"
#                                        , "Saturation Hospitalisation" = 'hosp_sat'
#                                        , "Reanimation" = "rea"
#                                        , "Saturation Reanimation" = 'rea_sat'
#                                        , "Sorties" = "rad"
#                                        , "Decedes" = "dc"
#                                        , "Grouper les 4 categories (histogramme)" = "all"
#                                     )
#                                     , selected = "hosp"
#                                     , inline = TRUE
#                                     )
#                                     )
#                        , conditionalPanel(
#                                      condition = "input.type == 'xyplot_fr'"
#                                                 , radioButtons(inputId = "xyplot_fr"
#                                                 , label = "Donnees a montrer :"
#                                                 , choices = c(
#                                                   "Reanimation" = "xyplot_rea_fr", "Hospitalisation" = "xyplot_hosp_fr"
#                                                  )
#                                                 , selected = "xyplot_rea_fr"
#                                                 , inline = TRUE
#                                                 )
#                                            )

                        , hr(style="border-color: black")
#                        , strong("Options graphiques:")
                        , h3("Options graphiques:")
                        , em("Pour les courbes (selections multiples possibles)")
                        , conditionalPanel(
                            condition = "input.type_analyse == FALSE"
                            , numericInput(inputId = "linewidth_fr"
                                , label = "Epaisseur des courbes"
                                , value = 3
                                )
                            , checkboxInput(inputId="smoothed_fr"
                                , label = "Courbe lissee", value = FALSE
                                )

                            , checkboxInput(inputId="log_fr"
                                , label = "Representer l'axe des Y en echelle logarithmique", value = FALSE)
                                , checkboxInput(inputId="percapita_fr"
                                , label = "Normaliser en fonction de la taille de la population", value = FALSE
                                )
                           , checkboxInput(inputId="dailyscale_fr"
                                , label = "Representer les jours sur l'axe des X", value = FALSE
                                )
                           , checkboxInput(inputId="sync_fr"
                                , label = "Synchroniser les epidemies departementales/regionales (nombre minimal de cas/morts pour definir le debut)
                                          \n (si ne fonctionne pas : augmenter de 10 a 100 la valeur minimale ci-dessous)"
                                , value = FALSE
                                )
                           , numericInput(inputId = "num.min.fr"
                                , label = ""
                                , value = 5
                                )
                              )

                        , hr(style="border-color: black")
                        , h3("Parametres du calcul du R0:")
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
  #                      , numericInput(inputId = "window.R0"
  #                                    , label = ""
  #                                    , value = 3
  #                                    )

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
                       sliderInput(inputId="dates.fr",
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


#############################################################################################

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
          #  onevent("mouseover", "R0", alert("aide R0"))
          #  onevent("mouseleave", "R0", alert("aide R0"))

          ###############################
          # 1. It is "reactive" and therefore should be automatically
          #    re-executed when inputs (input$countries) change
          # 2. Its output type is a plot
          ###############################


############################### world

  output$worldplot <- renderPlot({

      #################
      data_column <- input$data_column
      #data_column <- input$curve_data

      #################

      dates_range <- seq(input$dates[1], input$dates[2], by = "days")
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
      data_selected.def <- data_selected[data_selected$date %in% dates_range, ]

      if(input$map | input$xyplot){
        sPDF <- joinCountryData2Map(map.df.2
        , joinCode = "ISO2"
        , nameJoinColumn = "iso2c")
        # creating a user defined colour palette
        # op <- palette(c("green", "yellow", "orange", "red"))
        #
        if(input$socialvar == "NONE"){
            var = data_column
            sPDF@data[["data_to_plot"]] <- sPDF@data[[var]]
            # sPDF@data[["data_to_plot"]] <- cut(sPDF@data[[var]]
            #    , breaks = 4
            #    , cutVector
            #    , include.lowest=TRUE
            #  )
            # levels(sPDF@data[["data_to_plot"]]) <- c("low","med", "high", "vhigh")
            } else {
            var <- data_column
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
      #  COLS <- c(which(names(covdat) %in% c("date", "location", data_column)))
      #  DAT.0 = covdat[covdat$location %in% input$countries_sel, COLS]
        #DAT.0 = covdat[covdat$location %in% input$countries_sel, c("date", "location", "new_deaths")]

      #  data_selected.def <- data_selected

#### for testing purpose...
#### input <- list() ; input$SI.min = 4 ; input$SI.max = 8 ; input$num.min = 10 ; data_column = "new_deaths" ; input$countries_sel = c("Germany", "France", "Denmark", "Spain", "Italy",  "Sweden", "United States")
#### data_selected.def <- covdat[(covdat$location %in% input$countries_sel),]
        eval(parse(text = paste("DAT.0 = na.omit(data_selected.def[, c('date', 'location', '", data_column, "')])", sep = "")))


  #      if(data_column == "new_cases"){
  #          DAT.0 = data_selected[, c(2, 3, 4)]
  #      } else if(data_column == "new_deaths") {
  #          DAT.0 = data_selected[, c(2, 3, 5)]
  #      } else {stop(safeError(("Incompatible Data to show / plot option combination")))}
        #
        names(DAT.0) <- c("date", "location", "data")
        DAT.0[DAT.0$data < 0, "data"] <- 0  #####   forced by Italy, Spain... data with "negative deaths"
        #
        RES <- list()
        #
        config <- make_config(list(mean_si = (mean(c(input$SI.min, input$SI.max)))
                                  , std_mean_si = 1
                                  , min_mean_si = input$SI.min
                                  , max_mean_si = input$SI.max
                                  , std_si = 1.5, std_std_si = 0.5
                                  , min_std_si = 0.5, max_std_si = 2.5
                                   )
                              )
        #
        #window = input$window.R0
        #
        for(c in unique(DAT.0$location)){
          after <- which(DAT.0[DAT.0$location == c, "data"] >= input$num.min)
          DAT.1 <- DAT.0[DAT.0$location == c, ][-c(1:after[1]), ]
    #      DAT.1 <- DAT.0[DAT.0$location == c  & (DAT.0$data >= input$num.min), ]
          rownames(DAT.1) <- DAT.1$date
          DAT.1 <- DAT.1[, -c(1, 2)]
          es_uncertain_si <- estimate_R(DAT.1
                                         , method = "uncertain_si"
                                         , config = config
                                         )
          #
          # max.length <- max(table(DAT.0[DAT.0$data >= input$num.min, c("location")]))
          df <- rbind(do.call("rbind"
                                    #      , replicate(n = (max.length - length(as.matrix(DAT.1)))
                                          , replicate(n = (after[1] + 1)
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
  #        dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          data_selected.def <- data_selected.sync[data_selected.sync$date %in% dates_range, ]
        } else {
  #        dates_range <- seq(input$dates[1], input$dates[2], by = "days")
          data_selected.def <- data_selected[data_selected$date %in% dates_range, ]
          }

    ######
    myplot <- ggplot() +
          #scale_color_brewer(palette="Paired", name = "Country")
          scale_color_discrete(name = "Locations:") +
          theme_linedraw(base_size = 15)
    ######
    #    dates_range <- seq(input$dates[1], input$dates[2], by = "days")
    #    covdat_selected <- data_selected[(data_selected$location %in% input$countries_sel) & (data_selected$date %in% dates_range),]



    if(input$sync){
    if(input$percapita){
#    myplot <- myplot + labs(x = "Date", y = "Number per capita")
    if(data_column == "total_cases"){
      DF <- data_selected.def[, c("date", "total_cases_percapita", "location")]
      names(DF) <- c("x", "y", "location")
      myplot <- ggplot(DF, aes(x, y, colour = location))+
                ggtitle("Number of Confirmed Cases Per Capita throughout time"
                , subtitle = "the raw number is divided by the country population")
      }
    else if(data_column == "new_cases"){
    DF <- data_selected.def[, c("date", "new_cases_percapita", "location")]
    names(DF) <- c("x", "y", "location")
    myplot <- ggplot(DF, aes(x, y, colour = location))+
              ggtitle("Number of Daily New Confirmed Cases Per Capita"
              , subtitle = "the raw number is divided by the country population")
      }
    else if(data_column == "total_deaths"){
    DF <- data_selected.def[, c("date", "total_deaths_percapita", "location")]
    names(DF) <- c("x", "y", "location")
    myplot <- ggplot(DF, aes(x, y, colour = location))+
              ggtitle("Number of Deaths Per Capita throughout time"
              , subtitle = "the raw number is divided by the country population")
    }
    else if(data_column == "new_deaths"){
    DF <- data_selected.def[, c("date", "new_deaths_percapita", "location")]
    names(DF) <- c("x", "y", "location")
    myplot <- ggplot(DF, aes(x, y, colour = location))+
              ggtitle("Number of Daily New Deaths Per Capita"
              , subtitle = "the raw number is divided by the country population")
    }
    }  else {

    myplot <- myplot + labs(x = "Date", y = "Raw Number")
    if(data_column == "total_cases"){
      DF <- data_selected.def[, c("date", "total_cases", "location")]
      names(DF) <- c("x", "y", "location")
      myplot <- ggplot(DF, aes(x, y, colour = location))+
                ggtitle("Number of Confirmed Cases throughout time"
                )
                }
    else if(data_column == "new_cases"){
    DF <- data_selected.def[, c("date", "new_cases", "location")]
    names(DF) <- c("x", "y", "location")
    myplot <- ggplot(DF, aes(x, y, colour = location))+
              ggtitle("Number of Daily New Confirmed Cases"
              )
              }
    else if(data_column == "total_deaths"){
    DF <- data_selected.def[, c("date", "total_deaths", "location")]
    names(DF) <- c("x", "y", "location")
    myplot <- ggplot(DF, aes(x, y, colour = location))+
              ggtitle("Number of Deaths throughout time"
              )
              }
    else if(data_column == "new_deaths"){
    DF <- data_selected.def[, c("date", "new_deaths_percapita", "location")]
    names(DF) <- c("x", "y", "location")
    myplot <- ggplot(DF, aes(x, y, colour = location))+
              ggtitle("Number of Daily New Deaths"
              )
              }
            }
    } else if(input$R0){
              #      if( == "rea"){
              #                 EST <- "'patients en reanimation pour COVID'"
              #                 }
              #      if(input$xcol == "hosp"){
              #                 EST <- "'patients hospitalises pour COVID'"
              #                 }
              #      if(input$xcol == "pos"){
              #                 EST <- "'nombre de tests positifs au COVID'"
              #                 }
              #      if(input$xcol == "dc"){
              #                 EST <- "'patients decedes du COVID'"
              #                 }
                     myplot <- ggplot(data = RES, aes(x = J, y = R0_point, colour = location))+
                                 geom_line(size = 1)+
                                 geom_ribbon(aes(ymin = R0_low, ymax = R0_high, colour = location), linetype = 2, alpha = 0.1)+
                                 #xlim(0, NA)+
                                 ylim(-1, NA)+
                                 geom_hline(
                                 yintercept = 1, linetype = 5, colour = "black",
                                 )+
                                 geom_text(aes(min(J), 1, label = "R0 = 1", vjust = -1), colour = "black")+
                                 ggtitle("R0 estimate on sliding windows"
                                 , subtitle = paste(
                                                  "computing based on the indicator"
                                  #                , EST
                                                   , data_column
                                                  )
                                              )+
                                 labs(x = "TIME (from past to today) in DAYS", y = "Estimate of the Basic Reproduction Number (R0)")+
                                 xlim(-length(unique(RES$J)), 0)+
                                 theme_minimal()

                      } else {

    if(input$percapita){
      myplot <- myplot + labs(x = "Date", y = "Number per capita")
      if(data_column == "total_cases"){
        DF <- data_selected.def[, c("date", "total_cases_percapita", "location")]
        names(DF) <- c("x", "y", "location")
        myplot <- ggplot(DF, aes(x, y, colour = location))+
                        ggtitle("Number of Confirmed Cases per capita throughout time"
                                , subtitle = "the raw number is divided by the country population")
                                }
        else if(data_column == "new_cases"){
        DF <- data_selected.def[, c("date", "new_cases_percapita", "location")]
        names(DF) <- c("x", "y", "location")
        myplot <- ggplot(DF, aes(x, y, colour = location))+
                          ggtitle("Number of Daily New Confirmed Cases per capita"
                                  , subtitle = "the raw number is divided by the country population")
                                  }
      else if(data_column == "total_deaths"){
        DF <- data_selected.def[, c("date", "total_deaths_percapita", "location")]
        names(DF) <- c("x", "y", "location")
        myplot <- ggplot(DF, aes(x, y, colour = location))+
                          ggtitle("Number of Deaths Per Capita throughout time"
                                  , subtitle = "the raw number is divided by the country population")
                                  }
      else if(data_column == "new_deaths"){
        DF <- data_selected.def[, c("date", "new_deaths_percapita", "location")]
        names(DF) <- c("x", "y", "location")
        myplot <- ggplot(DF, aes(x, y, colour = location))+
                          ggtitle("Number of Daily New Deaths Per Capita"
                                  , subtitle = "the raw number is divided by the country population")
                                  }
        } else {
          myplot <- myplot + labs(x = "Date", y = "Raw Number")
          if(data_column == "total_cases"){
            DF <- data_selected.def[, c("date", "total_cases", "location")]
            names(DF) <- c("x", "y", "location")
            myplot <- ggplot(DF, aes(x, y, colour = location))+
                              ggtitle("Number of Confirmed Cases throughout time")
                              }
          else if(data_column == "new_cases"){
            DF <- data_selected.def[, c("date", "new_cases", "location")]
            names(DF) <- c("x", "y", "location")
            myplot <- ggplot(DF, aes(x, y, colour = location))+
                              ggtitle("Number of Daily New Confirmed Cases")
                              }
          else if(data_column == "total_deaths"){
            DF <- data_selected.def[, c("date", "total_deaths", "location")]
            names(DF) <- c("x", "y", "location")
            myplot <- ggplot(DF, aes(x, y, colour = location))+
                              ggtitle("Number of Deaths throughout time")
                              }
          else if(data_column == "new_deaths"){
            DF <- data_selected.def[, c("date", "new_deaths", "location")]
            names(DF) <- c("x", "y", "location")
            myplot <- ggplot(DF, aes(x, y, colour = location))+
                              ggtitle("Number of Daily New Deaths")
                              }
        }
      }

if(input$map){
  if(data_column == "total_cases" | data_column == "total_deaths"){
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
    if(data_column == "total_cases" | data_column == "total_deaths"){
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
    } else if(input$corrmap) {
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
      } else {
        if(input$smoothed){
          myplot <- myplot+ geom_smooth(data = DF, aes(x, y, colour = location), method = "loess")
          } else {
          myplot <- myplot+ geom_line(size = input$linewidth)
                  }
        if(input$log){
          myplot <- myplot+
                        scale_y_log10()+
                        labs(x = "Date", y = "log Number")
        } else {
          myplot <- myplot+labs(x = "Date", y = "Number")
                      }

        if(input$dailyscale){
            myplot <- myplot+
                        scale_x_date(date_minor_breaks = "1 day")
                      }

        myplot <- myplot+
                      theme_linedraw(base_size = 15)

      }

      return(myplot)
  }
)

###############################   France    ###############################

output$franceplot <- renderPlot({
#output$franceplot <- renderGirafe({
dates_range.fr <- seq(input$dates.fr[1], input$dates.fr[2], by = "days")

      if(input$echelle == "departement"){
        data_selected <- france.df[(france.df$dep %in% input$dep_sel) & (france.df$sexe == input$sexe),]
        data_selected$location <- data_selected$dep
      } else {
        data_selected <- regions.df[(regions.df$Region %in% input$region_sel) & (regions.df$Sexe == input$sexe),]
        data_selected$location <- data_selected$Region
      }
  data_selected.def <- data_selected[data_selected$jour %in% dates_range.fr, ]
  #data_selected.def <- data_selected.sync[data_selected.sync$date %in% dates_range, ]

##
if(input$type_analyse == TRUE){
          if(input$type_comp == "R0_fr"){

          #### for testing purpose...
          #### input <- list() ; input$SI.min = 4 ; input$SI.max = 8 ; input$num.min = 10 ; input$xcol = "hosp" ; input$region_sel = c("Occitanie", "Nouvelle-Aquitaine", "Auvergne-Rhone-Alpes") ; input$sexe = 0
          #### data_selected.def <- regions.df[(regions.df$Region %in% input$region_sel & (regions.df$Sexe == input$sexe)),] ; data_selected.def$location <- data_selected.def$Region ; data_selected.def$location <- data_selected.def$Region
          #### input$dates.fr = as.Date(c("2020-10-01", "2020-11-01")) ; dates_range.fr <- seq(input$dates.fr[1], input$dates.fr[2], by = "days") ; data_selected.def <- data_selected.def[data_selected.def$jour %in% dates_range.fr, ]



            DAT.0 <- na.omit(data_selected.def[, c("jour", "location", input$xcol)])
            names(DAT.0) <- c("date", "location", "data")
    #        DAT.0[DAT.0$data < 0, "data"] <- 0  #####   forced by Italy, Spain... data with "negative deaths"
            RES <- list()
            #
            config <- make_config(list(mean_si = (mean(c(input$SI.min, input$SI.max)))
                                      , std_mean_si = 1
                                      , min_mean_si = input$SI.min
                                      , max_mean_si = input$SI.max
                                      , std_si = 1.5, std_std_si = 0.5
                                      , min_std_si = 0.5, max_std_si = 2.5
                                       )
                                  )
            #
            #window = input$window.R0_fr
            #
            for(c in unique(DAT.0$location)){
              after <- which(DAT.0[DAT.0$location == c, "data"] >= input$num.min)
              DAT.1 <- DAT.0[DAT.0$location == c, ][-c(1:after[1]), ]
          #    DAT.1 <- DAT.0[DAT.0$location == c  & (DAT.0$data >= input$num.min.fr), ]
              rownames(DAT.1) <- DAT.1$date
              DAT.1 <- DAT.1[, -c(1, 2)]
              es_uncertain_si <- estimate_R(DAT.1
                                             , method = "uncertain_si"
                                             , config = config
                                             )
              #
              max.length <- max(table(DAT.0[DAT.0$data >= input$num.min.fr, c("location")]))
              df <- rbind(do.call("rbind"
                                        #      , replicate(n = (max.length - length(as.matrix(DAT.1)))
                                               , replicate(n = (after[1] + 1)
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


          if(input$type_comp == "xyplot_comp"){}
}

################################################
      if(input$sync_fr){
          before <- which(data_selected.def$location < input$num.min.fr)
      #    before <- which(data_selected.def$hosp < input$num.min.fr)
          data_selected.sync <- data_selected.def[-before, ]
          data_selected.sync$J <- 0
          for (c in unique(data_selected.sync$location)){
              L <- dim(data_selected.sync[data_selected.sync$location == c, ])[1]
              data_selected.sync[data_selected.sync$location == c, "J"] <- seq(length = L)
              }
            data_selected.sync$jour <- data_selected.sync$J
            data_selected.def <- data_selected.sync
            }
################################################

    #  myplot <- ggplot(data_selected, aes(tooltip = location, data_id = location,)) +
    myplot <- ggplot(data_selected.def) +
            #scale_color_brewer(palette="Paired", name = "Country")
            scale_color_discrete(name = paste(input$echelle, ":")) +
            theme_linedraw(base_size = 15)
      ######
      #    dates_range <- seq(input$dates[1], input$dates[2], by = "days")
      #    covdat_selected <- covdat[(covdat$location %in% input$countries_sel) & (covdat$date %in% dates_range),]

if(input$type_analyse == FALSE){
    if(input$type_viz == ("courbe_fr")){
      data_column_fr <- input$courbe_fr_data
        if(data_column_fr == "all"){
          data <- melt(data = data_selected.def, id.vars = c("location", "jour"), measure.vars = c("hosp", "rea", "rad", "dc"))
          myplot <- ggplot(data, aes(fill = variable, y = value, x = jour))+
                    geom_bar_interactive(position = "stack", stat = "identity")
                    }
              #################################################################
              ###### MESURES INDEPENDANTES DE LA TAILLE DE LA POPULATION ######
              #################################################################
        if((data_column_fr == "hosp_sat") | (data_column_fr == "rea_sat") | (data_column_fr == "incid")) {
          if(data_column_fr == "hosp_sat"){
            DF <- data_selected.def[, c("jour", "hosp", "location")]
            names(DF) <- c("x", "y", "location")
            DF$y <- (data_selected$hosp/(data_selected$SC_CHR_2018 + data_selected$SC_AUTRES_2018))*100
            ##  Polygone rouge au-dessus de 100%  ##
            df_poly <- data.frame(x = as.Date(c(0, 0, Inf, Inf), origin = min(DF$x))
                                        , y = c(100, Inf, Inf, 100)
                                        )

                    myplot <- myplot +
                                      labs(x = "Date", y = "Saturation en Hospitalisation (%)")+
                                      geom_polygon(data = df_poly, aes(x, y), fill="red", alpha=0.2)+
                                      labs(title = "Saturation en Lits par les patients COVID"
                                          , subtitle = "Zone Rouge : Departement/Region sature(e) (>100%)"
                                          )
                              }
          if(data_column_fr == "rea_sat"){
            DF <- data_selected.def[, c("jour", "rea", "location")]
            names(DF) <- c("x", "y", "location")
            DF$y <- (data_selected$rea/(data_selected$Rea_CHR_2018 + data_selected$Rea_AUTRES_2018))*100
            ##  Polygone rouge au-dessus de 100%  ##
            df_poly <- data.frame(x = as.Date(c(0, 0, Inf, Inf), origin = min(DF$x))
                                      , y = c(100, Inf, Inf, 100)
                                      )

                    myplot <- myplot +
                                      labs(x = "Date", y = "Saturation en Hospitalisation")+
                                      geom_polygon(data = df_poly, aes(x, y), fill="red", alpha=0.2)+
                                      labs(title = "Saturation en Lits de Reanimation par les patients COVID"
                                          , subtitle = "Zone Rouge : Departement/Region sature(e) (>100%)"
                                          )
                              }
          if(data_column_fr == "incid"){
            DF <- data_selected.def[, c("jour", "incid", "location", "PTOT")]
            names(DF) <- c("x", "y", "location", "PTOT")
                    myplot <- myplot+
                                      labs(x = "Date", y = "incidence (%)")+
                                      labs(title = "Incidence du COVID-19 mesure par les tests"
                                            , subtitle = "(rapport Nb positifs/Nb total de tests ; mesure independante de la population)"
                                            )
                              }
                              }
        #################################################################
        ###### MESURES DEPENDANTES DE LA TAILLE DE LA POPULATION ########
        #################################################################
        else {
          if(input$percapita_fr){
            if(data_column_fr == "pos"){
  DF <- data_selected.def[, c("jour", "pos", "location", "PTOT")]
  names(DF) <- c("x", "y", "location", "PTOT")
  DF$y <- DF$y*10e5/DF$PTOT

  myplot <- myplot+
                labs(x = "Date", y = "Tests positifs / 100.000 habitants")+
                labs(title = "Tests Positifs"
                    , subtitle = "Donnees brutes (pour 100.000 habitants)"
                    )
                    }
            if(data_column_fr == "neg"){
  DF <- data_selected.def[, c("jour", "neg", "location", "PTOT")]
  names(DF) <- c("x", "y", "location", "PTOT")
  DF$y <- DF$y*10e5/DF$PTOT

  myplot <- myplot+
                labs(x = "Date", y = "Tests négatifs / 100.000 habitants")+
                labs(title = "Tests Negatifs"
                    , subtitle = "Donnees brutes (pour 100.000 habitants)"
                    )
                    }
            if(data_column_fr == "hosp"){
  DF <- data_selected.def[, c("jour", "hosp", "location", "PTOT")]
  names(DF) <- c("x", "y", "location", "PTOT")
  DF$y <- DF$y*10e5/DF$PTOT

  myplot <- myplot+
                labs(x = "Date", y = "Patiens hospitalises / 100.000 habitants")+
                labs(title = "Nombre de Patients Hospitalises pour COVID-19"
                      , subtitle = "Donnees brutes (pour 100.000 habitants)"
                    )
                        }
            if(data_column_fr == "rea"){
  DF <- data_selected.def[, c("jour", "rea", "location", "PTOT")]
  names(DF) <- c("x", "y", "location", "PTOT")
  DF$y <- DF$y*10e5/DF$PTOT

  myplot <- myplot+
              labs(x = "Date", y = "Patiens en reanimation / 100.000 habitants")+
              labs(title = "Nombre de Patients en Reanimation pour COVID-19"
                    , subtitle = "Donnees brutes (pour 100.000 habitants)"
                  )
                        }
            if(data_column_fr == "rad"){
              DF <- data_selected.def[, c("jour", "rad", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              DF$y <- DF$y*10e5/DF$PTOT
              myplot <- myplot+
                labs(x = "Date", y = "Patiens sortis / 100.000 habitants")+
                labs(title = "Nombre de Patients Retournes a domicile ('gueris')"
                      , subtitle = "Donnees brutes (pour 100.000 habitants)"
                  )
                }
            if(data_column_fr == "dc"){
              DF <- data_selected.def[, c("jour", "dc", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              DF$y <- DF$y*10e5/DF$PTOT
              myplot <- myplot+
                labs(x = "Date", y = "Patiens decedes / 100.000 habitants")+
                labs(title = "Nombre de Patients decedes du COVID-19"
                    , subtitle = "Donnees brutes (pour 100.000 habitants)"
                        )
                      }
          } else {
            if(data_column_fr == "pos"){
              DF <- data_selected.def[, c("jour", "pos", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              #DF$y <- DF$y*10e5/DF$PTOT

              myplot <- myplot+
                            labs(x = "Date", y = "Tests positifs / 100.000 habitants")+
                            labs(title = "Tests Positifs"
                                , subtitle = "Donnees brutes (pour 100.000 habitants)"
                                )
                                }
            if(data_column_fr == "neg"){
              DF <- data_selected.def[, c("jour", "neg", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              #DF$y <- DF$y*10e5/DF$PTOT
              myplot <- myplot+
                            labs(x = "Date", y = "Tests négatifs / 100.000 habitants")+
                            labs(title = "Tests Negatifs"
                                , subtitle = "Donnees brutes (pour 100.000 habitants)"
                                )
                                }
            if(data_column_fr == "hosp"){
              DF <- data_selected.def[, c("jour", "hosp", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              #DF$y <- DF$y*10e5/DF$PTOT
              myplot <- myplot+
                            labs(x = "Date", y = "Patiens hospitalises")+
                            labs(title = "Nombre de Patients Hospitalises pour COVID-19"
                                  , subtitle = "Donnees brutes"
                                )
                                    }
            if(data_column_fr == "rea"){
              DF <- data_selected.def[, c("jour", "rea", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              #DF$y <- DF$y*10e5/DF$PTOT
              myplot <- myplot+
                          labs(x = "Date", y = "Patiens en reanimation")+
                          labs(title = "Nombre de Patients en Reanimation pour COVID-19"
                                , subtitle = "Donnees brutes"
                              )
                                    }
            if(data_column_fr == "rad"){
              DF <- data_selected.def[, c("jour", "rad", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              #DF$y <- DF$y*10e5/DF$PTOT
              myplot <- myplot+
                          labs(x = "Date", y = "Patiens sortis")+
                          labs(title = "Nombre de Patients Retournes a domicile ('gueris')"
                                , subtitle = "Donnees brutes (pour 100.000 habitants)"
                              )

                                    }
            if(data_column_fr == "dc"){
              DF <- data_selected.def[, c("jour", "dc", "location", "PTOT")]
              names(DF) <- c("x", "y", "location", "PTOT")
              #DF$y <- DF$y*10e5/DF$PTOT
              myplot <- myplot+
                          labs(x = "Date", y = "Patiens decedes")+
                          labs(title = "Nombre de Patients decedes du COVID-19"
                                , subtitle = "Donnees brutes"
                              )
                            }
                  }
            }
        }
if(input$type_viz == "xyplot_fr"){
        if(input$xyplot_fr_data_x == "rea" & input$xyplot_fr_data_y == "sr"){
              if(input$echelle == "departement"){
                      DF <- aggregate(france.df[france.df$sexe == input$sexe, c("rea", "Rea_CHR_2018", "Rea_AUTRES_2018")]
                          , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                          , FUN = last
                          )
                      df_poly <- data.frame(x = c(-Inf, Inf, Inf)
                              , y = c(-Inf, Inf, -Inf)
                              )
                      MAX <- max(DF$rea, DF$Rea_CHR_2018+DF$Rea_AUTRES_2018)
                      myplot <- ggplot(DF, aes(rea, Rea_CHR_2018+Rea_AUTRES_2018))+
                            #geom_abline(slope = mean(DF$ratio)) +
                            #geom_abline(slope = 1) +
                            coord_fixed()+
                            geom_polygon(data=df_poly, aes(x, y), fill="red", alpha=0.2) +
                            xlim(0, MAX)+ ylim(0, MAX)+
                            xlab("Patients COVID")+ ylab("Capacite Totale en Lits")+
                            labs(title = "Saturation en Lits de Reanimation par les patients COVID"
                                , subtitle = "Zone Rouge : Departements satures"
                                )+
                            geom_point(aes(size = rea, colour = region_name), show.legend = FALSE)+
                            #geom_text_repel(aes(label = paste(Region, "(", ratio.pct, "%)")), size=3)+
                            geom_text_repel(aes(label = dep), size=3)+
                            theme_minimal()
                  } else {
                      DF <- aggregate(france.df[france.df$sexe == input$sexe, c("rea", "Rea_CHR_2018", "Rea_AUTRES_2018")]
                        , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                        , FUN = last
                        )
                        DF <- aggregate(DF[, -c(1:2)], by = list(Region = DF$region_name), FUN = sum)
                        df_poly <- data.frame(x = c(-Inf, Inf, Inf)
                              , y = c(-Inf, Inf, -Inf)
                              )
          MAX <- max(DF$rea, DF$Rea_CHR_2018+DF$Rea_AUTRES_2018)
          myplot <- ggplot(DF, aes(rea, Rea_CHR_2018+Rea_AUTRES_2018))+
                          #geom_abline(slope = mean(DF$ratio)) +
                          #geom_abline(slope = 1) +
                          coord_fixed()+
                          geom_polygon(data=df_poly, aes(x, y), fill="red", alpha=0.2) +
                          xlim(0, MAX)+ ylim(0, MAX)+
                          xlab("Patients COVID")+ ylab("Capacite Totale")+
                          labs(title = "Saturation en Lits de Reanimation par les patients COVID"
                              , subtitle = "Zone Rouge : Regions saturees"
                              )+
                          geom_point(aes(size = rea, colour = Region), show.legend = FALSE)+
                          #geom_text_repel(aes(label = paste(Region, "(", ratio.pct, "%)")), size=3)+
                          geom_text_repel(aes(label = Region), size=3)+
                          theme_minimal()
                          }
            }
    if(input$xyplot_fr_data_x == "hosp" & input$xyplot_fr_data_y == "sc") {
            if(input$echelle == "departement"){
                    DF <- aggregate(france.df[france.df$sexe == input$sexe, c("hosp", "SI_CHR_2018", "SI_AUTRES_2018", "SC_CHR_2018", "SC_AUTRES_2018")]
                              , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                              , FUN = last
                              )
                    df_poly <- data.frame(x = c(-Inf, Inf, Inf)
                                  , y = c(-Inf, Inf, -Inf)
                                  )
                    MAX <- max(DF$hosp, DF$SI_CHR_2018+DF$SI_AUTRES_2018)
                    myplot <- ggplot(DF, aes(hosp, SI_CHR_2018+SI_AUTRES_2018))+
                                #geom_abline(slope = mean(DF$ratio)) +
                                #geom_abline(slope = 1) +
                                coord_fixed()+
                                geom_polygon(data=df_poly, aes(x, y), fill="red", alpha=0.2) +
                                xlim(0, MAX)+ ylim(0, MAX)+
                                xlab("Patients COVID")+ ylab("Capacite Totale en Lits")+
                                labs(title = "Saturation en Lits d'Hospitalisation par les patients COVID"
                                    , subtitle = "Zone Rouge : Departements satures"
                                    )+
                                geom_point(aes(size = hosp, colour = region_name), show.legend = FALSE)+
                                #geom_text_repel(aes(label = paste(Region, "(", ratio.pct, "%)")), size=3)+
                                geom_text_repel(aes(label = dep), size=3)+
                                theme_minimal()
            } else {
                    DF <- aggregate(france.df[france.df$sexe == input$sexe, c("hosp", "SI_CHR_2018", "SI_AUTRES_2018")]
                            , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                            , FUN = last
                            )
                    DF <- aggregate(DF[, -c(1:2)], by = list(Region = DF$region_name), FUN = sum)
                    df_poly <- data.frame(x = c(-Inf, Inf, Inf)
                                  , y = c(-Inf, Inf, -Inf)
                                  )
                    MAX <- max(DF$hosp, DF$SI_CHR_2018+DF$SI_AUTRES_2018)
              myplot <- ggplot(DF, aes(hosp, SI_CHR_2018+SI_AUTRES_2018))+
                              #geom_abline(slope = mean(DF$ratio)) +
                              #geom_abline(slope = 1) +
                              coord_fixed()+
                              geom_polygon(data=df_poly, aes(x, y), fill="red", alpha=0.2) +
                              xlim(0, MAX)+ ylim(0, MAX)+
                              xlab("Patients COVID")+ ylab("Capacite Totale")+
                              labs(title = "Saturation en Lits d'Hospitalisation par les patients COVID"
                                  , subtitle = "Zone Rouge : Regions saturees"
                                  )+
                              geom_point(aes(size = hosp, colour = REGION), show.legend = FALSE)+
                              #geom_text_repel(aes(label = paste(Region, "(", ratio.pct, "%)")), size=3)+
                              geom_text_repel(aes(label = Region), size=3)+
                              theme_minimal()
                      }
        } else {
              data_column_x <- input$xyplot_fr_data_x
              data_column_y <- input$xyplot_fr_data_y
              ################    ON PEUT ICI PREPARER UNE DF "TOTALE" AVEC BOUCLES POUR AVOIR TOUTES LES DONNEES SOUS LA MAIN....
                    DF.0 <- setNames(
                                      aggregate(france.df[france.df$sexe == input$sexe & (france.df$jour > (Sys.Date()-10) & france.df$jour < (Sys.Date()-3)),  "incid"]
                                              , by = as.list(france.df[france.df$sexe == input$sexe & (france.df$jour > Sys.Date()-10 & france.df$jour < Sys.Date()-3), c("DEP" = "dep", "REGION" = "region_name")])
                                              , FUN = mean
                                              )
                                              , c("dep", "region_name", "J-7")
                                              )


                  DF.0[["J-14"]] <- aggregate(france.df[france.df$sexe == input$sexe & (france.df$jour > Sys.Date()-17 & france.df$jour < Sys.Date()-10),  "incid"]
                                , by = as.list(france.df[france.df$sexe == input$sexe & (france.df$jour > Sys.Date()-10 & france.df$jour < Sys.Date()-3), c("DEP" = "dep", "REGION" = "region_name")])
                                , FUN = mean
                                )$x

                  for (d in c("hosp", "rea", "dc", "pos", "incid")){
                        DF.0[[d]] <- aggregate(france.df[france.df$sexe == input$sexe,  d]
                                      , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                                      , FUN = last
                                      )$x
                                      }
                  DF.0$sc <- setNames(
                              apply(
                              aggregate(france.df[france.df$sexe == input$sexe,  c("SC_CHR_2018", "SC_AUTRES_2018")]
                                , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                                , FUN = last
                                )[, 3:4]
                                , 1, sum
                                )
                                , c("sc")
                                )

                  DF.0$si <- setNames(
                              apply(
                              aggregate(france.df[france.df$sexe == input$sexe,  c("SI_CHR_2018", "SI_AUTRES_2018")]
                                , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                                , FUN = last
                                )[, 3:4]
                                , 1, sum
                                )
                                , c("si")
                                )

                  DF.0$sr <- setNames(
                              apply(
                              aggregate(france.df[france.df$sexe == input$sexe,  c("Rea_CHR_2018", "Rea_AUTRES_2018")]
                                , by = as.list(france.df[france.df$sexe == input$sexe, c("DEP" = "dep", "REGION" = "region_name")])
                                , FUN = last
                                )[, 3:4]
                                , 1, sum
                                )
                                , c("sr")
                                )

              if(input$echelle == "departement"){
                    DF <- DF.0[, c("dep", "region_name", input$xyplot_fr_data_x, input$xyplot_fr_data_y, input$xyplot_fr_data_z)]
                    names(DF) <- c("location", "REGION", "X", "Y", "Z")




                    } else if(input$echelle == "region") {
                    DF <- DF.0[, c("dep", "region_name", input$xyplot_fr_data_x, input$xyplot_fr_data_y, input$xyplot_fr_data_z)]
                    DF <- aggregate(DF[, -c(1:2)], by = list(Region = DF$region_name), FUN = sum)
                    names(DF) <- c("location", "X", "Y", "Z")
                    }
                    myplot <- ggplot(DF, aes(X, Y))+
                                    #geom_abline(slope = mean(DF$ratio)) +
                                    #geom_abline(slope = 1) +
                                    #coord_fixed()+
                                    #geom_polygon(data=df_poly, aes(x, y), fill="red", alpha=0.2) +
                                    #xlim(0, MAX)+ ylim(0, MAX)
                                #    ylab("Incidence a J-7")+
                                #    xlab("Patients en reanimation")+
                                    ylab(input$xyplot_fr_data_y)+
                                    xlab(input$xyplot_fr_data_x)+
                                #     labs(title = "Patients en reanimation versus incidence a J-7"
                                     labs(title = paste(input$xyplot_fr_data_y, "vs", input$xyplot_fr_data_x)
                                        , subtitle = paste("echelle = ", input$echelle)
                                        )+
                                    geom_point(
                                      aes(
                                      #size = hosp,
                                      colour = location
                                      )
                                      , size = normalize(DF$Z, method = "scale", range = c(10, 1000))
                                      , show.legend = FALSE
                                      )+
                                    geom_text_repel(aes(label = location)
                                        , size = 3
                                        )+
                                    theme_minimal()
                }

        }
}


##
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

#########################

if(input$type_analyse == FALSE){
               if(input$type_viz == "courbe_fr"){
                  if(input$smoothed_fr){
                        myplot <- myplot+ geom_smooth(
                               data = DF, aes(x, y, colour = location),
                               method = "loess"
                               , size =  input$linewidth_fr
                               )
                  } else {
                        myplot <- myplot+
        #                      geom_line_interactive(DF, mapping = aes(x, y, colour = location
        #                        , tooltip = location, data_id = location
                              geom_line(DF, mapping = aes(x, y, colour = location
                              )
                              , size =  input$linewidth_fr
                              )
                              }
                       }

               if(input$type_viz == "xyplot_fr"
                      #| data_column_fr == "all" | input$type == "xyplot_fr" | input$R0_fr
                       )
                       {
                       myplot <- myplot
                       }
                   }


if(input$type_analyse == TRUE){
               if(input$type_comp == "R0_fr") {
                if(input$xcol == "rea"){
                           EST <- "'patients en reanimation pour COVID'"
                           }
                if(input$xcol == "hosp"){
                           EST <- "'patients hospitalises pour COVID'"
                           }
                if(input$xcol == "pos"){
                           EST <- "'nombre de tests positifs au COVID'"
                           }
                if(input$xcol == "dc"){
                           EST <- "'patients decedes du COVID'"
                           }
                 myplot <- ggplot(data = RES, aes(x = J, y = R0_point, colour = location))+
                             geom_line(size = 1)+
                             geom_ribbon(aes(ymin = R0_low, ymax = R0_high, colour = location), linetype = 2, alpha = 0.1)+
                             xlim(0, NA)+
                             ylim(-1, NA)+
                             geom_hline(
                             yintercept = 1, linetype = 5, colour = "black",
                             )+
                             geom_text(aes(min(J), 1, label = "R0 = 1", vjust = -1), colour = "black")+
                             ggtitle("Estimation du R0 sur fenêtres glissantes"
                             , subtitle = paste(
                                              "calcul base sur l'indicateur"
                                              , EST
                                              )
                                          )+
                             labs(x = "Temps (du passe a aujourd'hui) en jours", y = "Estimation du Nombre  Basique de Reproduction (R0)")+
                             xlim(-length(unique(RES$J)), 0)+
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
