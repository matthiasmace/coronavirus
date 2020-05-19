##  data France

, fluidRow(
column(12,
  h1("CovId-19 for People", align="center")
  , h2("SARS-CoV-2 pandemics data display & analysis Webpage for the people", align="center")
  , p("Données Source",
          a("INSEE ???",
            href="https://ourworldindata.org/coronavirus"),
          "| Link to the dataset (last updated ",
          modifdate,
          "):",
          a("https://covid.ourworldindata.org/data/ecdc/full_data.csv",
            href = "https://covid.ourworldindata.org/data/ecdc/full_data.csv")
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
                             , choices = c("Hospitalisés" = "hosp"
                             , "Réanimation" = "rea"
                             , "Sorties" = "rad"
                             , "Décédés" = "dec"
                             )
                             , selected = "hosp"
                             )
                , selectInput(inputId = "dep_sel"
                            , label = "Départements (with at least 1 case):"
                            , list('Occitanie' = unique(france.df[france.df$region == 'Occitanie',]$dep),
                              #   'Africa' = unique(covdat[covdat$continent == 'Africa',]$location),
                              #   'Americas' = unique(covdat[covdat$continent == 'Americas',]$location),
                              #   'Asia' = unique(covdat[covdat$continent == 'Asia',]$location),
                              #   'Oceania' = unique(covdat[covdat$continent == 'Oceania',]$location)
                                 )
                            , selected = c(66, 31, 47, 11, 75, 67, 68)
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
               plotOutput(outputId = "distPlot", width="100%", height=750)
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




















france.df <- as.data.frame(read.csv("63352e38-d353-4b54-bfd1-f1b3ee1cabd7", header = T, sep =";"))
france.df$jour <- as.Date(france.df$jour)
france.df$dep <- as.character(as.numeric(france.df$dep))


input <- list(data_column = "nouveaux_deces"
              , dep_sel = c("68", "75", "31", "75", "13", "66")
              , SI.min = 4
              , SI.max = 8
              , num_min = 1
)

require(EpiEstim)
if(input$data_column == "nouveaux_cas"){
    DAT.0 = france.df[france.df$dep %in% input$dep_sel & france.df$sex ==0, c(1, 3, 4)]
} else if(input$data_column == "nouveaux_deces") {
    DAT.0 = france.df[france.df$dep %in% input$dep_sel & france.df$sex ==0, c(1, 3, 5)]
} else {stop(safeError(("Incompatible Data to show / plot option combination")))}
#
names(DAT.0) <- c("location", "date", "data")
#
print(DAT.0)
#
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
  DAT.1 <- DAT.0[DAT.0$location == c  & (DAT.0$data >= input$num_min), ]
  rownames(DAT.1) <- DAT.1$date
  DAT.1 <- DAT.1[, -c(1, 2)]
  es_uncertain_si <- estimate_R(DAT.1,
                                 method = "uncertain_si",
                                 config = config)
  #
  max.length <- max(table(DAT.0[DAT.0$data > input$num_min, c("location")]))
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





#######
ggplot(data = RES, aes(x = J, y = R0_point, colour = location)) +
geom_line(size = 3)+
geom_ribbon(aes(ymin=R0_low, ymax=R0_high, colour = location), linetype=2, alpha=0.2)+
xlim(0, NA)+
geom_hline(
      yintercept = 1,
      )+
labs(x = "Time in days", y = "Basic Reproduction Number (R) estimates")+
xlim(-length(unique(RES$J)), 0)+
theme_minimal()
