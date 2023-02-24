# Celiac app (CAVE version)
# Contributor: Lindsay Hracs
# Date: 26-Jan-23
# R version 4.1.2

##########

# If you are adapting this code, please cite:

# Buie MJ, Quan J, Windsor JW, et al. Global hospitalization trends for Crohn's disease and ulcerative colitis in the 21st century:
# A systematic review with temporal analyses. Clin. Gastroenterol. Hepatol. 2022. doi: https://doi.org/10.1016/j.cgh.2022.06.030
# Shiny app: https://kaplan-gi.shinyapps.io/hospitalization/

# King JA, Bakal JA, Bing L, et al. Variation in testing for and incidence of celiac autoimmunity in Canada: A population-based study. 
# Gastroenterology. 2023. doi: https://doi.org/10.1053/j.gastro.2022.12.040
# Shiny app: https://kaplan-gi.shinyapps.io/CAVE/

# Windsor JW, Hracs LH, Gorospe J, et al. The global evolution of inflammatory bowel disease across four epidemiologic stages:
# A systematic review of incidence and prevalence studies over the past century. Gastroenterology. To appear.
# Shiny app: https://kaplan-gi.shinyapps.io/GIVES21/

##########


source("global.R", local = TRUE)$value
source("testing.R", local = TRUE)$value
source("incidence.R", local = TRUE)$value
source("various_vis.R", local = TRUE)$value

# version notes
# 1. change UC logo to CSM Centre for Health Informatics link and logo
# 2. add CAVE title to title bar

ui <- fluidPage(theme = shinytheme("united"),
    
    useShinyjs(),
    
    title = "Celiac disease Analytical Visualization Environment",
    
    # format Shiny error messages
    tags$head(tags$style(HTML(".shiny-output-error-validation {text-align: center; color: #687B9C; font-weight: bold; font-size: 125%;}"))),
    # format link style
    tags$head(tags$style(HTML("a {color: #408697}"))),
    # format action button
    tags$head(tags$style(HTML(".btn {color:rgb(255,255,255); border-color: #408697; background-color: #408697;}"))),
    # format logo placement
    tags$head(tags$style(HTML(".logo {margin-left: 10px; margin-right: 10px; margin-bottom: 20px; vertical-align: middle;}"))),
    # format slider bar (for eachslider bar, withincreasing increments on js)
    tags$head(tags$style(HTML(".js-irs-0 .irs-bar {background-color: transparent; border-color: transparent}"))),
    tags$head(tags$style(HTML(".js-irs-0 .irs-single {color: black; background: #BEC4C6}"))),
    tags$head(tags$style(HTML(".js-irs-1 .irs-bar {background-color: transparent; border-color: transparent}"))),
    tags$head(tags$style(HTML(".js-irs-1 .irs-single {color: black; background: #BEC4C6}"))),
    tags$head(tags$style(HTML(".js-irs-2 .irs-bar {background-color: transparent; border-color: transparent}"))),
    tags$head(tags$style(HTML(".js-irs-2 .irs-single {color: black; background: #BEC4C6}"))),
    tags$head(tags$style(HTML(".js-irs-3 .irs-bar {background-color: transparent; border-color: transparent}"))),
    tags$head(tags$style(HTML(".js-irs-3 .irs-single {color: black; background: #BEC4C6}"))),
    tags$head(tags$style(HTML(".js-irs-4 .irs-bar {background-color: transparent; border-color: transparent}"))),
    tags$head(tags$style(HTML(".js-irs-4 .irs-single {color: black; background: #BEC4C6}"))),
    tags$head(tags$style(HTML(".js-irs-5 .irs-bar {background-color: transparent; border-color: transparent}"))),
    tags$head(tags$style(HTML(".js-irs-5 .irs-single {color: black; background: #BEC4C6}"))),
    
    # format slider input play button
    tags$head(tags$style(HTML(".slider-animate-button {font-size: 20px; color: #52D6F4;}"))),
    

    ########## CAVE header formatting ##########
    
    tags$header(tags$style(HTML(".box {white-space: nowrap; 
                              width: 150px;
                              overflow: hidden;
                              border: 2px solid #408697;
                              background-color: #363538;}"))),
    #tags$header(tags$style(HTML(".box:hover {overflow: visible;}"))),
    tags$header(tags$style(HTML(".wordart { font-family: Arial, sans-serif; 
                              font-size: 1em; 
                              font-weight: bold;
                              position: relative;
                              z-index: 1;
                              display: inline-block;
                              -webkit-font-smoothing: antialiased;
                              -moz-osx-font-smoothing: grayscale; }"))),
    tags$header(tags$style(HTML(".wordart.italic-outline { transform: scale(1, 1.3);
                              -webkit-transform: scale(1, 1.3);
                              -moz-transform: scale(1, 1.3);
                              -o-transform: scale(1, 1.3);
                              -ms-transform: scale(1, 1.3); }"))),
   tags$header(tags$style(HTML(".wordart.italic-outline .text { letter-spacing: -0.01em;
                              font-family: Arial, sans-serif;
                              font-weight: bold;
                              color: #fff;
                              -webkit-text-stroke: 0.01em #000;
                              filter: progid:DXImageTransform.Microsoft.Glow(Color=#000000, Strength=1);
                              text-shadow: 0.03em 0.03em 0 #408697; 
                              background-color: #363538;}"))),

    
    div(titlePanel(HTML("<p style = 'font-size: 75%;'>&nbsp</p>
                        <div class = 'wordart italic-outline'>&nbsp<div class = 'box' style = 'text-overflow:ellipsis; display: inline; text-align: center;'><span class = 'text'>&nbspC&nbsp</span></div></div><div class = 'wordart italic-outline'><span class = 'text'>eliac disease&nbsp&nbsp</span></div><div class='wordart italic-outline'><div class = 'box' style = 'text-overflow:ellipsis; display: inline; text-align: center;'><span class = 'text'>&nbspA&nbsp</span></div></div><div class = 'wordart italic-outline'><span class = 'text'>nalytical&nbsp&nbsp</span></div><div class='wordart italic-outline'><div class = 'box' style = 'text-overflow:ellipsis; display: inline; text-align: center;'><span class = 'text'>&nbspV&nbsp</span></div></div><div class = 'wordart italic-outline'><span class = 'text'>isualization&nbsp&nbsp</span></div><div class='wordart italic-outline'><div class = 'box' style = 'text-overflow:ellipsis; display: inline; text-align: center;'><span class = 'text'>&nbspE&nbsp</span></div></div><div class = 'wordart italic-outline'><span class = 'text'>nvironment</span></div>
                    <p style = 'color: #FFFFFF; font-size: 75%;'><br>&nbsp;&nbsp;Variation in the testing for and incidence of celiac disease autoimmunity throughout Alberta, Canada<br><br></p>")
    ), style = 'background-color: #363538;'), # wrapped in div to create static background for all elements
    
    tabsetPanel(id = "tabs",
        
        tabPanel(value = "incidence", htmltools::tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Incidence"),
            incidenceUI("incidence")         
        ),
        
        tabPanel(value = "testing", htmltools::tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "Testing Rates"),
            testingUI("testing")
        ),
        
        tabPanel(value = "various_vis", htmltools::tags$header(style = "text-align:center; font-weight: bold; font-size:125% ;", "3D Plots"),
            various_visUI("various_vis")
        )
        

    )
)

server <- function(input, output, session) {
    
    incidenceServer("incidence")
    testingServer("testing")
    various_visServer("various_vis")

}

shinyApp(ui, server)