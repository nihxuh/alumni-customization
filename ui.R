## ui.R ##

header <- dashboardHeader(
  title = paste0(nameInstitute, " Postdocs")
)

# https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header
# header$children[[2]]$children <-  tags$a(href='https://factor.niehs.nih.gov/2018/2/feature/feature-1-biomedical-trends/index.htm', tags$img(src='AroundTheWorldPD.jpg',height='60',width='60'))

side <- dashboardSidebar(
  # https://stackoverflow.com/questions/31013769/locking-r-shiny-dashboard-sidebar-shinydashboard
  sidebarMenu(#style = "position: fixed; overflow: visible;",
    id = "tabs",
    menuItem("Alumni project", tabName = "project", icon = icon("database")),
    menuItem("Career classification", tabName = "taxonomy", icon = icon("sitemap")),
    menuItem("Demographics", tabName = "demograph", icon = icon("venus-mars")),
    menuItem("Job locations", tabName = "location", icon = icon("globe")),
    menuItem("Career outcomes", icon = icon("bar-chart"), startExpanded = TRUE,
             menuSubItem("General distribution", tabName = "general", icon = icon("pie-chart")),
             menuSubItem("Relationship btw. categories", tabName = "relation", icon = icon("share-alt")),
             menuSubItem("Trends", tabName = "trend", icon = icon("line-chart"))),
             # menuSubItem("Highlights", tabName = "highlight", icon = icon("lightbulb-o"))),
    menuItem("Training time", tabName = "time", icon = icon("hourglass")),
    menuItem("Top 5 countries", tabName = "country", icon = icon("star")),
    menuItem("Doctoral degrees", tabName = "degree", icon = icon("graduation-cap")),
    # menuItem("Raw data", tabName = "rawdata", icon = icon("download")),
    # demograph conditionalPanel input
    conditionalPanel(
      condition = "input.tabs == 'demograph'",
      div(HTML("<strong>Select Inputs:</strong>"), style = "color:black;background-color:white;",
        # select time period
        selectInput("selectDmTp", label = "Choose time period to view", width="230px",
                    choices = c('Graduated in 2000-2014',
                                'Graduated in 2000-2004',
                                'Graduated in 2005-2009',
                                'Graduated in 2010-2014')),
        # select plot style
        selectInput("selectDmPc", label = "Plot category", width="230px",
                    choices = list("By job sector"=1, "By career type"=2, 
                                   "By job function"=3), selected = 1)
      )
    ),
    # location conditionalPanel input
    conditionalPanel(
      condition = "input.tabs == 'location'",
      div(HTML("<strong>Select Input:</strong>"), style = "color:black;background-color:white;",
        # select time period
        selectInput("selectLcTp", label = "Choose time period to view", width="230px",
                    choices = c('Graduated in 2000-2014',
                                'Graduated in 2000-2004',
                                'Graduated in 2005-2009',
                                'Graduated in 2010-2014'))
      )
    ),
    # general job conditionalPanel input
    conditionalPanel(
      condition = "input.tabs == 'general'",
      div(HTML("<strong>Select Input:</strong>"), style = "color:black;background-color:white;",
          # select time period
          selectInput("selectGnrTp", label = "Choose time period to view", width="230px",
                      choices = c('Graduated in 2000-2014',
                                  'Graduated in 2000-2004',
                                  'Graduated in 2005-2009',
                                  'Graduated in 2010-2014'))
      )
    ),
    # general job conditionalPanel input
    conditionalPanel(
      condition = "input.tabs == 'relation'",
      div(HTML("<strong>Select Input:</strong>"), style = "color:black;background-color:white;",
          # select time period
          selectInput("selectRlnTp", label = "Choose time period to view", width="230px",
                      choices = c('Graduated in 2000-2014',
                                  'Graduated in 2000-2004',
                                  'Graduated in 2005-2009',
                                  'Graduated in 2010-2014'))
      )
    ),
    # # highlight conditionalPanel input
    # conditionalPanel(
    #   condition = "input.tabs == 'highlight'",
    #   div(HTML("<strong>Select Input:</strong>"), style = "color:black;background-color:white;",
    #       # select time period
    #       selectInput("selectHlTp", label = "Choose time period to view", width="230px",
    #                   choices = c('Graduated in 2000-2014',
    #                               'Graduated in 2000-2004',
    #                               'Graduated in 2005-2009',
    #                               'Graduated in 2010-2014'))
    #   )
    # ),
    # training time conditionalPanel input
    conditionalPanel(
      condition = "input.tabs == 'time'",
      div(HTML("<strong>Select Input:</strong>"), style = "color:black;background-color:white;",
          # select time period
          selectInput("selectTmTp", label = "Choose time period to view", width="230px",
                      choices = c('Graduated in 2000-2014',
                                  'Graduated in 2000-2004',
                                  'Graduated in 2005-2009',
                                  'Graduated in 2010-2014'))
      )
    ),
    # top countries conditionalPanel input
    conditionalPanel(
      condition = "input.tabs == 'country'",
      div(HTML("<strong>Select Inputs:</strong>"), style = "color:black;background-color:white;",
          # select time period
          selectInput("selectCtTp", label = "Choose time period to view", width="230px",
                      choices = c('Graduated in 2000-2014',
                                  'Graduated in 2000-2004',
                                  'Graduated in 2005-2009',
                                  'Graduated in 2010-2014')),
          # select plot style
          selectInput("selectCtPc", label = "Plot category", width="230px",
                      choices = list("By job sector"=1, "By career type"=2, 
                                     "By job function"=3), selected = 1)
      )
    ),
    # degree conditionalPanel input
    conditionalPanel(
      condition = "input.tabs == 'degree'",
      div(HTML("<strong>Select Inputs:</strong>"), style = "color:black;background-color:white;",
          # select time period
          selectInput("selectDgTp", label = "Choose time period to view", width="230px",
                      choices = c('Graduated in 2000-2014',
                                  'Graduated in 2000-2004',
                                  'Graduated in 2005-2009',
                                  'Graduated in 2010-2014'))
      )
    )
  )
)

body <- dashboardBody(
  singleton(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
  ),
  
  tabItems(
    tabItem("project",
      fluidRow(
        column(12, align="right",
          actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
                       onclick =linkJScript),
          HTML("<p></p>")
        )
      ),
      fluidRow(
        valueBoxOutput("years"),
        valueBoxOutput("total"),
        valueBoxOutput("coverage")
      ),
      fluidRow(
        tabBox(
          title = "Introduction",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", width = 12,
          tabPanel("Background", includeHTML("background.html")),
          tabPanel("Data preparation", includeHTML("dataprep.html"))
        )
      )
    ),
 
       
    tabItem("taxonomy",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label = linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(
          title = "JOB TAXONOMY", width = 12, solidHeader = TRUE,
          "To make a comparison of alumni career outcomes, standard categories were created
           and assigned to each former fellow based on their job title,  employer and job
           description. A three-tiered taxonomy was developed--accompanied by definitions--to
           classify career outcomes by job sector, career type, and job function."
         )
      ),
      fluidRow(
        valueBoxOutput("jobSect", width=3),
        valueBoxOutput("jobType", width=3),
        valueBoxOutput("jobFunc", width=3),
        valueBoxOutput("jobExample", width=3)
      ),
      fluidRow(
        tabBox(
          title = "Job Categories",
          # The id lets us use input$tabset2 on the server to find the current tab
          id = "tabset2", width = 12,
          tabPanel("Sectors", includeHTML("employmentSector.html")),
          tabPanel("Career types", includeHTML("careerType.html")),
          tabPanel("Job functions", includeHTML("jobFunction.html")),
          tabPanel("Job classification examples", includeHTML("jobExample.html"))
        )
      )
    ),

        
    tabItem("demograph",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label = linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(
          title = "ALUMNI DEMOGRAPHICS", width = 12, solidHeader = TRUE,
          paste0("From ", minYrs, " to ", maxYrs, ", ", tMalePct, " percent of alumni are male, the rest are female. ", 
                tCitizenPct, " percent of alumni are US students, the rest are international students.")
        )
      ),
      fluidRow(
        uiOutput("demoInfoBox")
      ),
      div(id = "dmPlot", style="background-color:#d4e3fc;",
      fluidRow(
        column(4, align="center",
          selectInput("selectShowDm", label = "Choose data to view:",
                      choices = list("Gender only"=1, "Country origin only"=3, "Both"=2), selected = 2)
        ),
        column(4, align="center",
               sliderInput("sldWidthDm", "Plot Width (px)", min = 0, max = 1200, value = 500)
        ),
        column(4, align="center",
               sliderInput("sldHeightDm", "Plot Height (px)", min = 0, max = 1200, value = 700)
        )
      ),
      uiOutput("demoDynamicUI")
      )
    ),
    
    
    tabItem("location",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label = linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(
          title = "JOB LOCATION", width = 12, solidHeader = TRUE,
            "The majority of alumni remained in the U.S. after training. Among the alumni 
             that stayed in the U.S., the biggest group was in Michigan."
        )
      ),
      fluidRow(
        uiOutput("locaInfoBox")
      ),
      div(id = "lcPlot", style="background-color:#d4e3fc;",
          fluidRow(
            column(4, align="center",
                   selectInput("selectShowLc", label = "Choose data to view:",
                               choices = list("Migration only"=1, "State location only"=3, "Both"=2), selected = 2)
            ),
            column(4, align="center",
                   sliderInput("sldWidthLc", "Plot Width (px)", min = 0, max = 1200, value = 500)
            ),
            column(4, align="center",
                   sliderInput("sldHeightLc", "Plot Height (px)", min = 0, max = 1200, value = 500)
            )
          ),
          uiOutput("locaDynamicUI")
      )
    ),
    
    
    tabItem("general",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(title = "GENERAL CAREER OUTCOMES", width = 12, solidHeader = TRUE,
            paste0("From 2000 to 2014, a large pecentage ", max(tGnrSectorPct), "% of alumni are employed in '", names(which(tGnrSectorPct==max(tGnrSectorPct))), "' Employment Sector. ",
                   "More alumni ", max(tGnrTypePct), "% work as '", names(which(tGnrTypePct==max(tGnrTypePct))), "' than any other Career Type. ",
                   "The top Job Function is '", names(which(tGnrFuncPct==max(tGnrFuncPct))), "', with ", max(tGnrFuncPct), "% of alumni.")
        )
      ),
      fluidRow(
        uiOutput("genrInfoBox")
      ),
      div(id = "gnrPlot", style="background-color:#d4e3fc;",
        fluidRow(
          column(4, align="center",
            selectInput("selectShowGnr", label = "Choose data to view:",
                        choices = list("All"=1, "Employment Sector"=2, "Career Type"=3, "Job Function"=4), selected = 1)
          ),
          column(4, align="center",
                 sliderInput("sldWidthGnr", "Plot Width (px)", min = 0, max = 1200, value = 500)
          ),
          column(4, align="center",
                 sliderInput("sldHeightGnr", "Plot Height (px)", min = 0, max = 1200, value = 400)
          )
        ),
        uiOutput("genrDynamicUI")
      )
    ),

    tabItem("relation",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(title = "RELATIONSHIP BETWEEN EMPLOYMENT SECTORS, CAREER TYPES AND JOB FUNCTIONS", width = 12, solidHeader = TRUE,
            HTML("The width of the lines is proportional to the relative quantity of scholars within each group. 
                 <i>(Left & Middle) Division of employment sectors by career type.</i> Focusing on the academia sector as an example, most academic positions are in further training, primarily teaching or research career types. 
                 <i>(Middle & Right) Division of career types by job functions.</i> Focusing on primarily research career type as an example, it can be seen that the majority are tenured/tenure-track faculty or research staffs.</p><p>&nbsp;</p>")
        )
      ),
      # fluidRow(
      #   uiOutput("rltnInfoBox")
      # ),
      div(id = "rlnPlot", style="background-color:#d4e3fc;",
        fluidRow(
          column(6, align="center",
            sliderInput("sldWidthRln", "Plot Width (px)", min = 0, max = 1200, value = 800)
          ),
          column(6, align="center",
            sliderInput("sldHeightRln", "Plot Height (px)", min = 0, max = 1200, value = 600)
          )
        ),
        uiOutput("rltnDynamicUI")
      )
    ),

    
    # tabItem("highlight",
    #   fluidRow(
    #     column(12, align="right",
    #            actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
    #                         onclick =linkJScript),
    #            HTML("<p></p>")
    #     )
    #   ),
    #   fluidRow(
    #     box(title = "CAREER OUTCOME HIGHLIGHTS", width = 12, solidHeader = TRUE,
    #         "In job sub-groups, there are big differences between international fellows and US fellows.")
    #   ),
    #   div(id = "hlPlot", style="background-color:#d4e3fc;",
    #     fluidRow(
    #       column(4, align="center",
    #         selectInput("selectShowHl", label = "Choose data to view:",
    #                     choices = list("Tenure track"=1, "Additional postdoc"=3, "Both"=2), selected = 2)
    #       ),
    #       column(4, align="center",
    #              sliderInput("sldWidthHl", "Plot Width (px)", min = 0, max = 1200, value = 500)
    #       ),
    #       column(4, align="center",
    #              sliderInput("sldHeightHl", "Plot Height (px)", min = 0, max = 1200, value = 400)
    #       )
    #     ),
    #     uiOutput("hlDynamicUI")
    #   )
    # ),
    

    tabItem("trend",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(title = "CAREER OUTCOME TRENDS", width = 12, solidHeader = TRUE,
            "The distributions of alumni in Employment Sectors, Career Types and Job Functions change over time.")
      ),
      div(id = "trndPlot", style="background-color:#d4e3fc;",
        fluidRow(
          column(4, align="center",
            selectInput("selectShowTrnd", label = "Choose data to view:",
                        choices = list("All"=1, "Employment Sector"=2, "Career Type"=3, "Job Function"=4), selected=1)
          ),
          column(4, align="center",
                 sliderInput("sldWidthTrnd", "Plot Width (px)", min = 0, max = 1200, value = 500)
          ),
          column(4, align="center",
                 sliderInput("sldHeightTrnd", "Plot Height (px)", min = 0, max = 1200, value = 500)
          )
        ),
        uiOutput("trndDynamicUI")
      )
    ),

        
    tabItem("time",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(title = "TRAINING TIME", width = 12, solidHeader = TRUE,
            paste0("The overall average time spent at ", nameInstitute, " was ", cntAvgTime, 
                   " years. Sector Specific: Those entering the Academia sector spent an average of ", cntAcdTime,
                   " years at ", nameInstitute, " , Government ~ ", cntGvnTime, " years, and For-Profit sector ~ ", cntPrfTime, " years.")
        )
      ),
      fluidRow(
        uiOutput("tmInfoBox")
      ),
      div(id = "tmPlot", style="background-color:#d4e3fc;",
        fluidRow(
          column(4, align="center",
            selectInput("selectShowTm", label = "Choose data to view:",
                        choices = list("All"=1, "Employment Sector"=2, "Career Type"=3, "Job Function"=4, "Gender"=5, "Citizen"=6, "Funding"=7), selected = 1)
          ),
          column(4, align="center",
            sliderInput("sldWidthTm", "Plot Width (px)", min = 0, max = 1200, value = 500)
          ),
          column(4, align="center",
            sliderInput("sldHeightTm", "Plot Height (px)", min = 0, max = 1200, value = 500)
          )
        ),
        uiOutput("tmDynamicUI")
      )
    ),
    
    
    tabItem("country",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(
          title = "COMPARISON OF ALUMNI BASED ON THEIR COUNTRY OF ORIGIN", width = 12, solidHeader = TRUE,
            paste0("The ", paste(top5Ctry, collapse=', '), " are the ‘top five’ countries from which most ", nameInstitute, " trainees originate. 
                   There are some distinct differences in career outcomes depending on the country that a student originates from.  
                    We see differenes in the types of jobs they enter into, differences in gender distribution, differences in training time, 
                   and differences in job location.")
        )
      ),
      fluidRow(
        uiOutput("cntrInfoBox")
      ),
      div(id = "cntrPlot", style="background-color:#d4e3fc;",
        fluidRow(
          column(4, align="center",
            selectInput("selectShowCt", label = "Choose data to view:",
                        choices = list("All"=1, "Job Category"=2, "Training Time"=3, "Gender"=4, "Job Location"=5), selected = 1)
          ),
          column(4, align="center",
            sliderInput("sldWidthCt", "Plot Width (px)", min = 0, max = 1200, value = 500)
          ),
          column(4, align="center",
            sliderInput("sldHeightCt", "Plot Height (px)", min = 0, max = 1200, value = 400)
          )
        ),
        uiOutput("cntrDynamicUI")
      )
    ),

    
    tabItem("degree",
      fluidRow(
        column(12, align="right",
               actionButton(inputId = "goHome", label =linkText, class="pre_anchor",
                            onclick =linkJScript),
               HTML("<p></p>")
        )
      ),
      fluidRow(
        box(
          title = "DOCTORAL DEGREE FIELD OF STUDY", width = 12, solidHeader = TRUE,
          HTML("We also find career outcome differences between individuals in different fields of study.  
               In the graphs below, we used a threshold of N>10 in order for a field of study to be displayed in the
                 'total' year graph (2000-2014).  We used a threshold of N>5 in order for a field of study to be displayed in the '5-year 
                 period' graphs (ex:  2000-2004, 2005-2009, or 2010-2014).  The 95% <a href='https://en.wikipedia.org/wiki/Confidence_interval'>confidence intervals</a> of the binomial proportion are 
                 shown.")
        )
      ),
      fluidRow(
        uiOutput("degrInfoBox")
      ),
      div(id = "degrPlot", style="background-color:#d4e3fc;",
        fluidRow(
          column(4, align="center",
            selectInput("selectShowDg", label = "Choose data to view:",
              choices = list("All"=1, "Employment Sector"=2, "Career Type"=3, "Job Function"=4), selected = 1)
          ),
          column(4, align="center",
            sliderInput("sldWidthDg", "Plot Width (px)", min = 0, max = 1200, value = 500)
          ),
          column(4, align="center",
            sliderInput("sldHeightDg", "Plot Height (px)", min = 0, max = 1200, value = 400)
          )
        ),
        uiOutput("degrDynamicUI")
      )
    ),
    
    
    #####
    # tabItem("rawdata",
    #   "Only the top 25 rows are shown here. But the download contains whole data.",
    #   verbatimTextOutput("rawtable"),
    #   downloadButton("downloadCsv", "Download as CSV")
    # )
    
    tabItem("rawdata",
            "Only the top 5 rows are shown here. Download of whole data is disabled due to privacy concerns.",
            verbatimTextOutput("rawtable")
            # downloadButton("downloadCsv", "Download as CSV")
    )
    
  )
)

dashboardPage(
  header,
  side,
  body
)


# debug shiny app:
#   https://shiny.rstudio.com/articles/debugging.html
#   https://stackoverflow.com/questions/31920286/effectively-debugging-shiny-apps

# redirect URLs
#   https://stackoverflow.com/questions/9247067/when-someone-clicks-a-link-in-an-iframe-navigate-the-entire-window