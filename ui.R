# Load required packages
pacman::p_load(
  rio,
  here,
  janitor,
  bslib,
  bsicons,
  DT,
  shiny,
  shinyWidgets,
  thematic,
  gghighlight,
  plotly,
  highcharter,
  maps,
  cowplot,
  gridExtra,
  ggrepel,
  ggtext,
  glue,
  tidyverse
)

# Start the UI
ui <- page_navbar(
  title = "AMR Trend Explorer", fluid = TRUE, id = "navbar",
  theme = bs_theme(version = 4, bootswatch = "yeti"),
  
  # Article Info
  nav_panel(
    title = "Article Info",
    fluidRow(column(width = 12, em("This is the supplementary materials for the article:"), 
                    br(),
                    strong('Association between national policy and trends in antibiotic resistance: an analysis of 73 countries from 2000 to 2023'),
                    br(),
                    "Peter Søgaard Jørgensen", tags$sup("1,2,3,*,"), "Luong Nguyen Thanh", tags$sup("1,3,"), "Ege Pehlivanoglu", tags$sup("1,"), "Franziska Klein", tags$sup("1, #a,"), 
                    "Didier Wernli", tags$sup("4,"), "Dusan Jasovsky", tags$sup("5, #b,"), "Athena Aktipis", tags$sup("6,"), "Rob R. Dunn", tags$sup("7,"), "Yrjo Gröhn", tags$sup("8,"), 
                    "Guillaume Lhermie", tags$sup("8, #c,"), "H. Morgan Scott", tags$sup("9,"), "Eili Y. Klein", tags$sup("10,11"),
                    br(),
                    br(),
                    strong("Affiliations:"),
                    br(),
                    "1. Global Economic Dynamics and the Biosphere, The Royal Swedish Academy of Sciences, SE-114 18, Stockholm, Sweden",
                    br(),
                    "2. Stockholm Resilience Centre, Stockholm University, SE-106 91, Stockholm, Sweden",
                    br(),
                    "3. Uppsala Antibiotic Centre and Department of Women’s and Children’s Health, Uppsala University, SE-751 05, Uppsala, Sweden",
                    br(),
                    "4. University of Geneva, Global Studies Institute, Transformative Governance Lab, CH-1211 Genève 4, Switzerland",
                    br(),
                    "5. Uppsala University, ReAct Europe, SE-753 10, Uppsala, Sweden",
                    br(),
                    "6. Arizona State University, Department of Psychology, Tempe, AZ 85281, USA",
                    br(),
                    "7. North Carolina State University, Department of Applied Ecology, Raleigh, NC 27695-7617, USA",
                    br(),
                    "8. Cornell University, College of Veterinary Medicine, Ithaca, NY 14853, USA",
                    br(),
                    "9. Texas A&M University, College Station, TX 77843-4467, USA",
                    br(),
                    "10. One Health Trust, Washington, D.C. 20015, USA",
                    br(),
                    "11. Johns Hopkins School of Medicine, Department of Emergency Medicine, Baltimore, MD 21205, USA",
                    br(),
                    br(),
                    strong("Current addresses:"),
                    br(),
                    tags$sup("#a."), "Mercator Research Institute on Global Commons and Climate Change",
                    br(),
                    tags$sup("#b."), "Médicins Sans Frontières (MSF) International",
                    br(),
                    tags$sup("#c."), "Ecole Nationale Véterinaire Toulouse",
                    br(),
                    br(),
                    em("* Corresponding author E-mail: peter.sogaard.jorgensen@su.se (PSJ)"),
                    br(),
                    br(),
                    strong("Table of Contents"),
                    br(),
                    tags$ul(
                      tags$li(tags$strong("Annex 1. Main results")),
                      tags$ul(
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'main-fig2')", "Figure 2. Association between stated action and linear trend, and categorial trend")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'main-fig3')", "Figure 3. De-escalation of DPSE. De-escalation ratios for Drivers, Use, Resistance, and DRI")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'main-fig4')", "Figure 4. Most important variables in model selection")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'main-fig5')", "Figure 5. Actions levels in animal protein produced countries for animal and human related Antibitotic Indicators")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'main-fig6')", "Figure 6. Classification of country ABR governance syndrome"))
                      ),
                      
                      
                      tags$li(tags$strong("Annex 2. Methodology")),
                      tags$ul(
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'dpsea-description')", "DPSEA indicators description")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'country-selection')", "S1 Table - List of countries included in the study")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'indicator-used')", "S2 Table - Indicator selection for Driver, Use, Resistance, and DRI categories")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'governance-syndrome')", "S3 Table – Governance Syndrome questions")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'action-index')", "S4 Table – Questions used for calculating the action index")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'model-formulas')", "S5 Table – Model Formulas for Association between Action and Indicator change and sign of change")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'de-escalation-plots')", "S6 Table. De-escalation plot formulas for univariate models")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'global-models-data')", "S7 Table - Global Models Data Subset Formulas for The Model Selection")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'global-models-formulas')", "S8 Table - Global Model Formulas for The Model Selection"))
                      ),
                      
                      
                      
                      tags$li(tags$strong("Annex 3. Supporting results")),
                      tags$ul(
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'choropleth-plots')", "Figure S1. Changes of ABR DPSE- indicators over time (2000-2016) in 73 countries")),
                        tags$li(tags$a(href = "#", onclick = "Shiny.setInputValue('navTo', 'governance-changes')", "Figure S2. Changes of governance action between 2016 and 2023")),
                      )
                    )
    ))
  ),
  
  # Main results,
  navbarMenu(
    "Main results",
    tabPanel(
      "Association between stated action and DPSE. indicators",
      value = "main-fig2",
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput("fig2_income", "Select country income:",
                        choices = c("LMIC", "HIC"),
                        selected = c("LMIC", "HIC"), multiple = TRUE),
            pickerInput("fig2_outcome", "Select outcome type:",
                        choices = c("Linear Trend", "Categorical Trend"),
                        selected = c("Linear Trend", "Categorical Trend"), multiple = TRUE)),
          fluidRow(
            # Plain static text description
            tags$p(HTML("<strong>Figure 2. Association between stated action and linear trend (indicator change A-D), and sign of change (categorical trend E-H).</strong> <em>Indicator p-values are from linear mixed models with country income group as random effect. For detailed indicators, please see S5 Table. Blue represents drivers of antibiotics resistance, purple represents antibiotics use, orange represents resistance, and red represents DRI.</em>"))
          )),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("adaptive_plot", height = 800, width = "100%"),
        ))
    ),
    
    tabPanel(
      "De-escalation ratios of DPSE.",
      value = "main-fig3",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            # Plain static text description
            tags$p(HTML("<strong>Figure 3. De-escalation ratios of DPSE indicators for Drivers, Use, Resistance, and DRI.</strong> <em>De-escalating ratio is defined as the proportion of available lower-level indicators within a category that have witnessed a reduction from 2000 to 2016. Weight refers to the proportion of variables avaiable within a category. Uncertainty bands indicate standard errors.</em>"))
          )),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("fig3_plot", height = 800, width = "100%"),
        ))
      ),
    
    
    tabPanel(
      "Most important variables in model selection",
      value = "main-fig4",
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput("fig4_outcome", "Select outcome type:",
                        choices = c("Linear Trend", "Categorical Trend"),
                        selected = "Linear Trend", multiple = TRUE)),
          fluidRow(
            # Plain static text description
            tags$p(HTML("<strong>Figure 4. Most important variables in model selection.</strong> <em>The rank of the five most important variables (rows) is shown using color coding. Each column represents a unique model selection procedure on the linear trend (change, 17 procedures) or the categorical trend (binomial, 16 procedures). Model names refers to the subset of the dataset with certain DPSE indicators including D (Driver), P (Pressure), S (State), E (Exposure). noDr indicates exclusion of health system variables as explanatory variables. aX refers to analysis of DPSE for country subsets with X variable available. See S7-S8 Tables for details on each model selection procedure..</em>"))
          )),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotOutput("fig4_plot", height = 800, width = "100%"),
        ))
      ),
    tabPanel(
      "ABR action levels in animal protein-producing countries",
      value = "main-fig5",
      sidebarLayout(
        sidebarPanel(
          # fluidRow(h3(strong("Filters"))),
          # fluidRow(
          #   pickerInput("fig5_income", "Select country income:",
          #               choices = c("LMIC", "HIC"),
          #               selected = c("LMIC", "HIC"), multiple = TRUE)),
          fluidRow(
            # Plain static text description
            tags$p(HTML("<strong>Figure 5. Actions levels in animal protein produced countries for animal and human related Antibitotic Indicators.</strong> <em>Action level of governments belong to countries with large producers of animal protein. Stated government action animal and human health scores in relation to total production of vertebrate biomass (mammals, birds and fish) for high-income (HICs) and low- and middle-income countries (LMICs).</em>"))
          )),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("fig5_plot", height = 800, width = "100%"),
        ))
      ),
    
    
    
    
    tabPanel(
      "Classification of country ABR governance syndrome",
      value = "main-fig6",
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput("fig6_income", "Select country income:",
                        choices = c("LMIC", "HIC"),
                        selected = c("LMIC", "HIC"), multiple = TRUE),
            pickerInput("fig6_outcome", "Select outcome type:",
                        choices = c("Linear Trend", "Categorical Trend"),
                        selected = c("Linear Trend", "Categorical Trend"), multiple = TRUE),
            pickerInput("fig6_indicators", "Select indicators type:",
                        choices = c("DRIVERS", "USE", "RESISTANCE", "DRI"),
                        selected = c("DRIVERS", "USE", "RESISTANCE", "DRI"), multiple = TRUE)),
          fluidRow(
            # Plain static text description
            tags$p(HTML("<strong>Figure 6. Classification of country ABR governance syndrome.</strong> <em>(A) categorize country trajectory based on trend in ABR indicators and governmental action. Countries in vicious cycle are displayed with name ISO3 code (see S1 Table). (B) Comparison of country governance syndrome according to DPSE indicators and income level..</em>"))
          )),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("governance_syndrome1", height = 800, width = "100%"),
        ))
    )
  ),
  
  
  # Methodology
  navbarMenu(
    "Methodology",
    tabPanel(
      "DPSEA indicators description",
      value = "dpsea-description",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("DPSEA Indicators"))
        ),
        
        br(),
        fluidRow(
          column(12, strong("Drivers – health system"), span("Driving forces behind human antibiotic use is captured by analyzing the trends in time series data for fifteen variables across four tier 2 indicators including infection prevalence (primary driver), sanitation standards, vaccination coverage, and health care workforce. Total data for drivers are compiled for a total of 219 countries from the United Nations (UN), World Bank database and the World Health Organization (WHO). Data availability varies by indicator as detailed in S2 Table. "))
        ),
        br(),
        
        fluidRow(
          column(12, strong("Pressure - antibiotic use (ABU)"), span("Data from QuintilesIMS are estimates of the total volume of sales of each antibiotic molecule (or combination of molecules) based on national sample surveys of antibiotic sales. Antibiotic consumption data are in kilograms and converted into defined daily doses (DDDs) using the Anatomical Therapeutic Chemical Classification System (ATC/DDD, 2016) developed by the WHO Collaborating Centre for Drug Statistics Methodology as in Klein et al. [1]."))
        ),
        br(),
        
        fluidRow(
          column(12, strong("State - resistance"), span("Data obtained from ResistanceMap [2] which is a repository of global antimicrobial resistance data. ResistanceMap obtains data from public and private sources, including lab networks, hospitals, and government agencies. Data include resistance rates for eight high-priority pathogens isolated from blood and cerebrospinal fluid of patients and are aggregated at the country level on an annual basis. Data on ResistanceMap has been harmonized to present similar definitions of resistance across countries and regions to enable comparisons between countries."))
        ),
        br(),
        
        fluidRow(
          column(12, strong("Exposure – Drug resistance index"), span("The Drug Resistance Index (DRI) combines use and resistance rates into a single value that provides measures of antibiotic effectiveness relative to their use [3]. While DRI has been critiqued when used as a single indicator of antibiotic effectiveness [4], we here use it as part of a multi-indicator framework. We calculated the adaptive Drug Resistance Index for countries for which data on resistance and use is available over the time period following the methodology outlined in [3] and [5]. Briefly, the annual DRI was estimated for each country by the following equation:"))),
        
        withMathJax(),
        helpText('$$DRI = \\sum_k \\rho_k^{i,t} q_k^{i,t}$$'),
        fluidRow(
          column(12, span('where, for country i at time t, \\(\\rho_k^{i,t}\\) is the proportion of resistance among all included organisms to drug k and \\(\\ q_k^{i,t}\\) is the proportion of drug k used for their treatment in all drugs included in the index. Pathogens included in the analysis were E. coli, K. pneumoniae, P. aeruginosa, S. aureus, E. faecium, and E. faecalis. Antibiotics included in the analysis were aminoglycosides, broad-spectrum penicillin, carbapenems, cephalosporins, narrow-spectrum penicillin, and quinolones. Because not all countries had data for all combinations, we included a country if they had at least four of the six organisms, and 10 of the 17 total combinations possible (S2 Table).'))),
        br(),
        
        fluidRow(
          column(12, strong("Action – TrACSS"), span("All action indicators are self-reported data from the Global Database for Tracking Antimicrobial Resistance Country Self-Assessment Survey (TrACSS) spanning the period of 2016-2023 (https://amrcountryprogress.org/). The survey responses are publicly available with the yearly updated version providing information about countries ongoing actions to live up to the global action plan on antimicrobial resistance All answers are given on an ordinal scale from A to E (0-4)."))
        ),
        br(),
        
        fluidRow(
          column(12, strong("REFERENCE"))
        ),
        br(),
        fluidRow(
          column(12, span("1. Klein EY, Milkowska-Shibata M, Tseng KK, Sharland M, Gandra S, Pulcini C, et al. Assessment of WHO antibiotic consumption and access targets in 76 countries, 2000–15: an analysis of pharmaceutical sales data. The Lancet Infectious Diseases. 2021;21: 107–115. doi:10.1016/S1473-3099(20)30332-7"))),
        br(),
        fluidRow(
          column(12, span("2. OneHealthTrust. ResistanceMap. [cited 1 Sep 2017]. Available: https://resistancemap.onehealthtrust.org/"))),
        br(),
        fluidRow(
          column(12, span("3. Laxminarayan R, Klugman KP. Communicating trends in resistance using a drug resistance index. BMJ Open. 2011;1: e000135–e000135. doi:10.1136/bmjopen-2011-000135"))),
        br(),
        fluidRow(
          column(12, span("4. Vandenbroucke-Grauls CMJE, Kahlmeter G, Kluytmans J, Kluytmans-Van Den Bergh M, Monnet DL, Simonsen GS, et al. The proposed Drug Resistance Index (DRI) is not a good measure of antibiotic effectiveness in relation to drug resistance. BMJ Global Health. 2019;4: 1–3. doi:10.1136/bmjgh-2019-001838"))),
        br(),
        fluidRow(
          column(12, span("5. Pant S, Klein E, Gandra S, Laxminarayan R. Tracking Antibiotic Effectiveness Worldwide 1999–2014 Using the Drug Resistance Index. Open Forum Infectious Diseases. 2016;3: 1481. doi:10.1093/ofid/ofw172.1183")))
      )
    ),
    tabPanel(
      "Country selection",
      value = "country-selection",
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("S1 Table -"), span("List of countries included in the study"), align = "center")
        ),
        fluidRow(
          column(12, 
                 p("The total number of countries included in the study is 73. ISO3 codes refers to three-letter country codes according to ISO 3166-1.",align = "center", style = "margin-bottom: 20px; font-style: italic;"))),
        br(),
        fluidRow(
          DTOutput("country_table")
        )
      )
    ),
    tabPanel(
      "Indicators used in the study",
      value = "indicator-used",
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("S2 Table -"), span("Indicator selection for Driver, Use, Resistance, and DRI categories"), align = "center")
        ),
        br(),
        fluidRow(
          DTOutput("indicators_table")
        ),
        tags$h4("DRIVERS: FACTORS INFLUENCING ANTIBIOTIC USE"),
        DTOutput("drivers_table"),
        tags$h4("USE: PRESSURES FOR RESISTANCE TO SPREAD"),
        DTOutput("use_table"),
        tags$h4("RESISTANCE: CURRENT STATE OF ANTIBIOTIC RESISTANCE"),
        DTOutput("resistance_table"),
        tags$h4("DRUG RESISTANCE INDEX (DRI): PATHOGEN-ANTIBIOTIC COMBINATIONS"),
        DTOutput("dri_table"),
        tags$h4("ECOLOGICAL VARIABLES"),
        DTOutput("ecological_variables_table"),
        br(),
        fluidRow(
          column(12, strong("REFERENCE"))
        ),
        br(),
        fluidRow(
          column(12, span("1. Klein EY, Milkowska-Shibata M, Tseng KK, Sharland M, Gandra S, Pulcini C, et al. Assessment of WHO antibiotic consumption and access targets in 76 countries, 2000–15: an analysis of pharmaceutical sales data. The Lancet Infectious Diseases. 2021;21: 107–115. doi:10.1016/S1473-3099(20)30332-7"))),
      )
    ),
    tabPanel(
      "Governance syndrome questions",
      value = "governance-syndrome",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("S3 Table -"), span("Governance Syndrome questions"), align = "center")
        ),
        fluidRow(
          column(12, 
                 p("Questions used and their sectors included for the calculation of governance syndrome for the years between 2016-2022 according to TrACSS (1.1) 2016-2017 version. All surveys are available online (https://amrcountryprogress.org/#/download-responses). Only questions asked in all surveys were selected for the analysis. In case a question is separated in two questions, they were still kept for the analysis. Answers converted from the scale of A-E to 0-4. Categories’ scores were averaged from their constituted questions, and the overall governance score was calculated as the mean score of all categories.  NAP stands for National Action Plan",
                   style = "margin-bottom: 20px; font-style: italic;"))),
        br(),
        fluidRow(
          DTOutput("governance_syndrome"))
      )
    ),
    
    tabPanel(
      "Action index questions",
      value = "action-index",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("S4 Table -"), span("Questions used for calculating the action index"), align = "center")
        ),
        fluidRow(
          column(12, 
                 p("Topics and titles of questions from the TrACSS (1.1) survey in 2016-17 used to calculate the action index as well as the sub-categories used to group questions within a similar theme (https://amrcountryprogress.org/#/download-responses). Survey was answered on a scale of A-E and converted to 0-4.",
                   style = "margin-bottom: 20px; font-style: italic;"))),
        br(),
        fluidRow(
          DTOutput("action_index_table")),
        fluidRow(
          column(12, 
                 p("*Subcategories included in Animal Data Analysis and Figure 4",
                   style = "margin-bottom: 20px; font-style: italic;")))
      )
    ),
    
    tabPanel(
      "Model formulas",
      value = "model-formulas",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("S5 Table -"), span("Model Formulas for Association between Action and Indicator Change and Categorical Trend"), align = "center")
        ),
        fluidRow(
          column(12, 
                 p("Formulas for generalized linear mixed models to investigate the association between linear trend of indicators in 16 years and action. First type of model included linear trend of indicators as response variable. The second type of model included Action as a response variable and categorical trend as an explanatory variable to investigate the action difference between the countries with positive vs negative change. Results shown in Figure 2 for tier 1 indicators.",
                   style = "margin-bottom: 20px; font-style: italic;"))),
        br(),
        fluidRow(
          DTOutput("model_formulas"))
      )
    ),
    tabPanel(
      "De-escalation plot formulas",
      value = "de-escalation-plots",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("S6 Table -"), span("De-escalation plot formulas for univariate models"), align = "center")
        ),
        fluidRow(
          column(12, 
                 p("Formulas used for binomial general linear model to investigate the association between de-escalation of categories and action level. Declining proportion refers to declining numbers of tier 2 indicators divided by the total number of tier 2 indicators for countries, Baseline Mean refers to mean of baseline for tier 2 indicators for each DPSE indicator (See S2 Table). Models weighted by total number of tier 2 indicators within each DPSE indicator for specific country.",
                   style = "margin-bottom: 20px; font-style: italic;"))),
        br(),
        fluidRow(
          DTOutput("de_escalation_plots"))
      )
    ),
    tabPanel(
      "Multivariate model selection",
      value = "global-models-data",
      fluidPage(
        br(),
        fluidRow(
          column(12, strong("S7 Table -"), span("Global Models Data Subset Formulas for The Model Selection"), align = "center")
        ),
        fluidRow(
          column(12, 
                 p("Global Models for multivariate model selection data subsets.", align = "center", style = "margin-bottom: 20px; font-style: italic;"))
        ),
        br(),
        fluidRow(
          DTOutput("global_models_data")
        ),
        br(),
        br(),
        fluidRow(
          column(12, strong("S8 Table -"), span("Model selection global model formulas"), align = "center")
        ),
        fluidRow(
          column(12, 
                 p("Global models as starting points for creating and evaluating model subsets.", align = "center", style = "margin-bottom: 20px; font-style: italic;"))
        ),
        br(),
        fluidRow(
          column(2, 
                 selectInput("dataSubset", "Select Data Subset:",
                             choices = c("General", "HIC", "LMIC", "Binomial"),
                             selected = "General")
          ),
          column(10, uiOutput("global_model_formulas"))
        )
      )
    )
  ),
  
  

  
  # Supporting results
  navbarMenu(
    "Supporting results",
    tabPanel(
      "World map of DPSE indicators",
      value = "choropleth-plots",

      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          br(),
          fluidRow(pickerInput("dpse", "Select DPSE indicators:", choices = c("DRIVERS", "USE", "RESISTANCE", "DRI"), selected = "DRIVERS", multiple = FALSE)),
          fluidRow(pickerInput("level", "Select data tier:", choices = c("Tier 1", "Tier 2", "Tier 3"), selected = "Tier 1", multiple = FALSE)),
          
          # Tier 2 Specific Conditional Panels
          conditionalPanel(
            condition = "input.level == 'Tier 2' && input.dpse == 'DRIVERS'",
            fluidRow(pickerInput("group", "Select Group:", choices = c("Infections", "Sanitation", "Vaccines", "Workforce")))
          ),
          conditionalPanel(
            condition = "input.level == 'Tier 2' && input.dpse == 'USE'",
            fluidRow(pickerInput("shortname", "Select Short Name:", choices = c("BroadPerTotalABXUse" = "Broad Spectrum vs. Total ABX Use", "NewABXUse" = "New ABX Use", "TotalDDDPer1000Persons" = "Total DDD per 1000 Persons")))
          ),
          conditionalPanel(
            condition = "input.level == 'Tier 2' && input.dpse == 'RESISTANCE'",
            fluidRow(pickerInput("shortname", "Select Short Name:", choices = c("CR" = "Carbapenem-resistant", "STR" = "Streptomycin-resistant", "MRSA" = "Methicillin-resistant Staphylococcus aureus")))
          ),
          
          # Tier 3 Specific Conditional Panels
          conditionalPanel(
            condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS'",
            fluidRow(pickerInput("group", "Select Group:", choices = c("Infections", "Sanitation", "Vaccines", "Workforce")))
            # fluidRow(pickerInput("shortname", "Select Short Name:", choices = NULL))  # Initially empty, to be filled based on "group" selection
          ),
          conditionalPanel(
            condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Infections'",
            fluidRow(pickerInput("shortname", "Select Short Name:", choices = c("HIV", "TB")))
          ),
          
          # The color scheme seems to work well. However, when input.level = Tier 3, and input&group != "Infection), the color scale for the 3 plots need to be reverse
          
          conditionalPanel(
            condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Sanitation'",
            fluidRow(pickerInput("shortname", "Select Short Name:", choices = c("Drinking Water Source", "Overall Sanitation", "Water Source Access")))
          ),
          conditionalPanel(
            condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Vaccines'",
            fluidRow(pickerInput("shortname", "Select Short Name:", choices = c("DTP3", "HepB3", "Hib3", "Measles", "PCV3", "Pol3", "RCV1")))
          ),
          conditionalPanel(
            condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Workforce'",
            fluidRow(pickerInput("shortname", "Select Short Name:", choices = c("Physicians", "Nursing & midwifery")))
          ),
          
          # Note added here
          tags$hr(),  # Adds a horizontal line for visual separation
          HTML("<strong>Note:</strong> For variable name reference, please check the <strong>S2 Table</strong>. <em>The maps may take few seconds to load, please be patient!</em>"),
          HTML("<em>For the 2000-2008 and 2008-2016 data, a red color indicates worse status, while blue signifies good status. For the difference between the two periods, green indicates improvement and purple indicates worsening conditions </em>.")
          
        ),
        mainPanel(
          highchartOutput(outputId = "map_x0008", height = "500px"),
          highchartOutput(outputId = "map_x0816", height = "500px"),
          highchartOutput(outputId = "map_change", height = "500px")
        )
      )
    ),
    
    tabPanel(
      "Changes of governance action between 2016 and 2023",
      value = "governance-changes",
      
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          br(),
          fluidRow(pickerInput("income_gov", "Select country income", choices = c("LMIC", "HIC"), selected = c("LMIC", "HIC"), multiple = TRUE)),
          fluidRow(pickerInput("trend", "Select governance trend:", choices = c("Increase", "Decrease"), selected = c("Increase", "Decrease"), multiple = TRUE)),
          # Note added here
          tags$hr(),  # Adds a horizontal line for visual separation
          HTML("<b> Figure S2. </b> Changes of governance action between 2016 and 2023. <em> Governance action scrore reported in Tracking AMR Country Self-Assessment Survey (TrACSS) <b style='color:#092044'>increased</b> in most countries, which can be interpreted as a sign of progress. Notable exception where goverance score <b style='color:#C33C2E'>decreased</b>, is the Netherlands. </em>")
          
          
        ),
        mainPanel(
          plotlyOutput("governance_changes", height = 1000, width = "100%"),
        )
      )
    )
  )
)

