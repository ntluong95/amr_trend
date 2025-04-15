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
  title = div(
    # style = "display: flex; flex-direction: column; align-items: flex-start; padding-left: 30px;",
    class = "d-flex align-items-center",
    img(
      src = "logo-gedb.png",
      width = "380px",
      style = "padding-left: 30px;",
      height = "77.5px"
    ),
    div(
      "AMR Trend Explorer",
      style = "
      /*css*/
      font-size: 1.2rem; 
      font-weight: 600; 
      margin-right: 10px;
      margin-left: 20px;
      color: #fff !important;
      /*!css*/
      "
    ),
  ),
  #REVIEW Work with R 4.3.2 or need some packages to make it work
  navbar_options = list(class = "bg-primary", theme = "light"),
  fluid = TRUE,
  id = "navbar",
  #REVIEW file _brand.yml define the theme
  #fmt: skip
  theme = bs_theme(version = 5, bootswatch = "zephyr", fontawesome = TRUE) %>%
    bslib::bs_add_rules(
      rules = "
      /*css*/
      .navbar.navbar-default {
        background-color: $primary !important; 
        color: #secondary !important;
      }

      .navbar .navbar-brand {
        display: flex;
        align-items: center;
      }

      .navbar-nav .nav-link {
        color: #d1dddf !important; 
      }

      .navbar-nav .nav-link.active,
      .navbar-nav .nav-link:hover {
        color: white !important;
      }
     
      .bslib-gap-spacing {
          gap: 12px !important; 
      }
      /*!css*/
      "),

  #TODO Home
  nav_panel(
    title = "Home",
    layout_columns(
      col_widths = c(3, 5, 4),
      # --- LEFT COLUMN ---
      div(
        card(
          card_header(
            "Article Information",
            class = "bg-primary text-white",
            style = "white-space: normal; overflow-x: hidden;"
          ),
          card_body(
            #fmt: skip
            # em("This is an interactive web application to explore the results for the article:"),
            div(
              em("This shinyApp provides interactive visualizations of the results from the paper: "),
              br(),
              br(),
              strong(
                "Association between national action and trends in antibiotic resistance: an analysis of 73 countries from 2000 to 2023",
                style = "font-size: 17px;"
              ),
              a(
                href = "https://journals.plos.org/globalpublichealth/",
                target = "_blank",
                style = "margin-left: 8px;",
                icon("external-link-alt", lib = "font-awesome")
              ),
              a(
                href = "mailto:peter.sogaard.jorgensen@su.se",
                style = "margin-left: 8px;",
                icon("envelope", lib = "font-awesome")
              )
            ),

            "Peter Søgaard Jørgensen,",
            "Luong Nguyen Thanh,",
            "Ege Pehlivanoğlu,",
            "Franziska Klein,",
            "Didier Wernli,",
            "Dusan Jasovsky,",
            "Athena Aktipis,",
            "Rob R. Dunn,",
            "Yrjö Gröhn,",
            "Guillaume Lhermie,",
            "H. Morgan Scott,",
            "Eili Y. Klein,",

            accordion(
              id = "guide-accordion",
              open = FALSE,
              accordion_panel(
                title = "Instructions",
                HTML(
                  "
              <!--html-->
              <span style='color:#C33C2E; font-style:italic;'>Some results may take few seconds to load, please be patient</span>   
              <br><br>
              The results are interactive, which means that you can hover over the data points, or click on the legend, to see more details. For variable name reference, please check the <strong>S1-4 Table</strong>. 
              <br><br>
              For the 2000-2008 and 2008-2016 data, a <span style='color:#C33C2E; font-weight:bold;'>red</span> color indicates worse status, while <span style='color:blue; font-weight:bold;'>blue</span> signifies good status. For the difference between the two periods, <span style='color:green; font-weight:bold;'>green</span> indicates improvement and <span style='color:purple; font-weight:bold;'>purple</span> indicates worsening conditions.
              <br><br>    
              <em>Governance action score reported in Tracking AMR Country Self-Assessment Survey (TrACSS) 
              <b style='color:#092044'>increased</b> in most countries, which can be interpreted as a sign of progress. However, some countries showed <b style='color:#F0C94C'>unchange</b> or even <b style='color:#C33C2E'>decreased</b> in governance action score.</em>
              <!--!html-->
              "
                )
              )
            ),

            accordion(
              id = "toc-accordion",
              open = FALSE,
              accordion_panel(
                title = "Table of Contents",
                tags$ul(
                  tags$li(tags$strong("Annex 1. Main results")),
                  tags$ul(
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'main-fig2')",
                      "Figure 2. Association between stated action and linear trend, and categorial trend"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'main-fig3')",
                      "Figure 3. De-escalation of DPSE. De-escalation ratios for Drivers, Use, Resistance, and DRI"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'main-fig4')",
                      "Figure 4. Most important variables in model selection"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'main-fig5')",
                      "Figure 5. Actions levels in animal protein produced countries for animal and human related Antibitotic Indicators"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'main-fig6')",
                      "Figure 6. Classification of country ABR governance syndrome"
                    ))
                  ),

                  tags$li(tags$strong("Annex 2. Methodology")),
                  tags$ul(
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'dpsea-description')",
                      "S1 Text - Description of DPSEA indicators"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'indicator-used')",
                      "S1 Table - Indicator selection for Driver categories"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'indicator-used')",
                      "S2 Table - Indicator selection for Use and Resistance categories"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'indicator-used')",
                      "S3 Table - Indicator selection for DRI (exposure) categories"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'indicator-used')",
                      "S4 Table - Ecological variables used as covariates"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'country-selection')",
                      "S5 Table - List of countries included in the study"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'governance-syndrome')",
                      "S6 Table - Governance Syndrome questions"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'action-index')",
                      "S7 Table - Questions used for calculating the action index"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'model-formulas')",
                      "S8 Table - Model Formulas for Association between Action and Indicator change and sign of change"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'de-escalation-plots')",
                      "S9 Table - De-escalation plot formulas for univariate models"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'global-models-data')",
                      "S10 Table - Global Models Data Subset Formulas for The Model Selection"
                    )),
                    tags$li(tags$a(
                      href = "#",
                      onclick = "Shiny.setInputValue('navTo', 'global-models-formulas')",
                      "S11 Table - Global Model Formulas for The Model Selection"
                    ))
                  )
                )
              )
            )
          )
        ),
        #REVIEW
        card(
          card_header(
            "Indicator Explorer",
            class = "bg-primary text-white",
            style = "white-space: normal; overflow-x: hidden;"
          ),
          card_body(
            pickerInput(
              "dpse",
              "Select DPSE indicators:",
              choices = c("DRIVERS", "USE", "RESISTANCE", "DRI"),
              selected = "DRIVERS",
              multiple = FALSE
            ),
            pickerInput(
              "level",
              "Select data tier:",
              choices = c("Tier 1", "Tier 2", "Tier 3"),
              selected = "Tier 1",
              multiple = FALSE
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 2' && input.dpse == 'DRIVERS'",
              pickerInput(
                "group",
                "Select Group:",
                choices = c("Infections", "Sanitation", "Vaccines", "Workforce")
              )
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 2' && input.dpse == 'USE'",
              pickerInput(
                "shortname",
                "Select Short Name:",
                choices = c(
                  "BroadPerTotalABXUse" = "Broad Spectrum vs. Total ABX Use",
                  "NewABXUse" = "New ABX Use",
                  "TotalDDDPer1000Persons" = "Total DDD per 1000 Persons"
                )
              )
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 2' && input.dpse == 'RESISTANCE'",
              pickerInput(
                "shortname",
                "Select Short Name:",
                choices = c(
                  "CR" = "Carbapenem-resistant",
                  "STR" = "Streptomycin-resistant",
                  "MRSA" = "Methicillin-resistant Staphylococcus aureus"
                )
              )
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS'",
              pickerInput(
                "group",
                "Select Group:",
                choices = c("Infections", "Sanitation", "Vaccines", "Workforce")
              )
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Infections'",
              pickerInput(
                "shortname",
                "Select Short Name:",
                choices = c("HIV", "TB")
              )
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Sanitation'",
              pickerInput(
                "shortname",
                "Select Short Name:",
                choices = c(
                  "Drinking Water Source",
                  "Overall Sanitation",
                  "Water Source Access"
                )
              )
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Vaccines'",
              pickerInput(
                "shortname",
                "Select Short Name:",
                choices = c(
                  "DTP3",
                  "HepB3",
                  "Hib3",
                  "Measles",
                  "PCV3",
                  "Pol3",
                  "RCV1"
                )
              )
            ),
            conditionalPanel(
              condition = "input.level == 'Tier 3' && input.dpse == 'DRIVERS' && input.group == 'Workforce'",
              pickerInput(
                "shortname",
                "Select Short Name:",
                choices = c("Physicians", "Nursing & midwifery")
              )
            ),

            tags$hr(),
            #fmt: skip
            pickerInput(
              inputId = "income_gov",
              label = "Select country income:",
              choices = c("LMIC", "HIC"),
              selected = c("LMIC", "HIC"),
              multiple = TRUE
            ),
            pickerInput(
              inputId = "trend",
              label = "Select governance trend:",
              choices = c("Increase", "Decrease"),
              selected = c("Increase", "Decrease"),
              multiple = TRUE
            ),
          )
        )
      ),
      #TODO
      card(
        style = "height: auto; min-height: 1200px;",
        card_header(
          "Changes of DPSE indicator between 2000 and 2016",
          class = "bg-primary text-white",
          style = "white-space: normal; overflow-x: hidden;"
        ),
        card_body(
          # Card body content remains the same
          div(
            style = "min-height: 400px; height: 50vh;",
            highchartOutput(outputId = "map_x0008", height = "100%")
          ),
          br(),
          div(
            style = "min-height: 400px; height: 50vh;",
            highchartOutput(outputId = "map_x0816", height = "100%")
          ),
          br(),
          div(
            style = "min-height: 400px; height: 50vh;",
            highchartOutput(outputId = "map_change", height = "100%")
          )
        )
      ),
      div(
        card(
          style = "height: auto; min-height: 1200px;",
          card_header(
            "Changes of governance action between 2016 and 2023",
            class = "bg-primary text-white",
            style = "white-space: normal; overflow-x: hidden;"
          ),
          card_body(plotlyOutput(
            "governance_changes",
            height = 1000,
            width = "100%"
          ))
        )
      )
    )
  ),

  #TODO Main results,
  navbarMenu(
    title = "Annex 1. Main results",
    tabPanel(
      "Association between stated action and DPSE. indicators",
      value = "main-fig2",
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput(
              "fig2_income",
              "Select country income:",
              choices = c("LMIC", "HIC"),
              selected = c("LMIC", "HIC"),
              multiple = TRUE
            ),
            pickerInput(
              "fig2_outcome",
              "Select outcome type:",
              choices = c("Linear Trend", "Categorical Trend"),
              selected = c("Linear Trend", "Categorical Trend"),
              multiple = TRUE
            )
          ),
          fluidRow(
            # Plain static text description
            tags$p(HTML(
              "<strong>Figure 2. Association between stated action and linear trend (indicator change A-D), and sign of change (categorical trend E-H).</strong> <em>Indicator p-values are from linear mixed models with country income group as random effect. For detailed indicators, please see S8 Table. Blue represents drivers of antibiotics resistance, purple represents antibiotics use, orange represents resistance, and red represents DRI.</em>"
            ))
          )
        ),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("adaptive_plot", height = 800, width = "100%"),
        )
      )
    ),

    tabPanel(
      "De-escalation ratios of DPSE.",
      value = "main-fig3",
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            # Plain static text description
            tags$p(HTML(
              "<strong>Figure 3. De-escalation ratios of DPSE indicators for Drivers, Use, Resistance, and DRI.</strong> <em>De-escalating ratio is defined as the proportion of available lower-level indicators within a category that have witnessed a reduction from 2000 to 2016. Weight refers to the proportion of variables avaiable within a category. Uncertainty bands indicate standard errors.</em>"
            ))
          )
        ),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("fig3_plot", height = 800, width = "100%"),
        )
      )
    ),

    tabPanel(
      "Most important variables in model selection",
      value = "main-fig4",
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput(
              "fig4_outcome",
              "Select outcome type:",
              choices = c("Linear Trend", "Categorical Trend"),
              selected = c("Linear Trend", "Categorical Trend"),
              multiple = TRUE
            )
          ),
          fluidRow(
            # Plain static text description
            tags$p(HTML(
              "<strong>Figure 4. Most important variables in model selection.</strong> <em>The rank of the five most important variables (rows) is shown using color coding. Each column represents a unique model selection procedure on the linear trend (change, 17 procedures) or the categorical trend (binomial, 16 procedures). Model names refers to the subset of the dataset with certain DPSE indicators including D (Driver), P (Pressure), S (State), E (Exposure). noDr indicates exclusion of health system variables as explanatory variables. aX refers to analysis of DPSE for country subsets with X variable available. See S10-S11 Tables for details on each model selection procedure.</em>"
            ))
          )
        ),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotOutput("fig4_plot", height = 800, width = "100%"),
        )
      )
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
            tags$p(HTML(
              "<strong>Figure 5. Actions levels in animal protein produced countries for animal and human related Antibitotic Indicators.</strong> <em>Action level of governments belong to countries with large producers of animal protein. Stated government action animal and human health scores in relation to total production of vertebrate biomass (mammals, birds and fish) for high-income (HICs) and low- and middle-income countries (LMICs).</em>"
            ))
          )
        ),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("fig5_plot", height = 800, width = "100%"),
        )
      )
    ),

    tabPanel(
      "Classification of country ABR governance syndrome",
      value = "main-fig6",
      sidebarLayout(
        sidebarPanel(
          fluidRow(h3(strong("Filters"))),
          fluidRow(
            pickerInput(
              "fig6_income",
              "Select country income:",
              choices = c("LMIC", "HIC"),
              selected = c("LMIC", "HIC"),
              multiple = TRUE
            ),
            pickerInput(
              "fig6_outcome",
              "Select outcome type:",
              choices = c("Linear Trend", "Categorical Trend"),
              selected = c("Linear Trend", "Categorical Trend"),
              multiple = TRUE
            ),
            pickerInput(
              "fig6_indicators",
              "Select indicators type:",
              choices = c("DRIVERS", "USE", "RESISTANCE", "DRI"),
              selected = c("DRIVERS", "USE", "RESISTANCE", "DRI"),
              multiple = TRUE
            )
          ),
          fluidRow(
            # Plain static text description
            tags$p(HTML(
              "<strong>Figure 6. Classification of country ABR governance syndrome.</strong> <em>(A) categorize country trajectory based on trend in DPSE indicators and governmental action. Countries in vicious cycle are displayed with name ISO3 code (see S5 Table). (B) Comparison of country governance syndrome according to DPSE indicators and income level.</em>"
            ))
          ),
          width = 3 # Customize sidebar width (default is 4)
        ),
        mainPanel(
          # plotOutput("adaptive_plot", width = "100%", height = "800px"),
          plotlyOutput("governance_syndrome1", height = 800, width = "100%")
        )
      )
    )
  ),

  #TODO Methodology
  navbarMenu(
    "Annex 2. Methodology",
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
          column(
            12,
            strong("Drivers – health system"),
            span(
              "Driving forces behind human antibiotic use is captured by analyzing the trends in time series data for fifteen variables across four tier 2 indicators including infection prevalence (primary driver), sanitation standards, vaccination coverage, and health care workforce. Total data for drivers are compiled for a total of 219 countries from the United Nations (UN), World Bank database and the World Health Organization (WHO). Data availability varies by indicator as detailed in S1 Table."
            )
          )
        ),
        br(),

        fluidRow(
          column(
            12,
            strong("Pressure - antibiotic use (ABU)"),
            span(
              "Data from IQVIA database [1] are estimates of the total volume of sales of each antibiotic molecule (or combination of molecules) based on national sample surveys of antibiotic sales. Antibiotic consumption data are in kilograms and converted into defined daily doses (DDDs) using the Anatomical Therapeutic Chemical Classification System (ATC/DDD, 2016) developed by the WHO Collaborating Centre for Drug Statistics Methodology as in Klein et al. [2]. Newly Available Antibiotic Use defined as antibiotics first introduced in 1999 or later [2]."
            )
          )
        ),
        br(),

        fluidRow(
          column(
            12,
            strong("State - resistance"),
            span(
              "Data obtained from ResistanceMap [3] which is a repository of global antimicrobial resistance data. ResistanceMap obtains data from public and private sources, including lab networks, hospitals, and government agencies. Data include resistance rates for eight high-priority pathogens isolated from blood and cerebrospinal fluid of patients and are aggregated at the country level on an annual basis. Data on ResistanceMap has been harmonized to present similar definitions of resistance across countries and regions to enable comparisons between countries."
            )
          )
        ),
        br(),

        fluidRow(
          column(
            12,
            strong("Exposure – Drug resistance index"),
            span(
              "The Drug Resistance Index (DRI) combines use and resistance rates into a single value that provides measures of antibiotic effectiveness relative to their use [4]. While DRI has been critiqued when used as a single indicator of antibiotic effectiveness [5], we here use it as part of a multi-indicator framework. We calculated the adaptive Drug Resistance Index for countries for which data on resistance and use is available over the time period following the methodology outlined in [4] and [6]. Briefly, the annual DRI was estimated for each country by the following equation:"
            )
          )
        ),

        withMathJax(),
        helpText('$$DRI = \\sum_k \\rho_k^{i,t} q_k^{i,t}$$'),
        fluidRow(
          column(
            12,
            span(
              "where, for country i at time t, \\(\\rho_k^{i,t}\\) is the proportion of resistance among all included organisms to drug k and \\(\\ q_k^{i,t}\\) is the proportion of drug k used for their treatment in all drugs included in the index. Pathogens included in the analysis were E. coli, K. pneumoniae, P. aeruginosa, S. aureus, E. faecium, and E. faecalis. Antibiotics included in the analysis were aminoglycosides, broad-spectrum penicillin, carbapenems, cephalosporins, narrow-spectrum penicillin, and quinolones. Because not all countries had data for all combinations, we included a country if they had at least four of the six organisms, and 10 of the 17 total combinations possible (S3 Table)."
            )
          )
        ),
        br(),

        fluidRow(
          column(
            12,
            strong("Action – TrACSS"),
            span(
              "All action indicators are self-reported data from the Global Database for Tracking Antimicrobial Resistance Country Self-Assessment Survey (TrACSS) spanning the period of 2016-2023 (https://amrcountryprogress.org/). The survey responses are publicly available with the yearly updated version providing information about countries ongoing actions to live up to the global action plan on antimicrobial resistance. All answers are given on an ordinal scale from A to E (0-4)."
            )
          )
        ),
        br(),

        fluidRow(
          column(12, strong("REFERENCE"))
        ),
        br(),
        fluidRow(
          column(
            12,
            span(
              "1. OneHealthTrust. ResistanceMap: Antibiotic Use. 2024 [cited 13 Sep 2024]. Available: https://resistancemap.onehealthtrust.org/AntibioticUse.php"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            span(
              "2. Klein EY, Milkowska-Shibata M, Tseng KK, Sharland M, Gandra S, Pulcini C, et al. Assessment of WHO antibiotic consumption and access targets in 76 countries, 2000–15: an analysis of pharmaceutical sales data. The Lancet Infectious Diseases. 2021;21: 107–115. doi:10.1016/S1473-3099(20)30332-7"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            span(
              "3. OneHealthTrust. ResistanceMap. [cited 1 Sep 2017]. Available: https://resistancemap.onehealthtrust.org/"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            span(
              "4. Laxminarayan R, Klugman KP. Communicating trends in resistance using a drug resistance index. BMJ Open. 2011;1: e000135–e000135. doi:10.1136/bmjopen-2011-000135"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            span(
              "5. Vandenbroucke-Grauls CMJE, Kahlmeter G, Kluytmans J, Kluytmans-Van Den Bergh M, Monnet DL, Simonsen GS, et al. The proposed Drug Resistance Index (DRI) is not a good measure of antibiotic effectiveness in relation to drug resistance. BMJ Global Health. 2019;4: 1–3. doi:10.1136/bmjgh-2019-001838"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            12,
            span(
              "6. Pant S, Klein E, Gandra S, Laxminarayan R. Tracking Antibiotic Effectiveness Worldwide 1999–2014 Using the Drug Resistance Index. Open Forum Infectious Diseases. 2016;3: 1481. doi:10.1093/ofid/ofw172.1183"
            )
          )
        )
      )
    ),
    tabPanel(
      "Country selection",
      value = "country-selection",
      fluidPage(
        br(),
        fluidRow(
          column(
            12,
            strong("S5 Table -"),
            span("List of countries included in the study"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "The total number of countries included in the study is 73. ISO3 codes refers to three-letter country codes according to ISO 3166-1.",
              align = "center",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        ),
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
          column(
            12,
            strong("S1 Table -"),
            span("Indicator selection for Driver categories"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Driver (as the first DPSE: Driver, Pressure, State, and Exposure indicator) data was available for 148 countries. However, countries which had driver data only included if they had one of the other indicator categories (Use, Resistance, and DRI), hence only 73 countries included in drivers (data is standardised SD=1 by denoting the ratio of standard deviation to mean). Driver data included 3 tiers. Average of tier 3 indicators are taken from different data sources.",
              style = "margin-bottom: 20px; font-style: italic;"
            ),
            align = "center"
          )
        ),
        # br(),
        # fluidRow(
        #   DTOutput("indicators_table")
        # ),
        tags$h4("DRIVERS: FACTORS INFLUENCING ANTIBIOTIC USE"),
        DTOutput("drivers_table"),

        br(),
        fluidRow(
          column(
            12,
            strong("S2 Table -"),
            span("Indicator selection for Use and Resistance categories"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Use or resistance data were available for 73 countries with antibiotic usage (ABU), and antibiotic resistance (ABR) available for 65 and 32 countries, respectively. Data is standardised to the ratio of standard deviation (SD=1). For NewABXUse, year indicates year of introduction of antibiotic.",
              style = "margin-bottom: 20px; font-style: italic;"
            ),
            align = "center"
          )
        ),
        br(),
        tags$h4("USE: PRESSURES FOR RESISTANCE TO SPREAD"),
        DTOutput("use_table"),

        tags$h4("RESISTANCE: CURRENT STATE OF ANTIBIOTIC RESISTANCE"),
        DTOutput("resistance_table"),

        br(),
        fluidRow(
          column(
            12,
            strong("S3 Table -"),
            span("Indicator selection for DRI (exposure) category"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "The drug resistance index (DRI) was calculated for 25 countries for the pathogen and antibiotics combinations listed below. Data is standardized by the standard deviation (SD=1).",
              style = "margin-bottom: 20px; font-style: italic;"
            ),
            align = "center"
          )
        ),

        tags$h4(
          "DRUG RESISTANCE INDEX (DRI): PATHOGEN-ANTIBIOTIC COMBINATIONS"
        ),
        DTOutput("dri_table"),

        br(),
        fluidRow(
          column(
            12,
            strong("S4 Table -"),
            span("Ecological variables used as covariates"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Ecological variables are the non-related variables to DPSE indicators and referred as covariates in the analyses. Covariates are grouped according to their context.",
              style = "margin-bottom: 20px; font-style: italic;"
            ),
            align = "center"
          )
        ),

        tags$h4("ECOLOGICAL VARIABLES"),
        DTOutput("ecological_variables_table")
        # br(),
        # fluidRow(
        #   column(12, strong("REFERENCE"))
        # )
        # br(),
        # fluidRow(
        #   column(12, span("1. Klein EY, Milkowska-Shibata M, Tseng KK, Sharland M, Gandra S, Pulcini C, et al. Assessment of WHO antibiotic consumption and access targets in 76 countries, 2000–15: an analysis of pharmaceutical sales data. The Lancet Infectious Diseases. 2021;21: 107–115. doi:10.1016/S1473-3099(20)30332-7"))),
      )
    ),
    tabPanel(
      "Governance syndrome questions",
      value = "governance-syndrome",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(
            12,
            strong("S6 Table -"),
            span("Governance Syndrome questions"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Questions used and their sectors included for the calculation of governance syndrome for the years between 2016-2022 according to TrACSS (1.1) 2016-2017 version. All surveys are available online (https://amrcountryprogress.org/#/download-responses). Only questions asked in all surveys were selected for the analysis. In case a question is separated in two questions, they were still kept for the analysis. Answers converted from the scale of A-E to 0-4. Categories’ scores were averaged from their constituted questions, and the overall governance score was calculated as the mean score of all categories.  NAP stands for National Action Plan",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        ),
        br(),
        fluidRow(
          DTOutput("governance_syndrome")
        )
      )
    ),

    tabPanel(
      "Action index questions",
      value = "action-index",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(
            12,
            strong("S7 Table -"),
            span("Questions used for calculating the action index"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Topics and titles of questions from the TrACSS (1.1) survey in 2016-17 used to calculate the action index as well as the sub-categories used to group questions within a similar theme. Answers were answered on a scale of A-E and converted to 0-4.",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        ),
        br(),
        fluidRow(
          DTOutput("action_index_table")
        ),
        fluidRow(
          column(
            12,
            p(
              "*Subcategories included in Animal Data Analysis and Figure 4",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        )
      )
    ),

    tabPanel(
      "Model formulas",
      value = "model-formulas",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(
            12,
            strong("S8 Table -"),
            span(
              "Model Formulas for Association between Action and Indicator Change and Categorical Trend"
            ),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Formulas for generalized linear mixed models to investigate the association between linear trend of indicators in 16 years and action. First type of model included linear trend of indicators as response variable. The second type of model included Action as a response variable and categorical trend as an explanatory variable to investigate the action difference between the countries with positive vs negative change. Results shown in Figure 2 for tier 1 indicators.",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        ),
        br(),
        fluidRow(
          DTOutput("model_formulas")
        )
      )
    ),
    tabPanel(
      "De-escalation plot formulas",
      value = "de-escalation-plots",
      # Defining the layout
      fluidPage(
        br(),
        fluidRow(
          column(
            12,
            strong("S9 Table -"),
            span("De-escalation plot formulas for univariate models"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Formulas used for binomial general linear model to investigate the association between de-escalation of categories and action level. Declining proportion refers to declining numbers of tier 2 indicators divided by the total number of tier 2 indicators for countries, Baseline Mean refers to mean of baseline for tier 2 indicators for each DPSE indicator (See S2 Table). Models weighted by total number of tier 2 indicators within each DPSE indicator for specific country.",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        ),
        br(),
        fluidRow(
          DTOutput("de_escalation_plots")
        )
      )
    ),
    tabPanel(
      "Multivariate model selection",
      value = "global-models-data",
      fluidPage(
        br(),
        fluidRow(
          column(
            12,
            strong("S10 Table -"),
            span("Global Models Data Subset Formulas for The Model Selection"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Global Models for multivariate model selection data subsets.",
              align = "center",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        ),
        br(),
        fluidRow(
          DTOutput("global_models_data")
        ),
        br(),
        br(),
        fluidRow(
          column(
            12,
            strong("S11 Table -"),
            span("Model selection global model formulas"),
            align = "center"
          )
        ),
        fluidRow(
          column(
            12,
            p(
              "Global models as starting points for creating and evaluating model subsets.",
              align = "center",
              style = "margin-bottom: 20px; font-style: italic;"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            2,
            selectInput(
              "dataSubset",
              "Select Data Subset:",
              choices = c("General", "HIC", "LMIC", "Binomial"),
              selected = "General"
            )
          ),
          column(10, uiOutput("global_model_formulas"))
        )
      )
    )
  )
)
