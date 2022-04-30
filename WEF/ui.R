#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rsconnect)
library(shinythemes)
library(bslib)
library(shiny)
library(tidyverse)
library(readr)
library(shinydashboard)
library(shinythemes)
#library(gapminder)
#library(here)
library(janitor)
#library(leaflet)
#library(rgdal)
library(plotly)
library(shinyWidgets)

#List of Choices and static variables ----

listOfCountries <- c("Albania "="Albania",
                     "Armenia"="Armenia",
                     "Azerbaijan"="Azerbaijan",
                     "Belarus"="Belarus",
                     "Bosnia & Herzegovina"="Bosnia & Herzegovina",
                     "Bulgaria"="Bulgaria",
                     "Croatia"="Croatia",
                     "Czech Republic"="Czech Republic",
                     "Estonia"="Estonia",
                     "Georgia"="Georgia",
                     "Hungary"="Hungary",
                     "Kazakhstan"="Kazakhstan",
                     "Kyrgz Republic"="Kyrgz Republic",
                     "Latvia"="Latvia",
                     "Lithuania"="Lithuania",
                     "Moldova"="Moldova",
                     "Montenegro"="Montenegro",
                     "North Macedonia"="North Macedonia",
                     "Poland"="Poland",
                     "Romania"="Romania",
                     "Russian Federation"="Russian Federation",
                     "Serbia"="Serbia",
                     "Slovak Republic"="Slovak Republic",
                     "Slovenia"="Slovenia",
                     "Tajikistan"="Tajikistan",
                     "Ukraine"="Ukraine",
                     "Israel"="Israel",
                     "UAE"="UAE",
                     "Tunisia"="Tunisia",
                     "Egypt"="Egypt",
                     "Jordan"="Jordan",
                     "Lebanon"="Lebanon",
                     "Turkey"="Turkey",
                     "Algeria"="Algeria",
                     "Bahrain"="Bahrain",
                     "Niger"="Niger",
                     "Qatar"="Qatar",
                     "Kuwait"="Kuwait",
                     "Morocco "="Morocco",
                     "Oman"="Oman",
                     "Mauritania"="Mauritania",
                     "Saudi Arabia"="Saudi Arabia",
                     "Iran, Islamic Rep"="Iran, Islamic Rep",
                     "Syria"="Syria",
                     "Iraq"="Iraq",
                     "Yemen"="Yemen",
                     "New Zealand "="New Zealand",
                     "Philippines "="Philippines",
                     "Lao PDR "="Lao PDR",
                     "Australia "="Australia",
                     "Singapore"="Singapore",
                     "Timor-leste "="Timor-leste",
                     "Mongolia "="Mongolia",
                     "Thailand "="Thailand",
                     "Viet Nam "="Viet Nam",
                     "Indonesia "="Indonesia",
                     "Korea "="Korea",
                     "Cambodia "="Cambodia",
                     "China "="China",
                     "Myanmar "="Myanmar ",
                     "Brunei Darussalam "="Brunei Darussalam",
                     "Malaysia "="Malaysia",
                     "Fiji "="Fiji",
                     "Japan "="Japan",
                     "Papua New Guinea "="Papua New Guinea",
                     "Vanuatu "="Vanuatu",
                     "Argentina"="Argentina",
                     "Bahamas"="Bahamas",
                     "Barbados"="Barbados",
                     "Belize"="Belize",
                     "Bolivia"="Bolivia",
                     "Brazil"="Brazil",
                     "Cape Verde"="Cape Verde",
                     "Chile"="Chile",
                     "Colombia"="Colombia",
                     "Costa Rica"="Costa Rica",
                     "Cuba"="Cuba",
                     "Dominican Republic"="Dominican Republic",
                     "Ecuador"="Ecuador",
                     "El Salvador"="El Salvador",
                     "Guatemala"="Guatemala",
                     "Guyana"="Guyana",
                     "Honduras"="Honduras",
                     "Jamaica"="Jamaica",
                     "Mexico"="Mexico",
                     "Nicaragua"="Nicaragua",
                     "Panama"="Panama",
                     "Paraguay"="Paraguay",
                     "Peru"="Peru",
                     "Suriname"="Suriname",
                     "Trinidad and Tobago"="Trinidad and Tobago",
                     "Uruguay "="Uruguay ",
                     "Venezuela"="Venezuela",
                     "Canada"="Canada",
                     "USA"="USA",
                     "Afghanistan"="Afghanistan",
                     "Bangladesh "="Bangladesh",
                     "Bhutan "="Bhutan",
                     "India "="India",
                     "Maldives "="Maldives",
                     "Nepal "="Nepal",
                     "Pakistan "="Pakistan",
                     "Sri Lanka "="Sri Lanka",
                     "Namibia"="Namibia",
                     "Rwanda"="Rwanda",
                     "South Africa"="South Africa",
                     "Burundi"="Burundi",
                     "Mozambique"="Mozambique",
                     "Zimbabwe"="Zimbabwe",
                     "Eswatini"="Eswatini",
                     "Zambia"="Zambia",
                     "Madagascar"="Madagascar",
                     "Uganda"="Uganda",
                     "Cape Verde"="Cape Verde",
                     "Botswana"="Botswana",
                     "Tanzania"="Tanzania",
                     "Lesotho"="Lesotho",
                     "Liberia"="Liberia",
                     "Kenya"="Kenya",
                     "Cameroon"="Cameroon",
                     "Ethiopia"="Ethiopia",
                     "Senegal"="Senegal",
                     "Togo"="Togo",
                     "Mauritius"="Mauritius",
                     "Malawi"="Malawi",
                     "Ghana"="Ghana",
                     "Guinea"="Guinea",
                     "Angola"="Angola",
                     "Sierra Leone"="Sierra Leone",
                     "Benin"="Benin",
                     "Burkina Faso"="Burkina Faso",
                     "Gambia, The"="Gambia, The",
                     "Côte d'Ivoire"="Côte d'Ivoire",
                     "Niger"="Niger",
                     "Nigeria"="Nigeria",
                     "Chad"="Chad",
                     "Mali"="Mali",
                     "Congo, Democratic Rep."="Congo, Democratic Rep.",
                     "Austria"="Austria",
                     "Belgium"="Belgium",
                     "Cyprus"="Cyprus",
                     "Denmark"="Denmark",
                     "Finland"="Finland",
                     "France"="France",
                     "Germany"="Germany",
                     "Greece"="Greece",
                     "Iceland"="Iceland",
                     "Ireland"="Ireland",
                     "Italy"="Italy",
                     "Luxembourg"="Luxembourg",
                     "Malta"="Malta",
                     "Netherlands"="Netherlands",
                     "Norway"="Norway",
                     "Portugal"="Portugal",
                     "Spain"="Spain",
                     "Sweden"="Sweden",
                     "Switzerland"="Switzerland",
                     "United Kingdom"="United Kingdom")

# Introduction and About Panel ----
intro_panel <- tabPanel(
 # titlePanel("Introduction and Guide:"),
  "Introduction and Guide",
  tags$h4("What is This?!"),
  "This is a Shiny Dashboard that contains data about countries and metrics surrounding the gender gap worldwide. Anyone can interact with,explore and visualize findings without any coding or typing!",
  tags$br(), tags$br(),
  tags$h4("Data:"),
  tags$h5("Source:"),
  strong("This data originates from the Global Gender Gap Report PDF published on March 2021 by the World Economic Forum."),
  "You can find the original report ",tags$a(href="https://www.weforum.org/reports/global-gender-gap-report-2021", "here."),
  p("The data was manually collected from this PDF by a previous Smithie taking SWG238: Women, Money and Transnational Movements. This dashboard was created with the data collected previously as a final project for the same course."),
  tags$h5("What is in this data?"),
  strong("This dashboard does not contain all of the metrics described on the Global Gender Gap Report but 13 metrics which had to do with SWG238."),
  p("These metrics were chosen by the previous Smithies and were categorized into 5 groups shown below. We are using data from 157 different countries but not all countries had data for all the metrics."),
  strong("Economic Participation and Opportunity"),
  p("- Labor force participation ratio\n- Labor force participation (female %)\n - Estimated Earned Income Ratio"),
  strong("Work Participation and Leadership"),
  p("- Proportion of unpaid work per day, female/male ratio\n- Share of workers in informal sector (female)"),
  strong("Access to Finance"),
  p("- Women’s access to land use, control & ownership, 0-1 (worst)"),
  strong("Family and Care"),
  p("- Unmet Demand for Family Planning, % women 15-49\n - Public Childcare Expenditure, % GDP - Right to Equal Justice, 0-1 (worst)"),
  strong("Region Specific Data"),
  p("- Wage Equality (0-1best)\n- Maternal mortality, deaths per 100,000 live births\n- Women in Ministerial Positions\n- Women in Parliament"),
  tags$h4("What can you do with this dashboard:"),
  strong(tags$h5("Features in all visualizations:")),
  p("For every visualization, you can hover over the bar to get information about what country it represents, the value being displayed, what region that country is a part on and the page number this data was found on."),
  p("For every visualization there are filtering select boxes or check boxes in order for you to customize the view of your data to the metric, country or region you desire."),
  strong(tags$h5("Different Forms of Visualizations:")),
  p("In the navigation bar you can find different tabs and they each visualize the information differently"),
  strong(p("Explore by Metric:")),
  p("Explore the distribution of the metrics value per country by choosing it on the selection bar. You can see all regions together or also filter by certain region. "),
  strong(p("Explore by Individual Country:")),
  p("Take a look at an individual country's metric values by picking the country you desire."),
  strong(p("Explore by Decided Topics:")),
  p("The metrics were divided into 5 groups described above. Explore those 5 groups metrics with bar graphs of their value per country. You can also decide to filter by region."),
  strong(p("Pick and Choose Country Comparison:")),
  p("My favorite visualization lets you pick and choose any of the countries you desire by each metric."),
  p(""),
  tags$br(),
  tags$h4("Do You Want To Contribute?"),
  "Code and data used to generate this Shiny app are available on ",tags$a(href="https://github.com/Elaineyex/world_health_shiny", "Github."),
  "If you are not a coder please give your suggestions or found inconsistencies in this google form: ",tags$a(href="https://forms.gle/JPNABp6RjEdEsghK7", "Google Form."),
  tags$br(), tags$br(),
  tags$h4("Webapp"),
  "The Shiny app is published ",tags$a(href="https://casey-mae-perez.shinyapps.io/2020-World-Health-Statistics/", "online."),
  tags$br(), tags$br(),
  tags$h4("Original Creator of Dashboard:"),
  "Adriana Beltran Andrade"
)

#Individual Topic/Predictor Panel ----
predictor_panel <- tabPanel(
  "Explore By Metric",
  titlePanel("Bar Chart of Countries for a Given Metric"),
  fluidPage(
    # Creating Drop down with different variables
    sidebarLayout(
    sidebarPanel(width = 12,
      fluidRow(
      column(width = 5,
    selectInput(inputId ="variablesAvailable",  # this ID will help us communicate between UI and server
                label = "Choose Metric",
                choices = c("Labor Force Participation Ratio" = "labor_force_participation_ratio",
                            "Labor Force Participation Female in %" = "labor_force_participation_female_percent",
                            "Estimated Earned Income Ratio"= "estimated_earned_income_ratio",
                            "Proportion of Unpaid Work per Day Female/Male Ratio"= "proportion_of_unpaid_work_per_day_female_male_ratio",
                            "Share of Workers in Informal Sector that are Female"= "share_of_workers_in_informal_sector_female",
                            "Women Access to Land (Use & Control Ownership) from 0-1"= "women_s_access_to_land_use_control_ownership_0_1_worst",
                            "Unmet Demand For Family Planning % of Women"= "unmet_demandfor_family_planning_percent_women15_49",
                            "Public Childcare expenditure % by GDP"= "public_childcare_expenditure_percent_gdp",
                            "Right to Equal Justice from 0-1"= "rightto_equal_justice_0_1_worst",
                            "Wage Equality 0-1"= "wage_equality_0_1best",
                            "Maternal Mortality Death's per 100,000 Livebirths"= "maternal_mortality_deathsper100_000livebirths",
                            "Women in Ministerial Positions"= "womenin_ministerial_positions",
                            "Women in Parliment"= "womenin_parliament")
                )
    ),
    column(width = 5,
    selectInput(inputId ="regionsTopic",  # this ID will help us communicate between UI and server
                label = "Choose a Region",
                choices = c("None" = "Skip",
                            "East Asia and the Pacific" = "East Asia and the Pacific",
                            "Eastern Europe and Central Asia"= "Eastern Europe and Central Asia",
                            "Latin America and The Carribean"= "Latin America and The Carribean",
                            "Middle East & North Africa"= "Middle East & North Africa",
                            "North America" = "North America",
                            "South Asia"= "South Asia",
                            "Sub-Saharan Africa"= "Sub-Saharan Africa",
                            "Western Europe" = "Western Europe")
              )
    )
      ),
    ), #sidebarPanel
    mainPanel(width = 12,
      plotlyOutput("SpecificTopic"),
      p(strong("WARNING: "), "Some bars may appear higher than they are supposed to. This is an issue with one the packages used to build the interactive graph but it is only a cosmetic error. Please hover over the bar to see its real value. Apologies for this error and we hope to fix it next patch. If you'd like to help fix it, please see our open source code and contribute ", strong(tags$a(href="https://www.github.com", "here.")))
    )
  )# sidebarLayout
  )
)


# Individual Countries Panel ----
individual_panel <- tabPanel(
  "Explore by Individual Country",
  titlePanel("Bar Chart of Different Metrics Given a Country"),
  fluidPage(
    sidebarPanel(
    selectInput(inputId ="countriesAvailable",  # this ID will help us communicate between UI and server
                label = "Choose A Country",
                choices = listOfCountries
    )),
    mainPanel(width = 12,
      plotlyOutput("IndividualTopic")
    )
  )
)


# Grouping Topics Panel ----
grouping_panel <- tabPanel(
  "Explore By Decided Topics",
  titlePanel("Bar Charts of the Metrics of your Topic of Choice by Countries"),
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 10,
    # Creating Drop down with different variables
        fluidRow(
    column(width = 5,
    selectInput(inputId ="groupingAvailable",  # this ID will help us communicate between UI and server
                label = "Choose Topic",
                choices = c("Economic Patricipation and Opportunities" = "EconomicParticipationandOpp",
                            "Work Participation and Leadership" = "WorkParticipationAndLeadership",
                            "Access to Finance"= "AccesstoFinance",
                            "Family and Care"= "FamilyandCare",
                            "Region Specific Data"= "RegionSpecificData")
      )#Select Input
    ),#column
    
    column(width = 5,
    selectInput(inputId ="regionsGrouping",  # this ID will help us communicate between UI and server
                label = "Choose a Region",
                choices = c("None" = "Skip",
                            "East Asia and the Pacific" = "East Asia and the Pacific",
                            "Eastern Europe and Central Asia"= "Eastern Europe and Central Asia",
                            "Latin America and The Carribean"= "Latin America and The Carribean",
                            "Middle East & North Africa"= "Middle East & North Africa",
                            "North America" = "North America",
                            "South Asia"= "South Asia",
                            "Sub-Saharan Africa"= "Sub-Saharan Africa",
                            "Western Europe" = "Western Europe")
    )
    )#Columns
        ) #fluid row
    ),
    mainPanel(
      width = 12,
      plotlyOutput("GroupingTopic"),
      p(strong("WARNING: "), "Some bars may appear higher than they are supposed to. This is an issue with one the packages used to build the interactive graph but it is only a cosmetic error. Please hover over the bar to see its real value. Apologies for this error and we hope to fix it next patch. If you'd like to help fix it, please see our open source code and contribute ", strong(tags$a(href="https://www.github.com", "here.")))
            )
      
    )#sidebar layout
  )
)

# Pick and choose Countries Panel ----
PickandChoose_panel <- tabPanel(
  "Pick and Choose Country Comparisons",
  titlePanel("Pick Countries and Compare their Metrics"),
  fluidPage(
    # Creating Drop down with different variables
    # checkboxGroupInput(inputId ="checkbox",  # this ID will help us communicate between UI and server
    #             label = "Choose countries",
    #             choices = listOfCountries,
    #             width = '100%'
    # ),
    sidebarLayout(
      sidebarPanel(
        width = 14,
      # fluidrow ----
      fluidRow(#allows you to create columns
        column(1,
               checkboxGroupInput(inputId ="checkbox",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[1:16],
                                  width = '25%',
                                  selected = "Albania"
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox1",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[17:29],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox2",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[31:43],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox3",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[44:55],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox4",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[56:66],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox5",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[67:79],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox6",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[80:92],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox7",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[93:103],
                                  width = '25%'
               ),
        ),
        column(1,
               checkboxGroupInput(inputId ="checkbox8",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[104:115],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox9",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[116:126],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox0",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[128:138],
                                  width = '25%'
               ),
        ),#column
        column(1,
               checkboxGroupInput(inputId ="checkbox10",  # this ID will help us communicate between UI and server
                                  label = "Choose countries",
                                  choices = listOfCountries[139:158],
                                  width = '25%'
               ),
        ),#column
        # column(1,
        #        checkboxGroupInput(inputId ="checkbox0",  # this ID will help us communicate between UI and server
        #                           label = "Choose countries",
        #                           choices = listOfCountries[149:158],
        #                           width = '25%'
        #        ),
        # ),#column
      ),#Fluidrow
      #FluidRow end ----
    ), 
    mainPanel(width = 12, height = 10,
      plotlyOutput("PickandChoose")
     ),
    ),#SidebarPanel
    
  )#FluidPage
)



#“cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, “litera”, “lumen”, “lux”, “materia”, “minty”, “pulse”, “sandstone”, “simplex”, “sketchy”, “slate”, “solar”, “spacelab”, “superhero”, “united”, “yeti”
# Define UI for application ----
shinyUI(fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
    # Application title
    titlePanel("World Economic Forum Gender Gap Data 2021 Dashboard"),
    #includeCSS("Untitled.css"),
    #theme = bs_theme(version = 4, bootswatch = "minty"),
    navbarPage("Visualize The Data: ",
               #theme = shinytheme("quartz"),
               #theme = bs_theme(version = 3, bootswatch = "minty",primary = "#ED79F9"),
               intro_panel,
               predictor_panel,
               individual_panel,
               grouping_panel,
               PickandChoose_panel)#
    
# end of ShinyUI ----
))

