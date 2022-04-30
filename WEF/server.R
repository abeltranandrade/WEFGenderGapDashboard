#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(readr)
library(janitor)
library(RColorBrewer)

# it goes ui -> reactive -> output plot
# gganimate()

# OG Dataset Loading and Cleaning of NA, making it all doubles ----
#"~/Desktop/Academics/Spring 2022/Money:Women:Transnational Movements/ShinyProject/WebGlobalGenderGapData.csv"
WefGlobalGenderGapDataOG <- read_csv("WebGlobalGenderGapData.csv") %>%
  clean_names()

WefGlobalGenderGapDataOG$estimated_earned_income_ratio[WefGlobalGenderGapDataOG$estimated_earned_income_ratio == "no data"] <- NA
WefGlobalGenderGapDataOG$proportion_of_unpaid_work_per_day_female_male_ratio[WefGlobalGenderGapDataOG$proportion_of_unpaid_work_per_day_female_male_ratio == "n/a" | WefGlobalGenderGapDataOG$proportion_of_unpaid_work_per_day_female_male_ratio == "-"] <- NA
WefGlobalGenderGapDataOG$share_of_workers_in_informal_sector_female[WefGlobalGenderGapDataOG$share_of_workers_in_informal_sector_female == "n/a" | WefGlobalGenderGapDataOG$share_of_workers_in_informal_sector_female == "N/A"] <- NA
WefGlobalGenderGapDataOG$women_s_access_to_land_use_control_ownership_0_1_worst[WefGlobalGenderGapDataOG$women_s_access_to_land_use_control_ownership_0_1_worst == "n/a" | WefGlobalGenderGapDataOG$women_s_access_to_land_use_control_ownership_0_1_worst == "no data"] <- NA
WefGlobalGenderGapDataOG$unmet_demandfor_family_planning_percent_women15_49[WefGlobalGenderGapDataOG$unmet_demandfor_family_planning_percent_women15_49 == "n/a" | WefGlobalGenderGapDataOG$unmet_demandfor_family_planning_percent_women15_49 == "no data" | WefGlobalGenderGapDataOG$unmet_demandfor_family_planning_percent_women15_49 == "N/A"] <- NA
WefGlobalGenderGapDataOG$public_childcare_expenditure_percent_gdp[WefGlobalGenderGapDataOG$public_childcare_expenditure_percent_gdp == "n/a" |  WefGlobalGenderGapDataOG$public_childcare_expenditure_percent_gdp == "N/A"] <- NA

WefGlobalGenderGapDataOG$estimated_earned_income_ratio = as.double(WefGlobalGenderGapDataOG$estimated_earned_income_ratio)
WefGlobalGenderGapDataOG$proportion_of_unpaid_work_per_day_female_male_ratio = as.double(WefGlobalGenderGapDataOG$proportion_of_unpaid_work_per_day_female_male_ratio )
WefGlobalGenderGapDataOG$share_of_workers_in_informal_sector_female = as.double(WefGlobalGenderGapDataOG$share_of_workers_in_informal_sector_female)
WefGlobalGenderGapDataOG$women_s_access_to_land_use_control_ownership_0_1_worst = as.double(WefGlobalGenderGapDataOG$women_s_access_to_land_use_control_ownership_0_1_worst)
WefGlobalGenderGapDataOG$unmet_demandfor_family_planning_percent_women15_49 = as.double(WefGlobalGenderGapDataOG$unmet_demandfor_family_planning_percent_women15_49)
WefGlobalGenderGapDataOG$public_childcare_expenditure_percent_gdp = as.double(WefGlobalGenderGapDataOG$public_childcare_expenditure_percent_gdp)


# Pivot Data ----

WefGlobalGenderGapData <- WefGlobalGenderGapDataOG %>% 
  select(-x1) %>% 
  pivot_longer(!country_by_region & !region & !page_number, names_to = "metric", values_to = "value")



#Clean Region Dataset Creation ----
# EconomicParticipationandOpp <- WefGlobalGenderGapData %>%
#   filter(metric == 'labor_force_participation_ratio' | metric == 'labor_force_participation_female_percent' | metric == 'estimated_earned_income_ratio')
# 
# WorkParticipationAndLeadership <- WefGlobalGenderGapData %>%
#   filter(metric == 'proportion_of_unpaid_work_per_day_female_male_ratio' | metric == 'share_of_workers_in_informal_sector_female')
# 
# AccesstoFinance <- WefGlobalGenderGapData %>%
#   filter(metric == 'women_s_access_to_land_use_control_ownership_0_1_worst')
# 
# FamilyandCare <- WefGlobalGenderGapData %>%
#   filter(metric == 'unmet_demandfor_family_planning_percent_women15_49' | metric == 'public_childcare_expenditure_percent_gdp')
# 
# RegionSpecificData <- WefGlobalGenderGapData %>%
#   filter(metric == 'rightto_equal_justice_0_1_worst' | metric == 'wage_equality_0_1best' | metric == 'maternal_mortality_deathsper100_000livebirths'  | metric == 'womenin_ministerial_positions'  | metric == 'womenin_parliament')


# Labels ----
# variableLabels <- c("Labor Force Participation Ratio" = labor_force_participation_ratio,
#                      "Labor Force Participation Female in %" = labor_force_participation_female_percent,
#                      "Estimated Earned Income Ratio" = estimated_earned_income_ratio,
#                      "Proportion of Unpaid Work per Day Female/Male Ratio" = proportion_of_unpaid_work_per_day_female_male_ratio,
#                      "Share of Workers in Informal Sector that are Female" = share_of_workers_in_informal_sector_female,
#                      "Women Access to Land (Use & Control Ownership) from 0-1" = women_s_access_to_land_use_control_ownership_0_1_worst,
#                      "Unmet Demand For Family Planning % of Women" = unmet_demandfor_family_planning_percent_women15_49,
#                      "Public Childcare expenditure % by GDP" = public_childcare_expenditure_percent_gdp,
#                      "Right to Equal Justice from 0-1" = rightto_equal_justice_0_1_worst,
#                      "Wage Equality 0-1" = wage_equality_0_1best,
#                      "Maternal Mortality Death's per 100,000 Livebirths" = maternal_mortality_deathsper100_000livebirths,
#                      "Women in Ministerial Positions"= womenin_ministerial_positions,
#                      "Women in Parliment" = womenin_parliament
#                       )
# 
CBpallette <- "Spectral"
variableLabels <- c("labor_force_participation_ratio" ="Labor Force Participation Ratio" ,
                    "labor_force_participation_female_percent" = "Labor Force Participation Female in %",
                    "estimated_earned_income_ratio" = "Estimated Earned Income Ratio",
                    "proportion_of_unpaid_work_per_day_female_male_ratio" = "Proportion of Unpaid Work per Day Female/Male Ratio",
                    "share_of_workers_in_informal_sector_female" = "Share of Workers in Informal Sector that are Female" ,
                    "women_s_access_to_land_use_control_ownership_0_1_worst" ="Women Access to Land (Use & Control Ownership) from 0-1",
                    "unmet_demandfor_family_planning_percent_women15_49" = "Unmet Demand For Family Planning % of Women",
                    "public_childcare_expenditure_percent_gdp" = "Public Childcare expenditure % by GDP",
                    "rightto_equal_justice_0_1_worst" = "Right to Equal Justice Ranging from 0-1",
                    "wage_equality_0_1best"= "Wage Equality 0-1",
                    "maternal_mortality_deathsper100_000livebirths" = "Maternal Mortality Death's per 100,000 Livebirths" ,
                    "womenin_ministerial_positions" = "Women in Ministerial Positions",
                    "womenin_parliament" = "Women in Parliment"
)

#https://davidmathlogic.com/colorblind/#%23332288-%23117733-%2344AA99-%2388CCEE-%23DDCC77-%23CC6677-%23AA4499-%23882255
regionColorLabels <- c("East Asia and the Pacific" = "#882255",
                       "Eastern Europe and Central Asia"= "#AA4499",
                       "Latin America and The Carribean"= "#CC6677",
                       "Middle East & North Africa"= "#DDCC77",
                       "North America" = "#88CCEE",
                       "South Asia"= "#44AA99",
                       "Sub-Saharan Africa"= "#117733",
                       "Western Europe" = "#332288")
#http://mkweb.bcgsc.ca/colorblind/palettes.mhtml
metricsLabels <- c("labor_force_participation_ratio" ="#9F0162" ,
                   "labor_force_participation_female_percent" = "#009F81",
                   "estimated_earned_income_ratio" = "#008DF9",
                   "proportion_of_unpaid_work_per_day_female_male_ratio" = "#00C2F9",
                   "share_of_workers_in_informal_sector_female" = "#FFB2FD" ,
                   "women_s_access_to_land_use_control_ownership_0_1_worst" ="#A40122",
                   "unmet_demandfor_family_planning_percent_women15_49" = "#E20134",
                   "public_childcare_expenditure_percent_gdp" = "#FF6E3A",
                   "rightto_equal_justice_0_1_worst" = "#FFC33B",
                   "wage_equality_0_1best"= "#FF5AAF",
                   "maternal_mortality_deathsper100_000livebirths" = "#00FCCF" ,
                   "womenin_ministerial_positions" = "#8400CD",
                   "womenin_parliament" = "#003C86")
# Old Region Dataset Loading ----
# EconomicParticipationandOpp <- WefGlobalGenderGapData %>%
#   select(country_by_region, region, labor_force_participation_ratio,labor_force_participation_female_percent, estimated_earned_income_ratio)  %>%
#   mutate(estimated_earned_income_ratio = as.double(estimated_earned_income_ratio)) %>% 
#   pivot_longer(!country_by_region & !region, names_to = "metric", values_to = "value")
# 
# WorkParticipationAndLeadership <- WefGlobalGenderGapData %>%
#   select(country_by_region, region, proportion_of_unpaid_work_per_day_female_male_ratio, share_of_workers_in_informal_sector_female) %>% 
#   pivot_longer(!country_by_region & !region, names_to = "metric", values_to = "value")
#   
# AccesstoFinance <- WefGlobalGenderGapData %>%
#   select(country_by_region, region, women_s_access_to_land_use_control_ownership_0_1_worst) %>% 
#   pivot_longer(!country_by_region & !region, names_to = "metric", values_to = "value")
# 
# FamilyandCare <- WefGlobalGenderGapData %>%
#   select(country_by_region, region, unmet_demandfor_family_planning_percent_women15_49, public_childcare_expenditure_percent_gdp) %>% 
#   pivot_longer(!country_by_region & !region, names_to = "metric", values_to = "value")
# 
# RegionSpecificData <- WefGlobalGenderGapData %>%
#   select(country_by_region, region, rightto_equal_justice_0_1_worst,wage_equality_0_1best, maternal_mortality_deathsper100_000livebirths,womenin_ministerial_positions ,womenin_parliament) %>% 
#   pivot_longer(!country_by_region & !region, names_to = "metric", values_to = "value")
# 
# Changed <- EconomicParticipationandOpp %>% 
#   #select(-region) %>% 
#   pivot_longer(!country_by_region & !region, names_to = "metric", values_to = "value")
#   





# Shiny Server ----
shinyServer(function(input, output) {
    
    #Old Reactives ----
    # Reactive for just certain Variable analysis -- the inputs
  # varInput <- reactive({    # datasetInput = predictorInput 
  #   switch(input$variablesAvailable, 
  #          "Labor Force Participation Ratio" = WefGlobalGenderGapData$labor_force_participation_ratio,
  #          "Labor Force Participation Female in %" = WefGlobalGenderGapData$labor_force_participation_female_percent,
  #          "Estimated Earned Income Ratio" = WefGlobalGenderGapData$estimated_earned_income_ratio,
  #          "Proportion of Unpaid Work per Day Female/Male Ratio" = WefGlobalGenderGapData$proportion_of_unpaid_work_per_day_female_male_ratio,
  #          "Share of Workers in Informal Sector that are Female" = WefGlobalGenderGapData$share_of_workers_in_informal_sector_female,
  #          "Women Access to Land (Use & Control Ownership) from 0-1" = WefGlobalGenderGapData$women_s_access_to_land_use_control_ownership_0_1_worst,
  #          "Unmet Demand For Family Planning % of Women" = WefGlobalGenderGapData$unmet_demandfor_family_planning_percent_women15_49,
  #          "Public Childcare expenditure % by GDP" = WefGlobalGenderGapData$public_childcare_expenditure_percent_gdp, 
  #          "Right to Equal Justice from 0-1" = WefGlobalGenderGapData$rightto_equal_justice_0_1_worst,
  #          "Wage Equality 0-1" = WefGlobalGenderGapData$wage_equality_0_1best,
  #          "Maternal Mortality Death's per 100,000 Livebirths" = WefGlobalGenderGapData$maternal_mortality_deathsper100_000livebirths,
  #          "Women in Ministerial Positions"= WefGlobalGenderGapData$womenin_ministerial_positions,
  #          "Women in Parliment" = WefGlobalGenderGapData$womenin_parliament
  #   )#switch
  # })#reactive
  # 
  # Old code
  # varOutputData <- reactive(
  #   req(input$variablesAvailable),
  #   df <- WefGlobalGenderGapData %>% select(input$variablesAvailable, country_by_region, region) %>% 
  #     filter(!is.na(input$ variablesAvailable))
  # )
  # 
  # groupingInput <- reactive({    # datasetInput = predictorInput 
  #   switch(input$topic, 
  #          "Economic Patricipation and Opportunities" = EconomicParticipationandOpp,
  #          "Work Participation and Leadership" = WorkParticipationAndLeadership,
  #          "Access to Finance" = AccesstoFinance,
  #          "Family and Care" = FamilyandCare,
  #          "Region Specific Data" = RegionSpecificData
  #   )#switch
  # })#reactive
  # 
  # Label Reactives ----
  variableLabelsR <- reactive({    # datasetInput = predictorInput
    switch(input$variablesAvailable,
           "labor_force_participation_ratio" ="Labor Force Participation Ratio" ,
           "labor_force_participation_female_percent" = "Labor Force Participation Female in %",
           "estimated_earned_income_ratio" = "Estimated Earned Income Ratio",
           "proportion_of_unpaid_work_per_day_female_male_ratio" = "Proportion of Unpaid Work per Day Female/Male Ratio",
           "share_of_workers_in_informal_sector_female" = "Share of Workers in Informal Sector that are Female" ,
           "women_s_access_to_land_use_control_ownership_0_1_worst" ="Women Access to Land (Use & Control Ownership) from 0-1",
           "unmet_demandfor_family_planning_percent_women15_49" = "Unmet Demand For Family Planning % of Women",
           "public_childcare_expenditure_percent_gdp" = "Public Childcare expenditure % by GDP",
           "rightto_equal_justice_0_1_worst" = "Right to Equal Justice Ranging from 0-1",
           "wage_equality_0_1best"= "Wage Equality 0-1",
           "maternal_mortality_deathsper100_000livebirths" = "Maternal Mortality Death's per 100,000 Livebirths" ,
           "womenin_ministerial_positions" = "Women in Ministerial Positions",
           "womenin_parliament" = "Women in Parliment"
           
    )#switch
  })#reactive
# Working Data Reactives  ----
  topicData <- reactive({
    req(input$variablesAvailable)
    req(input$regionsTopic)
    #This allows you to see what you are calling at the UI selectInput level
    print(input$variablesAvailable)
    
    #declare region as the whole dataset in case it does not go into the if statement(this is for skip case)
    region <- WefGlobalGenderGapData
    if(input$regionsTopic != "Skip"){
      region <-region %>% 
        filter(region == input$regionsTopic)
    }
    
    #Filter by the metric they pick on the drop down
    df <- region %>%
      filter(metric %in% input$variablesAvailable) %>% 
      na.omit()
    # df <- WefGlobalGenderGapData %>%
    #   filter(metric == input$variablesAvailable)
    #View(df) # for debugging
    return(df)
  })
    
  groupingData <- reactive({
    req(input$groupingAvailable)
    req(input$regionsGrouping)
    print(input$groupingAvailable)
    
    #declare this so if they pick No region we can still filter by the grouping they want
    region <- WefGlobalGenderGapData
    #Filtering for region
    if(input$regionsGrouping != "Skip"){
      region <- region %>% 
        filter(region == input$regionsGrouping)
    }
      
    #filtering for group
    if(input$groupingAvailable == "EconomicParticipationandOpp"){
      df <- region %>%
        filter(metric == 'labor_force_participation_ratio' | metric == 'labor_force_participation_female_percent' | metric == 'estimated_earned_income_ratio')
      
    } else if(input$groupingAvailable == "WorkParticipationAndLeadership"){
      df <- region %>%
        filter(metric == 'proportion_of_unpaid_work_per_day_female_male_ratio' | metric == 'share_of_workers_in_informal_sector_female')
      
    }else if(input$groupingAvailable == "AccesstoFinance"){
      df <- region %>%
        filter(metric == 'women_s_access_to_land_use_control_ownership_0_1_worst')
    }else if(input$groupingAvailable == "FamilyandCare"){
      df <- region %>%
        filter(metric == 'unmet_demandfor_family_planning_percent_women15_49' | metric == 'public_childcare_expenditure_percent_gdp')
      
    }else if(input$groupingAvailable == "RegionSpecificData"){
      df <- region %>%
        filter(metric == 'rightto_equal_justice_0_1_worst' | metric == 'wage_equality_0_1best' | metric == 'maternal_mortality_deathsper100_000livebirths'  | metric == 'womenin_ministerial_positions'  | metric == 'womenin_parliament')
    }
    
    # Cosmetic change for the plot
    df$metric <- replace(df$metric, df$metric == "labor_force_participation_ratio" , "Labor Force Participation Ratio")
    df$metric <- replace(df$metric, df$metric == "labor_force_participation_female_percent" , "Labor Force  Participation Female in %")
    df$metric <- replace(df$metric, df$metric == "estimated_earned_income_ratio" , "Estimated Earned Income Ratio")
    df$metric <- replace(df$metric, df$metric == "proportion_of_unpaid_work_per_day_female_male_ratio" , "Proportion of Unpaid Work per Day Female/Male Ratio")
    df$metric <- replace(df$metric, df$metric == "share_of_workers_in_informal_sector_female" , "Share of Workers in Informal Sector that are Female")
    df$metric <- replace(df$metric, df$metric == "women_s_access_to_land_use_control_ownership_0_1_worst" , "Women Access to Land (Use & Control Ownership) from 0-1")
    df$metric <- replace(df$metric, df$metric == "unmet_demandfor_family_planning_percent_women15_49" , "Unmet Demand For Family Planning % of Women")
    df$metric <- replace(df$metric, df$metric == "public_childcare_expenditure_percent_gdp" , "Public Childcare expenditure % by GDP")
    df$metric <- replace(df$metric, df$metric == "rightto_equal_justice_0_1_worst" , "Right to Equal Justice Ranging from 0-1")
    df$metric <- replace(df$metric, df$metric == "wage_equality_0_1best" , "Wage Equality from 0-1")
    df$metric <- replace(df$metric, df$metric == "maternal_mortality_deathsper100_000livebirths" , "Maternal Mortality Deaths per 100,000 Livebirths")
    df$metric <- replace(df$metric, df$metric == "womenin_ministerial_positions" , "Women in Ministerial Positions")
    df$metric <- replace(df$metric, df$metric == "womenin_parliament" , "Women in Parliment")
    return(df)
  })
  
  individualData <- reactive({
    req(input$countriesAvailable)
  
    
    df <- WefGlobalGenderGapData %>% 
      filter(country_by_region == input$countriesAvailable) %>% 
      na.omit(value)
    
    # Cosmetic change for the plot
    df$metric <- replace(df$metric, df$metric == "labor_force_participation_ratio" , "Labor Force\nParticipation\nRatio")
    df$metric <- replace(df$metric, df$metric == "labor_force_participation_female_percent" , "Labor Force \n Participation \n Female in %")
    df$metric <- replace(df$metric, df$metric == "estimated_earned_income_ratio" , "Estimated Earned \n Income Ratio")
    df$metric <- replace(df$metric, df$metric == "proportion_of_unpaid_work_per_day_female_male_ratio" , "Proportion of \nUnpaid Work per Day \nFemale/Male Ratio")
    df$metric <- replace(df$metric, df$metric == "share_of_workers_in_informal_sector_female" , "Share of Workers \nin Informal Sector \nthat are Female")
    df$metric <- replace(df$metric, df$metric == "women_s_access_to_land_use_control_ownership_0_1_worst" , "Women Access to Land \n (Use & Control Ownership) \n from 0-1")
    df$metric <- replace(df$metric, df$metric == "unmet_demandfor_family_planning_percent_women15_49" , "Unmet Demand \n For Family Planning \n % of Women")
    df$metric <- replace(df$metric, df$metric == "public_childcare_expenditure_percent_gdp" , "Public Childcare \nexpenditure % by GDP")
    df$metric <- replace(df$metric, df$metric == "rightto_equal_justice_0_1_worst" , "Right to \n Equal Justice\n Ranging from 0-1")
    df$metric <- replace(df$metric, df$metric == "wage_equality_0_1best" , "Wage Equality\n from 0-1")
    df$metric <- replace(df$metric, df$metric == "maternal_mortality_deathsper100_000livebirths" , "Maternal Mortality Deaths \nper 100,000 Livebirths")
    df$metric <- replace(df$metric, df$metric == "womenin_ministerial_positions" , "Women in \nMinisterial Positions")
    df$metric <- replace(df$metric, df$metric == "womenin_parliament" , "Women in \nParliment")
    return(df)
    
  })
  
  PickData <- reactive({
    #req(input$checkbox, input$checkbox0) this will make it required to have 
    #This allows you to see what you are calling at the UI selectInput level
    print("this is checkboxes")
    print(input$checkbox)
    print(input$checkbox0)
    print(input$checkbox1)
    
    checkboxes = c(input$checkbox,input$checkbox0,input$checkbox1,input$checkbox2,input$checkbox3,input$checkbox4,input$checkbox5,input$checkbox6,input$checkbox7,input$checkbox8,input$checkbox9,input$checkbox10)
    
    
    df <- WefGlobalGenderGapData %>%
      filter(country_by_region %in% checkboxes) %>% 
      na.omit(value)
    
    df$metric <- replace(df$metric, df$metric == "labor_force_participation_ratio" , "Labor Force\nParticipation\nRatio")
    df$metric <- replace(df$metric, df$metric == "labor_force_participation_female_percent" , "Labor Force \n Participation \n Female in %")
    df$metric <- replace(df$metric, df$metric == "estimated_earned_income_ratio" , "Estimated Earned \n Income Ratio")
    df$metric <- replace(df$metric, df$metric == "proportion_of_unpaid_work_per_day_female_male_ratio" , "Proportion of \nUnpaid Work per Day \nFemale/Male Ratio")
    df$metric <- replace(df$metric, df$metric == "share_of_workers_in_informal_sector_female" , "Share of Workers \nin Informal Sector \nthat are Female")
    df$metric <- replace(df$metric, df$metric == "women_s_access_to_land_use_control_ownership_0_1_worst" , "Women Access to Land \n (Use & Control Ownership) \n from 0-1")
    df$metric <- replace(df$metric, df$metric == "unmet_demandfor_family_planning_percent_women15_49" , "Unmet Demand \n For Family Planning \n % of Women")
    df$metric <- replace(df$metric, df$metric == "public_childcare_expenditure_percent_gdp" , "Public Childcare \nexpenditure % by GDP")
    df$metric <- replace(df$metric, df$metric == "rightto_equal_justice_0_1_worst" , "Right to \n Equal Justice\n Ranging from 0-1")
    df$metric <- replace(df$metric, df$metric == "wage_equality_0_1best" , "Wage Equality\n from 0-1")
    df$metric <- replace(df$metric, df$metric == "maternal_mortality_deathsper100_000livebirths" , "Maternal Mortality Deaths \nper 100,000 Livebirths")
    df$metric <- replace(df$metric, df$metric == "womenin_ministerial_positions" , "Women in \nMinisterial Positions")
    df$metric <- replace(df$metric, df$metric == "womenin_parliament" , "Women in \nParliment")
    #View(df) # for debugging purposes
    return(df)
  })
  # Output Plots ----
    #This is the basic one that came in 
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    
    #This is the one for the specific topic 
    output$SpecificTopic <- renderPlotly({
      # load data from the reactive and view is just to check whats going on
      data <- topicData()
      label <- variableLabelsR()
      #View(data)# for debugging purposes
      # noticed data variable class is a function so cast it as a data frame which is what you are returning
      data = as.data.frame(data)
      #View(data)# for debugging purposes
      print(data$region)
      #Create the plot
      print("I am right on top of the ggplot!")
      chart <- ggplot(data, aes(x = reorder(country_by_region, -value),   # Take the reactive topicData
                                  y = value,
                                  fill = region,
                                  text = paste("Country:",country_by_region,"\n",
                                               "Value",value,"\n",
                                               "Region:",region,"\n",
                                               "Page Source:",page_number,"\n"
                                  )))+
        scale_fill_brewer(palette =CBpallette)+
        labs(x = "Countries", y = label)+
        theme(axis.text.x = element_blank())+
        geom_col() 
      
      # create the ggplotly and give it a name, display it
      final<- ggplotly(chart, tooltip = "text")
      #print(class(final))
      final
     
    })
    
    output$GroupingTopic <- renderPlotly({
      # load data from the reactive and view is just to check whats going on
      data <- groupingData()
      # make the data from reactive a dataframe
      data = as.data.frame(data)
      
      myColors <- brewer.pal(5,CBpallette)
      names(myColors) <- levels(data$region)
      colScale <- scale_colour_manual(name = "Region",values = myColors)
      
      #Create the plot
      chart <- ggplot(data, aes(fill = region,
                                x = reorder(country_by_region, -value),   # Take the reactive topicData
                                y = value,
                                text = paste("Country:",country_by_region,"\n",
                                             "Value",value,"\n",
                                             "Region:",region,"\n",
                                             "Page Source:",page_number,"\n"
                                )))+
        labs(x = "Countries", y = "Value of Metric", fill = "Metric")+
        scale_fill_brewer(palette = CBpallette)+
        theme(axis.text.x = element_blank())+
        geom_col(position = "stack") +
        facet_wrap(~metric, scales = "free_y", ncol = 1)
      
      # create the ggplotly and give it a name, display it
      final<- ggplotly(chart, tooltip = "text")
      final
      
    })
    
    output$IndividualTopic <- renderPlotly({
      data <- individualData()
      data = as.data.frame(data)
      filllabel <- variableLabelsR()
      
      chart <- ggplot(data, aes(fill = metric,
                                x = reorder(country_by_region, -value),   # Take the reactive topicData
                                y = value,
                                text = paste("Country:",country_by_region,"\n",
                                             "Value of Metric:",value,"\n",
                                             "Region:",region,"\n",
                                             "Page Source:",page_number,"\n"
                                )))+
        labs( x=input$countriesAvailable, y = "Value of the Metric" ,fill = "Different Metrics Available:")+ #still needs fill label
        theme(axis.text.x = element_blank())+
        scale_fill_brewer(palette = CBpallette)+
        geom_col(position = "dodge") 
      
      # create the ggplotly and give it a name, display it
      final <- ggplotly(chart, tooltip = "text")
      final
      
    })
    
    output$PickandChoose <- renderPlotly({
      data <- PickData()
      data = as.data.frame(data)

      chart <- ggplot(data, aes(fill = country_by_region,
                                x = reorder(metric, -value),   # Take the reactive topicData
                                y = value,
                                text = paste("Country:",country_by_region,"\n",
                                             "Value:",value,"\n",
                                             "Region:",region,"\n",
                                             "Page Source:",page_number,"\n"
                                )))+
        scale_fill_brewer(palette = CBpallette)+
        labs(x = "Metrics", y = "Value of the Metric", fill = "Countries")+
        geom_col(position = "dodge") 
      
      
      # create the ggplotly and give it a name, display it
      finishedChart <- ggplotly(chart, tooltip = "text")
      finishedChart
      
    })
    # Other Plot Attempt ----
    # output$topics <- renderPlotly({
    #   #topics <- groupingInput()
    #   
    # 
    #   
    #   lol <- ggplot(WefGlobalGenderGapData, aes(x = reorder(country_by_region,-input$variablesAvailable),
    #                                         y = input$variablesAvailable,
    #                                         fill = region,
    #                                         text = paste("Country:",country_by_region,"\n",
    #                                                      "Women In Parliment Ratio:",input$variablesAvailable,"\n",
    #                                                      "Region:",region,"\n"
    #                                         )))+
    #     theme(axis.text.x = element_blank())+
    #     geom_col() 
    #   
    #   
    #   final <- ggplotly(lol, tooltip = "text")
    #   final
    #   
    # })
    
    #cleaning <- reactive({})
# end ----
})


# testing out plots with new wrangle ----
# Test2 <- Test %>% 
#   filter(metric == 'estimated_earned_income_ratio')
# 
# EPO <- Test %>% 
#   filter(metric == 'labor_force_participation_ratio' | metric == 'labor_force_participation_female_percent' | metric == 'estimated_earned_income_ratio')
# 
# hehe <- ggplot(EPO, aes(x = country_by_region, y = value, fill = region)) +
#   geom_col()
# 
# hehe2 <- ggplot(Test2, aes(x = country_by_region, y = value, fill = region)) +
#   geom_col()+
#   theme(axis.text.x = element_blank())
# final2 <- ggplotly(hehe2)
# final2



# Old Commented Wrangling ----
# #(3 NA: n/a, -, NA)
# WorkDistribution <- WorkParticipationAndLeadership %>% 
#   group_by(`ProportionOfUnpaidWorkPerDay,Female/maleRatio`) %>% 
#   summarize(count = order(n()))
# # 3 NA: NA, N/A,n/a
# InformalDistribution <- WorkParticipationAndLeadership %>% 
#   group_by(`ShareOfWorkersInInformalSector(female)`) %>% 
#   summarize(count = order(n()))
# 
# 
# ggplot(WefGlobalGenderGapData, aes(x = reorder(`Country (by Region)`, -LaborForceParticipationRatio), y = LaborForceParticipationRatio, fill = Region))+
#   facet_wrap(~Region)+
#   geom_col()
# 


# library(plotly)
# 
# filtered <- subset(RegionSpecificData, !is.na(womenin_parliament))
# filtered[order(filtered$womenin_parliament)]
# 
# lol <- ggplot(filtered, aes(x = reorder(country_by_region,-womenin_parliament),
#                             y = womenin_parliament,
#                             fill = region,
#                             text = paste("Country:",country_by_region,"\n",
#                                          "Women In Parliment Ratio:",womenin_parliament,"\n",
#                                          "Region:",region,"\n"
#                                          )))+
#   theme(axis.text.x = element_blank())+
#   geom_col()
# 
# ggplotly(lol, tooltip = "text")


# 
# View(JusticeDistribution)
# View(InformalDistribution)

#shinyApp(ui = ui, server = server)