##############PACKAGES##############

library(comprehenr)
library(dplyr)
library(plotly)
library(readxl)
library(shiny)
library(tidyr)



##############CONSTANTS##############
#No Scientific Notation.
options(scipen = 999)

#Metrics.
pop_list <- c("Electoral College",
              "Total Population",
              "Eligible Population",
              "Registered Population",
              "Voter Population")

#States, add District of Columbia in alphabetical order.
state_list <- c(state.name[1:8],
                "District of Columbia",
                state.name[9:50])

#St, add DC in alphabetical order. Add USA at end.
state_abb <- c(state.abb[1:8],
               "DC",
               state.abb[9:50],
               "USA")

abb_list <- rep(state_abb, 7)



##############READ FILES##############

#Read data.
df <- as.data.frame(read_excel("./Electoral College.xlsx"))


#Total voting population is sum of Red, Blue, and Other votes.
df <- df %>%
  mutate(p_voting = Republican + Democrat + other,
         abb = abb_list) %>%
  select(2:5, 10, 1, 11, 6:9)

#DF of only USA total data.
df_usa <- df %>%
  filter(state == "United States")

#DF of all 50 states and DC.
df <- df %>%
  filter(state != "United States")



##############FUNCTIONS##############

#Format tab title.
tab_title <- function(input_text) {
  strong(span(textOutput(input_text), style = "text-decoration: underline"))
}



#Helper function for Links.  Creates HTML.
create_html <- function(description, title, link, end) {
  tagList(description, a(title, href = link), end)
}



#Gets the other party.
get_other_party <- function(party) {
  setdiff(c("Republican", "Democrat"), party)
}



##############UI##############
ui <- fluidPage(
  
  #Remove warnings.
  tags$style(type = "text/css",
             ".shiny-output-error {visibility: hidden;}",
             ".shiny-output-error:before {visibility: hidden;}"),
  
  titlePanel("US Election and Electoral College Exploration"),
  sidebarLayout(
    
    ##############SIDEBAR PANEL##############
    sidebarPanel(width = 3,
                 #Used in all tabs.
                 h6("Established December 2020"),
                 h3("Select:"),
                 radioButtons("elec_yr",
                              "Election Year :",
                              c(2016, 2012, 2008, 2004, 2000, 1996)),
                 h6("November 2020 Election Data is not yet available."),
      
                 #Values by State.
                 conditionalPanel(condition = "input.mytabs == 1",
                                  br(),
                                  radioButtons("color_map",
                                               "Color Map By:", 
                                               c("Electoral College" = 1,
                                                 "Total Population" = 2,
                                                 "Eligible Population" = 3,
                                                 "Registered Population" = 4,
                                                 "Voter Population" = 5))),
                 
                 #Electoral College Weight per State.
                 conditionalPanel(condition = "input.mytabs == 2",
                                  br(),
                                  radioButtons("color_map2",
                                               "Divide Electoral College Into:",
                                               c("Total Population" = 2,
                                                 "Eligible Population" = 3,
                                                 "Registered Population" = 4,
                                                 "Voter Population" = 5))),
                 
                 #Counterintuitive State Comparisons.
                 conditionalPanel(condition = "input.mytabs == 3",
                                  br(),
                                  selectInput("state_select1",
                                              "State to Compare to Others:",
                                              state_list,
                                              selected = "California")),
                 
                 #Sum of States.
                 conditionalPanel(condition = "input.mytabs == 4",
                                  br(),
                                  selectInput("state_select2",
                                              "States to Sum:",
                                              state_list,
                                              selected = c("California", "New York"),
                                              multiple = TRUE),
                                  h6("(Select and click delete with alt or ctrl to remove.)"),
                                  br(),
                                  radioButtons("pop_metric",
                                               "Metric to Sum",
                                               c("Total Population" = 2,
                                                 "Eligible Population" = 3,
                                                 "Registered Population" = 4,
                                                 "Voter Population" = 5)))),
    
    

    ##############MAIN PANEL##############
    
    mainPanel(width = 9,
      navbarPage(uiOutput("main_panel"), id = "mytabs",

                 #Values by State.                 
                 tabPanel(textOutput("tab1_title"), value = 1,
                          br(),
                          plotlyOutput("pop_plot", height = 600),
                          verbatimTextOutput("tab1_summary")),

                 #Electoral College Weight per State.
                 tabPanel(textOutput("tab2_title"), value = 2,
                          br(),
                          plotlyOutput("vote_w_plot", height = 600),
                          verbatimTextOutput("tab2_summary")),

                 #Counterintuitive State Comparisons.                          
                 tabPanel(textOutput("tab3_title"), value = 3,
                          br(),
                          plotlyOutput("comparisons_plot", height = 600),
                          uiOutput("tab3_faithless"),
                          br(),
                          verbatimTextOutput("tab3_summary")),

                 #Sum of States.                 
                 tabPanel(textOutput("tab4_title"), value = 4,
                          br(),
                          textOutput("tab4_text1"),
                          br(),
                          htmlOutput("tab4_text2"),
                          br(),
                          plotlyOutput("sum_plot", height = 600),
                          verbatimTextOutput("tab4_summary")),

                 navbarMenu("More",
                            tabPanel("General Information",
                                     tab_title("tab9_title"),
                                     br(),
                                     textOutput("tab9_text1"),
                                     br(),
                                     textOutput("tab9_text2"),
                                     br(),
                                     textOutput("tab9_text3"),
                                     br(),
                                     uiOutput("tab9_links1"),
                                     br(),
                                     textOutput("tab9_text4"),
                                     br(),
                                     uiOutput("tab9_links2"),
                                     br(),
                                     uiOutput("tab9_links3"),
                                     br(),
                                     textOutput("tab9_email1"),
                                     br(),
                                     textOutput("tab9_email2")),
                            
                            "------",
                            
                            tabPanel("Sources",
                                     tab_title("tab10_title"),
                                     br(),
                                     uiOutput("tab10_links1"),
                                     br(),
                                     uiOutput("tab10_links2"),
                                     br(),
                                     uiOutput("tab10_links3")),
                            
                            "------",
                            
                            tabPanel("Related Links",
                                     tab_title("tab11_title"),
                                     br(),
                                     uiOutput("tab11_link1"),
                                     br(),
                                     uiOutput("tab11_link2"),
                                     br(),
                                     uiOutput("tab11_link3"),
                                     br(),
                                     uiOutput("tab11_link4")),
                            
                            "------")))))



##############SERVER##############
server <- function(input, output, session) {
  
  output$main_panel <- renderUI(paste(input$elec_yr, "Election Year:"))

  
  
  ##############REACTIVE FUNCTIONS##############  
  
  #Hovertext helper.
  win_text <- function(df, party) {
    paste("In", paste(df$year, ",", sep = ""), df$state, "went", party, "with",
          prettyNum(round(df[, party], 0), big.mark = ","), "votes,<br>winning the", df$ec,
          "Electoral College Votes.")
  }
  
  
  
  #Update dataframe for plotlys.
  fill_df <- reactive({

    #Filter for year.
    fill_df <- df %>%
      filter(year == input$elec_yr)

    #Values by State.
    if (input$mytabs == 1) {

      #Create hovertext.
      fill_df$hover <- with(fill_df,
                            paste("In", paste(year, ",", sep = ""), state, "had", prettyNum(p_total, big.mark = ","), "people and", ec, "Electoral College Votes.<br>",
                                  "<br>", prettyNum(p_eligible, big.mark = ","), "were eligible voters.",
                                  "<br>", prettyNum(p_registered, big.mark = ","), "were registered voters.",
                                  "<br>", prettyNum(p_voting, big.mark = ","), "actually voted."))

    #Electoral College Weight per State.
    } else if (input$mytabs == 2) {
      
      #Get index and metric from user input.
      scale_index <- length(integer(input$color_map2))
      metric <- pop_list[scale_index]
      
      #Divide metric by EC.
      fill_df$ec_ratio <- fill_df[, scale_index] / fill_df[, 1]

      #State for year and metric with the highest pop per EC.
      anchor <- max(fill_df$ec_ratio)
      anchor_state <- fill_df$state[fill_df$ec_ratio == anchor]
      
      #All states compared to state with highest pop per EC.
      fill_df$ec_scaled <- anchor / fill_df$ec_ratio

      #Create hovertext.
      fill_df$hover <- with(fill_df,
                            paste("In", paste(year, ",", sep = ""), state, "had a", metric, "of", prettyNum(fill_df[, scale_index], big.mark = ","), "and", ec, "Electoral College Votes.<br>",
                                  "<br>Dividing yields", prettyNum(round(ec_ratio, 0), big.mark = ","), "people per Electoral College Vote.<br>",
                                  "<br>Normalizing for the minimum,", anchor_state, "as 1.00, each", state, metric,
                                  "<br>vote weighs", round(ec_scaled, 2), "times that of a", anchor_state, metric, "vote."))
      
      #Counterintuitive State Comparisons.  
    } else if (input$mytabs == 3) {
      
      #Did a state have more Republican or Democrat votes in the year?
      fill_df <- fill_df %>%
        mutate(aff = ifelse(Republican > Democrat, "Republican", "Democrat")) %>%
        select(year, ec, state, abb, Republican, Democrat, aff)
      
      #Data only for user selected state.
      ref_df <- fill_df[fill_df[,"state"] == input$state_select1, ]
      
      #Party and number of votes of user selected state.
      anchor_party <- ref_df$aff
      #Opposite color.
      other_party <- get_other_party(anchor_party)
      
      
      #Exceptions.
      
      exception_text <- function(df) {
        
        anchor_party <- df$aff 
        other_party <- get_other_party(anchor_party) 
        
        with(df,
             paste("In", paste(year, ",", sep = ""), state, "had", prettyNum(round(df[, anchor_party], 0), big.mark = ","), anchor_party,
                   "votes and", prettyNum(round(df[, other_party], 0), big.mark = ","), other_party, "votes.<br>",
                   paste("<br>", anchor_party, "s", sep = ""), "won", ec - 1, "Electoral College Votes and",
                   paste(other_party, "s", sep = ""), "won 1 Electoral College Vote.<br>",
                   "<br>See General Information."))
      }
      
      #Maine and Nebraska are not winner-take-all.
      if (((ref_df$year == 2008) & (ref_df$state == "Nebraska")) | 
          ((ref_df$year == 2016) & (ref_df$state == "Maine")) |
          ((ref_df$year == 2020) & (ref_df$state %in% c("Maine", "Nebraska")))) {
        
        ref_df$filling <- 1
        ref_df$hover <- exception_text(ref_df) 
          
        #Dummy.
        com_df <- fill_df %>%
          filter(aff == "other")
        
        #Select states that are of the other party and
        #have more votes of that party than than user selected state.
        extra_df1 <- fill_df %>%
          filter(aff == other_party)
        
        #Select states that are of the same party and deselect user selected state.
        extra_df2 <- fill_df %>%
          filter(aff == anchor_party,
                 state != input$state_select1)
        
      #Else.
      } else {
        
        anchor_votes <- ref_df[, anchor_party][[1]]
        other_votes <- ref_df[, other_party][[1]]
        
        #Select states that are of the other party and
        #have less votes of that party than user selected state.
        com_df <- fill_df %>%
          filter(aff == other_party,
                 fill_df[, other_party] < other_votes)
        
        #Select states that are of the other party and
        #have more votes of that party than than user selected state.
        extra_df1 <- fill_df %>%
          filter(aff == other_party,
                 fill_df[, other_party] > other_votes)
        
        #Select states that are of the same party and deselect user selected state.
        extra_df2 <- fill_df %>%
          filter(aff == anchor_party,
                 state != input$state_select1)
        
        #Hovertext.
        
        #User selected state.
        ref_df$filling <- 1
        ref_df$hover <- paste(win_text(ref_df, anchor_party),
                              "<br>",
                              "<br>The winner-take-all system does not take into consideration the<br>",
                              prettyNum(round(other_votes, 0), big.mark = ","),
                              "who voted", other_party, "in", paste(ref_df$state, ".", sep = ""))
      }
    
      #Same party states.
      extra_df2$filling <- -2
      extra_df2$hover <- win_text(extra_df2, anchor_party)
      
      #There could be no states with more other party votes.
      if (nrow(extra_df1) > 0) {
        extra_df1$filling <- -1
        extra_df1$hover <- win_text(extra_df1, other_party)
      }
      
      #There could be no states with less other party votes.
      if (nrow(com_df) > 0) {
        com_df$filling <- 0
        com_df$hover <- paste(win_text(com_df, other_party),
                              "<br>",
                              "<br>This is less than the", prettyNum(round(other_votes, 0), big.mark = ","), 
                              other_party, "votes in", ref_df$state, "<br>ignored by the Electoral College.")
        
        fill_df <- rbind(ref_df, com_df, extra_df1, extra_df2)
        
      } else {
        fill_df <- rbind(ref_df, extra_df1, extra_df2)
      }
      
      if ((input$elec_yr == 2008) & (input$state_select1 != "Nebraska")) {
        fill_df[fill_df$state == "Nebraska",]$filling <- -2
        fill_df[fill_df$state == "Nebraska",]$hover <- exception_text(fill_df[fill_df$state == "Nebraska",])
        
      } else if ((input$elec_yr == 2016) & (input$state_select1 != "Maine")) {
        fill_df[fill_df$state == "Maine",]$filling <- -2
        fill_df[fill_df$state == "Maine",]$hover <- exception_text(fill_df[fill_df$state == "Maine",])
        
      } else if ((input$elec_yr == 2020) & !(input$state_select1 %in% c("Maine", "Nebraska"))) {
        fill_df[fill_df$state == "Maine",]$filling <- -2
        fill_df[fill_df$state == "Nebraska",]$filling <- -2
        fill_df[fill_df$state == "Maine",]$hover <- exception_text(fill_df[fill_df$state == "Maine",])
        fill_df[fill_df$state == "Nebraska",]$hover <- exception_text(fill_df[fill_df$state == "Nebraska",])
      }

      #States to Sum.              
    } else {
      pop_index <- length(integer(input$pop_metric))
      fill_df <- fill_df %>%
        select(all_of(pop_index), 6:9) %>%
        mutate(f = ifelse(state %in% input$state_select2,
                          2,
                          sample(0:1, 2)))
    }
    
    fill_df
    
  })
  
  
  
  #Create ticks for colorbar, depending on year and metric.
  colorbar_info <- reactive({

    if (input$mytabs == 1) {
      year <- length(integer(input$elec_yr))
      
      #Maximum is based on California.
      max = c(56,
              (36 + 4 * floor(year / 2008)) * 1000000,
              (24 + 4 * floor(year / 2016)) * 1000000,
              (16 + 4 * floor(year / 2016)) * 1000000,
              (12 + 4 * floor(year / 2004)) * 1000000)
      
      ref = max[length(integer(input$color_map))]
      
      cb_limits = c(0, ref)
      cb_ticks = to_vec(for (t in 0:4) t*ref / 4)
      cb_labels = to_vec(for (t in cb_ticks) prettyNum(t, big.mark = ","))
      
      } else if (input$mytabs == 2) {
      ref <- max(fill_df()$ec_scaled)
      cb_limits <- c(0.75, ref)
      cb_ticks <- c(1, 1.5, 2, 2.5, 3, 3.5, 4)
      cb_labels <- c(1, 1.5, 2, 2.5, 3, 3.5, 4)
      
      } else {
        return()
      }
    
    return(list(cb_limits,
                cb_ticks,
                cb_labels))
  })
  

  
  map <- reactive({
    
    #Projection.
    g <- list(scope = 'usa',
              projection = list(type = 'albers usa'),
              showlakes = TRUE,
              lakecolor = toRGB('white'))
    
    #Initialize
    map <- plot_geo(fill_df(), locationmode = 'USA-states')
    
    if (input$mytabs %in% c(1,2)) {
      
      #Values by State.
      if (input$mytabs == 1) {
        
        fill_index <- length(integer(input$color_map))
        
        #Choropleth.
        map <- map %>% add_trace(z = fill_df()[, fill_index],
                                 text = ~hover,
                                 hoverinfo = 'text',
                                 locations = ~abb,
                                 color = fill_df()[, fill_index],
                                 colors = 'viridis')
        
        metric_list <- strsplit(pop_list[fill_index], " ")[[1]]
        
        colorbar_text <- paste("<b>", metric_list[1], "<br>", metric_list[2], "</b>", sep = "")
        
        title_text <- paste(input$elec_yr, 'Election Data for each State <br> (Hover for Information)')
        
      #Electoral College Weight per State.
      } else {
        
        fill_index <- length(integer(input$color_map2))
        
        #Choropleth.
        map <- map %>% add_trace(z = ~ec_scaled,
                                 text = ~hover,
                                 hoverinfo = 'text',
                                 locations = ~abb,
                                 color = ~ec_scaled,
                                 colors = 'viridis')
        
        colorbar_text <- paste("<b>Relative EC<br>Weight to ",
                               fill_df()$state[fill_df()$ec_ratio == max(fill_df()$ec_ratio)],
                               "</b>",
                               sep = "")
        
        title_text <- paste(input$elec_yr, 'Electoral College Weight by State <br> (Hover for Information)')
      }
      map <- map %>% colorbar(title = list(text = colorbar_text),
                              limits = colorbar_info()[[1]],
                              tickmode = 'array',
                              tickvals = colorbar_info()[[2]],
                              ticktext = colorbar_info()[[3]],
                              titlefont = list(size = 16),
                              tickfont = list(size = 12))
    
    } else {
      #Counterintuitive State Comparisons.
      if (input$mytabs == 3) {
        
        if (fill_df()$aff[1] == "Democrat") {
          colors <- c("white", "white", "red", "blue")
          other_party <- "Republican" 
          } else {
            colors <- c("white", "white", "blue", "red")
            other_party <- "Democrat" 
          }
        
        map <- map %>% add_trace(z = ~filling,
                                 text = ~hover,
                                 hoverinfo = 'text',
                                 locations = ~abb,
                                 color = ~filling,
                                 colors = colors)
        
        title_text <- with(fill_df(),
                           paste(paste(state[1], "'s", sep = ""),
                                 other_party, "Votes in", year,
                                 "<br> (Hover for Information)"))
        
        #States to Sum.
        } else {
      
          map <- map %>% add_trace(z = ~f,
                                   text = NULL,
                                   hoverinfo = 'text',
                                   locations = ~abb,
                                   color = ~f,
                                   colors = c("white", "white", "darkgreen"))
          
          title_text <- paste("Sum of Population of Selected States")
        }
      
        map <- map %>% hide_colorbar()
    }
    
    map <- map %>% layout(title = list(text = title_text, y = 0.965),
                          font = list(size = 18),
                          geo = g)
    map    
  })
  
  
  
  states_to_sum <- function(df, metric, column) {
    
    metric_states <- sum(df[df$f == 2,][, column])
    metric_us <- sum(df[, column])
    
    percent <- paste(round(100 * metric_states / metric_us, 2), "%", sep = "")
    
    paste("The", metric, "of the selected state(s) was <b>", percent, "of the US's",
          paste(metric, "</b>, or", sep = ""), prettyNum(metric_states, big.mark = ","),
          "out of", paste(prettyNum(metric_us, big.mark = ","), ".", sep = ""))
  }
  
  

  ##############Values by State##############
  output$tab1_title <- renderText("Values by State")

  output$pop_plot <- renderPlotly(map())

  output$tab1_summary <- renderText(
    "This tab was created to provide raw numbers."
  )
    
  
  
  ##############Electoral College Weight by State##############
  output$tab2_title <- renderText("Electoral College Weight by State")
  
  output$vote_w_plot <- renderPlotly(map())

  output$tab2_summary <- renderText(
    "This tab was created to divide and show how 2 senators per state drastically increases the weight of the\nElectoral College Vote in the lesser populated states."
  )
  
  
  
  ##############Counterintuitive State Comparisons##############
  output$tab3_title <- renderText("Counterintuitive State Comparisons")
  
  output$comparisons_plot <- renderPlotly(map())
  
  output$tab3_faithless <- renderUI({
    
    #Helper Text.
    faithless_link <- function(text) {
      
      year <- input$elec_yr
      
      create_html(paste(text, "  See", sep = ""),
                  paste(year, "Presidential Election"),
                  paste("https://www.270towin.com/", year, "_Election/", sep = ""),
                  "for more information.\n\n")
    }
    
    if ((input$elec_yr == 2000) & (input$state_select1 =="District of Columbia")) {
      
      faithless <- faithless_link("Of Washington DC's 3 Electoral College Votes, 1 was not cast.")
      
    } else if ((input$elec_yr == 2004) & (input$state_select1 =="Minnesota")) {
      
      faithless <- faithless_link("Of Minnesota's 10 Electoral College Votes, 1 was cast for John Edwards.")
      
    } else if ((input$elec_yr == 2016) & (input$state_select1 %in% c("Hawaii", "Texas", "Washington"))) {
      
      if (input$state_select1 =="Hawaii") {
        
        faithless <- faithless_link("Of Hawaii's 4 Electoral College Votes, 1 was cast for Bernie Sanders.")
        
      } else if (input$state_select1 == "Texas") {
        
        faithless <- faithless_link("Of Texas's 38 Electoral College Votes, 1 was cast for Jon Kasich and 1 was cast for Ron Paul.")
        
      } else {
        
        faithless <- faithless_link("Of Washington's 12 Electoral College Votes, 3 were cast for Colin Powell and 1 was cast for Faith Spotted Eagle.")
      }
    }
  })
  
  output$tab3_summary <- renderText({
    
    year <- input$elec_yr
    
    #Tab.
    tab <- "This tab was created to show how popular votes are ignored because of their location.\n\n"
    
    #Votes ignored.
    
    #Helper.
    ignored <- function(year) {
      
      ignored_text <- function(df, party) {
        
        other_party <- get_other_party(party)
        
        ignored_votes <- prettyNum((df %>%
                                      filter(aff == party) %>%
                                      select(other_party) %>%
                                      sum),
                                   big.mark = ",")
        
        paste("In", paste(year, ", the", sep = ""), other_party, "party received", ignored_votes, "votes from states won by", paste(party, "s,", sep = ""), "which the Electoral College ignored.\n\n")
        }

      #Exceptions.
      if (year == 2008) {
        #Modify df.
        ignored_df <- fill_df() %>% 
          filter(state != "Nebraska")
        
      } else if (year == 2016) {
        #Modify df.
        ignored_df <- fill_df() %>% 
          filter(state != "Nebraska")
        
      } else if (year == 2020) {
        #Modify df.
        
      } else {
        ignored_df <- fill_df()
      }  
      
      ignored_rep <- ignored_text(ignored_df, "Democrat")  
      ignored_dem <- ignored_text(ignored_df, "Republican") 
      
      paste(ignored_rep, ignored_dem, sep = "")
    }
    
    
    
    #Reform Party.
    
    #Helper.
    reform_text <- function(year, votes) {
      paste("In", year, "the Reform Party recieved", prettyNum(votes, big.mark = ","), "popular votes across the US, which the Electoral College ignored.")
    }
    
    if (year == 1992) {
      reform <- reform_text(year, 19743821)
    
    } else if (year == 1996) {
      reform <- reform_text(year, 8085294)
      
    } else {
      reform <- ""
    }
    
    #Full text.
    paste(tab, ignored(year), reform, sep = "")
  })
  
  
  
  ##############States to Sum##############
  output$tab4_title <- renderText("States to Sum")
  
  output$tab4_text1 <- renderText({
    "In 2016:"
  })
  
  output$tab4_text2 <- renderText({
    
    total_text <- states_to_sum(fill_df(),
                                pop_list[length(integer(input$pop_metric))],
                                1)
    
    rep_text <- states_to_sum(fill_df(),
                              "Republican Vote",
                              4)
    
    dem_text <- states_to_sum(fill_df(),
                              "Democratic Vote",
                              5)
    
    paste(total_text, rep_text, dem_text, sep = "<br/><br/>")
  })

  
  output$sum_plot <- renderPlotly(map())
  
  output$tab4_summary <- renderText(paste("A common argument I have seen is something along the lines of:",
                                          "\n'But if we get rid of the Electoral College, California and New York will decide the president!'.",
                                          "\n\nThis is simply a silly, exaggerated claim, with absolutely no numerical basis from anytime in history.",
                                          "\n\nThis tab was created to show the percentage of a metric from user selected states to the US total."))
  
  
  
  ##############MORE MENU############## 

  #General Information.
  output$tab9_title <- renderText("General Information")
  
  output$tab9_text1 <- renderText("Thank you for visiting my page!")

  output$tab9_text2 <- renderText("I have never accepted the Electoral College.  It is an outdated system that undermines the popular vote.  This was extremely apparent in 2000 when Gore won the popular vote, but Bush won the Electoral College.")

  output$tab9_text3 <- renderText("Recently, it was even more appalling in 2016 when Clinton won the popular vote, but Trump won the Electoral College.")

  output$tab9_links1 <- renderUI({
    create_html("It should be deeply concerning to citizens that in 2016, the president lost by 2.8 million votes, and in 2020, won by 7 million votes, yet",
                "the Electoral College result was the same numerical split",
                "https://www.cbsnews.com/news/presidential-election-results-2020-electoral-college-same-2016/",
                ".")})
  
  output$tab9_text4 <- renderText("I created this interactive to exemplify flaws with a few metrics and calculations from recent elections.  Are some votes worth more because of their location?  Are some votes worth nothing at all?  Unfortunately, the answer to both these questions is currently 'Yes'.")

  output$tab9_links2 <- renderUI({
    tagList(create_html("Maine and Nebraska are slightly different from the other states.  This made a numerical difference for Nebraska in 2008, Maine in 2016, and both in 2020.  See",
                       "FairVote",
                       "https://www.fairvote.org/maine_nebraska",
                       "and"),
            create_html(" ",
                       "270 to Win",
                       "https://www.270towin.com/content/split-electoral-votes-maine-and-nebraska/",
                       "for more information."))})
  
  
  output$tab9_links3 <- renderUI({
    create_html("Source Code can be found here at my",
                "GitHub Repository",
                "https://github.com/HumanRickshaw/Electoral_College",
                ".")
  })
  
  output$tab9_email1 <- renderText({
    "Questions?  Comments?"
  })
  
  output$tab9_email2 <- renderText({
    "Email : rohan.lewis@gmail.com"
  })
  
  
  #Sources.
  output$tab10_title <- renderText("Sources")
    
  output$tab10_links1 <- renderUI({
    create_html("State, DC, and USA Population: ",
                "Population and Housing Unit Estimation Tables (United States Census Bureau)",
                "https://www.census.gov/programs-surveys/popest/data/tables.html",
                ".")
    })
  
  output$tab10_links2 <- renderUI({
    create_html("State, DC, and USA Registration: ",
                "Voting and Registration Tables (United States Census Bureau)",
                "https://www.census.gov/topics/public-sector/voting/data/tables.All.html",
                ".")
    })
  
  output$tab10_links3 <- renderUI({
    create_html("State, DC, and USA Election Results: ",
                "Election and voting information (Federal Election Commision)",
                "https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/",
                ".")
    })
  
  
  
  #Related Links.
  output$tab11_title <- renderText("Related Links")
  
  output$tab11_link1 <- renderUI({
    create_html("MSNBC's Chris Hayes breaks down some numbers and comparisons of the 2020 election in this",
                "video",
                "https://www.youtube.com/watch?v=XtYA2p8G3rg",
                ".")
  })
  
  output$tab11_link2 <- renderUI({
    create_html("XKCD's beautiful and informative",
                "2020 Election Map",
                "https://xkcd.com/2399/",
                ".")
  })
  
  output$tab11_link3 <- renderUI({
    tagList(create_html("Shoutout to",
                        "National Popular Vote",
                        "http://www.nationalpopularvote.com/",
                        ""),
            create_html("on",
                        "Facebook",
                        "https://www.facebook.com/nationalpopularvoteinc",
                        "for running a Nonprofit organization and providing me a platform for some social media publicity."))})
}



# Run the application 
shinyApp(ui = ui, server = server)