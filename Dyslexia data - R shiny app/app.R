#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggrepel)
library(tibble)
library(GGally)
library(ggcorrplot)
library(hrbrthemes)



#Read the data
dys_dt <- read.csv("dyslexia_desktop_data.csv", header = TRUE, sep=";")

#check and remove nans
is.na(dys_dt)
na.omit(dys_dt)

#Since there are a lot of variables/questions let's make a new dataframe with only accuracy scores to make it easier to work with
#Group the accuracy scores that covers the same linguistic domains
#make the new dataframe, keep the categorical variables and add the new accuracy scores

dys_dt_new <- subset(dys_dt, select=c("Age", 
                                      "Gender", 
                                      "Dyslexia"))

#Variables covering Alphabetic Awareness, Phonological Awareness and Visual discrimination and categorization
dys_dt_new$Q1_4_accuracy <- (dys_dt$Hits1 + 
                         dys_dt$Hits2 + 
                         dys_dt$Hits3 +
                           dys_dt$Hits4) / (dys_dt$Clicks1 + 
                                              dys_dt$Clicks2 + 
                                              dys_dt$Clicks3 +
                                              dys_dt$Clicks4)

#Variables covering Phonological Awareness, Syllabic Awareness and Auditory Discrimination and Categorization. 
dys_dt_new$Q5_9_accuracy <- (dys_dt$Hits5 +
                           dys_dt$Hits6 +
                           dys_dt$Hits7 +
                           dys_dt$Hits8 +
                           dys_dt$Hits9) / (dys_dt$Clicks5 +
                                              dys_dt$Clicks6 +
                                              dys_dt$Clicks7 +
                                              dys_dt$Clicks8 +
                                              dys_dt$Clicks9)

#Variables covering Lexical Awareness, Auditory Working Memory, and Auditory Discrimination and Categorization
dys_dt_new$Q10_13_accuracy <- (dys_dt$Hits10 +
                             dys_dt$Hits11 +
                             dys_dt$Hits12 +
                             dys_dt$Hits13) / ( dys_dt$Clicks10 +
                                                  dys_dt$Clicks11 +
                                                  dys_dt$Clicks12 +
                                                  dys_dt$Clicks13)

#Variables covering Visual Discrimination and Categorization, and Executive Functions
dys_dt_new$Q14_17_accuracy <- (dys_dt$Hits14 +
                             dys_dt$Hits15 +
                             dys_dt$Hits16 +
                             dys_dt$Hits17) / (dys_dt$Clicks14 +
                                                 dys_dt$Clicks15 +
                                                 dys_dt$Clicks16 +
                                                 dys_dt$Clicks17)



#Variables covering Visual Working Memory, Sequential Auditory Working Memory, and Auditory Discrimination and Categorization.
dys_dt_new$Q18_21_accuracy <- (dys_dt$Hits18 +
                             dys_dt$Hits19 +
                             dys_dt$Hits20 +
                             dys_dt$Hits21) / (dys_dt$Clicks18 +
                                                 dys_dt$Clicks19 +
                                                 dys_dt$Clicks20 +
                                                 dys_dt$Clicks21)

#Variables covering Lexical, Phonological, and Orthographic Awareness
dys_dt_new$Q22_23_accuracy <- (dys_dt$Hits22 + dys_dt$Hits23) / (dys_dt$Clicks22 + dys_dt$Clicks23)

dys_dt_new$Q24_26_accuracy <- (dys_dt$Hits24 + dys_dt$Hits25 + dys_dt$Hits26) / (dys_dt$Clicks24 + dys_dt$Clicks25 + dys_dt$Clicks26)


#Variables covering Syllabic, Lexical and Orthographic Awareness.
dys_dt_new$Q27_29_accuracy <- (dys_dt$Hits27 + dys_dt$Hits28 + dys_dt$Hits29) / (dys_dt$Clicks27 + dys_dt$Clicks28 + dys_dt$Clicks29)


#variables covering Lexical, Orthographic Awareness and Auditory Working Memory and Sequential Auditory Working Memory and Phonological Awareness.
dys_dt_new$Q30_32_accuracy <- (dys_dt$Hits30 +dys_dt$Hits31 + dys_dt$Hits32) / (dys_dt$Clicks30 + dys_dt$Clicks31 + dys_dt$Clicks32)

#Remove NaNs if any
dys_dt_new <- na.omit(dys_dt_new)

#Dataframe with just accuracy_values
accuracies_new <- subset(dys_dt_new[c(1,4:12)])


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #Title
  titlePanel("Exploring Dyslexia PLOS ONE data"),
  theme = shinytheme("sandstone"),
  
  #Create list panel at left side of screen
  navlistPanel("Introducing the data", widths = c(3,8),
    
    #first tab is all info about the dataset
    tabPanel("Dataset info", 
             p("A complete description of the methods and questions used to collect the data can be found", 
               a("here.", href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0241687#sec001.")),
             p("This dataset contains the results of a online gamified test targeted towards detection of dyslexia.
             The test contains questions/tasks involving auditory and visual discrimination and categorization of phonemes -sounds-, graphemes -letters-, syllables, words, pseudo-words, with increasing difficulty.
             Parts of the final tasks also target sequential visual and auditory working memory by asking the player to write sequences of letters in an specific order as well as words and pseudo-words."),
             strong("To quantify task performance, following dependent measures were used for each question:"),
             br(),
             em("(i) number of Clicks"),
             br(),
             em("(ii) number of correct answers (Hits)"),
             br(),
             em("(iii) number of incorrect answers (Misses)"),
             br(),
             em("(iv) Score defined as sum of Hits per set of exercises"),
             br(),
             em("(v) Accuracy, defined as the number of Hits divided by the number of Clicks"),
             br(),
             em("(vi) Missrate, defined as the number of Misses divided by the number of Clicks"),
             br(),
             br(),
             strong("Features from 1 to 4 correspond to demographic features:"),
             br(),
             em("Gender"),
             br(),
             em("Native language (whether the child is bilingual or has Spanish as native language"),
             br(),
             em("Language subject (whether the child has failed a language subject at school)"),
             br(),
             em("Age."), 
             br(),
             p("Features from 5 to 196 are the performance features (Hits, Clicks, Misses, Score, Accuracy, Missrate), derived from the participants performance while completing the different tasks of the test."),
             br(),
             h6("Note: I had nothing to do with the study, I just found the data set interesting. To visualise the data, I chose to only use the performance measure accuracy scores from the study - since this is the most telling performance measure when looking at how well the participants manage each task."),
             h6("To make it easier to analyze, I chose to group the accuracy scores according to the different linguistic domains."),
             h6("The tasks Q1-5 covers Alphabetic Awareness, Phonological Awareness and Visual discrimination and categorization, 
             Q5_9 covers Phonological Awareness, Syllabic Awareness and Auditory Discrimination and Categorization,
             Q10_13 covers Lexical Awareness, Auditory Working Memory, and Auditory Discrimination and Categorization, 
             Q14_17 covers Visual Discrimination and Categorization, and Executive Functions,
             Q18_21 covers Visual Working Memory, Sequential Auditory Working Memory, and Auditory Discrimination and Categorization,
             Q22_23 covers Lexical, Phonological, and Orthographic Awareness,
             Q24_26 covers Morphological, Syntactic and Semantic Awareness.
             Q27_29 covers Syllabic, Lexical and Orthographic Awareness,
             Q30_32 covers Lexical, Orthographic Awareness and Auditory Working Memory, and Sequential Auditory Working Memory."),
             h6("Of course this might not be the best approach, but this project is just me trying out different visualisations using R Shiny"),
             ),
    
    #this tab shows some of the participants data such as age etc...
    tabPanel("Participants - distributions of categorical variables",
             
             #make a selectinput for stat to show
             selectInput("choose_stat",
                         "Choose a statistic to view",
                         choices = c("Age", 
                                     "Gender", 
                                     "Language",
                                     "Dyslexia",
                                     "Gender/Age",
                                     "Dyslexia/Age",
                                     "Dyslexia/Gender"),
                         selected = "Age",
                         multiple = FALSE, width = "110%"),
             
             #plot the chosen stat
             plotOutput("plot_1", width = "100%")),
    
    #This panel is all about performance measures/task performance
    tabPanel("Distributions - Accuracy Scores on Tasks",
             #make a sliderinput to choose the age-range
             sliderInput("choose_age_1",
                         "Choose age range for shown violinplots:",
                         min=7, max=17,
                         value = c(7,8), ticks=TRUE,
                         width = "100%"),
             fluidRow(
               #Make a selectinput to choose which group to look at
               column(6, selectInput("choose_group",
                                     "Select a grouping variable:",
                                     c("Dyslexia", "Gender"))),
               
               #make a selectinput to choose the variable
               column(6, selectInput("choose_var", 
                                     "Select a variable for the plot",
                                     names(dys_dt_new[4:12]),
                                     selected="Q1_4_accuracy"))),
            
      #Plot a violin plot for the variable
             plotOutput("plot_2", width = "100%", height="500px"),
      p("The authors behind the studies use 'Accuracy-scores' defined as the number of Hits divided by the number of Clicks to quantify task performance on each question.")),
    
    tabPanel("Performance overview - medians by age",
             fluidRow(
               column(6, sliderInput("age_range", "Filter by Age Range",
                                     min = 7,
                                     max = 17,
                                     value = c(7,10),
                                     step = 1)),
               
               column(6, selectInput("var_choice",
                                     "Choose variable to show:",
                                     names(accuracies_new[2:10]),
                                     selected=c("Q1_4_accuracy"),
                                     multiple = FALSE))),
             plotOutput("age_group_plot", width = "100%", height="400px"),
             p("The authors behind the studies use 'Accuracy-scores' defined as the number of Hits divided by the number of Clicks to quantify task performance on each question.")),
  
    #Correlation-part:
    "Correlation Analysis",
    tabPanel("Correlation Matrix Plot",
             fluidRow(
               #selectInput to choose variables to show in the correlation matrice
               column(6, selectInput("choose_stat_3",
                                     "Choose variables to show correlations:",
                                     names(accuracies_new[2:10]),
                                     selected=c("Q1_4_accuracy","Q5_9_accuracy"),
                                     multiple = TRUE)),
             
               #sliderInput to choose age range
               column(6, sliderInput("choose_age",
                                    "Choose age range for shown correlations:",
                                    min=7, max=17,
                                    value = c(7,8), ticks=TRUE))),
               
             #Plot bar charts according to chosen variables
             plotOutput("plot_4", width = "100%", height="400px"),
             p("The authors behind the studies use 'Accuracy-scores' defined as the number of Hits divided by the number of Clicks to quantify task performance on each question.
               Here you can se the correlation between the different accuracy scores, using Pearsons coefficient")),
    
    #Tabpanel for scatterplots
    tabPanel("Scatterplots",
             
             fluidRow(
             #select variable for x-axis
             column(6, selectInput("scatterX",
                         "Choose variables for X-axis:",
                         names(accuracies_new[2:10]),
                         selected="Q1_4_accuracy",
                         multiple = FALSE)),
             
             #select variable for y-axis
             column(6, selectInput("scatterY",
                         "Choose variables for Y-axis:",
                         names(accuracies_new[2:10]),
                         selected="Q5_9_accuracy",
                         multiple = FALSE))),
             plotOutput("scatterplot"),
             p("Scatterplots showing the relationship between chosen variables coloured by the Dyslexia variable."))
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot_1 <- renderPlot({
    if (input$choose_stat == "Age"){
      ggplot(data = dys_dt_new, aes(x = Age)) +
        scale_x_continuous(breaks=7:17) +
        geom_bar(color="lightsalmon", fill="lightsalmon")+
        geom_text(aes(label = after_stat(count)), 
                  stat = "count", 
                  vjust = -0.2, 
                  size=5, 
                  colour = "black")}
        
    
    else if (input$choose_stat == "Gender"){
      ggplot(data = dys_dt, aes(x = Gender, fill = Gender)) +
        geom_bar() +
        scale_fill_brewer(palette="Set2") +
        geom_text(aes(label = after_stat(count)), 
                  stat = "count", 
                  vjust = -0.2, 
                  size=5, 
                  colour = "black")}
    
    else if (input$choose_stat == "Language"){
      ggplot(data = dys_dt, aes(x = Nativelang, fill= Nativelang)) +
        xlab("Has Spanish as native language") +
        geom_bar() +
        scale_fill_brewer(palette="Set2") +
        geom_text(aes(label = after_stat(count)),
                  stat = "count",
                  vjust = -0.2,
                  size=5,
                  colour = "black")}
    
    
    else if (input$choose_stat == "Dyslexia"){
      ggplot(data = dys_dt, aes(x = Dyslexia, fill = Dyslexia)) +
        geom_bar() +
        xlab("Has dyslexia diagnosis") +
        scale_fill_brewer(palette="Set2") +
        geom_text(aes(label = after_stat(count)),
                  stat = "count",
                  vjust = -0.2,
                  size=5,
                  colour = "black")}
    
    else if (input$choose_stat == "Gender/Age"){
      ggplot(data=dys_dt, aes(x=Age, fill=Gender)) +
        geom_bar(position = "dodge2", width=0.3) +
        scale_fill_brewer(palette="Set2") +
        scale_x_continuous(breaks=7:17) +
        geom_text_repel(aes(label = after_stat(count)),
                        stat = "count",
                        size=5,
                        colour = "black")}
    
    else if (input$choose_stat == "Dyslexia/Age"){
      ggplot(data=dys_dt, aes(x=Age, fill=Dyslexia)) + 
        geom_bar(position = "dodge2", width=0.3) +
        scale_fill_brewer(palette="Set2") +
        scale_x_continuous(breaks=7:17) +
        geom_text(aes(label = after_stat(count)),
                  vjust = -0.2,
                  stat = "count",
                  size=5,
                  colour = "black")}
    
    else if (input$choose_stat == "Dyslexia/Gender"){
      ggplot(data=dys_dt, aes(x=Dyslexia, fill=Gender)) + 
        geom_bar(position = "dodge2", width=0.5) +
        scale_fill_brewer(palette="Set2") +
        geom_text_repel(aes(label = after_stat(count)),
                        vjust = -0.2,
                        stat = "count",
                        size=5,
                        colour = "black")}
  })
    
  #Plot the violinplots for section
    output$plot_2 <- renderPlot({
      
      #filter the data by sliderinput
      filter_age_1 <- subset(dys_dt_new, Age >= input$choose_age_1[1] & Age <= input$choose_age_1[2])
      
      #make the violin plot by chosen variables from selectinputs
      ggplot(filter_age_1, aes(x = !!as.symbol(input$choose_group), y= !!as.symbol(input$choose_var), fill=!!as.symbol(input$choose_group))) +
        geom_violin(width=1, linewidth=0.2) +
        #theme_ipsum() +
        scale_fill_brewer(palette="Set2") +
        labs(title = paste("Violin plot of", input$choose_var, "by", input$choose_group),
             x=input$choose_group, 
             y=input$choose_var) +
        coord_flip()
      
      })
    
    #Bar chart for median accuracy scores
    output$age_group_plot <- renderPlot({
      # Filter data based on age range
      age_filter <- subset(accuracies_new, Age >= input$age_range[1] & Age <= input$age_range[2])
      
      # Group data by age and calculate mean accuracy for each age group
      median_data <- age_filter %>%
        group_by(Age) %>%
        summarise(median_value = median(!!as.symbol(input$var_choice)))
      
      # Create a bar chart
      ggplot(median_data, aes(x = factor(Age), y = median_value)) +
        geom_bar(stat = "identity", fill = "lightsalmon", color = "black") +
        labs(title = "Median Accuracy by Age Group",
             x = "Age Group", 
             y = "Median Accuracy") +
        geom_text(aes(label = signif((median_value),2)),
                  vjust = -0.2, 
                  size=5, 
                  colour = "black")
    })
    
    #This is the correlation parts
    #filter the data according to sliderinput for the correlation matrice
    filter_age <- reactive({
      subset(accuracies_new, Age >= input$choose_age[1] & Age <= input$choose_age[2])
    })
    
    #plot the correlations in the correlation matrice
    output$plot_4 <- renderPlot({
      validate(
        need(input$choose_stat_3, "Choose at least two variables")
      )
      selected_variables <- filter_age()[,input$choose_stat_3, drop = FALSE]
      
      #using ggcorr returns Pearson Correlation Coefficients using pairwise observations
      corr <- round(cor(selected_variables), 1)
      p.mat <- cor_pmat(selected_variables)
      ggcorrplot(corr, lab=TRUE,  type = "lower", colors=c("lightsalmon","white","orangered4"))
    })
    
    #Make the scatterplot
    output$scatterplot <- renderPlot({
      #make the scatter plot by chosen variables from selectinputs
      ggplot(dys_dt_new, aes(x = !!as.symbol(input$scatterX), y= !!as.symbol(input$scatterY))) +
        geom_point(aes(colour = factor(Dyslexia)))+
        scale_color_brewer(palette="Set2")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

