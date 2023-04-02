#----------------------------ADAMU & ADETUNJI--------------------

library(ggplot2)
library(kableExtra)
library(plotly)
library(RColorBrewer)
library(reshape2)
library(tidyverse)
library(shiny)
library(DT)
load ("OECD skill job level 1.rdata")
load ("OECD skill job level 2.rdata")
# -----------------------DATA EXPLORATION& CLEANING-------------------------

#remove unwanted rows 
sjd1<- subset(sjd1, !(country %in% c("OECD", "European Union")))
sjd2<- subset(sjd2, !(country %in% c("OECD", "European Union")))

names(sjd1)
names(sjd2)

#rename columns for sjd1
names(sjd1)[1:3] <- c("Country", "AggregatedSkills", "SkillsIndicator")

#rename columns for sjd2
names(sjd2)[1:4] <- c("Country", "AggregatedSkills", "DisaggregatedSkills", "SkillsIndicator")

#mimimum and maximum value
df_value <- as.data.frame(sjd2$SkillsIndicator)
min(df_value)
max(df_value)

# Use ifelse to code values as shortage, surplus, or balanced
sjd2$SkillsSupply<- ifelse(sjd2$SkillsIndicator < -0.5, "shortage",
                          ifelse(sjd2$SkillsIndicator > 0.5, "surplus",
                              ifelse(sjd2$SkillsIndicator >= 0.1 & 
                                       sjd2$SkillsIndicator <= 0.5, "balanced", "shortage")))

#--------------------- EXPLANATORY ANALYSIS ------------------------------
#TABLE 1 - SUMMARY STATISTICS - SJD1
# Group the data by 'skill1' and calculate the mean, min., max., and sd values for each skill

skill_stat <- sjd1 %>%
  group_by(AggregatedSkills) %>%
  summarise(mean = round(mean(SkillsIndicator), 3),
            min = round(min(SkillsIndicator), 3),
            max = round(max(SkillsIndicator), 3),
            sd = round(sd(SkillsIndicator), 3))

skill_stat%>% kbl(caption = "Countries and Skills They Have in surplus") %>% 
     kable_styling(bootstrap_options = c("striped"),font_size = 12)

#BARCHART - SJD1
# Aggregate  average value for each skill1 and # Plot the results

plotdata <- aggregate(SkillsIndicator ~ AggregatedSkills, data = sjd1, FUN = mean)

ggplot(plotdata, aes(x = AggregatedSkills, y = SkillsIndicator)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs("Skill1 Distribution in OECD Countries", x = "Skill", y = "Supply Level")

#HEAT MAP -SJD1
data_wide <- dcast(sjd1, AggregatedSkills ~ Country, value.var = "SkillsIndicator")

# Convert the wide-format data frame to long format
data_long <- tidyr::gather(data_wide, key = "Country", value = "SkillsIndicator", -AggregatedSkills)


# Create a tile plot of value as a function of country and skill1

skill1_heat_map <- ggplot(data_long, aes(x = Country, y = AggregatedSkills, fill = SkillsIndicator)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Country", y = "Skill", fill = "Skill Imbalance")

# creating a plotly object with a hover function to display the values in the heatmap

ggplotly(skill1_heat_map, tooltip = c ("x", "y", "fill"),
         text = paste("Country:", sjd1$country, "<br>",
                      "Skill:", sjd1$AggregatedSkills, "<br>",
                      "Value:", sjd1$SkillsIndicator))

#-------------------------SJD2--------------------
#OTHER TABLES -SJD2
#subset to view skills that are surplus
surplus_data <- subset(sjd2, SkillsSupply == "surplus")
surplus_data%>% kbl(caption = "Countries and Skills They Have in surplus") %>% 
  kable_styling(bootstrap_options = c("striped"),font_size = 12)

#subset to view skills that have a shortage
shortage_data <- subset(sjd2, SkillsSupply == "shortage")
shortage_data %>% kbl(caption = "Countries and Skills They Have in Balance") %>% 
  kable_styling(bootstrap_options = c("striped"),font_size = 12)

#subset to view skills that have a shortage
shortage_data <- subset(sjd2, SkillsSupply == "balanced")
shortage_data %>% kbl(caption = "Countries and Skills They Have in Balance") %>% 
  kable_styling(bootstrap_options = c("striped"),font_size = 12)

#----COLUMN CHARTS-------
#Aggreagate the value for skill2 and plot the results 
avg_by_skill2 <- aggregate(SkillsIndicator ~ DisaggregatedSkills, data = sjd2, FUN = mean)
ggplot(avg_by_skill2, aes(x= SkillsIndicator, y = reorder(DisaggregatedSkills, -SkillsIndicator), fill = "color" )) +
  geom_col() +
  scale_fill_manual(values = c("#004494", "#00449490", "#00449450"))+
  theme (
    axis.text.x = element_text (color = "black", angle=90, hjust=0.9),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "none")+
  labs(x = "Skill", y = "Average Value", title = "Which Skills Have the Least Supply?",
       subtitle = "OECD Skills Inbalance in 2015",
       caption = "Source: OECD, 2015")

# Group the data by country and calculate the average value for each skill
country_avg <- aggregate(SkillsIndicator ~ Country, data = sjd2, FUN = mean)
country_avg
ggplot(country_avg, aes(x = SkillsIndicator, y = Country, fill = "color")) +
  geom_col() +
  scale_fill_manual(values = c("#004494", "#00449490", "#00449450"))+
  theme (
    axis.text.x = element_text (color = "black", angle=90, hjust=0.9),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "none")+
  labs(x = "Skill", y = "Average Value", title = "Which Countries Have the Most Skill Deficit?",
       subtitle = "OECD Skills Inbalance in 2015",
       caption = "Source: OECD, 2015")


# BOXPLOT
ggplot(sjd2, aes(x = AggregatedSkills, y = SkillsIndicator, fill = AggregatedSkills)) +
  geom_boxplot() +
  labs(x = "Skill", y = "Skill Imbalance", fill = "Skill") +
  scale_fill_manual(values = c("#A2C8EC", "blue","steelblue", "navyblue", "skyblue", "royalblue", "#BCBFCC")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Skill Imbalance by Aggregated Skills") + 
  theme_classic()

#SCATTERPLOT
# hierarchical clustering to group skills with generally similar levels of supply
skill_hierachy <- sjd2[, c("DisaggregatedSkills", "SkillsIndicator")]
set.seed(123)
kmeans_clusters <- kmeans(skill_hierachy$SkillsIndicator, centers = 4)
skill_hierachy$cluster <- kmeans_clusters$cluster
view(skill_hierachy)

#Here we attempt to group various skills into clusters based on their level of supply.

# create histogram of value variable
p <- ggplot(skill_hierachy, aes(x = SkillsIndicator, fill = factor(cluster))) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("black", "#004494", "#A2C8EC", "grey" )) +
  labs(x = "Level of supply", y = "Frequency") +
  theme_classic()

# add vertical lines to show cluster means
cluster_means <- aggregate(SkillsIndicator ~ cluster, skill_hierachy, mean)
p <- p + geom_vline(data = cluster_means, aes(xintercept = SkillsIndicator, color = factor(cluster)),
                    linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("black", "#004494", "#A2C8EC", "grey" )) 

p

# --------------------- France Focus ---------------

#Since we live in France, we will be focusing most of our analysis on France. 
#We want to see the general level of supply for aggregated  and disaggreated skills 
Skills_France <- sjd2 %>%
  filter(Country == "France") %>%
  mutate(color = case_when(
    SkillsSupply == "surplus" ~ 1,
    SkillsSupply == "balanced" ~ 2,
    SkillsSupply != "surplus" & SkillsSupply != "balanced" ~ 3
  ))
View(Skills_France)

ggplot (Skills_France, aes (x = SkillsIndicator, y = AggregatedSkills, fill = factor(color))) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "#A2C8EC")) +
  theme (
    axis.text.x = element_text (color = "black", angle=45, hjust=0.9),
    axis.title.x = element_blank(),
    axis.line = element_line(),
    panel.background = element_rect (fill = NA),
    panel.grid.major = element_line (colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "none") +
  labs (title = "Skill Availability in France",
        subtitle = "Technical Skills are the Least in Supply in France, 2015")

#..........................................
# Plot a column chart that allows you see the availabilty of social skill by country. 
#..........................................
ggplot(Skills_France, aes(x = SkillsIndicator, y = DisaggregatedSkills, fill = factor(color))) +
  geom_col(position = "stack", width = 1.0) +
  scale_fill_manual(values = c("#004494", "#0072C6", "#A2C8EC"),
                    labels = c("Skill Surplus", "Skill Balance", "Skill Shortage")) +
  theme(
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1.5),
    axis.title.x = element_blank(),
    axis.line = element_line(),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "#00000010"),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(lineheight = 5.0)) +
  labs(
    title = "Disaggregated Skill Supply in France",
    subtitle = "Workers and Different levels of Skill Supply, 2015",
    caption = "Source: OECD, 2015",
    fill = "Skills Availability",
    x = "Skills",
    y = "Level of Supply"
  )

# ----------------------- Shiny --------------------

# Create a list of unique countries from sjd2 dataframe
Country <- unique(sjd2$Country)

ui <- fluidPage(
  titlePanel("Skill Imbalance in OECD Countries"),
  
  # Create a tab showing the overview of selected countries
  tabsetPanel(
    tabPanel("Overview",
             fluidRow(
               column(2, selectInput("Country", "Choose the country", Country)),
               column(1),
               column(5, plotOutput(outputId = "averageskills")),
               column(4)
             ),
             fluidRow(
               column(3),
               column(5,dataTableOutput("tbl")),
               column(4)
             )
    ),
    # Create a tab showing the heatmap and the scatter plot of the aggregated skill level
    tabPanel("Skill Distribution",
             fluidRow(
               column(6, plotOutput(outputId = "SkillAvg")),
               column(6, plotOutput(outputId = "CountryAvg"))
             ),
             # Create a tab showing the skill imbalance distribution by aggregated skills
             fluidRow(
               column(3),
               column(6, plotOutput(outputId = "Boxplot")),
               column(3)
             )
             
    ),
    
    
    # Create a tab showing the general level of skills in France
    tabPanel("France",
             fluidRow(
               column(6, plotOutput(outputId = "SkillsFrance")),
               column(6, plotOutput(outputId = "SocialSkills"))
             )
             
    )
  ),
  
  fluidRow(
    column(2),
    column(10, "Source: OECD Skill Job Level, 2015")
  )
  
)


server <- function(input, output) {
  plotdata <- reactive({
    sjd2 %>% filter(Country == input$Country)
  })
  
  output$averageskills <- renderPlot({
    ggplot(plotdata(), aes(x = AggregatedSkills, y = SkillsIndicator)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Aggregated skill Distribution in OECD Countries", x = "Skill", y = "Supply Level")
  })

  output$SkillAvg <- renderPlot({
    ggplot(avg_by_skill2, aes(x= SkillsIndicator, y = reorder(DisaggregatedSkills, -SkillsIndicator), fill = "color" )) +
      geom_col() +
      scale_fill_manual(values = c("#004494", "#00449490", "#00449450"))+
      theme (
        axis.text.x = element_text (color = "black", angle=90, hjust=0.9),
        panel.background = element_rect (fill = NA),
        panel.grid.major = element_line (colour = "#00000010"),
        panel.grid.major.x = element_blank(),
        legend.position = "none")+
      labs(x = "Skill", y = "Average Value", title = "Which Skills Have the Least Supply?",
           subtitle = "OECD Skills Inbalance in 2015")
  })
  
  output$CountryAvg <- renderPlot({
    ggplot(country_avg, aes(x = SkillsIndicator, y = Country, fill = "color")) +
      geom_col() +
      scale_fill_manual(values = c("#004494", "#00449490", "#00449450"))+
      theme (
        axis.text.x = element_text (color = "black", angle=90, hjust=0.9),
        panel.background = element_rect (fill = NA),
        panel.grid.major = element_line (colour = "#00000010"),
        panel.grid.major.x = element_blank(),
        legend.position = "none")+
      labs(x = "Skill", y = "Average Value", title = "Which Countries Have the Most Skill Deficit?",
           subtitle = "OECD Skills Inbalance in 2015")
  })
  
  output$Heatmap <- renderPlotly({
    ggplotly(skill1_heat_map, data = data_long, tooltip = c("x", "y", "fill"),
             text = paste("Country:", data_long$Country, "<br>",
                          "Skill:", data_long$AggregatedSkills, "<br>",
                          "Value:", data_long$SkillsIndicator),
             hoverinfo = "text")
  })
  
  output$Boxplot <- renderPlot({
    ggplot(sjd2, aes(x = AggregatedSkills, y = SkillsIndicator, fill = AggregatedSkills)) +
      geom_boxplot() +
      labs(x = "Skill", y = "Skill Imbalance", fill = "Skill") +
      scale_fill_manual(values = c("#A2C8EC", "blue","steelblue", "navyblue", "skyblue", "royalblue", "#BCBFCC")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Skill Distribution by Aggregated Skills") + 
      theme_classic()
  })
  
  output$Cluster <- renderPlot({
    hierarchy <- ggplot(skill_hierachy, aes(x = SkillsIndicator, fill = factor(cluster))) +
      geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("black", "#004494", "#A2C8EC", "grey" )) +
      labs(x = "Level of supply", y = "Frequency", title = "Skill hierarchy and cluster means") +
      theme_classic()
    
    # add vertical lines to show cluster means
    cluster_means <- aggregate(SkillsIndicator ~ cluster, skill_hierachy, mean)
    hierarchy <- hierarchy + geom_vline(data = cluster_means, aes(xintercept = SkillsIndicator, color =
                                                                    factor(cluster)),
                                        linetype = "dashed", linewidth = 1) +
      scale_color_manual(values = c("black", "#004494", "#A2C8EC", "grey" ))
  })
  
  output$SkillsFrance <- renderPlot({
    ggplot (Skills_France, aes (x = SkillsIndicator, y = AggregatedSkills, fill = factor(color))) +
      geom_col() +
      scale_fill_manual(values = c("steelblue", "#A2C8EC")) +
      theme (
        axis.text.x = element_text (color = "black", angle=45, hjust=0.9),
        axis.title.x = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect (fill = NA),
        panel.grid.major = element_line (colour = "#00000010"),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
      labs (title = "Skill Availability in France",
            subtitle = "Technical Skills are the Least in Supply in France, 2015")
  })
  
  output$SocialSkills <- renderPlot({
    ggplot(Skills_France, aes(y = DisaggregatedSkills, x=SkillsIndicator, fill = factor(color))) +
      geom_col(position = "stack",  width = 1.0) +
      scale_fill_manual(values = c("#004494", "#0072C6", "#A2C8EC"),
                        labels = c("Skill Surplus", "Skill Balance", "Skill Shortage")) +
      theme(
        axis.text.x = element_text(color = "black", angle = 45, hjust = 1.5),
        axis.title.x = element_blank(),
        axis.line = element_line(),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "#00000010"),
        panel.grid.major.x = element_blank(),
        legend.position = "right",
        axis.text.y = element_text(lineheight = 5.0)) +
      labs(
        title = "Disaggregated Skill Supply in France",
        subtitle = "Workers and Different levels of Skill Supply, 2015",
        fill = "Skills Availability"
      )
  }, res = 96)
  
  # Render the data table
  output$tbl <- renderDataTable(datatable(
    plotdata()))
  
}

shinyApp(ui, server)
