#ITEC-610 - Project 1: Descriptive Data Analysis
#Project_1 Conrad Linus Muhirwe 
          #Learnmore Mawisire
#Date 9-September-2024

setwd("C:/Users/lm4687a/OneDrive - american.edu/American University Policies/Fall 2024 Class Materials/ITEC-610-002 - Applied Managerial Statistics - 2024FALL/Projects/Project 1")
FIFA <- read.csv("FIFA_Data.csv")

library(car)
library(dplyr)
library(summarytools)
library(ggplot2)
install.packages("e1071")
library(e1071)
install.packages("forcats")
library(forcats)
install.packages("psych")
library(psych)

#Subsetting by Football club
FIFA_Sub <- FIFA |>
filter(Nationality=="Spain" |
      Nationality=="England" |
      Nationality=="Germany" |
      Nationality=="France" |
      Nationality=="Argentina") |>
select(Nationality,Potential,Best.Overall.Rating,Age,Preferred.Foot)

#Checking for Missing Values in R
colSums(is.na(FIFA_Sub))

#Summary Statistics for numeric the variables
#Summary Stats for Potential, Best Overall Rating and Age
round(length(FIFA_Sub$Potential),1)
round(sd(FIFA_Sub$Potential),1)
round(length(FIFA_Sub$Best.Overall.Rating),1)
round(sd(FIFA_Sub$Best.Overall.Rating),1)
round(length(FIFA_Sub$Age),1)
round(sd(FIFA_Sub$Age),1)

#Skewness
#descr function adopted from GPT 4.0 (R-Wizard)
summary_stats <- FIFA_Sub %>% select(Potential,Age, Best.Overall.Rating) %>%
descr()
round(summary_stats,1)

#Summary stats for categorical variables
freq(FIFA_Sub$Nationality)
freq(FIFA_Sub$Preferred.Foot)


#7(a)	Perform at least 6 bivariate descriptive analyses.
# Correlation and scatter plot between Potential and Best Overall Rating
# Calculate correlation
correlation <- cor(FIFA_Sub$Potential, FIFA_Sub$Best.Overall.Rating, use = "complete.obs")
correlation

# Create scatter plot with linear regression line and correlation annotation
#Figure 1 (Code adopted from https://r-graph-gallery.com/ and modidied in Chat GPT 4.0 R Wizard)
ggplot(FIFA_Sub, aes(x = Potential, y = Best.Overall.Rating)) +
geom_point(color = "#000066", size = 2) +
geom_smooth(method = "lm", color = "red") + 
labs(title = "Figure 1:             Potential vs Best Overall Rating", 
     x = "Player Potential", 
     y = "Best Overall Rating") +
annotate("text", x = 60, y = 90, label = paste("Correlation: ", round(correlation, 1)), color = "black", size = 6, hjust = 1) +  
theme(plot.title = element_text(size = 16, hjust = 0),  
      axis.text.x = element_text(size = 16, color = "black"), 
      axis.text.y = element_text(size = 16, color = "black"),  
      axis.title = element_text(size = 16))


# Correlation between Age and Potential
#Figure 2 (Code adopted from https://r-graph-gallery.com/ and modidied in Chat GPT 4.0 R Wizard)
correlation_age_potential <- cor(FIFA_Sub$Age, FIFA_Sub$Potential, use = "complete.obs")
print(correlation_age_potential)
ggplot(FIFA_Sub, aes(x = Age, y = Potential)) +
geom_point() +
geom_smooth(method = "lm") +
labs(title = "Figure 2:         Players Age vs Players Potential", x = "Age", y = "Potential") +
theme(plot.title = element_text(size = 16, hjust = 0),
    axis.text.x = element_text(size = 16, color = "black", angle = 360, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16))


#Preferred Foot vs. Best Overall Rating
#Figure 3 (Code adopted from https://r-graph-gallery.com/ and modidied in Chat GPT 4.0 R Wizard)
ggplot(FIFA_Sub, aes(x = Preferred.Foot, y = Best.Overall.Rating, fill = Preferred.Foot)) +
geom_boxplot() +
scale_fill_manual(values = c("Left" = "#196f3d", "Right" = "#1f618d")) +
labs(title = "Figure 3:     Best Overall Rating Compared to Preferred Foot", 
    x = "Preferred Foot", 
    y = "Best Overall Rating") +
theme(plot.title = element_text(size = 16, hjust = 0),  
    axis.text.x = element_text(size = 16, color = "black"),  
    axis.text.y = element_text(size = 16, color = "black"),  
    axis.title = element_text(size = 16),  
    legend.position = "none")

# Age and Best overall rating
#Figure 4 (Code adopted from https://r-graph-gallery.com/ and modidied in Chat GPT 4.0 R Wizard)
ggplot(FIFA_Sub, aes(x = Age, y = Best.Overall.Rating))+
geom_point()+
geom_smooth(method = "lm")+
labs(title = "Figure 4:          Players Age vs Best Overall Rating", x = "Age", y = "Best Overall Rating") +
theme(plot.title = element_text(size = 16, hjust = 0),
      axis.text.x = element_text(size = 16, color = "black", angle = 360, hjust = 0.5, vjust = 0.5), 
      axis.text.y = element_text(size = 16, color = "black"),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16))

#Nationality vs. Preferred Foot
# Figure 5 (Code adopted from https://r-graph-gallery.com/ and modidied in Chat GPT 4.0 R Wizard)
ggplot(FIFA_Sub, aes(x = Nationality, fill = Preferred.Foot)) +
geom_bar(position = "dodge") +
scale_fill_manual(values = c("Left" = "#000066", "Right" = "#196f3d")) +  
labs(title = "Figure 5:        Nationality by Preferred Foot", 
    x = "Nationality", 
    y = "Players Preferred Foot Count") +
theme_classic() +
theme(plot.title = element_text(size = 16, hjust = 0),
    axis.text.x = element_text(size = 16, color = "black", angle = 360, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size = 16, color = "black"),
    legend.title = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    legend.position = "right") + ylim(0, 1500)

#Nationality Vs Age
#Figure 6 (Code adopted from https://r-graph-gallery.com/ and modidied in Chat GPT 4.0 R Wizard)
FIFA_Sub %>% filter(!is.na(Age)) %>%
mutate(Nationality = fct_reorder(Nationality, Age, .fun = mean)) %>%
ggplot(aes(x = Nationality, y = Age, fill = Nationality)) + 
geom_boxplot() + 
xlab("Nationality") +
theme_classic() +
theme(legend.position = "none",
      axis.text.x = element_text(size = 16, color = "black", angle = 360, hjust = 0.5),  
      axis.text.y = element_text(size = 16, color = "black"),
      plot.title = element_text(size = 16, hjust = 0),
      axis.title.y = element_text(size = 16),
      axis.title.x = element_text(size = 16)) +
      labs(title = "Figure 6:        Age Distribution by Nationality")

#Trivariate Analysis
# Scatter plot of Best Overall Rating vs Age, Potential
#Figure 7 (Code adopted from https://r-graph-gallery.com/ and modidied in Chat GPT 4.0 R Wizard)
ggplot(FIFA_Sub, aes(x = Age, y = Best.Overall.Rating, color = Potential)) +
geom_point(alpha = 0.6) +  
labs(title = "Figure 7:           Best Overall Rating, Age and Potential Analysis",
    x = "Age",
    y = "Best Overall Rating",
    color = "Potential") +
theme_minimal() +
theme(plot.title = element_text(size = 16, hjust = 0),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.text.x = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 16), 
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 16)) 

#Trivariate table
triv <- FIFA_Sub %>% select(Age,Best.Overall.Rating,Potential) %>%
descr()
round(triv,1)

#END