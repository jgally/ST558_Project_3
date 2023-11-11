# ST558_Project_3  


# Purpose of the repo

#This repository has been created with the purpose of analyzing data related to one of the most widespread chronic diseases in the United States, Diabetes. The Diabetes Health Indicators Dataset encompasses over 200,000 survey responses, offering valuable insights into how this disease affects the health of millions of Americans and its impact on both individuals and the nation's economy.
#The data set includes information on various aspects such as the current diabetes status of respondents, their demographics, physical and mental health metrics, habits, addictions, diet, and more. These data hold the potential to predict an individual's likelihood of having diabetes or being at a high risk of developing it, based on specific factors. Additionally, it aims to identify the key factors contributing to the risk of diabetes. 
#Furthermore, this analysis will be presented separately based on the level of education, allowing us to examine how this demographic factor influences the overall findings.

# List of R packages

library(rmarkdown) To load package  

library(readr)     To read data into R  

library(dplyr)      To clean data  

library(ggplot2)   To plot graphs  

library(caret)     To create predictive models and test them  

library(shiny)    To automate the creation of 5 github_documents  

library(purr)    

library(glm)     

library(randomForest)

# Render the R Markdown file to README.md

rmarkdown::render("Project 3.Rmd", output_format = "github_document", output_file = "README.md")

# Links to .html files of the generated analyses

#Analysis for [None/Elementary](link)
#Analysis for [Some High School] (link)
#Analysis for [High School Graduate] (link)
#Analysis for [Some Collage] (link)
#Analysis for [College Graduate] (link)
