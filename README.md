# ST558_Project_3  


## Purpose of the repo

This repository has been created with the purpose of analyzing data related to one of the most widespread chronic diseases in the United States, Diabetes. The Diabetes Health Indicators Dataset encompasses over 200,000 survey responses, offering valuable insights into how this disease affects the health of millions of Americans and its impact on both individuals and the nation's economy.  

The data set includes information on various aspects such as the current diabetes status of respondents, their demographics, physical and mental health metrics, habits, addictions, diet, and more. These data hold the potential to predict an individual's likelihood of having diabetes or being at a high risk of developing it, based on specific factors. Additionally, it aims to identify the key factors contributing to the risk of diabetes.  

Furthermore, this analysis will be presented separately based on the level of education, allowing us to examine how this demographic factor influences the overall findings.



## List of R packages

library(readr)  

library(dplyr)  

library(ggplot2)  

library(caret)  

library(rmarkdown)  

library(purrr)  

library(glmnet)  

library(randomForest)  

library(gbm)  

library(Metrics)  #For logLoss()  

library(cvms)  

library(rpart)  



## Automation code of different education level .md files 

lapply(unique(diabetes_data$Education), function(Education.i) {
  rmarkdown::render("Project3.Rmd",
                    params = list(Education = Education.i),
                    output_file = paste0(Education.i, ".md"))
})

## Links to .html files of the generated analyses

Analysis for [Elementary](https://jgally.github.io/ST558_Project_3/Elementary.html)  

Analysis for [Some High School](https://jgally.github.io/ST558_Project_3/Some_High_School.html)  

Analysis for [High School Graduate](https://jgally.github.io/ST558_Project_3/High_School_Graduate.html)  

Analysis for [Some Collage](https://jgally.github.io/ST558_Project_3/Some_College.html)  

Analysis for [College Graduate](https://jgally.github.io/ST558_Project_3/College_Graduate.html)  
