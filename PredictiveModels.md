Project 3
================
Jasmine Gallaway and Keren Vivas
2023-10-30

# 1. Introduction section

The data analyzed in this report is derived from the Diabetes Health
Indicators Dataset, which was gathered through the Behavioral Risk
Factor Surveillance System (BRFSS) in the year 2015. The BRFSS is an
annual health-related telephone survey conducted by the Centers for
Disease Control and Prevention (CDC). This dataset encompasses a
substantial collection of over 200,000 survey responses, featuring 21
distinct variables or factors, along with a pivotal response variable
termed “Diabetes_binary.” This response variable comprises three
discrete levels:

0 = No diabetes or diabetes only during pregnancy. 1 = Prediabetes. 2 =
Established diabetes.

The dataset encompasses a diverse array of variables that cover a wide
spectrum of information, including the current diabetes status of the
survey respondents, various demographic details, physical and mental
health metrics, dietary habits, and lifestyle factors. The specific
variables selected for analysis in this report are as follows:

- Demographics: This category includes two categorical variables: “Age”
  and “Sex” of the respondents.
- Physical Conditions: These variables encompass “HighBP,” “HighChol,”
  and “HeartDiseaseorAttack.” These are categorical variables, taking
  values of 0 or 1, indicating the presence or absence of high blood
  pressure, high cholesterol, and a history of heart disease or heart
  attacks. Additionally, the “Body Mass Index (BMI)” is included as a
  continuous variable, with values ranging from 12 to 98, providing
  insights into the respondents’ body composition.
- Mental Conditions: The variable “MentHlth” is a continuous variable
  that quantifies the number of days in a month during which the
  respondent’s mental health was not good. It ranges from 1 to 30.
- Diet Conditions: Two categorical variables, “Fruits” and “Veggies,”
  denote whether respondents consume fruits and vegetables one or more
  times per day. These variables take values of 1 (indicating
  consumption) or 0 (indicating non-consumption).
- Habits: This category includes two categorical variables: “Smoker” and
  “PhysActivity.” “Smoker” indicates whether respondents have smoked at
  least 100 cigarettes in their lifetime (with values 1 for yes and 0
  for no), while “PhysActivity” assesses whether respondents have
  engaged in physical activity within the last month (with values 1 for
  yes and 0 for no).

The Exploratory Data Analysis (EDA) of the selected variables is
designed to offer a holistic understanding of the health and lifestyle
characteristics of the survey respondents. This preliminary phase of
analysis helps us uncover patterns within the data.

Subsequently, when we move to modeling the data, our objective is to
delve deeper and gain insights into the intricate ways these factors
impact an individual’s diabetes status. Our analysis will not only
reveal the influence of these factors but also quantify the individual
contributions of each factor to the risk of diabetes.

In the end, the entire analysis will be segmented based on the level of
education. This segmentation enables us to investigate how this specific
demographic factor influences the overall findings. By doing so, we can
discern whether educational levels play a significant role in shaping
the relationship between health, lifestyle, and diabetes risk.

# 2. Required packages section

``` r
library(readr)  
library(dplyr)  
library(ggplot2)  
library(caret) 
library(rmarkdown)
library(shiny)
library(purrr)  
library(randomForest)  
library(gbm)  
library(MLmetrics)  #For logLoss()
```

# 3. Data section

``` r
#readin data  
diabetes_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")  
```

    ## Rows: 253680 Columns: 22
    ## ── Column specification ────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (22): Diabetes_binary, HighBP, HighChol, CholCheck, BMI, Smoker, Stroke, HeartDiseaseorAttack, PhysActiv...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#making a tibble
diabetes_data <- as_tibble(diabetes_data)  

#changing diabetes_binary to factor  
diabetes_data$Diabetes_binary <- diabetes_data$Diabetes_binary %>% 
  factor(levels = c(0,1,2),
         labels = c("No Diabetes", "Prediabetes", "Diabetes"))  

#combine education levels one and two  
diabetes_data$Education <- diabetes_data$Education %>% 
  factor(levels = c(1,2,3,4,5,6), 
            labels = c("None/Elementary", "None/Elementary", "Some High School", "High School Graduate", "Some College", "College Graduate"))  

#changing sex to factor  
diabetes_data$Sex <- diabetes_data$Sex %>% 
  factor(levels = c(0,1),
         labels = c("female", "male"))  

#changing age to factor  
diabetes_data$Age <- diabetes_data$Age %>% 
  factor(levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
         labels = c("18-24", "25-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "61-65", "66-70", "71-75", "76-80", "80+"))  

#Changing fruit to factor  
diabetes_data$Fruits <- diabetes_data$Fruits %>% 
  factor(levels = c(0, 1),
         labels = c("None", "Yes 1 or more"))  

#Changing physical activity to factor  
diabetes_data$PhysActivity <- diabetes_data$PhysActivity %>% 
  factor(levels = c(0, 1),
         labels = c("None", "Yes"))  

#Changing smoker to factor  
diabetes_data$Smoker <- diabetes_data$Smoker %>% 
  factor(levels = c(0, 1),
         labels = c("No", "Yes"))  

#Changing stroke to factor  
diabetes_data$Stroke <- diabetes_data$Stroke %>%
  factor(levels = c(0, 1),
         labels = c("No", "Yes"))  

#Changing heart disease or attack to factor  
diabetes_data$HeartDiseaseorAttack <- diabetes_data$HeartDiseaseorAttack %>% 
  factor(levels = c(0, 1),
         labels = c("No", "Yes"))  

#Changing veggies to factor  
diabetes_data$Veggies <- diabetes_data$Veggies %>% 
  factor(levels = c(0, 1),
         labels = c("None", "Yes 1 or more"))  

#Changing Heavy alcohol consumption to factor  
diabetes_data$HvyAlcoholConsump <- diabetes_data$HvyAlcoholConsump %>% 
  factor(levels = c(0, 1),
         labels = c("No", "Yes"))  

#Changing any healthcare to factor  
diabetes_data$AnyHealthcare <- diabetes_data$AnyHealthcare %>% 
  factor(levels = c(0, 1),
         labels = c("No", "Yes"))  

#Changing nodocbcCost to factor  
diabetes_data$NoDocbcCost <- diabetes_data$NoDocbcCost %>% 
  factor(levels = c(0, 1),
         labels = c("No", "Yes"))  

#Changing GenHlth to factor  
diabetes_data$GenHlth <- diabetes_data$GenHlth %>% 
  factor(levels = c(1, 2, 3, 4, 5),
         labels = c("Excellent", "Very Good", "Good", "Fair", "Poor"))  

#Changing DiffWalk to factor  
diabetes_data$DiffWalk <- diabetes_data$DiffWalk %>% 
  factor(levels = c(0, 1),
         labels = c("No", "Yes")) 

#changing HighBP to factor
diabetes_data$HighBP <- diabetes_data$HighBP %>%
  factor(levels = c(0, 1),
         labels = c("No high BP", "High BP"))

#changing HighChol to factor
diabetes_data$HighChol <- diabetes_data$HighChol %>%
  factor(levels = c(0,1),
         labels = c("No high cholesterol", "High cholesterol"))

#changing CholCheck to factor
diabetes_data$CholCheck <- diabetes_data$CholCheck %>%
  factor(levels = c(0,1),
         labels = c("No check in 5 years", "Check in 5 years"))

#changing Income to factor
diabetes_data$Income <- diabetes_data$Income %>%
  factor(levels = c(1,2,3,4,5,6,7,8),
         labels = c("Less than $10k", "$10k to less than $15k", "$15kto less than $20k", "$20k to less than $25k", "$25k to less than $35k", "$35k to less than $50k", "$50k to less than $75k", "$75k or more"))
```

``` r
# Subset data into a single Education level
subset_data <- diabetes_data %>%
 filter(Education == params$Education)
```

# 4. Summarizations section

``` r
#gathering summary statistics of BMI  
sum_data1 <- diabetes_data %>% 
  group_by(Diabetes_binary) %>%
  summarize(mean = mean(BMI),
            sd = sd(BMI),
            min = min(BMI),
            max = max(BMI),
            IQR = IQR(BMI))  
#printing out  
print(sum_data1)  
```

    ## # A tibble: 2 × 6
    ##   Diabetes_binary  mean    sd   min   max   IQR
    ##   <fct>           <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 No Diabetes      27.8  6.29    12    98     7
    ## 2 Prediabetes      31.9  7.36    13    98     8

``` r
#Summary statistics of mental health 
sum_data2 <- diabetes_data %>% 
  group_by(Diabetes_binary) %>% 
  summarize(mean = mean(MentHlth),
            sd = sd(MentHlth),
            min = min(MentHlth),
            max = max(MentHlth))  
print(sum_data2)
```

    ## # A tibble: 2 × 5
    ##   Diabetes_binary  mean    sd   min   max
    ##   <fct>           <dbl> <dbl> <dbl> <dbl>
    ## 1 No Diabetes      2.98  7.11     0    30
    ## 2 Prediabetes      4.46  8.95     0    30

``` r
 #1-way contingency table
 one_way_table <- table(diabetes_data$Fruits)
 one_way_table
```

    ## 
    ##          None Yes 1 or more 
    ##         92782        160898

``` r
 #2-way contingency table
 two_way_table <- table(diabetes_data$Smoker, diabetes_data$HighChol)
 two_way_table
```

    ##      
    ##       No high cholesterol High cholesterol
    ##   No                87033            54224
    ##   Yes               59056            53367

``` r
 #3-way contingency table
 three_way_table <- table(diabetes_data$HighBP, diabetes_data$HighChol, diabetes_data$HeartDiseaseorAttack)
 three_way_table  
```

    ## , ,  = No
    ## 
    ##             
    ##              No high cholesterol High cholesterol
    ##   No high BP               99044            39842
    ##   High BP                  39905            50996
    ## 
    ## , ,  = Yes
    ## 
    ##             
    ##              No high cholesterol High cholesterol
    ##   No high BP                2876             3089
    ##   High BP                   4264            13664

``` r
#4 Contingency table for diabetes_binary vs no doctors visits because of cost  
 tbl_four <- table(diabetes_data$NoDocbcCost, diabetes_data$Diabetes_binary)  
 tbl_four
```

    ##      
    ##       No Diabetes Prediabetes Diabetes
    ##   No       200722       31604        0
    ##   Yes       17612        3742        0

``` r
#5 Contingency table for diabetes_binary vs general health ratings  
 tbl_five <- table(diabetes_data$GenHlth, diabetes_data$Diabetes_binary)  
 tbl_five  
```

    ##            
    ##             No Diabetes Prediabetes Diabetes
    ##   Excellent       44159        1140        0
    ##   Very Good       82703        6381        0
    ##   Good            62189       13457        0
    ##   Fair            21780        9790        0
    ##   Poor             7503        4578        0

``` r
#6 Contingency table for diabetes_binary vs NoDocbcCost vs GenHlth  
 tbl_six <- table(diabetes_data$GenHlth, diabetes_data$Diabetes_binary, diabetes_data$NoDocbcCost)  
 tbl_six
```

    ## , ,  = No
    ## 
    ##            
    ##             No Diabetes Prediabetes Diabetes
    ##   Excellent       42402        1086        0
    ##   Very Good       78238        6089        0
    ##   Good            56214       12386        0
    ##   Fair            18137        8421        0
    ##   Poor             5731        3622        0
    ## 
    ## , ,  = Yes
    ## 
    ##            
    ##             No Diabetes Prediabetes Diabetes
    ##   Excellent        1757          54        0
    ##   Very Good        4465         292        0
    ##   Good             5975        1071        0
    ##   Fair             3643        1369        0
    ##   Poor             1772         956        0

``` r
#Violin plot
ggplot(diabetes_data, aes(x = Sex, y = MentHlth)) +
  geom_violin(fill = "lightblue", color = "blue", alpha = 0.7) +
  labs(title = "Mental health condition across gender", x = "Gender", y = "Mental health condition") +
  theme_minimal()
```

![](PredictiveModels_files/figure-gfm/Plots-1.png)<!-- -->

``` r
#Bar plot
ggplot(data = diabetes_data, aes(x = Age, y = PhysActivity , fill = Age)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Physical activity days by Age Group", x = "Age Group", y = "Physical activity (days)") +
  theme_minimal()
```

![](PredictiveModels_files/figure-gfm/Plots-2.png)<!-- -->

``` r
#Scatter plot
ggplot(diabetes_data, aes(x = BMI, y = MentHlth)) +
  geom_point(aes(color = Sex), size = 3) +
  labs(title = "Scatterplot of BMI vs. Mental health", x = "BMI", y = "MentHlth") +
  theme_minimal()
```

![](PredictiveModels_files/figure-gfm/Plots-3.png)<!-- -->

# 5. Modeling

Selected variables Sex, Income, Age, BMI, MentHlth, PhysHlth, HighBP,
Fruits, HvyAlcoholConsu, NoDocbcCost

``` r
set.seed(3033)
intrain <- createDataPartition(y = diabetes_data$Diabetes_binary, p= 0.7, list = FALSE)
```

    ## Warning in createDataPartition(y = diabetes_data$Diabetes_binary, p = 0.7, : Some classes have no records (
    ## Diabetes ) and these will be ignored

``` r
training <- diabetes_data[intrain,]
testing <- diabetes_data[-intrain,]
```

## *What log loss is and why we may prefer it to things like accuracy?*

Logarithmic loss is metric used to evaluate how well a model fits. Also
known as cross-entropy loss, log loss is used on binary classification
models where the responses are either 0 or 1. Log loss calculates the
difference between the predicted probabilities and the actual values.
The lower the log loss value, the better the fit of the model is to the
data. Log loss is preferred with this data set because of our focus on
the binary response of whether or not someone has diabetes. Log loss
focuses on models that predict the class better and penalizes models
that are further from the class.

## *What a logistic regression is and why we apply it to this kind of data?*

Logistic regression is a form of predictive analysis in which the
regression models the relationship(s) of a binary response variable to
one or more other variables. Logistic regression is similar to the use
of linear regression except that logistic regression is used for
classification problems, has a range of 0 to 1 like log loss, and does
not require a linear relationship between the variables.

## Fit 3 candidate logistic regression models and choose the best

\#\`\`\`{r Three candidate logistic models} \# Model 1: Logistic
Regression with default parameters \#model1 \<- glm(Diabetes_binary ~ .,
data = training, family = binomial) \#y_pred1 \<- predict(model1,
newdata = testing, type = “response”) \#log_loss_model1 \<- logLoss(obs
= testing\$Diabetes_binary, pred = y_pred1)

# Model 2: Logistic Regression with different parameters (e.g., regularization)

\#model2 \<- glm(Diabetes_binary ~ ., data = training, family =
binomial, control = glm.control(alpha = 0.1)) \#y_pred2 \<-
predict(model2, newdata = testing, type = “response”) \#log_loss_model2
\<- logLoss(obs = testing\$Diabetes_binary, pred = y_pred2)

# Model 3: Logistic Regression with more parameter variations

\#model3 \<- glm(Diabetes_binary ~ ., data = training, family =
binomial, control = glm.control(alpha = 1.0, weight = TRUE)) \#y_pred3
\<- predict(model3, newdata = testing, type = “response”)
\#log_loss_model3 \<- logLoss(obs = testing\$Diabetes_binary, pred =
y_pred3)

# Choose the model with the lowest log loss

\#log_loss_values \<- c(log_loss_model1, log_loss_model2,
log_loss_model3) \#best_model_index \<- which.min(log_loss_values)

\#best_model \<- switch( \# best_model_index, \# model1 = model1, \#
model2 = model2, \# model3 = model3 \#) \#best_log_loss \<-
log_loss_values\[best_model_index\] \#cat(“The best model is Model”,
best_model_index, “with a log loss of”, best_log_loss, “”) \#\`\`\`

## *What a LASSO logistic regression model is and why we might try to use this over basic logistic regresion?*

LASSO (Least Absolute Shrinkage and Selection Operator) is a type of
regularized logistic regression. It adds a penalty term to the logistic
regression’s likelihood function to encourage feature selection by
driving some coefficients to exactly zero. This can improve model
interpretability and reduce overfitting and that is why it is used over
the basic logistic regresion.

LASSO logistic regression is a valuable tool when you have
high-dimensional datasets with potentially redundant or irrelevant
features. It can improve model performance, reduce overfitting, and
enhance model interpretability by automatically selecting the most
relevant predictors while eliminating the least important ones. However,
the choice between basic logistic regression and LASSO logistic
regression depends on the specific characteristics of your data and the
goals of your analysis

## Fit a LASSO logistic regression model and choose the best model

\#model_lasso \<- train(Diabetes_binary ~ ., data = training_data,
method = “glmnet”, \# trControl = trainControl(method = “cv”, number =
5), \# tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, by =
0.001)))

## *what a classification tree model is and why we might try to use it?*

A classification tree is a decision tree that is used for classification
tasks. It recursively splits the data into subsets based on the
predictors and their values to make predictions. Different values of the
complexity parameter control the tree’s depth and complexity.
Cross-validation is typically used to select the best complexity
parameter.

## Fit a classification tree with varying values for the complexity parameter and choose the best model (best complexity parameter)

library(rpart)

# Fit a classification tree

\#tree_model \<- rpart(Diabetes_binary ~ ., data = training_data, method
= “class”)

# Define the control parameters for the rpart function

\#ctrl \<- rpart.control(cp = 0.01) \# Start with a small value for cp

# Fit the tree with cross-validation and find the best cp value

\#cv_tree_model \<- rpart(Diabetes_binary ~ ., data = training_data,
method = “class”, control = ctrl)

# Print the complexity parameter table

\#printcp(cv_tree_model)

\#Prune the tree \#best_tree_model \<- prune(cv_tree_model, cp =
best_cp_value)

## *what a random forest is and why we might use it instead of a basic classification tree?*

A random forest is an ensemble tree method of modeling. Random forest is
similar to the bagging method of modeling except that a random forest
does not use all the variables/predictors. A random forest uses a subset
of predictors which reduces variance better than bagging or a basic
classification tree does.

## Fit a random forest model and choose the best model

Add answer

## Two models thast was not done in class

Model 1: Add answer Model 2: Add answer

## What the Model 1 is?

Add answer

## Fit a Model 1 and choose the best model

Add answer

## What the Model 2 is?

Add answer

## Fit a Model 2 and choose the best model

Add answer

## 6. Final Model Selection (Compare the six best above and declare an overall winner)

Add answer

## NO SURE IN WHAT POINT WE SHOULD ADD THIS PIECE OF CODE :/

## X. Automation

``` r
#Do it for 1 first and then all level of education
#rmarkdown::render("Project 3.Rmd", out_put_file = "None_Elementary.html", params = list(Education = "None/Elementary"))

#For all level of education
#get unique education levels
#Education_levels <- unique(diabetes_data$Education)

#create filenames
#output_file <- paste0(Education_levels, ".html")

#create a list for each level of education with just the level name parameter
#params = lapply(Education_levels, FUN = function(x){list(Education = x)})

#Put into a data frame
#reports <- tibble(output_file, params)

# knit
#apply(reports, MARGIN = 1, FUN = function(x){ 
# render(input = "Project 3.Rmd", output_file = x[[1]], params = x[[2]])
#})
```

End report! :)
