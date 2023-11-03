library(rmarkdown) #To load package

# Render the R Markdown file to README.md
rmarkdown::render("Project 3.Rmd", output_format = "github_document", output_file = "README.md")