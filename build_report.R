# Set working directory
setwd("C:/Users/natha/OneDrive/Documents/Evans School/5 - Winter 2019/PUBPOL 582/ACS data/ACS R files/New stuff/ACS_Demographics")

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("index.Rmd")
markdownToHTML('index.md', 'index.html', options=c("use_xhml"))
system("pandoc -s index.html -o index.pdf")