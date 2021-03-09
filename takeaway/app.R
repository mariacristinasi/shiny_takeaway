
# The app should make use of commands in packages: tidyverse, shinythemes, plotly and shinyjs.

library(shiny)
library(datasets)

library(httr)
library(readxl)
GET("https://query.data.world/s/xzozlqhuagxyazzgc3avtgcaw2yqxk", write_disk(tf <- tempfile(fileext = ".xls")))
df <- read_excel(tf)
# https://data.world/us-doi-gov/24d428dc-97cb-4ef3-9bec-5dbd4f966f12

for (i in 1:ncol(df)){
    if (is.na(df[1,i])==0){
        names(df)[i]= df[1,i]
    }
}
df=df[-1,]

nas_columns <- apply(is.na(df), 2,sum)/nrow(df)
null_columns=vector()
a=0
for (i in 1:ncol(df)){
    if (nas_columns[i]>=0.9){
        a=a+1
        null_columns[a]=i
    }
}
df=df[,-null_columns]

nas_rows <- apply(is.na(df), 1, sum)
null_rows=vector()
a=0
for (i in 1:nrow(df)){
    if (nas_rows[i]/ncol(df)>=0.8){
        a=a+1
        null_rows[a]=i
    }
    else if(is.na(df[i,1])==1){
        df[i,1]=df[i-1,1]
    }
}
df=df[-null_rows,]

#We have 1 NA in manganese, we are assuming there are no manganese in this sample
df[is.na(df)] ="-"


runGitHub("shiny_takeaway","mariacristinasi")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

)

# Define server logic required to draw a histogram
server <- function(input, output) {


}



# Run the application 
shinyApp(ui = ui, server = server)
