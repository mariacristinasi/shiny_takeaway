
# The app should make use of commands in packages: tidyverse, shinythemes, plotly and shinyjs.

library(shiny)
library(ggplot2)
library(magrittr)
library(httr)
library(readxl)
library(tidyr)
library(stringr)
library(tidyverse)
library(shinyjs)
library(plyr)
library(ggthemes)

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

#We have 1 NA and 3 "?", we are assuming these components are no present
df[is.na(df)] ="-"

b=0
for (i in 1:ncol(df)){
    for (j in 1:nrow(df)){
        if (df[j,i]=="?"){
            b=b+1
            df[j,i]="-"
        } 
    }
}

df$core=df$`Core #`
df$`Core #`=NULL

df$depth=df$"Depth (cm)"
df$"Depth (cm)"=NULL

for (i in 1:nrow(df)){
    if (df$'Grain Size'[i]=="silt v.f. sand"){
        df$'Grain Size'[i]="silt-v.f. sand"
    }
}

df= df %>% separate('Grain Size', c("grain1", "grain2"), "-")
levels(df$grain1) = c(levels(df$grain1),"f.sand", "v.f.sand", "silt", "clay")
levels(df$grain2) = c(levels(df$grain2),"f.sand", "v.f.sand", "silt", "clay")

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
levels_core=levels(df$core)

df[df=="v.f. sand"]="v.f.sand"
df[df=="v.f sand"]="v.f.sand"
revalue(df$grain1, c("f. sand"="f.sand"))


levels(df$grain2)=c(levels(df$grain2), "NA")
df$grain2[is.na(df$grain2)]<- "NA"

df_long <- gather(df, material, proportion, Quartz:Pteropods, factor_key=TRUE)
df_long[sapply(df_long, is.character)] <- lapply(df_long[sapply(df_long, is.character)], as.factor)

levels_material=levels(df_long$material)

#runGitHub("shiny_takeaway","mariacristinasi")!!!!!!!!!!!!!!

# Define UI for application that draws a histogram
ui <-fluidPage(titlePanel("Analysis of composition and grain size of rocks"),
     tabsetPanel(type = "tabs",

    # Filtering per core - write a text explaining clay, etc!!!!!!!!!
    tabPanel("Grain size",
             sidebarLayout(position="left",
                           sidebarPanel(
                               checkboxInput("all", label = h5("All cores"), value = TRUE),
                               selectInput("cores", label = h5("Select the core:"), 
                                                      choices = levels_core,
                                                      selected = 1),
                                downloadButton("report", "Generate report")),
                           mainPanel(plotly::plotlyOutput("cores_plot") 
                           )) 
    ),
    tabPanel("Composition per point", #Explain what components are organic and non-organic
             selectInput("cores2", label = h5("Select the core:"), 
                         choices = levels_core,
                         selected = 1),
             selectInput("depth_selection", label = h5("Select the depth (cm):"), 
                         choices = levels(as.factor(df_long$depth)),
                         selected = 1),
             plotOutput("compos_plot") 
    ),
    
    tabPanel("Materials pressence per depth", #Explain what components are organic and non-organic
             selectInput("mat", label = h5("Select a material:"), 
                         choices = levels_material,
                         selected = 1),
             plotOutput(outputId="compos_depth", click="point_click"),
             tableOutput("plot_point")
    ),
    useShinyjs()
    
))



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe(if(input$all) disable("cores") else enable("cores"))
    
    core_selected <- reactive({if(!input$all) {df %>% filter(core %in% input$cores)} else{df}})
    
    core_selected2 <- reactive({df %>% filter(core %in% input$cores2)})
    
    observeEvent(input$cores2,
            updateSelectInput(session, "depth_selection",
                         choices = df_long$depth[df_long$core == input$cores2]
      ))

    core_depth_selected <- reactive({df_long %>% filter(depth == input$depth_selection & core == input$cores2)})
    
    mat_selected <- reactive({df_long %>% filter(material %in% input$mat)})
    
    output$cores_plot <- plotly::renderPlotly(ggplot(core_selected(),
                             aes(x=grain1, y=depth, size=grain1)) + ylab("Depth (cm)") +
                          geom_point(colour = "brown4", shape=1) + theme(legend.position = "none") +
                          ggtitle("Grain size per depth") +  
                          geom_point(aes(x=grain2, y=depth, size=grain2), colour= "brown4", shape=1)+
                          xlim("f.sand", "v.f.sand", "silt", "clay") + ylim(250,0) +
                          scale_size_manual(values =c("NA"=2,"f.sand"=2, "v.f.sand"=3, "silt"=4, "clay"=5,
                                                      "f.sand"=2, "v.f.sand"=3, "silt"=4, "clay"=5)))

    output$compos_plot <- renderPlot(ggplot(core_depth_selected(),
                             aes(x=material, y=proportion))  +  theme(legend.position = "none") +
                          geom_bar(stat='identity', colour="gray27", aes(fill=material)) + 
                          scale_fill_manual(values=setNames(c("mistyrose", "lightsalmon1", "orange", "lightsteelblue4", "mediumturquoise", "khaki1", 
                                                                "gold", "mediumorchid1"),
                                                              c("Quartz", "Feldespar", "Dark Lithics", "Manganese", "Forams", 
                                                                "Sponge Spicules", "Carbonate Fragments", "Pteropods"))) +
                          xlim("Quartz", "Feldespar", "Dark Lithics", "Manganese", "Forams", 
                               "Sponge Spicules", "Carbonate Fragments", "Pteropods") + 
                          ylim("R", "R-P", "P", "P-C", "C", "C-A", "A"))
    
    output$compos_depth <- renderPlot(ggplot(mat_selected(),
                                            aes(x=depth, y=proportion)) + theme(legend.position = "none") +
                                         geom_point(aes(fill=material, colour=material, size=3)) + 
                                         scale_fill_manual(values=setNames(c("mistyrose", "lightsalmon1", "orange", "lightsteelblue4", "mediumturquoise", "khaki1", 
                                                                             "gold", "mediumorchid1"),
                                                                           c("Quartz", "Feldespar", "Dark Lithics", "Manganese", "Forams", 
                                                                             "Sponge Spicules", "Carbonate Fragments", "Pteropods"))) +
                                         scale_colour_manual(values=setNames(c("mistyrose", "lightsalmon1", "orange", "lightsteelblue4", "mediumturquoise", "khaki1", 
                                                                             "gold", "mediumorchid1"),
                                                                           c("Quartz", "Feldespar", "Dark Lithics", "Manganese", "Forams", 
                                                                             "Sponge Spicules", "Carbonate Fragments", "Pteropods"))) +
                                         xlim(0,250) + 
                                         ylim("R", "R-P", "P", "P-C", "C", "C-A", "A"))
    
    output$plot_point <- renderTable({
            nearPoints(mat_selected() %>% select(core, depth, proportion), 
                       input$point_click, maxpoints = 1)
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)
