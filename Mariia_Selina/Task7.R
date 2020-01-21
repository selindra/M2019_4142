library(shiny)
library(ggplot2)
df<-mtcars
df$cyl<-as.factor(df$cyl)
df$vs<-as.factor(df$vs)
df$am<-as.factor(df$am)
df$gear<-as.factor(df$gear)
df$carb<-as.factor(df$carb)
data<- df
# Define UI for application that draws a histogram
ui <- fluidPage(
sidebarLayout(
    sidebarPanel(
        textInput(inputId="title",
                  label="Plot title"),
        selectInput(inputId = "x",
                    label = "x-axis",
                    choices = c("mpg","disp","hp","drat","wt","qsec"),
                    selected = "mpg"),
        selectInput(inputId = "y",
                    label = "y-axis",
                    choices = c("mpg","disp","hp","drat","wt","qsec"),
                    selected = "disp"),
        selectInput(inputId = "m",
                    label = "m-axis",
                    choices = c("cyl","vs","am","gear","carb"),
                    selected = "cyl"),
        sliderInput(inputId = 'slider',
                    label = 'alpha',
                    min=0,max=1,
                    value=0.2),
        numericInput(inputId = "size",
                     label="Dot size:",
                     value=3,
                     min=1, max=9),
        checkboxInput(inputId="show_d",
                      label="Show data?",
                      value=FALSE),
        checkboxInput(inputId="show_s",
                      label="Show summary?",
                      value=FALSE),
        #submitButton("Apply Changes",
                     #icon = icon("check"))
    ),
        mainPanel(
            plotOutput(outputId = "scatter"),
            
        fluidRow(
            column(width = 5,
                   tableOutput(outputId = "data_selected")
            ),
            column(width = 5,
                   tableOutput(outputId = "summary"))
    

    ))))

    # Application title



# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter<-renderPlot({
        req(input$size)
        ggplot(data, aes_string(x=input$x,y=input$y, col=input$m))+
            geom_point(alpha=input$slider, size=input$size)+
            ggtitle(tools::toTitleCase(isolate(input$title)))
    })
    new_data <- reactive({
        data %>% select_(input$x, input$y, input$m)
    })
    output$data_selected<- renderTable({
        if (input$show_d){
            new_data()
        }
    })
    output$summary <- renderTable({
        if (input$show_s) {
            new_data() %>% 
                group_by_(input$m) %>% 
                summarise_all(funs(mean, sd))
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
