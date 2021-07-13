library(shiny)
library(ggmap)
library(googleway)
#library(leaflet)

propertyData = read.csv("Property Data Samples.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(propertyData) = c(substring(colnames(propertyData)[1], 4, nchar(colnames(propertyData)[1])),colnames(propertyData)[2:length(colnames(propertyData))])
leaseComp = read.csv("Lease Comp Samples.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(leaseComp) = c(substring(colnames(leaseComp)[1], 4, nchar(colnames(leaseComp)[1])),colnames(leaseComp)[2:length(colnames(leaseComp))])
availabilityData = read.csv("Availability Data Samples.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(availabilityData) = c(substring(colnames(availabilityData)[1], 4, nchar(colnames(availabilityData )[1])),colnames(availabilityData )[2:length(colnames(availabilityData ))])
salesComp = read.csv("Sales Comp Samples.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(salesComp ) = c(substring(colnames(salesComp )[1], 4, nchar(colnames(salesComp )[1])),colnames(salesComp )[2:length(colnames(salesComp ))])

api_key = "AIzaSyA7oKoCXdWhKMXzPf1Ln9RGeFPSFLIi_UU"
register_google(key=api_key)

#bldg name, addr, city name
x = 1#length(propertyData[,4])
OrigAddress = geocode(paste(propertyData[1:x,4],propertyData[1:x,8],propertyData[1:x,9]))


#OrigAddress = propertyData[,4]
#for (i in 1:length(OrigAddress))
#{
#result  = geocode(OrigAddress, output="latlona",source="google")
#OrigAdress$lat[i] = as.numeric(result[1])
#OrigAdress$lon[i] = as.numeric(result[2])
#OrigAddress$geoAddress[i] = as.character(result[3])
#}
#write.csv(origAddress,"geocoded.csv", row.names=false)


ui <- fluidPage(theme = "bootstrap.css",
# App title ----
  tags$head(tags$style("#text1{font-size: 20px; font-style: italic;}")),
  titlePanel("PROPERTY PROPHET"),

  sidebarLayout(position = "right",
                    
                sidebarPanel(

                    selectInput(input = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Property", "Lease", "Sales","Availability")
                  ),
                    selectInput(input = "version",
                  label = "Choose a Display Version:",
                  choices = c("Listed", "Narrative")
                  )
                ),
                mainPanel(
                    fluidRow(
                    
                        column(5,
                        selectizeInput("prop", label = "Enter Property ID", choices = propertyData[,1],
                                  options = list(create = TRUE)),
                        #numericInput("prop", "Enter Property ID:", value = 25),
                       htmlOutput("text")
                   ),
                    br(),
                    google_mapOutput("map")
                )
    )
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
       output$map = renderGoogle_map({
              google_map(key = api_key, data=as.data.frame(OrigAddress), location=as.data.frame(OrigAddress[1,])) %>%
             add_markers()
       })

       output$text = renderText({
              writeMeSomeStuff()
       })
       writeMeSomeStuff = reactive({
              row = which(propertyData$PropertyID == input$prop)
              rowSales = which(salesComp$PropertyID == input$prop)

              if (length(row)==0) {
                    text = "Sorry, invalid PropertyID"
              }
              else if (!is.na(input$version) && input$version=="Narrative") {
                    text = paste(propertyData[row,11],"is located at", propertyData[row,4],",", 
       propertyData[row,8],",", propertyData[row,9], "in the", propertyData[row,15], 
       "Submarket of the", propertyData[row,14],"Market.") 
                    text = paste(text,"The property is a Class", propertyData[row,20],",", 
       propertyData[row,17],"building, built in",propertyData[row,83],", standing", propertyData[row,50], 
       "stories tall with a total size of", propertyData[row,24], "SQ ft. ")
                    text = paste(text,"The property most recently sold on",salesComp[rowSales,6],"for ",salesComp[rowSales,22],
       "from",salesComp[rowSales,34],"to the current owner of 601 W Companies.")
#The property is leased by Telos Group and managed by JLL. 
#Aon Center is well occupied, at 96% with the five largest active tenants being Aon with XXX,000 SQ ft, KPMG with XXX,000 SQ ft, JLL with XXX,000 SQ ft, Integrys Energy Group with XXX,000 SQ ft and Edelman Public Relations with XXX,000 SQ ft. 
#The largest available spaces in the building are on the 69th floor with 34,129 SQ ft of contiguous space, 15th floor with 31,710 SQ ft and 16th floor with 31,710 SQ ft. 
#Asking rents are roughly $XX SQ ft full service gross.
              } else {
                    text = ""
                    ourTable = switch(input$dataset,
                                  "Property"=propertyData,
                                  "Lease"=leaseComp, 
                                  "Sales"=salesComp,
                                  "Availability"=availabilityData)
                    for (i in seq(1:length(colnames(ourTable)))) {
                           # if useful data
                           #if (!is.na(ourTable[row,i]) && !(ourTable[row,i]=="")) {
                                  # if stastically significant
                                  if (is.numeric(ourTable[,i]) && !is.na(ourTable[row,i]) && !is.na(mean(ourTable[,i])) && !is.na(sd(ourTable[,i]))) {
                                         if ((ourTable[row,i]>mean(ourTable[,i])+0.5*sd(ourTable[,i])) | 
                                            `    (ourTable[row,i]<mean(ourTable[,i])-0.5*sd(ourTable[,i]))
                                             ) {
                                               text = paste(text, "<B><em>", (colnames(ourTable)[i]), ": ", as.character(ourTable[row,i]))
                                               text = paste(text, "</B></em>", sep="<br />")
                                         }
                                  }
                                  else {                                            
                                         text = paste(text, (colnames(ourTable)[i]), ": ", as.character(ourTable[row,i]))
                                         text = paste(text, " ", sep="<br />")
                                  }
                           #}
                    }
              }
              text
       })
}

# Create Shiny app ----
shinyApp(ui, server)




