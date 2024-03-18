#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(flextable)
library(GGally)
library(ggraph)
library(gutenbergr)
library(igraph)
library(Matrix)
library(network)
library(quanteda)
library(sna)
library(tidygraph)
library(tidyverse)
library(tm)
library(tibble)
library(readxl)

##


Links <- read_excel("//sciensano.be/fs/1441_RiskAsse_Employee/E13 FOOD EXPO/PROJECTS/Projects_EU/EU_HORIZON/Horizon2020_IMPTOX/MNP Cluster/CUSP-EXPOmet_position paper/Network/mindmap-uncertainties.xlsx",
                    sheet = "Links"
)

Links<-Links%>%
  data.frame()%>%
  rename(from=From)

levelcol1<-vector()
levelcol2<-vector()
namecol1<-vector()
for (i in 1:nrow(Links)) {
  if (i==1){
    levelcol1<-c(15)
    levelcol2<-c(14)
  }else{
    if(Links[i,1]==Links[i-1,1]){
      newval<-levelcol1[i-1]
      levelcol1<-c(levelcol1, newval) 
      
      levelcol2<-c(levelcol2, (newval-1) )
      
    }else{
      
      numrow<-which(Links[2] == Links[i,1])
      
      newval<-levelcol2[numrow]
      levelcol1<-c(levelcol1, newval)
      levelcol2<-c(levelcol2, (newval-1) )
      
    }
    
  }
  newname<-Links[i,1]
  namecol1<-c(namecol1, newname)
}

linkc1<-Links%>%
  cbind(data.frame(level=levelcol1))%>%
  select(from,level,Branche.group )%>%
  rename(id=from)

linkc2<-Links%>%
  cbind(data.frame(level=levelcol2))%>%
  select(to,level,Branche.group )%>%
  rename(id=to)

nodes<-linkc1%>%
  rbind(linkc2)%>%distinct()%>%arrange(desc(level))%>%
  filter(!id=="MNP exposure assessment" & Branche.group %in% c("Data","Exposure scenarios", "MNP definition","Methodology harmonisation","Purpose"))%>%
  rbind(data.frame(id="MNP exposure assessment",level=15,Branche.group="MNP exposure assessment"))%>%
  
  mutate(color.background=case_when(
    Branche.group=="MNP exposure assessment"~"black",
    Branche.group=="MNP definition" ~"#3aa7bd",
    Branche.group=="Exposure scenarios"~"#d624b0",
    Branche.group=="Purpose" ~"#75349e",
    Branche.group=="Data"~"#4cb1f5",
    Branche.group=="Methodology harmonisation"~"#bab5b9"
  ))

library("xlsx")

#write.xlsx(nodes, "Node.xlsx", sheetName = "Sheet1",col.names = TRUE, row.names = TRUE, append = FALSE)

Node_grouping <- read_excel("//sciensano.be/fs/1441_RiskAsse_Employee/E13 FOOD EXPO/PROJECTS/Projects_EU/EU_HORIZON/Horizon2020_IMPTOX/MNP Cluster/CUSP-EXPOmet_position paper/Network/Node.xlsx")




fct<-function(x){
  non_empty_values <- x[!is.na(x)]
  vec<-(paste(non_empty_values, collapse = ", "))
  vec[ vec == ""] <- NA
  return(vec)
}



# Apply the function row-wise to concatenate values
concatenated <- apply(Node_grouping[4:ncol(Node_grouping)], 1, fct)

Node_grouping<-Node_grouping%>%mutate(`uncertainty type`=concatenated)

library("visNetwork") 


# We can visualize the network right away - visNetwork() will accept 
# our node and link data frames (it needs node data with an 'id' column,
# and edge data with 'from' and 'to' columns).

#visNetwork(nodes, links)

vis.nodes <- nodes
vis.links <- Links
vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$id # Text on click
vis.nodes$label  <- vis.nodes$id # Node label
vis.nodes$size   <- exp(vis.nodes$level/4) # Node size
vis.nodes$borderWidth <- 2 # Node border width

# We can set the color for several elements of the nodes:
# "background" changes the node color, "border" changes the frame color;
# "highlight" sets the color on click, "hover" sets the color on mouseover.

# vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]

vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"


# visNetwork(vis.nodes, vis.links)%>%
#   visOptions(highlightNearest = list(enabled = T, degree = 10, hover = T,algorithm="hierarchical"),collapse=T)

Node_grouping<-Node_grouping%>% select(id,`uncertainty type` )

vis.nodes<-vis.nodes%>%
  left_join(Node_grouping, by="id")



#vis.nodes$grouping <- Node_grouping$Grouping

#vis.nodes$group<-vis.nodes$Branche.group





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MNP exposure assessment Mindmap"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
  
        sidebarPanel(width = 2,
            sliderInput("size",
                        "size",
                        min = 1,
                        max = 50,
                        value = 30),
       

          selectInput("Focus",
                      "focus",
                     choices=unique(vis.nodes$id)),
          
          selectInput("Group",
                      "Select uncertainties",
                      choices=unique(vis.nodes$`uncertainty type`))
        ),
        
  
        
        
        # Show a plot of the generated distribution
        mainPanel(
           visNetworkOutput("network")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
visnet<- reactive({
  
  vis.nodes<-vis.nodes%>%
    filter(`uncertainty type`==input$Group)

  
 visNetwork(vis.nodes, vis.links)

  })
  
    output$network<-renderVisNetwork({
    visnet()%>%
    visNodes()%>%
      #   visGroups(groupname = "MNP definition", color = "#d624b0") %>%
      #   visGroups(groupname = "Purpose", color = "#75349e") %>%
      #   visGroups(groupname = "Methodology harmonisation", color = "#bab5b9") %>%
      #   visGroups(groupname =  "Data", color = "#241f78") %>%
      #   visGroups(groupname =  "Exposure scenarios", color = "#3aa7bd") %>%
      #   visGroups(groupname =  "MNP exposure assessment", color = "#403e3f") %>%
      # visLegend(useGroups=T)%>%
      visOptions(highlightNearest = list(enabled = T, degree = 10, hover = F,algorithm="hierarchical"),collapse=F,selectedBy=list(variable=c("uncertainty type"), multiple=T),nodesIdSelection = TRUE)%>%
      visLayout(randomSeed = 3)
    })
    
    observe({
      visNetworkProxy("network") %>%
        visNodes(font=list(size=input$size)) 
    })
    

    observe({
      visNetworkProxy("network") %>%
        visFocus(id = input$Focus, scale=1)
    })
    
    # scale = input$scale_id
}



# Run the application 
shinyApp(ui = ui, server = server)
