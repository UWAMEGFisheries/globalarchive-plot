navbarPage(
  "Plot GlobalArchive data",
  tabPanel(
    "Upload Data",
    sidebarLayout(
      sidebarPanel(
        
        fileInput("complete.maxn", "Upload complete maxn FST File",
                  accept = c("image/vnd.fst",
                             ".fst")),
        
        fileInput("complete.length", "Upload complete length FST File",
                  accept = c("image/vnd.fst",
                             ".fst")),
      
        fileInput("complete.mass", "Upload complete mass FST File",
                accept = c("image/vnd.fst",
                           ".fst"))),
    mainPanel(plotOutput(outputId= "line.plot.test", height = "300px")))),
  tabPanel(
    "MaxN",
    sidebarLayout(
      sidebarPanel(
        
        #fileInput("complete.maxn", "Upload complete maxn FST File",
        #          accept = c("image/vnd.fst",
        #          ".fst")),
        
        # Select campaignid
        selectInput(inputId = "campaignid.selector", label = "CampaignID",
                    choices = NULL),
        # Select metric
        #htmlOutput("key.selector"),
        # Select Fam, Gen and spe
        htmlOutput("family.selector",multiple=TRUE),
        htmlOutput("genus.selector",multiple=TRUE),
        htmlOutput("species.selector",multiple=TRUE)
      ),
      mainPanel(
        leafletOutput(outputId = "spatial.plot", height = "500px"),
        plotOutput(outputId = "status.plot", height = "300px"),
        plotOutput(outputId = "location.plot", height = "300px"),
        plotOutput(outputId = "site.plot", height = "300px")
      )
    )
  ),
  tabPanel(
    "Length",
    sidebarLayout(
      sidebarPanel(
        #
        #fileInput("complete.length", "Upload complete length FST File",
        #          accept = c("image/vnd.fst",
        #                     ".fst")),
        
        # Select campaignid
        selectInput(inputId = "length.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        # Select metric
        #htmlOutput("length.key.selector"),
        # Select Fam, Gen and spe
        htmlOutput("length.family.selector"),
        htmlOutput("length.genus.selector"),
        htmlOutput("length.species.selector"),
        
        helpText(h4("",
                    "Adjust plotting parameters below.")),
        radioButtons("length.colour.fill", "Colour and Fill by",choices = list("Status" = "status", "Location" = "location","Site" = "site"),selected = "status"),
        numericInput("length.binwidth","Binwidth", value = 5)),
      mainPanel(
        plotOutput(outputId = "length.histogram", height = "300px"),
        plotOutput(outputId = "length.vs.range", height = "300px"),
        leafletOutput(outputId = "length.spatial.plot", height = "500px")
      )
    )
  ),
  tabPanel(
    "Mass",
    sidebarLayout(
      sidebarPanel(
        #
        #fileInput("complete.length", "Upload complete length FST File",
        #          accept = c("image/vnd.fst",
        #                     ".fst")),
        
        # Select campaignid
        selectInput(inputId = "mass.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        # Select metric
        #htmlOutput("length.key.selector"),
        # Select Fam, Gen and spe
        htmlOutput("mass.family.selector"),
        htmlOutput("mass.genus.selector"),
        htmlOutput("mass.species.selector"),
        
        helpText(h4("",
                    "Adjust plotting parameters below.")),
        radioButtons("mass.colour.fill", "Colour and Fill by",choices = list("Status" = "status", "Location" = "location","Site" = "site"),selected = "status"),
        numericInput("mass.binwidth","Binwidth", value = 5)),
      mainPanel(
        plotOutput(outputId = "mass.histogram", height = "300px"),
        plotOutput(outputId = "length.vs.mass", height = "300px"),
        leafletOutput(outputId = "mass.spatial.plot", height = "500px")
      )
    )
  ),
  tabPanel(
    "Habitat",
    sidebarLayout(
      sidebarPanel(
        #
        #fileInput("complete.length", "Upload complete length FST File",
        #          accept = c("image/vnd.fst",
        #                     ".fst")),
        
        # Select campaignid
        selectInput(inputId = "mass.campaignid.selector", label = "CampaignID",
                    choices = NULL),
        # Select metric
        #htmlOutput("length.key.selector"),
        # Select Fam, Gen and spe
        #htmlOutput("mass.family.selector"),
        #htmlOutput("mass.genus.selector"),
        #htmlOutput("mass.species.selector"),
        
        #helpText(h4("",
                    "Adjust plotting parameters below.")),
        #radioButtons("mass.colour.fill", "Colour and Fill by",choices = list("Status" = "status", "Location" = "location","Site" = "site"),selected = "status"),
        #numericInput("mass.binwidth","Binwidth", value = 5)),
      mainPanel(
        leafletOutput(outputId = "mass.spatial.plot", height = "500px"),
        plotOutput(outputId = "mass.histogram", height = "300px"),
        plotOutput(outputId = "length.vs.mass", height = "300px")
        
      )
    )
  ),
  tabPanel(
    "Download Scripts",
    #headerPanel("Simple Shiny Ace!"),
    sidebarPanel(
      downloadButton("downloadData", label = "Download"),
      #selectInput("mode", "Mode: ", choices = modes, selected = "plain_text"),
      #selectInput("theme", "Theme: ", choices = themes, selected = "textmate"),
      #numericInput("size", "Tab size:", 4),
      #radioButtons("soft", NULL, c("Soft tabs" = TRUE, "Hard tabs" = FALSE), inline = TRUE),
      #radioButtons("invisible", NULL, c("Hide invisibles" = FALSE, "Show invisibles" = TRUE), inline = TRUE),
      #actionButton("reset", "Reset text"),
      #actionButton("clear", "Clear text"),
      HTML("<hr />"),
      helpText(HTML("A simple Shiny Ace editor.
                  <p>Created using <a href = \"http://github.com/trestletech/shinyAce\">shinyAce</a>."))
    ),
    mainPanel(
      aceEditor("ace", theme = "tomorrow_night_blue", mode = "r", value = "# functions for summarising data on plots----
se <- function(x) sd(x) / sqrt(length(x))

# se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

# status plot
ggplot(trends_data(),aes(x = factor(status), y = maxn, colour = status, fill = status,notch=FALSE, outlier.shape = NA))+ 
      theme( panel.background = element_blank(),axis.line = element_line(colour = \"black\"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom=\"point\", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab(\"Status\") + ylab(\"Abundance per stereo-BRUV\") +
      ggtitle(\"Plot of abundance by Status\")")
    )
)
)
