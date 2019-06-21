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
        htmlOutput("key.selector"),
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
        numericInput("binwidth","Binwidth", value = 5)),
      mainPanel(
        plotOutput(outputId = "length.histogram", height = "300px"),
        leafletOutput(outputId = "length.spatial.plot", height = "500px")
      )
    )
  )
)