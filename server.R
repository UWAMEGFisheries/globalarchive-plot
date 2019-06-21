
function(input, output, session) {

  # Populate the CampaignID dropdown when the app loads
  observe({
    
    req(input$complete.maxn)
    # read in fst data
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      dplyr::mutate(key="maxn")%>%
      dplyr::mutate(value=maxn)%>%
      as.data.frame()
    
    options <- maxn.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "campaignid.selector", choices = options, selected = "All")
  })
  
  
  observe({
    req(input$complete.length)
    # read in fst data
    length.data<-fst::read_fst(input$complete.length$datapath)%>%
      dplyr::mutate(key="length")%>%
      dplyr::mutate(value=length)%>%
      as.data.frame()
    
    options <- length.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "length.campaignid.selector", choices = options, selected = "All")
  })
  
  observe({
      req(input$complete.mass)
    mass.data<-fst::read_fst(input$complete.mass$datapath)%>%
      dplyr::mutate(key="mass")%>%
      dplyr::mutate(value=mass.g)%>%
      as.data.frame()
    
    options <- mass.data %>%
      dplyr::distinct(campaignid) %>% 
      pull("campaignid")%>%
      sort()
    
    options <- c("All", options)
    updateSelectInput(session, "mass.campaignid.selector", choices = options, selected = "All")
  })
  
  # Create a dropdown
  create_dropdown <- function(input_name, choices, label) {
    if (!is.null(input[[input_name]]) && input[[input_name]] %in% choices) {
      selected <- input[[input_name]] 
    } else {
      selected <- choices[1]
    }
    
    selectInput(
      inputId = input_name, 
      label = label,
      choices = choices,
      selected = selected
    )
  }
  
  campaignid_data <- reactive({
    req(input$campaignid.selector)
    maxn.data <- fst::read_fst(input$complete.maxn$datapath)%>%
      dplyr::mutate(key="maxn")%>%
      as.data.frame()
    
    if (input$campaignid.selector == "All") {
      maxn.data
      
    } else {
      campaign.name <- input$campaignid.selector
      filter(maxn.data, campaignid == campaign.name)
    }
  })
  
  length_campaignid_data <- reactive({
    req(input$length.campaignid.selector)
    length.data <- fst::read_fst(input$complete.length$datapath)%>%
      dplyr::mutate(key="length")%>%
      as.data.frame()
    
    if (input$length.campaignid.selector == "All") {
      length.data
      
    } else {
      campaign.name <- input$length.campaignid.selector
      filter(length.data, campaignid == campaign.name)
    }
  })
  
  # output$key.selector <- renderUI({
  #   
  #   df<-campaignid_data()
  #   
  #   if (!is.null(df)) {
  #     options <- df %>%
  #       dplyr::distinct(key) %>%
  #       dplyr::pull("key")
  #     create_dropdown("key.selector", options, "Metric:")
  #   }
  # })

  output$family.selector <- renderUI({
   # req(input$key.selector)
    
    if (input$campaignid.selector == "All") {
      df<-campaignid_data()
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("family.selector", options, "Family:")
      
      
    } else {
      
      df<-campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        filter(campaignid == input$campaignid.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(family) %>%
        pull("family")%>%
        sort()
      
      create_dropdown("family.selector", options, "Family:")
    }
    # options <- campaignid_data() %>%
    #   filter(key == input$key.selector) %>%
    #   distinct(family) %>%
    #   pull("family")%>%
    #   sort()
    # create_dropdown("family.selector", options, "Family:")
  })
  
  output$length.family.selector <- renderUI({

      options <- length_campaignid_data() %>%
        distinct(family) %>%
        pull("family") %>%
        sort()
      create_dropdown("length.family.selector", options, "Family:")
  })
  
  # output$key.selector <- renderUI({
  #   
  #   df<-campaignid_data()
  #   
  #   if (!is.null(df)) {
  #     options <- df %>%
  #       dplyr::distinct(key) %>%
  #       dplyr::pull("key")%>%
  #       sort()
  #     create_dropdown("key.selector", options, "Metric:")
  #   }
  # })

  output$genus.selector <- renderUI({
    req(input$family.selector)
    
    if (input$campaignid.selector == "All") {
      df<-campaignid_data()
      
      options <- df %>%
        filter(#key == input$key.selector,
               family == input$family.selector) %>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
        create_dropdown("genus.selector", options, "Genus:")
      
      
    } else {
      
      df<-campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        filter(campaignid == input$campaignid.selector) %>%
        filter(family == input$family.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        distinct(genus) %>%
        pull("genus")%>%
        sort()
      
      create_dropdown("genus.selector", options, "Genus:")
    }
    
    # options <- campaignid_data() %>%
    #   filter(
    #     key == input$key.selector,
    #     family == input$family.selector
    #   ) %>%
    #   distinct(genus) %>%
    #   pull("genus")%>%
    #   sort()
    # create_dropdown("genus.selector", options, "Genus:")
  })
  
  output$length.genus.selector <- renderUI({
    req(input$length.campaignid.selector, input$length.family.selector)
    options <- length_campaignid_data() %>%
      filter(
        #key == input$length.key.selector,
        family == input$length.family.selector
      ) %>%
      distinct(genus) %>%
      pull("genus")%>%
      sort()
    create_dropdown("length.genus.selector", options, "Genus:")
  })

  output$species.selector <- renderUI({
    req(input$family.selector, input$genus.selector)
    
    if (input$campaignid.selector == "All") {
      df<-campaignid_data()
      
      options <- df %>%
        filter(
          #key == input$key.selector,
          family == input$family.selector,
          genus == input$genus.selector
        ) %>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("species.selector", options, "Species:")
      
      
    } else {
      
      df<-campaignid_data()
      
      family.genus.species.to.keep<-df%>%
        filter(maxn>0)%>%
        filter(campaignid == input$campaignid.selector)%>%
        distinct(family,genus,species)
      
      options <- df %>%
        #filter(key == input$key.selector) %>%
        filter(campaignid == input$campaignid.selector) %>%
        filter(family == input$family.selector,
          genus == input$genus.selector) %>%
        semi_join(family.genus.species.to.keep)%>%
        dplyr::select("species") %>%
        distinct() %>%
        pull("species")%>%
        sort()
      create_dropdown("species.selector", options, "Species:")
    }
    # options <- campaignid_data() %>%
    #   filter(
    #     key == input$key.selector,
    #     family == input$family.selector,
    #     genus == input$genus.selector
    #   ) %>%
    #   dplyr::select("species") %>%
    #   distinct() %>%
    #   pull("species")%>%
    #   sort()
    # create_dropdown("species.selector", options, "Species:")
  })
  
  output$length.species.selector <- renderUI({
    req(input$length.campaignid.selector, input$length.family.selector, input$length.genus.selector)
    options <- length_campaignid_data() %>%
      filter(
        #key == input$length.key.selector,
        family == input$length.family.selector,
        genus == input$length.genus.selector
      ) %>%
      dplyr::select("species") %>%
      distinct() %>%
      pull("species")%>%
      sort()
    create_dropdown("length.species.selector", options, "Species:")
  })
  

  trends_data <- reactive({
    req(input$family.selector, input$genus.selector, input$species.selector)
    campaignid_data() %>%
      filter(
        #key == input$key.selector,
        family == input$family.selector,
        genus == input$genus.selector,
        species == input$species.selector
      )
  })
  
  length_trends_data <- reactive({
    req(input$length.family.selector, input$length.genus.selector, input$length.species.selector)
    length_campaignid_data() %>%
      filter(
        family == input$length.family.selector,
        genus == input$length.genus.selector,
        species == input$length.species.selector
      )
  })
  

  # Create scatterplot object the plotOutput function is expecting

  output$status.plot <- renderPlot({
    ggplot(trends_data(),aes(x = factor(status), y = maxn, colour = status, fill = status,notch=FALSE, outlier.shape = NA)) + 
      #scale_fill_manual("",values=c("No-take"="lightgrey","Fished"="lightgrey"))+
      theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab("Status") + ylab("Abundance per stereo-BRUV") +
      theme_bw() +
      ggtitle("Plot of abundance by Status")+
      Theme1
  })
  
  output$location.plot <- renderPlot({
    ggplot(trends_data(),aes(x = factor(location), y = maxn, colour = location, fill = location,notch=FALSE, outlier.shape = NA)) + 
      #scale_fill_manual("",values=c("No-take"="lightgrey","Fished"="lightgrey"))+
      theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab("Location") + ylab("Abundance per stereo-BRUV") +
      ggtitle("Plot of abundance by Location") +
      theme_bw() +
      Theme1
  })
  
  output$site.plot <- renderPlot({
    ggplot(trends_data(),aes(x = factor(site), y = maxn, colour = site, fill = site,notch=FALSE, outlier.shape = NA)) + 
      #scale_fill_manual("",values=c("No-take"="lightgrey","Fished"="lightgrey"))+
      theme( panel.background = element_blank(),axis.line = element_line(colour = "black"))+
      stat_boxplot(geom='errorbar')+
      geom_boxplot(outlier.color = NA, notch=FALSE)+
      stat_summary(fun.y=mean, geom="point", shape=23, size=4)+ #this is adding the dot for the mean
      theme_bw()+
      Theme1+
      xlab("Site") + ylab("Abundance per stereo-BRUV") +
      ggtitle("Plot of abundance by Site")+
      theme_bw() +
      Theme1
  })
  
  output$length.histogram <- renderPlot({
    ggplot(length_trends_data(),aes(x = length,colour = status,fill=status))+
      geom_histogram(alpha=0.5, position="identity",binwidth=input$binwidth)+
      #geom_density(alpha=0.6)+
      xlab("Length (mm)") + ylab("Count") +
      theme_bw() +
      Theme1
  })

  # Create spatial plot
  output$spatial.plot <- renderLeaflet({
    map <- leaflet(trends_data()) %>%
      addTiles()%>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))

    overzero <- filter(trends_data(), maxn > 0)
    equalzero <- filter(trends_data(), maxn == 0)
    if (nrow(overzero)) {
      map <- map %>%
        addCircleMarkers(
          data = overzero, lat = ~ latitude, lng = ~ longitude,
          radius = ~((maxn/max(maxn))*15), fillOpacity = 0.5, stroke = FALSE,
          label = ~as.character(maxn)
        )
    }
    if (nrow(equalzero)) {
      map <- map %>%
        addCircleMarkers(
          data = equalzero, lat = ~ latitude, lng = ~ longitude,
          radius = 2, fillOpacity = 0.5, color = "white",stroke = FALSE,
          label = ~as.character(maxn)
        )
    }
    map
  })
}
