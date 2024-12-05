# Load data, objects, and helper functions
source(here("methodology.R"))
# source(here("international_data.R"))
# Load your data

server <- function(input, output, session) {
  observeEvent(input$navTo, {
    switch(input$navTo,
           "dpsea-description" = showTab("dpsea-description"),
           "country-selection" = showTab("country-selection"),
           "indicator-used" = showTab("indicator-used"),
           "action-index" = showTab("action-index"),
           "governance-syndrome" = showTab("governance-syndrome"),
           "model-formulas" = showTab("model-formulas"),
           "de-escalation-plots" = showTab("de-escalation-plots"),
           "global-models-data" = showTab("global-models-data"),
           "global-models-formulas" = showTab("global-models-data"),
           
           "main-fig2" = showTab("main-fig2"),
           "main-fig3" = showTab("main-fig3"),
           "main-fig4" = showTab("main-fig4"),
           "main-fig5" = showTab("main-fig5"),
           "main-fig6" = showTab("main-fig6"),
           
           "choropleth-plots" = showTab("choropleth-plots"),
           "governance-changes" = showTab("governance-changes")
    )
  })
  
  # Helper function to show a specific tab
  showTab <- function(tabName) {
    updateTabsetPanel(session, "navbar", selected = tabName)
  }


# Table S1 -------------------------------------------------------------------------------------------------

  # Observe Event ----------------------
  reactive_map_info <- reactive({
    map_info
  })
  
  # Render the country table
  output$country_table <- renderDT({
    reactive_map_info() %>%
      select(country, iso3, continent, income_group) %>%
      rename(
        "Country" = "country",
        "Country Code" = "iso3",
        "Continent" = "continent",
        "Income Group" = "income_group"
      )
  })
  

# Table S2 -------------------------------------------------------------------------------------------------

  # Render indicators table:
  
  # Drivers data
  output$drivers_table <- renderDataTable({
    driver_indicators
  })
  
  # Use data
  output$use_table <- renderDataTable({
    use_indicators
  })
  
  # Resistance data
  output$resistance_table <- renderDataTable({
    resistance_indicators
  })
  
  # DRI data
  output$dri_table <- renderDataTable({
    dri_indicators
  })
  
  # Ecological Variables data
  output$ecological_variables_table <- renderDataTable({
    ecology_indicators
  })

  

# Table S3 -------------------------------------------------------------------------------------------------
  output$action_index_table <- renderDataTable({
    action_index_table
  })


# Table S4 -------------------------------------------------------------------------------------------------
  output$governance_syndrome <- renderDataTable({
    governance_syndrome
  })
  
  
# Table S5 -------------------------------------------------------------------------------------------------
  output$model_formulas <- renderDataTable({
    model_formulas
  })
  

# Table S6 -------------------------------------------------------------------------------------------------
  output$de_escalation_plots <- renderDataTable({
    de_escalation_plots
  })
  
  
# Table S7 & S8 -------------------------------------------------------------------------------------------------
  output$global_models_data <- renderDataTable({
    global_models_data
  })
  
  # Reactive expression to return the selected dataset
  selectedData <- reactive({
    switch(input$dataSubset,
           "General" = global_model_formulas_general,
           "HIC" = global_model_formulas_hic,
           "LMIC" = global_model_formulas_lmic,
           "Binomial" = global_model_formulas_binomial)
  })
  
  # Render the DataTable
  # output$global_model_formulas <- renderDataTable({
  #   datatable(selectedData(), 
  #             options = list(pageLength = 25), 
  #             escape = FALSE)
  # })

  output$global_model_formulas <- renderUI({
    
    knitr::kable(selectedData(), align = "c", format = "html") %>%
      kableExtra::kable_styling(bootstrap_options = "condensed", full_width = FALSE, position = "left", font_size = 12) %>%
      kableExtra::collapse_rows(columns = 2:3, valign = "middle") %>%
      as.character() %>%   # Convert to character to integrate into HTML
      HTML()  # Ensure the output is treated as HTML 
  })
  
  


# Main figures ---------------------------------------------------------------------------------------------
  
  # Figure 2
 
  try.col.vec <- scales::brewer_pal(palette = "Set1")(5)[c(3, 2, 4, 5, 1)]
 
  output$adaptive_plot <- renderPlotly({
    # Ensure input data is available and user has made selections
    req(input$fig2_income, input$fig2_outcome)
    
    # Filter data based on user input
    df_cont <- dpsir %>%
      filter(income %in% input$fig2_income)
    df_bin <- dpsir_bin_plot %>%
      filter(income %in% input$fig2_income)
    
    # Initialize list to hold plots
    plots <- list()
    
    # Check for "Continuous" selection and prepare plot
    if ("Linear Trend" %in% input$fig2_outcome && nrow(df_cont) > 0) {
      p1 <- ggplot(df_cont, aes(x=RESPONSE, y=change, colour=DPSIR)) +
        geom_point(aes(shape = income, group = name), alpha=0.8) + 
        facet_wrap(~ DPSIR,nrow=1) + 
        geom_line(data=mr2_pred,size=1,alpha=0.8) +
        geom_ribbon(data=mr2_pred,aes(ymin = ci.low, ymax = ci.high), colour=NA, alpha = 0.1) + 
        geom_smooth(data=dpsir, 
                    aes(group = income, linetype = income),
                    method="lm", 
                    se=FALSE, 
                    colour="black", 
                    linewidth=0.3, 
                    alpha=0.1) + 
        scale_colour_manual(values=try.col.vec[2:5]) + 
        geom_hline(yintercept = 0) + 
        geom_text(data=letter_df[which(letter_df[,"plot"]=="change"),],
                  aes(x=3.8,y = c(1,1,1,1),
                      label = letter2,
                      fontface = "bold"),
                  inherit.aes = FALSE) +
        geom_text(data = dat_text,mapping = aes(x = -Inf, y = -Inf, label = label, color = DPSIR),
                  hjust   = -0.1, vjust   = -0.5) +
        facet_grid(~factor(DPSIR, levels = c("DRIVERS", "USE", "RESISTANCE", "DRI"))) +
        geom_hline(yintercept=0) +
        scale_y_continuous(
          limits = c(-1, 1),
          breaks = c(-1, -0.5, 0, 0.5, 1)) +
        scale_x_continuous(
          limits = c(0, 4),
          breaks = c(0, 1, 2, 3, 4)) +
        guides(color = "none") +
        theme_bw()+
        theme(legend.position='top')+
        theme(axis.text.x=element_blank(),
              #       legend.position= c(0.9, 0.1),
              axis.title.x=element_blank(),
              strip.text.x = element_blank()) +
        labs(y = "Linear Trend (SD=1)", x = "Action index [0-4]")
      
      plots[[length(plots) + 1]] <- ggplotly(p1) %>% layout(dragmode = "select")
    }
    
    # Check for "Binary" selection and prepare plot
    if ("Categorical Trend" %in% input$fig2_outcome && nrow(df_bin) > 0) {
      p2 <- ggplot(df_bin, aes(x=factor(sign), y=RESPONSE, fill=DPSIR)) +
        geom_boxplot(alpha=0.8) + 
        facet_wrap(~ DPSIR,nrow=1) +
        geom_text(data=letter_df[which(letter_df[,"plot"]=="box"),],
                  aes(x=2.5,y=3.8,label=letter2,fontface="bold")) + 
        facet_wrap(~ DPSIR,nrow=1) +
        # geom_text(dpsir_bin_plot, 
        #           mapping = aes(x = -Inf, y = -Inf, label = p.label, color = DPSIR),
        #           hjust = -0.1, vjust = -0.5) +
        geom_text(data = dat_text2,mapping = aes(x = -Inf, y = -Inf, label = label, color =DPSIR),
                  hjust   = -0.1, vjust   = -0.5) +
        scale_fill_manual(values = try.col.vec[2:5]) +  # Using fill for boxplot
        scale_colour_manual(values=try.col.vec[2:5]) +
        scale_y_continuous(
          limits = c(0, 4),
          breaks = c(0, 1, 2, 3, 4)) +
        facet_grid(~factor(DPSIR, levels=c("DRIVERS", "USE", "RESISTANCE", "DRI"))) +
        guides(color = "none")+ 
        xlab("Categorial trend [+/-]") + 
        ylab("Action index [0-4]") + 
        coord_flip() +
        labs(caption = "Sample Sizes Response Stated Action= 148;  Drivers=73; Use=65; Resistance=32; DRI=25") +
        theme_bw() +
        theme(legend.position="none",
              strip.text.x = element_blank())
      
      plots[[length(plots) + 1]] <- ggplotly(p2) %>% layout(dragmode = "select")
    }
    
    # Conditionally display the appropriate plot(s)
    if (length(plots) == 1) {
      return(plots[[1]])  # Return single plot
    } else if (length(plots) > 1) {
      return(subplot(plots, nrows = length(plots), titleY = TRUE, titleX = TRUE))
    }
  })
  
  
  
  
  
  
  
  
  
  

  # Figure 3
  output$fig3_plot <- renderPlotly({
    p <- ggplot() + 
      #geom_point(data=all_wide,aes(alpha=tot.alpha,colour=factor(DPSIR,levels=c("USE","RESISTANCE","DRI")))) +
      geom_ribbon(data=newdataDPSI,aes(x=RESPONSE, ymin=dec.propmse,ymax=dec.proppse,fill=DPSIR), lwd=0.75,alpha=0.5)+
      geom_line(data=newdataDPSI,aes(RESPONSE, dec.prop,colour=DPSIR), lwd=1.2)+
      geom_point(data=DRIVERS[DRIVERS$ISO3%in%unique(all_wide$ISO3),],aes(x=RESPONSE, y=dec.prop,alpha=tot.alpha,colour=DPSIR, group = name)) +
      geom_point(data=all_wide,aes(x=RESPONSE, y=dec.prop,alpha=tot.alpha,colour=DPSIR,  group = name)) +
      ylim(0,1)+xlim(0,4)+xlab("Stated action")+ylab("De-escalating ratio") +
      theme_bw()+
      scale_color_manual(values=scales::brewer_pal(palette="Set1")(5)[c(2,4,5,1)]) +
      scale_fill_manual(values=scales::brewer_pal(palette="Set1")(5)[c(2,4,5,1)]) +
      labs(alpha = "Weight",color = "DPSE",fill="DPSE")+
      theme(legend.position = c(0.11, 0.3),
            legend.background = element_rect(fill = NA),
            legend.key = element_rect(fill = NA, color = NA)) +
      guides(alpha = "none", color = "none") 
    
    ggplotly(p) %>% layout(dragmode = "select")
    
  })
  
 
  # Figure 4
  output$fig4_plot <- renderPlot({
    x_axis_levels <- c("DPSEA", "DPSEA.noDr", "aP.noDr", "aS.noDr", "aE.noDr", 
                       "aP", "aS", "aE", "Dr", "P", "S", "E", "DPS", "PSE", 
                       "DP", "PS", "SE")
    y_axis_levels <- c( "Animal Production",
                        "Mean Temperature",
                        "Population Density",
                        "income",
                        "GDP",
                        "Gini",
                        "Vaccination",
                        "Workforce",
                        "Infection",
                        "Sanitation",
                        "DPSE",
                        "Awareness and Education",
                        "General",
                        "Monitoring and Surveillance",
                        "Action",
                        "Baseline")
    
    req(input$fig4_outcome)  # Ensure input is received before proceeding
    
    # Filter the dataset based on selected outcome types
    data_to_plot <- rank_df_main %>%
      filter(mod_type %in% input$fig4_outcome)  # Assuming 'mod_type' column contains 'Linear Trend' or 'Categorical Trend'
    
    # Base plot
    p <- ggplot(data_to_plot, aes(x=mod_names, y=variable_names, fill=rank)) +
      geom_tile() +
      scale_fill_brewer(palette = "OrRd", na.value = "gray", direction = -1) +
      theme_light() +
      scale_y_discrete(limit = y_axis_levels, drop = FALSE) +
      scale_x_discrete(limit = x_axis_levels) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      geom_hline(yintercept = c(11.5, 15.5)) +
      ylab("Variable Names") + xlab("Model Names") + ggtitle("Variable Ranks in Each Model") +
      labs(fill = "Rank")
    
    # Conditionally add facets if more than one outcome type is selected
    if (length(input$fig4_outcome) > 1) {
      p <- p + facet_grid(mod_type ~ mod_inc, scales = "free_x", drop = FALSE)
    }
    
    # Return the plot
    return(p)
  })
  
  
  # Figure 5
  output$fig5_plot <- renderPlotly({
    ggplot(aniRmmmmALLig.df[which( !aniRmmmmALLig.df$responseType%in%c("Gmean")&
                                     aniRmmmmALLig.df$cvar==c("prodValue")),],
           aes(y=responseValue,x=log10(cval),colour=responseType))+
      geom_point(aes(group = name), alpha=0.3)+theme_bw()+
      geom_smooth(data=aniRmmmmALLig.df[which( !aniRmmmmALLig.df$responseType%in%c("Gmean")&
                                                 aniRmmmmALLig.df$cvar==c("prodValue")&log10(aniRmmmmALLig.df$cval)<7&log10(aniRmmmmALLig.df$cval)>3),],
                  size=1,level=0.5,alpha=0.3,method = "lm")+
      ylab("Action")+xlab("Animal Production (log10[tonnes])")+
      theme(legend.position = c(0.10, 0.8))+
      scale_colour_manual(name="Sector",labels=c("Animal","Human"),values=c("#66c2a5","#fc8d62"))+
      #scale_linetype_discrete(name="Income group")+scale_shape_discrete(name="Income grou")+
      facet_grid(cols=vars(region2))+theme(legend.background = element_blank())
  })
  
  
  # Figure 6
  output$governance_syndrome1 <- renderPlotly({
    req(input$fig6_income, input$fig6_outcome, input$fig6_indicators)  # Ensure all inputs are available
    
    # Filter data based on user selection of income and indicators
    data_viz <- data_viz %>%
      filter(income %in% input$fig6_income, DPSIR %in% input$fig6_indicators)  # Include indicator filter here
    
    annotation <- annotation %>%
      filter(income %in% input$fig6_income, DPSIR %in% input$fig6_indicators)  # Include indicator filter here
    
    # Initialize plot objects as NULL to handle each case
    plot_continuous <- NULL
    plot_category <- NULL
    
    # Create the Continuous plot if selected
    if ("Linear Trend" %in% input$fig6_outcome) {
      plot_continuous <- ggplot(data_viz, aes(y = change, x = diff_resp)) +
        geom_point(aes(fill = syndrome, group = name), shape = 21, size = 3) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        scale_fill_manual(values = c("Virtuous cycle" = "#4DAC26", "Meeting challenge" = "#B8E186", "Relaxed response" = "#F1B6DA", "Vicious cycle" = "#D01C8B")) +
        facet_wrap(~DPSIR) +
        theme_bw() +
        labs(y = "Changes in ABR indicators between 2000-2016", x = "Difference in governance response between 2016-2022") 
        # geom_text_repel(data = filter(data_viz, syndrome == "D"), aes(label = ISO3), box.padding = 0.5, point.padding = 0.1, size = 4, color = "#D01C8B", max.overlaps = 30)
      
      plot_continuous <- ggplotly(plot_continuous)  # Convert ggplot to plotly
    }
    
    # Create the Category plot if selected
    if ("Categorical Trend" %in% input$fig6_outcome) {
      plot_category <- ggplot(annotation, aes(x = DPSIR, y = freq, fill = syndrome)) +
        geom_col() +
        geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black", size = 3) +
        coord_flip() +
        facet_wrap(~income, ncol = 1, scales = "free_y") +
        scale_fill_manual(values = c("Virtuous cycle" = "#4DAC26", "Meeting challenge" = "#B8E186", "Relaxed response" = "#F1B6DA", "Vicious cycle" = "#D01C8B")) +
        labs(x = NULL, y = NULL) +
        theme_minimal() +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
        theme(axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank(), strip.text = element_text(size = 14, face = "bold"), legend.position = "bottom")
      
      plot_category <- ggplotly(plot_category)  # Convert ggplot to plotly
    }
    
    # Decide which plot(s) to render based on selection
    if (!is.null(plot_continuous) && !is.null(plot_category)) {
      # If both are selected, display them side by side
      return(subplot(plot_continuous, plot_category, nrows = 1, shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE))
    } else if (!is.null(plot_continuous)) {
      # If only Continuous is selected
      return(plot_continuous)
    } else if (!is.null(plot_category)) {
      # If only Category is selected
      return(plot_category)
    } else {
      # If no plot is selected, return NULL
      return(NULL)
    }
  })
  
  # New result added -----------------------------------------------------------------------------------------
  
  # Update 'shortname' choices based on 'group' selection in Tier 3, DRIVERS
  observeEvent(input$group, {
    if (input$level == "Tier 3" && input$dpse == "DRIVERS") {
      shortname_choices <- switch(input$group,
                                  "Infections" = c("HIV" = "HIV", "TB" = "Tuberculosis"),
                                  "Sanitation" = c("Drinking Water Source", "Overall Sanitation", "Water Source Access"),
                                  "Vaccines" = c("DTP3" = "DTP3", "HepB3" = "HepB3", "Hib3" = "Hib3", "Measles" = "Measles", "PCV3" = "PCV3", "Pol3" = "Pol3", "RCV1" = "RCV1"),
                                  "Workforce" = c("Physicians" = "Physicians", "Nursing & midwifery" = "Nursing & midwifery"),
                                  character(0)  # default to empty if no match or null
      )
      updatePickerInput(session, "shortname", choices = shortname_choices)
    }
  }, ignoreNULL = FALSE)
  
  # Reactive data based on user selections
  data <- reactive({
    query <- dpsir_shiny %>%
      filter(DPSIR == input$dpse, level == input$level)
    
    # Conditionally filter by GROUP if it's available and relevant
    if (input$level == "Tier 2" && input$dpse == "DRIVERS" && !is.null(input$group)) {
      query <- query %>% filter(GROUP == input$group)
    }
    
    # Further conditional filtering for USE and RESISTANCE at Tier 2
    if (input$level == "Tier 2" && (input$dpse == "USE" || input$dpse == "RESISTANCE") && !is.null(input$shortname)) {
      query <- query %>% filter(SHORTNAME == input$shortname)
    }
    
    # Filtering for Tier 3 GROUP specific SHORTNAMEs
    if (input$level == "Tier 3" && !is.null(input$group) && !is.null(input$shortname)) {
      query <- query %>% filter(SHORTNAME == input$shortname)
    }
    
    return(query)
  })
  
  conditional_color_scheme <- function(data_column, group, dpse, level) {
    # Base color scheme function with quantiles
    breaks <- quantile(data_column, probs = seq(0, 1, length.out = 7), na.rm = TRUE)
    sorted_breaks <- sort(unique(breaks))
    color_scheme <- list()
    # Define the base color palette
    color_palette <- c("#2166ac", "#67a9cf", "#d1e5f0", "#fddbc7", "#ef8a62", "#b2182b")  # Blue to red
    
    # Reverse the color scale based on specific conditions
    if ((dpse == "DRIVERS" && level == "Tier 1") || 
        (dpse == "DRIVERS" && (level == "Tier 2" || level == "Tier 3") && group != "Infections")) {
      color_palette <- rev(color_palette)  # Red to blue
    }
    
    if (length(sorted_breaks) > 1) {
      for (i in 1:(length(sorted_breaks) - 1)) {
        color_scheme[[i]] <- list(from = sorted_breaks[i], to = sorted_breaks[i + 1], color = color_palette[i])
      }
    } else {
      # Fallback to a single color range if only one unique break exists
      color_scheme[[1]] <- list(from = min(data_column, na.rm = TRUE), to = max(data_column, na.rm = TRUE), color = color_palette[1])
    }
    
    return(color_scheme)
  }
  
  
  # Green-Red divergent color scheme for change data
  green_red_divergent <- function(data_column) {
    # Remove any NA values for accurate min/max computation
    valid_data <- na.omit(data_column)
    
    if (length(valid_data) == 0) {
      # Return a default scheme if no valid data
      return(list(list(from = 0, to = 0, color = "#FFFFFF")))  # White or any neutral color
    }
    
    # Establishing dynamic breaks around zero
    min_val <- min(valid_data, na.rm = T)
    max_val <- max(valid_data, na.rm = T)
    zero_point <- ifelse(min_val < 0 && max_val > 0, 0, ifelse(min_val >= 0, min_val, max_val))
    
    # Setting colors based on the sign of the values
    colors <- c("#7fbf7b", "#af8dc3")  # Green for positive, red for negative
    if (min_val >= 0) {
      # Only positive values
      return(list(list(from = min_val, to = max_val, color = "#af8dc3")))
    } else if (max_val <= 0) {
      # Only negative values
      return(list(list(from = min_val, to = max_val, color = "#7fbf7b")))
    }
    
    # Normal case: values span zero
    return(list(
      list(from = min_val, to = zero_point, color = "#7fbf7b"),
      list(from = zero_point, to = max_val, color = "#af8dc3")
    ))
  }
  

  
  # Function to generate maps based on column data
  
  render_map <- function(column_name) {
    renderHighchart({
      dpse_data <- data()

      year_info <- ifelse(grepl("x0008", column_name), "between 2000-2008", "between 2008-2016")
      
      # Determine which color scheme to use based on the column
      color_scheme <- conditional_color_scheme(dpse_data[[column_name]], input$group, input$dpse, input$level)
      
      
      # The color scheme seems to work well. However, when input.level = Tier 3, and input&group != "Infection), the color scale for the 3 plots need to be reverse
      
      # # Reverse the color scheme if the conditions are met
      # if (input$level == "Tier 3" && input$group != "Infections") {
      #   colors <- rev(colors)
      # }
      # 
      hcmap("custom/world", data = dpse_data, value = column_name,
            joinBy = c("iso-a3", "ISO3"), showInLegend = TRUE) %>%
        hc_title(text = paste("Map of", input$dpse, "data", year_info, "by country")) %>%
        hc_colorAxis(dataClasses = color_scheme) %>%
        hc_tooltip(pointFormat = "{point.name}: {point.value}")
    })
  }
  
  render_map_change <- function(column_name) {
    renderHighchart({
      dpse_data <- data()
      
      if (is.null(dpse_data) || nrow(dpse_data) == 0) {
        return(NULL)  # Handle cases with no data to render
      }
      
      # Determine the appropriate color scheme
      colors <- green_red_divergent(dpse_data[[column_name]])
      
      # Reverse the color scheme if the conditions are met
      if (input$level == "Tier 3" && input$group != "Infections") {
        colors <- rev(colors)
      }
      
      hcmap("custom/world", data = dpse_data, value = column_name,
            joinBy = c("iso-a3", "ISO3"), showInLegend = TRUE) %>%
        hc_title(text = paste("Map of", input$dpse, "data difference between two periods by country")) %>%
        hc_colorAxis(dataClasses = colors) %>%
        hc_tooltip(pointFormat = "{point.name}: {point.value}")
    })
  }
  
  
  # Rendering maps for each data column
  output$map_x0008 <- render_map("x0008")
  output$map_x0816 <- render_map("x0816")
  output$map_change <- render_map_change("change")
  
  
  
  # Governance score changes
  
  # Reactive expression to filter and process data based on input
  filtered_data <- reactive({
    req(input$income_gov, input$trend)  # Ensure inputs are available
    
    # Assuming governance_begin_after is available globally or loaded within server
    governance_begin_after %>%
      filter(income %in% input$income_gov, 
             trend %in% input$trend)  # Example filters based on UI inputs
  })
  
  # Plotly output
  output$governance_changes <- renderPlotly({
    # Call the reactive data
    df <- filtered_data()


    plot_governance <- df %>%
      ggplot(aes(factor(Year), value, group = name, color = highlight)) +
      geom_line(aes(size = ifelse(highlight == "Other", 0.1, 0.7))) +
      # use 2 geoms to make sure highlighted countries' dots are placed on top
      geom_point(data = . %>% filter(highlight == "Other"), size = 0.5) +
      geom_point(data = . %>% filter(highlight != "Other")) +
      ggrepel::geom_text_repel(
        data = . %>% filter(highlight != "other"),
        aes(x = ifelse(Year == min(Year), 1-0.05, 2+0.05),
            label = glue::glue("{name} ({scales::number(value, accuracy = 0.01)})"),
            hjust = ifelse(Year == min(Year), 0.8, 0)),
        size = 2.5, nudge_x = 0, direction = "y", family = "Fira Sans",
        segment.size = 0) +
      scale_x_discrete(position = "top") +
      scale_size_identity() +
      coord_cartesian(clip = "off") +
      scale_color_manual(
        values = c("Other" = "grey60", "Increase" = "#092044", "Decrease" = "#C33C2E",
                   "Unchange" = colorspace::darken("#F0C94C", 0.2))) +
      guides(col = "none") +
      theme_minimal(base_family = "Fira Sans") +
      theme (
        plot.background = element_rect(color = NA, fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#ECEEF2", size = 5),
        text = element_text(color = "#555555"),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold", color = "grey38"),
        axis.text.y = element_blank(),
        plot.margin = margin(t = 6, l = 16, r = 16, b = 4),
        plot.title = element_text(family = "Playfair Display", size = 14, color = "grey12"),
        plot.subtitle = element_textbox_simple(
          margin = margin(t = 6, b = 12)
        ),
        plot.caption = element_markdown(
          hjust = 0, margin = margin(t = 8))
      )
    
      plot_governance <- ggplotly(plot_governance)  
  })
  
  
  
}



