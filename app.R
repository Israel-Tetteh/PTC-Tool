# Writing shiny script for PTC Analysis tool.
# Load necessary libraries.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(broom)
library(openxlsx)
library(tibble)
library(stats)
library(shinyWidgets)
library(rlang)
library(car)
library(readxl)
library(shinybusy)

# Source to pull functions-- I will write the modules later.
source(file = "utils/utils_helper_functions_ptc.R")

ui <- navbarPage(
  title = "PTC Analysis Tool v.01", theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  setBackgroundColor(
    color = "#f8f9fa",
    gradient = "linear",
    direction = "bottom"
  ),
  # Replace your current nav_panel "Welcome!" with this:
  
  nav_panel(
    title = "Welcome!", icon = icon("home"),
    
    # Custom CSS for the landing page
    tags$head(
      tags$style(HTML("
      :root {
        --primary-green: #2d5016;
        --secondary-green: #4a7c59;
        --accent-green: #7fb069;
        --light-green: #a7c957;
        --bg-light: #f8fffe;
        --text-dark: #1a1a1a;
        --shadow: rgba(45, 80, 22, 0.15);
      }
      
      .landing-page {
        margin: -15px;
        padding: 0;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background: linear-gradient(135deg, var(--bg-light) 0%, #e8f5e8 100%);
        min-height: 100vh;
        position: relative;
        overflow: hidden;
      }
      
      .particles {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        pointer-events: none;
        z-index: 1;
      }
      
      .particle {
        position: absolute;
        width: 4px;
        height: 4px;
        background: var(--accent-green);
        border-radius: 50%;
        opacity: 0.6;
        animation: float 6s infinite linear;
      }
      
      @keyframes float {
        0% { transform: translateY(100vh) rotate(0deg); opacity: 0; }
        10% { opacity: 0.6; }
        90% { opacity: 0.6; }
        100% { transform: translateY(-10vh) rotate(360deg); opacity: 0; }
      }
      
      .hero-section {
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
        text-align: center;
        position: relative;
        z-index: 2;
        padding: 0 2rem;
      }
      
      .hero-content {
        max-width: 800px;
        animation: fadeInUp 1s ease-out;
      }
      
      @keyframes fadeInUp {
        from { opacity: 0; transform: translateY(50px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .hero-title {
        font-size: 4rem;
        font-weight: 800;
        background: linear-gradient(135deg, var(--primary-green), var(--secondary-green));
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        margin-bottom: 1rem;
        line-height: 1.2;
      }
      
      .hero-subtitle {
        font-size: 1.3rem;
        color: var(--text-dark);
        margin-bottom: 3rem;
        line-height: 1.6;
      }
      
      .hero-buttons {
        display: flex;
        gap: 1rem;
        justify-content: center;
        flex-wrap: wrap;
      }
      
      .btn-hero-primary {
        background: linear-gradient(135deg, var(--secondary-green), var(--accent-green));
        color: white;
        padding: 1rem 2rem;
        border: none;
        border-radius: 50px;
        font-weight: 600;
        font-size: 1.1rem;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 6px 20px var(--shadow);
        text-decoration: none;
        display: inline-block;
      }
      
      .btn-hero-primary:hover {
        transform: translateY(-3px);
        box-shadow: 0 10px 30px var(--shadow);
        color: white;
        text-decoration: none;
      }
      
      .btn-hero-secondary {
        background: transparent;
        color: var(--primary-green);
        border: 2px solid var(--secondary-green);
        padding: 1rem 2rem;
        border-radius: 50px;
        font-weight: 600;
        font-size: 1.1rem;
        cursor: pointer;
        transition: all 0.3s ease;
        text-decoration: none;
        display: inline-block;
      }
      
      .btn-hero-secondary:hover {
        background: var(--secondary-green);
        color: white;
        transform: translateY(-3px);
        text-decoration: none;
      }
      
      .features-section {
        padding: 6rem 2rem;
        background: white;
        position: relative;
        z-index: 2;
      }
      
      .features-title {
        text-align: center;
        font-size: 3rem;
        font-weight: 700;
        color: var(--primary-green);
        margin-bottom: 3rem;
      }
      
      .features-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
        gap: 2rem;
        max-width: 1200px;
        margin: 0 auto;
      }
      
      .feature-card {
        background: white;
        padding: 2.5rem;
        border-radius: 20px;
        box-shadow: 0 10px 30px var(--shadow);
        text-align: center;
        transition: all 0.3s ease;
        border: 1px solid rgba(127, 176, 105, 0.1);
        position: relative;
        overflow: hidden;
      }
      
      .feature-card:hover {
        transform: translateY(-10px);
        box-shadow: 0 20px 40px var(--shadow);
      }
      
      .feature-icon {
        width: 80px;
        height: 80px;
        background: linear-gradient(135deg, var(--accent-green), var(--secondary-green));
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        margin: 0 auto 1.5rem;
        font-size: 2rem;
        color: white;
      }
      
      .feature-card h3 {
        font-size: 1.5rem;
        color: var(--primary-green);
        margin-bottom: 1rem;
        font-weight: 600;
      }
      
      .feature-card p {
        color: var(--text-dark);
        line-height: 1.6;
      }
      
      .stats-section {
        padding: 4rem 2rem;
        background: linear-gradient(135deg, var(--primary-green), var(--secondary-green));
        color: white;
        position: relative;
        z-index: 2;
      }
      
      .stats-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 2rem;
        text-align: center;
        max-width: 1000px;
        margin: 0 auto;
      }
      
      .stat-item h3 {
        font-size: 3rem;
        font-weight: 800;
        margin-bottom: 0.5rem;
      }
      
      .stat-item p {
        font-size: 1.1rem;
        opacity: 0.9;
      }
      
      .cta-section {
        padding: 6rem 2rem;
        background: linear-gradient(135deg, #f8fffe 0%, #e8f5e8 100%);
        text-align: center;
        position: relative;
        z-index: 2;
      }
      
      .cta-content {
        max-width: 600px;
        margin: 0 auto;
      }
      
      .cta-section h2 {
        font-size: 2.5rem;
        color: var(--primary-green);
        margin-bottom: 1rem;
        font-weight: 700;
      }
      
      .cta-section p {
        font-size: 1.2rem;
        color: var(--text-dark);
        margin-bottom: 2rem;
        line-height: 1.6;
      }
      
      .footer-section {
        background: var(--primary-green);
        color: white;
        padding: 3rem 2rem 1rem;
        position: relative;
        z-index: 2;
      }
      
      .footer-content {
        max-width: 1200px;
        margin: 0 auto;
      }
      
      .footer-columns {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
        gap: 2rem;
        margin-bottom: 2rem;
      }
      
      .footer-col h3 {
        margin-bottom: 1rem;
        color: var(--light-green);
        font-weight: 600;
      }
      
      .footer-col p {
        color: rgba(255, 255, 255, 0.8);
        margin-bottom: 0.5rem;
        line-height: 1.6;
      }
      
      .footer-col ul {
        list-style: none;
        padding: 0;
      }
      
      .footer-col ul li {
        color: rgba(255, 255, 255, 0.8);
        margin-bottom: 0.5rem;
        transition: color 0.3s ease;
        cursor: pointer;
      }
      
      .footer-col ul li:hover {
        color: var(--light-green);
      }
      
      .footer-col a {
        color: rgba(255, 255, 255, 0.8);
        text-decoration: none;
        transition: color 0.3s ease;
      }
      
      .footer-col a:hover {
        color: var(--light-green);
        text-decoration: none;
      }
      
      .footer-bottom {
        border-top: 1px solid rgba(255, 255, 255, 0.1);
        padding-top: 2rem;
        text-align: center;
        color: rgba(255, 255, 255, 0.7);
        font-size: 0.9rem;
      }
      
      @media (max-width: 768px) {
        .hero-title { font-size: 2.5rem; }
        .hero-subtitle { font-size: 1.1rem; }
        .hero-buttons { flex-direction: column; align-items: center; }
        .features-title { font-size: 2rem; }
        .feature-card { padding: 1.5rem; }
      }
    "))
    ),
    
    # JavaScript for animations
    tags$script(HTML("
    $(document).ready(function() {
      // Create particles
      function createParticle() {
        var particle = $('<div class=\"particle\"></div>');
        particle.css({
          'left': Math.random() * 100 + '%',
          'animation-delay': Math.random() * 6 + 's',
          'animation-duration': (Math.random() * 3 + 3) + 's'
        });
        $('.particles').append(particle);
        
        setTimeout(function() {
          particle.remove();
        }, 6000);
      }
      
      // Create particles continuously
      setInterval(createParticle, 300);
      
      // Smooth scrolling for anchor links
      $('a[href^=\"#\"]').click(function(e) {
        e.preventDefault();
        var target = $($(this).attr('href'));
        if (target.length) {
          $('html, body').animate({
            scrollTop: target.offset().top - 80
          }, 800);
        }
      });
      
      // Scroll to sections when navigation buttons are clicked
      $('.btn-hero-secondary').click(function(e) {
        e.preventDefault();
        $('html, body').animate({
          scrollTop: $('.features-section').offset().top - 80
        }, 800);
      });
    });
  ")),
    
    # Main content
    div(class = "landing-page",
        # Particles background
        div(class = "particles"),
        
        # Hero Section
        div(class = "hero-section",
            div(class = "hero-content",
                h1(class = "hero-title", "Advanced Plant Tissue Culture Analysis"),
                p(class = "hero-subtitle", 
                  "Streamline your research with automated datasheet generation, comprehensive statistical analysis, and publication-ready visualizations for plant tissue culture experiments."
                ),
                div(class = "hero-buttons",
                    # Action buttons for navigation
                    actionButton("go_to_datasheet", "Start Analyzing", 
                                 class = "btn-hero-primary", 
                                 icon = icon("rocket")),
                    tags$a(href = "#features", class = "btn-hero-secondary", "Learn More")
                )
            )
        ),
        
        # Features Section
        div(class = "features-section", id = "features",
            h2(class = "features-title", "Powerful Research Tools"),
            div(class = "features-grid",
                div(class = "feature-card",
                    div(class = "feature-icon", "📊"),
                    h3("Smart Datasheet Generation"),
                    p("Create customized data collection templates for single factor or factorial experiments with predefined parameters and flexible customization options.")
                ),
                div(class = "feature-card",
                    div(class = "feature-icon", "🔬"),
                    h3("Advanced Statistical Analysis"),
                    p("Perform comprehensive statistical analysis including ANOVA, LSD, Tukey's HSD, and automatic normality testing with Yeo-Johnson transformations.")
                ),
                div(class = "feature-card",
                    div(class = "feature-icon", "📈"),
                    h3("Growth Trend Visualization"),
                    p("Track and visualize growth patterns over time with interactive plots and smooth trend analysis for better insights into plant development.")
                ),
                div(class = "feature-card",
                    div(class = "feature-icon", "🎨"),
                    h3("Publication-Ready Plots"),
                    p("Generate professional boxplots and bar charts with customizable aesthetics, perfect for research papers and presentations.")
                ),
                div(class = "feature-card",
                    div(class = "feature-icon", "⚡"),
                    h3("Automated Processing"),
                    p("Intelligent data preprocessing with outlier detection, missing value handling, and automatic selection of appropriate statistical tests.")
                ),
                div(class = "feature-card",
                    div(class = "feature-icon", "💾"),
                    h3("Export & Download"),
                    p("Download complete analysis results in Excel format and high-quality plots in PDF format for easy sharing and publication.")
                )
            )
        ),
        
        # Stats Section
        div(class = "stats-section",
            div(class = "stats-grid",
                div(class = "stat-item",
                    h3("10+"),
                    p("Built-in Parameters")
                ),
                div(class = "stat-item",
                    h3("3"),
                    p("Statistical Test Types")
                ),
                div(class = "stat-item",
                    h3("∞"),
                    p("Custom Treatments")
                ),
                div(class = "stat-item",
                    h3("100%"),
                    p("Open Source")
                )
            )
        ),
        
        # CTA Section
        div(class = "cta-section",
            div(class = "cta-content",
                h2("Ready to Transform Your Research?"),
                p("Join researchers worldwide who are streamlining their plant tissue culture analysis with our comprehensive toolkit."),
                # Action button for navigation
                actionButton("go_to_datasheet_2", "Launch PTC Analysis Tool",
                             class = "btn-hero-primary",
                             icon = icon("play"))
            )
        ),
        
        # Footer Section
        tags$footer(class = "footer-section",
                    div(class = "footer-content",
                        div(class = "footer-columns",
                            div(class = "footer-col",
                                h3("PTC Analysis Tool"),
                                p("Advanced statistical analysis and visualization platform for plant tissue culture research.")
                            ),
                            div(class = "footer-col",
                                h3("Features"),
                                tags$ul(
                                  tags$li("Datasheet Generation"),
                                  tags$li("Statistical Analysis"),
                                  tags$li("Growth Trends"),
                                  tags$li("Data Visualization")
                                )
                            ),
                            div(class = "footer-col",
                                h3("Support"),
                                tags$ul(
                                  tags$li("Documentation"),
                                  tags$li("Tutorials"),
                                  tags$li("FAQ"),
                                  tags$li("Contact Support")
                                )
                            ),
                            div(class = "footer-col",
                                h3("Connect"),
                                p(tags$a(href = "mailto:israeltetteh715@gmail.com", "israeltetteh715@gmail.com")),
                                p("Teaching & Research Assistant"),
                                p("Academic Year 2023/2024")
                            )
                        ),
                        div(class = "footer-bottom",
                            p("© 2024 PTC Analysis Tool. Created by Israel Tawiah Tetteh. All rights reserved.")
                        )
                    )
        )
    )
  ),
  #   # Navbar menu items with other functionalities included.
  nav_menu(
    title = "Generate Datasheet for PTC",
    # Single Factor Treatment Tab
    tabPanel( 
      title = "Single Factor Treatment",
      sidebarLayout(
        sidebarPanel(
          h3(tags$b("Single Factor Treatment")), hr(),
          textInput("experiment_title_sf", "Experiment Title", ""),
          textInput("researcher_sf", "Researcher's Name", ""),
          selectInput("collection_frequency_sf", "Collection Frequency (Weeks)",
            choices = c("1 week", "2 weeks", "3 weeks", "4 weeks", "5 weeks", "6 weeks", "7 weeks")
          ),
          numericInput("num_records_sf", "Number of data recordings during the experiment", 10, min = 1),
          textInput("treatments_sf", "Treatments (comma separated)", "A, B, C"),
          numericInput("replicates_per_treatment_sf", "Replicates per Treatment", 3, min = 1),
          selectInput("parameters_sf", "Select Parameters",
            choices = c(
              "Plantlet_Height", "Number_of_Leaves", "Number_of_Roots",
              "Length_of_Shoot", "Length_of_Roots", "Number_of_Shoots",
              "Number_of_Buds", "Number_of_Days_for_Shoot_Formation",
              "Number_of_Days_for_Root_Formation",
              "Number_of_Days_for_Leaf_Formation"
            ),
            selected = c("Plantlet_Height", "Number_of_Leaves"),
            multiple = TRUE
          ),
          textInput("additional_parameters_sf", "Additional Parameters (comma separated)", placeholder = "Fruit_Count"),
          div(
            style = "display: flex ; justify-content: center ;",
            actionButton("generate_sf", "Generate Datasheet", class = "btn btn-primary", icon = icon("rocket"))
          )
        ),
        mainPanel(
          # Add validation message container
          uiOutput("validation_msg_sf"),
          # Data table output
          DTOutput("dataPreview_sf"),
          downloadButton("downloadData_sf", "Download Datasheet(.xlsx)", class = "btn btn-success")
        )
      )
    ),
    # Factorial Treatment Tab
    tabPanel( 
      title = "Factorial Treatment",
      sidebarLayout(
        sidebarPanel(
          h3(tags$b("Factorial Treatment")), hr(),
          textInput("experiment_title_f", "Experiment Title", ""),
          textInput("researcher_f", "Researcher's Name", ""),
          selectInput("collection_frequency_f", "Collection Frequency (Weeks)",
            choices = c("1 week", "2 weeks", "3 weeks", "4 weeks", "5 weeks", "6 weeks", "7 weeks")
          ),
          numericInput("num_records_f", "Number of data recordings during the experiment", 10, min = 1),
          numericInput("num_factors", "Number of Factors", 2, min = 2),
          uiOutput("treatments_input"),
          numericInput("replicates_per_treatment_f", "Replicates per Treatment Combination", 3, min = 1),
          selectInput("parameters_f", "Select Parameters",
            choices = c(
              "Plantlet_Height", "Number_of_Leaves", "Number_of_Roots",
              "Length_of_Shoot", "Length_of_Roots", "Number_of_Shoots",
              "Number_of_Buds", "Number_of_Days_for_Shoot_Formation",
              "Number_of_Days_for_Root_Formation",
              "Number_of_Days_for_Leaf_Formation"
            ),
            selected = c("Plantlet_Height", "Number_of_Leaves"),
            multiple = TRUE
          ),
          textInput("additional_parameters_f", "Additional Parameters (comma separated)", placeholder = "Fruit_Count"),
          div(
            style = "display: flex ; justify-content: center ;",
            actionButton("generate_f", "Generate Datasheet", class = "btn btn-primary", icon = icon("rocket"))
          )
        ),
        mainPanel(
          # Add validation message container
          uiOutput("validation_msg_f"),
          DTOutput("dataPreview_f"),
          downloadButton("downloadData_f", "Download Datasheet(.xlsx)", class = "btn btn-success")
        )
      )
    )
  ),
  # Upload data sheet tab
  tabPanel(
    title = "Get Analysis",
    navset_card_tab(
      nav_panel(
        title = "Import Datasheet",  
        sidebarLayout(
          sidebarPanel(
            fileInput(inputId = "file_upload", label = "Upload PTC Datasheet", accept = c(".xlsx", ".xls", ".csv"), placeholder = "Data Sheet (Izzi Datasheet)"),
            div(
              style = "display: flex ; justify-content: center ;",
              actionButton(
                inputId = "submit_1", label = "Submit",
                width = "70%", class = "btn-primary",
                icon = icon("upload")
              )
            ),
            # Add help text
            tags$div(
              class = "mt-3 text-muted",
              tags$p("Note: Please upload a datasheet generated from this tool or one with a similar format."),
              tags$p("Supported file formats: .xlsx, .xls, .csv")
            )
          ),
          mainPanel(
            # Add error message container
            uiOutput("upload_error_msg"),
            bslib::accordion(
              width = "100%",
              bslib::accordion_panel(
                title = "Preview of Datasheet Uploaded.",
                DT::DTOutput(outputId = "Preview")
              ), br(),
              bslib::accordion(
                width = "100%",
                bslib::accordion_panel(
                  title = "Summary of Information Seen from Uploaded Datasheet",
                  DT::dataTableOutput(outputId = "dat_sum")
                )
              )
            )
          )
        )
      ),
      nav_panel(
        title = "Analyse Datasheet",  
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "col_id", label = "Select Response Variable to Analyse", choices = NULL),
            input_switch(id = "dat_trim", label = "Tidy Data", value = TRUE),
            div(
              style = "display: flex; justify-content: center;",
              actionButton(inputId = "submit_2", label = "Get Results", icon = icon("chart-bar"), class = "btn-primary", width = "70%")
            ), hr(),
            # div(style = 'display: flex; justify-content: center;',
            #     actionButton(inputId = 'submit_2',label = 'Get Results',icon = icon('rocket'), class = 'btn-primary',width = '70%')),br(),hr(),
            #
            div(
              style = "display: flex; justify-content: center;",
              downloadButton(outputId = "download_des", label = "Download Results.(xlsx)", icon = icon("download"), class = "btn-success", width = "60%")
            ),
            # Add validation message
            uiOutput("analysis_error_msg"),
            # Add help text
            tags$div(
              class = "mt-4 text-muted",
              tags$h5(icon("question-circle"), " How to use:"),
              tags$ol(
                tags$li("Select a response variable"),
                tags$li("Click 'Get Results' to analyze"),
                tags$li("Download results or view in the panels")
              ),
              tags$p(
                icon("info-circle"),
                " Analysis includes descriptive statistics, normality tests, ANOVA, and post-hoc tests."
              ),
              tags$p(
                icon("exclamation-triangle"),
                " If data doesn't meet ANOVA assumptions, non-parametric tests will be used."
              )
            )
          ),
          mainPanel(
            bslib::accordion(
              width = "100%",
              bslib::accordion_panel(
                title = "Descriptive Statistics for Response Variable Selected",
                DT::dataTableOutput(outputId = "des_stat_id")
                # div(style = 'display: flex; justify-content: center;',
                #     downloadButton(outputId = 'download_des',label = 'Download Result',icon = icon('download'), class = 'btn-success',width = '60%'))
              )
            ), br(),
            bslib::accordion(
              width = "100%",
              bslib::accordion_panel(
                title = "Auto-normalization Info",
               # verbatimTextOutput(outputId = "powert_res"),
                verbatimTextOutput(outputId = "interpretation")
              ), br(),
              uiOutput(outputId = "non_para_section"),
              br(),
              uiOutput(outputId = "anova_section"),
              bslib::accordion(
                width = "100%",
                bslib::accordion_panel(
                  title = "Boxplot for Response Variable Selected",
                  fluidRow(
                    column(width = 3, textInput(inputId = "p_title", label = "Enter Plot Title", value = "My Experiment")),
                    column(width = 3, textInput(inputId = "x_title", label = "Enter X-axis Title", value = "Counts")),
                    column(width = 3, textInput(inputId = "y_title", label = "Enter Y-axis Title", value = "Treatments")),
                    column(width = 3, selectInput(inputId = "color_use", label = "Choose Fill Colour For Plots", choices = colors(), selected = "skyblue", multiple = FALSE))
                  ),
                  fluidRow(
                    column(width = 3, numericInput(inputId = "atext", label = "Enter Font Size for Axis Text", value = 14, min = 10, max = 30, step = 1)),
                    column(width = 3, numericInput(inputId = "atitle", label = "Enter Font Size for Axis Title", value = 14, min = 10, max = 30, step = 1)),
                    column(width = 3, numericInput(inputId = "ptitle", label = "Enter Font Size for Plot Title", value = 14, min = 10, max = 30, step = 1)),
                    # div(style = 'display: flex; justify-content: center;',
                    #                       actionButton(inputId = 'submit_3',label = 'Plot',icon = icon('rocket'), class = 'btn-primary',width = '70%'))
                  ), hr(), br(), addSpinner(plotOutput(outputId = "boxplot_id"), spin = "circle", color = "#E41A1C"),
                  div(
                    style = "display: flex; justify-content: center;",
                    downloadButton(outputId = "download_box", label = "Download Plot", icon = icon("download"), class = "btn-success", width = "60%")
                  )
                )
              ), br(),
              bslib::accordion(
                width = "100%",
                bslib::accordion_panel(
                  title = "Barplot  for Response Variable Selected",
                  fluidRow(
                    column(width = 3, textInput(inputId = "x_title_1", label = "Enter X-axis Title", value = "Treatments")),
                    column(width = 3, textInput(inputId = "y_title_1", label = "Enter Y-axis Title", value = "Means")),
                    column(width = 3, textInput(inputId = "p_title_1", label = "Enter Plot Title", value = "My Experiment"))
                  ),
                  fluidRow(
                    column(width = 3, numericInput(inputId = "atext_1", label = "Enter Font Size for Axis Text", value = 14, min = 10, max = 30, step = 1)),
                    column(width = 3, numericInput(inputId = "atitle_1", label = "Enter Font Size for Axis Title", value = 14, min = 10, max = 30, step = 1)),
                    column(width = 3, numericInput(inputId = "ptitle_1", label = "Enter Font Size for Plot Title", value = 14, min = 10, max = 30, step = 1)),
                    # div(style = 'display: flex; justify-content: center;',
                    #     actionButton(inputId = 'submit_4',label = 'Plot',icon = icon('rocket'), class = 'btn-primary',width = '70%'))
                  ), hr(), br(), addSpinner(plotOutput(outputId = "barplot_id"), spin = "circle", color = "#E41A1C"),
                  div(
                    style = "display: flex; justify-content: center;",
                    downloadButton(outputId = "download_bar", label = "Download Plot", icon = icon("download"), class = "btn-success", width = "60%")
                  )
                )
              )
            )
          )
        )
      ),
      nav_panel(title = 'See Growth Trend',
                sidebarLayout(
                  sidebarPanel(
                    fileInput(inputId = "file_upload_trend", label = "Upload PTC Datasheet", accept = c(".xlsx", ".xls", ".csv"), placeholder = "Data Sheet (Izzi Datasheet)"),
                    selectInput(inputId = 'parameter',label = 'Select Paramter',
                                choices = NULL,
                                multiple = FALSE),
                    input_switch(id = 'smooth',label = 'Smooth Trend',value = TRUE),
                    div(
                      style = "display: flex ; justify-content: center ;",
                      actionButton(
                        inputId = "submit_2", label = "Get trend",
                        width = "70%", class = "btn-primary",
                        icon = icon("upload")
                      )
                    )
                  ),
                  mainPanel(
                    bslib::accordion(
                      width = "100%",
                      bslib::accordion_panel(
                        title = "Growth Trend",
                        plotOutput(outputId = 'growth_plot'),
                        div(
                          style = "display: flex; justify-content: center;",
                          downloadButton(outputId = "download_trend", label = "Download Plot", icon = icon("download"), class = "btn-success", width = "60%")
                        )
                      )
                    )
                  )
                )       
                
                )
    )
  )
)

server <- function(input, output, session) {
  
  #Growth trend
  # Get the data.
  final_result <- reactive({
    req(input$file_upload_trend)
    Generate_trend_data(file_path = input$file_upload_trend$datapath)
  })
  

  observe({
    req(final_result())
    
    updateSelectInput(session,inputId = 'parameter',
                      choices = colnames(final_result())[4:ncol(final_result())])
  })
  
  # get plot
  plot_store <- reactiveVal()
  
  observeEvent(input$submit_2,{
    req(input$parameter , final_result())
    
    show_modal_spinner(
      spin =    "double-bounce",
      color = "#112446",
      text = 'Please wait...'
    )
    
    tryCatch({
    
     plot_store(Generate_trends(data = final_result() , parameter = input$parameter,smooth = input$smooth))  
     
    },finally = {
      Sys.sleep(5)
      remove_modal_spinner()
      show_alert(title = "Successfull", showCloseButton = TRUE, type = "success")
    })
    
  
    
  })
  
  #Print plot.
  output$growth_plot <- renderPlot({
    req(plot_store())
    print(plot_store())
    
  })
  
  # download trend.
  output$download_trend <- downloadHandler(
    filename = function() {
      paste(input$parameter, "_growth_trend.pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      print(plot_store())
      dev.off()
    }
  )
  
  # Function to read file and return output.
  # create a reactiveval
  hold_file <- reactiveVal()

  observeEvent(input$submit_1, {
    req(input$file_upload$datapath)
    show_modal_spinner(
      spin =    "double-bounce",
      color = "#112446",
      text = 'Please wait...'
    )
    
    
    
    tryCatch({
      hold_file(
        read_datasheet(file_path = input$file_upload$datapath)
      ) 
      
    },finally = {
      Sys.sleep(5)
      remove_modal_spinner()
      
      show_alert(title = "Successfull", showCloseButton = TRUE, type = "success")
    })
     
  })

  # Output for preview of data
  output$Preview <- DT::renderDT({
    req(hold_file())

    DT::datatable(hold_file(), options = list(pageLength = 10, scrollX = TRUE))
  })

  # Output of summary.
  sum_info <- reactive({
    req(hold_file())
    dat_sum(hold_file())
  })

  output$dat_sum <- DT::renderDT({
    DT::datatable(sum_info())
  })

  # Portion to populate select input.
  # get colum names
  all_col <- reactive({
    req(hold_file())
    get_colnames(hold_file())
  })

  observeEvent(hold_file(), {
    req(hold_file())

    updateSelectInput(session,
      inputId = "col_id",
      choices = all_col()[-c(1, 2)]
    )
  })

  # Portion for descriptive statistics calculation.

  dat_trim <- reactive({
    req(hold_file() , input$col_id)
    na_trim(data = hold_file(), res_name = input$col_id)
  })

  # Portion to perform descriptive statistics.
  des_react <- reactiveVal(value = NULL)
  observeEvent(input$submit_2, {
    req(dat_trim(), input$col_id)
    des_react(compute_descriptive_stats(data = dat_trim(), nam_col = input$col_id))

     
  })

  # Give an alert.
  observe({
    if (!is.null(des_react())) {
      show_alert(title = "Successfull", showCloseButton = TRUE, type = "success")
    }
  })

  # Output for descript stat.
  output$des_stat_id <- DT::renderDT({
    req(des_react)
    DT::datatable(des_react())
  })

  box_plot_res <- reactive({
    req(
      hold_file(), input$x_title,des_react(),
      input$y_title, input$p_title, input$atitle, input$atext,
      input$ptitle, input$col_id, input$color_use
    )


    plot_boxplot(
      data = hold_file(),
      x_title = input$x_title,
      y_title = input$y_title,
      Title = input$p_title,
      axis_title_size = input$atitle,
      axis_text_size = input$atext,
      plot_title_size = input$ptitle,
      nam_col = input$col_id,
      bar_colour = input$color_use
    )
  })

  # Output to return

  output$boxplot_id <- renderPlot({
    box_plot_res()
  })


  bar_plotresult <- reactive({
    req(
      des_react(), input$x_title_1,
      input$y_title_1, input$atitle_1, input$atext_1,
      input$ptitle_1, input$color_use
    )

    plot_graph(
      data = des_react(),
      x_title = input$x_title_1,
      y_title = input$y_title_1,
      Title = input$p_title_1,
      axis_title_size = input$atitle_1,
      axis_text_size = input$atext_1,
      plot_title_size = input$ptitle_1,
      bar_color = input$color_use
    )
  })

  # Output for bar plot.
  output$barplot_id <- renderPlot({
    req(bar_plotresult())

    bar_plotresult()
  })


  norma_data <- reactiveVal()
  # normalize data
  observeEvent(input$submit_2, {
    req(dat_trim(), input$col_id)
    norma_data(
      auto_normalize(data = dat_trim(), nam_col = input$col_id)
    )
  })

  # Display info on iterations.
  # output$powert_res <- renderPrint({
  #   req(norma_data())
  #   norma_data()$inform |> unlist()
  # })

  # Interpretation
  output$interpretation <- renderPrint({
    req(norma_data())
    interpret_yeojohnson(norma_data())
  })

  # Perform anova
  bulk_ano <- reactive({
    req(norma_data())
    anova_function(data = norma_data())
  })

  observeEvent(bulk_ano(), {
    req(bulk_ano())
    if (!is.null(bulk_ano()$non_parametric)) {
      output$non_para_section <- renderUI({
        bslib::accordion(
          width = "100%",
          bslib::accordion_panel(
            title = "Non-Parametric Test( Kruskal-wallis & Pairwise.wilcox_test )",
            DT::dataTableOutput(outputId = "non_para")
          )
        )
      })
    }
  })

  observeEvent(bulk_ano(), {
    req(bulk_ano())
    if (!is.null(bulk_ano()$Anova_result)) {
      output$anova_section <- renderUI({
        tagList(
          bslib::accordion(
            width = "100%",
            bslib::accordion_panel(
              title = "Analysis of Variance for Selected Response Variable Against Explanatory Variable",
              DT::dataTableOutput(outputId = "anov_res")
            )
          ),
          br(),
          bslib::accordion(
            width = "100%",
            bslib::accordion_panel(
              title = "Least Significance Difference Computed for Selected Response Variable Against Explanatory Variable",
              DT::dataTableOutput(outputId = "lsd_res")
            )
          ),
          br(),
          bslib::accordion(
            width = "100%",
            bslib::accordion_panel(
              title = "Tukey's HSD Computed for Selected Response Variable Against Explanatory Variable",
              DT::dataTableOutput(outputId = "Tukey_res")
            )
          ),
          br()
        )
      })
    }
  })


  # Output for non-parametric test.
  output$non_para <- renderDT({
    req(bulk_ano()$non_parametric)

    DT::datatable(bulk_ano()$non_parametric, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Output for anova result
  output$anov_res <- renderDT({
    req(bulk_ano()$Anova_result)
    DT::datatable(bulk_ano()$Anova_result, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Output for LSD
  output$lsd_res <- renderDT({
    req(bulk_ano()$LSD_result)
    DT::datatable(bulk_ano()$LSD_result, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Output for Tukey
  output$Tukey_res <- renderDT({
    req(bulk_ano()$Tukey_result)
    DT::datatable(bulk_ano()$Tukey_result, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Section to download descriptive statistics table.
  output$download_des <- downloadHandler(
    filename = function() {
      paste(input$col_id, "_Complete Analysis.xlsx") # File name
    },
    content = function(file) {
      writexl::write_xlsx(list_result(descript_stat = des_react(), bulk_ano()), file) # Save table as an Excel file
    }
  )



  # Section to download boxplot.
  output$download_box <- downloadHandler(
    filename = function() {
      paste(input$col_id, "_boxplot.pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      print(box_plot_res())
      dev.off()
    }
  )
  # Section to download barplot
  output$download_bar <- downloadHandler(
    filename = function() {
      paste(input$col_id, "_barplot.pdf") # File name
    },
    content = function(file) {
      pdf(file, width = 8, height = 6)
      print(bar_plotresult())
      dev.off()
    }
  )


  
  

  # Reactive expression for Single Factor Treatment
  preview_data_sf <- reactive({
    req(input$generate_sf)

    experiment_title <- input$experiment_title_sf
    researcher <- input$researcher_sf
    collection_frequency <- input$collection_frequency_sf
    num_records <- input$num_records_sf
    treatments <- strsplit(input$treatments_sf, ",")[[1]]
    replicates_per_treatment <- input$replicates_per_treatment_sf

    # Combine dropdown and manual parameters
    parameters <- c(input$parameters_sf, unlist(strsplit(input$additional_parameters_sf, ",")))
    parameters <- trimws(parameters) # Remove extra whitespace

    generate_single_factor_sheet(experiment_title, researcher, collection_frequency, num_records, treatments, replicates_per_treatment, parameters)
  })

  # Reactive expression for Factorial Treatment
  preview_data_f <- reactive({
    req(input$generate_f)

    experiment_title <- input$experiment_title_f
    researcher <- input$researcher_f
    collection_frequency <- input$collection_frequency_f
    num_records <- input$num_records_f
    replicates_per_treatment <- input$replicates_per_treatment_f

    # Combine dropdown and manual parameters
    parameters <- c(input$parameters_f, unlist(strsplit(input$additional_parameters_f, ",")))
    parameters <- trimws(parameters) # Remove extra whitespace

    # Collect treatments for each factor
    treatments <- lapply(1:input$num_factors, function(i) {
      strsplit(input[[paste0("treatments", i)]], ",")[[1]]
    })

    generate_factorial_sheet(experiment_title, researcher, collection_frequency, num_records, replicates_per_treatment, parameters, input$num_factors, treatments)
  })

  # Download handlers for Single Factor Treatment
  output$downloadData_sf <- downloadHandler(
    filename = function() {
      paste0(input$experiment_title_sf, "_", "Datasheet", ".xlsx")
    },
    content = function(file) {
      file.copy(preview_data_sf()$file, file)
    }
  )

  output$dataPreview_sf <- renderDT({
    req(preview_data_sf())
    datatable(preview_data_sf()$data, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Download handlers for Factorial Treatment
  output$downloadData_f <- downloadHandler(
    filename = function() {
      paste0(input$experiment_title_f, "_", "Datasheet", ".xlsx")
    },
    content = function(file) {
      file.copy(preview_data_f()$file, file)
    }
  )
  # Hello

  output$dataPreview_f <- renderDT({
    req(preview_data_f())
    datatable(preview_data_f()$data, options = list(pageLength = 10, scrollX = TRUE))
  })

  # Generate UI elements for treatment inputs based on the number of factors
  output$treatments_input <- renderUI({
    req(input$num_factors)
    treatment_inputs <- lapply(1:input$num_factors, function(i) {
      textInput(paste0("treatments", i), paste("Treatments Factor ", i, " (comma separated)"), paste0("Factor", i, "A, Factor", i, "B"))
    })
    do.call(tagList, treatment_inputs)
  })
}

shinyApp(ui, server)
