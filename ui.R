#####ICDE-Portal#####
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(vembedr)
library(readxl)
library(tippy)
library(shinyvalidate)
library(DT)
library(tidyverse)
library(shinyalert)


library(bslib)
################# CSS/STYLE/INFO #################
landing_panel <- "color: #333333;height: 200px; width: 260px"
spinner_color <- "#1d89ff"
sub_style <- "color:gray; font-style: italic; font-size: 13px;"

#function for additional test integration #

#function ends 

source("server.R")

# UI
shinyUI(
  div(style = 'background-color: ghostwhite',
      #main barcolor 
      tags$style(HTML("
    .navbar, .navbar-default {
      background-color: purple !important;
    }
  ")),
      
      #tabs clickable top bar color 
      tags$style(HTML("
          .navbar .nav > li.active > a {
            color: #ccccff !important; /* Adjust text color if needed */
          }
          
          .navbar .nav > li > a {
            color: black !important; /* Adjust text color if needed */
          }
          
          .navbar .nav > li > a:hover {
            color: #ccccff !important; 
          }
          
          .navbar .nav > li > a {
            font-size: 18px;
          }
        ")),
      tags$style(HTML("
    .navbar .navbar-brand {
      color: #ccccff !important;
    }
  ")),
      # --- Footer styles + footer bar ---
      tags$style(HTML("
  body { padding-bottom: 64px; }         /* room for fixed footer */
  .app-footer {
    position: fixed; bottom: 0; left: 0; right: 0;
    background: #f7f7f7; border-top: 1px solid #ccc;
    padding: 8px 16px; z-index: 1000; font-size: 14px;
  }
  .app-footer a { color: #6a1b9a; text-decoration: underline; }
  .app-footer .right { float: right; }
")),
      
      tags$footer(
        class = "app-footer",
        span(HTML("&copy; IC-CoDE Portal")),
        span(class = "right",
             actionLink("privacy_link", "Privacy Policy"),
             HTML("&nbsp;|&nbsp; Last updated: "), span(id = "pp_date", "Aug 2025")
        )
      ),
      
      navbarPage(
        theme = "portaltheme.css",
        title = "IC-CoDE",
        windowTitle = "IC-CoDE",
        #footer = div(align = "center", "© 2023", style = "margin-top:200px; margin-bottom:50px"),
        id = "TabDisplay",
        header = tagList(useShinydashboard()),
        
        tabPanel("Welcome", icon = icon("location-arrow"),
                 fluidRow(style = "background-color: ghostwhite;",
                          column(12, br(), align = "center",
                                 h1(strong("Welcome to the IC-CoDE Portal"), style = "color: #303030; font-size: 40px;"),
                                 h3("An interactive website to apply the International Classification of Cognitive Disorders in Epilepsy (IC-CoDE)", style = "color:#303030")),
                          br(),
                          br(),
                          column(12, align = "left",
                            div(width = 8, align = "center", style = "margin-bottom: 40px;",
                                shiny::a(img(src = "ICE-logo.jpg", width = "40%"), href = "", target = '_blank'))
                          ),
                          column(12, align = "center",   # Centering the button column
                                 actionButton("getStartedButton", "Get Started", 
                                              style = "font-size: 20px; color: white; background-color: purple; border-color: #303030;"))  # Styling the button
                 )
        ),
        
        
        #Guidelines Panel ####
        tabPanel("Instructions for Use", value = "guidelinetab",
                 div(style = "margin: 5% 10%; text-align:left; max-width:1200px;",
                     
                     div(
                       style = "margin-top:20px; text-align:center;",
                       h1("Tutorial"),
                     ),
                     
                     div(
                       style = "display:flex; justify-content:center; margin-top:10px;",
                       tags$video(
                         id = "tutorial",
                         type = "video/mp4",
                         src = "IC_CoDE_Instructions.mp4",
                         controls = NA,        
                         width = "800px",
                         autoplay = FALSE,
                         allowfullscreen = NA,
                         webkitallowfullscreen = NA,
                         mozallowfullscreen = NA
                       )
                     ),
                     
                     #### PART 1: INDIVIDUAL DATA ENTRY ####
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     h1("Part 1: Individual Data Entry"),
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Step 1 (image right)
                     fluidRow(
                       column(
                         6,
                         h4(tags$b("Step 1: Set Parameters")),
                         # Main bullets = disc; sub-bullets = circle (to match the template)
                         tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                 tags$li(HTML('Click the <b>“Individual”</b> tab')),
                                 tags$li(
                                   tagList(
                                     HTML('<b>Cutoff Selection:</b> Choose 1, 1.5, and/or 2 SD below the normative mean to define cognitive impairment.'),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li(HTML('<i>Tip:</i> Remove any others that you are not interested in using.'))
                                     )
                                   )
                                 ),
                                 tags$li(
                                   tagList(
                                     HTML('<b>Default Score Scale:</b> Choose from:'),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li('Standard score, scaled score, T-score, z-score, or percentile.'),
                                             tags$li(HTML('<i>Note:</i> This is only the default — you can change it per test.'))
                                     )
                                   )
                                 )
                         )
                       ),
                       column(6, align = "center",
                              tags$img(src = "instructions_1.png", 
                                       style = "width: 400px; height:auto; max-height:100%; object-fit:contain;")
                       )
                     ),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Step 2 (image right)
                     fluidRow(
                       column(
                         6,
                         h4(tags$b("Step 2: Enter Cognitive Test Data")),
                         p("Cognitive domains include:"),
                         tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                 
                                 # Language
                                 tags$li(
                                   tagList(
                                     tags$b("Language"),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li(HTML("<b>Subdomains:</b> Naming, Fluency")),
                                             tags$li(HTML("Example Tests: Boston Naming Test, Category Fluency, etc.")),
                                             tags$li(HTML("Enter the test name, score, and scale on which the test score is provided for this and all other domains.")),
                                             tags$li(HTML("<i>Note:</i> The test names provided are commonly used measures. If you do not use these measures, you may leave them blank or type over them to include the names of the tests you have available.")),
                                             tags$li(HTML("<i>Note:</i> The specific test and test version used does not matter. The calculator will work for any test that you enter into the Measure field."))
                                     )
                                   )
                                 ),
                                 
                                 # Memory
                                 tags$li(
                                   tagList(
                                     tags$b("Memory"),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li(HTML("<b>Subdomains:</b> Word List Recall, Story Recall, Word Pair Recall, Design Recall")),
                                             tags$li(HTML("Example Tests: CVLT, Logical Memory, BVMT, etc.")),
                                             tags$li(HTML("<i>Tip:</i> Make sure the score scale matches the data (e.g., T-score, z-score, scale score)."))
                                     )
                                   )
                                 ),
                                 
                                 
                                 # Executive Function
                                 tags$li(
                                   tagList(
                                     tags$b("Executive Function"),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li(HTML("<b>Subdomains:</b> Set-Shifting, Problem-Solving, Response Inhibition")),
                                             tags$li(HTML("Example Tests: Trail Making Test Part B, WCST")),
                                             tags$li(HTML("<i>Tip:</i> When possible, use scores from different measures within the same domain (e.g., Trail Making Test – Part B, WCST Perseverative Errors) rather than two scores from a single measure (e.g., WCST perseverative errors and conceptual level responses)."))
                                     )
                                   )
                                 ),
                                 
                                 # Attention / Processing Speed
                                 tags$li(
                                   tagList(
                                     tags$b("Attention/Processing Speed"),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li(HTML("<b>Subdomains:</b> Attention, Processing Speed")),
                                             tags$li(HTML("Example Tests: Digit Span, Trail Making Test Part A, Coding")),
                                             tags$li(HTML("<i>Tip:</i> Make sure the score scale matches your source (e.g., scaled scores from WAIS subtests, T-scores from Trail Making Test)."))
                                     )
                                   )
                                 ),
                                 
                                 # Visuospatial
                                 tags$li(
                                   tagList(
                                     tags$b("Visuospatial"),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li(HTML("<b>Subdomains:</b> Visuoconstruction, Visuoperception")),
                                             tags$li(HTML("Example Tests: Block Design, Judgment of Line Orientation")),
                                             tags$li(HTML("<i>Tip:</i> Adjust the scale type and include the relevant scores."))
                                     )
                                   )
                                 ),
                                 
                                 # Mood
                                 tags$li(
                                   tagList(
                                     tags$b("MOOD AND BEHAVIOR (Optional)"),
                                     tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                             tags$li(HTML("<b>Subdomains:</b> Depression, Anxiety, Behavior")),
                                             tags$li(HTML("Example Tests: BDI (Depression), BAI (Anxiety), CBCL (Behavior)")),
                                             tags$li(HTML("<i>Note:</i> The mood and behavior inventories are not used in phenotype classification, but may be included as potential modifiers."))
                                     )
                                   )
                                 )
                         ),
                         
                         # General tip and note (with red warning icon)
                         p(HTML("<i>General Tip:</i> It is recommended that users include tests of different types within a domain whenever possible (e.g., naming and fluency rather than two fluency tasks in the Language domain).")),
                         p(HTML(
                           '<i>General Note:</i> A small warning button ',
                           as.character(tags$i(class="fa fa-exclamation-triangle", style="color:red;")),
                           " may appear to the right of a test score to indicate that a score may have been entered incorrectly or the incorrect scale type selected. This is simply a prompt to check for errors; the calculator will still work if this warning button is present."
                         ))
                       ),
                       column(6, align = "center",
                              tags$img(src = "instructions_2.png", 
                                       style = "width: 400px; height:auto; object-fit:contain; margin-top:10px;")
                       )
                     ),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Step 3 (image right)
                     fluidRow(
                       column(6,
                              h4(tags$b("Step 3: Submit")),
                              tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                      tags$li(HTML("Scroll back up and <b>review all entries.</b>")),
                                      tags$li(HTML("Make sure at <b>least two tests per domain</b> are included in at <b>least four domains.</b>")),
                                      tags$li(HTML('Click <b>"Submit Entries"</b>.')),
                                      tags$li(
                                        HTML('The IC Code system will display:'),
                                        tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                                tags$li("Cutoff(s) used"),
                                                tags$li("Overall phenotype (e.g., Generalized, Bi-Domain Impairment)"),
                                                tags$li("Domains impaired"),
                                                tags$li("Any selected modifiers"),
                                                tags$li("Tests included in the phenotype calculation")
                                        )
                                      ),
                                      tags$li(HTML("You may download <b>results</b> or <b>reset</b> to enter a new patient/participant."))
                              )
                       ),
                       column(6, align = "center",
                              tags$img(src = "instructions_3.png", 
                                       style = "width: 400px; height:auto; object-fit:contain; margin-top:10px;")
                       )
                     ),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Individual Workflow Summary (centered)
                     h4(tags$b("Individual Data Entry – Workflow Summary")),
                     div(style = "text-align:left; margin:20px 0;",
                         tags$img(src = "instructions_4.png", 
                                  style = "width: 600px; height:auto; object-fit:contain;")
                     ),
                     br(),
                     br(),
                     br(),
                     
                     #### PART 2: GROUP DATA ENTRY ####
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     h1("Part 2: Group Data Entry"),
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Step 1 (image right)
                     fluidRow(
                       column(6,
                              h4(tags$b("Step 1: Set Parameters")),
                              tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                      tags$li(HTML('Select the <b>“Group”</b> tab')),
                                      tags$li(
                                        tagList(
                                          HTML('<b>Cutoff Selection:</b> Choose 1, 1.5, and/or 2 SD below the normative mean to define cognitive impairment.'),
                                          tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                                  tags$li(HTML('<i>Tip:</i> Remove any others that you are not interested in using.'))
                                          )
                                        )
                                      ),
                                      tags$li(
                                        tagList(
                                          HTML('<b>Default Score Scale:</b> Choose from:'),
                                          tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                                  tags$li('Standard score, T-score, z-score, scaled score, or percentile.'),
                                                  tags$li(HTML('<i>Note:</i> This is only the default — you can change it per test.'))
                                          )
                                        )
                                      )
                              )
                       ),
                       column(6, align = "center",
                              tags$img(src = "instructions_group_1.png", 
                                       style = "width: 400px; height:auto; object-fit:contain; margin-top:10px;")
                       )
                     ),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Step 2 (image right)
                     fluidRow(
                       column(6,
                              h4(tags$b("Step 2: Create Group Template")),
                              tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                      tags$li(HTML('Under the <b>Group</b> tab, click <b>Create Template.</b>')),
                                      tags$li(HTML('As described above under the <b>Individual Data</b> instructions, for each cognitive domain:')),
                                      tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                              tags$li(HTML("Select or enter <b>all tests</b> in your battery.")),
                                              tags$li(HTML("Set the correct <b>score scale</b> (T-score, z-score, scaled score).")),
                                              tags$li(HTML('Place a check in the “Include” box for all tests you want included in the cognitive phenotype generation.')),
                                              tags$li(HTML("If your test is not listed, <b>type it in</b>."))
                                      ),
                                      tags$li(HTML("Include <b>at least two tests per domain in at least four domains.</b>")),
                                      tags$li(HTML("<b>Enter Filters (optional).</b>")),
                                      tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                              tags$li(HTML('<i>Note:</i> You can use any of the provided filters or enter other filters of your own (simply type over the existing filter names). Be sure to check the “Include” box for any filters you want included in your dataset.'))
                                      ),
                                      tags$li(HTML('Once complete, click <b>Download Group Table Template.</b>'))
                              ),
                              p(HTML('<i>General Tip:</i> It is recommended that users include tests of different types within a domain whenever possible (e.g., naming and fluency tasks rather than two fluency tasks in the Language domain).'))
                       ),
                       column(6, align = "center",
                              tags$img(src = "instructions_group_2.png", 
                                       style = "width: 400px; height:auto; object-fit:contain; margin-top:10px;")
                       )
                     ),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Step 3 (image right)
                     fluidRow(
                       column(6,
                              h4(tags$b("Step 3: Populate Template")),
                              tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                      tags$li("Open the downloaded spreadsheet file."),
                                      tags$li("Enter data:"),
                                      tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                              tags$li(HTML("<b>Each row = one participant.</b>")),
                                              tags$li(HTML("<b>Each column = one test.</b>"))
                                      ),
                                      tags$li("Match scores to correct formats (e.g., scaled scores for WAIS subtests).")
                              ),
                              p(HTML('<i>General Note:</i> Do <u><b>not</b></u> modify the variable names in row 1 of the spreadsheet. These are required for the phenotype calculations to run correctly.')),
                              p(HTML('<i>General Note:</i> Some test scores entered may appear in <b style="color:red;">RED font</b> to indicate that a score may have been entered incorrectly or that the score entered may not match the scale selected for that score (e.g., standard score entered where there should be a scaled score). This is simply a prompt to check for errors; the calculator will still work if these warnings are present in a file uploaded to the calculator.'))
                       )
                     ),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Step 4 (image right)
                     fluidRow(
                       column(6,
                              h4(tags$b("Step 4: Upload and Generate Results")),
                              tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                      tags$li(HTML('Return to IC Code Portal → <b>Group tab.</b>')),
                                      tags$li(HTML('Click <b>“Upload Data and Generate Results.”</b>')),
                                      tags$li(HTML("Use <b>Browse</b> to select your completed template file.")),
                                      tags$li("Results will include:"),
                                      tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                              tags$li(HTML("Display results presented as pie chart(s) showing IC Code phenotype distribution(s) — if you hover over the single domain or bi-domain slices, you can see a breakdown of impaired domains.")),
                                              tags$li(HTML("Option to visualize pie charts by desired filters (e.g., sex, side of seizures).")),
                                              tags$li(HTML("Option to <b>download</b> the results in a spreadsheet format."))
                                      )
                              )
                       ),
                       column(6, align = "center",
                              tags$img(src = "instructions_group_3.png", 
                                       style = "width: 350px; height:auto; object-fit:contain; margin-top:10px;")
                       )
                     ), 
                     
                     br(),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     h1("BEST PRACTICES"),
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     fluidRow(
                       column(12,
                              tags$ul(style = "list-style-type: disc; padding-left: 20px; margin-left: 0;",
                                      tags$li("Make sure scores and scales match correctly (e.g., T-score = 50 mean, z-score = 0 mean)."),
                                      tags$li(
                                        tagList(
                                          "Double-check that:",
                                          tags$ul(style = "list-style-type: circle; padding-left: 20px; margin-top: 6px;",
                                                  tags$li(HTML('All included tests have scores (individual calculator) or that all included tests have the “Include” button checked (group calculator).')),
                                                  tags$li(HTML('All domains are sufficiently represented, ideally tapping into different constructs within the domain (e.g., fluency and naming for language).')),
                                                  tags$li(HTML('Be certain that there are <b>at least two tests</b> in <b>at least four domains</b>.')),
                                                  tags$li(HTML('Be sure that domains that are highly relevant for the patient group of interest (e.g., language and memory for temporal lobe epilepsy) are included.'))
                                          )
                                        )
                                      )
                              )
                       )
                     ),
                     br(),
                     
                     tags$hr(style = "border-top: 1.5px solid black; margin-top:10px; margin-bottom:10px;"),
                     
                     # Group Workflow Summary (centered)
                     h4(tags$b("Group Data Entry – Workflow Summary")),
                     div(style = "text-align:left; margin:20px 0;",
                         tags$img(src = "instructions_group_4.png", 
                                  style = "width: 900px; height:auto; object-fit:contain;")
                     ),
                 )
        ),
        
        #Calculator Panel ####
        tabPanel("Calculator", value = "calculatortab",
                 div(style = "margin: 5% 10%;", 
                     tags$style(HTML("
                     .box-header.with-border {
                              border: none;
                     }
                     .disbox .box.box-solid {
                              border-top: none;
                              border: 2px solid purple;
                    }")),
                     
                     div(class = "disbox",
                         box(
                           title = NULL,  # Remove default box title
                           style = "margin-top: -30px;",  # Keeps border, removes shadow
                           h2(strong("Terms of Use"), style = "text-align: justify;margin-top: 0px;"),
                           p("These Terms of Use (the 'Terms') govern your access to and use of websites, applications, and services (the 'Services') that are provided by The Cleveland Clinic Foundation ('Cleveland Clinic' or 'us' or 'we') and linked to these Terms. IF YOU DO NOT AGREE TO THESE TERMS, YOU MAY NOT USE THE SERVICES.", 
                             style = "text-align: justify; font-size: 16px; color: #333;"),
                           div(
                             style = "text-align: center; margin-top: 10px;",
                             actionButton(
                               inputId = "terms_of_use_info",
                               label = "View the Complete Terms of Use",
                               icon = icon("info"),
                               class = "btn",
                               style = "padding: 10px 20px; font-size: 14px; font-weight: bold; background-color: #6a1b9a; color: white; border: none;"
                             )
                           ),
                           width = 12
                         )
                     ),
                     
                     column(12, 
                            h2(strong("IC-CoDE Calculator")),
                            h4("Please answer the series of questions below to customize the IC-CoDE calculator in the way that best suits your research. You have the option to select how many tests you would like to use in each cognitive domain to generate cognitive phenotypes; however, you must have a minimum of at least 2 tests per cognitive domain for the calculator to generate IC-CoDE phenotypes. It is recommended that you include tests of different types within each cognitive domain whenever possible (e.g., naming and fluency tasks rather than two fluency tasks). For detailed instructions on using the IC-CoDE calculator, please use the “Instructions” tab above.", style = "line-height: 1.8; margin-bottom: 40px;"),
                       h3("Would you like to calculate cognitive phenotype(s) for an individual patient or for a group of patients?"),
                       tags$style(HTML("
                          .caltab .nav > li.active > a {
                            color: white !important; /* Adjust text color if needed */
                            background-color: purple  !important;
                          }
                          
                          .caltab .nav > li > a {
                            background-color: purple  !important;
                            color: black !important; /* Adjust text color if needed */
                          }
                          
                          .caltab .nav > li > a:hover {
                            background-color: purple  !important;
                            color: white !important; 
                          }
                          
                          .caltab .nav > li > a {
                            font-size: 18px;
                          }
                        ")),
                       div(class = "caltab",
                         tabsetPanel(
                           #individual group ####
                             tabPanel("Individual", 
                                      #box(width = 12,
                                        #div(style = "margin-bottom: 30px;"),
                                      br(),
                                        selectInput("cutoffSelection", 
                                                    label = h4("What cutoff would you like to use to define cognitive impairment on each cognitive measure (select all that apply; to remove a cutoff, hit the delete or backspace button on your keyboard)?"),
                                                    choices = c("1 standard deviation", 
                                                                "1.5 standard deviations", 
                                                                "2 standard deviations"),
                                                    selected = "1 standard deviation",
                                                    multiple = TRUE,
                                                    width = "100%" ),
                                      br(),
                                      selectInput("scaleSelection", 
                                                  label = h4("Which scale would you like to use at the default scale for score entry?"),
                                                  choices = c("Standard Score (M=100, SD=15)", 
                                                              "Scaled Score (M=10, SD=3)", 
                                                              "T-score (M=50, SD=10)",
                                                              "Z-score (M=0, SD=1)",
                                                              "Percentile Score (%tile)"),
                                                  selected = "Standard Score (M=100, SD=15)",
                                                  multiple = FALSE,
                                                  width = "100%" ),
                                      br(),
                                        h4("Please use the drop down menus below to select the measures in your neuropsychological battery and indicate the scale on which each score is reported (e.g., standard score, T-score). A higher test score always indicates a better test result. Several common measures are provided as examples, but these can be deleted or replaced with the measures you have available by simply typing over the measure name. The calculator will generate an IC-CoDE phenotype using only the measures for which you enter a score.", strong("PLEASE NOTE: If data is entered and then the sub-category is closed (i.e., user clicks the "),strong("(+)",style = "color:purple;"), strong(" sign), the entered data will delete. So, users are encouraged to leave all test lists expanded until they have generated the cognitive phenotype.")),
                                        #insert here the new code#
                                      useShinyjs(),
                                      tags$style("
                                          .custom-action-link {
                                            background-color: transparent;
                                            border: none;
                                            color: purple;
                                            font-weight: bold; 
                                            font-size: 20px; 
                                            padding: 0;
                                            margin: 0;
                                            margin-bottom: 10px;
                                          }
                                          .custom-action-link:hover,
                                          .custom-action-link:active,
                                          .custom-action-link:focus {
                                            background-color: transparent;
                                            color: black;
                                            outline: none;
                                            box-shadow: none;
                                          }
                                        "),
                                                                              
                                                        tags$head(
                                        tags$style(HTML("
                                          .custom-text-box {
                                            background-color: purple;
                                            color: white;
                                            font-weight: bold;
                                            padding: 10px 20px;
                                            border-radius: 5px;
                                            display: inline-block;
                                            margin: 5px;
                                            margin-top: 30px;
                                          }
                                        "))
                                      ),

                                      

                                      div(class = "custom-text-box", "LANGUAGE"),
                                      uiOutput("language_warning_ui"),
                                      
                                      column(12, #offset = 1,
                                             actionLink(inputId = "naming_btn", label = "Naming (+)", class = "custom-action-link", onclick = "toggleContent('naming_content')"),
                                             div(id = "naming_content", style = "display: none;", uiOutput("naming_content_ui")),
                                             br(),
                                             actionLink(inputId = "fluency_btn", label = "Fluency (+)", class = "custom-action-link", onclick = "toggleContent('fluency_content')"),
                                             div(id = "fluency_content", style = "display: none;", uiOutput("fluency_content_ui")),
                                      ),
                                      
                                      br(),
                                      div(class = "custom-text-box", "MEMORY"),
                                      uiOutput("memory_warning_ui"),
                                      
                                      column(12,# offset = 1,
                                             actionLink(inputId = "word_list_delayed_recall_btn", label = "Word List Delayed Recall (+)", class = "custom-action-link"),
                                             hidden(div(id = "word_list_delayed_recall_content", uiOutput("word_list_delayed_recall_content_ui"))),
                                             br(),
                                             actionLink(inputId = "sp_delay_recall_btn", label = "Story/Prose Delayed Recall (+)", class = "custom-action-link"),
                                             hidden(div(id = "sp_delay_recall_content", uiOutput("sp_delay_recall_content_ui"))),
                                             br(),
                                             actionLink(inputId = "wp_delay_recall_btn", label = "Word Pair Delayed Recall (+)", class = "custom-action-link"),
                                             hidden(div(id = "wp_delay_recall_content", uiOutput("wp_delay_recall_content_ui"))),
                                             br(),
                                             actionLink(inputId = "d_delay_recall_btn", label = "Design Delayed Recall (+)", class = "custom-action-link"),
                                             hidden(div(id = "d_delay_recall_content", uiOutput("d_delay_recall_content_ui"))),
                                      ),

                                      br(),
                                      div(class = "custom-text-box", "EXECUTIVE FUNCTION"),
                                      uiOutput("executive_function_warning_ui"),

                                      column(12,# offset = 1,
                                             actionLink(inputId = "ss_btn", label = "Set-Shifting (+)", class = "custom-action-link"),
                                             hidden(div(id = "ss_content", uiOutput("ss_content_ui"))),
                                             br(),
                                             actionLink(inputId = "ps_btn", label = "Problem-Solving (+)", class = "custom-action-link"),
                                             hidden(div(id = "ps_content", uiOutput("ps_content_ui"))),
                                             br(),
                                             actionLink(inputId = "ri_btn", label = "Response Inhibition (+)", class = "custom-action-link"),
                                             hidden(div(id = "ri_content", uiOutput("ri_content_ui"))),
                                      ),

                                      br(),
                                      div(class = "custom-text-box", "ATTENTION / PROCESSING SPEED"),
                                      uiOutput("attention_processing_speed_warning_ui"),

                                      column(12,# offset = 1,
                                             actionLink(inputId = "a_btn", label = "Attention (+)", class = "custom-action-link"),
                                             hidden(div(id = "a_content", uiOutput("a_content_ui"))),
                                             br(),
                                             actionLink(inputId = "pro_s_btn", label = "Processing Speed (+)", class = "custom-action-link"),
                                             hidden(div(id = "pro_s_content", uiOutput("pro_s_content_ui"))),
                                      ),

                                      br(),
                                      div(class = "custom-text-box", "VISUOSPATIAL"),
                                      uiOutput("visuospatial_warning_ui"),

                                      column(12,# offset = 1,
                                             actionLink(inputId = "vc_btn", label = "Visuoconstruction (+)", class = "custom-action-link"),
                                             hidden(div(id = "vc_content", uiOutput("vc_content_ui"))),
                                             br(),
                                             actionLink(inputId = "vp_btn", label = "Visuoperception (+)", class = "custom-action-link"),
                                             hidden(div(id = "vp_content", uiOutput("vp_content_ui"))),
                                      ),
                                      
                                      br(),
                                      div(class = "custom-text-box", "MOOD AND BEHAVIOR (optional)"),
                                      uiOutput("mood_symptoms_warning_ui"),
                                      
                                      column(12,# offset = 1,
                                             actionLink(inputId = "depression_btn", label = "Depression (+)", class = "custom-action-link", onclick = "toggleContent('depression_content')"),
                                             div(id = "depression_content", style = "display: none;", uiOutput("depression_content_ui")),
                                             br(),
                                             actionLink(inputId = "anxiety_btn", label = "Anxiety (+)", class = "custom-action-link", onclick = "toggleContent('anxiety_content')"),
                                             div(id = "anxiety_content", style = "display: none;", uiOutput("anxiety_content_ui")),
                                             br(),
                                             actionLink(inputId = "behavior_btn", label = "Behavior (+)", class = "custom-action-link", onclick = "toggleContent('behavior_content')"),
                                             div(id = "behavior_content", style = "display: none;", uiOutput("behavior_content_ui")),
                                      ),


                                      tags$style("
                                            .output_action_btn {
                                              border: 4px solid purple;
                                              background-color: white;
                                            }

                                            .output_action_btn:hover {
                                              border: 4px solid purple;
                                              background-color: white;
                                            }

                                            .output_action_btn:focus,
                                            .output_action_btn:active {
                                                background-color: purple !important;
                                                box-shadow: none !important;
                                                outline: none !important;
                                                color:white;
                                            }

                                            #results {
                                                font-size: 18px; /* Increase the font size */
                                                align: left;
                                            }
                                          "),

                                      tags$style(HTML('
  .output_action_btn {
    border: 4px solid purple;
    background-color: white;
    padding: 10px 20px; /* Optional: Adjust padding to your liking */
    font-size: 18px; /* Optional: Adjust font size to your liking */
    transition: background-color 0.3s; /* Smooth transition for background color */
  }
 .output_action_btn:hover {
    border: 4px solid purple;
    background-color: #f5f5dc; /* Beige background on hover */
    color: purple; /* Text color on hover */
  }

  .output_action_btn:active {
    border: 4px solid red;
    background-color: white !important; /* Beige background when active/clicked */
    color: white !important; /* Maintaining the text color when active/clicked */
  }
  .output_action_btn:focus,
  .output_action_btn:active {
    border: 4px solid purple;
    background-color: #f5f5dc !important; /* Beige background when active/clicked */
    color: purple !important; /* Text color maintained when active/clicked */
}


  .impairment-output-container {
    padding: 20px;
    text-align: left;
    background-color: #f5f5f5;
    border-left: 4px solid purple;
    margin-bottom: 20px;
  }
  .impairment-output-container h2,
  .impairment-output-container h3 {
    color: #800080; /* Purple color for headers */
    margin-bottom: 10px;
  }
  .impairment-summary {
    font-size: 16px;
    margin-bottom: 20px;
  }
  ul {
    list-style-type: none;
    padding-left: 0;
  }
  li {
    margin-bottom: 5px;
  }
')),


                                     column(8, align = "center",
                                       div(style = "margin-top: 40px; margin-bottom: 40px;",
                                           actionButton(inputId = "output_button", label = h3(strong("Submit entries")), class = "output_action_btn")
                                       )
                                     ),
                                     
                                     tags$style(HTML('
  .reset_action_btn {
    border: 4px solid red;
    background-color: white;
    padding: 10px 20px; /* Optional: Adjust padding to your liking */
    font-size: 18px; /* Optional: Adjust font size to your liking */
    transition: background-color 0.3s; /* Smooth transition for background color */
  }
  .reset_action_btn:hover {
    border: 4px solid red;
    background-color: #ffcccc; /* Light red background on hover */
    color: red; /* Text color on hover */
  }
  .reset_action_btn:active {
    border: 4px solid red;
    background-color: white !important; /* White background when active/clicked */
    color: white !important; /* Text color when active/clicked */
  }
  .reset_action_btn:focus,
  .reset_action_btn:active {
    border: 4px solid red;
    background-color: #ffcccc !important; /* Light red background when active/clicked */
    color: red !important; /* Text color when active/clicked */
  }
')),
                           
                                     column(4, align = "center",
                                            align = "center",
                                            div(style = "margin-top: 40px; margin-bottom: 40px;",
                                                actionButton(inputId = "reset_button", label = h3(strong("Reset")), class = "reset_action_btn"))
                                     ),
                                    
                                     column(12, align = "center",         
                                            div(style = "margin-top: 40px; margin-bottom: 40px;",
                                                hidden(div(id = "output_content", uiOutput("results"))),
                                                downloadButton("download_link", "Download HTML"),
                                            )
                                     ),
                                     p("When publishing manuscripts that make use of the IC-CoDE portal to generate phenotype data, we ask that you please include an acknowledgement. We recommend the following boilerplate language: IC-CoDE phenotype data were generated using the IC-CoDE Portal (IC-CoDE-Portal.ccf.org), which was funded by an American Epilepsy Society Infrastructure Grant (Award ID 1153665).")
                                     
                      
                                      
                             ),
                           #group #####
                             tabPanel("Group", 
                                      br(),
                                selectInput("cutoffSelection_MG", 
                                            label = h4("What cutoff would you like to use to define cognitive impairment on each cognitive measure (select all that apply; to remove a cutoff, hit the delete or backspace button on your keyboard)?"),
                                            choices = c("1 standard deviation", 
                                                        "1.5 standard deviations", 
                                                        "2 standard deviations"),
                                            selected = "1 standard deviation",
                                            multiple = TRUE,
                                            width = "100%" ),
                                br(),
                                selectInput("scaleSelection_MG", 
                                            label = h4("Which scale would you like to use at the default scale for score entry?"),
                                            choices = c("Standard Score (M=100, SD=15)", 
                                                        "Scaled Score (M=10, SD=3)", 
                                                        "T-score (M=50, SD=10)",
                                                        "Z-score (M=0, SD=1)",
                                                        "Percentile Score (%tile)"),
                                            selected = "Standard Score (M=100, SD=15)",
                                            multiple = FALSE,
                                            width = "100%" ),
                                br(),
                                
                                
                                h3("Please select an option below: create a data entry template or upload your completed data template to process and generate results"),
                                tabsetPanel(
                                  ##create template ####
                                  tabPanel("Create Template",
                                    h4("Please use the drop down menus below to select the measures in your neuropsychological battery and indicate the scale on which each score is reported (e.g., standard score, T-score). A higher test score always indicates a better test result. At the bottom, you also have the option of including data for mood questionnaires and filters, or covariates, you may want to use in your research (e.g., demographic and disease variables). Several common measures and filters are provided as examples, but these can be deleted or replaced (simply type over the existing measures or filter) with the measures or filters relevant to your research. ",tags$b("Please be sure to check the “Include” box to the right of each measure that you want included in your data entry template.")),
                                    h4(tags$b("Note:"),"Once you are done selecting your test list, you will be able to download your data entry template in an Excel format in which you can enter the relevant test scores and variables at your convenience. You can then return to the website with the completed data file at a later time and use the 'Upload Data and Generate Results' tab to generate cognitive phenotypes."),
                                    
                                    #Group UI 
                                    div(class = "custom-text-box", "LANGUAGE"),
                                    uiOutput("language_warning_ui_MG"),
                                    
                                    column(12,
                                           actionLink(inputId = "naming_btn_MG", label = "Naming (+)", class = "custom-action-link", onclick = "toggleContent('naming_content_MG')"),
                                           div(id = "naming_content_MG", style = "display: none;", uiOutput("naming_content_ui_MG")),
                                           br(),
                                           actionLink(inputId = "fluency_btn_MG", label = "Fluency (+)", class = "custom-action-link", onclick = "toggleContent('fluency_content_MG')"),
                                           div(id = "fluency_content_MG", style = "display: none;", uiOutput("fluency_content_ui_MG"))
                                    ),
                                    
                                    br(),
                                    div(class = "custom-text-box", "MEMORY"),
                                    uiOutput("memory_warning_ui_MG"),
                                    
                                    column(12,
                                           actionLink(inputId = "word_list_delayed_recall_btn_MG", label = "Word List Delayed Recall (+)", class = "custom-action-link"),
                                           hidden(div(id = "word_list_delayed_recall_content_MG", uiOutput("word_list_delayed_recall_content_ui_MG"))),
                                           br(),
                                           actionLink(inputId = "sp_delay_recall_btn_MG", label = "Story/Prose Delayed Recall (+)", class = "custom-action-link"),
                                           hidden(div(id = "story_prose_delayed_recall_content_MG", uiOutput("story_prose_delayed_recall_content_ui_MG"))),
                                           br(),
                                           actionLink(inputId = "wp_delay_recall_btn_MG", label = "Word Pair Delayed Recall (+)", class = "custom-action-link"),
                                           hidden(div(id = "word_pair_delayed_recall_content_MG", uiOutput("word_pair_delayed_recall_content_ui_MG"))),
                                           br(),
                                           actionLink(inputId = "d_delay_recall_btn_MG", label = "Design Delayed Recall (+)", class = "custom-action-link"),
                                           hidden(div(id = "design_delayed_recall_content_MG", uiOutput("design_delayed_recall_content_ui_MG")))
                                    ),
                                    
                                    br(),
                                    div(class = "custom-text-box", "EXECUTIVE FUNCTION"),
                                    uiOutput("executive_function_warning_ui_MG"),
                                    
                                    column(12,
                                           actionLink(inputId = "ss_btn_MG", label = "Set-Shifting (+)", class = "custom-action-link"),
                                           hidden(div(id = "set-shifting_content_MG", uiOutput("set-shifting_content_ui_MG"))),
                                           br(),
                                           actionLink(inputId = "ps_btn_MG", label = "Problem-Solving (+)", class = "custom-action-link"),
                                           hidden(div(id = "problem-solving_content_MG", uiOutput("problem-solving_content_ui_MG"))),
                                           br(),
                                           actionLink(inputId = "ri_btn_MG", label = "Response Inhibition (+)", class = "custom-action-link"),
                                           hidden(div(id = "response_inhibition_content_MG", uiOutput("response_inhibition_content_ui_MG")))
                                    ),
                                    
                                    br(),
                                    div(class = "custom-text-box", "ATTENTION / PROCESSING SPEED"),
                                    uiOutput("attention_processing_speed_warning_ui_MG"),
                                    
                                    column(12,
                                           actionLink(inputId = "a_btn_MG", label = "Attention (+)", class = "custom-action-link"),
                                           hidden(div(id = "attention___working_memory_content_MG", uiOutput("attention_content_ui_MG"))),
                                           br(),
                                           actionLink(inputId = "pro_s_btn_MG", label = "Processing Speed (+)", class = "custom-action-link"),
                                           hidden(div(id = "processing_speed_content_MG", uiOutput("processing_speed_content_ui_MG")))
                                    ),
                                    
                                    br(),
                                    div(class = "custom-text-box", "VISUOSPATIAL"),
                                    uiOutput("visuospatial_warning_ui_MG"),
                                    
                                    column(12,
                                           actionLink(inputId = "vc_btn_MG", label = "Visuoconstruction (+)", class = "custom-action-link"),
                                           hidden(div(id = "visuoconstruction_content_MG", uiOutput("visuoconstruction_content_ui_MG"))),
                                           br(),
                                           actionLink(inputId = "vp_btn_MG", label = "Visuoperception (+)", class = "custom-action-link"),
                                           hidden(div(id = "visuoperception_content_MG", uiOutput("visuoperception_content_ui_MG")))
                                    ),
                                    
                                    div(class = "custom-text-box", "MOOD AND BEHAVIOR (optional)"),
                                    uiOutput("mood_symptoms_warning_ui_MG"),
                                    
                                    column(12,
                                           actionLink(inputId = "depression_btn_MG", label = "Depression (+)", class = "custom-action-link", onclick = "toggleContent('depression_content_MG')"),
                                           div(id = "depression_content_MG", style = "display: none;", uiOutput("depression_content_ui_MG")),
                                           br(),
                                           actionLink(inputId = "anxiety_btn_MG", label = "Anxiety (+)", class = "custom-action-link", onclick = "toggleContent('anxiety_content_MG')"),
                                           div(id = "anxiety_content_MG", style = "display: none;", uiOutput("anxiety_content_ui_MG")),
                                           br(),
                                           actionLink(inputId = "behavior_btn_MG", label = "Behavior (+)", class = "custom-action-link", onclick = "toggleContent('behavior_content_MG')"),
                                           div(id = "behavior_content_MG", style = "display: none;", uiOutput("behavior_content_ui_MG"))
                                    ),
                                    
                                    div(class = "custom-text-box", "FILTERS (optional)"),
                                    
                                    column(12,
                                           actionLink(inputId ="filters_btn_MG", label ="Filters (+)",class="custom-action-link", onclick ="toggleContent('filters_content_MG')"),
                                           div(id ="filters_content_MG", style ="display: none;", uiOutput("filters_content_ui_MG"))),
                                    
                                    
                                    column(8, align = "left",         
                                           div(style = "margin-top: 40px; margin-bottom: 20px;",
                                               #hidden(div(id = "output_content", uiOutput("results"))),
                                               h4("Download your Excel data template (Note: The website does NOT save your Excel data template for future use, so please be sure to download and save it elsewhere until it is completed and ready to upload for analysis)."),
                                               downloadButton("download_table", "Download group table template"),
                                           )
                                    ),
                                    column(12),
                                  ),
                                  tabPanel("Upload Data and Generate Results",
                                           h4(HTML(paste0(tags$b("Upload completed Excel data template to generate IC-CoDE phenotypes."), "The website does not require any additional information to generate IC-CoDE phenotypes from a completed data template in which all scores have been entered. Once your Excel file has been uploaded, if the data loaded correctly, a blue bar will appear that says “Upload complete.” Please be patient as the system processes the data. Once the data are processed, you will receive a message saying, “File uploaded and processed successfully!”  You can then scroll down to the bottom of the page and hit the “DOWNLOAD RESULTS” button to download the file with IC-CoDE phenotypes. If after uploading your data, the page turns a light gray, this indicates that something is wrong with the uploaded file, and the data cannot be processed. Please double check your file to correct any errors before uploading again."))),
                                           #hidden(div(id = "output_content", uiOutput("results"))),
                                           fileInput("file_upload",label="",
                                                     accept = c("excel",".xlsx")),
                                #pop up download 
                                tags$script(HTML('
  $(document).on("shiny:connected", function(event) {
    Shiny.addCustomMessageHandler("triggerDownload", function(message) {
      setTimeout(function() {
        $("#" + message).click();
      }, 500); // Delay in milliseconds
    });
  });
')),
                                
                                
                                column(12,
                                       uiOutput("file_upload_status")),
                                
                                p("When publishing manuscripts that make use of the IC-CoDE portal to generate phenotype data, we ask that you please include an acknowledgement. We recommend the following boilerplate language: IC-CoDE phenotype data were generated using the IC-CoDE Portal (IC-CoDE-Portal.ccf.org), which was funded by an American Epilepsy Society Infrastructure Grant (Award ID 1153665).")
                                
                                
                                  ),
                                )#close group split panel


                             )#close tab panle group 
                         )
                       )
                     )
                              
                 )
        ),
        
        
        #Publications ####
        tabPanel("Publications", value = "publicationstab",
                 div(style = "margin: 5% 10%;", 
                     tags$style(HTML("
                     .box-header.with-border {
                              border: none;
                     }
                     .disbox .box.box-solid {
                              border-top: none;
                              border: 2px solid purple;
                    }")),
                   tabsetPanel(
                     #individual group ####
                     tabPanel("Publications", 
                       div(style = "margin: 1% 1%;", 
                           column(6,
                             box(width = 12,
                                 title = h2("Epilepsy", align = "center"),
                                 timelineBlock(reversed = FALSE, width = 12,
                                               timelineLabel(2025, color = "teal"),
                                               timelineItem(title = "Application of the International Classification of Cognitive Disorders in Epilepsy (IC-CoDE) to youths with drug-resistant epilepsy",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Ferguson et al.", href="https://pubmed.ncbi.nlm.nih.gov/40737958/", target = '_blank'),
                                               ),
                                               timelineLabel(2025, color = "teal"),
                                               timelineItem(title = "Application of the International Classification of Cognitive Disorders in Epilepsy (IC-CoDE) to youths with new and recent onset epilepsies",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Almane et al.", href="https://pubmed.ncbi.nlm.nih.gov/40700775/", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "Cortical Thickness Patterns of Cognitive Impairment Phenotypes in Drug-ResistantTemporal Lobe Epilepsy",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Miron et al.", href="https://onlinelibrary.wiley.com/doi/epdf/10.1002/ana.26893", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "Polygenic burden and its association with baseline cognitive function and postoperative cognitive outcome in temporal lobe epilepsy",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Arrotta et al.", href="https://pubmed.ncbi.nlm.nih.gov/38394790/", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "Validity of the MoCA as a cognitive screening tool in epilepsy: Are there implications for global care and research?",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Reyes et al.", href="https://pubmed.ncbi.nlm.nih.gov/38878272/", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "A user's guide for the International Classification of Cognitive Disorders in Epilepsy",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Hermann et al.", href="https://pubmed.ncbi.nlm.nih.gov/39141394/", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "Cross-cultural application of the international classification of cognitive disorders in epilepsy cognitive phenotypes in people with temporal lobe epilepsy in India",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Shah et al.", href="https://pubmed.ncbi.nlm.nih.gov/38878272/", target = '_blank'),
                                               ),
                                               timelineLabel(2023, color = "teal"),
                                               timelineItem(title = "Association of neighborhood deprivation with cognitive and mood outcomes in adults with pharmacoresistant temporal lobe epilepsy.",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Busch et al.", href="https://pubmed.ncbi.nlm.nih.gov/37076308/", target = '_blank'),
                                               ),
                                               timelineLabel(2023, color = "teal"),
                                               timelineItem(title = "The relationship between mood and anxiety and cognitive phenotypes in adults with pharmacoresistant temporal lobe epilepsy.",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Bingaman et al.", href="https://pubmed.ncbi.nlm.nih.gov/37814399/", target = '_blank'),
                                               ),
                                               timelineLabel(2023, color = "teal"),
                                               timelineItem(title = "Establishing the cross-cultural applicability of a harmonized approach to cognitive diagnostics in epilepsy: Initials results of the IC-CoDE in a Spanish-speaking sample.",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Reyes et al.", href="https://pubmed.ncbi.nlm.nih.gov/36625416/", target = '_blank'),
                                               ),
                                               timelineLabel(2023, color = "teal"),
                                               timelineItem(title = "Application of the International Classification of Cognitive Disorders in Epilepsy (IC-CoDE) to frontal lobe epilepsy using multi-center data.",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Arrotta et al.", href="https://pubmed.ncbi.nlm.nih.gov/37866248/", target = '_blank'),
                                               ),
                                               timelineLabel(2023, color = "teal"),
                                               timelineItem(title = "Development and application of the International Classification of Cognitive Disorders in Epilepsy (IC-CoDE): Initial results from a multi-center study of adults with temporal lobe epilepsy.",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("McDonald et al.", href="https://pubmed.ncbi.nlm.nih.gov/35084879/", target = '_blank'),
                                               ),
                                               timelineLabel(2021, color = "teal"),
                                               timelineItem(title = "Addressing neuropsychological diagnostics in adults with epilepsy: Introducing the International Classification of Cognitive Disorder in Epilepsy: The IC-Code Initiative.",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Norman et al.", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8166800/", target = '_blank'),
                                               ),
      
                                 )
                           )
                           ),# close column 
                           column(6,
                                  box(width = 12,
                                      title = h2("Other Disorders", align = "center"),
                                      timelineBlock(reversed = FALSE, width = 12,
                                                    timelineLabel(2025, color = "teal"),
                                                    timelineItem(title = "Longitudinal Study of Cognitive Phenotypes in Patients with Relapsing-Remitting Multiple Sclerosis",
                                                                 border = FALSE,
                                                                 #icon = icon("user"),
                                                                 #color = "yellow",
                                                                 time = shiny::a("Sousa et al.", href="https://pubmed.ncbi.nlm.nih.gov/39964061/", target = '_blank'),
                                                    ),
                                                    timelineLabel(2025, color = "teal"),
                                                    timelineItem(title = "Cognitive phenotypes in patients with relapsing-remitting multiple sclerosis with different disease duration, applying the international classification of cognitive disorders in MS (IC-CoDiMS)",
                                                                 border = FALSE,
                                                                 #icon = icon("user"),
                                                                 #color = "yellow",
                                                                 time = shiny::a("Sousa et al.", href="https://pubmed.ncbi.nlm.nih.gov/38715441/", target = '_blank'),
                                                    ),
                                                    timelineLabel(2024, color = "teal"),
                                                    timelineItem(title = "Cognitive profile in multiple sclerosis and post-COVID condition: a comparative study using a unified taxonomy",
                                                                 border = FALSE,
                                                                 #icon = icon("user"),
                                                                 #color = "yellow",
                                                                 time = shiny::a("Delgado-Alonso et al.", href="https://pubmed.ncbi.nlm.nih.gov/39366169/", target = '_blank'),
                                                    ),
                                                    timelineLabel(2024, color = "teal"),
                                                    timelineItem(title = "European cross-cultural neuropsychological test battery (CNTB) for the assessment of cognitive impairment in multiple sclerosis: Cognitive phenotyping and classification supported by machine learning techniques",
                                                                 border = FALSE,
                                                                 #icon = icon("user"),
                                                                 #color = "yellow",
                                                                 time = shiny::a("Delgado-´Alvarez et al.", href="https://pubmed.ncbi.nlm.nih.gov/39366169/", target = '_blank'),
                                                    ),
                                                    timelineLabel(2023, color = "teal"),
                                                    timelineItem(title = "Development of criteria for cognitive dysfunction in post-COVID syndrome: the IC-CoDi-COVID approach.",
                                                                 border = FALSE,
                                                                 #icon = icon("user"),
                                                                 #color = "yellow",
                                                                 time = shiny::a("Matias-Guiu et al.", href="https://pubmed.ncbi.nlm.nih.gov/36521337/", target = '_blank'),
                                                    ),
                                                    
                                                    timelineLabel(2023, color = "teal"),
                                                    timelineItem(title = "A proposed new taxonomy of cognitive phenotypes in multiple sclerosis: The International Classification of Cognitive Disorders in MS (IC-CoDiMS).",
                                                                 border = FALSE,
                                                                 #icon = icon("user"),
                                                                 #color = "yellow",
                                                                 time = shiny::a("Hancock et al.", href="https://pubmed.ncbi.nlm.nih.gov/36239099/", target = '_blank'),
                                                    )
                                      ))
                           ),
                           column(12),
                       )# close div 
                     ),#close tabpanel 
                     tabPanel("Editorials", 
                              div(style = "margin: 1% 1%;", 
                                  column(6,
                                         box(width = 12,
                                             title = h2("", align = "center"),
                                             timelineBlock(reversed = FALSE, width = 12,
                                                           timelineLabel(2024, color = "teal"),
                                                           timelineItem(title = "Hiding in plain sight – Neighborhood versus individual determinants of psychological outcomes in patients with epilepsy.",
                                                                        border = FALSE,
                                                                        #icon = icon("user"),
                                                                        #color = "yellow",
                                                                        time = shiny::a("Terman et al.", href="https://journals.sagepub.com/doi/full/10.1177/15357597231223588", target = '_blank'),
                                                           ),
                                                           timelineLabel(2024, color = "teal"),
                                                           timelineItem(title = "Depression associated with worse cognitive phenotype in temporal lobe epilepsy.",
                                                                        border = FALSE,
                                                                        #icon = icon("user"),
                                                                        #color = "yellow",
                                                                        time = shiny::a("Physiciansweekly", href="https://www.physiciansweekly.com/depression-associated-with-worse-cognitive-phenotype-in-temporal-lobe-epilepsy/", target = '_blank'),
                                                           ),
                                                           timelineLabel(2023, color = "teal"),
                                                           timelineItem(title = "Breaking the CoDE of cognitive disorders in epilepsy.",
                                                                        border = FALSE,
                                                                        #icon = icon("user"),
                                                                        #color = "yellow",
                                                                        time = shiny::a("Widdess-Walsh et al.", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10273814/", target = '_blank'),
                                                           ),
                                                           timelineLabel(2023, color = "teal"),
                                                           timelineItem(title = "For people with epilepsy, neighborhood may be tied to memory, mental health.",
                                                                        border = FALSE,
                                                                        #icon = icon("user"),
                                                                        #color = "yellow",
                                                                        time = shiny::a("American Academy of Neurology", href="https://www.aan.com/PressRoom/Home/PressRelease/5072", target = '_blank'),
                                                           ),
                                                           timelineLabel(2023, color = "teal"),
                                                           timelineItem(title = "The importance of understanding neighborhood environments in neurology care.",
                                                                        border = FALSE,
                                                                        #icon = icon("user"),
                                                                        #color = "yellow",
                                                                        time = shiny::a("Kobau et al.", href="https://pubmed.ncbi.nlm.nih.gov/37076311/", target = '_blank'),
                                                           ),
                                                           timelineLabel(2023, color = "teal"),
                                                           timelineItem(title = "Update in progress: Cognitive phenotypes in temporal lobe epilepsy.",
                                                                        border = FALSE,
                                                                        #icon = icon("user"),
                                                                        #color = "yellow",
                                                                        time = shiny::a("Sarkis et al.", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10805095/", target = '_blank'),
                                                           )
                                             )#close time block 
                                  )
                              )
                            )
                     )#close tabpanel 
                   )#close tabsetpanel 
                 )#clsoe div class 
        ),
        
        
        #About Panel ####
        tabPanel("About", value = "abouttab",
                 div(style = "margin: 5% 10%; text-align:justify; max-width:1400px;",   # This will create margins on top/bottom and left/right
                     
                     h2(HTML("<u> The International Classification of Cognitive Disorders in Epilepsy (IC-CoDE) </u>"), style = "margin-bottom: 30px;"),  
                     br(),
                     p(h4(HTML("The IC-CoDE represents a consensus-based, empirically-driven approach to diagnosing cognitive disorders in adults with epilepsy. It was developed in 2020 through a memorandum of understanding (MOU) between the International League Against Epilepsy (ILAE) Neuropsychology Task force and the International Neuropsychological Society (INS). The main goal of the IC-CoDE is to accelerate global research in the neuropsychology of epilepsy by providing an internationally-applicable framework for cognitive diagnostics in epilepsy with clear operational criteria and established impairment cut-offs. The IC-CoDE was developed as a guide for harmonizing multi-site cognitive research in epilepsy. It has not been validated as a diagnostic tool for individual patients in clinical settings.
                               <br>
                               <br>
                               The IC-CoDE framework was originally tested in a large, multicenter cohort of 2,485 adults with temporal lobe epilepsy and subsequently in a multicenter cohort of 455 with frontal lobe epilepsy. The IC-CoDE is now currently being tested in youth with epilepsy and in adults at international sites. IC-CoDE has not yet been applied to other epilepsy syndromes (e.g., juvenile myoclonic epilepsy, absence seizures). IC-CoDE has also been modified and applied to several other disorders outside of epilepsy (e.g., multiple sclerosis, COVID-19).  A list of published studies that have used IC-CoDE is provided under the Publications tab above."))),
                     h2(HTML("<u> Initial Considerations for IC-CoDE Users </u>"), style = "margin-bottom: 30px;"), 
                     h4(HTML("The goal of the IC-CoDE is to apply the cognitive model (Figure 1) using the operational definitions provided in order to arrive at a diagnostic cognitive phenotype (Figure 2). Step-by-step instructions for using the IC-CoDE calculator are provided below. For more information refer to Norman et al., 2021, <i>Epilepsia</i>, McDonald et al., 2023, <i>Neuropsychology</i>, and Hermann et al., 2024, <i>Epileptic Disorders</i>.")),
                     br(),
                     HTML('<img src="GuidelineFigure1.jpeg" alt="Your Image" width="500", max-height="300" />'),
                     HTML('<img src="GuidelineFigure2.jpeg" alt="Your Image" width="860", max-height="300" />'),
                     br(),
                     br(),
                     h2(HTML("<u>Funding</u>"), style = "margin-bottom: 30px;"), 
                     h4(HTML("Funding for development of the IC-CoDE Portal was provided by an American Epilepsy Society Infrastructure Award (Award ID 1153665).")),
                     h2(HTML("<u>Privacy policy</u>"), style = "margin-bottom: 30px;"), 
                     p(h4("We value your privacy. The full policy is available ",
                       actionLink("privacy_link_about", "here"), ". "
                     ))
                 )
        ),
        
        #FAQ Panel####
        tabPanel("FAQ", value = "faqtab",
                 
               div(style = "margin: 5% 10%;text-align:justify; max-width:1100px;",   # This will create margins on top/bottom and left/right
                     
                     h2(HTML("<u> Frequently Asked Questions </u>"), style = "margin-bottom: 30px;"), 
                   
                   h4(HTML("<b>How to cite the IC-CODE Portal</b>
                             <br>
                             <br>
                             When publishing manuscripts that make use of the IC-CoDE portal to generate phenotype data, we ask that you please include an acknowledgement. We recommend the following boilerplate language: IC-CoDE phenotype data were generated using the IC-CoDE Portal (IC-CoDE-Portal.ccf.org), which was funded by an American Epilepsy Society Infrastructure Grant (Award ID 1153665).

                             <br>
                             <br>")),
                     
                     h4(HTML("<b>Can the IC-CoDE calculator be used for clinical purposes?</b>
                             <br>
                             <br>
                             At present, IC-CoDE is recommended only for use in the context of research. While the longer-term goal is to make IC-CoDE applicable for clinical use, more research is needed before IC-CoDE can be used in this manner. For users interested in early clinical application, IC-CoDE should only be used in the context of contemporary clinical standards and practices.
                             <br>
                             <br>")),
                     
                     
                     h4(HTML("<b> What normative data should be used to generate IC-CoDE phenotypes?</b>
                             <br>
                             <br>
                             The IC-CoDE does not require use of specific normative data. We encourage each center to carefully select and apply normative data based on best practices (e.g., examination of psychometric properties, clinical characteristics that align with the patient sample, etc.). Research is underway to better understand several important issues, i.e., how varied selection of normative data across centers may impact IC-CoDE phenotypes and how the number of tests per domain as well as variable base rate sensitivities of specific tests may impact diagnostic outcomes. 
                             <br>
                             <br>")),
                   
                   h4(HTML("<b> Can the IC-CoDE calculator be used if patients in your sample completed a different version of the same test (e.g., CVLT, CVLT-2)?</b>
                             <br>
                             <br>
                The IC-CoDE is not a test-specific taxonomy; thus, different versions of the same test can be used interchangeably. However, research is ongoing to better understand the influence different tests and test versions may have on IC-CoDE phenotypes. 
                             <br>
                             <br>")),
                   
                   h4(HTML("<b> Can the IC-CoDE calculator be used in children with epilepsy?</b>
                             <br>
                             <br>
                Yes!  The IC-CoDE calculator can easily be applied to children by simply entering data for pediatric measures. The default measures provided in the calculator are only examples. The user can replace these default measures with any measure they would like.  For example, under naming, they could replace the Boston Naming Test with the Expressive One Word Vocabulary Test if they have that measure of naming for a child they assessed. Similarly, under Attention, they could replace the WAIS Digit Span score with the WISC or WPPSI Digit Span score or any other measure of attention in their pediatric battery. 
                             <br>
                             The IC-CoDE has recently been applied to children with new and recent onset idiopathic epilepsies as well as to those with pharmacoresistant focal epilepsies. These findings were recently published, and the references can be found under the “Publications” tab. 
                             <br>
                             <br>")),
                   
                   h4(HTML("<b>Can the IC-CoDE calculator be applied to patient populations outside of epilepsy? </b>
                             <br>
                             <br>
                The IC-CoDE taxonomy has been applied to several populations outside of epilepsy, including adults with multiple sclerosis and adults with COVID-19. Findings from these studies have been published in scientific journals, and references are provided under the “Publications” tab above.
                             <br>
                             <br>")),
                   
                   h4(HTML("<b>How many cognitive domains are required to generate IC-CoDE phenotypes?  </b>
                             <br>
                             <br>
                Ideally, at least two cognitive measures from all 5 cognitive domains (i.e., language, memory, executive, visuospatial, attention/speed) should be used when generating IC-CoDE phenotypes. However, in instances where there were not enough measures administered within each domain, it may be possible to generate an IC-CoDE phenotype with only 4 cognitive domains. However, careful consideration should be given to the patient population of interest to ensure that an “essential” cognitive domain is not omitted (i.e., domains known to have a high base rate of impairment in that population). For example, memory and language would be considered essential domains in temporal lobe epilepsy and visuospatial would be considered an essential domain for parietal lobe epilepsy.  We do not recommend generating IC-CoDE phenotypes in any patient with fewer than 4 cognitive domains.
                             <br>
                             <br>")),
                   
                   h4(HTML("<b> Can the IC-CoDE calculator be applied to patient populations outside of the United States? </b>
                             <br>
                             <br>
                Yes! The IC-CoDE calculator can be applied to patient populations in other regions and to patients who were assessed in other languages. The default measures provided in the calculator are only examples. The user can replace these default measures with any measure they would like.  For example, under naming, they could replace the Boston Naming Test with whatever measure they used to assess naming in whatever region or language the patient was evaluated. Similarly, under Attention, they could replace the WAIS Digit Span score with whatever measure they used to assess attention in their neuropsychological battery.
                             <br>
                             To date, the IC-CoDE has been tested with temporal lobe epilepsy patients in Mumbai, India where there is considerable language and cultural diversity. The findings were published in Shah et al., 2024. Epilepsia. We are currently working with other research groups in South Africa and Japan, who are validating the IC-CoDE in their local samples.
                             <br>
                             <br>")),
                   
                   h4(HTML("<b> Can the IC-CoDE calculator be applied to bilingual/multilingual patient populations? </b>
                             <br>
                             <br>
                Yes! The IC-CoDE taxonomy was validated in a multilingual sample from Mumbai, India that included individuals who were bilingual and multilingual. Notably, we recommend that researchers interpret the calculator findings, particularly verbally-mediated cognitive domains in the context of the procedures used for testing (e.g., language of testing, tests and norms used) and the degree of bilingualism/multilingualism of the patient or sample.
                             <br>
                             <br>")),
                   
                   h4(HTML("<b> Can the IC-CoDE calculator be applied to non-English monolingual speakers of other languages?</b>
                             <br>
                             <br>
                The IC-CoDE was validated in a sample of Spanish-speaking patients with temporal lobe epilepsy. The findings were published in Epilepsia, Reyes et al., 2023. Although researchers can use any test, as the IC-CoDE is not a test-specific taxonomy, we recommend that the battery used for non-English monolingual speakers follows the same or similar cognitive domains outlined in the IC-CoDE Taxonomy. We recognize that there is a dearth of neuropsychological tests available in other languages, and we hope that the flexibility of the taxonomy allows for validation of the IC-CoDE across other languages and cultural groups. 
                             <br>
                             <br>")),
                   
                   h4(HTML("<b>How do you use the IC-CoDE modifiers?</b>
                             <br>
                             <br>
                We have examined the relationship between depression and anxiety and IC-CoDE phenotypes, and results were published in Epilepsia, Bingaman et al., 2023.  While we have included the opportunity to code mood in your dataset, the influence of mood will not automatically be explored in this IC-CoDE version. Thus, interpretation and further exploration is up to individual investigators. 
                             <br>
                             <br>")),
                   
                   h4(HTML("<b>Is IC-CoDE technical assistance available?</b>
                             <br>
                             <br>
                Technical assistance is not currently available, but detailed instructions are provided under the “Instructions for Use” tab above to aid the user in navigating the IC-CoDE portal and calculator. 
                             <br>
                             <br>")),
                   
                     
                 )
        )
        
        
        
        
      )
  )
)

