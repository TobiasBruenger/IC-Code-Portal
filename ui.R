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
                                 h3("An interactive website to apply the international classification of cognitive disorders in epilepsy (IC-CoDE)", style = "color:#303030")),
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
        tabPanel("Guidelines", value = "guidelinetab",
                 div(style = "margin: 5% 10%; text-align:justify; max-width:1400px;", 
                    h2(HTML("<u> Initial Considerations for IC-CoDE Users </u>"), style = "margin-bottom: 30px;"), 
                    h4(HTML("The goal of the IC-CoDE is to apply the cognitive model (Figure 1) using the operational definitions provided in order to arrive at a diagnostic cognitive phenotype (Figure 2). Step-by-step instructions for using the IC-CoDE calculator are provided below. For more information refer to Norman et al., 2021, <i>Epilepsia</i> and McDonald et al., 2023, <i>Neuropsychology</i>.")),
                    br(),
                    HTML('<img src="GuidelineFigure1.jpeg" alt="Your Image" width="500", max-height="300" />'),
                    HTML('<img src="GuidelineFigure2.jpeg" alt="Your Image" width="860", max-height="300" />'),
                    br(),
                    br(),
                    h4(HTML("<b> Step 1: </b> Use all available neuropsychological data to construct the five target cognitive domains (i.e., language, memory, executive, visuospatial, attention/speed) consisting of at least two test metrics per cognitive domain.
Note: Ideally, cognitive measures from all 5 cognitive domains should be used when generating IC-CoDE phenotypes. However, in instances where there were not enough measures administered within each domain, it may be possible to general an IC-CoDE phenotype with only 4 cognitive domains. However, careful consideration should be given to the patient population of interest to ensure that an “essential” cognitive domain is not omitted (i.e., domains known to have a high base rate of impairment in that population). For example, memory and language would be considered essential domains in temporal lobe epilepsy and visuospatial would be considered an essential domain for parietal lobe epilepsy. 
                           <br> <br>
                           <b> Step 2: </b> If there are more than 2 measures in a given cognitive domain and control or normative data are available, we recommended including the 2 most sensitive measures (i.e., those with the highest base rate of impairment) to generate cognitive phenotypes.
                           <br> <br>
                           <b> Step 3: </b> Decide on the operational definition of abnormality to be used (-1 or -1.5 or -2SD)"))
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
                       box(title = h2("Legal disclaimer placeholder", align = "center"),
                           width = 12), 
                     ),
                     column(12, 
                            h2(strong("Directions")),
                            h4("Please answer the series of questions below to customize the IC-CoDE calculator in the way that best suits your research. You have the option to select how many tests you would like to use in each cognitive domain to generate cognitive phenotypes; however, you must have a minimum of at least 2 tests per cognitive domain for the calculator to generate IC-CoDE phenotypes. It is recommended that you include tests of different types within each cognitive domain whenever possible (e.g., naming and fluency tasks rather than two fluency tasks).", style = "line-height: 1.8; margin-bottom: 40px;"),
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
                                                    label = h4("What cutoff would you like to use to define cognitive impairment on each cognitive measure?"),
                                                    choices = c("1 standard deviation", 
                                                                "1.5 standard deviations", 
                                                                "2 standard deviations"),
                                                    selected = "1 standard deviation",
                                                    multiple = TRUE,
                                                    width = "100%" ),
                                      br(),
                                      selectInput("scaleSelection", 
                                                  label = h4("Which scale would you like to use at the default scale for score entry"),
                                                  choices = c("Standard Score (M=100, SD=15)", 
                                                              "Scaled Score (M=10, SD=3)", 
                                                              "T-score (M=50, SD=10)",
                                                              "Z-score (M=0, SD=1)",
                                                              "Percentile Score (%tile)"),
                                                  selected = "Standard Score (M=100, SD=15)",
                                                  multiple = FALSE,
                                                  width = "100%" ),
                                      br(),
                                        h3("Please use the drop down menus below to select the measures in your neuropsychological battery and indicate the scale on which each score is reported (e.g., standard score, T-score). A higher test score always indicates a better test result. Please put a check mark next to all measures you would like to include in the phenotype."),
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
                                             actionLink(inputId = "a_btn", label = "Attention / Working Memory (+)", class = "custom-action-link"),
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
                                      div(class = "custom-text-box", "Mood Symptoms (optional)"),
                                      uiOutput("mood_symptoms_warning_ui"),
                                      
                                      column(12,# offset = 1,
                                             actionLink(inputId = "depression_btn", label = "Depression (+)", class = "custom-action-link", onclick = "toggleContent('depression_content')"),
                                             div(id = "depression_content", style = "display: none;", uiOutput("depression_content_ui")),
                                             br(),
                                             actionLink(inputId = "anxiety_btn", label = "Anxiety (+)", class = "custom-action-link", onclick = "toggleContent('anxiety_content')"),
                                             div(id = "anxiety_content", style = "display: none;", uiOutput("anxiety_content_ui")),
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
                                     )
                                     
                      
                                      
                             ),
                           #group #####
                             tabPanel("Group", 
                                      br(),
                                selectInput("cutoffSelection_MG", 
                                            label = h4("What cutoff would you like to use to define cognitive impairment on each cognitive measure?"),
                                            choices = c("1 standard deviation", 
                                                        "1.5 standard deviations", 
                                                        "2 standard deviations"),
                                            selected = "1 standard deviation",
                                            multiple = TRUE,
                                            width = "100%" ),
                                br(),
                                selectInput("scaleSelection_MG", 
                                            label = h4("Which scale would you like to use at the default scale for score entry"),
                                            choices = c("Standard Score (M=100, SD=15)", 
                                                        "Scaled Score (M=10, SD=3)", 
                                                        "T-score (M=50, SD=10)",
                                                        "Z-score (M=0, SD=1)",
                                                        "Percentile Score (%tile)"),
                                            selected = "Standard Score (M=100, SD=15)",
                                            multiple = FALSE,
                                            width = "100%" ),
                                br(),
                                h3("Please use the drop down menus below to select the measures in your neuropsychological battery and indicate the scale on which each score is reported (e.g., standard score, T-score)."),
                                h3(tags$b("Note:"),"Once you've filled in the Excel template, you can upload it directly using the form below, no further specifications below are needed."),
                                
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
                                       actionLink(inputId = "a_btn_MG", label = "Attention / Working Memory (+)", class = "custom-action-link"),
                                       hidden(div(id = "attention___working_memory_content_MG", uiOutput("attention___working_memory_content_ui_MG"))),
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
                                
                                div(class = "custom-text-box", "Mood Symptoms"),
                                uiOutput("mood_symptoms_warning_ui_MG"),
                                
                                column(12,
                                       actionLink(inputId = "depression_btn_MG", label = "Depression (+)", class = "custom-action-link", onclick = "toggleContent('depression_content_MG')"),
                                       div(id = "depression_content_MG", style = "display: none;", uiOutput("depression_content_ui_MG")),
                                       br(),
                                       actionLink(inputId = "anxiety_btn_MG", label = "Anxiety (+)", class = "custom-action-link", onclick = "toggleContent('anxiety_content_MG')"),
                                       div(id = "anxiety_content_MG", style = "display: none;", uiOutput("anxiety_content_ui_MG"))
                                ),
                                
                                div(class = "custom-text-box", "Filter options"),
                                
                                column(12,
                                       actionLink(inputId ="filters_btn_MG", label ="Filters (+)",class="custom-action-link", onclick ="toggleContent('filters_content_MG')"),
                                       div(id ="filters_content_MG", style ="display: none;", uiOutput("filters_content_ui_MG"))),
                                
                                
                                column(8, align = "left",         
                                       div(style = "margin-top: 40px; margin-bottom: 20px;",
                                           #hidden(div(id = "output_content", uiOutput("results"))),
                                           h4("Download excel table template"),
                                           downloadButton("download_table", "Download group table template"),
                                       )
                                ),
                                column(12),
                                
                                column(8, align = "left",         
                                       div(style = "margin-top: 0px; margin-bottom: 40px; width:100%;",
                                           #hidden(div(id = "output_content", uiOutput("results"))),
                                           fileInput("file_upload", h4(HTML(paste0(tags$b("Upload excel file"), " (no additional specification on website are needed)")))
, accept = c("excel",".xlsx")),
                                       )
                                ),
                                
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
                                       uiOutput("file_upload_status"))

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
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "European cross-cultural neuropsychological test battery (CNTB) for the assessment of cognitive impairment in multiple sclerosis: Cognitive phenotyping and classification supported by machine learning techniques",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Delgado-´Alvarez et al.", href="https://pubmed.ncbi.nlm.nih.gov/39366169/", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "A user's guide for the International Classification of Cognitive Disorders in Epilepsy",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Hermann et al.", href="https://pubmed.ncbi.nlm.nih.gov/39141394/", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "Validity of the MoCA as a cognitive screening tool in epilepsy: Are there implications for global care and research?",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Reyes et al.", href="https://pubmed.ncbi.nlm.nih.gov/38878272/", target = '_blank'),
                                               ),
                                               timelineLabel(2024, color = "teal"),
                                               timelineItem(title = "Cross-cultural application of the international classification of cognitive disorders in epilepsy cognitive phenotypes in people with temporal lobe epilepsy in India",
                                                            border = FALSE,
                                                            #icon = icon("user"),
                                                            #color = "yellow",
                                                            time = shiny::a("Shah et al.", href="https://pubmed.ncbi.nlm.nih.gov/38878272/", target = '_blank'),
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
                                                           ),
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
                 div(style = "margin: 5% 10%; text-align:justify; max-width:1100px;",   # This will create margins on top/bottom and left/right
                     
                     h2(HTML("<u> The International Classification of Cognitive Disorders in Epilepsy (IC-CoDE) </u>"), style = "margin-bottom: 30px;"),  
                     br(),
                     p(h4(HTML("The IC-CoDE represents a consensus-based, empirically-driven approach to diagnosing cognitive disorders in adults with epilepsy. It was developed in 2020 through a memorandum of understanding (MOU) between the International League Against Epilepsy (ILAE) Neuropsychology Task force and the International Neuropsychological Society (INS). The main goal of the IC-CoDE is to accelerate global research in the neuropsychology of epilepsy by providing an internationally-applicable framework for cognitive diagnostics in epilepsy with clear operational criteria and established impairment cut-offs. The IC-CoDE was developed as a guide for harmonizing multi-site cognitive research in epilepsy. It has not been validated as a diagnostic tool for individual patients in clinical settings.
                               <br>
                               <br>
                               The IC-CoDE framework was originally tested in a large, multicenter cohort of 2,485 adults with temporal lobe epilepsy and subsequently in a multicenter cohort of 455 with frontal lobe epilepsy. The IC-CoDE is now currently being tested in youth with epilepsy and in adults at international sites. IC-CoDE has not yet been applied to other epilepsy syndromes (e.g., juvenile myoclonic epilepsy, absence seizures). IC-CoDE has also been modified and applied to several other disorders outside of epilepsy (e.g., multiple sclerosis, COVID-19).  A list of published studies that have used IC-CoDE is provided under the Publications tab above.")))
                 )
        ),
        
        #FAQ Panel####
        tabPanel("FAQ", value = "faqtab",
                 
               div(style = "margin: 5% 10%;text-align:justify; max-width:1100px;",   # This will create margins on top/bottom and left/right
                     
                     h2(HTML("<u> Frequently Asked Questions </u>"), style = "margin-bottom: 30px;"), 
                     
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
                   
                   h4(HTML("<b> Can IC-CoDE calculator be used if patients in your sample completed a different version of the same test (e.g., CVLT, CVLT-2)?</b>
                             <br>
                             <br>
                The IC-CoDE is not a test-specific taxonomy; thus, different versions of the same test can be used interchangeably. However, research is ongoing to better understand the influence different tests and test versions may have on IC-CoDE phenotypes. 
                             <br>
                             <br>")),
                   
                   h4(HTML("<b> Can the IC-CoDE calculator be used in children with epilepsy?</b>
                             <br>
                             <br>
                The IC-CoDE calculator has recently been applied to children with new and recent onset idiopathic epilepsies as well as to those with pharmacoresistant focal epilepsies. Study findings have been presented at scientific meetings and have been submitted for publication. References will be provided under the “Publications” tab above as soon as they are available. 
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
                Ideally, two cognitive measures from all 5 cognitive domains (i.e., language, memory, executive, visuospatial, attention/speed) should be used when generating IC-CoDE phenotypes. However, in instances where there were not enough measures administered within each domain, it may be possible to generate an IC-CoDE phenotype with only 4 cognitive domains. However, careful consideration should be given to the patient population of interest to ensure that an “essential” cognitive domain is not omitted (i.e., domains known to have a high base rate of impairment in that population). For example, memory and language would be considered essential domains in temporal lobe epilepsy and visuospatial would be considered an essential domain for parietal lobe epilepsy.  We do not recommend generating IC-CoDE phenotypes in any patient with fewer than 4 cognitive domains.
                             <br>
                             <br>")),
                   
                   h4(HTML("<b> Can the IC-CoDE calculator be applied to patient populations outside of the United States? </b>
                             <br>
                             <br>
                The IC-CoDE has been tested with temporal lobe epilepsy patients in Mumbai, India where there is considerable language and cultural diversity. The findings were presented at the International Neuropsychological Society 2023 Annual Meeting and will soon be submitted for publication. We are currently working with other research groups in South Africa and Japan, who are validating the IC-CoDE in their local samples. 
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
                Technical assistance is not currently available, but a brief video tutorial has been provided to aid the user in navigating the IC-CoDE portal and calculator. 
                             <br>
                             <br>")),
                   
                     
                 )
        )
        
        
        
        
      )
  )
)

