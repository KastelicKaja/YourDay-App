# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#This code was created by Kaja Kastelic with Google Gemini 2.5 Pro assistance.
#Her affiliation is: University of Primorska Andrej Marušič Institute, Koper, Slovenia
#The code was created in July 2025 within the project GIB24. For more information on the project see: https://fvz.upr.si/project/52310/
#
# Install the shiny packages if you haven't already
# install.packages(c("shiny", "shinythemes", "plotly", "rmarkdown", "ggplot2", "dplyr"))
# For PDF export, you will also need a LaTeX distribution. The easiest way is:
# tinytex::install_tinytex()

# Load the required libraries
library(shiny)
library(shinythemes)
library(plotly)
library(rmarkdown)
library(ggplot2)
library(dplyr)

# --- Define the User Interface (UI) ---
# The UI determines how the app looks and what the user sees.
ui <- fluidPage(
  
  # Use a theme for a cleaner look
  theme = shinytheme("lumen"),
  
  # --- More Specific Custom CSS to force-remove the logo border ---
  tags$head(
    tags$style(HTML(
      "img#logo { 
        border: 0 !important; 
      }",
      "body {
        font-family: 'Lato', sans-serif; /* Explicitly set Lato font for the entire body */
      }"
    ))
  ),
  
  # App title with logo, using a more robust layout structure
  titlePanel(
    windowTitle = "Your Day", # This sets the browser tab title
    title = div(
      style = "display: flex; justify-content: space-between; align-items: center; min-height: 100px;",
      h2("Kako uravnotežen je vaš dan?"),
      # The logo is now always visible
      img(id = "logo", src = "logo09042024.png", height = "100px",
          # Add an onerror handler for the image
          onerror = "this.onerror=null; this.src='https://placehold.co/100x100/FFFFFF/000000?text=Logo';")
    )
  ),
  
  # Add a horizontal rule after the title panel for visual separation
  hr(),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for user inputs
    sidebarPanel(
      
      p("Za pridobitev poročila odgovorite na spodnja vprašanja."),
      
      # --- SLEEP Section ---
      h3("SPANJE"),
      p("Koliko časa (povprečno na posamičen dan) ste v zadnjih sedmih dneh dejansko spali (upoštevajte tudi morebitne dnevne dremeže)?"),
      p(tags$small("Opomba: Dremež je kratki čas spanja.")),
      fluidRow(
        column(6,
               div(style="display: flex; align-items: center; gap: 8px;",
                   div(style="flex-grow: 1;",
                       numericInput("sleep_hours", label=NULL, value = "", min = 2, max = 16, step = 1)
                   ),
                   tags$label("ur", `for`="sleep_hours", style="margin-bottom: 0;")
               )
        ),
        column(6,
               div(style="display: flex; align-items: center; gap: 8px;",
                   div(style="flex-grow: 1;",
                       numericInput("sleep_minutes", label=NULL, value = "", min = 0, max = 60, step = 1)
                   ),
                   tags$label("minut", `for`="sleep_minutes", style="margin-bottom: 0;")
               )
        )
      ),
      
      hr(), # Add a horizontal line for separation
      
      # --- SEDENTARY BEHAVIOUR Section ---
      h3("SEDENTARNO VEDENJE"),
      p("Kolikšen delež časa budnosti ste v zadnjih sedmih dneh sedeli ali ležali?"),
      p(tags$small("Npr.: Na delovnem mestu, med gledanjem TV, uporabo računalnika, branjem, v avtomobilu, med jedjo itd.")),
      sliderInput("sedentary_prop",
                  label = NULL,
                  min = 0, max = 100, value = 0, post = "%"),
      
      hr(), # Add a horizontal line for separation
      
      # --- PHYSICAL ACTIVITY Section ---
      h3("TELESNA DEJAVNOST"),
      # Question text for days
      p("Koliko dni ste se v zadnjih sedmih dneh ukvarjali s telesno napornejšimi aktivnostmi, ob katerih je bilo vaše dihanje vsaj nekoliko pospešeno?"),
      p(tags$small("Npr.: Hitra hoja, ukvarjanje s športom, dvigovanje bremen, intenzivno čiščenje, aktivna igra z otroki, vrtnarjenje itd.")),
      # Input box with label on the right for days, now with reduced width
      div(style="display: flex; align-items: center; gap: 8px; width: 50%;",
          div(style="flex-grow: 1;",
              numericInput("mvpa_days", label=NULL, value = "", min = 0, max = 7, step = 1)
          ),
          tags$label("dni", `for`="mvpa_days", style="margin-bottom: 0;")
      ),
      
      # Question text for duration
      p("Koliko časa (na tak dan) ste se v povprečju ukvarjali z zgoraj omenjenimi aktivnostmi?"),
      # Input box with label on the right for duration, now with reduced width
      div(style="display: flex; align-items: center; gap: 8px; width: 50%;",
          div(style="flex-grow: 1;",
              numericInput("mvpa_duration", label=NULL, value = "", min = 0, max = 960, step = 5)
          ),
          tags$label("minut", `for`="mvpa_duration", style="margin-bottom: 0;")
      ),
      
      hr(),
      
      # --- AGE Section ---
      h3("Priporočila so prilagojena starosti"),
      p("Kakšna je vaša starost v letih?"),
      div(style="display: flex; align-items: center; gap: 8px; width: 50%;",
          div(style="flex-grow: 1;",
              numericInput("age", label=NULL, value = "", min = 0, max = 120, step = 1)
          ),
          tags$label("let", `for`="age", style="margin-bottom: 0;")
      ),
      
      # Action button to trigger the calculation
      actionButton("calculate", "Ustvari poročilo", class = "btn-primary")
    ),
    
    # Main panel for displaying the output report
    mainPanel(
      uiOutput("report_title_output"), # Placeholder for the dynamic title
      uiOutput("summary_code_output"), # Placeholder for the summary code
      div(style = "width: 50%; margin: auto; text-align: justify;",
          uiOutput("intro_text_output") # Placeholder for the introductory text
      ),
      # Add a placeholder for the interactive pie chart
      plotlyOutput("time_pie_chart"),
      # Add a placeholder for the final message, now centered and with 50% width
      div(style = "width: 50%; margin: auto; text-align: justify;",
          htmlOutput("final_message_output")
      ),
      # Add a placeholder for the financing information and logos
      uiOutput("sponsors_output"),
      # Add a download button, which only appears after the report is generated successfully
      conditionalPanel(
        condition = "output.report_generated_successfully",
        hr(),
        downloadButton("download_report", "Shrani poročilo kot PDF", class="btn-success"),
        p(tags$small("Opomba: Generiranje PDF poročila lahko traja nekaj trenutkov."))
      )
    )
  )
)

# --- Define the Server Logic ---
# The server contains the instructions to build and update the objects displayed in the UI.
server <- function(input, output) {
  
  # Use eventReactive to make the report generation dependent on the "calculate" button
  report_and_plot_data <- eventReactive(input$calculate, {
    
    # --- Validation for all other fields ---
    if (!is.numeric(input$age) || input$age == "") {
      return(list(error_message = "<p style='color: red;'>Prosimo vnesite vašo starost.</p>"))
    }
    if (input$age < 18) {
      return(list(error_message = "<p>Žal je bila ta aplikacija zasnovana le za odrasle, stare 18 let in več.</p>"))
    }
    if (input$age > 120) {
      return(list(error_message = "<p style='color: red;'>Vnesena starost mora biti od 18 do 120.</p>"))
    }
    if (!is.numeric(input$sleep_hours) || input$sleep_hours == "") {
      return(list(error_message = "<p style='color: red;'>Prosimo vnesite število ur spanja.</p>"))
    }
    if (input$sleep_hours < 2 || input$sleep_hours > 16) {
      return(list(error_message = "<p style='color: red;'>Vneseno število ur spanja mora biti od 2 do 16.</p>"))
    }
    sleep_mins_val <- if (is.na(input$sleep_minutes) || input$sleep_minutes == "") 0 else input$sleep_minutes
    if (!is.numeric(sleep_mins_val) || sleep_mins_val < 0 || sleep_mins_val > 60) {
      return(list(error_message = "<p style='color: red;'>Vneseno število minut spanja mora biti od 0 do 60.</p>"))
    }
    if (!is.numeric(input$mvpa_days) || input$mvpa_days == "") {
      return(list(error_message = "<p style='color: red;'>Prosimo vnesite število dni ukvarjanja z zmerno- do visoko-intenzivno telesno dejavnostjo.</p>"))
    }
    if (input$mvpa_days < 0 || input$mvpa_days > 7) {
      return(list(error_message = "<p style='color: red;'>Vneseno število dni ukvarjanja z zmerno- do visoko-intenzivno telesno dejavnostjo mora biti od 0 do 7.</p>"))
    }
    mvpa_duration_val <- if (is.na(input$mvpa_duration) || input$mvpa_duration == "") 0 else input$mvpa_duration
    if (!is.numeric(mvpa_duration_val) || mvpa_duration_val < 0 || mvpa_duration_val > 960) {
      return(list(error_message = "<p style='color: red;'>Vneseno število minut ukvarjanja z zmerno- do visoko-intenzivno telesno dejavnostjo mora biti od 0 do 960.</p>"))
    }
    
    # --- Time Consistency Validation ---
    total_sleep_minutes <- (input$sleep_hours * 60) + sleep_mins_val
    wake_minutes <- 1440 - total_sleep_minutes
    active_minutes <- wake_minutes * (1 - (input$sedentary_prop / 100))
    mvpa_avg_daily_minutes <- (input$mvpa_days * mvpa_duration_val) / 7
    
    if (mvpa_avg_daily_minutes > active_minutes) {
      return(list(error_message = "<p>Ups, videti je, da vaš dan traja več kot 24 ur. Preverite svoje odgovore.</p>"))
    }
    
    # --- Age-specific Recommendation Thresholds ---
    if (input$age >= 18 && input$age < 65) {
      sleep_min <- 7
      sleep_max <- 10
      sleep_recommendation_text <- "7 do 9 ur na dan"
    } else { # This covers ages 65 to 120
      sleep_min <- 7
      sleep_max <- 9
      sleep_recommendation_text <- "7 do 8 ur na dan"
    }
    
    sedentary_max <- 7 # hours per day
    mvpa_min <- 150 # minutes per week
    lpa_min_hours <- 3 # hours per day
    
    # --- Perform Calculations for Report ---
    sleep_val <- input$sleep_hours + (sleep_mins_val / 60)
    total_mvpa_mins_per_week <- input$mvpa_days * mvpa_duration_val
    sedentary_minutes <- wake_minutes * (input$sedentary_prop / 100)
    sedentary_hours <- sedentary_minutes / 60
    lpa_minutes <- active_minutes - mvpa_avg_daily_minutes
    lpa_hours <- lpa_minutes / 60
    
    # --- Calculate dynamic rotation based on user's formula ---
    percent_mvpa <- (mvpa_avg_daily_minutes / 1440) * 100
    dynamic_rotation <- -(360 * percent_mvpa) / 200
    
    # --- Evaluate each metric against its recommendation ---
    meets_sleep <- FALSE
    meets_sedentary <- FALSE
    meets_mvpa <- FALSE
    meets_lpa <- FALSE
    
    # 1. Sleep Evaluation
    if (sleep_val >= sleep_min && sleep_val < sleep_max) {
      meets_sleep <- TRUE
    }
    sleep_status_text <- if(meets_sleep) "dosegate" else "ne dosegate"
    sleep_part_html <- paste0("<b>", sleep_status_text, "</b> priporočila za spanje (", sleep_recommendation_text, ")")
    sleep_part_text <- paste0("**", sleep_status_text, "** priporočila za spanje (", sleep_recommendation_text, ")")
    
    # 2. Sedentary Behavior Evaluation
    if (sedentary_hours <= sedentary_max) {
      meets_sedentary <- TRUE
    }
    sedentary_status_text <- if(meets_sedentary) "dosegate" else "ne dosegate"
    sedentary_part_html <- paste0("<b>", sedentary_status_text, "</b> priporočila za sedentarno vedenje (7 ur na dan ali manj)")
    sedentary_part_text <- paste0("**", sedentary_status_text, "** priporočila za sedentarno vedenje (7 ur na dan ali manj)")
    
    # 3. Light Physical Activity (LPA) Evaluation
    if (lpa_hours >= lpa_min_hours) {
      meets_lpa <- TRUE
    }
    lpa_status_text <- if(meets_lpa) "dosegate" else "ne dosegate"
    lpa_part_html <- paste0("<b>", lpa_status_text, "</b> priporočila za nizko-intenzivno telesno dejavnost (nekaj ur na dan)")
    lpa_part_text <- paste0("**", lpa_status_text, "** priporočila za nizko-intenzivno telesno dejavnost (nekaj ur na dan)")
    
    # 4. Moderate-to-Vigorous Physical Activity (MVPA) Evaluation
    if (total_mvpa_mins_per_week >= mvpa_min) {
      meets_mvpa <- TRUE
    }
    mvpa_status_text <- if(meets_mvpa) "dosegate" else "ne dosegate"
    mvpa_part_html <- paste0("<b>", mvpa_status_text, "</b> priporočila za zmerno- do visoko-intenzivno telesno dejavnost (vsaj 150 minut na teden)")
    mvpa_part_text <- paste0("**", mvpa_status_text, "** priporočila za zmerno- do visoko-intenzivno telesno dejavnost (vsaj 150 minut na teden)")
    
    # --- Combine results into a single summary block ---
    results_summary_html <- paste0(
      "Na podlagi vaših odgovorov, ",
      sleep_part_html, "; ",
      sedentary_part_html, "; ",
      lpa_part_html, "; in ",
      mvpa_part_html, "."
    )
    results_summary_text <- paste0(
      "Na podlagi vaših odgovorov, ",
      sleep_part_text, "; ",
      sedentary_part_text, "; ",
      lpa_part_text, "; in ",
      mvpa_part_text, "."
    )
    
    # --- Add final message based on overall results ---
    if (meets_sleep && meets_sedentary && meets_mvpa && meets_lpa) {
      final_message_html <- paste0("<p style='margin-top: 20px;'>", results_summary_html, " <b>Čestitamo za doseganje priporočil, samo tako naprej! Za več informacij o smernicah za 24-urno gibalno vedenje obiščite spletno stran <a href='http://www.nijz.si' target='_blank'>www.nijz.si</a></b></p>")
      final_message_text <- paste0(results_summary_text, " **Čestitamo za doseganje priporočil, samo tako naprej!** Za več informacij o smernicah za 24-urno gibalno vedenje obiščite spletno stran [www.nijz.si](http://www.nijz.si).")
    } else {
      final_message_html <- paste0("<p style='margin-top: 20px;'>", results_summary_html, " <b>Spodbujamo vas, da dosežete vsa priporočila! Vsaka sprememba v gibalnem vedenju, ki vas bo približala doseganju priporočil, bo koristna za zdravje! Za več informacij o smernicah za 24-urno gibalno vedenje obiščite spletno stran <a href='http://www.nijz.si' target='_blank'>www.nijz.si</a></b></p>")
      final_message_text <- paste0(results_summary_text, " **Spodbujamo vas, da dosežete vsa priporočila! Vsaka sprememba v gibalnem vedenju, ki vas bo približala doseganju priporočil, bo koristna za zdravje!** Za več informacij o smernicah za 24-urno gibalno vedenje obiščite spletno stran [www.nijz.si](http://www.nijz.si).")
    }
    
    # --- Prepare data for the pie chart ---
    pie_chart_labels <- c("Zmerno- do visoko-intenzivna telesna dejavnost", "Nizko-intenzivna telesna dejavnost", "Spanje", "Sedentarno vedenje")
    pie_values <- c(mvpa_avg_daily_minutes, lpa_minutes, total_sleep_minutes, sedentary_minutes)
    pie_chart_values_formatted <- c(
      gsub("\\.", ",", paste0(round(mvpa_avg_daily_minutes, 0), " min/dan")),
      gsub("\\.", ",", paste0(round(lpa_minutes / 60, 1), " ur/dan")),
      gsub("\\.", ",", paste0(round(total_sleep_minutes / 60, 1), " ur/dan")),
      gsub("\\.", ",", paste0(round(sedentary_minutes / 60, 1), " ur/dan"))
    )
    
  
    # --- Create the summary code ---
    summary_code <- sprintf("[24MoVS: %.1f-%.1f-%.1f-%03.0f]", sleep_val, sedentary_hours, lpa_hours, mvpa_avg_daily_minutes)
    # Replace periods with commas for decimal points
    summary_code <- gsub("\\.", ",", summary_code)
    
    # --- Return list with all data ---
    list(
      final_message_html = HTML(final_message_html),
      final_message_text = final_message_text,
      summary_code_html = HTML(paste0("<p style='text-align: center; font-family: 'Lato', sans-serif;'>", summary_code, "</p>")),
      summary_code_text = summary_code,
      pie_values = pie_values,
      pie_labels = pie_chart_labels,
      pie_text = paste(pie_chart_labels, pie_chart_values_formatted, sep = "\n"),
      pie_hover_values = pie_chart_values_formatted,
      pie_colors = c("#B4246C", "#319447", "#1168B1", "#F0961E"),
      pie_definitions = c(
        "Tista telesna dejavnost, ki pospeši vaše dihanje in srčni utrip (primeri aktivnosti: hitra hoja, tek, kolesarjenje, telovadba, telesno napornejše delo na vrtu).",
        "Tista telesna dejavnost, pri kateri vaše dihanje in srčni utrip nista opazneje pospešena (primeri aktivnosti: počasna hoja, stoja, kuhanje, nakupovanje).",
        "Celotni čas spanja, vključno z dremeži tekom dneva.",
        "Vsakršno vedenje v času budnosti, pri katerem sedite ali ležite in razmeroma mirujete (primeri aktivnosti: gledanje televizije, delo na računalniku, branje knjige, vožnja z avtomobilom)."
      ),
      dynamic_rotation = dynamic_rotation
    )
  })
  
  # Render the dynamic report title
  output$report_title_output <- renderUI({
    data <- report_and_plot_data()
    if (is.null(data$error_message)) {
      h3("Poročilo", style = "text-align: center;")
    }
  })
  
  # Render the summary code
  output$summary_code_output <- renderUI({
    data <- report_and_plot_data()
    if(is.null(data$error_message)){
      data$summary_code_html
    }
  })
  
  # Render the introductory text
  output$intro_text_output <- renderUI({
    data <- report_and_plot_data()
    if(is.null(data$error_message)){
      HTML("<p style='margin-top: 15px; margin-bottom: 15px;'>Vsak dan ima 24 ur, pri čemer del dneva običajno spimo in del dneva se ukvarjamo z različnimi dejavnostmi, med katerimi smo sedentarni (npr. gledanje TV), nizko-intenzivno telesno dejavni (npr. počasna hoja) in zmerno- do visoko-intenzivno telesno dejavni (npr. kolesarjenje). <b>Zdrav dan vključuje ravno pravo ravnovesje med spanjem, sedentarnim vedenjem in telesno dejavnostjo.</b></p>")
    }
  })
  
  # Render the pie chart
  output$time_pie_chart <- renderPlotly({
    data <- report_and_plot_data()
    # Only render the plot if there is no error message
    if (is.null(data$error_message)) {
      df <- data.frame(
        values = data$pie_values,
        labels = data$pie_labels,
        definitions = data$pie_definitions,
        slice_text = data$pie_text,
        hover_values = data$pie_hover_values
      )
      
      plot_ly(df, labels = ~labels, values = ~values, type = 'pie',
              text = ~slice_text,
              textinfo = 'text',
              textposition = 'outside',
              customdata = ~paste0("<b>", labels, "</b><br><i>", definitions, "</i>"),
              hovertemplate = '%{customdata}<extra></extra>',
              marker = list(colors = data$pie_colors, line = list(color = '#FFFFFF', width = 1)),
              sort = FALSE,
              rotation = data$dynamic_rotation) %>%
        layout(title = list(text = "Sestava vašega dne",
                            font = list(size = 20),
                            x = 0.5,
                            xanchor = 'center'),
               margin = list(t = 100, l = 50, r = 50), # Add top and side margins
               showlegend = FALSE) # Legend is redundant with labels on slices
    }
  })
  
  # Render the final message below the pie chart
  output$final_message_output <- renderUI({
    data <- report_and_plot_data()
    if (is.null(data$error_message)) {
      data$final_message_html
    } else {
      HTML(data$error_message)
    }
  })
  
  # Create a reactive output to track if the report was generated successfully
  # This will be TRUE only when the calculate button is pressed and no error is found.
  output$report_generated_successfully <- reactive({
    data <- report_and_plot_data()
    # Return TRUE if there is no error message AND the button has been clicked.
    return(is.null(data$error_message) && input$calculate > 0)
  })
  
  # Ensure the reactive output is available for the conditionalPanel in the UI
  outputOptions(output, 'report_generated_successfully', suspendWhenHidden = FALSE)
  
  # Render the financing/sponsors section
  output$sponsors_output <- renderUI({
    data <- report_and_plot_data()
    if (is.null(data$error_message)) {
      tagList(
        hr(),
        fluidRow(
          column(6,
                 p("Partnerji na projektu:"),
                 img(src = "logo_partnerjev.png", height = "48px", 
                     onerror = "this.onerror=null; this.src='https://placehold.co/200x80/FFFFFF/000000?text=Partner+Logo';")
          ),
          column(6, style="display: flex; justify-content: flex-end;", # Align content to the right
                 div(
                   p("Financerja projekta:"),
                   img(src = "logo_financerjev.png", height = "30px", 
                       onerror = "this.onerror=null; this.src='https://placehold.co/200x80/FFFFFF/000000?text=Funder+Logo';")
                 )
          )
        )
      )
    }
  })
  
  # --- Download Handler for PDF Report ---
  output$download_report <- downloadHandler(
    filename = function() {
      paste('Porocilo_24MoVS-', Sys.Date(), '.pdf', sep='')
    },
    content = function(file) {
      withProgress(message = 'Generating your report...', value = 0, {
        
        temp_report_path <- file.path(tempdir(), "report_template.Rmd")
        
        # R Markdown content string with upgraded ggplot2 code for a visually identical chart.
        rmd_content <- "
---
title: 'Poročilo' # Changed font size to Huge
output: pdf_document
params:
  summary_code: ''
  intro_text: ''
  plot_data: NULL
  final_text: ''
  dynamic_rotation: 0
header-includes:
  - \\usepackage{fancyhdr} # Required for advanced header/footer control
  - \\pagestyle{empty} # Set default page style to empty
  - \\fancypagestyle{plain}{\\fancyhf{}\\renewcommand{\\headrulewidth}{0pt}\\renewcommand{\\footrulewidth}{0pt}} # Redefine 'plain' page style on a single line
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(ggplot2)
library(dplyr)
```
\\begin{center}
`r params$summary_code`
\\end{center}
`r params$intro_text`
\\begin{center}
\\subsubsection*{Sestava vašega dne}
\\end{center}


```{r pie_chart, fig.align='center', out.width='90%'}
# Recreate the pie chart using ggplot2 to mimic the plotly output
df <- params$plot_data

# --- Explicitly set the order for the PDF chart ---
# Define the desired order of slices
desired_order <- c('Zmerno- do visoko-intenzivna telesna dejavnost', 'Sedentarno vedenje', 'Spanje', 'Nizko-intenzivna telesna dejavnost')
# Convert the 'labels' column to a factor with the specified order
df$labels <- factor(df$labels, levels = desired_order)
# Arrange the dataframe rows to match the factor levels before calculating cumulative sums
df <- df %>% dplyr::arrange(labels)


# Calculate positions for the donut chart and labels
df <- df %>%
  mutate(
    fraction = values / sum(values),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_angle = (ymax + ymin) / 2 * 360, # Angle in degrees for rotation
    label_pos = (ymax + ymin) / 2 # Midpoint for text y-position
  )

# Create the text for the labels using the pre-formatted values
df$label_text <- paste0(df$labels, '\\n', df$formatted_values)

# Define radii for donut chart and label placement
donut_outer_radius <- 8
donut_inner_radius <- 1
label_x_pos <- donut_outer_radius + 2.0

# Convert the dynamic rotation from degrees to radians for ggplot2
start_angle_rad <- ((params$dynamic_rotation) * pi / 180)

ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = donut_outer_radius, xmin = donut_inner_radius, fill = labels)) +
  # The donut chart slices
  geom_rect() +
  
  # Leader lines from slices to text
  #geom_segment(aes(x = donut_outer_radius + 0.05, xend = label_x_pos - 0.1, y = label_pos, yend = label_pos),
  #             color = 'black', linewidth = 0.5) +
  
  # Text labels outside the chart
  geom_text(aes(x = label_x_pos, y = label_pos, label = label_text),
            size = 3.0, color = 'black', hjust = 0.5) +
  
  # Convert to polar coordinates to make it a pie/donut, applying the dynamic rotation
  coord_polar(theta = 'y', start = start_angle_rad, clip = 'off') +
  
  # Set x-axis limits to make space for the labels
  xlim(c(1, label_x_pos + 1)) +
  
  # Use the same colors as the plotly chart
  scale_fill_manual(values = df$colors) +
  
  # Remove all theme elements for a clean look
  theme_void() +
  theme(legend.position = 'none')
```

`r params$final_text`

---

Spletna aplikacija je bila zasnovana v okviru projekta Umeščanje načela 24-urnega gibalnega vedenja kot determinante zdravja v slovenski prostor (GIB24) (številka projekta V3-2305), ki ga financira Javna agencija za znanstvenoraziskovalno in inovacijsko dejavnost Republike Slovenije in Ministrstvo za zdravje.
"
        writeLines(rmd_content, temp_report_path)
        
        incProgress(0.2, detail = "Preparing data...")
        
        data <- report_and_plot_data()
        
        # Prepare data frame for ggplot2, ensuring correct order and labels
        plot_df <- data.frame(
          labels = data$pie_labels,
          values = data$pie_values,
          colors = data$pie_colors,
          formatted_values = data$pie_hover_values # Use the pre-formatted text
        )
        # Ensure the order is the same as plotly by setting factor levels
        plot_df$labels <- factor(plot_df$labels, levels = data$pie_labels)
        
        params_list <- list(
          summary_code = data$summary_code_text,
          intro_text = "Vsak dan ima 24 ur, pri čemer del dneva običajno spimo in del dneva se ukvarjamo z različnimi dejavnostmi, med katerimi smo sedentarni (npr. gledanje TV), nizko-intenzivno telesno dejavni (npr. počasna hoja) in zmerno- do visoko-intenzivno telesno dejavni (npr. kolesarjenje). **Zdrav dan vključuje ravno pravo ravnovesje med spanjem, sedentarnim vedenjem in telesno dejavnostjo.**",
          plot_data = plot_df,
          final_text = data$final_message_text,
          dynamic_rotation = data$dynamic_rotation
        )
        
        incProgress(0.6, detail = "Knitting PDF...")
        
        rmarkdown::render(temp_report_path, 
                          output_file = file,
                          params = params_list,
                          envir = new.env(parent = globalenv())
        )
        incProgress(1, detail = "Done!")
      })
    }
  )
}

# --- Run the application ---
# This command combines the UI and server into a running app.
shinyApp(ui = ui, server = server)
