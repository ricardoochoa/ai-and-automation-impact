# ==============================================================================
# AI IMPACT ON KOTA DENPASAR: INTERACTIVE SHINY DASHBOARD
# ==============================================================================
# This script builds an interactive web application to visualize the 2050 
# macroeconomic projection model for Denpasar, Bali.
# ==============================================================================

# Install required packages if missing
required_packages <- c("shiny", "ggplot2", "bslib", "dplyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ==============================================================================
# GLOBAL VARIABLES & THEME
# ==============================================================================

# Required Color Palette
palette <- c(
  dark_teal = "#294648", 
  lime_green = "#9cbd1b", 
  olive_green = "#88b027", 
  dark_green = "#00883c", 
  mid_green = "#439f36", 
  bright_lime = "#bbc808", 
  strong_green = "#0d933a", 
  yellow_green = "#c7d301"
)

# Global theme for consistent, professional economic plots
eco_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", color = palette["dark_teal"], size = 16),
    plot.subtitle = element_text(color = "gray30", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.5)
  )

# S-Curve Function
calc_scurve <- function(t, L, k, t0, accel) {
  t0_adjusted <- t0 - accel # Shift left if acceleration is applied
  return(L / (1 + exp(-k * (t - t0_adjusted))))
}

# ==============================================================================
# USER INTERFACE (UI)
# ==============================================================================

ui <- page_navbar(
  title = "Denpasar 2050: AI Macroeconomic Impact",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#294648"),
  
  # ----------------------------------------------------------------------------
  # TAB 1: BASELINE
  # ----------------------------------------------------------------------------
  nav_panel("Phase 1: Baseline Context",
            fluidPage(
              h2("Why Denpasar? The Perfect AI Storm", style = paste0("color: ", palette["dark_teal"], ";")),
              p("When you think of Denpasar, you probably picture the bustling gateway to Bali's world-famous tourism. You might not picture it as ground zero for an Artificial Intelligence revolution."),
              p("But mapping Denpasar's economic baseline using the latest 2024-2025 data from Statistics Indonesia (BPS) reveals two critical factors:"),
              tags$ul(
                tags$li(strong("High Formalization:"), " A massive 61.8% of Denpasar's 411,042 workers are formally employed, heavily concentrated in Services, Education, and Administration. AI targets formal, structured data environments."),
                tags$li(strong("Hyper-Connectivity:"), " Denpasar boasts near-universal 4G/5G connectivity across all its wards, erasing the 'infrastructure friction' that slows down tech adoption elsewhere.")
              ),
              hr(),
              h3("Occupational AI Exposure", style = paste0("color: ", palette["dark_teal"], ";")),
              p("AI doesn't replace entire jobs; it replaces tasks. By breaking down the daily tasks of Denpasar’s labor force, our models grouped workers into three distinct futures:"),
              
              fluidRow(
                column(8, plotOutput("plot_phase1", height = "450px")),
                column(4, 
                       div(class = "card", style = "padding: 15px;",
                           h5("1. Displacement Risk (High)", style = paste0("color: ", palette["dark_teal"], ";")),
                           p("Knowledge workers and administrators. Their routine cognitive tasks are highly susceptible to Generative AI."),
                           h5("2. Augmented (Medium)", style = paste0("color: ", palette["dark_green"], ";")),
                           p("Tourism and retail workers. AI will supercharge their logistics, but cannot replace the 'human touch' needed for hospitality."),
                           h5("3. Shielded (Low)", style = paste0("color: ", palette["lime_green"], ";")),
                           p("Informal and physical laborers. Shielded from immediate AI replacement, but highly vulnerable to economic stagnation.")
                       )
                )
              )
            )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 2: DIFFUSION MODEL
  # ----------------------------------------------------------------------------
  nav_panel("Phase 2: Temporal Diffusion",
            layout_sidebar(
              sidebar = sidebar(
                h4("Adjust Diffusion Drivers"),
                p("Simulate how local infrastructure accelerates or delays the AI 'tipping point'."),
                sliderInput("accel", "Local Acceleration Shift (Years):", 
                            min = 0, max = 5, value = 3, step = 1),
                hr(),
                p(em("E.g., 5G blanket coverage or Gov AI pilots can push integration timelines years ahead of the national average."))
              ),
              fluidPage(
                h2("The S-Curve of Task Automation", style = paste0("color: ", palette["dark_teal"], ";")),
                p("The integration of general-purpose technologies like AI rarely happens linearly. It follows a logistic growth curve (an S-Curve): slow incubation, rapid exponential acceleration, and eventual saturation."),
                plotOutput("plot_phase2", height = "500px")
              )
            )
  ),
  
  # ----------------------------------------------------------------------------
  # TAB 3: MACRO PROJECTIONS
  # ----------------------------------------------------------------------------
  nav_panel("Phase 3: Macroeconomic Outcomes",
            layout_sidebar(
              sidebar = sidebar(
                h4("Set 2050 Scenario"),
                selectInput("scenario", "Macroeconomic Trajectory:", 
                            choices = c("Low Stagnation", "Historical Baseline", "High Transformation"),
                            selected = "High Transformation"),
                
                # Only show policy toggle if High Transformation is selected
                conditionalPanel(
                  condition = "input.scenario == 'High Transformation'",
                  hr(),
                  h5("Government Intervention"),
                  p("Implement aggressive, universal digital upskilling to protect informal workers?"),
                  checkboxInput("policy", "Activate Upskilling Policy", value = TRUE)
                ),
                hr(),
                p("Watch how Denpasar's potential 'AI Dividend' triggers massive inequality without structural intervention.")
              ),
              fluidPage(
                h2("The 60-Trillion Rupiah Jackpot vs. The Great Wage Divide", style = paste0("color: ", palette["dark_teal"], ";")),
                p("If Denpasar successfully integrates AI (High Transformation), GRDP could surge to 195 Trillion IDR by 2050. That extra 60 Trillion IDR is the 'AI Dividend'. However, this wealth acts as a wedge, ruthlessly dividing the formal and informal economies."),
                
                fluidRow(
                  column(6, plotOutput("plot_grdp", height = "400px")),
                  column(6, plotOutput("plot_ineq", height = "400px"))
                )
              )
            )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # --- PHASE 1 PLOT (STATIC) ---
  output$plot_phase1 <- renderPlot({
    phase1_data <- data.frame(
      Sector = factor(c("Knowledge & Admin (High)", "Tourism & Retail (Medium)", "Physical & Informal (Low)"),
                      levels = c("Knowledge & Admin (High)", "Tourism & Retail (Medium)", "Physical & Informal (Low)")),
      Workers = c(90400, 185000, 135642)
    )
    
    ggplot(phase1_data, aes(x = Sector, y = Workers, fill = Sector)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = scales::comma(Workers)), vjust = -0.5, fontface = "bold", color = palette["dark_teal"], size=5) +
      scale_fill_manual(values = unname(c(palette["dark_teal"], palette["dark_green"], palette["lime_green"]))) +
      scale_y_continuous(labels = scales::comma, limits = c(0, 220000)) +
      labs(
        title = "Denpasar Labor Force AI Exposure (2024 Baseline)",
        subtitle = "Total Workforce: 411,042",
        x = "Occupational Sector", y = "Number of Workers"
      ) +
      eco_theme + theme(legend.position = "none")
  })
  
  # --- PHASE 2 PLOT (REACTIVE TO SLIDER) ---
  output$plot_phase2 <- renderPlot({
    years <- 2024:2050
    a <- input$accel
    
    phase2_data <- data.frame(
      Year = rep(years, 3),
      Sector = rep(c("Knowledge & Admin", "Tourism & Retail", "Physical & Informal"), each = length(years))
    )
    
    # Calculate S-Curves based on reactive input 'a'
    phase2_data$Adoption_Rate <- c(
      calc_scurve(years, L = 90, k = 0.6, t0 = 2031, accel = a),
      calc_scurve(years, L = 65, k = 0.4, t0 = 2035, accel = a),
      calc_scurve(years, L = 20, k = 0.2, t0 = 2042, accel = 0) # Physical doesn't benefit from digital acceleration
    )
    
    ggplot(phase2_data, aes(x = Year, y = Adoption_Rate, color = Sector)) +
      geom_line(linewidth = 1.5) +
      scale_color_manual(values = unname(c(palette["dark_teal"], palette["lime_green"], palette["dark_green"]))) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      labs(
        title = "AI Temporal Diffusion (2024 - 2050)",
        subtitle = paste("Tipping points shifted by", a, "years due to local accelerators."),
        x = "Year", y = "AI Task Integration (%)"
      ) +
      eco_theme
  })
  
  # --- PHASE 3 LOGIC (REACTIVE TO DROPDOWN AND CHECKBOX) ---
  
  # Reactive Spline Data for GRDP
  phase3_grdp_data <- reactive({
    years <- 2024:2050
    scen <- input$scenario
    pol <- input$policy
    
    if (scen == "Low Stagnation") {
      pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 56, 105))
      col <- palette["lime_green"]
    } else if (scen == "Historical Baseline") {
      pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 65, 135))
      col <- palette["mid_green"]
    } else { # High Transformation
      if (pol) {
        pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 80, 205))
        col <- palette["yellow_green"]
      } else {
        pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 75, 195))
        col <- palette["dark_teal"]
      }
    }
    
    curve <- spline(pts$x, pts$y, xout = years)
    list(data = data.frame(Year = curve$x, GRDP = curve$y), color = col)
  })
  
  # Reactive Spline Data for Inequality
  phase3_ineq_data <- reactive({
    years <- 2024:2050
    scen <- input$scenario
    pol <- input$policy
    
    if (scen == "Low Stagnation") {
      pts <- data.frame(x = c(2024, 2035, 2050), y = c(100, 105, 110))
      col <- palette["lime_green"]
    } else if (scen == "Historical Baseline") {
      pts <- data.frame(x = c(2024, 2035, 2050), y = c(100, 115, 130))
      col <- palette["mid_green"]
    } else { # High Transformation
      if (pol) {
        pts <- data.frame(x = c(2024, 2035, 2045, 2050), y = c(100, 120, 130, 125))
        col <- palette["yellow_green"]
      } else {
        pts <- data.frame(x = c(2024, 2035, 2045, 2050), y = c(100, 140, 180, 175))
        col <- palette["dark_teal"]
      }
    }
    
    curve <- spline(pts$x, pts$y, xout = years)
    list(data = data.frame(Year = curve$x, Inequality = curve$y), color = col)
  })
  
  # Render GRDP Plot
  output$plot_grdp <- renderPlot({
    p_data <- phase3_grdp_data()
    ggplot(p_data$data, aes(x = Year, y = GRDP)) +
      geom_line(color = p_data$color, linewidth = 2) +
      scale_y_continuous(limits = c(0, 220), breaks = seq(0, 220, 40)) +
      labs(
        title = "Projected Municipal GRDP",
        subtitle = paste("Scenario:", input$scenario),
        x = "Year", y = "Constant GRDP (Trillion IDR)"
      ) +
      eco_theme
  })
  
  # Render Inequality Plot
  output$plot_ineq <- renderPlot({
    p_data <- phase3_ineq_data()
    
    # Dynamic subtitle based on policy
    sub_text <- "Base 2024 = 100"
    if(input$scenario == "High Transformation"){
      sub_text <- ifelse(input$policy, "Policy Active: Inequality growth controlled.", "No Policy: Severe wage polarization.")
    }
    
    ggplot(p_data$data, aes(x = Year, y = Inequality)) +
      geom_line(color = p_data$color, linewidth = 2) +
      scale_y_continuous(limits = c(80, 200), breaks = seq(80, 200, 20)) +
      labs(
        title = "Wage Inequality Index",
        subtitle = sub_text,
        x = "Year", y = "Index (Formal vs. Informal)"
      ) +
      eco_theme
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================
shinyApp(ui = ui, server = server)