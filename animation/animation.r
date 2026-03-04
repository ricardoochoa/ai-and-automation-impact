# ==============================================================================
# AI IMPACT ON KOTA DENPASAR: 2050 MACROECONOMIC PROJECTION MODEL (ANIMATED)
# ==============================================================================
# This script models the occupational exposure, temporal diffusion (S-Curves), 
# and macroeconomic impacts (GRDP & Inequality) of AI adoption in Denpasar.
# 
# *ANIMATED VERSION*: Instead of static plots based on a single parameter, 
# this script generates data across multiple parameters and uses gganimate 
# to output GIF animations showing how the curves shift.
# ==============================================================================

# Install and load required libraries
required_packages <- c("ggplot2", "gganimate", "transformr", "gifski")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Ensure the output directory exists
dir.create("img", showWarnings = FALSE)

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

# ==============================================================================
# PHASE 1: EXPOSURE ASSESSMENT (Shift-Share & Task Exposure)
# Phase 1 is based on a static 2024 baseline and does not change with scenarios.
# We will save it as a static PNG to complete the set.
# ==============================================================================
cat("Executing Phase 1: Generating Static Occupational Exposure Data...\n")

phase1_data <- data.frame(
  Sector = factor(c("Knowledge & Admin (High)", "Tourism & Retail (Medium)", "Physical & Informal (Low)"),
                  levels = c("Knowledge & Admin (High)", "Tourism & Retail (Medium)", "Physical & Informal (Low)")),
  Workers = c(90400, 185000, 135642),
  Exposure_Type = c("Displacement Risk", "Augmentation Opportunity", "Shielded / Stagnant")
)

plot_phase1 <- ggplot(phase1_data, aes(x = Sector, y = Workers, fill = Sector)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::comma(Workers)), vjust = -0.5, fontface = "bold", color = palette["dark_teal"]) +
  scale_fill_manual(values = unname(c(palette["dark_teal"], palette["dark_green"], palette["lime_green"]))) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 220000)) +
  labs(
    title = "Phase 1: Denpasar Labor Force AI Exposure (2024 Baseline)",
    subtitle = "Total Workforce: 411,042. High formal employment accelerates exposure.",
    x = "Occupational Sector", y = "Number of Workers"
  ) +
  eco_theme + theme(legend.position = "none")

ggsave("img/Denpasar_Phase1_Exposure.png", plot_phase1, width = 8, height = 5, dpi = 300)

# ==============================================================================
# PHASE 2: TEMPORAL DIFFUSION (S-CURVES) ANIMATION
# ==============================================================================
cat("Executing Phase 2: Building S-Curve Animation...\n")

calc_scurve <- function(t, L, k, t0, accel) {
  t0_adjusted <- t0 - accel
  return(L / (1 + exp(-k * (t - t0_adjusted))))
}

years <- 2024:2050
phase2_list <- list()

# Generate S-Curve data for multiple Acceleration Years (0 to 5)
for(a in 0:5) {
  tmp <- data.frame(
    Year = rep(years, 3),
    Sector = rep(c("Knowledge & Admin", "Tourism & Retail", "Physical & Informal"), each = length(years)),
    Acceleration = a,
    Accel_Label = paste("Local Acceleration Shift:", a, "Years")
  )
  
  tmp$Adoption_Rate <- c(
    calc_scurve(years, 90, 0.6, 2031, a), # Knowledge
    calc_scurve(years, 65, 0.4, 2035, a), # Tourism
    calc_scurve(years, 20, 0.2, 2042, 0)  # Physical (No acceleration benefit)
  )
  phase2_list[[a+1]] <- tmp
}

phase2_data <- do.call(rbind, phase2_list)

plot_phase2 <- ggplot(phase2_data, aes(x = Year, y = Adoption_Rate, color = Sector)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = unname(c(palette["dark_teal"], palette["lime_green"], palette["dark_green"]))) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title = "Phase 2: AI Temporal Diffusion in Denpasar (2024 - 2050)",
    subtitle = "{closest_state}", # gganimate will inject the Accel_Label here
    x = "Year", y = "AI Task Integration (%)"
  ) +
  eco_theme +
  transition_states(Accel_Label, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out')

# ==============================================================================
# PHASE 3: MACROECONOMIC PROJECTION ANIMATIONS (GRDP & Inequality)
# ==============================================================================
cat("Executing Phase 3: Building Macroeconomic Animations...\n")

# Combine all 4 core scenarios into one dataframe to animate the transitions
grdp_list <- list()
ineq_list <- list()

# 1. Low Stagnation
grdp_list[[1]] <- data.frame(Year = years, GRDP = spline(c(2024, 2035, 2050), c(38, 56, 105), xout=years)$y, State = "1. Low Stagnation", Color = palette["lime_green"])
ineq_list[[1]] <- data.frame(Year = years, Inequality = spline(c(2024, 2035, 2050), c(100, 105, 110), xout=years)$y, State = "1. Low Stagnation", Color = palette["lime_green"])

# 2. Historical Baseline
grdp_list[[2]] <- data.frame(Year = years, GRDP = spline(c(2024, 2035, 2050), c(38, 65, 135), xout=years)$y, State = "2. Historical Baseline", Color = palette["mid_green"])
ineq_list[[2]] <- data.frame(Year = years, Inequality = spline(c(2024, 2035, 2050), c(100, 115, 130), xout=years)$y, State = "2. Historical Baseline", Color = palette["mid_green"])

# 3. High Transformation (No Policy - Wide Inequality)
grdp_list[[3]] <- data.frame(Year = years, GRDP = spline(c(2024, 2035, 2050), c(38, 75, 195), xout=years)$y, State = "3. High Transformation (No Policy)", Color = palette["dark_teal"])
ineq_list[[3]] <- data.frame(Year = years, Inequality = spline(c(2024, 2035, 2045, 2050), c(100, 140, 180, 175), xout=years)$y, State = "3. High Transformation (No Policy)", Color = palette["dark_teal"])

# 4. High Transformation (With Policy - Controlled Inequality)
grdp_list[[4]] <- data.frame(Year = years, GRDP = spline(c(2024, 2035, 2050), c(38, 80, 205), xout=years)$y, State = "4. High Transformation (With Policy)", Color = palette["yellow_green"])
ineq_list[[4]] <- data.frame(Year = years, Inequality = spline(c(2024, 2035, 2045, 2050), c(100, 120, 130, 125), xout=years)$y, State = "4. High Transformation (With Policy)", Color = palette["yellow_green"])

# Bind and factorize for logical transition order
phase3_grdp <- do.call(rbind, grdp_list)
phase3_grdp$State <- factor(phase3_grdp$State, levels = unique(phase3_grdp$State))

phase3_ineq <- do.call(rbind, ineq_list)
phase3_ineq$State <- factor(phase3_ineq$State, levels = unique(phase3_ineq$State))

# --- Plot Phase 3A: GRDP Animation ---
# Note: Using group = 1 forces gganimate to morph the same line instead of drawing a new one
plot_phase3_grdp <- ggplot(phase3_grdp, aes(x = Year, y = GRDP, group = 1, color = Color)) +
  geom_line(linewidth = 2) +
  scale_color_identity() + # Use literal hex codes from the dataframe
  scale_y_continuous(limits = c(0, 220), breaks = seq(0, 220, 40)) +
  labs(
    title = "Phase 3A: Projected Municipal GRDP",
    subtitle = "Scenario: {closest_state}",
    x = "Year", y = "Constant GRDP (Trillion IDR)"
  ) +
  eco_theme +
  transition_states(State, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out')

# --- Plot Phase 3B: Inequality Animation ---
plot_phase3_ineq <- ggplot(phase3_ineq, aes(x = Year, y = Inequality, group = 1, color = Color)) +
  geom_line(linewidth = 2) +
  scale_color_identity() +
  scale_y_continuous(limits = c(80, 200), breaks = seq(80, 200, 20)) +
  labs(
    title = "Phase 3B: Wage Inequality Index",
    subtitle = "Scenario: {closest_state}",
    x = "Year", y = "Inequality Index (Base 2024 = 100)"
  ) +
  eco_theme +
  transition_states(State, transition_length = 2, state_length = 2) +
  ease_aes('cubic-in-out')


# ==============================================================================
# RENDER AND SAVE ANIMATIONS
# ==============================================================================
cat("\nRendering GIFs... This may take a minute...\n")

anim_save("img/Denpasar_Phase2_SCurves_Animated.gif", 
          animation = animate(plot_phase2, width = 800, height = 500, res = 100, renderer = gifski_renderer()))

anim_save("img/Denpasar_Phase3_GRDP_Animated.gif", 
          animation = animate(plot_phase3_grdp, width = 800, height = 500, res = 100, renderer = gifski_renderer()))

anim_save("img/Denpasar_Phase3_Inequality_Animated.gif", 
          animation = animate(plot_phase3_ineq, width = 800, height = 500, res = 100, renderer = gifski_renderer()))

cat("\nDone! Check the 'img/' folder for your animated projections.\n")