# ==============================================================================
# AI IMPACT ON KOTA DENPASAR: 2050 MACROECONOMIC PROJECTION MODEL
# ==============================================================================
# This script models the occupational exposure, temporal diffusion (S-Curves), 
# and macroeconomic impacts (GRDP & Inequality) of AI adoption in Denpasar.
# ==============================================================================

# Load required library
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# ==============================================================================
# 1. USER PARAMETERS (Adjust these to simulate different scenarios)
# ==============================================================================

# Select the Macroeconomic Scenario to plot for Phase 3
# Options: "Low Stagnation", "Historical Baseline", "High Transformation"
SELECTED_SCENARIO <- "High Transformation"

# Toggle the Aggressive Upskilling Policy (TRUE or FALSE)
# TRUE  = Limits wage inequality and slightly boosts GRDP
# FALSE = Allows market-driven inequality spikes (widening wage gap)
POLICY_ACTIVE <- TRUE

# S-Curve Acceleration Factor (Years)
# Represents local accelerators like Denpasar's blanket 5G or Gov AI pilots.
# E.g., setting to 3 shifts the tipping point for AI adoption 3 years earlier.
ACCELERATION_YEARS <- 3

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
# ==============================================================================
# Baseline data: Denpasar's 411,042 workers split by AI exposure risk

cat("Executing Phase 1: Generating Occupational Exposure Data...\n")

phase1_data <- data.frame(
  Sector = factor(c("Knowledge & Admin (High)", "Tourism & Retail (Medium)", "Physical & Informal (Low)"),
                  levels = c("Knowledge & Admin (High)", "Tourism & Retail (Medium)", "Physical & Informal (Low)")),
  Workers = c(90400, 185000, 135642),
  Exposure_Type = c("Displacement Risk", "Augmentation Opportunity", "Shielded / Stagnant")
)

plot_phase1 <- ggplot(phase1_data, aes(x = Sector, y = Workers, fill = Sector)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = scales::comma(Workers)), vjust = -0.5, fontface = "bold", color = palette["dark_teal"]) +
  scale_fill_manual(values = c(palette["dark_teal"], palette["dark_green"], palette["lime_green"])) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 220000)) +
  labs(
    title = "Phase 1: Denpasar Labor Force AI Exposure (2024 Baseline)",
    subtitle = "Total Workforce: 411,042. High formal employment accelerates exposure.",
    x = "Occupational Sector", y = "Number of Workers"
  ) +
  eco_theme + theme(legend.position = "none")

# ==============================================================================
# PHASE 2: TEMPORAL DIFFUSION (S-CURVES)
# ==============================================================================
# Modeling the logistic growth (S-Curve) of AI adoption over time.
# Formula: Adoption = L / (1 + exp(-k * (t - t0)))
# L = Max adoption limit (%), k = steepness, t0 = tipping point year

cat("Executing Phase 2: Calculating Technological Diffusion S-Curves...\n")

# S-Curve Function
calc_scurve <- function(t, L, k, t0, accel) {
  t0_adjusted <- t0 - accel # Shift left if acceleration is applied
  return(L / (1 + exp(-k * (t - t0_adjusted))))
}

years <- 2024:2050

phase2_data <- data.frame(
  Year = rep(years, 3),
  Sector = rep(c("Knowledge & Admin", "Tourism & Retail", "Physical & Informal"), each = length(years))
)

# Apply S-Curve parameters for Denpasar
phase2_data$Adoption_Rate <- c(
  calc_scurve(years, L = 90, k = 0.6, t0 = 2031, accel = ACCELERATION_YEARS), # Fast, steep
  calc_scurve(years, L = 65, k = 0.4, t0 = 2035, accel = ACCELERATION_YEARS), # Moderate, early plateau
  calc_scurve(years, L = 20, k = 0.2, t0 = 2042, accel = 0)                   # Slow, friction-heavy
)

plot_phase2 <- ggplot(phase2_data, aes(x = Year, y = Adoption_Rate, color = Sector)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c(palette["dark_teal"], palette["lime_green"], palette["dark_green"])) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(
    title = "Phase 2: AI Temporal Diffusion in Denpasar (2024 - 2050)",
    subtitle = paste("S-Curve models. Local accelerators shifted tipping point by", ACCELERATION_YEARS, "years."),
    x = "Year", y = "AI Task Integration (%)"
  ) +
  eco_theme

# ==============================================================================
# PHASE 3: MACROECONOMIC PROJECTION (GRDP & Inequality)
# ==============================================================================
# We use spline interpolation between key economic anchor points to generate 
# smooth, realistic macroeconomic curves through 2050.

cat("Executing Phase 3: Projecting Spatial & Macroeconomic Outcomes...\n")

# --- 3A. GRDP PROJECTION (Trillion IDR) ---
# Define anchor points for GRDP based on the selected scenario
if (SELECTED_SCENARIO == "Low Stagnation") {
  grdp_pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 56, 105))
  scen_color <- palette["lime_green"]
} else if (SELECTED_SCENARIO == "Historical Baseline") {
  grdp_pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 65, 135))
  scen_color <- palette["mid_green"]
} else { # High Transformation
  if (POLICY_ACTIVE) {
    grdp_pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 80, 205))
    scen_color <- palette["yellow_green"]
  } else {
    grdp_pts <- data.frame(x = c(2024, 2035, 2050), y = c(38, 75, 195))
    scen_color <- palette["dark_teal"]
  }
}

grdp_curve <- spline(grdp_pts$x, grdp_pts$y, xout = years)
phase3_grdp <- data.frame(Year = grdp_curve$x, GRDP = grdp_curve$y)

plot_phase3_grdp <- ggplot(phase3_grdp, aes(x = Year, y = GRDP)) +
  geom_line(color = scen_color, linewidth = 2) +
  scale_y_continuous(limits = c(0, 220), breaks = seq(0, 220, 40)) +
  labs(
    title = paste("Phase 3A: Projected Municipal GRDP -", SELECTED_SCENARIO),
    subtitle = paste("Policy Intervention Active:", POLICY_ACTIVE),
    x = "Year", y = "Constant GRDP (Trillion IDR)"
  ) +
  eco_theme

# --- 3B. WAGE INEQUALITY INDEX ---
# Baseline is 100. Higher number = greater wage gap between formal and informal.
if (SELECTED_SCENARIO == "Low Stagnation") {
  ineq_pts <- data.frame(x = c(2024, 2035, 2050), y = c(100, 105, 110))
  ineq_color <- palette["lime_green"]
} else if (SELECTED_SCENARIO == "Historical Baseline") {
  ineq_pts <- data.frame(x = c(2024, 2035, 2050), y = c(100, 115, 130))
  ineq_color <- palette["mid_green"]
} else { # High Transformation
  if (POLICY_ACTIVE) {
    # Policy controls the widening gap, bringing it back down
    ineq_pts <- data.frame(x = c(2024, 2035, 2045, 2050), y = c(100, 120, 130, 125))
    ineq_color <- palette["yellow_green"]
  } else {
    # No policy: Wage gap spikes severely as augmented workers outpace informal
    ineq_pts <- data.frame(x = c(2024, 2035, 2045, 2050), y = c(100, 140, 180, 175))
    ineq_color <- palette["dark_teal"]
  }
}

ineq_curve <- spline(ineq_pts$x, ineq_pts$y, xout = years)
phase3_ineq <- data.frame(Year = ineq_curve$x, Inequality = ineq_curve$y)

plot_phase3_ineq <- ggplot(phase3_ineq, aes(x = Year, y = Inequality)) +
  geom_line(color = ineq_color, linewidth = 2) +
  scale_y_continuous(limits = c(80, 200), breaks = seq(80, 200, 20)) +
  labs(
    title = paste("Phase 3B: Wage Inequality Index -", SELECTED_SCENARIO),
    subtitle = "Index measures wage gap between Formal 'Augmented' & Informal workers.",
    x = "Year", y = "Inequality Index (Base 2024 = 100)"
  ) +
  eco_theme

# ==============================================================================
# DISPLAY PLOTS
# ==============================================================================
# Print plots to the active graphics device
cat("\nGenerating Plots...\n")

print(plot_phase1)
# Sys.sleep(2) # Uncomment if you want pauses between plot generation

print(plot_phase2)

print(plot_phase3_grdp)

print(plot_phase3_ineq)

# Optional: To save the plots as high-resolution images, uncomment the lines below:
# ggsave("Denpasar_Phase1_Exposure.png", plot_phase1, width = 8, height = 5, dpi = 300)
# ggsave("Denpasar_Phase2_SCurves.png", plot_phase2, width = 8, height = 5, dpi = 300)
# ggsave("Denpasar_Phase3_GRDP.png", plot_phase3_grdp, width = 8, height = 5, dpi = 300)
# ggsave("Denpasar_Phase3_Inequality.png", plot_phase3_ineq, width = 8, height = 5, dpi = 300)

cat("Modeling complete. To test other outcomes, change the User Parameters at the top of the script.\n")