# ==============================================================================
# AI IMPACT ON KOTA DENPASAR: ACADEMIC FIGURE GENERATOR (LaTeX OPTIMIZED)
# ==============================================================================
# This script generates PDF figures optimized for a two-column academic journal.
# Adjustments include: 3.5-inch column widths, colorblind-safe palettes, 
# removal of in-plot titles (for LaTeX \caption integration), and multiple 
# linetypes for grayscale printing resilience.
# ==============================================================================

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Ensure the output directory exists
dir.create("img", showWarnings = FALSE)

# ==============================================================================
# GLOBAL ACADEMIC THEME & PALETTES
# ==============================================================================

# Okabe-Ito Colorblind-Safe Palette (Gold standard for scientific journals)
journal_colors <- c(
  "orange" = "#E69F00", 
  "sky_blue" = "#56B4E9", 
  "bluish_green" = "#009E73", 
  "yellow" = "#F0E442", 
  "blue" = "#0072B2", 
  "vermilion" = "#D55E00", 
  "reddish_purple" = "#CC79A7",
  "black" = "#000000"
)

# Academic layout optimized for 3.5-inch single-column width
academic_theme <- theme_classic(base_size = 9) +
  theme(
    text = element_text(family = "sans", color = "black"),
    axis.text = element_text(color = "black", size = 8),
    axis.title = element_text(color = "black", size = 9, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 7),
    legend.key.width = unit(0.8, "cm"),
    legend.margin = margin(t = 0, b = 0),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(t = 5, r = 10, b = 0, l = 5)
  )

# Output dimensions for LaTeX single-column (inches)
fig_width <- 3.5
fig_height <- 2.8

# ==============================================================================
# PHASE 1: EXPOSURE ASSESSMENT (Shift-Share & Task Exposure)
# ==============================================================================
cat("Generating Academic Phase 1: Occupational Exposure...\n")

phase1_data <- data.frame(
  Sector = factor(c("Knowledge & Admin", "Tourism & Retail", "Physical & Informal"),
                  levels = c("Knowledge & Admin", "Tourism & Retail", "Physical & Informal")),
  Workers = c(90400, 185000, 135642)
)

plot_phase1 <- ggplot(phase1_data, aes(x = Sector, y = Workers, fill = Sector)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", linewidth = 0.3) +
  geom_text(aes(label = scales::comma(Workers)), vjust = -0.6, size = 3) +
  scale_fill_manual(values = unname(c(journal_colors["vermilion"], journal_colors["blue"], journal_colors["bluish_green"]))) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 220000), expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Occupational Sector", y = "Number of Workers") +
  academic_theme + 
  theme(legend.position = "none", axis.text.x = element_text(angle = 15, hjust = 1))

# ==============================================================================
# PHASE 2: TEMPORAL DIFFUSION (S-CURVES)
# ==============================================================================
cat("Generating Academic Phase 2: S-Curves...\n")

calc_scurve <- function(t, L, k, t0, accel) {
  t0_adjusted <- t0 - accel
  return(L / (1 + exp(-k * (t - t0_adjusted))))
}

years <- 2024:2050
ACCEL <- 3 # 3 years acceleration applied

phase2_data <- data.frame(
  Year = rep(years, 3),
  Sector = factor(rep(c("Knowledge & Admin", "Tourism & Retail", "Physical & Informal"), each = length(years)),
                  levels = c("Knowledge & Admin", "Tourism & Retail", "Physical & Informal"))
)

phase2_data$Adoption_Rate <- c(
  calc_scurve(years, 90, 0.6, 2031, ACCEL),
  calc_scurve(years, 65, 0.4, 2035, ACCEL),
  calc_scurve(years, 20, 0.2, 2042, 0)
)

plot_phase2 <- ggplot(phase2_data, aes(x = Year, y = Adoption_Rate, color = Sector, linetype = Sector)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = unname(c(journal_colors["vermilion"], journal_colors["blue"], journal_colors["bluish_green"]))) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0,0)) +
  labs(x = "Year", y = "AI Task Integration (%)") +
  academic_theme +
  guides(color = guide_legend(ncol = 1)) # Stack legend vertically for compact width

# ==============================================================================
# PHASE 3: MACROECONOMIC PROJECTION (Combined Scenarios)
# ==============================================================================
cat("Generating Academic Phase 3: Macroeconomic Projections...\n")

# To provide a rigorous academic chart, we map all scenarios onto the same plot.
scenarios <- c("Low Stagnation", "Historical Baseline", "High Trans. (No Policy)", "High Trans. (Policy)")

# Define spline anchor points
pts_grdp <- list(
  spline(c(2024, 2035, 2050), c(38, 56, 105), xout = years),
  spline(c(2024, 2035, 2050), c(38, 65, 135), xout = years),
  spline(c(2024, 2035, 2050), c(38, 75, 195), xout = years),
  spline(c(2024, 2035, 2050), c(38, 80, 205), xout = years)
)

pts_ineq <- list(
  spline(c(2024, 2035, 2050), c(100, 105, 110), xout = years),
  spline(c(2024, 2035, 2050), c(100, 115, 130), xout = years),
  spline(c(2024, 2035, 2045, 2050), c(100, 140, 180, 175), xout = years),
  spline(c(2024, 2035, 2045, 2050), c(100, 120, 130, 125), xout = years)
)

# Build Dataframes
phase3_grdp <- data.frame()
phase3_ineq <- data.frame()

for(i in 1:4) {
  phase3_grdp <- rbind(phase3_grdp, data.frame(Year = years, Value = pts_grdp[[i]]$y, Scenario = scenarios[i]))
  phase3_ineq <- rbind(phase3_ineq, data.frame(Year = years, Value = pts_ineq[[i]]$y, Scenario = scenarios[i]))
}

# Factor ordering
phase3_grdp$Scenario <- factor(phase3_grdp$Scenario, levels = scenarios)
phase3_ineq$Scenario <- factor(phase3_ineq$Scenario, levels = scenarios)

# Colors and Linetypes for scenarios
scen_colors <- unname(c(journal_colors["orange"], journal_colors["black"], journal_colors["reddish_purple"], journal_colors["blue"]))
scen_lines <- c("dotted", "solid", "dashed", "dotdash")

# GRDP Plot
plot_phase3_grdp <- ggplot(phase3_grdp, aes(x = Year, y = Value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_lines) +
  scale_y_continuous(limits = c(0, 220), breaks = seq(0, 200, 50), expand = c(0,0)) +
  labs(x = "Year", y = "Constant GRDP (Trillion IDR)") +
  academic_theme +
  guides(color = guide_legend(ncol = 2))

# Inequality Plot
plot_phase3_ineq <- ggplot(phase3_ineq, aes(x = Year, y = Value, color = Scenario, linetype = Scenario)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = scen_colors) +
  scale_linetype_manual(values = scen_lines) +
  scale_y_continuous(limits = c(90, 190), breaks = seq(100, 180, 20), expand = c(0,0)) +
  labs(x = "Year", y = "Wage Inequality Index (2024 = 100)") +
  academic_theme +
  guides(color = guide_legend(ncol = 2))

# ==============================================================================
# EXPORT AS PDFs
# ==============================================================================
cat("\nExporting plots to PDF format for LaTeX...\n")

ggsave("img/Denpasar_Phase1_Exposure_Academic.pdf", plot_phase1, width = fig_width, height = fig_height, device = "pdf")
ggsave("img/Denpasar_Phase2_SCurves_Academic.pdf", plot_phase2, width = fig_width, height = fig_height, device = "pdf")
ggsave("img/Denpasar_Phase3_GRDP_Academic.pdf", plot_phase3_grdp, width = fig_width, height = fig_height, device = "pdf")
ggsave("img/Denpasar_Phase3_Inequality_Academic.pdf", plot_phase3_ineq, width = fig_width, height = fig_height, device = "pdf")

cat("Academic PDF exports complete.\n")