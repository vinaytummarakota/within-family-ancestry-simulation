library(ggplot2)
library(ggthemes)

source('../helper_functions.R')

# the non-centrality parameter of the chi-squared distribution which achieves 80% power 
ncp <- 7.787826
# verify ncp achieves 80% power using below line of code
# 1 - pchisq(qchisq(1-0.05, 1), 1, ncp)

sib_corrs <- seq(0.05, 0.95, 0.05) # possible values of sibling correlation in IQ
n <- 175 # sibling pairs in Cremieux's analysis
mde <- 1.35 # effect of 100 percentage point ancestry increase on IQ in standard deviations

calculate_within_family_sd <- function(n, sib_corr, mde, ncp) {
    # Given a non-centrality parameter that achieves the desired level of statistical power, 
    # the sibling correlation in IQ, the minimum detectable effect in standard deviations, 
    # and a sample size, estimate the within-family standard deviation needed to achieve the desired level of power
    # where within-family standard deviation is the standard deviation of the deviation from the parental mean ancestry proportion

    within_family_sd <- sqrt(ncp * (1 - sib_corr) / (n * mde^2))
    return(within_family_sd)
}

within_family_sds <- c()
for(sib_corr in sib_corrs) {
    within_family_sds <- c(within_family_sds, calculate_within_family_sd(n, sib_corr, mde, ncp))
}

power_curve_df <- data.frame(
  sib_corrs = sib_corrs, 
  within_family_sds = within_family_sds
)

sources <- c("Wang et al (2025)", "Markel et al (2025)")
notes <- c("Within-family standard deviation in ancestry is the standard deviation of the deviation from parental mean ancestry proportion.", 
           "Each point on the line represents a pair of conditions that jointly need to be satisified to achieve adequate power.")
caption <- create_caption(sources, notes)

ggplot(power_curve_df, aes(x = sib_corrs, y = within_family_sds))+
  geom_line(
    lineend = "round", 
    linejoin = "round"
  )+
  geom_point(col="red")+
  labs(
    x = "Sibling Correlation in IQ", 
    y = "Within-Family Standard Deviation in Ancestry", 
    subtitle = "Power calculations assume that a 100 percentage point increase in ancestry impacts IQ by 1.35 SD",
    title = "What would need to be true for 175 sibling pairs to provide enough\npower for a within-family admixture regression of ancestry and IQ?", 
    caption = caption
  )+
  geom_vline(
    xintercept = 0.287, 
    linetype = "dashed"
  )+
  annotate("text", x = 0.28, y = Inf, label = "UK Biobank Sibling Correlation\nin Fluid Intelligence Score", hjust = 0.5, vjust = -0.5, fontface="bold")+
  geom_rect(
    xmin = -Inf, 
    xmax = Inf, 
    ymin = -Inf, 
    ymax = 0.0508, 
    fill = "red", 
    alpha = 0.005
  )+
  geom_hline(
    yintercept = 0.0508, 
    linetype = "dashed"
  )+
  annotate("text", x = Inf, y = 0.0508, label = "Genetic Maximum", hjust = -0.1, vjust = 0.4, fontface="bold")+
  scale_y_continuous(labels = scales::label_percent(), limits = c(0.005, NA))+
  coord_cartesian(clip = "off") +
  theme_economist()+
  theme(
    plot.margin = margin(t = 60, r = 115, b = 10, l = 10), 
    plot.title = element_text(margin = margin(b = 10)),
    plot.subtitle = element_text(margin = margin(b = 50, l = -98)),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 12, face = "bold"), 
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0)
  )

# estimate within-family SD needed to achieve p-value of 0.008
p_val <- 0.008
z_score <- qnorm(1 - p_val / 2)
se <- mde / z_score

within_family_sds <- c()
for(sib_corr in sib_corrs) {
    # se = (sqrt(1 - rho)) / (sqrt(n) * within-fam-sd) where within-fam-sd is based on deviation from mean parental ancestry
    within_family_sds <- c(within_family_sds, sqrt(1-sib_corr)/(sqrt(n)*se))
}

power_curve_df <- data.frame(
  sib_corrs = sib_corrs, 
  within_family_sds = within_family_sds
)

sources <- c("Markel et al (2025)")
notes <- c("Each point on the line represents a pair of conditions that jointly need to be satisified to yield a p-value of 0.008.",
"Within-family standard deviation in ancestry is the standard deviation of the deviation from parental mean ancestry proportion.")
caption <- create_caption(sources, notes)

ggplot(power_curve_df, aes(x = sib_corrs, y = within_family_sds))+
  geom_line(
    lineend = "round", 
    linejoin = "round"
  )+
  geom_point(col="red")+
  labs(
    x = "Sibling Correlation in IQ", 
    y = "Within-Family Standard Deviation in Ancestry", 
    subtitle = "Calculations assume that the estimated within-family ancestry effect is 1.35 SD",
    title = "What would need to be true for 175 sibling pairs to yield a p-value of 0.008\nin a within-family global admixture study of ancestry and IQ?", 
    caption = caption
  )+
  geom_vline(
    xintercept = 0.287, 
    linetype = "dashed"
  )+
  annotate("text", x = 0.28, y = Inf, label = "UK Biobank Sibling Correlation\nin Fluid Intelligence Score", hjust = 0.5, vjust = -0.5)+
  geom_rect(
    xmin = -Inf, 
    xmax = Inf, 
    ymin = -Inf, 
    ymax = 0.0508, 
    fill = "red", 
    alpha = 0.005
  )+
  geom_hline(
    yintercept = 0.0508, 
    linetype = "dashed"
  )+
  annotate("text", x = Inf, y = 0.0508, label = "Genetic Maximum", hjust = -0.1, vjust = 0.4)+
  scale_y_continuous(labels = scales::label_percent(), limits = c(0.007, NA))+
  coord_cartesian(clip = "off") +
  theme_bw()+
  theme(
    plot.margin = margin(t = 60, r = 115, b = 10, l = 10), 
    plot.title = element_text(margin = margin(b = 10), face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 50, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 10), 
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "#cdcdcd", linewidth = 0.5),
    axis.line.y = element_line(color = "#cdcdcd", linewidth = 0.5), 
    plot.caption.position = 'plot',
    plot.caption = element_text(hjust = 0)
  )
