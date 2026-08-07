library(ggplot2)
library(ggthemes)

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

    within_family_sd <- sqrt(ncp * (1 - sib_corr) / (2 * n * mde^2))
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

ggplot(power_curve_df, aes(x = sib_corrs, y = within_family_sds))+
  geom_line(
    lineend = "round", 
    linejoin = "round"
  )+
  geom_point(col="red")+
  labs(
    x = "Sibling Correlation in IQ", 
    y = "Within-Family Standard Deviation in Ancestry", 
    title = "What would need to be true for 175 sibling pairs to provide enough\npower for a within-family admixture regression of ancestry and IQ?"
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
    ymax = 0.036, 
    fill = "red", 
    alpha = 0.005
  )+
  geom_hline(
    yintercept = 0.036, 
    linetype = "dashed"
  )+
  annotate("text", x = Inf, y = 0.036, label = "Genetic Maximum", hjust = -0.1, vjust = 0.4, fontface="bold")+
  scale_y_continuous(labels = scales::label_percent())+
  coord_cartesian(clip = "off") +
  theme_economist()+
  theme(
    plot.margin = margin(t = 50, r = 115, b = 10, l = 10), 
    plot.title = element_text(margin = margin(b = 50)),
    axis.title.x = element_text(margin = margin(t = 10)), 
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title = element_text(size = 12, face = "bold")
  )
