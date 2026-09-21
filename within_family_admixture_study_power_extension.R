library(tidyverse)
library(ggplot2)
library(latex2exp)
library(scales)

Power <- function(N, rho, sigma_d, beta, alpha=0.05) {
  return(1-pchisq(qchisq(1-alpha, 1), 1, (N*10^2) * sigma_d^2 * beta^2 / (1-rho)))
}

# N <- seq(from = 1, to = 9, by = 1)
N <- seq(from = 1, to = 9, by = 1)
sigma_d <- seq(from = 0.01, to = 0.04, by = 0.01)
rho <- seq(from = 0.1, to = 0.4, by = 0.1)
beta <- c(0.1, 0.5, 1, 2)

power_df <- tidyr::expand_grid(N, sigma_d, rho, beta)
power_df <- power_df %>% mutate(
  power = pmap_dbl(power_df, Power)
)

beta_labels <- c("0.1"="β = 0.1", "0.5"="β = 0.5", "1"="β = 1", "2"="β = 2")
rho_labels <- c("0.1"="r = 0.1", "0.2"="r = 0.2", "0.3"="r = 0.3", "0.4"="r = 0.4")
colors <- c("0.01" = "#f8cc1a", "0.02" = "#91bcdd", "0.03" = "#b6b975", "0.04" = "#8a508f")

ggplot(power_df, aes(x = N, y=power, color = as.factor(sigma_d)))+
  geom_line(linewidth=1.2)+
  geom_point(size=2)+
  facet_grid(
    rho ~ beta, 
    labeller = labeller(beta = as_labeller(beta_labels), rho = as_labeller(rho_labels))
  )+
  labs(
    x = TeX("Number of sibling pairs (x$10^{2}$)"), 
    y = "Power"
  ) + 
  scale_x_continuous(breaks = seq(from=1, to=9, by = 1))+
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = percent_format(accuracy = 1)
  )+
  scale_color_manual(name = "Within-family\nSD in ANC", values = colors)+
  theme_bw()+
  theme(
    strip.text = element_text(face = "bold", size=12), 
    strip.background = element_rect(fill = "#f5f5f5", color = "#cdcdcd", linewidth = 0.5),
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "#cdcdcd", linewidth = 0.5),
    axis.line.y = element_line(color = "#cdcdcd", linewidth = 0.5), 
    axis.text = element_text(color = "black", size = 12), 
    axis.title = element_text(color = "black", size = 14), 
    legend.title = element_text(color = "black", size = 12),
    legend.text = element_text(color = "black", size = 12),
    panel.grid.minor = element_blank()
  )
