# rm(list=ls()) # Remove environment
require(data.table)
require(knitr)
require(ggplot2)
require(magrittr)

# Load list from script 01
dt_list <- readRDS("dt_list.RData")

#sample_sizes <- c(2014:2018)
est <- list()
for (i in 1:length(dt_list)){ 
  
  D <- dt_list[[i]]
  
  # Reference estimate
  ref = sum(D$n1, na.rm = T) 
  
  # LP (CRC)
  LP = sum(D$n1) * sum(D$n2) / sum(D$n1 * D$n2)
  
  # print(years[i])
  # print(c(TDsurv, TDsurvRef, TDsurvXRef, TDlpRef))
  # print((TDsurvXRef - TDsurvRef) / TDsurvRef)
  # print((TDlpRef - TDsurvRef) / TDsurvRef)
  
  # Table of estimated sample totals (per year)
  output <- data.table(Estimator = c("Reference", "Lincoln-Peterson"),
                       Estimate = c(ref, LP),
                       # Perc.diff = c(NA, NA, diff.survx, diff.lp), 
                       Dependency = unique(D$Dependency))
  
  # print(knitr::kable(est, caption = "years[i]", format = "latex"))
  
  est[[i]] <- output
  
  # rm(list=setdiff(ls(), c("est", "est_table", "years")))
  
}


# Plotting the estimates --------------------------------------------------
est <- do.call(rbind, est) 
est$Estimator <- factor(est$Estimator, 
                        levels = unique(est$Estimator))
# 1. Plot D-estimates standardized by sample size

ggplot(est, aes(x = Dependency, y = Estimate, group = Estimator, colour = Estimator)) + 
  geom_line(cex = 0.5) + 
  geom_point(cex = 1.5) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.margin = margin(0.2, 2.35, 0.2, 2.35, "cm")) +
  theme(axis.text = element_text(size = 10)) +
  theme(legend.text = element_text(size = 10)) +
  ylab("Population estimate") + 
  scale_color_manual(values = c("#006633", "#66CC99"))

ggsave("Estimation_estimates.pdf", width = 8, height = 4)

