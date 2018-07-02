library(tidyverse)
library(ggthemes)
library(cowplot)

roccurve <- function(df){
  dfroc <- df
  
  dfrocp <- dfroc %>% 
    mutate(Positive1 = Nclones == "1 Clone") %>%
    group_by(clonesize,  rsq) %>%
    summarise(Positive = sum(Positive1),
              Negative = n() - sum(Positive1)) %>%
    arrange(-rsq) %>%
    mutate(TPR = cumsum(Positive) / sum(Positive),
           FPR = cumsum(Negative) / sum(Negative))
  
  auc <- dfrocp %>% 
    group_by(clonesize) %>%
    summarise(AUC = sum(diff(FPR) *na.omit(lead(TPR) + TPR)) / 2)
  auc <- as.data.frame(auc)
  
  g <- ggplot(dfrocp, aes(FPR, TPR, color = clonesize)) +
    geom_line(size = 2, alpha = 0.8) +
    geom_abline(lty = 2) + 
    xlab("False positive rate (1 - specificity)") +
    ylab("True positive rate (sensitivity)") +
    scale_colour_ptol() +
    theme(legend.title=element_blank())
  
  return(list(g, auc, dfrocp))
}