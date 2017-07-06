library(tidyverse)
library(ggplot2)

bind_rows(read_csv("westbrook.csv") %>% mutate(algorithm = "approx"),
          read_csv("westbrook_rff.csv") %>% mutate(algorithm = "random fourier"),
          read_csv("westbrook_yuedong.csv") %>% mutate(algorithm = "yuedong")) %>% ggplot(aes(time, mean)) +
  geom_ribbon(aes(ymin = q1, ymax = q4, fill = algorithm, group = algorithm), alpha = 0.15) + 
  geom_line(aes(color = algorithm, group = algorithm)) + 
  geom_line(aes(y = q4, color = algorithm, group = algorithm), linetype = "dashed") + 
  geom_line(aes(y = q1, color = algorithm, group = algorithm), linetype = "dashed") +
  xlab("Game time") +
  ylab("Shooting percentage") +
  ggtitle("Russell Westbrook's shooting percentage (w/ est. 95% conf. intervals)")
