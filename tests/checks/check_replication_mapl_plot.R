library(ggplot2)
df <- read.csv("check_replication_mapl_out.csv")
df$n <- as.factor(df$n)
df <- do.call(
  rbind,
  by(df, INDICES = list(df$n, df$tune.parameters), FUN = function(data) {
    data$regret.mean <- mean(data$regret)
    data
  })
)
df$tune.parameters <- ifelse(df$tune.parameters == "all", "Tuned", "Not tuned")

ggplot(df, aes(x = n, y = regret)) +
  geom_boxplot() +
  facet_wrap(. ~ tune.parameters, scales = "free_y") +
  geom_line(aes(x = as.numeric(n), y = regret.mean)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12))
ggsave("check_replication_mapl_6.4.2.CAIPWL-opt.png", width = 6.6, height = 5)
