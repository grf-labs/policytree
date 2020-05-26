library(ggplot2)
library(policytree)

df <- read.csv("check_replication_epl_out.csv")
df <- do.call(
  rbind,
  by(df, INDICES = list(df$n, df$dgp), FUN = function(data) {
    data$Ai.mean <- mean(data$Ai)
    data
  })
)
df$n <- as.factor(df$n)
df.cont <- df[df$dgp == "continuous", ]
df.jump <- df[df$dgp == "jump", ]
tau.cont <- gen_data_epl(10000, type = "continuous")$tau
tau.jump <- gen_data_epl(10000, type = "jump")$tau
df.cont$upper <- mean(pmax(tau.cont, -tau.cont))
df.cont$lower <- max(mean(tau.cont), mean(-tau.cont))
df.jump$upper <- mean(pmax(tau.jump, -tau.jump))
df.jump$lower <- max(mean(tau.jump), mean(-tau.jump))

ggplot(df.cont, aes(x = n, y = Ai)) +
  geom_boxplot() +
  geom_line(aes(x = as.numeric(n), y = Ai.mean)) +
  geom_hline(yintercept = df.cont$lower, linetype = "dashed") +
  geom_hline(yintercept = df.cont$upper, linetype = "dashed")
ggsave("check_replication_epl_fig3.54.png")

ggplot(df.jump, aes(x = n, y = Ai)) +
  geom_boxplot() +
  geom_line(aes(x = as.numeric(n), y = Ai.mean)) +
  geom_hline(yintercept = df.jump$lower, linetype = "dashed") +
  geom_hline(yintercept = df.jump$upper, linetype = "dashed")
ggsave("check_replication_epl_fig3.55.png")
