library(policytree)

df <- read.csv("figure_2_raw_output.csv")
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
tau.cont <- gen_data_epl(100000, type = "continuous")$tau
tau.jump <- gen_data_epl(100000, type = "jump")$tau
df.cont$upper <- mean(pmax(tau.cont, -tau.cont))
df.cont$lower <- max(mean(tau.cont), mean(-tau.cont))
df.jump$upper <- mean(pmax(tau.jump, -tau.jump))
df.jump$lower <- max(mean(tau.jump), mean(-tau.jump))

cols = RColorBrewer::brewer.pal(3, "Set2")

pdf("figure_2_boxplot_add.pdf")
pardef = par(xpd = FALSE, mar = c(4.5, 5, 3, 3) + 0.5, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
boxplot(Ai ~ n, data = df.cont, ylim = range(c(df.cont$Ai, 0, df.cont$upper, df.cont$lower)), col = cols[1], xlab = "n", ylab = "A")
lines(df.cont$n, df.cont$Ai.mean, col = cols[2], lwd = 4)
abline(h=df.cont$upper, lwd = 4, lty = 2, col = cols[3])
abline(h=df.cont$lower, lwd = 4, lty = 4, col = cols[3])
par(pardef)
dev.off()

pdf("figure_2_boxplot_prod.pdf")
pardef = par(xpd = FALSE, mar = c(4.5, 5, 3, 3) + 0.5, cex.lab=1.4, cex.axis=1.4, cex.main=1.4, cex.sub=1.4)
boxplot(Ai ~ n, data = df.jump, ylim = range(c(df.jump$Ai, 0, df.jump$upper, df.jump$lower)), col = cols[1], xlab = "n", ylab = "A")
lines(df.jump$n, df.jump$Ai.mean, col = cols[2], lwd = 4)
abline(h=df.jump$upper, lwd = 4, lty = 2, col = cols[3])
abline(h=df.jump$lower, lwd = 4, lty = 4, col = cols[3])
par(pardef)
dev.off()
