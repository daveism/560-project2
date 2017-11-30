#used code from https://rpubs.com/jelsner/5342
#forsome reason this fails running with source, you will have to copy and paste.

graphics.off()
par("mar")
par(mar=c(1,1,1,1))

if (!is.installed("ismev")){ install.packages("ismev") }
if (!is.installed("mgcv")){ install.packages("mgcv") }
if (!is.installed("quantreg")){ install.packages("quantreg") }
if (!is.installed("xtable")){ install.packages("xtable") }

library(ismev)
library(mgcv)
library(quantreg)
library(xtable)

  #all storms
  LMI.df <- subset(hurr_meta, !is.na(hurr_meta$max_wind_ms) & hurr_meta$basin == "Western Atlantic")
  LMI.df$WmaxS <- as.numeric(LMI.df$max_wind_ms)
  LMI.df$SYear <- as.numeric(LMI.df$year)
  LMI.df$WmaxS_diff <- as.numeric(c(NA,diff(LMI.df$WmaxS)))
  LMI.df$WmaxS_diff <-  as.numeric(LMI.df$WmaxS_diff)
  StartYear <- 1970
  EndYear <- 2016
  # intensity_level <- LMI.df$named

  #intense
  # LMI.dfi <- subset( LMI.df, intensity_level == 1)
  LMI.dfi <- subset( LMI.df,LMI.df$max_category == 3  | LMI.df$max_category == 4  )
  LMI.dfi <- subset( LMI.dfi , SYear >= StartYear)


  par(mfrow=c(3,1))

  plot(LMI.dfi$SYear,log(LMI.dfi$WmaxS))
  abline(lm( log(LMI.dfi$WmaxS) ~ LMI.dfi$SYear), col = "red", lwd = 2)

  # par(mfrow=c(1,1))

  diftemp <- subset(LMI.dfi, !is.infinite(LMI.dfi$WmaxS_diff))
  plot(diftemp$SYear, diftemp$WmaxS_diff)
  abline(lm( diftemp$WmaxS_diff ~ diftemp$SYear), col = "red", lwd = 2)

  # par(mfrow=c(1,1))

  plot(LMI.dfi$SYear,log(LMI.dfi$ace))
  abline(lm( log(LMI.dfi$ace) ~ LMI.dfi$SYear), col = "red", lwd = 2)


  mfit <- lm( log(LMI.dfi$WmaxS) ~ LMI.dfi$SYear)
  mfitdiff <- lm( LMI.dfi$WmaxS_diff ~ LMI.dfi$SYear)
  mfitace <- lm( log(LMI.dfi$ace) ~ LMI.dfi$SYear)

  W =  LMI.dfi$WmaxS
  Year =  LMI.dfi$SYear

  par(mfrow=c(1,1))

  quantile( LMI.dfi$WmaxS, c(0.25, 0.75))

  pt = seq(0, 1, 0.01)
  qp = quantile( LMI.dfi$WmaxS, prob = pt)
  par(mfrow = c(1, 2), las = 1, mgp = c(2, 0.4, 0), tcl = -0.3)
  plot(ecdf( LMI.dfi$WmaxS), xlab = "Wind speed [m/s]", ylab = "Cumulative distribution",
      main = "")
  abline(v = 40, col = "red")
  abline(v = 65, col = "red")
  rug( LMI.dfi$WmaxS)
  mtext("a", side = 3, line = 1, adj = 0, cex = 1.1)

  plot(pt, qp, type = "l", xlab = "Quantile", ylab = "Wind speed [m/s]", lwd = 2)
  mtext("b", side = 3, line = 1, adj = 0, cex = 1.1)

  par(mfrow=c(1,1))

  boxplot(LMI.dfi$WmaxS ~ as.factor(LMI.dfi$SYear))

  yrs = StartYear:EndYear
  n = length(yrs)

  ### plot - least regresion
  maxw = numeric()
  plot(c(StartYear, EndYear), c(15, 85), type = "n", xaxt = "n", bty = "n", xlab = "Year",
      ylab = "Wind speed [m/s]")
  axis(1, at = yrs, labels = yrs, cex = 0.5)
  for (i in 1:n) {
      fn = fivenum(LMI.dfi$WmaxS[LMI.dfi$SYear == yrs[i]])
      points(yrs[i], fn[3], pch = 19)
      lines(c(yrs[i], yrs[i]), c(fn[1], fn[2]))
      lines(c(yrs[i], yrs[i]), c(fn[4], fn[5]))
      maxw[i] = fn[5]
  }
  abline(lm(maxw ~ yrs), col = "red", lwd = 2)
  abline(lm(WmaxS ~ SYear, data = LMI.dfi), lwd = 2)


  ###
  abs = round(quantile(W, seq(0.2, 0.8, 0.2)))
  par(las = 1, mgp = c(2, 0.4, 0), tcl = -0.3)

  Year =  LMI.dfi$SYear
  model = rq(W ~Year, tau = seq(0.025, 0.975, 0.05))

  plot(summary(model, se = "boot"), parm = 2, lcol = "transparent", xaxt = "n",
     mar = c(5, 5, 4, 2) + 0.1, pch = 16, lwd = 2, ylab = expression(paste(beta[Year],
         " [m/s/C]")), xlab = expression(paste("Wind speed quantile [", tau,
         "]")), main = "")
  grid()
  abline(h = 0)
  axis(3, at = seq(0.2, 0.8, 0.2), labels = labs)
  mtext("Lifetime highest wind speed [m/s]", side = 3, line = 2)
  model = rq(W ~  LMI.dfi$SYear, tau = seq(0.025, 0.975, 0.05))

  par(mfrow=c(1,1))

par(.pardefault)

W = LMI.dfi$WmaxS
hist(W, main = "", las = 1, col = "gray", border = "white", xlab = "Wind Speed (m/s)")
rug(W)

###################

Year = LMI.dfi$SYear
W = LMI.dfi$WmaxS
qrm = rq(W ~ Year, tau = 0.5)
qrm
summary(qrm)


par(las = 1, mgp = c(2, 0.4, 0), tcl = -0.3)
plot(Year, W, type = "n", xlab = "Year [C]", ylab = "Wind speed [m/s]")
abline(lm(W ~ Year), col = "red", lwd = 2)
taus = c(0.1, 0.25, 0.5, 0.75, 0.9)
for (i in 1:length(taus)) {
    abline(rq(W ~ Year, tau = taus[i]), col = "gray", lwd = 2)
}
points(Year, W, pch = 19, cex = 0.4)

par(mfrow=c(1,1))

model = rq(W ~ Year, tau = -1)
plot.rq.process(model)


model = rq(W ~ Year, tau = seq(0.025, 0.975, 0.05))
labs = round(quantile(W, seq(0.2, 0.8, 0.2)))

# par(las = 1, mgp = c(2, 0.4, 0), tcl = -0.3)
# plot(summary(model, se = "boot"), parm = 2, lcol = "transparent", xaxt = "n",
#     mar = c(5, 5, 4, 2) + 0.1, pch = 16, lwd = 2, ylab = expression(paste(beta[Year],
#         " [m/s/C]")), xlab = expression(paste("Wind speed quantile [", tau,
#         "]")), main = "")
# grid()
# abline(h = 0)
# axis(3, at = seq(0.2, 0.8, 0.2), labels = labs)
# mtext("Test Lifetime highest wind speed [m/s]", side = 3, line = 2)

par(mfrow=c(1,1))


W = LMI.dfi$WmaxS
hist(W, main = "", las = 1, col = "gray", border = "white", xlab = "Wind Speed (m/s)")
rug(W)

##############
n = length(StartYear:EndYear)
Ws = sort(W, decreasing = TRUE)
round(Ws, 1)[1:6]


m = rev(rank(Ws))
round((n + 1)/m, 0)[1:6]

par(mfrow = c(1, 2), las = 1, mgp = c(2, 0.4, 0), tcl = -0.3)
curve(dnorm(x), from = -4, to = 4, ylab = "Density", lwd = 2)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.1)
m = numeric()
for (i in 1:1000) m[i] = max(rnorm(100))
plot(density(m), xlab = "Maxima of x", main = "", lwd = 2)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.1)

par(mfrow=c(1,1))

sGpd = function(w, u, sigma, xi) {
    d = (w - u) * (w > u)
    sapply(xi, function(xi) if (xi == 0)
        exp(-d/sigma) else ifelse(1 + xi/sigma * d < 0, 0, (1 + xi/sigma * d)^(-1/xi)))
}

sGpd(w = 70, u = 60, sigma = 10, xi = 0)


par(mfrow = c(1, 2), las = 1, mgp = c(2, 0.4, 0), tcl = -0.3)
curve(sGpd(x, u = 60, sigma = 5, xi = 0), from = 60, to = 100, lwd = 2, col = "green",
    xlab = "Wind speed [m/s]", ylab = "Exceedance probability")
curve(sGpd(x, u = 60, sigma = 10, xi = 0), from = 60, to = 100, add = TRUE,
    lwd = 2)
curve(sGpd(x, u = 60, sigma = 15, xi = 0), from = 60, to = 100, add = TRUE,
    lwd = 2, col = "red")
legend("topright", legend = c("sigma = 5", "sigma = 10", "sigma = 15"), lwd = 2,
    col = c(3, 1, 2), cex = 0.8)
mtext("a", side = 3, line = 1, adj = 0, cex = 1.1)
curve(sGpd(x, u = 60, sigma = 10, xi = -0.5), from = 60, to = 100, lwd = 2,
    col = "green", xlab = "Wind speed [m/s]", ylab = "Exceedance probability")
curve(sGpd(x, u = 60, sigma = 10, xi = 0), from = 60, to = 100, add = TRUE,
    lwd = 2)
curve(sGpd(x, u = 60, sigma = 10, xi = 0.5), from = 60, to = 100, add = TRUE,
    lwd = 2, col = "red")
legend("topright", legend = c("xi = -0.5", "xi = 0", "xi = +0.5"), lwd = 2,
    col = c(3, 1, 2), cex = 0.8)
mtext("b", side = 3, line = 1, adj = 0, cex = 1.1)

par(mfrow=c(1,1))


######
library(ismev)
model = gpd.fit(W, threshold = 62)

v = seq(63, 85, 0.1)
p = sGpd(v, u = 62, sigma = model$mle[1], xi = model$mle[2])

plot(v, p, type = "l", lwd = 2, xlab = "Wind Speed (m/s)", ylab = "p(W > v | W > 62)")

par(mfrow=c(1,1))

rate = model$nexc/length(StartYear:EndYear)
rate

round((1 - ppois(0, rate)) * 100, 2)


rp = 1/(1 - exp(-rate * p))
plot(rp, v, type = "l", lwd = 2, log = "x", xlab = "Return Period (yr)", ylab = "Return Level (m/s)")
grid(equilogs = FALSE)

par(mfrow=c(1,1))

v = 73
p = sGpd(v, u = 62, sigma = model$mle[1], xi = model$mle[2])
rp = round(1/(1 - exp(-rate * p)))

thr = 62
v = 73
rps = numeric()
m = 1000
for (i in 1:m) {
    Wbs = sample(W, size = length(W), replace = TRUE)
    modelbs = gpd.fit(Wbs, threshold = thr, show = FALSE)
    ps = sGpd(v, u = thr, sigma = modelbs$mle[1], xi = modelbs$mle[2])
    rps[i] = 1/(1 - exp(-rate * ps))
}

ci = round(quantile(rps, probs = c(0.025, 0.975)))

mean(W[W >= 60] - 60)

mrl.plot(W)
grid()




#############
#
# startyear <- 1980
#
# year_ace_intense_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= startyear  & !is.na(year_ace$intense_ace))
# plot(year_ace_intense_wa$year , year_ace_intense_wa$intense_ace)
# abline(lm( year_ace_intense_wa$intense_ace ~ year_ace_intense_wa$year), col = "red", lwd = 2)
#
#
# year_ace_intense_wa <- subset(year_ace, year_ace$basin == "Western Atlantic"  & year_ace$year >= startyear  & !is.na(year_ace$intense_ace))
# plot(year_ace_intense_wa$year , year_ace_intense_wa$intense_ace)
# abline(lm( year_ace_intense_wa$intense_ace ~ year_ace_intense_wa$year, weights = year_ace_intense_wa$intense_hurricane_count ), col = "red", lwd = 2)
