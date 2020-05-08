# ------------------------------------------------------------
# Subgroup Anylysis with the Lung Dataset
# Chen Canyi
# chency1997@ruc.edu.cn
# Fri May 08 22:22:43 2020
# ------------------------------------------------------------


# ------------------------------------------------------------
# Reproducibility: Reproducible_Subgroup script
# ------------------------------------------------------------

# This file contains instructionss for reproducing data,
# figures, tables and all analysis in the report.


# ------------------------------------------------------------
# Step 0: Load necessary packages
# ------------------------------------------------------------

library(tidyverse)
library(glmnet)
library(Amelia) # for missmap
library(Ckmeans.1d.dp)
library(PRIMsrc)
sourceDir <- "./Code"
source(paste(sourceDir,"computeBIC.R",sep = "/"))

# ------------------------------------------------------------
# Step 1: Exploratory Data Analysis
# ------------------------------------------------------------

head(Real.2[, 1:10])
tail(Real.2[, 1:10])
dim(Real.2)
str(Real.2[, 1:10])
summary(Real.2[, 1:10])
(colSums(Real.2 == 0) / ncol(Real.2))[1:10] ## zero-inflate effect
missmap(Real.2)


# ------------------------------------------------------------
# Step2: Conduct parameter tuning for $\lambda$ by crossvali-
# dation
# ------------------------------------------------------------

x <- Real.2[, -c(1)] %>% apply(2, as.numeric)
y <- Real.2$y
cvfit <- cv.glmnet(x, y)
plot(cvfit)

# ------------------------------------------------------------
# Step3: Ajusting the effect of $\x_i$ using the best tuning 
# parameter $\lambda$
# ------------------------------------------------------------

beta <- coef(cvfit, s = "lambda.min")
beta <- beta[2:length(beta)]
yhat <- x %*% beta
res = y - x %*% beta
res = res %>% as.data.frame
colnames(res) = "ires"
ggplot(res, aes(x = ires, y = ..density..)) +
	geom_histogram(fill = "cornsilk",
								 colour = "grey60",
								 size = 0.2) + geom_density() + xlim(-2, 1) +
	theme_classic()


# ------------------------------------------------------------
# Step4: Conduct parameter tuning for $k$ by the BIC
# ------------------------------------------------------------

n <- dim(x)[1]
p <- dim(x)[2]
kL <- 1
kU <- n - 1
karr <- seq(kL, kU)
bicarr <- -1 * rep(1, length(karr))
for (i in 1:length(karr)) {
	k <- karr[i]
	r <- Ckmeans.1d.dp(res[[1]], k)
	muh <- r$centers[r$cluster]
	bicarr[i] <- computeBIC(x, y, muh, beta, i)
}
df <- data.frame(k = karr[1:10],
								 bic = bicarr[1:10])
df %>% ggplot(aes(x = k, y = bic)) +
	geom_point() +
	scale_x_continuous(breaks = karr[1:10]) +
	theme_classic()
kk <- df[which.min(df$bic),1]

# ------------------------------------------------------------
# Step5: Apply kmeans to the ajusted $\widetilde{Y_i}$
# ------------------------------------------------------------

cl <- kmeans(res, kk)
res$cluster <- cl$cluster
res1 <- res %>% as.data.frame() %>% filter(cl$cluster == 1)
res2 <- res %>% as.data.frame() %>% filter(cl$cluster == 2)
ggplot(res, aes(
	x = ires,
	y = ..density..,
	color = cluster,
	group = cluster
)) +
	geom_histogram() +
	geom_density(adjust = 1.5) +
	facet_wrap( ~ cluster) +
	theme(
		legend.position = "none",
		panel.spacing = unit(0.1, "lines"),
		axis.ticks.x = element_blank()
	) +
	theme_classic()

ggplot(res, aes(x = ires, color = cluster, group = cluster)) +
	geom_boxplot() +
	coord_flip() +
	theme_classic()

# ------------------------------------------------------------
# Step 6: Conduct t test for the resulting subgroups
# ------------------------------------------------------------

t.test(res1$ires, res2$ires)
