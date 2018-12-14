library(ggplot2)
library(gridExtra)
library(anytime)
library(tidyr)
library(reshape2)
library(stargazer)
library(RColorBrewer)
library(MASS)
library(glmnet)
library(foreach)
library(ggfortify)

houseDat = read.csv("http://www.math.chalmers.se/Stat/Grundutb/GU/MSG500/A18/kc_house_data.csv")
houseDat = houseDat[,-c(9, 10, 11, 12)]

set.seed(1337)

samp = sample(seq(1, dim(houseDat)[1]), 500)
houseDat = houseDat[samp,]
row.names(houseDat) = seq(1,500)
houseDat$date = anytime(houseDat$date)


houseDat2 = gather(houseDat, dat, value, -c(price, date, id))
ggplot(data = houseDat2, aes(value, log(price), color = log(price))) + geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~dat, scales = "free") +
  theme(axis.text.x= element_text(size = 6, angle= 45))

#
# Heatmap to check correlations
#
#
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

corrMat = round(cor(houseDat[-c(1,2)]), 2) #Remove date and id
corrMat[upper.tri(corrMat)] = NA
corrMat = data.frame(melt(corrMat, na.rm = TRUE))


ggplot(data = corrMat, aes(Var1, Var2)) + geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = hm.palette(100),name = "Pearson\nCorrelation") +
  theme(axis.text.x= element_text(size = 8, angle= 45, hjust = 1)) +
  scale_x_discrete(name = "") +
  scale_y_discrete(name = "") +
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


#
#                                 
# Let's log transform sqft_lot and sqft_lot15
#
#
#
p1 = ggplot(houseDat, aes(log(sqft_lot), log(price), color = log(price))) + geom_point() +
  theme(legend.position = "nonce")
p2 = ggplot(houseDat, aes(log(sqft_lot15), log(price), color = log(price))) + geom_point() +
  theme(legend.position = "none")



#
# Looks a bit better with not as extreme outliers. Let's keep the transform.
#
houseDat$sqft_lot = log(houseDat$sqft_lot)
houseDat$sqft_lot15 = log(houseDat$sqft_lot15)


#
#
# yr_renovated looks like it could be divided into groups
# value of 0 indicate not known or never renovated?
#
table(houseDat$yr_renovated)
#
# Dummy variables seems like a good choice
#
houseDat$yr_renovated[(houseDat$yr_renovated > 0 & houseDat$yr_renovated < 1980)] = 1
houseDat$yr_renovated[(houseDat$yr_renovated >= 1980 & houseDat$yr_renovated < 1990)] = 2
houseDat$yr_renovated[(houseDat$yr_renovated >= 1990 & houseDat$yr_renovated < 2000)] = 3
houseDat$yr_renovated[(houseDat$yr_renovated >= 2000 & houseDat$yr_renovated < 2010)] = 4
houseDat$yr_renovated[(houseDat$yr_renovated >= 2010)] = 5

p3 = ggplot(houseDat, aes(factor(yr_renovated), log(price), fill = yr_renovated)) + geom_boxplot(alpha = 0.9) +
  theme(legend.position = "none") +
  scale_x_discrete(name = "year renovated", 
                   labels = c("never/unknown", "0-1979", "1980-1989", "1990-1999", "2000-2009", "2010-"))

lay = rbind(c(1,2),
            c(3,3))

grid.arrange(p1,p2,p3, layout_matrix = lay)

# ******BUILDING LINEAR MODEL*****
# 
# Fitting to all variables, price being target
#
#

mm_all = lm(log(price)~., data = houseDat)
summary(mm_all)
#
# Bedrooms, sqft_living, yr_built, lat and long most significant.
# zip code, bathrooms, yr_renovated and sqft_living15 also significant
# Also, NA on sqft_basement due to singularity. Is it because of perfect colinnearity?  
# Odd as it did not show up in correlation matrix. INvestigate further.
#
#
alias(mm_all)
#
# We see that sqft_basement is really just sqft_living - sqft_above. Thus introducing 
# perfect correlation leading to the singularity.
# Remedy by removing sqft_basement

houseDat = houseDat[-10]
mm_all = lm(log(price)~., data = houseDat)
summary(mm_all)
#
# Similar significance
#
# ******START MODEL SELECTION******
#
# Starting with backward, forward and backward-stepwise selection.
# Done with stepAIC, meaning we use AIC score to determine fit.
# 
mm_intercept = lm(log(price)~1, data = houseDat) # Only intercept model for forward search

backward.model = stepAIC(mm_all, direction = 'backward')
forward.model = stepAIC(mm_intercept, scope = list(upper = mm_all, lower = mm_intercept), direction = 'forward')
both.model = stepAIC(mm_intercept, direction = 'both', scope = list(upper = mm_all, lower = mm_intercept))

#
# Let's also be careful with these greedy models. Adding instead some regularization
# with an elastic net to choose model.
#
houseDat.x = data.matrix(houseDat[-3])
houseDat.y = unlist(as.vector(log(houseDat[3])))

#
# 10 fold cross validation for lambda.
# Run over grid for alpha determining "level of regularization" (1=LASSO, 0=RIDGE)
#
search.grid = seq(0, 1, 0.05)
search.frame = foreach(i = search.grid, .combine = rbind) %do%{
  cv.lambda = cv.glmnet(houseDat.x, houseDat.y, family = "gaussian", nfold = 10, alpha = i)
  data.frame(cvm = cv.lambda$cvm[cv.lambda$lambda == cv.lambda$lambda.min], 
            lambda.min = cv.lambda$lambda.min, lambda.1se = cv.lambda$lambda.1se, alpha = i)
}
best.param = search.frame[search.frame$cvm == min(search.frame$cvm),]
lambda.seq = seq(best.param$lambda.1se, 0, -0.01)
elastic.model = glmnet(houseDat.x, houseDat.y,
                       family = "gaussian", 
                       lambda = lambda.seq, 
                       alpha = best.param$alpha)

#
#
# "Better models" when tending towards ridge regularization. 
# Could be an idea to take lambda.1se instead of lambda.min
# for a more scaled down version.
# Is it adjusted or non-adjusted R^2 that is reported?
# In latter case def. better to choose lambda.1se
#
#
# Some diagnostics plots of chosen model
#
fitted.vals = predict(elastic.model, newx = houseDat.x, type = "link", s = 0.02617)
elastic.residuals = log(houseDat$price) - fitted.vals
dat.tmp = cbind(fitted.vals, elastic.residuals)
colnames(dat.tmp) = c("fitted values", "residuals")
#
# Residuals vs fitted value
#
p1 = ggplot(data = dat.tmp ,aes(fitted.vals, elastic.residuals, color = elastic.residuals)) + geom_point() +
  stat_smooth(method="loess", color = "black") + 
  geom_hline(yintercept=0, col="red", linetype="dashed") +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) +
  xlab("Fitted values") + 
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")

#
#
# Normal QQ-plot
#
#
std.elastic.residuals = scale(elastic.residuals)
qqPoints.y = quantile(std.elastic.residuals, c(0.25,0.75), names=FALSE, type=7)
qqPoints.x = qnorm(c(0.25,0.75))
slope = diff(qqPoints.y)/diff(qqPoints.x)
intercept = qqPoints.y-slope*qqPoints.x

p2 = ggplot(dat.tmp, aes(qqnorm(std.elastic.residuals)[[1]], std.elastic.residuals, color = std.elastic.residuals))+
  geom_point() +
  geom_abline(slope = slope, intercept = intercept, color = "red", lwd = 0.5) + 
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) +
  xlab("Theoretical Quantiles") + 
  ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")

#
# Scale-location plot
#
#

p3 = ggplot(dat.tmp, aes(fitted.vals, sqrt(abs(std.elastic.residuals)), color = sqrt(abs(std.elastic.residuals))))+
  geom_point() +
  stat_smooth(method="loess", na.rm = TRUE, color = "red") +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) +
  xlab("Fitted Value") +
  ylab(expression(sqrt("|Standardized residuals|"))) +
  ggtitle("Scale-Location")
#
#
# Cooks distance
#
houseDat.x.1se = houseDat.x[,-c(1, 2, 3, 6, 8, 11, 15)]
hatMat = hat(houseDat.x.1se)
ones = rep(1,500)
MSE = 1/(500-12) * std.elastic.residuals[,1] %*% std.elastic.residuals[,1]
cooksD = (std.elastic.residuals[,1]/(ones-hatMat))^2 * hatMat/(MSE*12)

p4 = ggplot(data.frame(cooksD), aes(seq_along(cooksD), cooksD, color = cooksD)) + 
  geom_bar(stat="identity", position="identity") + 
  geom_text(data = data.frame(labs = 1:500), aes(label=ifelse(cooksD>0.05,labs,'')),hjust=-0.25,vjust=1) +
  theme(legend.position = "none", plot.title = element_text(size = 10, face = "bold")) +
  xlab("Obs. Number")+ylab("Cook's distance") +
  ggtitle("Cook's distance")
#
#
# Leverage vs standardized residuals
#
dat.tmp = cbind(dat.tmp, cbind(hatMat, cooksD))
p5 = ggplot(dat.tmp, aes(hatMat, std.elastic.residuals, color = std.elastic.residuals))+geom_point(aes(size=cooksD), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage")+ylab("Standardized Residuals") +
  ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range=c(1,5)) +
  theme(legend.position="none", plot.title = element_text(size = 10, face = "bold"))


lay = rbind(c(1,2),
            c(3,5),
            c(4,4))
            
grid.arrange(p1,p2,p3,p4,p5, layout_matrix = lay)

#
#
# Look at data with a map
#
#
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
register_google(key="<my_key>") #Private key can be gotten from google cloud service

relev.map.data = data.frame(houseDat$price, houseDat$long, houseDat$lat, houseDat$sqft_living)
colnames(relev.map.data) = c("price", "lon", "lat", "sqft_living")

bbox <- make_bbox(houseDat$long, houseDat$lat, f = 0.01)
king.map = get_map(bbox, maptype = "roadmap", zoom = 10)

ggmap(king.map) + 
  geom_density2d(data = relev.map.data, aes(lon, lat), size = 0.3) +
  stat_density2d(data = relev.map.data, 
               aes(x = lon, y = lat, alpha = ..level.., fill = ..level..), 
               bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", name = "Price density") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

ggmap(king.map) + geom_point(data = relev.map.data, aes(lon, 
                                                        lat, 
                                                        color = relev.map.data$price, 
                                                        size = relev.map.data$sqft_living/1000)) +
  scale_color_gradient(low = "green", high = "red") +
  labs(col = "Price") +
  scale_size_continuous("sqft_living/1000", range=c(1,5))
