library(ggplot2)
library(gridExtra)
library(MASS)
library(tidyr)
library(leaps)
library(RColorBrewer)
library(reshape2)


CDIdata = read.table("http://www.math.chalmers.se/Stat/Grundutb/GU/MSG500/A18/data18.txt")
colnames(CDIdata) = c("id","county","state","area","popul","pop1834","pop65plus","phys",
                      "beds","crimes","higrads","bachelors","poors","unemployed","percapitaincome",
                      "totalincome","region")
data = CDIdata
# Response in column 1
data[,1]= 1000*(CDIdata$crimes/CDIdata$popul)
colnames(data)[1]="crmpp"

# Set individual names on county's and assign them as row.names()
n_occur <- data.frame(table(data$county))
tmp = data[data$county %in% n_occur$Var1[n_occur$Freq > 1],c(2, 3)]
tmp = within(tmp, new <- paste(county, state, sep = "_"))
data$county = as.vector(data$county)
data$county[as.integer(row.names(tmp))] = tmp$new
rm(tmp)
row.names(data) = data$county
data = data[,-2]

# remove the name and convert the state to values (1:48)
## change statets to numbers
for (i in 1:48) {
  data$state[data$state == states[i]] = i
}

# correlation matrix,to detect possible colinearities
library(RColorBrewer)
library(ggplot2)
library(reshape2)
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')

corrMat = round(cor(data[-c(2, 16)]), 2) #Remove states and region
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

# Clearly strong correlation at totalincome and crime
# Removing totalincome and crime (see appendix for argumentation)
drops = c("totalincome", "crimes")
data = data[, !(names(data) %in% drops)]
rm(drops)

# Also redo phys and beds to per capita
data$phys = 1000*(data$phys/data$popul)
data$beds = 1000*(data$beds/data$popul)
names(data)[names(data) %in% c("phys", "beds")] = c("physpp", "bedspp")

#Redo the correlation matrix to check
corrMat = round(cor(data[-c(2, 14)]), 2) #Remove states and region
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

# Looks good now, continuing onwards let's check independent vs dependent
data2 = gather(data, dat, value, -c(crmpp, state, region))
ggplot(data = data2, aes(value, crmpp, color = crmpp)) + geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~dat, scales = "free") +
  theme(axis.text.x= element_text(size = 6, angle= 45))


# Need some major transforms. Log on: area, popul, maybe physpp? ALso, one MAJOR outlier, what is this?
data.transf = data
data.transf$area = log(data.transf$area)
data.transf$popul = log(data.transf$popul)
data.transf$physpp = log(data.transf$physpp)

names(data.transf)[names(data.transf) %in% c("area", "popul", "physpp")] = c("log(area)", "log(popul)", "log(physpp)")

#Redo plots
data2 = gather(data.transf, dat, value, -c(crmpp, state, region))
ggplot(data = data2, aes(value, crmpp, color = crmpp)) + geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~dat, scales = "free") +
  theme(axis.text.x= element_text(size = 6, angle= 45))

# OK! Now let's check crime levels in the different states

ggplot(data, aes(x=factor(region), y=crmpp, group=state))+
  geom_boxplot(fill="red", alpha=0.2, outlier.colour = "black", outlier.alpha = 1)+
  geom_text(aes(label=ifelse(crmpp>150,as.character(state),'')),hjust=0,vjust=-0.5)+
  scale_x_discrete(name = "Regions", labels = c("Northeast", "Midwest", "South", "West"))

# Let's plot crmpp over map of US
library(maps)
state.map = map_data(map = "state")


# hist(data$crmpp)
hist(log(data$crmpp))

### Multi-Linear regression