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

# Set individual names on county's and assign them as row.names()----------------------------------------------
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

# correlation matrix,to detect possible colinearities-------------------------------------------------------
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
# Removing totalincome and crime (see appendix for argumentation)-------------------------------------------
drops = c("totalincome", "crimes")
data = data[, !(names(data) %in% drops)]
rm(drops)

# Also redo phys and beds to per capita
data$phys = 1000*(data$phys/data$popul)
data$beds = 1000*(data$beds/data$popul)
names(data)[names(data) %in% c("phys", "beds")] = c("physpp", "bedspp")

#Redo the correlation matrix to check---------------------------------------------------------------------
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

# Looks good now, continuing onwards let's check independent vs dependent------------------------------------
library(tidyr)
data2 = gather(data, dat, value, -c(crmpp, state, region))
ggplot(data = data2, aes(value, crmpp, color = crmpp)) + geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~dat, scales = "free") +
  theme(axis.text.x= element_text(size = 6, angle= 45))


# Need some major transforms. Log on: area, popul, maybe physpp? ALso, one MAJOR outlier, what is this?-------
data.transf = data
data.transf$area = log(data.transf$area)
data.transf$popul = log(data.transf$popul)
data.transf$physpp = log(data.transf$physpp)

names(data.transf)[names(data.transf) %in% 
                     c("area", "popul", "physpp")] = 
  c("log(area)", "log(popul)", "log(physpp)")

#Redo plots
library(tidyr)
data2 = gather(data.transf, dat, value, -c(crmpp, state, region))
ggplot(data = data2, aes(value, crmpp, color = crmpp)) + geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~dat, scales = "free") +
  theme(axis.text.x= element_text(size = 6, angle= 45))+
  geom_text(aes(label = ifelse(crmpp > 200, as.character(state), '')), hjust = -0.5, vjust = 0.5, size = 2)

# OK! Now let's check crime levels in the different states----------------------------------------

ggplot(data, aes(x=factor(region), y=crmpp, group=state))+
  geom_boxplot(fill="red", alpha=0.2, outlier.colour = "black", outlier.alpha = 1)+
  geom_text(aes(label=ifelse(crmpp>150,as.character(state),'')),hjust=0,vjust=-0.5)+
  scale_x_discrete(name = "Regions", labels = c("Northeast", "Midwest", "South", "West"))

# Let's plot crmpp over map of US-------------------------------------------------------------------------------------
library(maps)
library(ggplot2)
state.map = map_data(map = "state")

#Need to rename all states in data to full state name as in map_data. Annoying...
n.obs = dim(data)[1]
names.col = rep(NA, n.obs)

# Check which states are missing from our data, remove these 
# from abb.
states.abb = state.abb[!is.na(match(state.abb, data$state))]
states.names = state.name[!is.na(match(state.abb, data$state))]

# This is for DC, not existing in state.name so we insert it manually
names.col[match("DC", as.character(data$state))] = "district of columbia" 

for(i in 1:nlevels(data$state))
  names.col[grep(states.abb[i], data$state)] = tolower(states.names[i])

imp.map.vars = data.frame("crm" = data$crmpp, "region" = names.col, "stateAbb" = as.character(data$state))

# Now we just join the crm data with the appropriate state in the map_data
library(dplyr)
state.map = left_join(state.map, imp.map.vars, by = "region")
tmp = state.map %>% group_by(region) %>% summarise(avgcrm = mean(crm), n = n())
state.map = left_join(state.map, tmp, by = "region")

rm(imp.map.vars, tmp)

# And now we can finally plot
library(viridis)
library(ggthemes)
no.data = unique(state.map$region[which(is.na(state.map$crm))])

ggplot(data = state.map, mapping = aes(x = long, y = lat, group = group))+
  geom_polygon(color = "white", aes(fill = avgcrm)) +
  scale_fill_viridis(option="viridis", na.value = "white")+
  theme_fivethirtyeight()+
  theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.text = element_text(size = 9),
    legend.title = element_blank()
  )+
  annotate("text", x = -93.7, y = 41.955, label = "No data", size = 2)+
  annotate("text", x = -108.18, y = 42.8, label = "No data", size = 2)+
  coord_map(projection = "conic", lat0 = 30)+
  labs(title = "Average crime rate per 1000 inhabitants per state.",
       subtitle = "Data from 1990 census.", size = 0.5)
  
#--------------------------------------------------------------------------------------------------------------------
#
# Plotting done. To efficiently work with model selection (regsubset) later we need to create dummy variables
# out of the region variable. Question is; should we have state as independent variable? Pros, cons?
# Let's leave it out for now. Too many variables if we would treat it as a factor.

data.transf = data.transf[,-2] # Removing state variable
data.transf$region = as.factor(data.transf$region)

# Need to create dummy variables out of region with "West" as baseline.

library(dummies)
data.transf = dummy.data.frame(data.transf)
data.transf = data.transf[,-16] # Remove region 4 (West) to use as baseline.
names(data.transf)[names(data.transf) %in% 
                     c("region1", "region2", "region3")] = 
  c("Northeast", "Midwest", "South")

# Now we can start with linear model fit. FIrst with full model. 
# We also divide into training and testing set.

nr.train = dim(data)[1]*0.7
nr.test = dim(data)[1] - nr.train
train.index = sample(1:dim(data)[1], nr.train)

train.data = data.transf[train.index,]
test.data = data.transf[-train.index,]

mm1 = lm(crmpp~., data = train.data)
summary(mm1)

# Significant variables: area, popul, poors, northeast, midwest. 
# poors make sense, for northeast and miwest we just get difference from baseline
# and it's expected these are significantly different from "West"
# poors is not strange, popul not either. Surprising is area. Did not expect 
# it to have soo much to do with the crime. Smaller area -> more crime pp
# which when I think of it makes sense. As smaller area and more people means more
# crowded, ie big city, which means more crime.

# Let's do the variable selection with regsubset. 
library(leaps)
best.sub.model = regsubsets(crmpp~., data = train.data, nvmax = 15)


