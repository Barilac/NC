######## ----- Intro ---------

#The file contains code for data analysis of the article
#Stable isotope analysis in soil prospection reveals the type of
#historic land-use under contemporary temperate forests in Europe".

# All data is available on the github repository.

######## ----- Figure 2 ---------
# Data loading, all data available at https://github.com/Barilac/SR in S1.txt

# Loading data
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)
str(raw)
data = raw[,c(2,3,9,11)]
levels(raw[,c(9,11)]) # levels of data - datasets, see Table 1
#install.packages("cowplot")
#install.packages("gridGraphics")
#install.packages("jpeg")
library(cowplot)
library(gridGraphics)
library(jpeg)

# function to create ecosystem boundaries
recttext <- function(xl, yb, xr, yt, text, rectArgs = NULL, textArgs = NULL) {
  center <- c(mean(c(xl, xr)), mean(c(yb, yt)))
  do.call('rect', c(list(xleft = xl, ybottom = yb, xright = xr, ytop = yt), rectArgs))
  do.call('text', c(list(x = center[1], y = center[2], labels = text), textArgs))
}

# Control
levels(data$Category2)

# Definition of colors
org <- rgb(255,165,0, max = 255, names = "myorange")
blck <- rgb(0,0,0, max = 255, alpha = 120, names = "myblack")
bl = rgb(0,0,255, max = 255, alpha = 120, names = "myblue")
yl = rgb(230,230,0, max = 255, alpha = 120, names = "myyellow")
rd = rgb(255,0,0, max = 255, alpha = 120, names = "myred")
gr = rgb(0,128,0, max = 255, alpha = 120, names = "mygreen")

c1 = rgb(0,255,0, max = 255, alpha = 120, names = "c1")
c2 = rgb(128,128,0, max = 255, alpha = 120,  names = "c2")
c3 = rgb(165,42,42, max = 255, alpha = 120,  names = "c3")
c4 = rgb(255,248,220, max = 255, alpha = 120,  names = "c4")
c5 = rgb(128,0,128, max = 255, alpha = 120, names = "c5")
c6 = rgb(33,199,188, max = 255, alpha = 120, names = "c6")


x <- seq(0, 20, 0.5)
y <- sin(x)
plot(x, y, type="l", col=yl)

barva = c(org,blck, rd, gr,bl,yl,c3,c5,c6)
barva = barva[as.numeric(data$Category2)]


# Define shapes
shapes = c(15, 17, 16) 
shapes <- shapes[as.numeric(unlist(data[4]))]


# adding images from wiki
#C3
c3_i = "https://upload.wikimedia.org/wikipedia/commons/6/6c/Melissa_Askew_2015-08-08_%28Unsplash%29.jpg"
download.file(c3_i,'c3_i.jpg', mode = 'wb')
c3_i <- readJPEG("c3_i.jpg",native=TRUE)
#plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
#rasterImage(c3_i,0,0,1,1)

#c4
c4_i = "https://upload.wikimedia.org/wikipedia/commons/6/6f/Klip_kukuruza_uzgojen_u_Me%C4%91imurju_%28Croatia%29.JPG"
download.file(c4_i,'c4_i.jpg', mode = 'wb')
c4_i <- readJPEG("c4_i.jpg",native=TRUE)
#plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
#rasterImage(c4_i,0,0,1,1)

#herbivores
herb = "https://upload.wikimedia.org/wikipedia/commons/0/0c/Cow_female_black_white.jpg"
download.file(herb,'herb.jpg', mode = 'wb')
herb <- readJPEG("herb.jpg",native=TRUE)
#plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
#rasterImage(herb,0,0,1,1)

# Plotting Figure 2 - isotopes

par(mar= c(5,5,1,1))
plot(data$d15N ~ data$d13C, data=data, pch = shapes, col=barva, cex.lab=1.3, cex.axis = 1.3,
     xlab = expression(paste(delta^13,"C"," (\u2030)")),
     ylab=expression(paste(delta^15,"N"," (\u2030)")), cex=0.8, xlim=c(-33,-4), ylim=c(-6,20))

# text(data$d15N ~ data$d13C, label = data$Type, cex=0.5)


recttext(-34, 2.5, -24, 7, 'C3 plants',
         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 4), lty = 'dashed'),
         textArgs = list(col = 'black', cex = .9))

# If you have data for carnivores, they can be inserted.

#recttext(-24, 15, -5, 20, 'carnivores',
#rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 10), lty = 'dashed'),
#textArgs = list(col = 'black', cex = 0.5))


recttext(-25, 7, -6, 16, 'herbivores',
         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 4), lty = 'dashed'),
         textArgs = list(col = 'black', cex = .9))

#recttext(-27, 4, -16, 8, 'CAM plants',
#         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 4), lty = 'dashed'),
#        textArgs = list(col = 'black', cex = 1.4))

recttext(-16, 4, -6.5, 8.5, 'C4 plants',
         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 4), lty = 'dashed'),
         textArgs = list(col = 'black', cex = .9))

#rastering images, xleft, ybottom, xright, ytop,
rasterImage(c3_i,-33,7,-27,15)
rasterImage(c4_i,-10,-2,-4,5)
rasterImage(herb,-9,14,-3,20)


legend(-18,-0.5, legend=levels(data$Category2), bty = "n",
       fill=c(org,blck, rd, gr,bl,yl,c3,c5,c6), cex=0.9, ncol=3)

legend(-22,-0.5, legend = c("1","2","3"), bty = "n",
       pch= c(15, 17, 16),  cex=0.9, ncol=1)



p1 <- recordPlot()



# Preparing map - Figure 2
library(ggplot2)

# Loading World data
world <- map_data("world")

# Remove the Antarctica region
world <- subset(world, region != "Antarctica")

# Remove the French Southern and Antarctic Lands region
world <- subset(world, region != "French Southern and Antarctic Lands")

# Select the countries you want to keep
countries <- subset(world, region %in% c("Czech Republic", "Peru","Slovakia","Bulgaria","Hungary", "Germany","Greece", "USA","Japan", "Kenya", "UK","Denmark"))

# Assign a code to the countries
cze <- subset(countries, region %in% c("Czech Republic"))
svk <- subset(countries, region %in% c("Slovakia"))
usa <- subset(countries, region %in% c("USA"))
ken <- subset(countries, region %in% c("Kenya"))
uk <- subset(countries, subregion %in% c("Great Britain"))
den <- subset(countries, subregion %in% c("Fyn"))
ger <- subset(countries, region %in% c("Germany"))
grc <- subset(countries, region %in% c("Greece"))
bul <- subset(countries, region %in% c("Bulgaria"))
hun <- subset(countries, region %in% c("Hungary"))
per <- subset(countries, region %in% c("Peru"))
bul <- subset(countries, region %in% c("Bulgaria"))
bul <- subset(countries, region %in% c("Bulgaria"))



map <- ggplot(data = world, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black", size = 0.3)

map



finalmap <- map + 
  geom_polygon(data = cze, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = svk, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = ken, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = uk,  fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = den, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = den, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = ger, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = grc, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = bul, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = hun, fill = "grey", size = 0.3, alpha = 0.6) +
  geom_polygon(data = per, fill = "grey", size = 0.3, alpha = 0.6) +
  

  geom_text(aes(x = 10.6115861, y =  50.4462469,
                label = "A, B, C, I"),
            stat = "unique",
            size = 4, color = "red") +

  geom_text(aes(x = 25.3991836, y = 42.6694375,
                label = "I"),
            stat = "unique",
            size = 4, color = "red") +

# geom_text(aes(x = -0.3, y = 52.4720122,
#                label = "I"),
#            stat = "unique",
#            size = 4, color = "red") +

  geom_text(aes(x = -75.6091172, y =   -8.6853950,
                label = "D,E,F"),
            stat = "unique",
            size = 4, color = "red") +
 
  geom_text(aes(x =   38.3850236, y = 0.0176794,
                label = "G, H"),
            stat = "unique",
            size = 4, color = "red") 

finalmap

p2=
finalmap + 
  theme_bw() + 
  # Choose the size of your map
  coord_fixed(ratio=1.5, xlim = c(-80,60), ylim = c(-30,70))
  
p2 


#Plot both plots together
png(height=3000, width=9000, res = 700, file=paste("Figure 2.png"))
plot_grid(p2, p1, labels = "AUTO")

graphics.off()


######## ----- Figures 4,5 ---------
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)
library(cowplot)
library(ggstatsplot)
newdata = raw[1:45,c(2,3,7,8,10)]
# Drop levels
levels(newdata$Category)
levels(droplevels(newdata)$Category)
newdata$Category <- as.factor(as.character(newdata$Category))
levels(newdata$Category)
str(newdata)

# Definition of colors

org <- rgb(255,165,0, max = 255, alpha = 160, names = "myorange")
bl = rgb(0,0,255, max = 255, alpha = 160, names = "myblue")
rd = rgb(255,0,0, max = 255, alpha = 160, names = "myred")
gr = rgb(0,128,0, max = 255, alpha = 160, names = "mygreen")



# Plotting Figure 4 as boxplot

fi4_1 = ggbetweenstats(
  data = newdata,
  x = Category,
  y = d15N,
  ylab= expression(paste(delta^15,"N"," (\u2030)")),
  type = "nonparametric",
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = T,
  bf.message = T
)+
  ggplot2::scale_color_manual(values = c("#FFA500", "#0000FFA0", "#FF0000A0","#008000A0"))



# Drop levels
newdata2=newdata
levels(newdata2$Category)
levels(droplevels(newdata2)$Category)
newdata2$Category <- as.factor(as.character(newdata2$Category))
levels(newdata2$Category)

fi4_2 = ggbetweenstats(
  data = newdata2,
  x = Category,
  y = d13C,
  ylab= expression(paste(delta^13,"C"," (\u2030)")),
  type = "nonparametric",
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = T,
  bf.message = T
)+
  ggplot2::scale_color_manual(values = c("#FFA500", "#0000FFA0", "#FF0000A0","#008000A0"))


# Plotting Figure 4 as boxplots
png(height=4000, width=9000, res = 800, file=paste("Figure 4.png"))
plot_grid(fi4_1, fi4_2, labels = "AUTO")

dev.off()
graphics.off()


  # Plotting Figure 5 as boxplot
  

fi5 = ggbetweenstats(
  data = newdata,
  x = Category,
  y = P,
  ylab= "P (ppm)",
  type = "nonparametric",
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = T,
  bf.message = T
)+
  ggplot2::scale_color_manual(values = c("#FFA500", "#0000FFA0", "#FF0000A0","#008000A0"))



# Plotting Figure 5 as boxplot
png(height=4000, width=9000, res = 800, file=paste("Figure 5.png"))
plot_grid(fi5, labels = "AUTO")

dev.off()
graphics.off()

 
######## ----- Figure 6  ---------
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)
str(raw)
raw2 = raw[1:45,2:9]
newdata = raw2
# Drop levels
levels(newdata$Category)
levels(droplevels(newdata)$Category)
newdata$Category <- as.factor(as.character(newdata$Category))
levels(newdata$Category)
str(newdata)
data = newdata

gr = rgb(0,128,0, max = 255, alpha = 160, names = "mygreen")

barva2 = c("orange", "blue","red", gr)
barva = barva2[as.numeric(data$Category)]


# Plotting Figure 6

png(height=3000, width=3500, res =600, file=paste("Figure 6.png"))
par(mar= c(5,5,1,1))
plot(data$d15N ~ data$d13C, data = data, pch = 16,col=barva,
     xlab = expression(paste(delta^13,"C"," (\u2030)")), ylab=expression(paste(delta^15,"N"," (\u2030)")), cex=1)

legend(-27.5, 2, legend=levels(data$Category),
       text.col = c("orange", "blue","red", gr),bty = "n",
       cex=1, box.col = "white")
#text(data$d15N ~ data$d13C, label = data$Category, cex=0.5)
graphics.off()


#----Figure 7 -----
library(cowplot)
library(ggstatsplot)
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)

newdata = raw[1:45,c(2,3,7,8,10)]
# Drop levels
levels(newdata$Category)
levels(droplevels(newdata)$Category)
newdata$Category <- as.factor(as.character(newdata$Category))
levels(newdata$Category)



fi7_1 = ggbetweenstats(
  data = newdata,
  x = Category,
  y = C_N_ratio,
  ylab= "C:N",
  type = "nonparametric",
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = T,
  bf.message = T
)+
  ggplot2::scale_color_manual(values = c("#FFA500", "#0000FFA0", "#FF0000A0","#008000A0"))


#N:P ratio
# selecting medieval site
raw2 = raw[1:45,]

# Phosphorus to percentage, see pXRF method in Methodology

phos = raw2[10]/10000

N_P = raw2[5]

np = N_P/phos

colnames(np) = "N_P_ratio"

data2 = cbind(raw2, np)

newdata2 = data2

# Drop levels
levels(newdata2$Category)
levels(droplevels(newdata2)$Category)
newdata2$Category <- as.factor(as.character(newdata2$Category))
levels(newdata2$Category)

fi7_2 = ggbetweenstats(
  data = newdata2,
  x = Category,
  y = N_P_ratio,
  ylab= "N:P",
  type = "nonparametric",
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = T,
  bf.message = T
)+
  ggplot2::scale_color_manual(values = c("#FFA500", "#0000FFA0", "#FF0000A0","#008000A0"))


# Plotting Figure 7 as boxplots
png(height=4000, width=9000, res = 800, file=paste("Figure 7.png"))
plot_grid(fi7_1, fi7_2, labels = "AUTO")
  
dev.off()
graphics.off()


# ----- Figure 8 -------
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)

raw2 = raw[1:45,c(2,3,5,6,7,8,10)]

# Removing category FIELDS - too different to include in PCA
# We want to compare only archaeological site with its medieval fields.
raw3 = raw2[-c(1,2,3,4,43,44,45),]
newdata = raw3

# Drop levels
levels(newdata$Category)
levels(droplevels(newdata)$Category)
newdata$Category <- as.factor(as.character(newdata$Category))
levels(newdata$Category)
str(newdata)

data_select = newdata[,c(1,2,3,4,5,7)]

data_normalized <- scale(data_select)
require(factoextra)
pca = prcomp(data_normalized)
fviz_pca_ind(pca,
             col.ind = "contrib", # Color by contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07") #assign gradient
)

# removing 2 outliers, archaeological features

outlier1 = data_select[-c(3,9),] # dataset with removed outliers

categories = newdata[-c(3,9),] # dataset with categories

# testing again
test <- scale(outlier1)

pca = prcomp(test)
fviz_pca_ind(pca,
             col.ind = "contrib", # Color by contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07") #assign gradient
)

data_normalized2 = scale(outlier1)

d = prcomp(data_normalized2, scale=F, center=T) #scale = F
plot(d)
biplot(d)

# Summary
s <- summary(d)
s
unclass(d)
biplot(d)
# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(d)
screeplot(d, type="lines")

gr = rgb(0,128,0, max = 255, alpha = 160, names = "mygreen")

barva = c("orange", "red", gr)
barva = barva[as.numeric(categories$Category)]

# Export PCA results for Table 3
library("writexl")
sum1 = as.data.frame(print(d[["rotation"]], digits = 3))
sum1 = sum1[1:2]
sum1$variable <- rownames(sum1)
sum2 = as.data.frame(summary(d)$importance)
sum2 = sum2[1:2]
sum2$parameter <- rownames(sum2)

sum3 = as.data.frame(d$sdev^2)


write_xlsx(sum1,"loadings.xlsx")
write_xlsx(sum2,"pca_summary.xlsx")
write_xlsx(sum3,"pca_eigenvalues.xlsx")

# Plotting Figure 8

png(filename="Figure 8.png", width = 2500, height = 2500, res=500)
par(mar=c(4.5,4.5,1,1))
plot(d$x[,1], d$x[,2], xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),
     pch=16, las=1, asp=1, ylim = c(-3,2.5),
     xlim = c(-4,2.5),cex=1,  col=barva, cex.lab=1.2)

s$importance #importance of components

# Add grid lines
abline(v=0, lty=2, col="grey50", lwd=1.5)
abline(h=0, lty=2, col="grey50", lwd=1.5)

# Add labels / optional
#text(d$x[,1], d$x[,2], labels=categories$Category,pos=c(1,3,4,2), font=2, cex=0.3)

# Get co-ordinates of variables (loadings), and multiply
l.x <- d$rotation[,1]*3.2
l.y <- d$rotation[,2]*3.2

# Draw arrows
arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="darkred", length=0.1, lwd=1)

# Label position
l.pos <- l.y # Create a vector of y axis coordinates
lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
hi <- which(l.y > 0) # Get variables on the top half
# Replace values in the vector
l.pos <- replace(l.pos, lo, "1")
l.pos <- replace(l.pos, hi, "3")

# Variable labels
text(l.x, l.y, labels=row.names(d$rotation),
     col="black", pos=l.pos, cex=1)



legend(-4.5, -2, legend=levels(categories$Category),
       text.col = c("orange", "red", gr),bty = "n",
       cex=1.2, box.col = "white")


graphics.off()