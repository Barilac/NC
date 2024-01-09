######## ----- Intro ---------

#The file contains code for data analysis of the article "Combination of geochemical prospection and stable isotope analysis document
#intrasite patterns indicative of grain production and manuring at a High medieval manorial site".

# All data is available on the github repository.

######## ----- Figure 1 ---------
# Data loading, all data available at https://github.com/Barilac/PNAS in S1.txt

# Loading data
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)
str(raw)
data = raw[,c(2,3,10)]
  levels(raw[,10]) # levels of data - datasets, see Table 1


# function to create ecosystem boundaries
recttext <- function(xl, yb, xr, yt, text, rectArgs = NULL, textArgs = NULL) {
  center <- c(mean(c(xl, xr)), mean(c(yb, yt)))
  do.call('rect', c(list(xleft = xl, ybottom = yb, xright = xr, ytop = yt), rectArgs))
  do.call('text', c(list(x = center[1], y = center[2], labels = text), textArgs))
}


shapes = c(16,16,16,16,16,16,16,16,16,16,16) 

# Control
levels(data$Category)

# Definition of colors
org <- rgb(255,165,0, max = 255, alpha = 255, names = "myorange")
blck <- rgb(0,0,0, max = 255, alpha = 160, names = "myblack")
bl = rgb(0,0,255, max = 255, alpha = 160, names = "myblue")
yl = rgb(250,250,0, max = 255, alpha = 160, names = "myyellow")
rd = rgb(255,0,0, max = 255, alpha = 160, names = "myred")
gr = rgb(0,128,0, max = 255, alpha = 160, names = "mygreen")

c1 = rgb(0,255,0, max = 255, alpha = 160, names = "c1")
c2 = rgb(128,128,0, max = 255, alpha = 160,  names = "c2")
c3 = rgb(165,42,42, max = 255, alpha = 160,  names = "c3")
c4 = rgb(255,248,220, max = 255, alpha = 160,  names = "c4")
c5 = rgb(128,0,128, max = 255, alpha = 160, names = "c5")
c6 = rgb(47,79,79, max = 255, alpha = 160, names = "c6")


x <- seq(0, 20, 0.5)
y <- sin(x)
plot(x, y, type="l", col=c6)

barva = c(org,blck, rd, gr,bl,yl,c3,c5,c6)
barva = barva[as.numeric(data$Category)]

# Plotting Figure 1

png(height=3000, width=3500, res = 500, file=paste("Figure 1.png"))


plot(data$d15N ~ data$d13C, data=data, pch = shapes, col=barva,
     xlab = "d13C", ylab="d15N", cex=0.8, xlim=c(-33,-10), ylim=c(-6,15))


recttext(-34, 3, -24, 7, 'C3 plants',
         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 5), lty = 'dashed'),
         textArgs = list(col = 'black', cex = 1.3))

# If you have data for carnivores, they can be inserted.

#recttext(-24, 15, -5, 20, 'carnivores',
#rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 10), lty = 'dashed'),
#textArgs = list(col = 'black', cex = 0.5))


recttext(-25, 7, -6, 16, 'herbivores',
         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 5), lty = 'dashed'),
         textArgs = list(col = 'black', cex = 1.3))

recttext(-27, 4, -16, 8, 'CAM plants',
         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 5), lty = 'dashed'),
         textArgs = list(col = 'black', cex = 1.3))

recttext(-16, 4, -6.5, 8.5, 'C4 plants',
         rectArgs = list(col =  rgb(0, 0, 255, max = 255, alpha = 5), lty = 'dashed'),
         textArgs = list(col = 'black', cex = 1.3))


legend(-17, 2, legend=levels(data$Category),
       text.col = c(org,blck, rd, gr,bl,yl,c3,c5,c6),bty = "n",
       cex=1.8, box.col = "white", ncol=2)


graphics.off()



######## ----- Figures 2,3,5 ---------
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)

newdata = raw[1:45,c(2,3,7,8)]
# Drop levels
levels(newdata$Category)
levels(droplevels(newdata)$Category)
newdata$Category <- as.factor(as.character(newdata$Category))
levels(newdata$Category)
str(newdata)

# Definition of colors
gr = rgb(0,128,0, max = 255, alpha = 160, names = "mygreen")
barva2 = c("orange", "blue", "red", gr)
  

# Function boxcount 
boxcount=function (df, var, kat, k = 4){
  pos=1:length(levels(df[,kat]))
  nos=c()
  for (i in pos)
  {nos=append(nos, length(is.na(df[df[,kat]==levels(df[,kat])[i],var])==F))}
  
  #boxplot(df[,var]~df[,kat])
  for (j in pos)
    #{text(pos[j], mean(df[,var], na.rm=T), labels=paste("N: ", nos[j], sep=""))}
  {text(pos[j],min(df[,var], na.rm=T)-(max(df[,var], na.rm=T)-min(df[,var], na.rm=T))/k, labels=paste("n =  ", nos[j], sep=""), xpd=NA, cex=1.5)}
  list(nos, pos, mean(df[,var],na.rm=T))
}

# Plotting Figures 2,3,5 as boxplots
  for (i in 1:3)
  {
    namei=colnames(newdata)[i]
    png(height=4500, width=8000, res = 600, file=paste("24 01 09_", i, "_trebokov_categories.png"))
    par(mar=c(6,6,6,10))
    par(cex.lab=1.35) # is for y-axis
    
    par(cex.axis=1.35) # is for x-axis
    boxplot(newdata[,i]~newdata[,4], xlab="", ylab=paste(namei), col=barva2,
           )
    boxcount(newdata,i,4)
    
    pvaluei=round(kruskal.test(newdata[,i]~newdata[,4])[[3]], 3)
    
    if (pvaluei<0.001) 
    {mtext(3, line=3, text="p-value K-W:  <0.001", cex=1.35)}
    else
    {mtext(3, line=3, text=paste("p-value KW:  ", pvaluei, sep=""),cex=1.35)}
    
    dfi=round(kruskal.test(newdata[,i]~newdata[,4])[[2]], 3)
    
    mtext(3, line=2, text=paste("df:  ", dfi, sep=""),cex=1.35)
    
    chi=round(kruskal.test(newdata[,i]~newdata[,4])[[1]], 2)
    
    mtext(3, line=1, text=paste("KW-H:  ", chi, sep=""),cex=1.35)
    
    dev.off()
  }
  graphics.off()


######## ----- Figure 4  ---------
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

barva2 = c("orange", "red", "blue", gr)
barva = barva2[as.numeric(data$Category)]


# Plotting Figure 4

png(height=3000, width=3500, res =600, file=paste("Figure 4.png"))

plot(data$d15N ~ data$d13C, data = data, pch = 16,col=barva, xlab = "d13C", ylab="d15N", cex=1)

legend(-27.5, 2, legend=levels(data$Category),
       text.col = c("orange", "blue", "red", gr),bty = "n",
       cex=1, box.col = "white")

graphics.off()


# ----- Figure 6 -------
raw = read.table(file = "S1.txt", header=TRUE, sep="\t", dec=".", check.names = F, stringsAsFactors = T)

raw2 = raw[1:45,c(2,3,5,6,7,8,11)]

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


# Plotting Figure 6

png(filename="Figure 6.png", width = 2500, height = 2500, res=500)
plot(d$x[,1], d$x[,2], xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""),
     ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),
     pch=16, las=1, asp=1, ylim = c(-3,2.5),
     xlim = c(-4,2.5),cex=1,  col=barva, cex.lab=1.2)

s$importance #importance of components

# Add grid lines
abline(v=0, lty=2, col="grey50", lwd=1.5)
abline(h=0, lty=2, col="grey50", lwd=1.5)

# Add labels / optional
#text(d$x[,1], d$x[,2], labels=data$`archaeological category`,pos=c(1,3,4,2), font=2, cex=0.3)

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



legend(-4.5, -1.5, legend=levels(categories$Category),
       text.col = c("orange", "red", gr),bty = "n",
       cex=1, box.col = "white")


graphics.off()
dev.off()