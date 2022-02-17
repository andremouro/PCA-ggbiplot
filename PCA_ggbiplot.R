####################################################
########PCA EXAMPLE - PLOTING WITH GGBIPLOT##########
####################################################
setwd('') #set the directory
atr <- read.csv('PlantAtr.csv',header = T, sep=';') #read the atributes table
#load the necessary packages
library('devtools')
install_github("vqv/ggbiplot", force = TRUE)
library('ggfortify')
library("ggrepel")
library('ggplot2')

#observation of the data.frame characteristics
str(atr)
is.na(atr) #check if there are any NAs

#perform PCA considering just the columns with plant atribute values (Kvalue - Lign)
model <- prcomp(atr[,3:12], scale = T)
model
#We can observe that the model extracted 10 PCA axis
#We can also observe which plant traits were positively or negatively correlated with each PCA axis

#using the summary we observe the standard deviation of each PCA axis, and the cumulative proportion of variance 'explained' by each axis
summary(model)

#We can visualize the cumulative proportion of variance explained by PCA axes
barplot(summary(model)$importance[3,],ylab='Cumulative Explained Variance')
#In this plot we visualize PC1 and PC2 explains more than a half of the variance of our data

#Lets plot the PCA and visualize
biplot(model)
#Here the red arrows represents the correlation of litter traits with each PCA axis.
#For instance, Phen is mainly negatively correlated with PC1 axis, while TotCResp is mainly positively correlated with PC1 axis.
#On the other hand, K is negatively correlated with PC2 axis.
#The numbers represents each of our litter species.

#Now we will prepare our PCA plot
#First we will call each of the litter species by its name
atr$serap.l <- c(expression(italic('A. guianensis')),
	expression(italic('B. rufa')),
	expression(italic('D. lasiocalyx')),
	expression(italic('M. albicans')),
	expression(italic('M. rubiginosa')),
	expression(italic('M. umbellata')),
	expression(italic('Q. grandiflora')),
	expression(italic('S. obovatum')),
	expression(italic('T. formosa')),
	expression(italic('V. tucanorum')))
str(atr)

#Now we will plot the PCA using the autoplot function
p1 <- autoplot(prcomp(atr[,3:12], scale = T),data=atr,size = 3,
fill = 'origin', loadings = T, loadings.colour = 'grey', #fill will distinguish the origin of our litter
loadings.label = T,loadings.label.colour = 'black',
loadings.label.vjust = 2,loadings.label.hjust = 1.2,shape = 21,stroke = 5)+
scale_fill_manual(name = '',values=c("black","white"))+
theme_classic()+
theme(legend.position = 'top')+
geom_text_repel(label = atr$serap.l, size=3) #here we add the names of each dot - the names of our litter species
p1

#Using the following code we can change the size of the arrows
p1$layers[[2]]$aes_params$size <- 1
p1

#The plot shows the proportion of data variance explained by each of the PCA axes (38.2% PC1 / 26.08% PC2)
#We can also distinguish which of the litter species occurs in savannas or forests.
#Finally, we can see the position of our litter species in the PCA plot.

#Lets save the image
ggsave('Figure.tiff', plot = p1, device = 'tiff', width = 100, height =60,
	dpi = 600, scale = 2, unit = 'mm', compression = "lzw")

#If needed we can also extract the exact value of each litter species in each of the PC axis
pc1 <- predict(model)[,1]
pc2 <- predict(model)[,2]

dat <- data.frame(atr[,2],pc1,pc2)
dat

#Now we can see, for example, that A. guianensis had a value of -2.49 in PC1 and -0.48 in PC2

