##################################    RESULTS POSTER CONFERENCE MANCHESTER     ##############################################

library(ggplot2)
library(gridExtra)    
library(plyr)
library(MASS)
library(caret)

myData <- read.csv('drespaper.csv', header=T, sep=",")

##choose the type of Dressel to do the analyse
myData= subset(myData, type %in% c("Dressel C","Dressel D","Dressel E","Dressel G"))
myData <- myData[,5:13]

#######  Minimum   ####### 
#we use this analyse to equate the sample 

sampleSize = min(count(myData,'site')$freq)

amph1 <- subset(myData, site=="delicias")
sample1 <- amph1[sample(nrow(amph1), sampleSize),]
amph2 <- subset(myData, site=="malpica")
sample2 <- amph2[sample(nrow(amph2), sampleSize),]
amph3 <- subset(myData, site=="belen")
sample3 <- amph3[sample(nrow(amph3), sampleSize),]
amph4 <- subset(myData, site=="parlamento")
sample4 <- amph4[sample(nrow(amph4), sampleSize),]
amph5 <- subset(myData, site=="villaseca")
sample5 <- amph5[sample(nrow(amph5), sampleSize),]

sample <- rbind(sample1,sample2)
sample <- rbind(sample, sample3)
sample <- rbind(sample, sample4)
sample <- rbind(sample, sample5)

######  Principal Component Analyse  ########

logDataSample <- log(sample[,1:8])
pcaResultsSample <- princomp(logDataSample, center=T, scale=T)

# plot to check the relevance of the first 2 PC'S
plot(pcaResultsSample)
# get the scores of the data
pcaValuesSample <- as.data.frame(pcaResultsSample$scores)
# put type in pcaValues
pcaValuesSample$site <- sample$site
ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point()
ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2, col=site)) + geom_point() + facet_wrap(~site,ncol=1)


#use summary to see the cumulative stuff, standard deviation, etc. Use head to see the resuts of the variables. 

#with the application of geom_density2d and dev.off to keep it

svg('fig_dist.svg3')    
ggplot(pcaValuesSample, aes(x=Comp.1, y=Comp.2)) + geom_density2d(aes(col=site), alpha=0.3) + geom_point(aes(col=site), size=2) + facet_wrap(~site, ncol=1) + theme(legend.position='none')
dev.off()


#########DISTANCE MEASUREMENT AND PLOT ##############################

library(ggplot2)
library(gridExtra)

# raw spatial distances through the river
spatialDist <- data.frame(site=c('parlamento','belen','malpica','delicias', 'villaseca'), distance=c(0,68,74,82,95))
spatialDist$site <- factor(spatialDist$site, levels = c('parlamento','belen','malpica', 'delicias','villaseca'))

g1 <- ggplot(spatialDist, aes(x=distance, y=0, col=site)) + xlim(c(0, 100)) + geom_segment(col='grey40', x=0, xend=max(spatialDist$distance), y=0, yend=0) + geom_point()  + geom_text(aes(label=site), vjust=-1,size=7)  + theme_bw() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.title.y=element_blank()) + xlab('distance (km)') + scale_color_manual(values=c("skyblue3","goldenrod1","indianred2","palegreen4", "darkorange3")) + theme(legend.position='none') + ylim(c(-0.05,0.1))


# PCA data
foo <- read.csv('drespaper.csv', sep=",", header=T)
bar <- subset(foo, !type %in% c('Dressel 23', 'Dressel A', 'Dressel B'))

logData <- log(bar[,5:12])

pcaLogResults <- princomp(logData, center=T, scale=T)

pcaLogValues <- as.data.frame(pcaLogResults$scores)
# west-east order
pcaLogValues$site <- factor(bar$site, levels = c('parlamento','belen','malpica','delicias', 'villaseca'))

#g2 <- ggplot(pcaLogValues, aes(y=Comp.1, x=Comp.2, col=site)) + geom_point() + facet_wrap(~site, ncol=5) + theme_bw() + theme(legend.position='none') + scale_color_manual(values=c("skyblue3","goldenrod1","indianred2","palegreen4", "darkorange3" ))

#with geom_density2d (alpha means the density 0 less density 10 more density (to see the lines more)

g2 <- ggplot(pcaLogValues, aes(y=Comp.1, x=Comp.2, col=site)) + geom_density2d(aes(col=site), alpha=0.3) + geom_point(aes(size = 3)) + facet_wrap(~site, ncol=5) + theme_bw() + theme(legend.position='none') + scale_color_manual(values=c("skyblue3","goldenrod1","indianred2","palegreen4", "darkorange3"))


# final composition

pdf('fig1.pdf', width=20, height=5)    
grid.arrange(arrangeGrob(g2,g1,heights=c(2/3, 1/3)))
dev.off()
























