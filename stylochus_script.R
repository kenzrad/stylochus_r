#Author: M.Kensey Barker
#Date last modified: 28Apr2014
#Statistical analyses for master's thesis (Stylochus ellipticus)

##############################################################################################

#CITATIONS
citation(package = "lme4", lib.loc = NULL) #citation information for package
#Bates D, Maechler M, Bolker B, and Walker S. 2013. lme4: linear mixed-effects models using Eigen and S4. R package version 1.1-6. <http://CRAN.R-project.org/package=lme4>.
citation() #citation for R
#R Core Team. 2014. A language and environment for statistical computing. R Foundation for Statistical Computing. Vienna, Austria. <http://www.R-project.org/>

#set working directory
setwd("~/Desktop/Kenzrad Drive/Thesis/Data/R Statistics")
#packages
library(lme4) #for nonlinear regression (presence/absence)



##############################################################################################



#-------------------------------------------#
#-------------------PT. 1-------------------#
#-----------STYLOCHUS ABUNDANCE-------------#
#-------------------------------------------#


#files for abundance (after reformatting pain and suffering)
a_all<-read.csv("pleasenomore.csv") #field data, both top and bottom tiles w/environmental
a_top<-read.csv("please_top.csv") #field data, top tiles w/environmental
a_bottom<-read.csv("please_bottom.csv") #field data, bottom tiles w/environmental
#for all figures, colors should be assigned by system:
    #James River: col = "deepskyblue3"
    #Piankatank River: col = "firebrick"
    #Great Wicomico River: col = "darkolivegreen4"
#tests used:
  #kruskal.test() #used for categorical 
  #wilcox() #used for categorical w/two possibilities (top/bottom)



#------------------#
#--ABUND~POSITION--#
#------------------#

#Test for differences in tile position (top/bottom)
#parametric
oneway.test(a_all$abundance~a_all$Orientation) #p-value = 0.02605
#non-parametric
wilcox.test(a_all$abundance~a_all$Orientation) #p-value = 0.002784
#boxplot of distrubtions of abundance~position
boxplot(a_all$abundance~a_all$Orientation, 
        names=c("Surface", "Benthic"),
        par(cex.axis=.5),
        xlab="Tile Position in Water Column", 
        ylab="Stylochus Abundance")
#differences suggest that top tiles and bottom tiles should be analyzed separately


#------------------#
#----ABUND~SITE----#
#------------------#

#top tiles
#box plot (site, abund)
boxplot(a_top$abundance~a_top$code, 
        names=c("Whaley's East", "Rough Point", "Glebe Point", "Ginney Point", "Wilton Creek", "Burton Point", "Day's Point", "Point of Shoal", "Deep Water Shoal"),
        main="Site Stylochus Abundance of Surface Tiles", 
        par(cex.axis=.5), #this changes the font in the x-axis
        xlab="Site", 
        ylab="Stylochus Abundance",
        col=c("darkolivegreen4", "darkolivegreen4", "darkolivegreen4", "firebrick", "firebrick", "firebrick", "deepskyblue3", "deepskyblue3", "deepskyblue3")) 
#non-parametric test 
kruskal.test(a_top$abundance~a_top$Site) #p-value = 0.0882

#bottom tiles
#box plot (site, abund) 
boxplot(a_bottom$abundance~a_bottom$code, 
        names=c("Whaley's East", "Rough Point", "Glebe Point", "Ginney Point", "Burton Point", "Day's Point", "Point of Shoal", "Deep Water Shoal"),
        main="Site Stylochus Abundance in Benthic Tiles", 
        par(cex.axis=.5),
        xlab="Site", 
        ylab="Stylochus Abundance",
        col=c("darkolivegreen4", "darkolivegreen4", "darkolivegreen4", "firebrick", "firebrick", "deepskyblue3", "deepskyblue3", "deepskyblue3"))
#non-parametric test 
kruskal.test(a_bottom$abundance~a_bottom$Site) #p-value = 0.2095

#all tiles
#box plot (site, abund) 
boxplot(a_all$abundance~a_all$code, 
        names=c("Whaley's East", "Rough Point", "Glebe Point", "Ginney Point", "Wilton Creek", "Burton Point", "Day's Point", "Point of Shoal", "Deep Water Shoal"),
        main="Site Stylochus Abundance", 
        par(cex.axis=.5),
        xlab="Site", 
        ylab="Stylochus Abundance",
        col=c("darkolivegreen4", "darkolivegreen4", "darkolivegreen4", "firebrick", "firebrick", "firebrick", "deepskyblue3", "deepskyblue3", "deepskyblue3"))
#non-parametric test 
kruskal.test(a_all$abundance~a_all$Site) #p-value = 0.01063



#------------------#
#----ABUND~DATE----#
#------------------#

#date top
#boxplot (date, abund)
boxplot(a_top$abundance~a_top$datecode, 
        names=c("20Jun", "11Jul", "01Aug", "22Aug", "12Sep", "26Sep"),
        par(cex.axis=.5),
        xlab="Date", 
        ylab="Stylochus Abundance")
#non-parametric test
kruskal.test(a_top$abundance~a_top$datecode) #p-value = 0.4429
anova(lm(a_top$abundance~a_top$datecode)) #p-value = 0.01801
plot(lm(a_top$abundance~a_top$datecode))

#date bottom
#boxplot (date, abund)
boxplot(a_bottom$abundance~a_bottom$datecode, 
        names=c("20Jun", "11Jul", "01Aug", "22Aug", "12Sep", "26Sep"),
        par(cex.axis=.5),
        xlab="Date", 
        ylab="Stylochus Abundance")
#non-parametric test
kruskal.test(a_bottom$abundance~a_bottom$datecode) #p-value = 0.5529
anova(lm(a_bottom$abundance~a_bottom$datecode)) #p-value= 0.4043
plot(lm(a_bottom$abundance~a_bottom$datecode))

#------------------#
#----ABUND~TEMP----#
#------------------#

#temp top
#scatter plot for (temp, abund)
plot(a_top$abundance~a_top$temp, 
     par(cex.axis=.5),
     xlab="Temp", 
     ylab="Stylochus Abundance",
     type="p",
     pch=20)
cor(a_top$temp, a_top$abundance) #r = 0.04302261
#scatter plot for (temp, log(abund+2)
plot(log(a_top$abundance+2)~a_top$temp, 
        par(cex.axis=.5),
        xlab="Temp", 
        ylab="log(Stylochus Abundance)",
        type="p",
        pch=20)
cor(a_top$temp, log(a_top$abundance+2)) #r = -0.01717795
#non-parametric test
kruskal.test(a_top$abundance~a_top$temp) #p-value = 0.3671
anova(lm(a_top$abundance~a_top$temp)) #p-value = 0.7574
plot(lm(a_top$abundance~a_top$temp))

#temp bottom
#scatterplot (temp, abund)
plot(a_bottom$abundance~a_bottom$temp, 
            par(cex.axis=.5),
            xlab="Temp", 
            ylab="Stylochus Abundance",
            type="p",
            pch=20)
#scatterplot (temp, log(abund+2)) 
plot(log(a_bottom$abundance+2)~a_bottom$temp, 
     par(cex.axis=.5),
     xlab="Temp", 
     ylab="log(Stylochus Abundance)",
     type="p",
     pch=20)
#correlation
cor(a_bottom$temp, a_bottom$abund)
#non-parametric test
kruskal.test(a_bottom$abundance~a_bottom$temp) #p-value = 0.3141
anova(lm(a_bottom$abundance~a_bottom$temp)) #p-value = 0.8266

#all
plot(log(a_all$abundance+2)~a_all$temp, 
     par(cex.axis=.5),
     xlab="Temp", 
     ylab="log(Stylochus Abundance)",
     type="p",
     pch=20)

#------------------#
#-----ABUND~SAL----#
#------------------#

#salinity top
#scatterplot (sal, abund)
plot(a_top$abundance~a_top$salinity, 
     par(cex.axis=.5),
     xlab="Salinity (ppt)", 
     ylab="Stylochus Abundance",
     type="p",
     pch=20)
#scatterplot (sal, log(abund+2))
plot(log(a_top$abundance+2)~a_top$salinity, 
     par(cex.axis=.5),
     xlab="Salinity (ppt)", 
     ylab="log(Stylochus Abundance)",
     type="p",
     pch=20)
#correlation
cor(a_top$salinity, a_top$abundance) #r = 0.1714807
#non-parametric test
kruskal.test(a_top$abundance~a_top$salinity) #p-value = 0.5155


#sal bottom
#scatterplot (sal, abund)
plot(a_bottom$abundance~a_bottom$salinity, 
     par(cex.axis=.5),
     xlab="Salinity (ppt)", 
     ylab="Stylochus Abundance",
     type="p",
     pch=20)
#scatterplot (sal,log(abund+2) 
plot(log(a_bottom$abundance+2)~a_bottom$salinity, 
     par(cex.axis=.5),
     xlab="Salinity (ppt)", 
     ylab="log(Stylochus Abundance)",
     type="p",
     pch=20)
#correlation
cor(a_bottom$salinity, a_bottom$abundance) #r = 0.1023485
#non-parametric test
kruskal.test(a_bottom$abundance~a_bottom$salinity) #p-value = 0.5846

#sal all
#scatter plot (abund, sal)
plot(a_all$abund~a_all$salinity, 
     par(cex.axis=.5),
     xlab="Salinity (ppt)", 
     ylab="Stylochus Abundance",
     main="Abundance of Stylochus as a function of Salinity (ppt)",
     type="p",
     pch=20)
#scatterplot (sal, log(abund+2)
plot(log(a_all$abundance+2)~a_all$salinity, 
     par(cex.axis=.5),
     xlab="Salinity (ppt)", 
     ylab="log(Stylochus Abundance)",
     main="Abundance of Stylochus as a function of Salinity (ppt)",
     type="p",
     pch=20)
#correlation
cor(a_all$salinity, a_all$abundance) #r = 0.1394279
#non-parametric test
kruskal.test(a_all$abundance~a_all$salinity) #p-value = 0.1013



#------------------#
#----ABUND~MULTI---#
#------------------#

#ANOVA with temp, salinity and temp + salinity
two.wayABUND <- lm(abundance~salinity+temp+salinity*temp, data=a_top)
anova(two.wayABUND)
#sal p-value = 0.2240
#temp p-value = 0.8687
#sal+temp p-value = 0.9293


#------------------#
#----ABUND~SPAT----#
#------------------#

head(a_bottom)
#top tiles
#box plot (site, abund)
boxplot(a_top$abundance~a_top$code, 
        names=c("Whaley's East", "Rough Point", "Glebe Point", "Ginney Point", "Wilton Creek", "Burton Point", "Day's Point", "Point of Shoal", "Deep Water Shoal"),
        main="Site Stylochus Abundance of Surface Tiles", 
        par(cex.axis=.5), #this changes the font in the x-axis
        xlab="Site", 
        ylab="Stylochus Abundance",
        col=c("darkolivegreen4", "darkolivegreen4", "darkolivegreen4", "firebrick", "firebrick", "firebrick", "deepskyblue3", "deepskyblue3", "deepskyblue3")) 
#non-parametric test 
kruskal.test(a_all$abundance~a_all$spat) #p-value = 0.0882

#bottom tiles
#box plot (site, abund) 
boxplot(a_bottom$abundance~a_bottom$code, 
        names=c("Whaley's East", "Rough Point", "Glebe Point", "Ginney Point", "Burton Point", "Day's Point", "Point of Shoal", "Deep Water Shoal"),
        main="Site Stylochus Abundance in Benthic Tiles", 
        par(cex.axis=.5),
        xlab="Site", 
        ylab="Stylochus Abundance",
        col=c("darkolivegreen4", "darkolivegreen4", "darkolivegreen4", "firebrick", "firebrick", "deepskyblue3", "deepskyblue3", "deepskyblue3"))
#non-parametric test 
kruskal.test(a_bottom$abundance~a_bottom$Site) #p-value = 0.2095



##############################################################################################



#-------------------------------------------#
#-------------------PT. 2-------------------#
#-----------STYLOCHUS PREDATION-------------#
#-------------------------------------------#

#Read in files
pres<-read.csv("binary_presence.csv") #binary data with concatanated sites

#load package for nonlinear mixed models
library(lme4) 


#------------------#
#-----POSITION-----#
#------------------#

#use function glmer() for generalize linear mixed-effect models with small-option fixed effect (doesn't work with date/salinity)
#oy is "presence/absence", binomial value
#position is fixed effect
#sitecode is random effect
prop.big.pos <- glmer(oy~position+(1 | sitecode), data=bigdata, family=binomial(link='logit'))
summary(prop.big.pos) #p-value = 0.343972

?glm
sal.glm<-glm(result~orienation+sal, family=binomial, data=pres)
anova(sal.glm)
summary(sal.glm)
predict(sal.glm, type="response")
residuals(sal.glm, type="response")
cdplot(result~sal, data=pres)

temp.glm<-glm(result~spat_rack+orienation, family=binomial, data=pres)
anova(temp.glm)
summary(temp.glm)


head(pres)

pres1.glmer <- glmer(result~orienation+(1 | site), data=pres, family=binomial(link='logit'))
summary(pres1.glmer) #p-value = 0.414


#------------------#
#-------SITE-------#
#------------------#

#site is a random effect, but may affect outcome - tested here
prop.big1 <- glm(oy~position, data=bigdata, family=binomial(link='logit'))
summary(prop.big1)
prop.big.test1 <- -2*(logLik(prop.big1)-logLik(prop.big))
prop.big.pvalue <- pchisq(prop.big.test1, df=1, lower.tail=FALSE)
prop.big.pvalue #log Like. 4.096265e-06
#site has an effect - but randomly sampled, so not known


#site is a random effect, but may affect outcome - tested here
pres.glm <- glm(result~orienation, data=pres, family=binomial(link='logit'))
summary(pres.glm)
pres.test <- -2*(logLik(pres.glm)-logLik(pres1.glmer))
pres.pvalue <- pchisq(pres.test, df=1, lower.tail=FALSE)
pres.pvalue #log Like. 1.173718e-06
#site has an effect - but randomly sampled, so not known

#------------------#
#--------DATE------#
#------------------#

unique(pres$date) #this function gives me all my unique classifications in the "date" column, allows pairwise analysis

#for each dat pairwise
presdate1 <- pres[pres$date %in% c("20-Jun","11-Jul"),]
presdate1.glmer<- glmer(result~date+(1 | site), data=presdate1, family=binomial(link='logit'))
summary(presdate1.glmer) #p-value = 0.974

presdate2 <- pres[pres$date %in% c("20-Jun","1-Aug"),]
presdate2.glmer<- glmer(result~date+(1 | site), data=presdate2, family=binomial(link='logit'))
summary(presdate2.glmer) #p-value = 0.000808

presdate3 <- pres[pres$date %in% c("20-Jun","22-Aug"),]
presdate3.glmer<- glmer(result~date+(1 | site), data=presdate3, family=binomial(link='logit'))
summary(presdate3.glmer) #p-value = 0.000149

presdate4 <- pres[pres$date %in% c("20-Jun","12-Sep"),]
presdate4.glmer<- glmer(result~date+(1 | site), data=presdate4, family=binomial(link='logit'))
summary(presdate4.glmer) #p-value = 0.080320

presdate5 <- pres[pres$date %in% c("20-Jun","26-Sep"),]
presdate5.glmer<- glmer(result~date+(1 | site), data=presdate5, family=binomial(link='logit'))
summary(presdate5.glmer) #p-value = 0.17417

presdate6 <- pres[pres$date %in% c("11-Jul","1-Aug"),]
presdate6.glmer<- glmer(result~date+(1 | site), data=presdate6, family=binomial(link='logit'))
summary(presdate6.glmer) #p-value: 0.0338

presdate7 <- pres[pres$date %in% c("11-Jul","22-Aug"),]
presdate7.glmer<- glmer(result~date+(1 | site), data=presdate7, family=binomial(link='logit'))
summary(presdate7.glmer) #p-value: 0.750

presdate8 <- pres[pres$date %in% c("11-Jul","12-Sep"),]
presdate8.glmer<- glmer(result~date+(1 | site), data=presdate8, family=binomial(link='logit'))
summary(presdate8.glmer) #p-value: 0.5131

presdate9 <- pres[pres$date %in% c("11-Jul","26-Sep"),]
presdate9.glmer<- glmer(result~date+(1 | site), data=presdate9, family=binomial(link='logit'))
summary(presdate9.glmer) #p-value: 0.7856

presdate10 <- pres[pres$date %in% c("1-Aug","22-Aug"),]
presdate10.glmer<- glmer(result~date+(1 | site), data=presdate10, family=binomial(link='logit'))
summary(presdate10.glmer) #p-value: 0.0338

presdate11 <- pres[pres$date %in% c("1-Aug","12-Sep"),]
presdate11.glmer<- glmer(result~date+(1 | site), data=presdate11, family=binomial(link='logit'))
summary(presdate11.glmer) #p-value: 0.1366

presdate12 <- pres[pres$date %in% c("1-Aug","26-Sep"),]
presdate12.glmer<- glmer(result~date+(1 | site), data=presdate12, family=binomial(link='logit'))
summary(presdate12.glmer) #p-value: 0.00523

presdate13 <- pres[pres$date %in% c("22-Aug","12-Sep"),]
presdate13.glmer<- glmer(result~date+(1 | site), data=presdate13, family=binomial(link='logit'))
summary(presdate13.glmer) #p-value: 0.39

presdate14 <- pres[pres$date %in% c("22-Aug","26=Sep"),]
presdate14.glmer<- glmer(result~date+(1 | site), data=presdate14, family=binomial(link='logit'))
summary(presdate14.glmer) #p-value: 0.3301

presdate15 <- pres[pres$date %in% c("12-Sep","26-Sep"),]
presdate15.glmer<- glmer(result~date+(1 | site), data=presdate15, family=binomial(link='logit'))
summary(presdate15.glmer) #p-value: 0.58804

