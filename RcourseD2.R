# Descriptive statistic

#test for git 

# Variable selection 
test <- subset(brain, BRAIN_VOLUME > 1000)
# same as:
test <- brain[brain$BRAIN_VOLUME >800,]

bardata <- aggregate(brain$BRAIN_VOLUME, list(brain$SEX), mean)
bardata2 <- aggregate(brain$BRAIN_VOLUME, list(brain$SEX, brain$tt), mean)
bardata2$sd <- aggregate(brain$BRAIN_VOLUME, list(brain$SEX, brain$tt), sd)



bargraph <- ggplot(bardata, aes(x=factor(Group.1), y=x, fill=factor(Group.1))) + geom_bar(stat="identity")
bar2 <-  bargraph + scale_fill_manual(values=c("red", "green"))

#-------------------------

t <- ggplot(brain, aes(y=brain$BRAIN_VOLUME, x=factor(brain$SEX), fill=factor(brain$SEX))) +geom_boxplot()
t <- t + scale_fill_manual(values = c('red', 'blue'))                                                                                                            
plot(t)

# create stacked boxplot
t <- ggplot(brain, aes(y=brain$BRAIN_VOLUME, x=factor(brain$SEX), fill=factor(brain$tt))) +  
  geom_boxplot(position = position_dodge(width=0.9)) +
  geom_point(position=position_jitterdodge(dodge.width=0.9), aes(colour=(factor(brain$tt)))) 
  
plot(t)

# change colours
t <- t + scale_fill_manual(values = c('red', 'blue')) +
  scale_colour_manual(values = c('green', 'yellow'))                                                                                                        
plot(t)
  
#------------------------------

# generate randum var.
v.1 <- c('c', 't')
v.2 <- rep(v.1, 50)
v.3 <- v.2[order(runif(100))]

brain$v.3 <- v.3

# t-test

t.test <- t.test(brain$BRAIN_VOLUME ~ brain$SEX)
bvm <- subset(brain$BRAIN_VOLUME, brain$SEX=='M')
bvf <- subset(brain$BRAIN_VOLUME, brain$SEX=='F')
bvf2 <- brain[brain$BRAIN_VOLUME, brain$SEX=='F']

t.test<-t.test(bvm, bvf)
names(t.test)

# linear regression

m.1 <- lm(data=brain, BRAIN_VOLUME ~ SEX)
summary(m.1)
m.1 <- lm(data=brain, BRAIN_VOLUME ~  HEAD_SIZE)
summary(m.1)

# 2 Gerade
scatter <- ggplot(brain, aes(y=brain$BRAIN_VOLUME, x=brain$HEAD_SIZE, colour=factor(brain$SEX))) + geom_point() + geom_smooth(method='lm')
scatter2 <- scatter + theme_classic()
plot(scatter2)

# eine Gerade
scatter <- ggplot(brain, aes(y=brain$BRAIN_VOLUME, x=brain$HEAD_SIZE)) + geom_point(aes(colour=factor(brain$SEX))) + geom_smooth(method='lm')
scatter2 <- scatter + theme_classic()
plot(scatter2)
