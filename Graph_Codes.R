if (!require(pacman)) install.packages("pacman")
pacman::p_load(gganimate,gapminder,rmdformats,ggpubr,ggplot2,readxl, RColorBrewer,data.table,astsa, foreign, haven,PortfolioAnalytics, ROI)





scatter=function(x,y, xtitle="X Title", ytitle="Y Title", title="Title"){
data=na.omit(data.frame(x,y))
x=data$x
y=data$y
r=cor(x, y)
if(r <=0){
xp=max(x)-(max(x)-min(x))/3
yp=max(y)-(max(y)-min(y))/5
}
if(r>0){
  xp=max(x)-(max(x)-min(x))/3
  yp=min(y)+(max(y)-min(y))/5
}
p=ggplot(data, aes(x =x, y=y)) + stat_cor(method = "pearson", label.x = xp, label.y = yp,p.accuracy = 0.01, r.accuracy = 0.01)
p=p+ geom_smooth(method=lm, se=T, fullrange=FALSE, colour="black")+geom_point(colour = "darkcyan", size = 2)
p=p+theme_bw()
p=p+theme(text=element_text(face="bold", size=11))
p=p+ggtitle(title)+grids(linetype = "dashed")+theme(panel.grid.minor = element_blank())
p=p+theme(legend.position = "bottom")+theme(text=element_text(face="bold", size=11))
p=p+labs(caption = "",x=xtitle,y=ytitle)
p=p + theme(plot.title = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0))
plot(p)
}

#data <- read_excel("C:/Users/RBI/Dropbox/Rcodes/data.xlsx")
#data=read_excel("Dropbox/Rcodes/data.xlsx")

x=rnorm(200)
y=rnorm(200)+1.02*x
data=data.frame(x,y)
colnames(data)=c("X Variable", "Y Variable")
head(data)
data=data
x=data$`X Variable`
y=data$`Y Variable`

#scatter(x, y) # scatter_plot(x variable, y variable, xtitle="", ytitle="", title="")



############################################################################
#############################  GRAPH BY CATEGORY ###########################


scatter_plot_cat=function(x,y, cat, xtitle="X Title", ytitle="Y Title", title="Title", catname="Category Name"){
  r=cor(x, y,use = "complete.obs")
  if(r <=0){
    xp=max(x)-(max(x)-min(x))/4
    yp=max(y)-(max(y)-min(y))/4
  }
  if(r>0){
    xp=max(x)-(max(x)-min(x))/4
    yp=min(y)+(max(y)-min(y))/4
  }
p=ggplot(data, aes(x =x, y =y, color=cat)) + stat_cor(method = "pearson", label.x =xp, label.y = yp, color="black")
p=p+ geom_smooth(method=lm, se=T, fullrange=FALSE, colour="black")+geom_point(colour = "darkcyan", size = 3)
p=p+theme_bw()
p=p+theme(text=element_text(face="bold", size=12))
p=p+ggtitle(title)+geom_point(aes(color = cat), size=2)
p=p+theme(legend.position = "bottom")+theme(text=element_text( face="bold", size=14))
p=p+labs(color=catname, caption = "",x=xtitle,y=ytitle)
p=p + theme(plot.title = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0))
plot(p)
}


#data <- read_excel("Book1.xlsx")


#data <- read_excel("C:/Users/RBI/Dropbox/Rcodes/data.xlsx")
x=rnorm(100)
y=rnorm(100)+1.5*x
z=rep(c("A","B","C","D"),25)
data=data.frame(z,x,y)
colnames(data)=c("Company","X Variable", "Y-Variable")
head(data)
x=data$`X Variable`
y=data$`Y-Variable`
cat=data$Company
#y=x+rnorm(length(x))


#scatter_plot_cat(x, y, cat,title="", xtitle = "log (Deposits)", ytitle="Net NPA to Net Advances (%)", catname="Ownership") # scatter_plot(x variable, y yariable,category, xtitle="", ytitle="", title="")


#####################################################################
############# BAR GRAPH #############################################
#####################################################################
plot_bar=function(x,y, xtitle="X Title", ytitle="Y Title", title="Title"){
  p=ggplot(data, aes(x =x, y =y)) + geom_bar(stat="identity", fill="steelblue")
  p=p+theme_linedraw()+geom_text(aes(label=y), vjust=1.1, color="white", size=4.5)
  p=p+theme(panel.grid.major = element_line(colour = "gray", linetype = "dotted"), panel.grid.minor = element_line(colour = "gray", linetype = "dotted"))
  p=p+theme(text=element_text(family="Arial", face="bold", size=16))
  p=p+ggtitle(title)
  p=p+theme(legend.position = "bottom")+theme(text=element_text(family="Arial", face="bold", size=17))
  p=p+labs(caption = "",x=xtitle,y=ytitle)
  p=p + theme(plot.title = element_text(hjust = 0.5),plot.caption = element_text(hjust = 0))
  plot(p)
}


x=c(2,5,7,5,8,3)
y=c("A","B","C","D","E","F")
data=data.frame(y,x)
colnames(data)=c("Category","Value")
head(data)
x=data$Category
y=data$Value

#plot_bar(x, y)


#######################################################

#data <- read_excel("C:/Users/RBI/Dropbox/Rcodes/data.xlsx", 
                   #sheet = "Sheet2")
#time=data$Time
#x=data$`Internal Financing`
#y=data$`External Financing`

plot_ts=function(x,y, time, linewidth=3, xtitle="X Title", ytitle="Y Title", title="Title", startyear=c(2001,1), frequency=12, legendname=c("x name", "y name")){

mx=max(max(x,y))*(1.05)
mn=min(min(x,y))*(0.95)
x=ts(x, start =startyear, frequency=frequency)
y=ts(y, start =startyear, frequency=frequency)
  
  
  tsplot(x, ylab=ytitle,xlab = xtitle,lwd=linewidth, col="steelblue", ylim=c(mn,mx))
  lines(y, lwd=linewidth, col="maroon")
  legend('topright', col=c("steelblue","maroon"), lwd=linewidth, legend=legendname, bg='white')  
  
}

#plot_ts(x, y, startyear = c(2012,1), frequency = 4)

#################################################################
####  Secondary Axis Plot ######
#################################################################

x=seq(1,1000,1)
y1=rnorm(1000,7,3)
y2=rnorm(1000,78,13)
xtitle=c("X Title")
y1title=c("Y1 Title")
y2title=c("Y2 Title")

secondary_plot=function(x,y1,y2,xtitle,y1title,y2title){
  data=data.frame(x,y1,y2)
  k=mean(mean(y1,na.rm = TRUE)/mean(y2,na.rm = TRUE),median(y1,na.rm = TRUE)/median(y2,na.rm = TRUE))
  p <- ggplot(data, aes(x = x))+theme_bw()
  p <- p + geom_line(aes(y = y1, colour =y1title))
  p <- p + geom_line(aes(y = y2*k, colour = y2title))
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./k, name = y2title))
  p <- p + scale_colour_manual(values =brewer.pal(n = 8, name = "Dark2"))
  p <- p + labs(y = y1title,
                x = xtitle,
                colour = "")
  p <- p + theme(legend.position = c(0.8, 0.9))
  p
}

#secondary_plot(x,y1,y2,xtitle,y1title,y2title)

##Animated Images
#pltt=secondary_plot(x,y1,y2,xtitle,y1title,y2title)
#pltt+transition_time(x)



###############################################################################################################
## SHADED PHASES TIME SERIES GRAPH ##


### With and Without Secondary Axis

if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr,tidyr,RColorBrewer,ggplot2)

df <- economics %>%select(date, psavert, unemploy,uempmed) # Data to be plotted
#colnames(df)=c("Time","Position Vacancy","Unemployment Rate")
df$rec=sample(c(0,1),length(df$date), replace = T)  # Recession Dummy


ggplot(df, aes(x=date)) +
  geom_line(aes(y = unemploy, color="U L")) +
  geom_line(aes(y = uempmed*1000, color="U R (right)")) +
  geom_line(aes(y = psavert*950, colour = "E L (right)")) +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = ""))+
  scale_y_continuous(sec.axis = sec_axis(~./950, name = ""))+labs(y = "",
                                                                   x ="Time",
                                                                   colour = "")+
  scale_colour_manual(values =brewer.pal(n = 8, name = "Dark2")) +
  stat_summary(geom = "rect",
               aes(group = data.table::rleid(rec),y=1),
               fun.min = min, fun.max = max,
               orientation = "y", ymin = -Inf, ymax = Inf, alpha=0.3)+theme_bw()+theme(legend.position ="bottom",legend.title=element_blank())



##########################################
####### Confidence Interval Plot #########
##########################################


shaded_plot=function(x,y,l,u,col="#006496", xlab="xlab",ylab="ylab"){
mydata = data.frame(x,y,l,u)
ggplot(mydata, aes(x=x)) +
  geom_line(aes(y = l),linetype = "dashed", colour=col)+
  geom_line(aes(y = u),linetype = "dashed",colour=col) + 
  geom_line(aes(y = y),colour=col) + 
  geom_ribbon(aes(ymin = l, ymax =u), fill = col, alpha = .1)+theme_bw()+theme(legend.position="none")+ 
  xlab(xlab) +
  ylab(ylab)+ grids(linetype = "dashed")
}

x <- seq(0, 20, 0.001)

# Data
set.seed(1)
y1 <- 2 * cos(x) + 8
y2 <- 3 * sin(x) + 4
z=(y1+y2)/2


x=x
y=z
l=y1
u=y2
conf_plot(x,y,l,u,col="#B54C6E", xlab="xlab",ylab="ylab")

conf_plot=function(x,y,l,u,col="#006496", xlab="xlab",ylab="ylab"){
mydata = data.frame(x,y,l,u)
ggplot(mydata, aes(x=x)) +
  geom_line(aes(y = l),linetype = "dashed", colour=col) + geom_hline(yintercept=0, linetype="dashed", color = "red")+
  geom_line(aes(y = u),linetype = "dashed",colour=col) + 
  geom_line(aes(y = y),colour=col) + 
  geom_ribbon(aes(ymin = l, ymax =u), fill = col, alpha = .1)+theme_bw()+theme(legend.position="none")+ 
  xlab(xlab) +
  ylab(ylab)+ grids(linetype = "dashed")
}

#####################################################################
######################## Kernel Density Plot ########################
#####################################################################


#### Single Variable
density.plot1=function(u,xtitle="Inflation",name1="Before",adj=2){
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(ggplot2,data.table,dplyr, plyr)
  g1=u
  value <- c(g1)
  Category  <- c(rep(name1, length(g1)))
  dt    <- data.table(Category,value)
  mu <- ddply(dt, "Category", summarise, grp.mean=mean(value))
  p<-ggplot(dt, aes(x=value, color=Category)) +
    geom_density(adjust=adj, alpha=0.2)+
    geom_vline(data=mu, aes(xintercept=grp.mean, color=Category),
               linetype="dashed")+theme_bw()+scale_color_manual(values=c( "dodgerblue4", "indianred3"))+scale_fill_manual(values=c("dodgerblue4", "indianred3"))
  p=p+expand_limits(x = min(value) -0.5*(max(value)-min(value)))+expand_limits(x = max(value) +0.5*(max(value)-min(value)))
  p=p+xlab(xtitle) + ylab("Density")+theme(legend.position="bottom",legend.title = element_blank())+ggplot2::annotate("text", x =c( max(value), max(value)), y = c(0.75*max((density(value)$y)),0.67*max((density(value)$y))), label =c(paste('Sample Mean = ',round(mean(value),digits=2)),""),parse = FALSE)
  p
}


#### Two Variables ####
density.plot2=function(u,v,xtitle="Inflation",name1="Before", name2="After",adj=2){
  if (!require(pacman)) install.packages("pacman")
  pacman::p_load(ggplot2,data.table,dplyr, plyr)
  g1=u
  g2=v
  tt=t.test(g1,g2,paired = TRUE)
  md=round(tt$estimate,digits=2)
  pval=round(tt$p.value,digits=2)
  value <- c(g1, g2)
  Category  <- c(rep(name1, length(g1)), rep(name2, length(g2)))
  dt    <- data.table(Category,value)
  mu <- ddply(dt, "Category", summarise, grp.mean=mean(value))
  p<-ggplot(dt, aes(x=value, color=Category)) +
    geom_density(adjust=adj, alpha=0.2)+
    geom_vline(data=mu, aes(xintercept=grp.mean, color=Category),
               linetype="dashed")+theme_bw()+scale_color_manual(values=c("indianred3", "dodgerblue4", "indianred3"))+scale_fill_manual(values=c("indianred3", "dodgerblue4", "indianred3"))
  p=p+expand_limits(x = min(value) -0.5*(max(value)-min(value)))+expand_limits(x = max(value) +0.5*(max(value)-min(value)))
  p=p+xlab(xtitle) + ylab("Density")+theme(legend.position="bottom",legend.title = element_blank())+ggplot2::annotate("text", x =c( max(value), max(value)), y = c(0.75*max((density(value)$y)),0.67*max((density(value)$y))), label =c(paste('Mean Difference = ',md),paste('t-test p-value =',max(pval,0.01))),parse = FALSE)
  p
}



make_panel=function(dataname=data, panelname="Panel Name", variable="Variable Name"){
  data=dataname
  colnames(data)[which(colnames(data)==panelname)]=c("Panel")
  colnames(data)[which(colnames(data)==variable)]=c("Variable")
  molted=melt(data,id.vars=c("Panel","Variable"))
  pdata=dcast(molted,Panel+variable~Variable, fun.aggregate =mean)   #id is column
  colnames(pdata)[which(colnames(pdata)=="Panel")]=panelname
  colnames(pdata)[which(colnames(pdata)=="variable")]=c("Time")
  return(pdata)
}
