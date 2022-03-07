if (!require(pacman)) install.packages("pacman")
pacman::p_load(gganimate,gapminder,rmdformats,ggpubr,ggplot2,readxl, RColorBrewer,data.table,astsa, foreign, haven,PortfolioAnalytics, ROI)





scatter=function(x,y, xtitle="X Title", ytitle="Y Title", title="Title"){
data=na.omit(data.frame(x,y))
x=data$x
y=data$y
r=cor(x, y)
if(r <=0){
xp=max(x)-(max(x)-min(x))/4
yp=max(y)-(max(y)-min(y))/4
}
if(r>0){
  xp=max(x)-(max(x)-min(x))/4
  yp=min(y)+(max(y)-min(y))/4
}
p=ggplot(data, aes(x =x, y=y)) + stat_cor(method = "pearson", label.x = xp, label.y = yp)
p=p+ geom_smooth(method=lm, se=T, fullrange=FALSE, colour="black")+geom_point(colour = "darkcyan", size = 3)
p=p+theme_bw()
p=p+theme(text=element_text(face="bold", size=12))
p=p+ggtitle(title)
p=p+theme(legend.position = "bottom")+theme(text=element_text(face="bold", size=14))
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
  k=mean(mean(y1,na.rm = TRUE)/mean(y2,na.rm = TRUE),median(y1,na.rm = TRUE)/median(y2,na.rm = TRUE))/sd(y2,na.rm=T)
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



