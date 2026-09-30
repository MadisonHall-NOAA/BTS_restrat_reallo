#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(here)
library(pspline)

#Files
wd=here()

ifile1=paste0(wd,"/data/rev_strata.csv")   # revised strata
ifile2=paste0(wd,"/data/ori_strata.csv")  # orig strata
ifile4=paste0(wd,"/data/ori_strata_to_rev.csv")  # orig to rev strata link
ifile3=paste0(wd,"/data/BTS2009_2024.csv") # full data file without zero observations
ifile5=paste0(wd,"/data/StockID.csv")   #  list of 85 stocks
ifile6=paste0(wd,"/data/StockLists.csv")   #  Strata included in each unique stock

strata.rev=read.csv(ifile1, header=TRUE) 
strata.ori=read.csv(ifile2, header=TRUE) 
strata.ori.to.rev=read.csv(ifile4, header=TRUE) # used for post strat estimates
# maps original strata to new strata
StockID=read.csv(ifile5, header=TRUE)   #list of stock ID names

StockID$XITIS.List=paste0("X",StockID$ITIS.List)  # add variable to allow 
#                                                   matching with StockStraList names


StockStrataList=read.csv(ifile6, header=TRUE)   # list of strata for each stock
sumums<-StockStrataList[4:74]
sumums$total<-rowSums(sumums)
TotalStockCount<-bind_cols(StockStrataList, sumums$total)
names(TotalStockCount)[75]<-"Total"
db=read.csv(ifile3, header=TRUE)   # main data file

#RESULTS FILES
AllStrata4=paste0(wd,"/data/AllStrata4.csv")
AllStrata.df=read.csv(AllStrata4, header=T)

AllSmooth4=paste0(wd,"/data/AllSmooth4.csv")
AllSmooth.df=read.csv(AllSmooth4, header=T)

AllResults4=paste0(wd,"/data/AllResults4.csv")
AllResults.df=read.csv(AllResults4, header=T)

#SVSPP.columns=c(4:74)  # use 4:74 for full set
#Not sure i believe the above, trying something else. I WAS RIGHT!
SVSPP.columns=c(1:71)
Season.list=c("SPRING","FALL")
ITIS.list=colnames(StockStrataList[,4:74])
ip=0
plot_list<-list()
plot_index<-data.frame(
  FIG = integer(),
  TITLE = character(),
  SVSPP = character(),
  COMMON = character(),
  SEASON = character(),
  DESCRIPTOR = character()
)
# loop over species and seasons
for (i.spec  in SVSPP.columns) {      # set to 4:74 for final run
  for (SEASON.sel in Season.list){
    SVSPP.sel=ITIS.list[i.spec] 
    common<-StockID$COMMON_NAME[StockID$XITIS.List == SVSPP.sel] #MBH added to generate common name
    x1=as.numeric(AllResults.df$year[AllResults.df$species==SVSPP.sel & AllResults.df$season==SEASON.sel])
    if(length(x1>0)){
      y1=as.numeric(AllResults.df$mean.ori[AllResults.df$species==SVSPP.sel & 
                                             AllResults.df$season==SEASON.sel])
      y1.err=as.numeric(AllResults.df$se.ori[AllResults.df$species==SVSPP.sel & 
                                               AllResults.df$season==SEASON.sel])
      y2=as.numeric(AllResults.df$mean.rev[AllResults.df$species==SVSPP.sel &
                                             AllResults.df$season==SEASON.sel])
      y2.err=as.numeric(AllResults.df$se.rev[AllResults.df$species==SVSPP.sel &
                                               AllResults.df$season==SEASON.sel])
      y3=as.numeric(AllResults.df$mean.post[AllResults.df$species==SVSPP.sel & 
                                              AllResults.df$season==SEASON.sel])
      y3.err=as.numeric(AllResults.df$se.post[AllResults.df$species==SVSPP.sel & 
                                                AllResults.df$season==SEASON.sel])
      delta.y=(y1-y3)/y1 *100 
      
      # get data on variances from bootstraps for original and pooled data
      y4=as.numeric(AllResults.df$var.boot.ori[AllResults.df$species==SVSPP.sel &
                                                 AllResults.df$season==SEASON.sel])
      y5= as.numeric(AllResults.df$var.boot.rev[AllResults.df$species==SVSPP.sel &
                                                  AllResults.df$season==SEASON.sel])
      
      # get data on variances from bootstraps for original and pooled data
      y6=as.numeric(AllResults.df$sd.boot.rev[AllResults.df$species==SVSPP.sel &
                                                AllResults.df$season==SEASON.sel])
      y7= as.numeric(AllResults.df$se.post[AllResults.df$species==SVSPP.sel &
                                             AllResults.df$season==SEASON.sel])
      
      # get design effect data for original design for total, alloc effect, and
      #    stratification effect
      y8=as.numeric(AllResults.df$mean.ori.alloc[AllResults.df$species==SVSPP.sel &
                                                   AllResults.df$season==SEASON.sel])
      y9=as.numeric(AllResults.df$mean.ori.str[AllResults.df$species==SVSPP.sel &
                                                 AllResults.df$season==SEASON.sel])
      y10=as.numeric(AllResults.df$design.effic.ori[AllResults.df$species==SVSPP.sel &
                                                      AllResults.df$season==SEASON.sel])
      
      ## get design effect data for poststratified design for total, alloc effect, and
      #    stratification effect  
      y11=as.numeric(AllResults.df$mean.rev.alloc[AllResults.df$species==SVSPP.sel &
                                                    AllResults.df$season==SEASON.sel])
      y12=as.numeric(AllResults.df$mean.rev.str[AllResults.df$species==SVSPP.sel &
                                                  AllResults.df$season==SEASON.sel])
      y13=as.numeric(AllResults.df$design.effic.rev[AllResults.df$species==SVSPP.sel &
                                                      AllResults.df$season==SEASON.sel])
      
      y14=as.numeric(AllResults.df$mean.ori.max.eff[AllResults.df$species==SVSPP.sel &
                                                      AllResults.df$season==SEASON.sel])
      
      ymax=as.numeric(max(y1,y2,y3))*1.5
      ip=ip+1
      ptitle=paste("Fig. ",ip," Post & Orig Mean vs Yr: spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"MEANS (POST, ORIGINAL)"
     # plot_index[[ip]]<-ptitle
      plot(x1+.1,y1, ylim=c(0,ymax), main=ptitle, cex.main=1.5, xlab="Year", ylab="Ave Wt/Tow (kg)")
      arrows(x1+.1, y1- 2*y1.err, x1+.1, y1 + 2*y1.err, length = 0.05, angle = 90, code = 3, col="black")
      #lines(x1,y2, col="blue")
      lines(x1,y3, col="red")
      arrows(x1, y3- 2*y3.err, x1, y3 + 2*y3.err, length = 0.05, angle = 90, code = 3, col="red")
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      ip=ip+1
      ptitle=paste("Fig. ",ip," %Dif Original -PostStrat  vs Yr: spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"% DIFFERENCE IN MEANS"
      deltamax<-max(delta.y)+0.5
      plot(x1,delta.y, main=ptitle, cex.main=1.5, xlab="Year", ylab="%Dif (orig-post)/orig"
           ,ylim=c(0,deltamax) # adding this bcs several plots have y axes where ymin and ymax and everything between are the same number
           )
      abline(h=0, col="red")
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      ip=ip+1
      ptitle=paste("Fig. ",ip," Postmean vs Orig Mean +/-2SE: spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"MEANS +/- 2SE"
      plot(y1,y3, xlab="mean.orig", main=ptitle, cex.main=1.5,
           ylab="mean.poststrat", ylim=c(0,1.5*max(y3)), xlim=c(0,1.5*max(y1)))
      abline(a=0,b=1, col="blue")
      arrows(y1, y3- 2*y3.err, y1, y3 + 2*y3.err, length = 0.05, angle = 90, code = 3, col="red")
      arrows(y1- 2*y1.err, y3, y1 + 2*y1.err, y3, length = 0.05, angle = 90, code = 3, col="green")
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      ip=ip+1
      ptitle=paste("Fig. ",ip," Ratio boot Var: Ori/Pooled. spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"RATIO BOOT VAR: ORIG/POOLED"
      plot(x1,y5/y4, xlab="year", main=ptitle, cex.main=1.5,
           ylab="Ratio Ori/Pooled Var" )
      abline(h=1, col="red")
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      # compare bootstrap variance using pooled vs variance based on post stratification
      ip=ip+1
      ptitle=paste("Fig. ",ip," Post Strata SE vs Boot.pooled SE. spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"SE (BOOT POOL, POST STRAT)"
      plot(y6,y7, xlab="Boot.pool.SE", ylab="Post Strat.SE", main=ptitle,
           cex.main=1.5)
      abline(a=0, b=1, col="red")
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      # BLAND ALTMAN PLOTS FOR MEANS 
      ip=ip+1
      ptitle=paste("Fig. ",ip," Bland Altman: ORI mean vs REV mean. spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"BLAND ALTMAN - MEANS"
      plot((y1+y2)/2,y1-y2, xlab="ave of Ori &Rev Mean", ylab="Dif of Ori & Rev Mean", main=ptitle,
           cex.main=1.5)
      abline(h=0, col="red")
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      # BLAND ALTMAN PLOTS FOR SE 
      ip=ip+1
      ptitle=paste("Fig. ",ip," Bland Altman: ORI SE vs REV SE. spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"BLAND ALTMAN - SE"
      plot((y1.err+y2.err)/2,y1.err-y2.err, xlab="ave of Ori &Rev SE", ylab="Dif of Ori & Rev SE", main=ptitle,
           cex.main=1.5)
      abline(h=0, col="red")
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      
      #   analyze smooths of data for instances where number of observations is greater than 3
      #    
      if(length(x1)>3){
        ip=ip+1
        ptitle=paste("Fig. ",ip," Smooth Est for Ori Strat vs Yr for spp=",SVSPP.sel, ", season=",SEASON.sel)
        plot_index[ip,1]<-ip
        plot_index[ip,2]<-ptitle
        plot_index[ip,3]<-SVSPP.sel
        plot_index[ip,4]<-common
        plot_index[ip,5]<-SEASON.sel
        plot_index[ip,6]<-"SMOOTH EST - ORIGINAL"
        plot(x1,y1,xlab="Year", ylab="mean Wt (orig)", cex.main=1.5, main=ptitle)
        pspl.ori=smooth.Pspline(x1, y1, df=7, method=2)
        lines(pspl.ori$x,pspl.ori$ysmth, col="red")
        f1.ori = predict(pspl.ori, x1, nderiv=1)    # compute the first derivative for the smooth
        #plot(x1, f1.ori, lwd=3, lty=2, col="blue")
        plot_list[[ip]]<- recordPlot()
        dev.off()
        
        ip=ip+1
        ptitle=paste("Fig. ",ip," Smooth Est for Post Strat vs Yr for spp=",SVSPP.sel, ", season=",SEASON.sel)
        plot_index[ip,1]<-ip
        plot_index[ip,2]<-ptitle
        plot_index[ip,3]<-SVSPP.sel
        plot_index[ip,4]<-common
        plot_index[ip,5]<-SEASON.sel
        plot_index[ip,6]<-"SMOOTH EST - POSTSTRAT"
        plot(x1,y3,xlab="Year", ylab="mean Wt (Post)",  main=ptitle,cex.main=1.5)
        pspl.post=smooth.Pspline(x1, y3, df=7, method=2)
        lines(pspl.post$x,pspl.post$ysmth, col="red")
        f1.post = predict(pspl.post, x1, nderiv=1)  # compute the first derivative for the smooth
        #plot(x1, f1.post, lwd=3, lty=2, col="green")
        plot_list[[ip]]<- recordPlot()
        dev.off()
        
        ip=ip+1
        ptitle=paste("Fig. ",ip," Smooth Est for Post Strat vs Ori Strat for spp=",SVSPP.sel, ", season=",SEASON.sel)
        plot_index[ip,1]<-ip
        plot_index[ip,2]<-ptitle
        plot_index[ip,3]<-SVSPP.sel
        plot_index[ip,4]<-common
        plot_index[ip,5]<-SEASON.sel
        plot_index[ip,6]<-"SMOOTH EST - BOTH"
        plot(pspl.ori$ysmth, pspl.post$ysmth, xlab="smooth(orig)",ylab="smooth(post)",
             main=ptitle,cex.main=1.5)
        abline(a=0,b=1, col="red")
        plot_list[[ip]]<- recordPlot()
        dev.off()
        
        ip=ip+1
        ptitle=paste("Fig. ",ip," Slope Est for Post Strat vs Ori Strat for spp=",SVSPP.sel, ", season=",SEASON.sel)
        plot_index[ip,1]<-ip
        plot_index[ip,2]<-ptitle
        plot_index[ip,3]<-SVSPP.sel
        plot_index[ip,4]<-common
        plot_index[ip,5]<-SEASON.sel
        plot_index[ip,6]<-"SLOPE EST - BOTH"
        plot(f1.ori,f1.post,xlab=" smooth slope: orig strat", ylab="smooth slope: post strat",
             main=ptitle,cex.main=1.5)
        abline(a=0,b=1, col="red")
        plot_list[[ip]]<- recordPlot()
        dev.off()
      }   # end of bypass loop for smooth analyses
      
      # Plots of design effects  >>>
      # compare bootstrap variance using pooled vs variance based on post stratification
      ip=ip+1
      ptitle=paste("Fig. ",ip," Effic Alloc & Effic Strat ORIG. spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"DESIGN EFFECTS - ORIGINAL"
      plot(x1,y10, xlab="Year", ylab="Design Effect%: Alloc(blue), Strat(red)", main=ptitle, type="b",
           cex.main=1.5, ylim=c(min(y8,y9,y10),  max(y8,y9,y10)))
      lines(x1,y9, col="red")
      lines(x1,y8, col="blue")
      abline(h=0, lty=2,col="black")
      abline(h=mean(y10), col="purple", lty=7)
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      ip=ip+1
      ptitle=paste("Fig. ",ip," Effic Alloc & Effic Strat REV. spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"DESIGN EFFECT - REVISED"
      plot(x1,y13, xlab="Year", ylab="Design Effect %: Alloc(blue), Strat(red)", main=ptitle, type="b",
           cex.main=1.5, ylim=c(min(y11, y12, y13), max(y11,y12, y13)))
      lines(x1,y12, col="red")
      lines(x1,y11, col="blue")
      abline(h=0, lty=2,col="black")
      abline(h=mean(y13), col="purple", lty=7)
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      ip=ip+1
      ptitle=paste("Fig. ",ip," Design Effect:  REV vs ORIG  spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"DESIGN EFFECT - BOTH"
      plot(y10,y13, xlab="Design Effect ORIGINAL", ylab="Design Effect REVISED", main=ptitle, 
           cex.main=1.5)   #, ylim=c(0,1), xlim=c(0,1))
      abline(a=0, b=1, lty=1,col="red")
      abline(h=0, v=0,lty=2, col=c("blue","blue"))
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
      ip=ip+1
      ptitle=paste("Fig. ",ip," Design Effect:  OPT vs ORIG  spp=",SVSPP.sel, ", season=",SEASON.sel)
      plot_index[ip,1]<-ip
      plot_index[ip,2]<-ptitle
      plot_index[ip,3]<-SVSPP.sel
      plot_index[ip,4]<-common
      plot_index[ip,5]<-SEASON.sel
      plot_index[ip,6]<-"DESIGN EFFECT - OPTIMAL VS ORIGINAL"
      plot(y10,y14, xlab="Design Effect ORIGINAL", ylab="Design Effect OPTIMAL", main=ptitle, 
           cex.main=1.5)   #, ylim=c(0,1), xlim=c(0,1))
      abline(a=0, b=1, lty=1,col="red")
      abline(h=0, v=0,lty=2, col=c("blue","blue"))
      plot_list[[ip]]<- recordPlot()
      dev.off()
      
    }  # end of bypass loop for missing seasons and years
  }  # end of loop over season
}   # end of loop over species

names(plot_list)<-plot_index$FIG
plot_index<-plot_index[, -c(2)]
