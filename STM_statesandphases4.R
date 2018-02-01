library(grid)
# Text -------------------------------------------------
Pathlabel <- read.delim("C:/workspace/pathlabel.txt")
Boxlabel <- read.delim("C:/workspace/boxlabel.txt")
Statelabel <- read.delim("C:/workspace/stateboxlabel.txt")
Translabel <- read.delim("C:/workspace/translabel.txt")
currentsite<-"ES003"

thisSiteBoxes<-Boxlabel[Boxlabel$ES %in% currentsite & Boxlabel$State %in% c(1:7),]
thisSiteStates<-Statelabel[Statelabel$ES %in% currentsite & Statelabel$State %in% c(1:7),]
thissitePaths<-Pathlabel[Pathlabel$ES %in% currentsite & Pathlabel$State %in% c(1:7),]
thissiteTrans<-Translabel[Translabel$ES %in% currentsite & Translabel$State %in% c(1:7),]
#function for identifying pathways-----------------------------
assignpath<-function(p,q){
  arrowpq<-data.frame(0:1,0:1)
  #1   overlapping staggered columns
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==0.5 & abs((Boxes[p,2]-Boxes[q,2])/4)==1){
    if(Boxes[p,1]<Boxes[q,1]){
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,3,],BoxEdge[q,12,])
      }else
        arrowpq<- data.frame(BoxEdge[p,9,],BoxEdge[q,2,])
    }else
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,1,],BoxEdge[q,10,])
      }else
        arrowpq<- data.frame(BoxEdge[p,11,],BoxEdge[q,4,])
  } 
  #2   single column, gap and offset
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==0 & abs((Boxes[p,2]-Boxes[q,2])/4)==2){
    if(Boxes[p,1]< 7){
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,1,],BoxEdge[q,12,])
      }else
        arrowpq<- data.frame(BoxEdge[p,11,],BoxEdge[q,2,])
    }else
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,3,],BoxEdge[q,10,])
      }else
        arrowpq<- data.frame(BoxEdge[p,9,],BoxEdge[q,4,])  
  }
  #3   distant staggered columnns
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==2 & abs((Boxes[p,2]-Boxes[q,2])/4)==0.5){
    if(Boxes[p,2]> 7){
      if(Boxes[p,1]<Boxes[q,1]){
        if(Boxes[p,2]<Boxes[q,2]){
          arrowpq<- data.frame(BoxEdge[p,5,],BoxEdge[q,16,])
        }else
          arrowpq<- data.frame(BoxEdge[p,7,],BoxEdge[q,16,])
      }else
        if(Boxes[p,2]<Boxes[q,2]){
          arrowpq<- data.frame(BoxEdge[p,15,],BoxEdge[q,8,])
        }else
          arrowpq<- data.frame(BoxEdge[p,15,],BoxEdge[q,6,])
    }else
      if(Boxes[p,1]<Boxes[q,1]){
        if(Boxes[p,2]<Boxes[q,2]){
          arrowpq<- data.frame(BoxEdge[p,6,],BoxEdge[q,13,])
        }else
          arrowpq<- data.frame(BoxEdge[p,7,],BoxEdge[q,14,])
      }else
        if(Boxes[p,2]<Boxes[q,2]){
          arrowpq<- data.frame(BoxEdge[p,13,],BoxEdge[q,8,])
        }else
          arrowpq<- data.frame(BoxEdge[p,14,],BoxEdge[q,5,])
  }
  
  
  #4   adjacent staggered columns, distant
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==1.5 & abs((Boxes[p,2]-Boxes[q,2])/4)==1.5){
    if(Boxes[p,1]<Boxes[q,1]){
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,3,],BoxEdge[q,14,])
      }else
        arrowpq<- data.frame(BoxEdge[p,9,],BoxEdge[q,16,])
    }else
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,15,],BoxEdge[q,10,])
      }else
        arrowpq<- data.frame(BoxEdge[p,13,],BoxEdge[q,4,])  
  }
  
  #5   adjacent staggered columns, adjacent
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==1.5 & abs((Boxes[p,2]-Boxes[q,2])/4)==0.5){
    if(Boxes[p,1]<Boxes[q,1]){
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,6,],BoxEdge[q,13,])
      }else
        arrowpq<- data.frame(BoxEdge[p,8,],BoxEdge[q,15,])
    }else
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,16,],BoxEdge[q,7,])
      }else
        arrowpq<- data.frame(BoxEdge[p,14,],BoxEdge[q,5,])  
  }
  
  #6   single column, adjacent
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==0 & abs((Boxes[p,2]-Boxes[q,2])/4)==1){
    
    if(Boxes[p,2]<Boxes[q,2]){
      arrowpq<- data.frame(BoxEdge[p,2,],BoxEdge[q,11,])
    }else
      arrowpq<- data.frame(BoxEdge[p,10,],BoxEdge[q,3,])
    
  }
  
  #7   side by side
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==2 & abs((Boxes[p,2]-Boxes[q,2])/4)==0){
    if(Boxes[p,1]<Boxes[q,1]){
      arrowpq<- data.frame(BoxEdge[p,6,],BoxEdge[q,15,])
    }else
      arrowpq<- data.frame(BoxEdge[p,14,],BoxEdge[q,7,])
  }
  #8   diagonal for a 4 box arrangement
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==2 & abs((Boxes[p,2]-Boxes[q,2])/4)==1){
    if(Boxes[p,1]<Boxes[q,1]){
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,6,],BoxEdge[q,13,])
      }else
        arrowpq<- data.frame(BoxEdge[p,7,],BoxEdge[q,16,])
    }else
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,15,],BoxEdge[q,8,])
      }else
        arrowpq<- data.frame(BoxEdge[p,14,],BoxEdge[q,5,])  
  }
  #9   diagonal for middle box of a 5 box arrangement
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==1 & abs((Boxes[p,2]-Boxes[q,2])/4)==1){
    if(Boxes[p,1]<Boxes[q,1]){
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,3,],BoxEdge[q,12,])
      }else
        arrowpq<- data.frame(BoxEdge[p,9,],BoxEdge[q,2,])
    }else
      if(Boxes[p,2]<Boxes[q,2]){
        arrowpq<- data.frame(BoxEdge[p,1,],BoxEdge[q,10,])
      }else
        arrowpq<- data.frame(BoxEdge[p,11,],BoxEdge[q,4,])  
      
  }
  #10   side by side closer
  if(abs((Boxes[p,1]-Boxes[q,1])/4)==1 & abs((Boxes[p,2]-Boxes[q,2])/4)==0){
    if(Boxes[p,1]<Boxes[q,1]){
      arrowpq<- data.frame(BoxEdge[p,6,],BoxEdge[q,15,])
    }else
      arrowpq<- data.frame(BoxEdge[p,14,],BoxEdge[q,7,])
  }
  return(arrowpq)
}
#Processing State Boxes-----------------------------
Boxlabelmax<-aggregate(thisSiteBoxes[,c("x","y")], by=list(thisSiteBoxes$ES,thisSiteBoxes$State), FUN=max)
colnames(Boxlabelmax)<-c("ES","State","xmax","ymax")
Boxlabelmin<-aggregate(thisSiteBoxes[,c("x","y")], by=list(thisSiteBoxes$ES,thisSiteBoxes$State), FUN=min)
colnames(Boxlabelmin)<-c("ES","State","xmin","ymin")
StateBoxes<-merge(Boxlabelmax,Boxlabelmin, by=c("ES","State"))
StateBoxes$xsize<-StateBoxes$xmax - StateBoxes$xmin + 6
StateBoxes$ysize<-StateBoxes$ymax - StateBoxes$ymin + 4

StateBox<-StateBoxes[,c(7,8)]

StateLab<-paste("State ",thisSiteStates$State,": ",thisSiteStates$State.Name,sep = "")
StatePos<-thisSiteStates[,c("x","y")]
MargL<-StatePos[,1]-StateBox[,1]/2
MargR<-StatePos[,1]+StateBox[,1]/2
MargB<-StatePos[,2]-StateBox[,2]/2
MargT<-StatePos[,2]+StateBox[,2]/2

grdfact<-20

#start drawing state boxes----------------

#png(filename=paste("stm_",currentsite,".png", sep=""),width = 39, height = 20, units = 'cm', res = 100)
grid.newpage()
dvr<-min(dev.size()[2]/8.5,dev.size()[2]/11)
vpscale<-1
pushViewport(viewport(x = unit(0.5,"snpc"), y = unit(0.5,"snpc"), width = unit(vpscale,"snpc"), height =unit(vpscale,"snpc")))
for (s in 1:nrow(StateBoxes)){
  pushViewport(viewport(x = unit(StatePos[s,1]/grdfact,"snpc"), y = unit(StatePos[s,2]/grdfact,"snpc"), width = unit(StateBox[s,1]/grdfact,"snpc"), height =unit(StateBox[s,2]/grdfact,"snpc")))
  #outer state rectangle-------------------------------
  grid.rect(gp=gpar(lwd=2))
  grid.text(StateLab[s] ,x = 0.05, y = 0.95, just=c("left","top"), gp=gpar(fontsize=20*dvr*vpscale*16/grdfact, col="black",fontface="bold"))
  upViewport()
  
}
#Define edge points along state boxes-----------------
p1<-c(-0.375,0.5)
p2<-c(-0.125,0.5)
p3<-c(0.125,0.5)
p4<-c(0.375,0.5)
p5<-c(0.5,0.375)
p6<-c(0.5,0.125)
p7<-c(0.5,-0.125)
p8<-c(0.5,-0.375)
p9<-c(0.375,-0.5)
p10<-c(0.125,-0.5)
p11<-c(-0.125,-0.5)
p12<-c(-0.375,-0.5)
p13<-c(-0.5,-0.375)
p14<-c(-0.5,-0.125)
p15<-c(-0.5,0.125)
p16<-c(-0.5,0.375)

#put points on stateboxes------------------------
EdgelistX<-c(p1[1], p2[1], p3[1], p4[1], p5[1], p6[1], p7[1], p8[1], p9[1], p10[1], p11[1], p12[1], p13[1], p14[1], p15[1], p16[1])

EdgelistY<-c(p1[2], p2[2], p3[2], p4[2], p5[2], p6[2], p7[2], p8[2], p9[2], p10[2], p11[2], p12[2], p13[2], p14[2], p15[2], p16[2])
EdgelistState<-data.frame(EdgelistX,EdgelistY)

trowtotal<-nrow(thisSiteStates)
SBoxEdge<-array(c(1:trowtotal,1:16,1:2),dim=c(7,16,2))
for (i in 1:trowtotal){
  for (j in 1:16){
    for (k in 1:2){
      SBoxEdge[i,j,k] <- StatePos[i,k]+EdgelistState[j,k]*StateBox[i,k]
    }}}


#function for identifying transitions-----------------------------
assigntrans<-function(p,q){
  tarrowpq<-data.frame(0:1,0:1,0:1,0:1)
  
####1   single column near
  if(abs((StatePos[p,1]-StatePos[q,1]))< 6 & (min(abs(MargT[p]-MargB[q]),abs(MargB[p]-MargT[q])))<=2){
    
    if(StatePos[p,2]<StatePos[q,2]){
      tarrowpq<- data.frame(SBoxEdge[p,2,],SBoxEdge[p,2,]*.75+SBoxEdge[q,11,]*.25, SBoxEdge[p,2,]*.25+SBoxEdge[q,11,]*.75,SBoxEdge[q,11,])
    }else
      tarrowpq<- data.frame(SBoxEdge[p,10,],SBoxEdge[p,10,]*.75+SBoxEdge[q,3,]*.25, SBoxEdge[p,10,]*.25+SBoxEdge[q,3,]*.75,SBoxEdge[q,3,])
    
  }
  
####2   side by side closer
if((min(abs(MargR[p]-MargL[q]),abs(MargL[p]-MargR[q]))) < 12 & abs((StatePos[p,2]-StatePos[q,2]))<2){
  if(StatePos[p,1]<StatePos[q,1]){
    tarrowpq<- data.frame(SBoxEdge[p,6,],(SBoxEdge[p,6,]*.75+SBoxEdge[q,15,]*.25),(SBoxEdge[p,6,]*.25+SBoxEdge[q,15,]*.75), SBoxEdge[q,15,],SBoxEdge[q,15,])
  }else
    tarrowpq<- data.frame(SBoxEdge[p,14,],(SBoxEdge[p,14,]*.75+SBoxEdge[q,7,]*.25),(SBoxEdge[p,14,]*.25+SBoxEdge[q,7,]*.75),SBoxEdge[q,7,])
}

  
####3   overlapping staggered columns
  if((min(abs(MargR[p]-StatePos[q,1]),abs(MargL[p]-StatePos[q,1])))<=0.5 & (min(abs(MargT[p]-MargB[q]),abs(MargB[p]-MargT[q])))<=2){
    if(StatePos[p,1]<StatePos[q,1]){
      if(StatePos[p,2]<StatePos[q,2]){
        tarrowpq<- data.frame(SBoxEdge[p,3,],SBoxEdge[p,3,]*0.75+SBoxEdge[q,12,]*0.25,SBoxEdge[p,3,]*0.25+SBoxEdge[q,12,]*0.75,SBoxEdge[q,12,])
      }else
        tarrowpq<- data.frame(SBoxEdge[p,9,],SBoxEdge[p,9,]*0.75+SBoxEdge[q,2,]*0.25,SBoxEdge[p,9,]*0.25+SBoxEdge[q,2,]*0.75,SBoxEdge[q,2,])
    }else
      if(StatePos[p,2]<StatePos[q,2]){
        tarrowpq<- data.frame(SBoxEdge[p,1,],SBoxEdge[p,1,]*.75+SBoxEdge[q,10,]*.25,SBoxEdge[p,1,]*.25+SBoxEdge[q,10,]*.75,SBoxEdge[q,10,])
      }else
        tarrowpq<- data.frame(SBoxEdge[p,11,],SBoxEdge[p,11,]*.75+SBoxEdge[q,4,]*.25,SBoxEdge[p,11,]*.25+SBoxEdge[q,4,]*.75,SBoxEdge[q,4,])
  }
  
  ####4   past the gap
  if(abs((StatePos[p,1]-StatePos[q,1]))==0 & abs((StatePos[p,2]-StatePos[q,2]))>6){
    if(StatePos[p,1]< 7){
      if(StatePos[p,2]<StatePos[q,2]){
        tarrowpq<- data.frame(SBoxEdge[p,1,],SBoxEdge[p,1,]*0.75+SBoxEdge[q,12,]*.25,SBoxEdge[p,1,]*0.25+SBoxEdge[q,12,]*.75, SBoxEdge[q,12,])
      }else
        tarrowpq<- data.frame(SBoxEdge[p,11,],SBoxEdge[p,11,]*.75+SBoxEdge[q,2,]*.25,SBoxEdge[p,11,]*.25+SBoxEdge[q,2,]*.75,SBoxEdge[q,2,])
    }else
      if(StatePos[p,2]<StatePos[q,2]){
        tarrowpq<- data.frame(SBoxEdge[p,3,],SBoxEdge[p,3,]*.75+SBoxEdge[q,10,]*.25,SBoxEdge[p,3,]*.25+SBoxEdge[q,10,]*.75,SBoxEdge[q,10,])
      }else
        tarrowpq<- data.frame(SBoxEdge[p,9,],SBoxEdge[p,9,]*.75+SBoxEdge[q,4,]*.25,SBoxEdge[p,9,]*.25+SBoxEdge[q,4,]*.75,SBoxEdge[q,4,])  
  }
  ####5   diagonal, no overlap
  
  
  if((MargR[p]<MargL[q])&(MargT[p]<MargB[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,3,1],SBoxEdge[p,3,1]*1+SBoxEdge[q,16,1]*0,SBoxEdge[p,3,1]*0.25+SBoxEdge[q,16,1]*0.75,SBoxEdge[q,16,1]),
                     c(SBoxEdge[p,3,2],SBoxEdge[p,3,2]*0+SBoxEdge[q,16,2]*1,SBoxEdge[p,3,2]*0+SBoxEdge[q,16,2]*1,SBoxEdge[q,16,2]))
  }
  if((MargL[p]>MargR[q])&(MargT[p]<MargB[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,1,1],SBoxEdge[p,1,1]*1+SBoxEdge[q,6,1]*0,SBoxEdge[p,1,1]*0.25+SBoxEdge[q,6,1]*0.75,SBoxEdge[q,6,1]),
                     c(SBoxEdge[p,1,2],SBoxEdge[p,1,2]*0+SBoxEdge[q,6,2]*1,SBoxEdge[p,1,2]*0+SBoxEdge[q,6,2]*1,SBoxEdge[q,6,2]))
  }
  if((MargR[p]<MargL[q])&(MargB[p]>MargT[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,5,1],SBoxEdge[p,5,1]*0.75+SBoxEdge[q,2,1]*0.25,SBoxEdge[p,5,1]*0+SBoxEdge[q,2,1]*1,SBoxEdge[q,2,1]),
                     c(SBoxEdge[p,5,2],SBoxEdge[p,5,2]*1+SBoxEdge[q,4,2]*0,SBoxEdge[p,5,2]*1+SBoxEdge[q,4,2]*0,SBoxEdge[q,4,2]))
  }
  if((MargL[p]>MargR[q])&(MargB[p]>MargT[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,15,1],SBoxEdge[p,15,1]*0.75+SBoxEdge[q,4,1]*0.25,SBoxEdge[p,15,1]*0+SBoxEdge[q,4,1]*1,SBoxEdge[q,4,1]),
                     c(SBoxEdge[p,15,2],SBoxEdge[p,15,2]*1+SBoxEdge[q,4,2]*0,SBoxEdge[p,15,2]*1+SBoxEdge[q,4,2]*0,SBoxEdge[q,4,2]))
  }
  
  if((MargR[p]<MargL[q])&(MargT[p]<MargB[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,7,1],SBoxEdge[p,7,1]*.75+SBoxEdge[q,12,1]*.25,SBoxEdge[p,7,1]*0+SBoxEdge[q,12,1]*1,SBoxEdge[q,12,1]),
                     c(SBoxEdge[p,7,2],SBoxEdge[p,7,2]*1+SBoxEdge[q,12,2]*0,SBoxEdge[p,7,2]*1+SBoxEdge[q,12,2]*0,SBoxEdge[q,12,2]))
  }
  if((MargL[p]>MargR[q])&(MargT[p]<MargB[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,13,1],SBoxEdge[p,13,1]*.75+SBoxEdge[q,10,1]*.25,SBoxEdge[p,13,1]*0+SBoxEdge[q,10,1]*1,SBoxEdge[q,10,1]),
                     c(SBoxEdge[p,13,2],SBoxEdge[p,13,2]*1+SBoxEdge[q,10,2]*0,SBoxEdge[p,13,2]*1+SBoxEdge[q,10,2]*0,SBoxEdge[q,10,2]))
  }
  if((MargR[p]<MargL[q])&(MargB[p]>MargT[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,9,1],SBoxEdge[p,9,1]*1+SBoxEdge[q,14,1]*0,SBoxEdge[p,9,1]*0.25+SBoxEdge[q,14,1]*0.75,SBoxEdge[q,14,1]),
                     c(SBoxEdge[p,9,2],SBoxEdge[p,9,2]*0+SBoxEdge[q,14,2]*1,SBoxEdge[p,9,2]*0+SBoxEdge[q,14,2]*1,SBoxEdge[q,14,2]))
  }
  if((MargL[p]>MargR[q])&(MargB[p]>MargT[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,11,1],SBoxEdge[p,11,1]*1+SBoxEdge[q,8,1]*0,SBoxEdge[p,11,1]*0.25+SBoxEdge[q,8,1]*0.75,SBoxEdge[q,8,1]),
                     c(SBoxEdge[p,11,2],SBoxEdge[p,11,2]*0+SBoxEdge[q,8,2]*1,SBoxEdge[p,11,2]*0+SBoxEdge[q,8,2]*1,SBoxEdge[q,8,2]))
  }
  
  ####5   diagonal, R-L match
  
  
  if((MargR[p]==MargL[q])&(MargT[p]<MargB[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,2,1],SBoxEdge[p,2,1]*1+SBoxEdge[q,14,1]*0,SBoxEdge[p,2,1]*0.25+SBoxEdge[q,14,1]*0.75,SBoxEdge[q,14,1]),
                     c(SBoxEdge[p,2,2],SBoxEdge[p,2,2]*0+SBoxEdge[q,14,2]*1,SBoxEdge[p,2,2]*0+SBoxEdge[q,14,2]*1,SBoxEdge[q,14,2]))
  }
  if((MargL[p]==MargR[q])&(MargT[p]<MargB[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,2,1],SBoxEdge[p,2,1]*1+SBoxEdge[q,8,1]*0,SBoxEdge[p,2,1]*0.25+SBoxEdge[q,8,1]*0.75,SBoxEdge[q,8,1]),
                     c(SBoxEdge[p,2,2],SBoxEdge[p,2,2]*0+SBoxEdge[q,8,2]*1,SBoxEdge[p,2,2]*0+SBoxEdge[q,8,2]*1,SBoxEdge[q,8,2]))
  }
  if((MargR[p]==MargL[q])&(MargB[p]>MargT[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,7,1],SBoxEdge[p,7,1]*0.75+SBoxEdge[q,3,1]*0.25,SBoxEdge[p,7,1]*0+SBoxEdge[q,3,1]*1,SBoxEdge[q,3,1]),
                     c(SBoxEdge[p,7,2],SBoxEdge[p,7,2]*1+SBoxEdge[q,3,2]*0,SBoxEdge[p,7,2]*1+SBoxEdge[q,3,2]*0,SBoxEdge[q,3,2]))
  }
  if((MargL[p]==MargR[q])&(MargB[p]>MargT[q])&((StatePos[p,2]>11)|(StatePos[q,2]>11))){
    tarrowpq<- rbind(c(SBoxEdge[p,13,1],SBoxEdge[p,13,1]*0.75+SBoxEdge[q,3,1]*0.25,SBoxEdge[p,13,1]*0+SBoxEdge[q,3,1]*1,SBoxEdge[q,3,1]),
                     c(SBoxEdge[p,13,2],SBoxEdge[p,13,2]*1+SBoxEdge[q,3,2]*0,SBoxEdge[p,13,2]*1+SBoxEdge[q,3,2]*0,SBoxEdge[q,3,2]))
  }
  
  if((MargR[p]==MargL[q])&(MargT[p]<MargB[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,5,1],SBoxEdge[p,5,1]*.75+SBoxEdge[q,11,1]*.25,SBoxEdge[p,5,1]*0+SBoxEdge[q,11,1]*1,SBoxEdge[q,11,1]),
                     c(SBoxEdge[p,5,2],SBoxEdge[p,5,2]*1+SBoxEdge[q,11,2]*0,SBoxEdge[p,5,2]*1+SBoxEdge[q,11,2]*0,SBoxEdge[q,11,2]))
  }
  if((MargL[p]==MargR[q])&(MargT[p]<MargB[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,15,1],SBoxEdge[p,15,1]*.75+SBoxEdge[q,11,1]*.25,SBoxEdge[p,15,1]*0+SBoxEdge[q,11,1]*1,SBoxEdge[q,11,1]),
                     c(SBoxEdge[p,15,2],SBoxEdge[p,15,2]*1+SBoxEdge[q,11,2]*0,SBoxEdge[p,15,2]*1+SBoxEdge[q,11,2]*0,SBoxEdge[q,11,2]))
  }
  if((MargR[p]==MargL[q])&(MargB[p]>MargT[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,10,1],SBoxEdge[p,10,1]*1+SBoxEdge[q,16,1]*0,SBoxEdge[p,10,1]*0.25+SBoxEdge[q,16,1]*0.75,SBoxEdge[q,16,1]),
                     c(SBoxEdge[p,10,2],SBoxEdge[p,10,2]*0+SBoxEdge[q,16,2]*1,SBoxEdge[p,10,2]*0+SBoxEdge[q,16,2]*1,SBoxEdge[q,16,2]))
  }
  if((MargL[p]==MargR[q])&(MargB[p]>MargT[q])&((StatePos[p,2]<9)|(StatePos[q,2]<9))){
    tarrowpq<- rbind(c(SBoxEdge[p,10,1],SBoxEdge[p,10,1]*1+SBoxEdge[q,6,1]*0,SBoxEdge[p,10,1]*0.25+SBoxEdge[q,6,1]*0.75,SBoxEdge[q,6,1]),
                     c(SBoxEdge[p,10,2],SBoxEdge[p,10,2]*0+SBoxEdge[q,6,2]*1,SBoxEdge[p,10,2]*0+SBoxEdge[q,6,2]*1,SBoxEdge[q,6,2]))
  }
  
  
  
  ####6   distant
  if((min(abs(MargR[p]-MargL[q]),abs(MargL[p]-MargR[q])))>=15){
    if(StatePos[p,1]<StatePos[q,1]){
      tarrowpq<- rbind(c(SBoxEdge[p,2,1],SBoxEdge[p,2,1]*1+SBoxEdge[q,3,1]*0,SBoxEdge[p,2,1]*0+SBoxEdge[q,3,1]*1,SBoxEdge[q,3,1]),
                       c(SBoxEdge[p,2,2],19,19,SBoxEdge[q,3,2]))
    }else
      tarrowpq<- rbind(c(SBoxEdge[p,10,1],SBoxEdge[p,10,1]*1+SBoxEdge[q,11,1]*0,SBoxEdge[p,10,1]*0+SBoxEdge[q,11,1]*1,SBoxEdge[q,11,1]),
                       c(SBoxEdge[p,10,2],1,1,SBoxEdge[q,11,2]))
  }
  
  return(tarrowpq)
}


#assign Transition to array using function, 1st: from, 2nd: to, 3rd: x or y, 4th: from or to -------------
TransMatrix<-array(c(0,0,0,0),dim=c(trowtotal,trowtotal,2,4))
for (p in 1:trowtotal){
  for (q in 1:trowtotal){
    TransMatrix[p,q,1,1]<-assigntrans(p,q)[1,1]
    TransMatrix[p,q,1,2]<-assigntrans(p,q)[1,2]
    TransMatrix[p,q,1,3]<-assigntrans(p,q)[1,3]
    TransMatrix[p,q,1,4]<-assigntrans(p,q)[1,4]
    TransMatrix[p,q,2,1]<-assigntrans(p,q)[2,1]
    TransMatrix[p,q,2,2]<-assigntrans(p,q)[2,2]
    TransMatrix[p,q,2,3]<-assigntrans(p,q)[2,3]
    TransMatrix[p,q,2,4]<-assigntrans(p,q)[2,4]
  }
}
#label text to trans array; third dimension is x versus y --------
TTexTMatrix<-array(c(0,0,0,0),dim=c(7,7,2))
for (p in 1:trowtotal){
  for (q in 1:trowtotal){
    TTexTMatrix[p,q,1]<-(TransMatrix[p,q,1,2]-TransMatrix[p,q,1,1])/6+TransMatrix[p,q,1,1]
    TTexTMatrix[p,q,2]<-(TransMatrix[p,q,2,2]-TransMatrix[p,q,2,1])/6+TransMatrix[p,q,2,1]
  }
}

thisTrans<-thissiteTrans[thissiteTrans$ES %in% currentsite,3:9]


#draw trans arrow loop----------
for (p in 1:trowtotal){
  
  for (q in 1:trowtotal){
    
    if (!is.na(thisTrans[p,q])&thisTrans[p,q]!=""){
      grid.lines(x=unit(TransMatrix[p,q,1,]/grdfact,"snpc"),y=unit(TransMatrix[p,q,2,]/grdfact,"snpc"), gp = gpar(col="black", fill="black"),arrow = arrow(angle = 20, length = unit(0.02*16/grdfact, "npc"), ends = "last", type = "closed") )
      
      grid.text(thisTrans[p,q] ,x = unit(TTexTMatrix[p,q,1]/grdfact,'snpc'), y=unit(TTexTMatrix[p,q,2]/grdfact,'snpc'), just=c("center","center"), gp=gpar(fontsize=14*dvr*vpscale*16/grdfact, col="black"))
    }
  }
}





for (s in 1:nrow(StateBoxes)){

currentstate<-s
thisState<-thisSiteStates[thisSiteStates$ES %in% currentsite & thisSiteStates$State %in% currentstate,]
thisPath<-thissitePaths[thissitePaths$ES %in% currentsite & thissitePaths$State %in% currentstate,4:10]
thisBox<-thisSiteBoxes[thisSiteBoxes$ES %in% currentsite & thisSiteBoxes$State %in% currentstate,3:7]


thisBox$labelA<-"placeholder"
thisBox$labelB<-"placeholder"
rowtotal<-nrow(thisPath)
row<-1
for (row in 1:rowtotal){
thisBox[row,]$labelA<-strtrim(paste(strwrap(paste(thisBox[row,]$Phase,": ", thisBox[row,]$Phase.Name, collapse=""),30), collapse = "\n"),35)
thisBox[row,]$labelB<-paste(strwrap(thisBox[row,]$Association,35), collapse = "\n")
}


max.x<-max(thisBox[which((!is.na(thisBox$Phase))&thisBox$Phase!=""),]$x)
min.x<-min(thisBox[which((!is.na(thisBox$Phase))&thisBox$Phase!=""),]$x)
max.y<-max(thisBox[which((!is.na(thisBox$Phase))&thisBox$Phase!=""),]$y)
min.y<-min(thisBox[which((!is.na(thisBox$Phase))&thisBox$Phase!=""),]$y)

# Define vertices of phase boxes in cm -------------------------------------------------
BoxSize<-c(4,2)
StateBoxSize<-StateBox[s,]
#StatePos<-c(StateBoxSize[1]/2,StateBoxSize[2]/2)
#StateText<-paste("State ",thisState$State,": ",thisState$State.Name,sep = "")
#subtract for excluded phases------------------------------------------
movebox<-((StatePos[s,]-StateBoxSize/2)+c(0,0))/grdfact
thisBox$x<- thisBox$x-min.x+3
thisBox$y<- thisBox$y-min.y+2

#convert to relative grid-----------------------------------------------
#grdfact<-16
#BoxSize<-BoxSize/grdfact
#StateBoxSize<-StateBoxSize/grdfact
#StatePos<-StatePos/grdfact
#thisBox$x<-thisBox$x/grdfact
#thisBox$y<-thisBox$y/grdfact

#vector positions on box------------------------------------------------------
p1<-c(-0.375,0.5)
p2<-c(-0.125,0.5)
p3<-c(0.125,0.5)
p4<-c(0.375,0.5)
p5<-c(0.5,0.375)
p6<-c(0.5,0.125)
p7<-c(0.5,-0.125)
p8<-c(0.5,-0.375)
p9<-c(0.375,-0.5)
p10<-c(0.125,-0.5)
p11<-c(-0.125,-0.5)
p12<-c(-0.375,-0.5)
p13<-c(-0.5,-0.375)
p14<-c(-0.5,-0.125)
p15<-c(-0.5,0.125)
p16<-c(-0.5,0.375)

#positions on box converted to relative grid then create array representing all box edges ------------------------------------------------------------
p1<-p1*BoxSize
p2<-p2*BoxSize
p3<-p3*BoxSize
p4<-p4*BoxSize
p5<-p5*BoxSize
p6<-p6*BoxSize
p7<-p7*BoxSize
p8<-p8*BoxSize
p9<-p9*BoxSize
p10<-p10*BoxSize
p11<-p11*BoxSize
p12<-p12*BoxSize
p13<-p13*BoxSize
p14<-p14*BoxSize
p15<-p15*BoxSize
p16<-p16*BoxSize

#put points on box------------------------
EdgelistX<-c(p1[1], p2[1], p3[1], p4[1], p5[1], p6[1], p7[1], p8[1], p9[1], p10[1], p11[1], p12[1], p13[1], p14[1], p15[1], p16[1])

EdgelistY<-c(p1[2], p2[2], p3[2], p4[2], p5[2], p6[2], p7[2], p8[2], p9[2], p10[2], p11[2], p12[2], p13[2], p14[2], p15[2], p16[2])
Edgelist<-data.frame(EdgelistX,EdgelistY)

Boxes<-thisBox[,c("x","y")]

BoxEdge<-array(c(1:rowtotal,1:16,1:2),dim=c(7,16,2))
for (i in 1:rowtotal){
  for (j in 1:16){
    for (k in 1:2){
      BoxEdge[i,j,k] <- Boxes[i,k]+Edgelist[j,k]
}}}



#assign pathway to array using function, 1st: from, 2nd: to, 3rd: x or y, 4th: from or to -------------
PhaseMatrix<-array(c(0,0,0,0),dim=c(rowtotal,rowtotal,2,2))
for (p in 1:rowtotal){
  for (q in 1:rowtotal){
    PhaseMatrix[p,q,1,1]<-assignpath(p,q)[1,1]
    PhaseMatrix[p,q,1,2]<-assignpath(p,q)[1,2]
    PhaseMatrix[p,q,2,1]<-assignpath(p,q)[2,1]
    PhaseMatrix[p,q,2,2]<-assignpath(p,q)[2,2]
  }
}

#label text to pathway array; third dimension is x versus y --------
PTexPMatrix<-array(c(0,0,0,0),dim=c(7,7,2))
for (p in 1:rowtotal){
  for (q in 1:rowtotal){
    PTexPMatrix[p,q,1]<-(PhaseMatrix[p,q,1,2]-PhaseMatrix[p,q,1,1])/6+PhaseMatrix[p,q,1,1]
    PTexPMatrix[p,q,2]<-(PhaseMatrix[p,q,2,2]-PhaseMatrix[p,q,2,1])/6+PhaseMatrix[p,q,2,1]
  }
}






#Make Viewports and rectangles--------------------------------------------------------------------------





#phase rectangle loop-------------------------------
for (i in 1:rowtotal){
  if (!is.na(thisBox[i,"Phase"])&thisBox[i,"Phase"]!=""){
    pushViewport(viewport(x = unit(Boxes[i,"x"]/grdfact+movebox[1],"snpc"), y = unit(Boxes[i,"y"]/grdfact+movebox[2],"snpc"), width = unit(BoxSize[1]/grdfact,"snpc"), height = unit(BoxSize[2]/grdfact,"snpc")))
    
    grid.rect()
    grid.text(thisBox[i,]$labelA ,x = 0.05, y = 0.9, just=c("left","top"), gp=gpar(fontsize=12*dvr*vpscale*16/grdfact, col="black",fontface="bold"))
    
    grid.text(thisBox[i,]$labelB ,x = 0.1, y = 0.60, just=c("left","top"), gp=gpar(fontsize=10*dvr*vpscale*16/grdfact, col="black", fontface="italic"))
    upViewport()
  }}


#draw arrow loop----------
for (p in 1:rowtotal){
  
  for (q in 1:rowtotal){
    
    if (!is.na(thisPath[p,q])&thisPath[p,q]!=""){
      grid.lines(x=unit(PhaseMatrix[p,q,1,]/grdfact+movebox[1,1],"snpc"),y=unit(PhaseMatrix[p,q,2,]/grdfact+movebox[1,2],"snpc"), gp = gpar(col="black", fill="black"),arrow = arrow(angle = 20, length = unit(0.02*16/grdfact, "npc"), ends = "last", type = "closed") )
      
      grid.text(thisPath[p,q] ,x = unit(PTexPMatrix[p,q,1]/grdfact+movebox[1,1],'snpc'), y=unit(PTexPMatrix[p,q,2]/grdfact+movebox[1,2],'snpc'), just=c("center","center"), gp=gpar(fontsize=14*dvr*vpscale*16/grdfact, col="black"))
    }
  }
}



}
#dev.off()