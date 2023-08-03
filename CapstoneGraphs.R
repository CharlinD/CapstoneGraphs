RunPlots <- function(n){
  #~~~~~~~~~~~~Data~~~~~~~~~~~~~~~~~~~~~~
  
  #Laptop Serial
  ls <- c(0.9932016,6.013698,21.74718,87.34786)
  #Laptop Parallel
  lp <- c(1.153678,2.401184,5.788374,21.49608)
  #Supercomputer Serial
  scs <- c(1.895688,12.92272,47.88498,193.1986)
  #Supercomputer GPU-only Parallel
  scg <- c(0.7874006,1.698462,4.576686,16.52704)
  #Supercomputer CPU & GPU Parallel
  scgc <- c(2.512148,2.815504,3.741916,7.71386)
  #Supercomputer 2 nodes CPU & GPU Parallel
  sc2 <-c(1.635658,1.952784,2.909022,6.679514)
  
  #Pixels
  p <- c(1000,2600,5000,10000)
  
  #1000x1000 Runtime Data on all
  t1 <- c(0.9932016,1.153678,1.895688,0.7874006,2.512148,1.635658)
  t1l <- c(1,2,3,4,5,6)
  t1n <- c("LS","LP","SCS","SCG","SCGC","SC2")
  
  #~~~~~~~~~~~~~~Plots~~~~~~~~~~~~~~~~~~~~~
  
  #Plotting all on Time v Pixels
  
  if (n == 1){
    plot(scs ~ p, type="o",col ="blue",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
       main = "Pixels vs Time (Wall Time in Seconds)", axes = FALSE)
    points(x = p, y = scs, col="blue",pch = 20, cex = 1.5)
    lines(x = p, y = lp,type = "o",col ="deeppink",lwd = 3)
    points(x = p, y = lp,col="deeppink", pch = 20, cex = 1.5)
    
    lines(x = p, y = ls,type = "o",col ="purple", lwd = 3)
    points(x = p, y = ls,col="purple", pch = 20, cex = 1.5)
    
    lines(x = p, y = scg,type = "o",col ="green2", lwd = 3)
    points(x = p, y = scg,col="green2", pch = 20, cex = 1.5)
    lines(x = p, y = scgc,type = "o",col ="orange2", lwd = 3)
    points(x = p, y = scgc,col="orange2", pch = 20, cex = 1.5)
    lines(x = p, y = sc2, type = "o", col = "red3",lwd =3)
    points(x = p, y = sc2, col="red3",pch = 20, cex=1.5)
 
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0.5, to = 194,by = 10))
  
    legend(1000,190, legend=c("Supercomputer Serial","Laptop Serial","Laptop GPU Parallel",
                        "Supercomputer GPU Parallel","Supercomputer CPU & GPU Parallel",
                        "SC CPU & GPU Parallel on 2 Nodes"),
         col=c("blue","purple","deeppink","green2","orange2","red3"),lty=1,lwd=3,cex=0.68)
  
     box()
  }
  
  #Plotting sc serial for each size
  if(n == 2){
    
    plot(scs ~ p, type="o",col ="blue",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Supercomputer Running in Serial", axes = FALSE)
    points(x = p, y = scs, col="blue",pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 195,by = 15))
    
    text(p[1]+100,scs[1]+10,labels = 1.896)
    text(p[2]-50,scs[2]+10,labels = 12.923)
    text(p[3],scs[3]-10,labels = 47.885)
    text(p[4]-700,scs[4],labels = 193.199)
    
    box()
  }
  
  #Plotting laptop serial for each size
  if(n == 3){
    
    plot(ls ~ p, type="o",col ="purple",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Laptop Running in Serial", axes = FALSE)
    points(y = ls, x = p, col="purple",pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 90,by = 5))
    
    text(p[1]+100,ls[1]+5,labels = 0.993)
    text(p[2]-50,ls[2]+5,labels = 6.014)
    text(p[3],ls[3]-5,labels = 21.747)
    text(p[4]-700,ls[4],labels = 87.348)
    
    box()
  }
  
  #Plotting laptop parallel for each size
  if(n == 4){
    
    plot(lp ~ p, type="o",col ="deeppink",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Laptop Running in Parallel on GPU", axes = FALSE)
    points(y = lp, x = p, col="deeppink",pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 24,by = 2))
    
    text(p[1]+100,lp[1]+1.2,labels = 1.154)
    text(p[2]-50,lp[2]+1.2,labels = 2.401)
    text(p[3],lp[3]-1.2,labels = 5.788)
    text(p[4]-700,lp[4],labels = 21.496)
    
    box()
  }
  
  #Plotting SC GPU only parallel
  if(n == 5){
    
    plot(scg ~ p, type="o",col ="green2",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Supercomputer Running in Parallel on GPU", axes = FALSE)
    points(y = scg, x = p, col="green2",pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 18,by = 1.5))
    
    text(p[1]+100,scg[1]+0.9,labels = 0.787)
    text(p[2]-50,scg[2]+0.9,labels = 1.698)
    text(p[3],scg[3]-0.9,labels = 4.577)
    text(p[4]-650,scg[4],labels = 16.527)
    
    box()
  }
  
  #Plotting SC CPU & GPU parallel
  if(n == 6){
    
    plot(scgc ~ p, type="o",col ="orange2",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Supercomputer Running in Parallel on CPU & GPU", axes = FALSE)
    points(y = scgc, x = p, col="orange2",pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 8,by = 0.5))
    
    text(p[1]+100,scgc[1]+0.3,labels = 2.512)
    text(p[2]-50,scgc[2]+0.3,labels = 2.816)
    text(p[3],scgc[3]-0.3,labels = 3.742)
    text(p[4]-650,scgc[4],labels = 7.714)
    
    box()
  }
  
  #Plotting SC 2 nodes CPU & GPU parallel
  if(n == 7){
    
    plot(sc2 ~ p, type="o",col ="red3",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Supercomputer Running 2 Nodes in Parallel on CPU & GPU", axes = FALSE)
    points(y = sc2, x = p, col="red3",pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 8,by = 0.5))
    
    text(p[1]+100,sc2[1]+0.3,labels = 1.636)
    text(p[2]-50,sc2[2]+0.3,labels = 1.953)
    text(p[3],sc2[3]-0.3,labels = 2.909)
    text(p[4]-650,sc2[4],labels = 6.679)
    
    box()
  }
  
  #Plotting SC serial vs max parallel
  if(n == 7){
    
    plot(scs ~ p, type="o",col ="blue",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Serial vs Max Parallelism on the Supercomputer", axes = FALSE)
    points(y = scs, x = p, col="blue",pch = 20, cex = 1.5)
  
    lines(x = p, y = sc2 ,type = "o",col ="red3", lwd = 3)
    points(x = p, y = sc2 ,col="red3", pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 200,by = 10))
    
    text(p[4]-700,scs[4],labels = 193.199)
    text(p[4],sc2[4]+10,labels = 6.679)
    
    box()
  }
  
  #Plotting All 1000x1000 runtimes
  if(n == 8){
    
    plot(t1l, t1, main="Runtime Data for 1000x1000 Pixel Image",
         xlab="Run Type", ylab="Time (s)", pch=20, axes = FALSE)
    
    points(y = t1[1], x = t1l[1], col="purple",pch = 20, cex = 1.8)
    
    points(y = t1[2], x = t1l[2], col="deeppink",pch = 20, cex = 1.8)
    
    points(y = t1[3], x = t1l[3], col="blue",pch = 20, cex = 1.8)
    
    points(y = t1[4], x = t1l[4], col="green2",pch = 20, cex = 1.8)
    
    points(y = t1[5], x = t1l[5], col="orange2",pch = 20, cex = 1.8)
    
    points(y = t1[6], x = t1l[6], col="red3",pch = 20, cex = 1.8)
    
    axis(side = 1, at = t1l,labels = t1n)
    axis(side = 2, at = seq(from = 0, to = 3,by = 0.1))
    
    text(t1l[1]+0.05,t1[1]+0.1,labels = 0.993)
    text(t1l[2],t1[2]+0.1,labels = 1.154)
    text(t1l[3],t1[3]+0.1,labels = 1.896)
    text(t1l[4],t1[4]+0.1,labels = 0.787)
    text(t1l[5],t1[5]-0.1,labels = 2.512)
    text(t1l[6]-0.05,t1[6]+0.1,labels = 1.636)
    
    box()
    
  }
  
  #Plotting Laptop serial vs GPU parallel
  if(n == 9){
    
    plot(ls ~ p, type="o",col ="purple",lwd = 3,xlab = "Pixels", ylab = "Time (s)",
         main = "Serial vs GPU Parallel on Charlin's Laptop", axes = FALSE)
    points(y = ls, x = p, col="purple",pch = 20, cex = 1.5)
    
    lines(x = p, y = lp ,type = "o",col ="deeppink", lwd = 3)
    points(x = p, y = lp ,col="deeppink", pch = 20, cex = 1.5)
    
    axis(side = 1, at = c(1000,2600,5000,10000),labels = c("1000x1000","2600x2600","5000x5000","10000x10000"))
    axis(side = 2, at = seq(from = 0, to = 100,by = 5))
    
    text(p[4]-700,ls[4],labels = 87.348)
    text(p[4]-200,lp[4]-5,labels = 21.496)
    
    box()
  }
  
  
  
  }
