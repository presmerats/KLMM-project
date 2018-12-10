plot.and.save.results <- function( xvalidation, yvalidation,yvalidation.pred, modelstring, total.time, rmse.validation){
  
  # output
  rmse = round(rmse.validation,3)
  # parse time correctly 
  timetext = format(total.time, format ="%Y-%m-%d %H:%M:%S")
  timetext = substr(timetext,1,nchar(timetext))
  i1 = regexpr('\\.',timetext)
  i2 = regexpr('\ ',timetext)
  timenum = round(as.numeric(substr(timetext,1,i2-1)),2)
  timetext = substr(timetext,i2+1,nchar(timetext))
  final.time.text = paste(timenum,timetext,sep="",collapse="")
  
  model.result <- paste(modelstring,final.time.text,"rmse",rmse,sep="_",collapse="")
  
  print(model.result)
  write(model.result, 
        file="results/results.txt",append=TRUE)
  # plot predict vs ground-truth
  print("preparing to write plot to disk")
  par(mfrow=c(1,1))
  ylimmax = max(max(yvalidation),max(yvalidation.pred))
  ylimmin = min(min(yvalidation),min(yvalidation.pred))
  
  plotname = paste(c('./plots/',model.result,'.jpeg'),sep="",collapse="")
  plotname = gsub(":","_",plotname)
  plotname = gsub(" ","_",plotname)
  plotname = gsub("=","_",plotname)
  plotname = gsub(",","-",plotname)
  jpeg(plotname, width = 480, height = 480, units = "px")
  plot(
    yvalidation,type="l", 
    main=paste("Training results",sep="",collapse=""),
    xlab=paste(model.result,sep="",collapse=""),
    ylim=c(ylimmin,ylimmax))
  lines(yvalidation.pred, col="red")
  dev.off()
  
  print("plot written to disk!")
  
  plot(
    yvalidation,type="l", 
    main=paste("Training results",sep="",collapse=""),
    xlab=paste(model.result,sep="",collapse=""),
    ylim=c(ylimmin,ylimmax))
  lines(yvalidation.pred, col="red")
}


plot.and.save.test.result <- function(xtest, ytest,ytest.pred,modelfilename,datafilename,rmse.plot){
  
  par(mfrow=c(1,1))
  ylimmax = max(max(ytest),max(ytest.pred))
  ylimmin = min(min(ytest),min(ytest.pred))
  # plot to disk
  print("modelfilename")
  print(modelfilename)
  print("datafilename")
  print(datafilename)
  plotname= paste("./plots/","test",modelfilename,paste("RMSE",rmse.plot,sep="_"), sep="_" )
  
  
  jpeg(plotname, width = 768, height = 480, units = "px")
  plot(
    ytest,type="l", 
    main=paste("Test result",sep="",collapse=""),
    xlab=paste(plotname,sep="",collapse=""),
    ylab="BER",
    ylim=c(ylimmin,ylimmax))
  lines(ytest.pred, col="red")
  legend(1, ylimmax-2, legend=c("Real Error", "Predicted Error"),
         col=c("black", "red"), lty=1:1, cex=0.8)
  dev.off()
  
  # plot in r studio
  plot(
    ytest,type="l", 
    main=paste("Test result",sep="",collapse=""),
    xlab=paste(plotname,sep="",collapse=""),
    ylab="BER",
    ylim=c(ylimmin,ylimmax))
  lines(ytest.pred, col="red")
  legend(1, ylimmax-2, legend=c("Real Error", "Predicted Error"),
         col=c("black", "red"), lty=1:1, cex=0.8)
  # save to file
  test.result <- plotname
  write(test.result, 
        file="results/results_tests.txt",append=TRUE)
  
  
}



sop.plot.prepare <- function(xtest,ytest, ytest.pred, raw=FALSE){
  # for first row (or first row of a group)
  #  read x1_1...x1_n ,x2_1...x2_n , x3_1...x3_n , error 1 .. error n
  #  sliding window x1_i, x2_j, x3_k, error_l
  #  create new almost empty with each of those 4 values and the rest empty using rbind()
  
  groups <- unique(xtest$group)
  xtest.backup <- xtest
  xtest.result <- xtest[1,]
  current.group <- -1
  last.group.index <- -1
  for (i in 1:nrow(xtest.backup)){
    
    group <- xtest.backup$group[i]
    if (current.group != group){
      print(paste("new group", group))
      current.group <- group
      
      #  add previous group values to result dataframe
      if (last.group.index>0){
        print(paste("adding",i-last.group.index+1 ))
        xtest.result <- rbind(xtest.result,xtest[last.group.index:i,])
        
        # smooothed or raw error version
        if (raw==TRUE) ytest.result <- c(ytest.result,xtest$y_0[last.group.index:i])
        else ytest.result <- c(ytest.result,ytest[last.group.index:i])
        
        ytest.pred.result <- c(ytest.pred.result,ytest.pred[last.group.index:i])
        
      } 
      last.group.index <- i
      
      # get indices
      index.sop1 = which(names(xtest)=="x1_0")
      index.sop2 = which(names(xtest)=="x2_0")
      index.sop3 = which(names(xtest)=="x3_0")
      w = index.sop2 - index.sop1 -1
      index.error = index.sop3 + w
      index.last.error = index.error + w 
      index.smoothed.error = index.last.error + 1
      index.last.smoothed.error = index.smoothed.error + w
      
      # extract previous windows
      x1.w = rev(as.numeric(xtest[i,(index.sop1+1):(index.sop2-1)]))
      x2.w = rev(as.numeric(xtest[i,(index.sop2+1):(index.sop3-1)]))
      x3.w = rev(as.numeric(xtest[i,(index.sop3+1):(index.error)]))
      y.w = rev(as.numeric(xtest[i,(index.error+1):index.last.error]))
      ys.w = rev(as.numeric(xtest[i,(index.smoothed.error+1):index.last.smoothed.error]))
      
      
      #print(ys.w)
      
      # print("sop.plot.prepare verification")
      # print(w)
      # print(length(x1.w))
      # print(length(x2.w))
      # print(length(x3.w))
      # print(length(y.w))
      # 
      # create this data frame fragment
      newdf1 <- data.frame(x1_0=x1.w)
      for (j in 1:w) newdf1 <- cbind(newdf1,rep(0,w) )
      thenames1 = paste(rep("x1_",w+1),0:w,sep="")
      colnames(newdf1) <- thenames1
      
      newdf2 <- data.frame(x1_0=x2.w)
      for (j in 1:w) newdf2 <- cbind(newdf2,rep(0,w) )
      thenames2 = paste(rep("x2_",w+1),0:w,sep="")
      colnames(newdf2) <- thenames2
      
      newdf3 <- data.frame(x1_0=x3.w)
      for (j in 1:w) newdf3 <- cbind(newdf3,rep(0,w) )
      thenames3 = paste(rep("x3_",w+1),0:w,sep="")
      colnames(newdf3) <- thenames3
      
      newdf4 <- data.frame(x1_0=y.w)
      for (j in 1:w) newdf4 <- cbind(newdf4,rep(0,w) )
      thenames4 = paste(rep("y_",w+1),0:w,sep="")
      colnames(newdf4) <- thenames4
      
      newdf5 <- data.frame(x1_0=ys.w)
      for (j in 1:w) newdf5 <- cbind(newdf5,rep(0,w) )
      thenames5 = paste(rep("ys_",w+1),0:w,sep="")
      colnames(newdf5) <- thenames5
      
      newdf <- cbind(newdf1,newdf2,newdf3,newdf4,newdf5, xtest$group[1:(w)])
      names2 <- colnames(newdf)
      names2[length(names2)] <- "group"
      colnames(newdf) <- names2
      
      # now put this result into a result dataframe
      if (i>1){
        
        xtest.result <- rbind(xtest.result, newdf,xtest[i,])
        # raw error version
        # smoothed error version
        if (raw==TRUE) ytest.result <- c(ytest.result,y.w,xtest$y_0[i])
        else ytest.result <- c(ytest.result,ys.w,ytest[i])
        print("length y.w")
        print(length(y.w))
        
        ytest.pred.result <- c(ytest.pred.result,rep(0,w-1),ytest.pred[i])
        
      } else {
        
        xtest.result <- rbind(newdf,xtest[i,])
        
        # smoothed error version
        # raw error version
        if (raw==TRUE)  ytest.result <- c(y.w,xtest$y_0[i])
        else ytest.result <- c(ys.w,ytest[i])
        
        #ytest.result <- c(y.w,xtest$y_0[i])
        ytest.pred.result <- c(rep(0,w-1),ytest.pred[i])
      }
      
      print(paste("new dim xtest",dim(xtest.result)[1],dim(xtest.result)[2]," new dim ytest",length(ytest.result)))
      
      # if (i<3000){
      #   par(mfrow=c(3,2))
      #   plot(xtest.result[,1])
      #   plot(xtest.result[,1002])
      #   plot(xtest.result[,2003])
      #   plot(xtest.result[,3004])
      #   plot(ytest.result)
      #   plot(ytest.pred.result)
      # } 
      
    }
  }
  
  print(paste("addind",i-last.group.index+1 ))
  xtest.result <- rbind(xtest.result,xtest[last.group.index:i,])
  # smoothed error version
  
  # raw error version
  if (raw==TRUE)   ytest.result <- c(ytest.result,xtest$y_0[last.group.index:i])
  else ytest.result <- c(ytest.result,ytest[last.group.index:i])
  
  ytest.pred.result <- c(ytest.pred.result,ytest.pred[last.group.index:i])
  
  print(paste("new dim xtest",dim(xtest.result)[1],dim(xtest.result)[2]," new dim ytest",length(ytest.result)))
  
  
  xtest <- xtest.result
  ytest <- ytest.result
  ytest.pred <- ytest.pred.result
  
  
  
  return(list(xtest, ytest,ytest.pred))
}

sop.plot <- function(xtest, ytest,ytest.pred,modelfilename,datafilename,rmse.plot, raw=FALSE){
  # plot the SOP variables
  #   select SOP variables by name
  #   include initial w measures of each var and error WITHOUT prediction
  #   separate initial w and lack of prediction for each group
  #   include last d prediction measure WITHOUT sop vars and error
  #   use raw error and predicted error 
  
  # select SOP vars by name
  index.sop1 = which(names(xtest)=="x1_0")
  index.sop2 = which(names(xtest)=="x2_0")
  index.sop3 =which(names(xtest)=="x3_0")
  
  # separate by group
  group0 = unique(xtest$group)[1]
  subselect <- xtest$group == group0
  # xtest <- xtest[subselect,]
  # ytest <- ytest[subselect]
  # ytest.pred <- ytest.pred[subselect]
  
  # prepare initial w window
  print("sop.plot verification")
  # print(w)
  print(nrow(xtest))
  print(length(ytest))
  print(length(ytest.pred))
  print(summary(xtest[,index.sop1]))
  print(summary(xtest[,index.sop2]))
  print(summary(xtest[,index.sop3]))
  
  #print(head(xtest[,2000:2500]))
  
  result <- sop.plot.prepare(xtest,ytest,ytest.pred, raw)
  xtest <- result[[1]]; ytest <- result[[2]]; ytest.pred <- result[[3]]
  
  print("sop.plot verification1")
  # print(w)
  print(nrow(xtest))
  print(length(ytest))
  print(length(ytest.pred))
  print(summary(xtest[,index.sop1]))
  print(summary(xtest[,index.sop2]))
  print(summary(xtest[,index.sop3]))
  
  # plot
  par(mfrow=c(1,1))
  ylimmax = max(max(ytest),max(ytest.pred))
  ytest.pred.margins <- ytest.pred
  ytest.pred.margins[ytest.pred.margins==0] <- ytest.pred.margins[length(ytest.pred.margins)]
  ylimmin = min(min(ytest),min(ytest.pred.margins))
  # plot to disk
  plotname= paste("./plots/","test_sop_",modelfilename,paste("RMSE",rmse.plot,sep="_"), sep="_" )
  
  modelfilename.multiline = c(substr(modelfilename,1,nchar(modelfilename)/2),substr(modelfilename,nchar(modelfilename)/2,nchar(modelfilename)))
  plotname.multiline = paste("./plots/","test_sop_",modelfilename.multiline,paste("RMSE",rmse.plot,sep="_"), sep="_" )
  jpeg(plotname, width = 768, height = 768, units = "px")
  par(mfrow=c(2,1))
  plot(
    xtest[,index.sop1],type="l", col="pink",
    main=paste("Test result",sep="",collapse=""),
    xlab="ms",
    ylab="SOP"
    #, xlim=c(450,500)
  )
  lines(xtest[,index.sop2], col="yellow")
  lines(xtest[,index.sop3], col="cyan")
  plot(
    ytest,type="l", 
    main=paste("Test result",sep="",collapse=""),
    xlab=paste(plotname.multiline,sep=""),
    ylab="BER",
    ylim=c(ylimmin,ylimmax),
    xlim=c(1,nrow(xtest))
  )
  lines(ytest.pred, col="red")
  w = index.sop2- index.sop2 +1
  legend(length(ytest.pred)-10*w, ylimmin+2, legend=c("Real Error", "Predicted Error"),
         col=c("black", "red"), lty=1:1, cex=0.8)
  dev.off()
  
  par(mfrow=c(2,1))
  plot(
    xtest[,index.sop1],type="l", col="pink",
    main=paste("Test result",sep="",collapse=""),
    xlab="ms",
    ylab="SOP"
    #, xlim=c(450,500)
  )
  lines(xtest[,index.sop2], col="yellow")
  lines(xtest[,index.sop3], col="cyan")
  plot(
    ytest,type="l", 
    main=paste("Test result",sep="",collapse=""),
    xlab=paste(plotname.multiline,sep=""),
    ylab="BER",
    ylim=c(ylimmin,ylimmax),
    xlim=c(1,nrow(xtest))
  )
  lines(ytest.pred, col="red")
  w = index.sop2- index.sop2 +1
  legend(length(ytest.pred)-10*w, ylimmin+2, legend=c("Real Error", "Predicted Error"),
         col=c("black", "red"), lty=1:1, cex=0.8)
  
  
}
