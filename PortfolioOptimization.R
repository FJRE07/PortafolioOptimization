library(PortfolioAnalytics)
library(quantmod)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

acciones<-c("AMZN","AAPL","NFLX","FB")
fecini<-"2016-01-02"
perio<-"daily"
TasaLibre <-0 #Tasa Libre de Riesgo para Sharpe Ratio
alfa<-.95 #Nivel de confianza
np <- 25 #Numero portafolios
ventacorta<-1 #1 se permite la venta en corto y 0 no se permite

optimizar <-function(acciones,fecini,perio,TasaLibre,alfa,np,ventacorta){
  options( warn = -1 )
 
  precios <- NULL
  for (acc in acciones) {
    precios<-cbind(precios,getSymbols(acc,src ="yahoo", from =fecini, periodicity = perio,auto.assign = FALSE)[,c(4)])
  }
  options(scipen =999)
  ret <-na.omit(ROC(precios)) 
  precios<-na.omit(precios)
  colnames(ret)<-acciones
  shar.indi<-SharpeRatio(R=ret,Rf=TasaLibre,FUN="StdDev",p=alfa)
        con<-"Con Venta En Corto"
        init.portfolio <-portfolio.spec(assets = acciones)
        if (ventacorta==0){
          init.portfolio<-add.constraint(portfolio = init.portfolio,type = "long_only")  
          con<-"Sin Venta En Corto"
          }
        init.portfolio<-add.constraint(portfolio = init.portfolio,type = "full_investment")
        init.portfolio<-add.constraint(portfolio = init.portfolio,type = "weight_sum",min_sum=1,max_sum=1)
  
  init.portfolio<- add.objective(portfolio = init.portfolio,type = "return",name="mean")
  init.portfolio<-add.objective(portfolio = init.portfolio,type = "risk",name = "StdDev")
  maxR<-optimize.portfolio(R=ret,portfolio = init.portfolio,optimize_method = "ROI",trace = TRUE)
  frontera<-extractEfficientFrontier(maxR,match.col = "StdDev",n.portfolios = np,
                           risk_aversion = NULL)
  chart.EfficientFrontier(frontera, 
                          match.col = "StdDev", n.portfolios = np, xlim = NULL, ylim = NULL,
                          cex.axis = 0.8, element.color = "darkgray", main = "Frontera Eficiente",
                          RAR.text = "SR", rf = TasaLibre, tangent.line = TRUE, cex.legend = 0.8,
                          chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                          cex.assets = 0.8,col="darkblue")
  
  final <- matrix("",4,2+length(acciones))
  final[,1]<-c("Media","Desv.Est.","Sharp.R","Porcentaje")
  
  colnames(final)<-c("Indicador",acciones,paste("Portafolio ",con, sep = ""))
  final[1,2:ncol(final)]<-c(round(apply(ret,2,mean),5),round(as.numeric(maxR$opt_values[1]),5))
  final[2,2:ncol(final)]<-c(round(apply(ret,2,sd),5),round(as.numeric(maxR$opt_values[2]),5))
  #SHARPE RATIO DEL PORTAFOLIO
  med<-apply(ret, 2,mean)%*%as.numeric(maxR$weights)
  sde<-sqrt((as.numeric(maxR$weights)%*%cov(ret)%*%as.numeric(maxR$weights)))
  Shaport<-(med-TasaLibre)/sde
  final[3,2:(ncol(final))]<-c(round(as.numeric(shar.indi),5),round(Shaport,5))
  final[4,2:ncol(final)]<-c(round(100*as.numeric(maxR$weights),5),"100%")

  return(final)
  
  
  
  }
optimizar(acciones,fecini,perio,TasaLibre,alfa,np,ventacorta)


