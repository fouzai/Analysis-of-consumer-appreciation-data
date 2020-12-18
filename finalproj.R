
library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(reshape2)
library(plyr)
library(agricolae)
library(plotly)
library(doBy)
library(stringr)
library(corrplot)
library(SensoMineR)
library(markdown)
library(shinythemes)
library(readxl)
ui=fluidPage( theme=shinytheme("slate"),
              themeSelector(),navbarPage("Sensory Analysis",
                        tabPanel("Sensory data",titlePanel("File Input"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file","Upload the file"), 
                                     helpText("Default max. file size is 5MB"),
                                     tags$hr(),
                                     h5(helpText("Select the read.table parameters below")),
                                     checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                     checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                     br(),
                                     
                                     radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                     textInput(inputId = "juge","choose the name of your juge",value = ""),
                                     br(),
                                     textInput(inputId ="seance","choose the name of your session",value = ""),
                                     br(),
                                     textInput(inputId ="produit","choose the name of your product",value = "")
                                     
                                   ),
                                   mainPanel(
                                     uiOutput("tb")
                                     
                                     
                                     
                                     
                                   )
                                   
                                 )),
                        tabPanel("Consumer data",titlePanel("File Input"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("filee","Upload the file"), 
                                     helpText("Default max. file size is 5MB"),
                                     tags$hr(),
                                     h5(helpText("Select the read.table parameters below")),
                                     checkboxInput(inputId = 'headerr', label = 'Header', value = FALSE),
                                     checkboxInput(inputId = "stringAsFactorss", "stringAsFactorss", FALSE),
                                     br(),
                                     radioButtons(inputId = 'sepp', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                                   ),
                                   mainPanel(
                                     uiOutput("th")
                                     
                                     
                                     
                                     
                                   )
                                   
                                 ))
                        ,tabPanel("Decriptive analysis",tabsetPanel(tabPanel("summary",verbatimTextOutput ("sum"),tableOutput("et"),downloadButton(outputId = "dowww",label = "Download")),tabPanel("boxplot",uiOutput("boxplot"),plotOutput("an"),plotOutput("ann"),plotOutput("annn"),downloadButton(outputId = "dowww1",label = "Download")),tabPanel("dynamic",uiOutput("d"),uiOutput("d12"),uiOutput("d13"),plotlyOutput("d1"),uiOutput("d88"),plotlyOutput("d2"))))
                        ,tabPanel("Univariate analysis",tabsetPanel( tabPanel("anova",uiOutput("aaa"),uiOutput("aov5"),uiOutput("aov55"),uiOutput("aov555"),verbatimTextOutput("aov21")),
                                                                     tabPanel("Tuckey test",plotOutput("tuck"),downloadButton(outputId = "dowww2",label = "Download"))
                        )),tabPanel("Multivariate Analysis"
                                    ,tabsetPanel(tabPanel("Mean table",tableOutput("mean"),downloadButton(outputId = "dowww3",label = "Download")),tabPanel("PCA",uiOutput("rrr"),plotOutput("innn"),plotOutput("va"),downloadButton(outputId = "dowww5",label = "Download")),tabPanel("Contribution",uiOutput("con"),plotOutput("contr"),downloadButton(outputId = "dowww7",label = "Download")),
                                                 tabPanel("cos2",uiOutput("coos"),plotOutput("cos"),downloadButton(outputId = "dowww4",label = "Download")),tabPanel("Eigen Value",plotOutput("eig"),downloadButton(outputId = "dowww6",label = "Download")),tabPanel("Biplot",plotOutput("cercle")))),tabPanel(" Mapping",tabsetPanel(tabPanel("mixed table",tableOutput("mix"),downloadButton(outputId = "dowww8",label = "Download")),tabPanel("PCA",plotOutput("pp")),tabPanel("Contribution",plotOutput("ppp"),plotOutput("pppp")),tabPanel("scree plot",plotOutput("z")))),tabPanel("Clustering",tabsetPanel( tabPanel("Cluster",plotOutput("cluster"),downloadButton(outputId = "dowww9",label = "Download")),tabPanel("Dendogram",plotOutput("dendo"),downloadButton(outputId = "dowww10",label = "Download")),tabPanel("Hierarchical clustering",plotOutput("td"),downloadButton(outputId = "dowww11",label = "Download"))))
                        
)

)


Server=function(input,output){
  
  
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    na.omit(data())
  })
  
  
  output$tb <- renderUI({
    
    tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary", tableOutput("sum")))
  })
  #importation hedo
  ##############
  ##################"
  ################"
  ################"
  
  
  hedo <- reactive({
    file1 <- input$filee
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sepp, header = input$headerr, stringsAsFactors = input$stringAsFactorss)
    
  })
  
  
  output$filedh <- renderTable({
    if(is.null(hedo())){return ()}
    input$file1
  })
  
  
  output$summ <- renderTable({
    if(is.null(hedo())){return ()}
    summary(hedo())
    
  })
  
  
  output$tablee <- renderTable({
    if(is.null(hedo())){return ()}
    hedo()
  })
  
  
  output$th <- renderUI({
    
    tabsetPanel(tabPanel("About file", tableOutput("filedh")),tabPanel("Data", tableOutput("tablee")),tabPanel("Summary", tableOutput("summ")))
  })
  #####################
  ##################
  ##################
  ####################
  #####################
  juge<- reactive({
    juge=input$juge
    
  })
  produit<- reactive({
    produit=input$produit
    
  })
  seance<- reactive({
    seance=input$seance
    
  })
  
  
  output$aaa=renderUI({
    selectInput("aov11","choose your descriptor",names(data()[,4:26]))
    
    
  })
  
  
  
  
  
  #######
  #####
  ##Boxplot
  
  output$boxplot=renderUI({
    selectInput("var","choose your descriptor",names(data()))
    
    
  })
  output$an=renderPlot({
    
    attach(data())
    ggplot(data(),aes(x = as.factor(data()[,juge()]),y=data()[,input$var]))+geom_boxplot()+
      ggtitle("effet juge")
    
    
    
    
  })
  output$ann=renderPlot({
    ggplot(data(),aes(x = as.factor(data()[,produit()]),y=data()[,input$var]))+geom_boxplot()+
      ggtitle("effet produit")
    
  })
  output$annn=renderPlot({
    ggplot(data(),aes(x = as.factor(data()[,seance()]),y=data()[,input$var]))+geom_boxplot()+
      ggtitle("effet seance")
  })
  
  
  
  
  
  
  #######test tuckey 
  
  output$tuck=renderPlot({
    
    ch=toString(input$ao4)
    ch=str_replace_all(ch," ","+")
    ch=str_replace_all(ch,",","")
    ch1=toString(input$ao44)
    ch1=str_replace_all(ch1," ","+")
    ch1=str_replace_all(ch1,",","")
    ch2=toString(input$ao444)
    ch2=str_replace_all(ch2," ","+")
    ch2=str_replace_all(ch2,",","")
    if(ch1!="" &&ch2!=""){sth=paste(ch,ch1,ch2,sep = "+")}
    else if(ch2==""){sth=paste(ch,ch1,sep = "+")}
    
    else{str=paste(ch,"+",sep = "")}
    if(ch!=""){
      formul=paste(input$aov11,"~",sth,sep = "")
      lmm=lm(as.formula(formul),data=data())
      aov4=aov(lmm,data=data())
      tuck=HSD.test(aov4,"produit",group=TRUE)
      return(plot(tuck))
    }
  })
  
  
  
  output$sum=renderPrint({
    summary(na.omit(data()))
  })
  ####Mean table
  #######
  output$mean=renderTable({
    base=data()
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    
    final
    
    
    
  })
  
  ####acp
  
  #output$acp=renderPlot({
  # se=na.omit(data())
  #se$produit=as.factor(se$produit)
  #se$juge=as.factor(se$juge)
  #se$seance=as.factor(se$seance)
  #se=summaryBy(.~produit,data=se[,-c(1:2)], FUN = c(mean) , na.rm= T)
  
  
  #PCA(se[,-1])
  # })
  
  
  #######
  #######
  ###anova
  output$aov5=renderUI({
    selectInput("ao4", "choose the first variable",
                names(data()[1:3]), multiple =TRUE)})
  output$aov55=renderUI({
    selectInput("ao44", "choose the second variable",
                names(data()[1:3]), multiple =TRUE)})
  output$aov555=renderUI({
    selectInput("ao444", "choose the third variable",
                names(data()[1:3]), multiple =TRUE)})
  
  
  output$aov21=renderPrint({
    
    
    ch=toString(input$ao4)
    ch=str_replace_all(ch," ","+")
    ch=str_replace_all(ch,",","")
    ch1=toString(input$ao44)
    ch1=str_replace_all(ch1," ","+")
    ch1=str_replace_all(ch1,",","")
    ch2=toString(input$ao444)
    ch2=str_replace_all(ch2," ","+")
    ch2=str_replace_all(ch2,",","")
    if(ch1!="" &&ch2!=""){sth=paste(ch,ch1,ch2,sep = "*")}
    else if(ch2==""){sth=paste(ch,ch1,sep = "*")}
    else{str=paste(ch,"*",sep = "")}
    if(ch!=""){
      formul=paste(input$aov11,"~",sth,sep = "")
      print(formul)
      lmm=lm(as.formula(formul),data=data())
      aov4=aov(lmm,data=data())
      tuck=HSD.test(aov4,"produit",group=TRUE)
      return(summary(aov(lmm,data=data())))
    }
  })
  
  ########
  
  output$d=renderUI({
    selectInput("dd","choose your descripter",names(data()[4:26]))
  })
  output$d12=renderUI({
    selectInput("d14","choose your descripter",names(data()[4:26]))
  })
  output$d13=renderUI({
    selectInput("d15","choose your descripter",names(data()[4:26]))
  })
  
  
  output$d1=renderPlotly({
    
    
    p1 <- plot_ly(data(), x = ~data()[,juge()], y = ~data()[,input$dd],color = ~data()[,juge()], type = "box") 
    
    p2 <- plot_ly(data(), x = ~data()[,juge()], y = ~data()[,input$d14],color = ~data()[,juge()], type = "box") 
    p3<-plot_ly(data(),x=~data()[,juge()],y=~data()[,input$d15],color = ~data()[,juge()],type="box")
    
    subplot(p1,p2,p3)
    
  })
  
  
  output$con=renderUI({
    selectInput("select","choose",choices = c("ind"="ind","var"="var","both"="both"))
  })
  
  output$contr=renderPlot({
    
    base=data()
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    final=final[,-1]
    mean_table=final
    
    
    
    res_pca=PCA(mean_table,quali.sup = 23)
    if (input$select=="ind"){
      fviz_pca_contrib(res_pca,choice = "ind",axes=1:2)
    }
    else if(input$select=="var"){
      fviz_pca_contrib(res_pca,choice = "var",axes=1:2)
    }
    else{
      p1=fviz_pca_contrib(res_pca,choice = "ind",axes=1:2)
      p2=fviz_pca_contrib(res_pca,choice = "var",axes=1:2)
      grid.arrange(p1,p2)
    }
    
  })
  
  mean_table=reactive({
    base=na.omit(data())
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    final=final[,-1]
    return(final)
  })
  
  output$coos=renderUI({
    selectInput("select1","choose",choices = c("ind"="ind","var"="var"))
    
  })
  
  output$cos=renderPlot({
    
    res_pca=PCA(mean_table(),quali.sup = 23,graph = F)
    var <- get_pca_var(res_pca)
    ind=get_pca_ind(res_pca)
    if (input$select1=="ind"){
      ind=get_pca_ind(res_pca)
      corrplot(ind$cos2, is.corr=FALSE)
    }
    else {
      var <- get_pca_var(res_pca)
      corrplot(var$cos2, is.corr=FALSE)}
  })
  
  output$eig=renderPlot({
    res_pca=PCA(mean_table(),quali.sup = 23)
    fviz_eig(res_pca, addlabels = TRUE)
  })
  
  cont=reactive({
    res_pca=PCA(mean_table(),quali.sup = 23,graph = F)
    if (input$select=="ind"){
      p=fviz_pca_contrib(res_pca,choice = "ind",axes=1:2)
      grid.arrange(p)
    }
    else if(input$select=="var"){
      p=fviz_pca_contrib(res_pca,choice = "var",axes=1:2)
      grid.arrange(p)
    }
    else{
      p1=fviz_pca_contrib(res_pca,choice = "ind",axes=1:2)
      p2=fviz_pca_contrib(res_pca,choice = "var",axes=1:2)
      grid.arrange(p1,p2)
    }
  })
  
  
  output$dowww=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww1=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww2=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww3=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww4=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww5=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww6=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww7=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww8=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww9=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww10=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )
  output$dowww11=downloadHandler(
    filename = "plot.png",
    content = function(file){
      png(file)
      cont()
      dev.off()
      
    },
    contentType = "image/png"
  )

  
  cluster=reactive({
    data=hedo()[,-1]
    return(cbind(mean_table(),data))
  })
  
  output$cluster=renderPlot({
    res_pca2=PCA(cluster(),quanti.sup = 1:23,quali.sup = 24)
    ress=HCPC(res_pca2,graph = F)
    fviz_cluster(ress)
  })
  
  output$dendo=renderPlot({
    res_pca2=PCA(cluster(),quanti.sup = 1:23,quali.sup = 24)
    ress=HCPC(res_pca2,graph = F)
    fviz_dend(ress)
  })
  output$td=renderPlot({
    res_pca2=PCA(cluster(),quanti.sup = 1:23,quali.sup = 24)
    ress=HCPC(res_pca2,graph = F)
    plot.HCPC(ress, choice = "3D.map")
  })
  

  
  output$mix=renderTable({
    base=data()
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    base2=hedo()
    
    base2=base2[,-1]
    final=final[,-1]
    mi=cbind.data.frame(final,base2)
    mii=cbind.data.frame(hedo()[,1],mi)
    mii
    
    
    
  })
  
  output$rrr=renderUI({
    selectInput("rr","choose",choices = c("cos2"="cos2","contrib"="contrib"))
  })
  
  output$innn=renderPlot({
    
    res.pca=PCA(mean_table(),quali.sup = 23)
    fviz_pca_var(res.pca, col.var = input$rr,
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE # Évite le chevauchement de texte
    )
  })
  
  output$va=renderPlot({
    res.pca=PCA(mean_table(),quali.sup = 23)
    fviz_pca_ind (res.pca, col.ind = input$rr,
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE # Évite le chevauchement de texte
    )
  })
  output$d88=renderUI({
    selectInput("re","choose",choices = c("juge effect" = "ej","Session effect" = "es","Product effect" = "ep"))
  })
  melted_senso=reactive({milt=melt(data(),id.vars = c("juge","produit","seance"),measure.vars = 4:26)
  return(milt)})
  melted_data=reactive({milt=melt(data(),id.vars = c("juge","seance"),measure.vars=4:26)
  return(milt)})
  
  output$d2=renderPlotly({
    
    if (input$re=="es"){
      plot_ly(melted_senso(),x=~seance,y=~value,frame = ~juge)%>%add_bars()}
    else if(input$re=="ej"){
      plot_ly(melted_senso(),x=~juge,y=~value,frame = ~variable)%>%add_bars()
    }
    else{plot_ly(melted_senso(),x=~produit,y=~value,frame = ~produit)%>%add_bars()}
  })
  output$et=renderTable({
    df=data.frame(data()[,-c(1:3)])
    df=na.omit(df)
    data5=do.call(data.frame,list(Moy=apply(df,2,mean),Ect_ty=apply(df, 2, sd),Mediane=apply(df,2,median),n=apply(df,2,length)))
    Descriptors=names(df)
    cbind(Descriptors,data5)
  })
  output$cercle=renderPlot({
    res=PCA(mean_table()[,-1])
    fviz_pca_biplot(res,repel = TRUE,col.var = "#2E9FDF",col.ind = "#696969")
  })
  
  output$pp=renderPlot({
    base=data()
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    base2=hedo()
    
    base2=base2[,-1]
    final=final[,-1]
    mi=cbind.data.frame(final,base2)
    mii=cbind.data.frame(hedo()[,1],mi)
    PCA(mii[,-1])
    
  })
  
  output$ppp<- renderPlot({
    base=data()
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    base2=hedo()
    
    base2=base2[,-1]
    final=final[,-1]
    mi=cbind.data.frame(final,base2)
    mii=cbind.data.frame(hedo()[,1],mi)
    res=PCA(mii[,-1])
    
    
    
    fviz_pca_var(res, col.var = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE )
    
  })
  
  
  output$pppp<- renderPlot({
    base=data()
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    base2=hedo()
    
    base2=base2[,-1]
    final=final[,-1]
    mi=cbind.data.frame(final,base2)
    mii=cbind.data.frame(hedo()[,1],mi)
    res=PCA(mii[,-1])
    
    
    fviz_pca_var(res, col.var="contrib")
    
  })
  output$z=renderPlot({
    base=data()
    base$produit=as.factor(base$produit)
    base$juge=as.factor(base$juge)
    base$seance=as.factor(base$seance)
    final=summaryBy(.~produit , data = base[, -c(1:2)] , FUN = c(mean) , na.rm= T)
    base2=hedo()
    
    base2=base2[,-1]
    final=final[,-1]
    mi=cbind.data.frame(final,base2)
    mii=cbind.data.frame(hedo()[,1],mi)
    res=PCA(mii[,-1])
    fviz_eig(res, addlabels = TRUE)
  })
  
}

shinyApp(ui,Server)
