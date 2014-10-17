library(shiny)
#library(rCharts)
library(quantmod)
library(shiny)
palettes <- c("Brown-DkGn","Wt-MdBlue","Wt-Yl-Gn","Orange-Blue","Wt-OrRd","LightBlue-Purple")
pkgs <- c("png","grid")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
library(png); 
library(grid)
library(ggplot2)
library('scales')
require(png)

shinyServer(
  function(input, output, session) {                
    
    output$PNumControl <- renderUI({
      
      poolNumChoices = NULL
      
      if(input$PlotChoiceInput == 'CumCER') {
        poolNumChoices = levels(DTIungrouped1$PoolNum)
      } 
      else if(input$BucketInput == 'LF'){
        poolNumChoices = levels(FicoLtvDlqBkt1$PoolNum)
      }
      else if(input$BucketInput == 'ALS') {
        poolNumChoices = levels(AlsDlqBkt1$PoolNum)
      }
      else if(input$BucketInput == 'ALL') {
        poolNumChoices = levels(DlqBkt1$PoolNum)
      }   
      
      if(!is.null(poolNumChoices)) {
        selectInput(
          "PoolNumInput",
          label = "PoolNum",
          choices = poolNumChoices,
          multiple = TRUE
        )
      }
    })
    
    output$OTypeControl <- renderUI({
      
      origTermChoices = NULL
      
      if(input$PlotChoiceInput == 'CumCER') {
        origTermChoices = DTIungrouped1$OrigTerm
      }
      else if(input$BucketInput == 'ALL') {
        origTermChoices = DlqBkt1$OrigTerm
      }
      else if (input$BucketInput == 'LF') {
        origTermChoices = FicoLtvDlqBkt1$OrigTerm
      }
      else if (input$BucketInput == 'ALS') {
        origTermChoices = AlsDlqBkt1$OrigTerm
      }
      
      if(!is.null(origTermChoices)){
        selectInput(
          "OrigTermInput",
          label = "OrigTerm",
          choices = origTermChoices
        )
      }
      
    })
    
    output$PTypeControl <- renderUI({
      
      prodTypeChoices = NULL
      
      if(input$PlotChoiceInput == 'CumCER') {
        prodTypeChoices = levels(DTIungrouped1$ProdType)
      }
      else if(input$BucketInput == 'ALL') {
        prodTypeChoices = levels(DlqBkt1$ProdType)
      }
      else if(input$BucketInput == 'LF') {
        prodTypeChoices = levels(FicoLtvDlqBkt1$ProdType)
      }
      else if (input$BucketInput == 'ALS') {
        prodTypeChoices = levels(AlsDlqBkt1$ProdType)
      }
     
      if(!is.null(prodTypeChoices)) {
        selectInput(
          "ProdTypeInput",
          label = "ProdType",
          choices = prodTypeChoices
        )
      }     
      
    })
    
    output$DlqStatusControl <- renderUI({
      
      dlqStatusChoices = NULL
      
      if(input$BucketInput == 'LF') {
        dlqStatusChoices = levels(FicoLtvDlqBkt1$DlqStatus)
      }
      else if(input$BucketInput == 'ALS') {
        dlqStatusChoices = levels(AlsDlqBkt1$DlqStatus)
      }
      else if(input$BucketInput == 'ALL') {        
        dlqStatusChoices = levels(DlqBkt1$DlqStatus)
      }
      
      if(!is.null(dlqStatusChoices)){
        selectInput(
          "DlqStatusInput",
          label = "DlqStatus",
          choices = dlqStatusChoices
        )
      }
    })
    
    output$distPlot <- renderPlot({ 
      
      if(input$PlotChoiceInput == 'CumCER'){
        
        plotCERData();
        
      }
      ###############Roll Rate###############
      else if(input$ComparisonInput == 'RR') {                             
      
      if(input$BucketInput == 'LF') {                     
        
        plotPCLTVData()
      }   
      else if(input$BucketInput == 'ALS') {                     
        
        plotPCLSData()
      }
      else if(input$BucketInput == 'ALL') {                     
        
        plotPCALLData()        
      }
      }
      ###############UPB Perc###############
      else if(input$ComparisonInput == 'UP') {
        print(input$ComparisonInput)
        if(input$BucketInput == 'LF') {                     
          print(input$BucketInput)
          plotPCLTVData()
        }   
        else if(input$BucketInput == 'ALS') {                               
          plotPCLSData()
        }
        else if(input$BucketInput == 'ALL') {                     
          
          plotPCALLData()        
        }
      }
      
      
    })
    
    output$view <- renderTable({
      
      data = NULL
      
      if(input$PlotChoiceInput == 'CumCER'){
        
        data = getLoansCERData()
        
        if(nrow(data) > 1){ 
          data$CumCER = paste(format(round(data$CumCER * 100, 4), nsmall=4), "%") 
        }
        
        data$AggOrigBal = format(data$AggOrigBal, trim = TRUE, nsmall=0)
      }
      else if(input$BucketInput == 'LF') {                     
        
        data = getLoansPCLTVData()
        #data$Dlq_Percent = paste(format(round(data$Dlq_Percent * 100, 4), nsmall=4), "%")         
      }
      else if(input$BucketInput == 'ALS') {
        
        data = getLoansPCLSData()
        #         if(nrow(data) > 1) 
        #         { data$Dlq_Percent = 
        #             paste(format(round(data$Dlq_Percent * 100, 4), nsmall=4), "%")
        #         }
      }
      else if(input$BucketInput == 'ALL') {        
        
        data = getLoansPCALLData()
        #data$Dlq_Percent = paste(format(round(data$Dlq_Percent * 100, 4), nsmall=4), "%")         
      }      
      if((!is.null(data)) && nrow(data) > 1){        
        data
      }
    })
    
    getLoansCERData = reactive({ 
      
      temp = subset(
        DTIungrouped1, 
        PoolNum %in% input$PoolNumInput
        & ProdType == input$ProdTypeInput
        & OrigTerm == input$OrigTermInput
        
      )[c("PoolNum","Period","AggOrigBal","AggDlq180PlusBal")]
      
      temp$Period <- as.factor(temp$Period);   
      
      temp[("CumCER")] <- temp$AggDlq180PlusBal/temp$AggOrigBal;
      
      colnames(temp) = c("PoolNum", "Period","AggOrigBal","AggDlq180PlusBal","CumCER")
      
      for (c in 1:nrow(temp)) {        
        
        if((c>1) && (temp[c,"PoolNum"] == temp[c-1,"PoolNum"])){
          
          temp[c,"CumCER"] <- temp[c,"CumCER"] + temp[c-1,"CumCER"]
        }
      }
      
      if(nrow(temp) > 1) { 
        
        rownames(temp) = 1:nrow(temp) 
      }
      
      temp      
    })    
    
    plotCERData = reactive({  
      
      data = getLoansCERData()
      
      if(nrow(data) > 1){
        
        data$Period = 0:(nrow(data)-1)  
        
        p <- ggplot(data, aes(x= Period,y=CumCER, group = PoolNum))          
        
        p <- p + geom_line(aes(fill = PoolNum, colour =  PoolNum),                            
                           label = 'STACR' ,size=1,na.rm = FALSE,show_guide=TRUE)                
        
        p <- p + geom_text(aes(label = percent(CumCER)))
        
        p <- p + scale_y_continuous(labels = percent_format())
        
        #         print(p)
        #         ggsave("plot.png", width=4, height=4, dpi=100)
        
        p           
      }
    })    
    
    getLoansPCLTVData = reactive({    
     
      
        if(input$ComparisonInput == 'UP'){
        temp  = subset(            
          FicoLtvDlqBkt1, 
          PoolNum %in% input$PoolNumInput    
          &  ProdType == input$ProdTypeInput
          & OrigTerm == input$OrigTermInput
          & FicoBucket == input$FicoBucketInput
          & LtvBucket == input$LtvBucketInput            
          & DlqStatus == input$DlqStatusInput        
        )[c("PoolNum","Period","AggCurrBal")]  
        print(temp)
        temp$Period <- as.factor(temp$Period)
        colnames(temp) = c("PoolNum", "Period","CurBal")     
        print(temp)
        }
        if(!(is.null(input$DlqChoiceInput) || input$DlqChoiceInput == '')) {
        if(input$ComparisonInput == 'RR'){
          temp  = subset(            
            FicoLtvDlqBkt1, 
            PoolNum %in% input$PoolNumInput    
            &  ProdType == input$ProdTypeInput
            & OrigTerm == input$OrigTermInput
            & FicoBucket == input$FicoBucketInput
            & LtvBucket == input$LtvBucketInput            
            & DlqStatus == input$DlqStatusInput        
          )[c("PoolNum","Period",input$DlqChoiceInput)]
          temp$Period <- as.factor(temp$Period)          
          colnames(temp) = c("PoolNum", "Period","DlqPer")
        }}
        
        
        if(nrow(temp) > 1) { 
          
          rownames(temp) = 1:nrow(temp) 
        }
        
        temp
      
    })
    
    plotPCLTVData = reactive({
      data = getLoansPCLTVData()
      print(data)
      print(input$ComparisonInput)
      
      if((!is.null(data)) && nrow(data) > 1) {
        
        #data$Period = 0:(nrow(data)-1 )
        
        if(input$ComparisonInput == 'RR'){
        p <- ggplot(data, aes(x= Period,y=DlqPer, group = PoolNum))     
        p <- p + geom_line(aes(fill = PoolNum, colour =  PoolNum),                            
                           label = 'STACR' ,size=1,na.rm = FALSE,show_guide=TRUE)                
        p <- p + geom_text(aes(label = percent(DlqPer)))
        }
        else if(input$ComparisonInput == 'UP'){
        p <- ggplot(data, aes(x= Period,y=CurBal, group = PoolNum))          
                
        p <- p + geom_line(aes(fill = PoolNum, colour =  PoolNum),                            
                           label = 'STACR' ,size=1,na.rm = FALSE,show_guide=TRUE)                
        p <- p + geom_text(aes(label = (CurBal)))        
      }
        #p <- p + scale_y_continuous(labels = percent_format())        
        p           
      }
    })    
    
    getLoansPCLSData = reactive({
      
      if(!(is.null(input$DlqChoiceInput) || input$DlqChoiceInput == '')) {
        
        if(input$ComparisonInput == 'RR'){
        temp  = subset(            
          AlsDlqBkt1, 
          PoolNum %in% input$PoolNumInput    
          & ProdType == input$ProdTypeInput
          & OrigTerm == input$OrigTermInput
          & LSBucket == input$LSBucketInput                  
          & DlqStatus == input$DlqStatusInput        
        )[c("PoolNum","Period","Wala","Fico","Oltv",input$DlqChoiceInput,"AggOrigBal","TotalLoans","ALS","AggCurrBal")]  
        
        temp$Period <- as.factor(temp$Period)           
        
        colnames(temp) = c("PoolNum", "Period","Wala","Fico","Oltv","Dlq_Percent","Orig Bal","Total Loans","ALS","Curr Bal")
        }
        if(input$ComparisonInput == 'UP'){
          temp  = subset(            
            AlsDlqBkt1, 
            PoolNum %in% input$PoolNumInput    
            & ProdType == input$ProdTypeInput
            & OrigTerm == input$OrigTermInput
            & LSBucket == input$LSBucketInput                  
            & DlqStatus == input$DlqStatusInput        
          )[c("PoolNum","Period","Wala","Fico","Oltv","AggOrigBal","TotalLoans","ALS","AggCurrBal")]  
          
          temp$Period <- as.factor(temp$Period)           
          
          colnames(temp) = c("PoolNum", "Period","Wala","Fico","Oltv","Orig Bal","Total Loans","ALS","Curr Bal")
        }
        
        if(nrow(temp) > 1) {
          rownames(temp) = 1:nrow(temp)
        }
        temp
      }
    })   
    
    plotPCLSData = reactive({      
      
      data = getLoansPCLSData()
      if((!is.null(data)) && nrow(data) > 1) {
        data$Period = 0:(nrow(data)-1 )
        
        
        if(input$ComparisonInput == 'RR'){
        p <- ggplot(data, aes(x= Period,y=Dlq_Percent, group = PoolNum))
        }
        else if(input$ComparisonInput == 'UP'){
        p <- ggplot(data, aes(x= Period,y="Curr Bal", group = PoolNum))
        }
        
        p <- p + geom_line(aes(fill = PoolNum, colour =  PoolNum),                            
                           label = 'STACR' ,size=1,na.rm = FALSE,show_guide=TRUE)
        p <- p + geom_text(aes(label = percent(Dlq_Percent)))
        p <- p + scale_y_continuous(labels = percent_format())
        p           
      }
    })    
    
    getLoansPCALLData = reactive({  
      
      if(!(is.null(input$DlqChoiceInput) || input$DlqChoiceInput == '')) 
      {
        if(input$ComparisonInput == 'RR'){
        temp  = subset(            
          DlqBkt1, 
          PoolNum %in% input$PoolNumInput    
          &  ProdType == input$ProdTypeInput
          & OrigTerm == input$OrigTermInput                  
          & DlqStatus == input$DlqStatusInput        
        )[c("PoolNum","Period",input$DlqChoiceInput)] 
        temp$Period <- as.factor(temp$Period)    
        colnames(temp) = c("PoolNum", "Period","DlqPer")  
        }
        if(input$ComparisonInput == 'UP'){
          temp  = subset(            
            DlqBkt1, 
            PoolNum %in% input$PoolNumInput    
            &  ProdType == input$ProdTypeInput
            & OrigTerm == input$OrigTermInput                  
            & DlqStatus == input$DlqStatusInput        
          )[c("PoolNum","Period","AggCurrBal")]
          temp$Period <- as.factor(temp$Period)    
          colnames(temp) = c("PoolNum", "Period","Current Balance")  
        }
        
        if(nrow(temp) > 1) {
          rownames(temp) = 1:nrow(temp) 
        }
        temp
      
      }
    })
    
    plotPCALLData = reactive({      
      
      data = getLoansPCALLData()
      
      if((!is.null(data)) && nrow(data) > 1) {
        data$Period = 0:(nrow(data)-1 )
        if(input$ComparisonInput == 'UP'){
        p <- ggplot(data, aes(x= Period, y="Current Balance", group = PoolNum))
        }
        else
        p <- ggplot(data, aes(x= Period, y=DlqPer, group = PoolNum))
        
        p <- p + geom_line(aes(fill = PoolNum, colour =  PoolNum),                            
                           label = 'STACR' ,size=1,na.rm = FALSE,show_guide=TRUE)                
        
        #p <- p + geom_text(aes(label = percent(DlqPer)))
        
        p <- p + scale_y_continuous(labels = percent_format())
        
        p     
      }
    })
    
    

    getLoansBalData = reactive({ 
      
      temp = subset(
        DTIungrouped1, 
        PoolNum %in% input$PoolNumInput
        & ProdType == input$ProdTypeInput
        & OrigTerm == input$OrigTermInput
        
      )[c("PoolNum","Period","AggCurrBal")]  
      
      if(nrow(temp) > 1) { 
        
        rownames(temp) = 1:nrow(temp) 
      }
      
      temp      
    })
    
    plotCERbal = reactive({  
      
      data = getLoansBalData()
      
      if(nrow(data) > 1){
        
        data$Period = 0:(nrow(data)-1)  
        
        p <- ggplot(data, aes(x= Period,y=AggCurrBal, group = PoolNum))          
        
        p <- p + geom_line(aes(fill = PoolNum, colour =  PoolNum),                            
                           label = 'STACR' ,size=1,na.rm = FALSE,show_guide=TRUE)                
        
        #p <- p + geom_text(aes(label = percent(CumCER)))
        
        p <- p + scale_y_continuous(expression(Balance))
        
        #         print(p)
        #         ggsave("plot.png", width=4, height=4, dpi=100)
        
        p           
      }
    })  
    
    
    # Plot Choice changed
    observe({
      input$PlotChoiceInput
      
      updateSelectInput(session, "BucketInput", selected = '')
      updateSelectInput(session, "PoolNumInput", selected = '')
      updateSelectInput(session, "ComparisonInput", selected = '')
      updateSelectInput(session, "ProdTypeInput", selected = '')
      updateSelectInput(session, "OrigTermInput", selected = '')
      updateSelectInput(session, "LTVBucketInput", selected = '')
      updateSelectInput(session, "FicoBucketInput", selected = '')
      updateSelectInput(session, "DlqStatusInput", selected = '') 
      updateSelectInput(session, "LSBucketInput", selected = '')      
      updateSelectInput(session, "DlqChoiceInput", selected = '')
    })
    
    # Comparision Choice changed
    observe({
      input$ComparisonInput
      
      updateSelectInput(session, "PoolNumInput", selected = '')
      updateSelectInput(session, "ProdTypeInput", selected = '')
      updateSelectInput(session, "OrigTermInput", selected = '')
      updateSelectInput(session, "LTVBucketInput", selected = '')
      updateSelectInput(session, "FicoBucketInput", selected = '')
      updateSelectInput(session, "DlqStatusInput", selected = '') 
      updateSelectInput(session, "LSBucketInput", selected = '')      
      updateSelectInput(session, "DlqChoiceInput", selected = '')      
    })
    
    #Reset Button
    observe({
      if (input$ResetInput == 0)
        return()      
      
      updateSelectInput(session, "PlotChoiceInput", selected = '')
      updateSelectInput(session, "ComparisonInput", selected = '')
      updateSelectInput(session, "BucketInput", selected = '')
      updateSelectInput(session, "PoolNumInput", selected = '')
      updateSelectInput(session, "ProdTypeInput", selected = '')
      updateSelectInput(session, "OrigTermInput", selected = '')
      updateSelectInput(session, "LTVBucketInput", selected = '')
      updateSelectInput(session, "FicoBucketInput", selected = '')
      updateSelectInput(session, "LSBucketInput", selected = '')      
      updateSelectInput(session, "DlqStatusInput", selected = '') 
      updateSelectInput(session, "DlqChoiceInput", selected = '')
    })
    
    output$downloadPlot <- downloadHandler(
      filename = function() { paste("test", '.png', sep='') },
      content = function(file) {
        device <- function(
          ..., width, height) 
          grDevices::png(..., width = 20, height = 15,
                         res = 300, units = "in")
        ggsave(file, plot = 
                 
                 ( if(input$PlotChoiceInput == 'CumCER'){
                   
                   plotCERData();
                   
                 }
                 else if(input$ComparisonInput == 'LF') {                     
                   
                   plotPCLTVData()
                 }   
                 else if(input$ComparisonInput == 'ALS') {                     
                   
                   plotPCLSData()
                 }
                 else if(input$ComparisonInput == 'ALL') {                     
                   
                   plotPCALLData()        
                 }     )          
               , device = device)
      }
    )   
}
)
