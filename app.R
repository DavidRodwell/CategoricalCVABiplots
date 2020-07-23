library(shiny)

Install.packagesCatCVA <- function() {
  #ensures all packages are installed
  
  list.of.packages <-
    c("Matrix", "caret", "MASS", "ratte.data", "readxl", "e1071")
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  
  library(caret)
  library(MASS)
  library(Matrix)
  library(rattle.data)
  library(readxl)
  library(e1071)
  
  #imports all necessary biplot drawing functions
  
  BiplotFunctions <<- source("source/BiplotFunctionsClean.R")$value
  
 #imports all data sets - wine found in rattle.data package, iris is built into base R
  
  wifi_localization <<-read.delim("data/wifi_localization.txt", header = FALSE)
  score_data <<-
    read_excel(
      "data/Data_User_Modeling_Dataset_Hamdi Tolga KAHRAMAN.xls",
      sheet = "Training_Data",
      col_types = c(
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "numeric",
        "text",
        "skip",
        "skip",
        "skip"
      )
    )
  
  Remuneration <<- read.csv("data/Remuneration")
  
}

confusion.biplot.full<-function(Zmat,groups,class.numb){
  check<-rep(1,class.numb)/class.numb
  lda.model<-lda(Zmat,grouping = groups,prior=check)
  lda.pred<-predict(lda.model, Zmat)
  lda.class <- lda.pred$class
  return(confusionMatrix(lda.class,groups))
}

Install.packagesCatCVA()

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

ui <- tagList(
  navbarPage(
    theme = "yeti",
    title = "Categorical Biplot Comparison",
    id = "headtab",
    
    
    #IRIS panel
    tabPanel(
      "IRIS",
      sidebarPanel(
        "Biplot Settings",
        helpText(" "),
        radioButtons(
          "bipltype.iris",
          "Underlying biplot used",
          selected = "CVA H",
          inline = T,
          choices = c("CVA H", "catPCA", "CVA En")
        ),
        checkboxInput(
          "classreg.iris",
          label = strong("Class Regions"),
          value = T
        ),
        sliderInput(
          "hrank.iris",
          label = "Rank of H matrix (CVA H)",
          min = 2,
          max = 4,
          step = 1,
          value = 2
        ),
        h5(strong("Data set information:")),
        h5("Number of classes: 3"),
        h5("Number of attributes: 4 (2 numerical, 2 categorical)"),
        h5("Number of observations: 150"),
        linebreaks(1),
        h5(strong("Data set description:")),
        h5(
          "This famous (Fisher's or Anderson's) iris data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica."
        ),
        linebreaks(1),
        h5(
          "The last two variables have been transformed into categorical variables with 5 levels. In addition, the data has no missing values."
        ),
        linebreaks(1),
        tableOutput("basicerror.iris"),
        linebreaks(1)
        ,
        h5(strong("Confusion matrix:")),
        tableOutput("confusion.iris")
      )
      
      
    ),
    
    #Wine Panel
    
    tabPanel(
      "Wine",
      sidebarPanel(
        "Biplot Settings",
        helpText(" "),
        radioButtons(
          "bipltype.wine",
          "Underlying biplot used",
          selected = "CVA H",
          inline = T,
          choices = c("CVA H", "catPCA", "CVA En")
        ),
        checkboxInput(
          "classreg.wine",
          label = strong("Class Regions"),
          value = T
        ),
        sliderInput(
          "hrank.wine",
          label = "Rank of H matrix (CVA H)",
          min = 2 ,
          max = 13,
          step = 1,
          value = 2
        ),
        h5(strong("Data set information:")),
        h5("Number of classes: 3"),
        h5("Number of attributes: 13 (9 numerical, 4 categorical)"),
        h5("Number of observations: 178"),
        linebreaks(1),
        h5(strong("Data set description:")),
        h5(
          "The wine dataset contains the results of a chemical analysis of wines grown in a specific area of Italy."
        ),
        linebreaks(1),
        h5(
          "Three types of wine are represented in the 178 samples, with the results of 13 chemical analyses recorded for each sample. The first four variables, in addition to the Type and first four numeric variables (Alcohol, Malic, Ash and Alcalinity), have been transformed into a categorical variables with three levels. The data contains no missing values, with a three class target variable (Type) for classification."
        ),
        linebreaks(1),
        tableOutput("basicerror.wine"),
        linebreaks(1)
        ,
        h5(strong("Confusion matrix:")),
        tableOutput("confusion.wine")
      )

    ),
    
    #User Modelling Panel
    
    tabPanel(
      "User Score",
      sidebarPanel(
        "Biplot Settings",
        helpText(" "),
        radioButtons(
          "bipltype.score",
          "Underlying biplot used",
          selected = "CVA H",
          inline = T,
          choices = c("CVA H", "catPCA", "CVA En")
        ),
        checkboxInput(
          "classreg.score",
          label = strong("Class Regions"),
          value = T
        ),
        sliderInput(
          "hrank.score",
          label = "Rank of H matrix (CVA H)",
          min = 2 ,
          max = 5,
          step = 1,
          value = 2
        ),
        h5(strong("Data set information:")),
        h5("Number of classes: 4"),
        h5("Number of attributes: 5 (2 numerical, 3 categorical)"),
        h5("Number of observations: 258"),
        linebreaks(1),
        h5(strong("Data set description")),
        h5( "The data collected from various students is used to determine their knowledge class. A weight value of [0-1] is assigned to each attribute. In addition, the first three attributes are treated as categorical variables consisting of three levels."), 
        linebreaks(1),
        h5(strong("Attributes:")),
        h5("STG - Degree of study time for goal objective"),
        h5("SCG - Degree of repetition number of user for goal objective"),
        h5("STR - Degree of student time of user for related objectives with goal objective"),
        h5("LPR - The exam performance of user for related objective with goal objective"),
        h5("PEG - The exam performance of user for goal objective"),
        linebreaks(1),
        tableOutput("basicerror.score"),
        linebreaks(1)
        ,
        h5(strong("Confusion matrix:")),
        tableOutput("confusion.score")
        
        
        
      )

    ),
    
    #Wifi Localisation Panel
    
    tabPanel(
      "Wifi",
      sidebarPanel(
        "Biplot Settings",
        helpText(" "),
        radioButtons(
          "bipltype.wifi",
          "Underlying biplot used",
          selected = "CVA H",
          inline = T,
          choices = c("CVA H", "catPCA", "CVA En")
        ),
        checkboxInput(
          "classreg.wifi",
          label = strong("Class Regions"),
          value = T
        ),
        sliderInput(
          "hrank.wifi",
          label = "Rank of H matrix",
          min = 2 ,
          max = 7,
          step = 1,
          value = 2
        ),
        h5(strong("Data set information:")),
        h5("Number of classes: 4"),
        h5("Number of attributes: 7 (4 numerical, 3 categorical)"),
        h5("Number of observations: 2000"),
        linebreaks(1),
        h5(strong("Data set description:")),
        h5( "The data set was collected in an indoor space by observing signal strengths of seven WiFi signals visible on a smartphone. The decision variable is one of the four locations."), 
        linebreaks(1), 
        h5("Each attribute is the WiFi signal strength observed on a smartphone. In addition, the first three cellphone signals were transformed into cateogorical levels with three levels.")
        ,
        linebreaks(1),
        tableOutput("basicerror.wifi"),
        linebreaks(1),
        h5(strong("Confusion matrix:")),
        tableOutput("confusion.wifi")
        )
      

    ),
    
    #------Remuneration Panel--------------
    
    tabPanel(
      "Remuneration",
      sidebarPanel(
        "Biplot Settings",
        helpText(" "),
        radioButtons(
          "bipltype.renum",
          "Underlying biplot used",
          selected = "CVA H",
          inline = T,
          choices = c("CVA H", "catPCA", "CVA En")
        ),
        checkboxInput(
          "classreg.renum",
          label = strong("Class Regions"),
          value = T
        ),
        sliderInput(
          "hrank.renum",
          label = "Rank of H matrix",
          min = 2 ,
          max = 6,
          step = 1,
          value = 2
        ),
        h5(strong("Data set information:")),
        h5("Number of classes: 2"),
        h5("Number of attributes: 6 (6 categorical)"),
        h5("Number of observations: 728"),
        linebreaks(1),
        h5(strong("Data set description:")),
        h5( "The data set of permanent full-time academic staff at Stellenbosch University for 2002 was collected to focus on gender Remuneration differentials."), 
        linebreaks(1), 
        h5(strong("Attributes:")),
        h5("Remun - Total cost of employeement before deductions (10 levels - ordinal)"),
        h5("Resrch - Research output (8 levels - ordinal)"),
        h5("Age - Age (5 levels - ordinal)"),
        h5("Rank - Academic position or rank (5 levels - ordinal)"),
        h5("AQual - Academic qualification (5 levels - ordinal)"),
        h5("Faclty - Faculty (9 levels - nominal)"),
        linebreaks(1),
        tableOutput("basicerror.renum"),
        linebreaks(1),
        h5(strong("Confusion matrix:")),
        tableOutput("confusion.renum")
      )
      
      
    ),
    
    mainPanel(helpText(" "),
                       tabsetPanel(id = "plotarea",
                         tabPanel("Biplot", plotOutput(
                           outputId = "main",
                           height = "700px"
                         )
                         ),
                         tabPanel("Accuracy Raw Output", helpText(" "), verbatimTextOutput("errormetrics"))
                         
                       ))
    
    
    
    
  )
)

server <- function(input, output,session) {
rv <<- reactiveValues()
er_rv <<- reactiveValues()


#iris inputs

order_cat <- lapply(iris[,3:4], function(x) {
  cut(x, breaks=5, include.lowest=TRUE,
      ordered=TRUE,labels = c("Very Small", "Small", "Average", "Large", "Very Large")) })

petal_ord<-data.frame(order_cat)

#--encoding

petal_temp <- petal_ord
petal_temp$Petal.Length <- as.numeric(as.factor(petal_temp$Petal.Length))
petal_temp$Petal.Width <- as.numeric(as.factor(petal_temp$Petal.Width))

iris_encoded <- cbind(iris[,1:2],petal_temp)


#wine inputs

wine_cat  <- lapply(wine[,2:5], function(x) {
  cut(x, breaks=3, include.lowest=TRUE,
      ordered=TRUE,labels = c( "Small", "Average","Large")) })
wine_cat<-data.frame(wine_cat)

#--encoding

wine_temp<- wine_cat
wine_temp$Alcohol<- as.numeric(as.factor(wine_temp$Alcohol))
wine_temp$Malic<- as.numeric(as.factor(wine_temp$Malic))
wine_temp$Ash<- as.numeric(as.factor(wine_temp$Ash))
wine_temp$Alcalinity<- as.numeric(as.factor(wine_temp$Alcalinity))

wine_encoded<-cbind(wine_temp,wine[,5:13])


#score inputs
Score_Train <- data.frame(score_data)

Score_Train_cat  <- lapply(Score_Train[,1:3], function(x) {
  cut(x, breaks=3, include.lowest=TRUE,
      ordered=TRUE,labels = c( "Low", "Average","High")) })

Score_Train_cat<-data.frame(Score_Train_cat)


#--encoding

Score_Train_Temp<-Score_Train_cat
Score_Train_Temp$STG<-as.numeric(as.factor(Score_Train_Temp$STG))
Score_Train_Temp$SCG<-as.numeric(as.factor(Score_Train_Temp$SCG))
Score_Train_Temp$STR<-as.numeric(as.factor(Score_Train_Temp$STR))

Score_Train_encoded <- cbind(Score_Train_Temp[,1:3],Score_Train[,4:5])

#wifi inputs

wifi_localization_cat  <- lapply(wifi_localization[,1:3], function(x) {
  cut(x, breaks=3, include.lowest=TRUE,
      ordered=TRUE,labels = c( "Small", "Average","Large")) })
wifi_localization_cat<-data.frame(wifi_localization_cat)

#--encoding

wifi_localization_temp<-wifi_localization_cat

wifi_localization_temp$V1 <- as.numeric(as.factor(wifi_localization_temp$V1))
wifi_localization_temp$V2 <- as.numeric(as.factor(wifi_localization_temp$V2))
wifi_localization_temp$V3 <- as.numeric(as.factor(wifi_localization_temp$V3))

wifi_localization_encoded <- cbind(wifi_localization_temp[,1:3],wifi_localization[,4:7])


#renum inputs

Remuneration_Temp <- Remuneration

Remuneration_Temp $Remun<-as.numeric(factor(Remuneration$Remun))
Remuneration_Temp $Resrch<-as.numeric(factor(Remuneration$Resrch))
Remuneration_Temp $Rank<-as.numeric(factor(Remuneration$Rank))
Remuneration_Temp $Age<-as.numeric(factor(Remuneration$Age))
Remuneration_Temp $AQual<-as.numeric(factor(Remuneration$AQual))
Remuneration_Temp $Faclty<-as.numeric(factor(Remuneration$Faclty))

Remuneration_encoded <- Remuneration_Temp[,c(3,4,5,6,9,10)]


#inputs for confusion statistics

class.num <<- NULL
groups.lda <<- NULL


#function which reads inputs and produces necessary plot

producePlot <-function(){
  renderPlot({
    bipltype.iris <<- input$bipltype.iris
    class.reg.choice.iris <<- input$classreg.iris
    hrank.iris <<-input$hrank.iris
    class.col.iris <<- NULL
    
    bipltype.wine <<- input$bipltype.wine
    class.reg.choice.wine <<- input$classreg.wine
    hrank.wine <<-input$hrank.wine
    class.col.wine <<- NULL
    
    bipltype.wifi <<- input$bipltype.wifi
    class.reg.choice.wifi <<- input$classreg.wifi
    hrank.wifi <<-input$hrank.wifi
    class.col.wifi <<- NULL
    
    bipltype.score <<- input$bipltype.score
    class.reg.choice.score <<- input$classreg.score
    hrank.score <<-input$hrank.score
    class.col.score <<- NULL
    
    bipltype.renum <<- input$bipltype.renum
    class.reg.choice.renum <<- input$classreg.renum
    hrank.renum <<-input$hrank.renum
    class.col.renum <<- NULL
    
    if(class.reg.choice.iris == TRUE){
      class.reg.choice.iris <<- "equal"
      class.col.iris <<-  c("#808080FF", "#BBBBBBFF" ,"#E6E6E6FF")
    }
    else{
      class.reg.choice.iris <<- NULL 
    }
    
    if(class.reg.choice.wine == TRUE){
      class.reg.choice.wine <<- "equal"
      class.col.wine <<-  c("#808080FF", "#BBBBBBFF" ,"#E6E6E6FF")
    }
    else{
      class.reg.choice.wine <<- NULL 
    }
    
    if(class.reg.choice.wifi == TRUE){
      class.reg.choice.wifi <<- "equal"
      class.col.wifi <<- c("#808080FF","#AAAAAAFF","#CACACAFF","#E6E6E6FF")
    }
    else{
      class.reg.choice.wifi <<- NULL 
    }
    
    if(class.reg.choice.score == TRUE){
      class.reg.choice.score <<- "equal"
      class.col.score <<- c("#808080FF","#AAAAAAFF","#CACACAFF","#E6E6E6FF")
    }
    else{
      class.reg.choice.score <<- NULL 
    }
    
    if(class.reg.choice.renum == TRUE){
      class.reg.choice.renum <<- "equal"
      class.col.renum <<-  c("#808080FF","#E6E6E6FF")
    }
    else{
      class.reg.choice.renum <<- NULL 
    }
    
    
    choice <<- input$headtab
    
    #-------- IRIS MAIN ------------
    
    
    if (choice == "IRIS") {
      class.num  <<- 3
      groups.lda <<- iris$Species
      if (bipltype.iris == "CVA En") {
        
        rv <<-CVAbiplot(
          X = iris_encoded,
          G = indmat(iris$Species),
          sample.pch = c(21, 22, 23),
          prior.p = class.reg.choice.iris,
          region.colours = class.col.iris
        )
        par(xpd=TRUE)
        legend("bottom", legend=c("Setosa","Versicolor","Virginica"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(3, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE)
      }
      
      
      if (bipltype.iris == "catPCA") {
        rv <<-
          CATPCAbiplot(
            X = petal_ord,
            Xcont = iris[, 1:2],
            G = indmat(iris$Species),
            sample.pch = c(21, 22, 23),
            prior.p = class.reg.choice.iris,
            factor.type = c('ord','ord'),
            region.colours = class.col.iris
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Setosa","Versicolor","Virginica"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(3, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
      if (bipltype.iris == "CVA H") {
        rv <<-
          CVA_H(
            X = petal_ord,
            Xcont = iris[, 1:2],
            G = indmat(iris$Species),
            sample.pch = c(21, 22, 23),
            prior.p = class.reg.choice.iris,
            h.rank = hrank.iris,
            factor.type = c('ord','ord'),
            region.colours = class.col.iris,
            ax.ordinal=list(reverse=c(T,T))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Setosa","Versicolor","Virginica"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(3, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
    }
    
    #------Wine Main ---------#
    
    if (choice == "Wine") {
      groups.lda <<- wine$Type
      class.num  <<- 3
      if (bipltype.wine == "CVA En") {
        
        
        rv <<-CVAbiplot(
          X = wine_encoded,
          G = indmat(wine$Type),
          sample.pch = c(21,22,23,24),
          prior.p = class.reg.choice.wine,
          region.colours = class.col.wine
        )
        
        par(xpd=TRUE)
        legend("bottom", legend=c("Cultivar 1","Cultivar 2","Cultivar 3"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(3, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
      }
      
      
      if (bipltype.wine == "catPCA") {
        rv <<-
          CATPCAbiplot(
            X = wine_cat,
            Xcont=wine[,6:14],
            G = indmat(wine$Type),
            sample.pch = c(21, 22, 23,24),
            prior.p = class.reg.choice.wine,
            factor.type = c('ord','ord','ord','ord'),
            region.colours = class.col.wine,
            ax.ordinal=list(reverse=c(T,F,T,F))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Cultivar 1","Cultivar 2","Cultivar 3"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(3, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
      if (bipltype.wine == "CVA H") {
        rv <<-
          CVA_H(
            X = wine_cat,
            Xcont=wine[,6:14],
            G = indmat(wine$Type),
            sample.pch = c(21, 22, 23,24),
            prior.p = class.reg.choice.wine,
            h.rank = hrank.wine,
            factor.type = c('ord','ord','ord','ord'),
            region.colours = class.col.wine,
            ax.ordinal=list(reverse=c(T,F,F,F))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Cultivar 1","Cultivar 2","Cultivar 3"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(3, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
    }
    #--------Score Main-------------#
    
    if (choice == "User Score") {
      
      groups.lda <<- Score_Train$UNS
      class.num  <<- 4
      
      if (bipltype.score == "CVA En") {
        
        rv <<-CVAbiplot(
          X = Score_Train_encoded,
          G = indmat(Score_Train$UNS),
          sample.pch = c(21, 22, 23,24),
          prior.p = class.reg.choice.score,
          region.colours = class.col.score
        )
        par(xpd=TRUE)
        legend("bottom", legend=c("Very Low","Low","Middle","High"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(4, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23,24) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
      }
      
      
      if (bipltype.score == "catPCA") {
        rv <<-
          CATPCAbiplot(
            X = Score_Train_cat,
            Xcont = Score_Train[,4:5],
            G = indmat(Score_Train$UNS),
            sample.pch = c(21, 22, 23,24),
            prior.p = class.reg.choice.score,
            factor.type = c('ord','ord','ord'),
            region.colours = class.col.score,
            ax.ordinal=list(reverse=c(T,T,F))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Very Low","Low","Middle","High"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(4, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23,24) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE)  
        
      }
      
      if (bipltype.score == "CVA H") {
        rv <<-
          CVA_H(
            X = Score_Train_cat,
            Xcont = Score_Train[,4:5],
            G = indmat(Score_Train$UNS),
            sample.pch = c(21, 22, 23,24),
            prior.p = class.reg.choice.score,
            h.rank = hrank.score,
            region.colours = class.col.score,
            factor.type = c('ord','ord','ord'),
            ax.ordinal=list(reverse=c(T,T,T))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Very Low","Low","Middle","High"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(4, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23,24) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
     
    }
    
    
    
    #---------Wifi Main--------------#
    
    if (choice == "Wifi") {
      class.num  <<- 4
      groups.lda <<- wifi_localization$V8
      
      if (bipltype.wifi == "CVA En") {
        
        
        rv <<-CVAbiplot(
          X = wifi_localization_encoded,
          G = indmat(wifi_localization$V8),
          sample.pch = c(21,22,23,24),
          prior.p = class.reg.choice.wifi,
          region.colours = class.col.wifi
        )
        par(xpd=TRUE)
        legend("bottom", legend=c("Location 1","Location 2","Location 3","Location 4"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(4, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23,24) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
      }
      
      
      if (bipltype.wifi == "catPCA") {
        rv <<-
          CATPCAbiplot(
            X = wifi_localization_cat,
            Xcont=wifi_localization[,4:7],
            G = indmat(wifi_localization$V8),
            sample.pch = c(21, 22, 23,24),
            prior.p = class.reg.choice.wifi,
            factor.type = c('ord','ord','ord'),
            region.colours = class.col.wifi,
            ax.ordinal=list(reverse=c(F,F,T))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Location 1","Location 2","Location 3","Location 4"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(4, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23,24) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
      if (bipltype.wifi == "CVA H") {
        rv <<-
          CVA_H(
            X = wifi_localization_cat,
            Xcont=wifi_localization[,4:7],
            G = indmat(wifi_localization$V8),
            sample.pch = c(21, 22, 23,24),
            prior.p = class.reg.choice.wifi,
            h.rank = hrank.wifi,
            factor.type = c('ord','ord','ord'),
            region.colours = class.col.wifi,
            ax.ordinal=list(reverse=c(F,T,T))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Location 1","Location 2","Location 3","Location 4"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(4, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22,23,24) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
     
    }
    
    
    #----------Remuneration Main------------
    
    if (choice == "Remuneration") {
      class.num  <<- 2
      groups.lda <<- Remuneration$Gender
      
      if (bipltype.renum == "CVA En") {

        rv <<-CVAbiplot(
          X = Remuneration_encoded,
          G = indmat(Remuneration$Gender),
          sample.pch = c(21,22),
          prior.p = class.reg.choice.renum,
          region.colours = class.col.renum
        )
        par(xpd=TRUE)
        legend("bottom", legend=c("Female","Male"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(2, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
      }
      
      
      if (bipltype.renum == "catPCA") {
        rv <<-
          CATPCAbiplot(
            X = Remuneration[,c(3,4,5,6,9,10)],
            G = indmat(Remuneration$Gender),
            sample.pch = c(21, 22),
            prior.p = class.reg.choice.renum,
            factor.type=c(rep("ord",5),"nom"),
            region.colours = class.col.renum,
            #ax.ordinal=list(reverse=c(T,T,T,T,F))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Female","Male"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(2, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
      if (bipltype.renum == "CVA H") {
        rv <<-
          CVA_H(
            X = Remuneration[,c(3,4,5,6,9,10)],
            G = indmat(Remuneration$Gender),
            sample.pch = c(21, 22),
            prior.p = class.reg.choice.renum,
            h.rank = hrank.renum,
            factor.type=c(rep("ord",5),"nom"),
            region.colours = class.col.renum,
            ax.ordinal=list(reverse=c(T,T,T,T,F))
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Female","Male"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex=0.85,pt.bg = gray.colors(2, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) ,pch=c(21,22) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
    }
    
    
  }
  )
  

}


#------End plotting function--------------#

basicMetrics <-function(){
  Accuracy.Metrics<-data.frame(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$overall[1]*100)
  Accuracy.Metrics<-data.frame(paste(round(Accuracy.Metrics,2),"%"))
  names(Accuracy.Metrics)<-"Accuracy:"
  return(Accuracy.Metrics)
}


#------------Output main display--------------

output$main <- producePlot()
output$errormetrics <- renderPrint({confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)})

output$confusion.iris <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
output$confusion.wine <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
output$confusion.score <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
output$confusion.wifi <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
output$confusion.renum <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)

output$basicerror <- renderTable({basicMetrics()})


#--------generateNew calculates all biplots and relevant tables to display--------#


generateNew <- function(){
  updateTabsetPanel(session,inputId = "plotarea",selected = "Biplot")
  
  producePlot()
  output$basicerror.iris <- renderTable({basicMetrics()})
  output$basicerror.wine <- renderTable({basicMetrics()})
  output$basicerror.score <- renderTable({basicMetrics()})
  output$basicerror.wifi <- renderTable({basicMetrics()})
  output$basicerror.renum <- renderTable({basicMetrics()})
  
  output$confusion.iris <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.wine <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.score <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.wifi <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.renum <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  
  output$errormetrics <- renderPrint({confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)})
}


#-------------If biplot option is changed----------------------

observeEvent(input$bipltype.iris,{
  generateNew()
})

observeEvent(input$bipltype.wine,{
  generateNew()
})

observeEvent(input$bipltype.wifi,{
  generateNew()
})

observeEvent(input$bipltype.score,{
  generateNew()
})

observeEvent(input$bipltype.renum,{
  generateNew()
})

#-------------If class regions is pressed ---------------------

observeEvent(input$classreg.iris,{
  generateNew()
})

observeEvent(input$classreg.wine,{
  generateNew()
})

observeEvent(input$classreg.wifi,{
  generateNew()
})

observeEvent(input$classreg.score,{
  generateNew()
})

observeEvent(input$classreg.renum,{
  generateNew()
})


#--------------If H slider is moved----------------------

observeEvent(input$hrank.iris,{
  generateNew()
})

observeEvent(input$hrank.wine,{
  generateNew()
})

observeEvent(input$hrank.wifi,{
  generateNew()
})

observeEvent(input$hrank.score,{
  generateNew()
})

observeEvent(input$hrank.renum,{
  generateNew()
})

#------------------If tab is changed---------------------

observeEvent(input$headtab,{
  generateNew()
})

observeEvent(input$plotarea,{
  producePlot()
  output$basicerror.iris <- renderTable({basicMetrics()})
  output$basicerror.wine <- renderTable({basicMetrics()})
  output$basicerror.score <- renderTable({basicMetrics()})
  output$basicerror.wifi <- renderTable({basicMetrics()})
  output$basicerror.renum <- renderTable({basicMetrics()})
  
  
  output$confusion.iris <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.wine <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.score <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.wifi <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  output$confusion.renum <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  
  output$errormetrics <- renderPrint({confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)})
})


  
}

shinyApp(ui, server)