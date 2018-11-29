library(shiny)

library(GNSplex)

shinyServer(function(input,output){

  ftype <- reactive({input$filetype})

  yint <- reactive({input$yin})

  mslp <- reactive({input$mslp})

  cint <- reactive({input$cint})


  file <- reactive({

    if(is.null(input$file)){

      infile <- NULL

    } else

    infile <- readGNS(input$file$datapath, type = ftype())

    x.initdata <- as.data.frame(slot(infile,"iData"))

    x.initdata

  })


  crep <- eventReactive(input$compute,{

    runif(input$file)

    isolate({

      if(!(is.null(file))){

        x.init <- file()

        x.data <- new("gnsdt",iData = x.init)

        x.crep <- CRepsd(x.data)

        x.rep <- as.data.frame(slot(x.crep,"cData"))

        x.sdns <- as.data.frame(slot(x.crep,"sdnsData"))

        x.sd <- as.data.frame(slot(x.crep,"sdData"))

        x.conc1 <- slot(x.crep,"conc")

        x.conc <- as.data.frame(x.conc1[(1:nrow(x.rep)),])

        x.rep <- as.data.frame(cbind(x.rep,x.sdns,x.sd, x.conc))
        names(x.rep) <- c("NI.crep","SI.crep","NI.sdns","SI.sdns","NI.sd","SI.sd","Concn")
        x.rep

      }

      else{

        return(NULL)

      }

    })

  })



  kd <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()

      nidata <- x.crep[,"NI.crep"]
      sd.ni <- x.crep[,"NI.sdns"]

      Concn.new <- x.crep[,"Concn"]

      ni.new <- as.data.frame(cbind(Concn.new,nidata,sd.ni))
      names(ni.new) <- c("x","y","sd")
      plot1 <- ggplot(data =ni.new, aes(x,y))+ ggtitle("Intensity vs Concentration")+
        geom_point(colour = "black", na.rm = TRUE)+
        geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+
        geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=.2,
                      position=position_dodge(0.05))+
        xlab("Concentration (nM)") +
        ylab("Normalized Intensity (cl/tl) [arbitrary unit]")+
        stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,
                                                      sep = "~~~")),
                     parse = TRUE)

      plot1

    }

  })

  kd_new <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      sidata <- x.crep[,"SI.crep"]
      Concn.new <- x.crep[,"Concn"]
      sd.si <- x.crep[,"SI.sdns"]
      si.new <- as.data.frame(cbind(Concn.new,sidata,sd.si))
      names(si.new) <- c("x","y","sd")
      plot1 <- ggplot(data =si.new, aes(x,y))+ ggtitle("Intensity vs Concentration")+
        geom_point(colour = "black", na.rm = TRUE)+
        geom_smooth(method = "lm", formula = y~x, se = FALSE, colour = "black")+
        geom_errorbar(aes(ymin= y-sd, ymax=y+sd), width=.2,
                      position=position_dodge(0.05))+
        xlab("Concentration (nM)") +
        ylab("Standardized Intensity (tl/cl) [arbitrary unit]")+
        stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label..,
                                                      sep = "~~~")),
                     parse = TRUE)
      plot1

    }

  })

  conf_new <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      nidata <- x.crep[,"NI.crep"]
      sidata <- x.crep[,"SI.crep"]
      si.new <- as.data.frame(cbind(nidata, sidata))
      colnames(si.new) <- c("NI","SI")
      x.data <- new("gnsdt",cData = si.new)
      conf1 <- gnsConf(x.data)
      conf2 <- as.data.frame(slot(conf1,"confData"))
      conf2
    }

  })

  cor_new <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      nidata <- x.crep[,"NI.crep"]
      sidata <- x.crep[,"SI.crep"]
      si.new <- as.data.frame(cbind(nidata, sidata))
      names(si.new) <- c("NI","SI")
      concn <- as.data.frame(x.crep[,"Concn"])
      colnames(concn) <- ("Conc")
      x.data <- new("gnsdt",cData = si.new, conc = concn)
      cor1 <- gnscor(x.data)
      cor2 <- as.data.frame(slot(cor1,"corData"))
      cor2
    }

  })

  lod_new <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      nidata <- x.crep[,"NI.crep"]
      sidata <- x.crep[,"SI.crep"]
      si.new <- as.data.frame(cbind(nidata, sidata))
      names(si.new) <- c("NI","SI")
      sd.ni <- x.crep[,"NI.sdns"]
      sd.si <- x.crep[,"SI.sdns"]
      sdnew <- as.data.frame(cbind(sd.ni,sd.si))
      names(sdnew) <- c("NI","SI")
      x.data <- new("gnsdt",cData = si.new, sdnsData = sdnew)
      lod1 <- gnsLOD(x.data)
      lod2 <- as.data.frame(slot(lod1,"lodData"))
      lod2
    }

  })

  lod_new2 <- reactive({

    if(is.null(crep())){

      return(NULL)

    }

    else{

      x.crep <- crep()
      nidata <- x.crep[,"NI.crep"]
      sidata <- x.crep[,"SI.crep"]
      si.new <- as.data.frame(cbind(nidata, sidata))
      names(si.new) <- c("NI","SI")
      sd.ni <- x.crep[,"NI.sdns"]
      sd.si <- x.crep[,"SI.sdns"]
      sdnew <- as.data.frame(cbind(sd.ni,sd.si))
      colnames(sdnew) <- c("NI","SI")
      x.data <- new("gnsdt",cData = si.new, sdnsData = sdnew)
      lod2 <- gnsLOD(x.data, LOB = TRUE)
      lod3 <- as.data.frame(slot(lod2,"lodData"))
      lod3
    }

  })

  concentration <- reactive({
    conc <- gnsConc(yint(),mslp(),cint())
    conc
  })

  output$tabset <- renderUI({

    if(is.null(input$file)) {

      tabPanel("No input detected")

    }

    else {

      tabsetPanel(

        tabPanel("Initial Data", tableOutput("file")),

        tabPanel("Combine replicates", tableOutput("crep")),

        tabPanel("NI_plot", plotOutput("kd", height = 600, width = 550)),

        tabPanel("SI_plot", plotOutput("kd_new", height = 600, width = 550)),

        tabPanel("Confidence Interval",tableOutput("conf_new")),

        tabPanel("Correlation", tableOutput("cor_new")),

        tabPanel("LOD_First Method", tableOutput("lod_new")),

        tabPanel("LOD_Second Method", tableOutput("lod_new2")),

        tabPanel("Concentration", tableOutput("concentration"))


      )

    }

  })



  output$file <- renderTable({

    x <- file()

    x

  })



  output$crep <- renderTable({

    x <- crep()

    x

  })


  output$kd <- renderPlot({kd()})

  output$kd_new <- renderPlot({kd_new()})

  output$conf_new <- renderTable({
    x <- conf_new()
    x
  })

  output$cor_new <- renderTable({
    x <- cor_new()
    x
  })

  output$lod_new <- renderTable({
    x <- lod_new()
    x
  })

  output$lod_new2 <- renderTable({
    x <- lod_new2()
    x
  })

  output$concentration <- renderTable({
    x <- concentration()
    x
  })


  output$download <- downloadHandler(

    filename  = "result_report.html",

    content <- function(file) {

      knitr:::knit(input = "result_report.Rmd",

                   output = "result_report.md", quiet = TRUE)

      markdown:::markdownToHTML("result_report.md", "result_report.html")

    }

  )



})
