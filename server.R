## server.R ## 
function(input, output, session) {
  # set ggplot2 default font size to 12
  ggplot2::theme_set(theme_classic(base_size = 14)) 

  # wrap string
  wrap_strings <- function(vector_of_strings,width) {
    new_stringvector <- gsub('/', ' / ', vector_of_strings)
    sapply(new_stringvector,FUN = function(x) {paste(strwrap(x,width=width), collapse="\n")})
  }
  
  # (3/26/18) change data table header
  changeTableHeader <- function(inTable, newHeader) {
    outTable = inTable
    colnames(outTable) <- newHeader
    return(outTable)
  }
  
  # funding colors
  fundColors = c("#800080","#FFD700")
  
  # (1/22/18 hack) convert time period
  timeConvert <- function(disp_time) {
    dfTime <- data.frame("key" = c('Graduated in 2000-2004',
                                   'Graduated in 2005-2009',
                                   'Graduated in 2010-2014',
                                   'Graduated in 2000-2014'), 
                         "value" = c('2000-2004',
                                     '2005-2009',
                                     '2010-2014',
                                     '2000-2014'))
    result <- as.character(dfTime$value[dfTime$key == disp_time])
    return(result)
  }
  
  # count of alumni
  alnCount = nrow(dataGroup)
  
  # coverage
  cUKnwn = table(dataGroup[,"job_sector"])
  
  if('Unknown' %in% names(cUKnwn)) {
    covRate = paste0(round(1 - cUKnwn["Unknown"] / alnCount, 3) * 100, "%")
  }
  else {
    covRate = '100%'
  }
  
  
  
  # count job categories
  cntSect = 6
  cntType = 6
  cntFunc = 26

  output$years <- renderValueBox({
    valueBox(
      value = totalYrs,
      subtitle = "Students graduated in this time span",
      icon = icon("calendar")
    )
  })
  
  output$total <- renderValueBox({
    # Total number of postdoc alumni.
    valueBox(
      value = alnCount,
      subtitle = "Total alumni",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$coverage <- renderValueBox({
    valueBox(
      value = covRate,
      subtitle = "Alumni with known outcome snapshots",
      icon = icon("pie-chart"),
      color = "yellow"
    )
  })
  
  output$jobSect <- renderValueBox({
    # number of job sector
    valueBox(
      subtitle = "Sector definitions",
      value = cntSect,
      icon = icon("th-large"),
      color = "maroon"
    )
  })

  output$jobType <- renderValueBox({
    # number of career type
    valueBox(
      subtitle = "Career type definitions",
      value = cntType,
      icon = icon("th-list"),
      color = "green"
    )
  })

  output$jobFunc <- renderValueBox({
    # number of job function
    valueBox(
      subtitle = "Job function definitions",
      value = cntFunc,
      icon = icon("th"),
      color = "blue"
    )
  })
  
  output$jobExample <- renderValueBox({
    # number of job function
    valueBox(
      subtitle = "Job classification examples",
      value = 30,
      icon = icon("table"),
      color = "yellow"
    )
  })

  ##### https://stackoverflow.com/questions/32971921/navigate-to-particular-sidebar-menu-item-in-shinydashboard
  ##### https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard
  
  # # dynamic input in sidebar
  # output$selectInput <- renderUI({
  #   if(input$tabs == "demograph") {
  #     menuItem("Select setting", startExpanded = TRUE,
  #       menuSubItem(# select time period
  #         selectInput("selectDmTp", label = "Time period",
  #                     choices = c(timePeriods,"Total","Trend"))),
  #       menuSubItem(# select plot style
  #         selectInput("selectDmPc", label = "Plot category",
  #                     choices = list("By job sector"=1, "By career type"=2, 
  #                                    "By job function"=3), selected = 1))
  #     )
  #   }
  # })
  
  #############################################################################
  # Demographics >>>
  #############################################################################

  # demographics data
  dmHeight <- function() {
    input$sldHeightDm
  }
  dmWidth <- function() {
    input$sldWidthDm
  }
  
  # likert plot help function
  likertHelper <- function(dataAll, dataCat, inType, titleAll, titleCat, inColors) {
    # convert data format for likert data
    pctAll <- round(dataAll/rowSums(dataAll)*100,2)
    pctCat <- round(dataCat[-1]/rowSums(dataCat[-1])*100,2)
    ldAll <- data.frame(Item = c(' '), pctAll)
    ldCat <- data.frame(dataCat[,1], pctCat)
    colnames(ldCat)[1] <- "Item"
    dcAll = data.frame(Item = ' ', Count = rowSums(dataAll))
    dcCat = data.frame(ldCat[,1], data.frame(rowSums(dataCat[-1])))
    colnames(dcCat)[1] <- "Item"
    colnames(dcCat)[2] <- "Count"
    
    # likert plot
    ltAll <- likert(summary=ldAll)
    p1 <- plot(ltAll, colors=inColors) + ggtitle(titleAll) + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12,face="bold"))
    p1 <- p1 + guides(fill=guide_legend(title=inType)) + geom_label(data=dcAll, aes(x=Item, y = 1, label=Count))

    ltCat <- likert(summary=ldCat)
    p2 <- plot(ltCat, colors=inColors, ordered = FALSE) + ggtitle(titleCat) + theme(plot.title = element_text(hjust = 0.5), axis.text=element_text(size=12,face="bold"))
    p2 <- p2 + guides(fill=guide_legend(title=inType)) + geom_label(data=dcCat, aes(x=Item, y = 1, label=Count))

    grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
  }
  
  # gender colors
  genderColors = c("#D81B60", "#4F94CD")
  
  # plot gender data
  output$genderLtPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Graduated in 2000-2014') {
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Gender distribution in all data \n", totalYrs)
        titleCat = paste0("Gender distribution in each job sector \n", totalYrs)
        likertHelper(gend4All, gend4Sect, 'Gender', titleAll, titleCat, genderColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Gender distribution in all data \n", totalYrs)
        titleCat = paste0("Gender distribution in each career type \n", totalYrs)
        likertHelper(gend4All, gend4Type, 'Gender', titleAll, titleCat, genderColors)
      }
      else {
        titleAll = paste0("Gender distribution in all data \n", totalYrs)
        titleCat = paste0("Gender distribution in each job function \n", totalYrs)
        likertHelper(gend4All, gend4Func, 'Gender', titleAll, titleCat, genderColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      # if (input$selectDmPc == 1) {
      #   p1 <- likert(years ~ ., gendYrAll, as.percent=TRUE, main=paste0("Gender distribution in data \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_sector, gendYrSect, as.percent=TRUE, main=paste0("Gender distribution in each job sector \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else if (input$selectDmPc == 2) {
      #   p1 <- likert(years ~ ., gendYrAll, as.percent=TRUE, main=paste0("Gender distribution in data \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | career_type, gendYrType, as.percent=TRUE, main=paste0("Gender distribution in each career type \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else {
      #   p1 <- likert(years ~ ., gendYrAll, as.percent=TRUE, main=paste0("Gender distribution in data \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_function, gendYrFunc, as.percent=TRUE, main=paste0("Gender distribution in each job function \n", trendYrs), col=genderColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltGndAll <- subset(gendYrAll, years == selectYears)
      pltGndAll$years <- NULL
      pltGndSec <- subset(gendYrSect, years == selectYears)
      pltGndSec$years <- NULL
      pltGndTyp <- subset(gendYrType, years == selectYears)
      pltGndTyp$years <- NULL
      pltGndSpe <- subset(gendYrFunc, years == selectYears)
      pltGndSpe$years <- NULL
      
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Gender distribution in all data \n", selectYears)
        titleCat = paste0("Gender distribution in each job sector \n", selectYears)
        likertHelper(pltGndAll, pltGndSec, 'Gender', titleAll, titleCat, genderColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Gender distribution in all data \n", selectYears)
        titleCat = paste0("Gender distribution in each career type \n", selectYears)
        likertHelper(pltGndAll, pltGndTyp, 'Gender', titleAll, titleCat, genderColors)
      }
      else {
        titleAll = paste0("Gender distribution in all data \n", selectYears)
        titleCat = paste0("Gender distribution in each job function \n", selectYears)
        likertHelper(pltGndAll, pltGndSpe, 'Gender', titleAll, titleCat, genderColors)
      }
    }
  },height=dmHeight,width=dmWidth)
  
  output$genderLtTxt  <- renderText({HTML("<p><strong>Likert Plot of Gender Distribution.</strong> <em>Upper panel</em>: Relative percentages of male and female alumni in the selected time period(s).
                                          <em>Lower panel</em>: Relative percentages of male and female alumni in each job category during selected time period(s). 
                                          The sum of male and female percentages in each row equals 100%. The number in the middle of each row indicates total number of alumni in a job category.</p><p>&nbsp;</p>")})

  output$genderPbPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Graduated in 2000-2014') {
      if (input$selectDmPc == 1) {
        gend4Sect.m <- melt(gend4Sect, id.vars = "job_sector")
        colnames(gend4Sect.m)[colnames(gend4Sect.m)=="variable"] <- "gender"
        p <- ggplot(gend4Sect.m, aes(job_sector, value)) + labs(x="Employment Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else if (input$selectDmPc == 2) {
        gend4Type.m <- melt(gend4Type, id.vars = "career_type")
        colnames(gend4Type.m)[colnames(gend4Type.m)=="variable"] <- "gender"
        p <- ggplot(gend4Type.m, aes(career_type, value)) + labs(x="Caeer Type", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else {
        # gdr4Func <- gend4Func[grepl('research', gend4Func$job_function),]
        gdr4Func <- gend4Func
        gend4Func.m <- melt(gdr4Func, id.vars = "job_function")
        colnames(gend4Func.m)[colnames(gend4Func.m)=="variable"] <- "gender"
        p <- ggplot(gend4Func.m, aes(job_function, value)) + labs(x="Job Function", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
        gdrYrSect.m <- melt(gendYrSect, id.vars = c("job_sector","years"))
        colnames(gdrYrSect.m)[colnames(gdrYrSect.m)=="variable"] <- "gender"
        p <- ggplot(gdrYrSect.m, aes(job_sector, value)) + labs(x="Employment Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=genderColors)
      }
      else if (input$selectDmPc == 2) {
        gdrYrType.m <- melt(gendYrType, id.vars = c("career_type","years"))
        colnames(gdrYrType.m)[colnames(gdrYrType.m)=="variable"] <- "gender"
        p <- ggplot(gdrYrType.m, aes(career_type, value)) + labs(x="Caeer Type", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=genderColors)
      }
      else {
        gdrYrFunc.m <- melt(gendYrFunc, id.vars = c("job_function","years"))
        colnames(gdrYrFunc.m)[colnames(gdrYrFunc.m)=="variable"] <- "gender"
        p <- ggplot(gdrYrFunc.m, aes(job_function, value)) + labs(x="Job Function", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=genderColors)
      }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltGndAll <- subset(gendYrAll, years == selectYears)
      pltGndSec <- subset(gendYrSect, years == selectYears)
      pltGndTyp <- subset(gendYrType, years == selectYears)
      pltGndSpe <- subset(gendYrFunc, years == selectYears)
      
      # plot
      if (input$selectDmPc == 1) {
        gdrPlSect.m <- melt(pltGndSec, id.vars = c("job_sector","years"))
        colnames(gdrPlSect.m)[colnames(gdrPlSect.m)=="variable"] <- "gender"
        p <- ggplot(gdrPlSect.m, aes(job_sector, value)) + labs(x="Employment Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else if (input$selectDmPc == 2) {
        gdrPlType.m <- melt(pltGndTyp, id.vars = c("career_type","years"))
        colnames(gdrPlType.m)[colnames(gdrPlType.m)=="variable"] <- "gender"
        p <- ggplot(gdrPlType.m, aes(career_type, value)) + labs(x="Caeer Type", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
      else {
        gdrPlFunc.m <- melt(pltGndSpe, id.vars = c("job_function","years"))
        colnames(gdrPlFunc.m)[colnames(gdrPlFunc.m)=="variable"] <- "gender"
        p <- ggplot(gdrPlFunc.m, aes(job_function, value)) + labs(x="Job Function", y="Number of Alumni") + guides(fill=guide_legend(title="Gender"))
        p <- p + geom_bar(aes(fill = gender), position = "dodge", stat="identity")
        p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=genderColors)
      }
    }
    p
  },height=dmHeight,width=dmWidth)
  output$genderPbTxt  <- renderText({HTML("<p><strong>Bar Chart of Gender Distribution.</strong> Absolute number of male and female alumni in each job category during selected time period(s).</p><p>&nbsp;</p>")})
  
  # output gender table
  output$genderTable <- renderTable({
    # choose data based on input$selectGt from ui.R
    if (input$selectDmTp == 'Graduated in 2000-2014') {
      if (input$selectDmPc == 1) {
        changeTableHeader(gend4Sect, c('Job_Sector','Female','Male'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(gend4Type, c('Career_Type','Female','Male'))
      }
      else {
        changeTableHeader(gend4Func, c('Job_Function','Female','Male'))
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
        changeTableHeader(gendYrSect, c('Years','Job_Sector','Female','Male'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(gendYrType, c('Years','Career_Type','Female','Male'))
      }
      else {
        changeTableHeader(gendYrFunc, c('Years','Job_Function','Female','Male'))
      }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltGndSec <- subset(gendYrSect, years == selectYears)
      pltGndTyp <- subset(gendYrType, years == selectYears)
      pltGndSpe <- subset(gendYrFunc, years == selectYears)
      if (input$selectDmPc == 1) {
        changeTableHeader(pltGndSec, c('Years','Job_Sector','Female','Male'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(pltGndTyp, c('Years','Career_Type','Female','Male'))
      }
      else {
        changeTableHeader(pltGndSpe, c('Years','Job_Function','Female','Male'))
      }
    }
  }, digits = 0)
  
  
  # plot citizenship data
  # visiting colors
  visitColors = c("#47427e","#6cc06d")
  
  output$visitLtPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Graduated in 2000-2014') {
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Country origin distribution in all data \n", totalYrs)
        titleCat = paste0("Country origin distribution in each job sector \n", totalYrs)
        likertHelper(citi4All, citi4Sect, 'Country', titleAll, titleCat, visitColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Country origin distribution in all data \n", totalYrs)
        titleCat = paste0("Country origin distribution in each career type \n", totalYrs)
        likertHelper(citi4All, citi4Type, 'Country', titleAll, titleCat, visitColors)
      }
      else {
        titleAll = paste0("Country origin distribution in all data \n", totalYrs)
        titleCat = paste0("Country origin distribution in each job function \n", totalYrs)
        likertHelper(citi4All, citi4Func, 'Country', titleAll, titleCat, visitColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      # if (input$selectDmPc == 1) {
      #   p1 <- likert(years ~ ., citiYrAll, as.percent=TRUE, main=paste0("Country origin distribution in data \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_sector, citiYrSect, as.percent=TRUE, main=paste0("Country origin distribution in each job sector \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else if (input$selectDmPc == 2) {
      #   p1 <- likert(years ~ ., citiYrAll, as.percent=TRUE, main=paste0("Country origin distribution in data \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | career_type, citiYrType, as.percent=TRUE, main=paste0("Country origin distribution in each career type \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
      # else {
      #   p1 <- likert(years ~ ., citiYrAll, as.percent=TRUE, main=paste0("Country origin distribution in data \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   p2 <- likert(years ~ . | job_function, citiYrFunc, as.percent=TRUE, main=paste0("Country origin distribution in each job specific \n", trendYrs), col=visitColors, ylab="Years", auto.key.in = list(cex=1.3), scales = list(cex = 1.1), ylab.right = 'Number of Alumni')
      #   grid.arrange(p1, p2, nrow=2, heights=c(3, 7))
      # }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltCtzAll <- subset(citiYrAll, years == selectYears)
      pltCtzAll$years <- NULL
      pltCtzSec <- subset(citiYrSect, years == selectYears)
      pltCtzSec$years <- NULL
      pltCtzTyp <- subset(citiYrType, years == selectYears)
      pltCtzTyp$years <- NULL
      pltCtzSpe <- subset(citiYrFunc, years == selectYears)
      pltCtzSpe$years <- NULL
      
      # change for Likert plot (3/15/18)
      if (input$selectDmPc == 1) {
        titleAll = paste0("Country origin distribution in all data \n", selectYears)
        titleCat = paste0("Country origin distribution in each job sector \n", selectYears)
        likertHelper(pltCtzAll, pltCtzSec, 'Country', titleAll, titleCat, visitColors)
      }
      else if (input$selectDmPc == 2) {
        titleAll = paste0("Country origin distribution in all data \n", selectYears)
        titleCat = paste0("Country origin distribution in each career type \n", selectYears)
        likertHelper(pltCtzAll, pltCtzTyp, 'Country', titleAll, titleCat, visitColors)
      }
      else {
        titleAll = paste0("Country origin distribution in all data \n", selectYears)
        titleCat = paste0("Country origin distribution in each job function \n", selectYears)
        likertHelper(pltCtzAll, pltCtzSpe, 'Country', titleAll, titleCat, visitColors)
      }
    }
  },height=dmHeight,width=dmWidth)
  
  output$visitLtTxt  <- renderText({HTML("<p><strong>Likert Plot of Country Origin Distribution.</strong> <em>Upper panel</em>: Relative percentages of US and international alumni in the selected time period(s).
                                          <em>Lower panel</em>: Relative percentages of US and international alumni in each job category during selected time period(s). 
                                          The sum of US and international percentages in each row equals 100%. The number in the middle of each row indicates total number of alumni in a job category.</p><p>&nbsp;</p>")})
  
  output$visitPbPlot <- renderPlot({
    # choose data based on input$selectDmTp from ui.R
    if (input$selectDmTp == 'Graduated in 2000-2014') {
      if (input$selectDmPc == 1) {
          citi4Sect.m <- melt(citi4Sect, id.vars = "job_sector")
          colnames(citi4Sect.m)[colnames(citi4Sect.m)=="variable"] <- "visiting"
          p <- ggplot(citi4Sect.m, aes(job_sector, value)) + labs(x="Employment Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else if (input$selectDmPc == 2) {
          citi4Type.m <- melt(citi4Type, id.vars = "career_type")
          colnames(citi4Type.m)[colnames(citi4Type.m)=="variable"] <- "visiting"
          p <- ggplot(citi4Type.m, aes(career_type, value)) + labs(x="Caeer Type", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else {
          citi4Func.m <- melt(citi4Func, id.vars = "job_function")
          colnames(citi4Func.m)[colnames(citi4Func.m)=="variable"] <- "visiting"
          p <- ggplot(citi4Func.m, aes(job_function, value)) + labs(x="Job Function", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
          vnvYrSect.m <- melt(citiYrSect, id.vars = c("job_sector","years"))
          colnames(vnvYrSect.m)[colnames(vnvYrSect.m)=="variable"] <- "visiting"
          p <- ggplot(vnvYrSect.m, aes(job_sector, value)) + labs(x="Employment Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=visitColors)
      }
      else if (input$selectDmPc == 2) {
          vnvYrType.m <- melt(citiYrType, id.vars = c("career_type","years"))
          colnames(vnvYrType.m)[colnames(vnvYrType.m)=="variable"] <- "visiting"
          p <- ggplot(vnvYrType.m, aes(career_type, value)) + labs(x="Caeer Type", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=visitColors)
      }
      else {
          vnvYrFunc.m <- melt(citiYrFunc, id.vars = c("job_function","years"))
          colnames(vnvYrFunc.m)[colnames(vnvYrFunc.m)=="variable"] <- "visiting"
          p <- ggplot(vnvYrFunc.m, aes(job_function, value)) + labs(x="Job Function", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + facet_grid( ~ years) + scale_fill_manual(values=visitColors)
      }
    }
    else {
      selectYears = timeConvert(input$selectDmTp)
      pltCtzAll <- subset(citiYrAll, years == selectYears)
      pltCtzSec <- subset(citiYrSect, years == selectYears)
      pltCtzTyp <- subset(citiYrType, years == selectYears)
      pltCtzSpe <- subset(citiYrFunc, years == selectYears)
      
      # plot
      if (input$selectDmPc == 1) {
          vnvPlSect.m <- melt(pltCtzSec, id.vars = c("job_sector","years"))
          colnames(vnvPlSect.m)[colnames(vnvPlSect.m)=="variable"] <- "visiting"
          p <- ggplot(vnvPlSect.m, aes(job_sector, value)) + labs(x="Employment Sector", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else if (input$selectDmPc == 2) {
          vnvPlType.m <- melt(pltCtzTyp, id.vars = c("career_type","years"))
          colnames(vnvPlType.m)[colnames(vnvPlType.m)=="variable"] <- "visiting"
          p <- ggplot(vnvPlType.m, aes(career_type, value)) + labs(x="Caeer Type", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
      else {
          vnvPlFunc.m <- melt(pltCtzSpe, id.vars = c("job_function","years"))
          colnames(vnvPlFunc.m)[colnames(vnvPlFunc.m)=="variable"] <- "visiting"
          p <- ggplot(vnvPlFunc.m, aes(job_function, value)) + labs(x="Job Function", y="Number of Alumni") + guides(fill=guide_legend(title="Country\n origin"))
          p <- p + geom_bar(aes(fill = visiting), position = "dodge", stat="identity")
          p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=visitColors)
      }
    }
    p
  },height=dmHeight,width=dmWidth)
  
  output$visitPbTxt  <- renderText({HTML("<p><strong>Bar Chart of Country Origin Distribution.</strong> Absolute number of US and international alumni in each job category during selected time period(s).</p><p>&nbsp;</p>")})
  
  # output citizen table
  output$visitTable <- renderTable({
    # choose data based on input$selectGt from ui.R
    if (input$selectDmTp == 'Graduated in 2000-2014') {
      if (input$selectDmPc == 1) {
        changeTableHeader(citi4Sect, c('Job_Sector','International','US'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(citi4Type, c('Career_Type','International','US'))
      }
      else {
        changeTableHeader(citi4Func, c('Job_Function','International','US'))
      }
    }
    else if (input$selectDmTp == 'Trend') {
      if (input$selectDmPc == 1) {
        changeTableHeader(citiYrSect, c('Years','Job_Sector','International','US'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(citiYrType, c('Years','Career_Type','International','US'))
      }
      else {
        changeTableHeader(citiYrFunc, c('Years','Job_Function','International','US'))
      }
    }
    else {
      pltCtzSec <- subset(citiYrSect, years == timeConvert(input$selectDmTp))
      pltCtzTyp <- subset(citiYrType, years == timeConvert(input$selectDmTp))
      pltCtzSpe <- subset(citiYrFunc, years == timeConvert(input$selectDmTp))
      if (input$selectDmPc == 1) {
        changeTableHeader(pltCtzSec, c('Years','Job_Sector','International','US'))
      }
      else if (input$selectDmPc == 2) {
        changeTableHeader(pltCtzTyp, c('Years','Career_Type','International','US'))
      }
      else {
        changeTableHeader(pltCtzSpe, c('Years','Job_Function','International','US'))
      }
    }
  }, digits = 0)
  
  #>>>
  output$demoDynamicUI<-renderUI({
    # zoom gender plot
    if (input$selectShowDm == 1) {
      fluidRow(
        tabBox(
          title = "Gender", width = 12,
          # The id lets us use input$tabset3 on the server to find the current tab
          id = "tab_gender",
          tabPanel("Gender Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderLtTxt"), plotOutput("genderLtPlot")),
          tabPanel("Gender bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderPbTxt"), plotOutput("genderPbPlot")),
          tabPanel("Gender data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("genderTable"))
        )
      )
    }
    # zoom visiting plot
    else if (input$selectShowDm == 3) {
      fluidRow(
        tabBox(
          title = "Country origin", width = 12,
          # The id lets us use input$tabset4 on the server to find the current tab
          id = "tab_visit",
          tabPanel("Origin Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitLtTxt"), plotOutput("visitLtPlot")),
          tabPanel("Origin bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitPbTxt"), plotOutput("visitPbPlot")),
          tabPanel("Origin data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("visitTable"))
        )
      )
    }
    # default to show both
    else {
      fluidRow(
        tabBox(
          title = "Gender",
          # The id lets us use input$tab_genderSml on the server to find the current tab
          id = "tab_genderSml",
          tabPanel("Gender Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderLtTxt"), plotOutput("genderLtPlot")),
          tabPanel("Gender bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("genderPbTxt"), plotOutput("genderPbPlot")),
          tabPanel("Gender data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("genderTable"))
        ),
        tabBox(
          title = "Country origin",
          # The id lets us use input$tab_visitSml on the server to find the current tab
          id = "tab_visitSml",
          tabPanel("Origin Likert plot", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitLtTxt"), plotOutput("visitLtPlot")),
          tabPanel("Origin bar chart", style = "overflow-x:scroll; overflow-y:scroll; height: 800px", htmlOutput("visitPbTxt"), plotOutput("visitPbPlot")),
          tabPanel("Origin data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 800px", tableOutput("visitTable"))
        )
      )
    }
  })
  #<<<

  # demographics information boxes
  output$demoInfoBox<-renderUI({
    if (input$selectDmTp == 'Trend') {
      gendData = gendYrAll
      citiData = citiYrAll
      
      # https://stackoverflow.com/questions/15059076/call-apply-like-function-on-each-row-of-dataframe-with-multiple-arguments-from-e
      gendData.m <- melt(gendData, id.vars = "years")
      gendData.nm <- gendData.m %>% group_by(years) %>% mutate(Percent=round(value/sum(value),3)*100)
      femalePct = paste0(gendData.nm[gendData.nm$variable == "Female",]$Percent,"%")
      malePct = paste0(gendData.nm[gendData.nm$variable == "Male",]$Percent,"%")

      citiData.m <- melt(citiData, id.vars = "years")
      citiData.nm <- citiData.m %>% group_by(years) %>% mutate(Percent=round(value/sum(value),3)*100)
      visitPct = paste0(citiData.nm[citiData.nm$variable == "International",]$Percent,"%")
      usaPct = paste0(citiData.nm[citiData.nm$variable == "US",]$Percent,"%")
      
      box(
        width = 12,
        fluidRow(
          infoBox("Female change", paste(femalePct, collapse = " -> "), icon = icon("female"), color = "maroon", fill=TRUE, width=3),
          infoBox("Male change", paste(malePct, collapse = " -> "), icon = icon("male"), color = "blue", fill=TRUE, width=3),
          infoBox("International alumni change", paste(visitPct, collapse = " -> "), icon = icon("globe"), color = "purple", fill=TRUE, width=3),
          infoBox("US alumni change", paste(usaPct, collapse = " -> "), icon = icon("home"), color = "green", fill=TRUE, width=3)
        )
      )
    }
    else {
      if (input$selectDmTp == 'Graduated in 2000-2014') {
        gendData = gend4All
        citiData = citi4All
      }
      else {
        gendData = subset(gendYrAll, years == timeConvert(input$selectDmTp))
        citiData = subset(citiYrAll, years == timeConvert(input$selectDmTp))
      }
      malePct = as.numeric(gendData[,"Male"] / (gendData[,"Male"] + gendData[,"Female"]))
      femalePct = 1 - malePct
      visitPct = as.numeric(citiData[,"International"] / (citiData[,"International"] + citiData[,"US"]))
      usaPct = 1 - visitPct
      
      box(
        width = 12,
        fluidRow(
          infoBox("Female", paste0(round(femalePct, 3)*100, "%"), icon = icon("female"), color = "maroon", fill=TRUE, width=3),
          infoBox("Male", paste0(round(malePct, 3)*100, "%"), icon = icon("male"), color = "blue", fill=TRUE, width=3),
          infoBox("Int'l alumni", paste0(round(visitPct, 3)*100, "%"), icon = icon("globe"), color = "purple", fill=TRUE, width=3),
          infoBox("US alumni", paste0(round(usaPct, 3)*100, "%"), icon = icon("home"), color = "green", fill=TRUE, width=3)
        )
      )
    }
  })
  
  
  #############################################################################
  # Location >>>
  #############################################################################
  
  
  # location data
  lcHeight <- function() {
    input$sldHeightLc
  }
  lcWidth <- function() {
    input$sldWidthLc
  }
  
  # plot migration data
  output$migratePlot <- renderPlot({
    # colors
    myColors <- c(brewer.pal(8,"Dark2"),brewer.pal(12,"Paired"),brewer.pal(12,"Set3"))
    
    # generate df1 for plotting
    createDF1 <- function(iM) {
      #### log2_count <- log2(rowSums(iM)+colSums(iM))
      mTRX <- iM
      
      countD10 <- (rowSums(mTRX)+colSums(mTRX)) / 10.00
      countD10 <- countD10[order(countD10, decreasing = TRUE)]
      srtMTRX <- data.frame(countD10)
      colnames(srtMTRX)[colnames(srtMTRX)=="countD10"] <- "max"
      srtMTRX$country <- rownames(srtMTRX)
      
      nMTRX <- nrow(srtMTRX)
      srtMTRX$col <- myColors[1:nMTRX]
      return(srtMTRX)
    }
    
    # circular plot
    createCirc <- function(iM, iDF) {
      # plot
      circos.clear()
      par(mar = rep(0, 4), cex=0.95)
      circos.par(start.degree = 90, gap.degree = 4)
      p <- chordDiagram(x = iM, directional = TRUE, order = iDF$country, grid.col = iDF$col, annotationTrack = "grid", transparency = 0.25,  preAllocateTracks=list(track.height = 0.33), direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE )
      # add in labels and axis
      circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim") #
        sector.index = get.cell.meta.data("sector.index")
        # circos.axis("top", labels.away.percentage = 0.1, labels.niceFacing = TRUE)
        circos.text(mean(xlim), ylim[1], sector.index, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
      }, bg.border = NA)
      circos.clear()
    }
    
    if (input$selectLcTp == 'Graduated in 2000-2014') {
      migrate <- makeCircMD(mgrCountryAll)
    }
    else {
      dct <- mgrCountryGroup[mgrCountryGroup$years == timeConvert(input$selectLcTp), ]
      migrate <- makeCircMD(dct)
    }
    
    dfDF1 <- createDF1(migrate)
    createCirc(migrate, dfDF1)
    # })
  },height=lcHeight,width=lcWidth)
  
  output$migrateTxt  <- renderText({
    slctYear <- timeConvert(input$selectLcTp)
    if (input$selectLcTp == 'Graduated in 2000-2014') {
      migrData = dataGroup
    }
    else {
      migrData = subset(dataGroup, years == slctYear)
    }
    
    talMigr = nrow(migrData)
    cntUS = round(sum(migrData$job_country == 'United States') / talMigr, 3) * 100
    
    HTML(paste0("<p><strong>Visualizing Alumni Migration by Pairing Country/Continent of Origin with Country/Continent of Job Location</strong> 
         This circular plot displays the countries/continents of origin alongside the countries/continents of job location (arrows point to job locations). In ",
         slctYear, ", ", cntUS, "% of all alumni remain in the U.S. after training. 
         *=North American countries excluding U.S. & Canada; **=European countries excluding France; 
         ***=Asian countries excluding China, India, Sri Lanka, South Korea, Taiwan, & Nepal. 
         Individual countries with enough alumni to visualize are shown in titlecase. Alumni from remaining countries are grouped and depicted by continent in all caps within enclosed brackets.</p><p>&nbsp;</p>"))
  })
  
  # output migration table
  output$migrateTable <- renderTable({
    if (input$selectLcTp == 'Graduated in 2000-2014') {
      migrate <- makeCircMD(mgrCountryAll)
    }
    else {
      dct <- mgrCountryGroup[mgrCountryGroup$years == timeConvert(input$selectLcTp), ]
      migrate <- makeCircMD(dct)
    }
    migrate_df <- as.data.frame(migrate)
    setDT(migrate_df, keep.rownames = TRUE)[]
  })
  
  # http://www.arilamstein.com/blog/2015/07/02/exploring-the-demographics-of-ferguson-missouri/
  highlight_state = function(states) {
    data(state.map, package="choroplethrMaps", envir=environment())
    df = state.map[state.map$region %in% states, ]
    geom_polygon(data=df, aes(long, lat, group = group), color = "red", fill = NA, size = 1)
  }
  
  # plot working state data
  output$statePlot <- renderPlot({
    # choose data based on input$selectLcTp from ui.R
    slctYear <- timeConvert(input$selectLcTp)
    tlYear <- slctYear
    if (input$selectLcTp == 'Graduated in 2000-2014') {
      stData <- dfStateAll
      tlYear <- totalYrs
    }
    else {
      stData <- dfStateYrs[dfStateYrs$years == slctYear, ]
    }
    
    # Setting breakpoints for data
    maxValue <- max(stData$value) # max state value
    maxState <- as.character(stData[which.max(stData$value),]$region)
    minValue <- min(stData$value[stData$value > 0]) # find min positive value
    nV <- length(stData$value)
    s2Value <- sort(stData$value,partial=nV-1)[nV-1]
    
    # http://stackoverflow.com/questions/5746544/r-cut-by-defined-interval
    # https://github.com/trulia/choroplethr/wiki/Choosing-a-Scale-Type
    sclData <- levels(cut(c(minValue, s2Value), 4))
    nBrks <- c(0, ceiling( as.numeric(sub("\\((.+),.*", "\\1", sclData)) ), s2Value, maxValue, maxValue)
    stData$value = Hmisc::cut2(stData$value, cuts=nBrks)
    
    sTitle <- paste0('Alumni Job Location in the United States (', tlYear, ')')
    p <- state_choropleth(stData, title = sTitle, legend = "Number of\nalumni") + theme(axis.line=element_blank())
    p <- p + highlight_state(maxState)
    p
    # })
  },height=lcHeight,width=lcWidth)
  
  output$stateTxt  <- renderText({
    slctYear <- timeConvert(input$selectLcTp)
    if (input$selectLcTp == 'Graduated in 2000-2014') {
      migrData = dataGroup
    }
    else {
      migrData = subset(dataGroup, years == slctYear)
    }
    
    talMigr = nrow(migrData)
    cntST = sum(migrData$job_state == 'Michigan')
    talUS = sum(migrData$job_country == 'United States')
    pctST = round(cntST / talMigr, 3) * 100
    pctSTUS = round(cntST / talUS, 3) * 100
    
    
    HTML(paste0("<p><strong>Alumni Job Location in the United States</strong> In ", slctYear, ", ", pctST,  
         "% of ALL alumni work in Michigan, and when only examining those working within the United States, 
         the proportion in Michigan is ", pctSTUS, "%. The next highest concentration of alumni includes those in California. 
         The remaining alumni are distributed across the United States in a manner approximately proportional to state populations.</p><p>&nbsp;</p>"))
  })
  
  # output working state table
  output$stateTable <- renderTable({
    slctYear <- timeConvert(input$selectLcTp)
    if (input$selectLcTp == 'Graduated in 2000-2014') {
      stData <- changeTableHeader(dfStateAll,c('State','Alumni_Count'))
    }
    else {
      stData <- changeTableHeader(dfStateYrs[dfStateYrs$years == slctYear, ], c('Years','State','Alumni_Count'))
    }
    
    setDT(stData)[order(-Alumni_Count)]
  })
  
  #>>> location dynamic UI
  output$locaDynamicUI<-renderUI({
    # zoom migration plot
    if (input$selectShowLc == 1) {
      fluidRow(
        tabBox(
          title = "Alumni migration", width = 12,
          # The id lets us use input$tab_migr on the server to find the current tab
          id = "tab_migr",
          tabPanel("Migration plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("migrateTxt"), plotOutput("migratePlot")),
          tabPanel("Migration data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("migrateTable"))
        )
      )
    }
    # zoom state plot
    else if (input$selectShowLc == 3) {
      fluidRow(
        tabBox(
          title = "Alumni within U.S.", width = 12,
          # The id lets us use input$tab_state on the server to find the current tab
          id = "tab_state",
          tabPanel("State plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("stateTxt"), plotOutput("statePlot")),
          tabPanel("State data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("stateTable"))
        )
      )
    }
    # default to show both
    else {
      fluidRow(
        tabBox(
          title = "Alumni migration",
          # The id lets us use input$tab_migrSml on the server to find the current tab
          id = "tab_migrSml",
          tabPanel("Migration plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("migrateTxt"), plotOutput("migratePlot")),
          tabPanel("Migration data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("migrateTable"))
        ),
        tabBox(
          title = "Alumni within U.S.",
          # The id lets us use input$tab_stateSml on the server to find the current tab
          id = "tab_stateSml",
          tabPanel("State plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("stateTxt"), plotOutput("statePlot")),
          tabPanel("State data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("stateTable"))
        )
      )
    }
  })
  #<<< location dynamic UI
  
  # location information boxes
  output$locaInfoBox<-renderUI({
    if (input$selectLcTp == 'Graduated in 2000-2014') {
      locData = dataGroup
    }
    else {
      locData = dataGroup[dataGroup$years == timeConvert(input$selectLcTp), ]
    }
    
    visTT = nrow(locData[locData$citizenship == 'International',])
    nvsTT = nrow(locData[locData$citizenship == 'US',])
    visUS = nrow(locData[locData$citizenship == 'International' & locData$job_country == 'United States',])
    nvsUS = nrow(locData[locData$citizenship == 'US' & locData$job_country == 'United States',])
    visST = nrow(locData[locData$citizenship == 'International' & locData$job_state == 'Michigan',])
    nvsST = nrow(locData[locData$citizenship == 'US' & locData$job_state == 'Michigan',])
    
    box(
      width = 12,
      fluidRow(
        infoBox("", value = tags$p(paste(visUS, "out of total", visTT, "international alumni remain in US"), style = "font-size: 75%;"), icon = icon("globe"), color = "maroon", fill=TRUE, width=3),
        infoBox("", value = tags$p(paste(nvsUS, "out of total", nvsTT, "US alumni remain in US"), style = "font-size: 75%;"), icon = icon("home"), color = "blue", fill=TRUE, width=3),
        infoBox("", value = tags$p(paste(visST, "international alumni remain in Michigan"), style = "font-size: 75%;"), icon = icon("globe"), color = "purple", fill=TRUE, width=3),
        infoBox("", value = tags$p(paste(nvsST, "US alumni remain in Michigan"), style = "font-size: 75%;"), icon = icon("home"), color = "green", fill=TRUE, width=3)
      )
    )
  }) 
  
  
  #############################################################################
  # General career >>>
  #############################################################################
  
  
  # general career information boxes
  output$genrInfoBox<-renderUI({
    if (input$selectGnrTp %in% c('Graduated in 2000-2014', 'Trend')) {
      itmdData = dataGroup
    }
    else {
      itmdData = subset(dataGroup, years == timeConvert(input$selectGnrTp))
    }
    gnrCount = nrow(itmdData)
    cGnrSector = table(itmdData[,"job_sector"])
    cGnrSectorPct = round(cGnrSector / gnrCount, 3) * 100
    cGnrType = table(itmdData[,"career_type"])
    cGnrTypePct = round(cGnrType / gnrCount, 3) * 100
    cGnrFunction = table(itmdData[,"job_function"])
    cGnrFuncPct = round(cGnrFunction / gnrCount, 3) * 100

    box(
      width = 12,
      fluidRow(
        valueBox(subtitle = paste0("in most common Employment Sector: ", names(which(cGnrSectorPct==max(cGnrSectorPct)))), value = tags$p(paste0(max(cGnrSectorPct), "%"), style = "font-size: 75%;"), icon = icon("plus"), color = "maroon", width=4),
        valueBox(subtitle = paste0("in most common Caeer Type: ", names(which(cGnrTypePct==max(cGnrTypePct)))), value = tags$p(paste0(max(cGnrTypePct), "%"), style = "font-size: 75%;"), icon = icon("plus"), color = "yellow", width=4),
        valueBox(subtitle = paste0("in most common Job Function: ", names(which(cGnrFuncPct==max(cGnrFuncPct)))), value = tags$p(paste0(max(cGnrFuncPct), "%"), style = "font-size: 75%;"), icon = icon("plus"), color = "blue", width=4)
      ),
      fluidRow(
        valueBox(subtitle = paste0("in least common Employment Sector: ", names(which(cGnrSectorPct==min(cGnrSectorPct)))), value = tags$p(paste0(min(cGnrSectorPct), "%"), style = "font-size: 75%;"), icon = icon("minus"), color = "maroon", width=4),
        valueBox(subtitle = paste0("in least common Caeer Type: ", names(which(cGnrTypePct==min(cGnrTypePct)))), value = tags$p(paste0(min(cGnrTypePct), "%"), style = "font-size: 75%;"), icon = icon("minus"), color = "yellow", width=4),
        valueBox(subtitle = paste0("in least common Job Function: ", names(which(cGnrFuncPct==min(cGnrFuncPct)))), value = tags$p(paste0(min(cGnrFuncPct), "%"), style = "font-size: 75%;"), icon = icon("minus"), color = "blue", width=4)
      )
    )
  })
  
  
  #>>> general career plot start
  
  # donut plot job sector data
  output$jobSectDonut <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Graduated in 2000-2014') {
      p <- dfJobSectAll %>% plot_ly(labels = ~Sector, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJSect, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
              add_pie(hole = 0.6) %>%
              layout(title = NULL,  showlegend = F,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Employment Sector<br>", "(", totalYrs, ")</b>"),
              font = list(color = 'black', size = 18), showarrow = F)
      p
    }
    else if (slctYear == 'Trend') {
      nTimeYrs = nlevels(factor(dfJobSectYrs$years))
      
      # number of rows for two plots in one row
      nPlotRws = ceiling(nTimeYrs / 2)
      rTraceHgt = round(1 / nPlotRws, digits = 2)
      
      # set layout domain list
      annoList = vector("list", nTimeYrs)
      
      p <- plot_ly(width = input$sldWidthGnr, height = input$sldHeightGnr, insidetextfont = list(color = '#FFFFFF'))
      for (i in 1:nTimeYrs) {
        iSlctYear = levels(factor(dfJobSectYrs$years))[i]
        iData <- dfJobSectYrs[dfJobSectYrs$years == iSlctYear, ]
        
        x1 = 0 + ((i+1) %% 2)*0.55
        x2 = x1 + 0.45
        # trace layer number
        iTraceLyr = ceiling(i / 2) - 1
        y1 = 1 - iTraceLyr * rTraceHgt - 0.1
        y2 = y1 - rTraceHgt + 0.1
        # domain
        iDomain = list(x = c(x1, x2), y = c(y1, y2))
        
        p <- p %>% add_pie(data = iData, labels = ~Sector, values = ~Count,
                           marker = list(colors = colorJSect, line = list(color = '#FFFFFF', width = 1)),
                           domain = iDomain, hole = 0.6)
        iAnno = list(x=x1+0.25, y=y1-rTraceHgt/2, text = paste0("<b>Employment Sector<br>", "(", iSlctYear, ")</b>"),
                     font = list(size = 18), showarrow = F, xanchor="center")
        annoList[[i]] = iAnno
      }
      p <- p %>% layout(title = NULL,  showlegend = F,
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% layout(annotations = annoList)
      p
    }
    else {
      jscData <- dfJobSectYrs[dfJobSectYrs$years == timeConvert(slctYear), ]
      p <- jscData %>% plot_ly(labels = ~Sector, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJSect, line = list(color = '#FFFFFF', width = 1)),width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Employment Sector<br>", "(", timeConvert(slctYear), ")</b>"),
               font = list(color = 'black', size = 18), showarrow = F)
      p
    }
  })
  
  output$jobSectDonutTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntACA = sum(gnrData$job_sector == 'Academia')
    pctACA = round(cntACA / talDAT, 3) * 100

    HTML(paste0("<p><strong>Career Outcomes in Different Employment Sectors (Donut Plot)</strong> In ", slctYear, ", ", pctACA, 
         "% of all alumni enter the academia sector, while the remainder largely enter the for-profit or government sector.</p><p>&nbsp;</p>"))
  })
  
  # bar plot job sector data
  output$jobSectBar <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Graduated in 2000-2014') {
      jscData = dfJobSectAll
      jscData$Text = paste0(jscData$Sector, ": ", jscData$Count, " (", round(jscData$Alumni*100,2), "%)")
      p <- plot_ly(jscData,x = "", y = ~Alumni*100, type = 'bar',color = ~Sector,colors = colorJSect,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
           layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else if (slctYear == 'Trend') {
      jscData <- dfJobSectYrs
      jscData$Text = paste0(jscData$Sector,"|",jscData$years,":<br> ", jscData$Count, " (", round(jscData$Alumni*100,2), "%)")
      p <- plot_ly(jscData,x = ~years, y = ~Alumni*100, type = 'bar',color = ~Sector,colors = colorJSect,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else {
      jscData <- dfJobSectYrs[dfJobSectYrs$years == timeConvert(slctYear), ]
      jscData$Text = paste0(jscData$Sector, ": ", jscData$Count, " (", round(jscData$Alumni*100,2), "%)")
      p <- plot_ly(jscData,x = "", y = ~Alumni*100, type = 'bar',color = ~Sector,colors = colorJSect,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
  })
  
  output$jobSectBarTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntACA = sum(gnrData$job_sector == 'Academia')
    pctACA = round(cntACA / talDAT, 3) * 100

    HTML(paste0("<p><strong>Career Outcomes in Different Employment Sectors (Bar Chart)</strong> In ", slctYear, ", ", pctACA, 
                "% of all alumni enter the academia sector, while the remainder largely enter the for-profit or government sector.</p><p>&nbsp;</p>"))
  })
  
  # output job sector table
  output$jobSectTable <- renderTable({
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      jscData <- changeTableHeader(dfJobSectAll,c('Job_Sector','Alumni_Count','Alumni_Portion'))
    }
    else if (input$selectGnrTp == 'Trend') {
      jscData <- changeTableHeader(dfJobSectYrs,c('Years','Job_Sector','Alumni_Count','Alumni_Portion'))
    }
    else {
      jscData <- changeTableHeader(dfJobSectYrs[dfJobSectYrs$years == timeConvert(input$selectGnrTp), ],c('Years','Job_Sector','Alumni_Count','Alumni_Portion'))
    }
    jscData
  })
  
  # donut plot career type data
  output$jobTypeDonut <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Graduated in 2000-2014') {
      p <- dfJobTypeAll %>% plot_ly(labels = ~Type, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJType, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Caeer Type<br>", "(", totalYrs, ")</b>"),
                                 font = list(color = 'black', size = 18), showarrow = F)
      p
    }
    else if (slctYear == 'Trend') {
      nTimeYrs = nlevels(factor(dfJobTypeYrs$years))
      
      # number of rows for two plots in one row
      nPlotRws = ceiling(nTimeYrs / 2)
      rTraceHgt = round(1 / nPlotRws, digits = 2)
      
      # set layout domain list
      annoList = vector("list", nTimeYrs)
      
      p <- plot_ly(width = input$sldWidthGnr, height = input$sldHeightGnr, insidetextfont = list(color = '#FFFFFF'))
      for (i in 1:nTimeYrs) {
        iSlctYear = levels(factor(dfJobTypeYrs$years))[i]
        iData <- dfJobTypeYrs[dfJobTypeYrs$years == iSlctYear, ]
        
        x1 = 0 + ((i+1) %% 2)*0.55
        x2 = x1 + 0.45
        # trace layer number
        iTraceLyr = ceiling(i / 2) - 1
        y1 = 1 - iTraceLyr * rTraceHgt - 0.1
        y2 = y1 - rTraceHgt + 0.1
        # domain
        iDomain = list(x = c(x1, x2), y = c(y1, y2))
        
        p <- p %>% add_pie(data = iData, labels = ~Type, values = ~Count,
                           marker = list(colors = colorJType, line = list(color = '#FFFFFF', width = 1)),
                           domain = iDomain, hole = 0.6)
        iAnno = list(x=x1+0.25, y=y1-rTraceHgt/2, text = paste0("<b>Caeer Type<br>", "(", iSlctYear, ")</b>"),
                     font = list(size = 18), showarrow = F, xanchor="center")
        annoList[[i]] = iAnno
      }
      p <- p %>% layout(title = NULL,  showlegend = F,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% layout(annotations = annoList)
      p
    }
    else {
      jtpData <- dfJobTypeYrs[dfJobTypeYrs$years == timeConvert(slctYear), ]
      p <- jtpData %>% plot_ly(labels = ~Type, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                               marker = list(colors = colorJType, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Caeer Type<br>", "(", timeConvert(slctYear), ")</b>"),
                                 font = list(color = 'black', size = 18), showarrow = F)
      p
    }
  })
  
  output$jobTypeDonutTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    talDAT = nrow(gnrData)
    cntTP1 = sum(gnrData$career_type == 'Primarily Research')
    cntTP2 = sum(gnrData$career_type == 'Science-Related')
    pctTP1 = round(cntTP1 / talDAT, 3) * 100
    pctTP2 = round(cntTP2 / talDAT, 3) * 100
    
    HTML(paste0("<p><strong>Career Outcomes in Different Career Types</strong> In ", slctYear, ", ", pctTP1,
         "% are in primarily research career type, while ", pctTP2, "% are in other science-related career type, 
         and the rest are in further training, primarily teaching, or non scientific career types.</p><p>&nbsp;</p>"))
  })
  
  # bar plot career type data
  output$jobTypeBar <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Graduated in 2000-2014') {
      jtpData = dfJobTypeAll
      jtpData$Text = paste0(jtpData$Type, ": ", jtpData$Count, " (", round(jtpData$Alumni*100,2), "%)")
      p <- plot_ly(jtpData,x = "", y = ~Alumni*100, type = 'bar',color = ~Type,colors = colorJType,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else if (slctYear == 'Trend') {
      jtpData <- dfJobTypeYrs
      jtpData$Text = paste0(jtpData$Type,"|",jtpData$years,":<br> ", jtpData$Count, " (", round(jtpData$Alumni*100,2), "%)")
      p <- plot_ly(jtpData,x = ~years, y = ~Alumni*100, type = 'bar',color = ~Type,colors = colorJType,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else {
      jtpData <- dfJobTypeYrs[dfJobTypeYrs$years == timeConvert(slctYear), ]
      jtpData$Text = paste0(jtpData$Type, ": ", jtpData$Count, " (", round(jtpData$Alumni*100,2), "%)")
      p <- plot_ly(jtpData,x = "", y = ~Alumni*100, type = 'bar',color = ~Type,colors = colorJType,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
  })
  
  output$jobTypeBarTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }

    talDAT = nrow(gnrData)
    cntTP1 = sum(gnrData$career_type == 'Primarily Research')
    cntTP2 = sum(gnrData$career_type == 'Science-Related')
    pctTP1 = round(cntTP1 / talDAT, 3) * 100
    pctTP2 = round(cntTP2 / talDAT, 3) * 100
    
    HTML(paste0("<p><strong>Career Outcomes in Different Career Types</strong> In ", slctYear, ", ", pctTP1,
                "% are in primarily research career type, while ", pctTP2, "% are in other science-related career type, 
                and the rest are in further training, primarily teaching, or non scientific career types.</p><p>&nbsp;</p>"))
  })
  
  # output career type table
  output$jobTypeTable <- renderTable({
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      jtpData <- changeTableHeader(dfJobTypeAll,c('Career_Type','Alumni_Count','Alumni_Portion'))
    }
    else if (input$selectGnrTp == 'Trend') {
      jtpData <- changeTableHeader(dfJobTypeYrs,c('Years','Career_Type','Alumni_Count','Alumni_Portion'))
    }
    else {
      jtpData <- changeTableHeader(dfJobTypeYrs[dfJobTypeYrs$years == timeConvert(input$selectGnrTp), ],c('Years','Career_Type','Alumni_Count','Alumni_Portion'))
    }
    jtpData
  })

    
  # donut plot job function data
  output$jobFuncDonut <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Graduated in 2000-2014') {
      p <- dfJobFuncAll %>% plot_ly(labels = ~Function, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                                    marker = list(colors = colorJFunc, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Function<br>", "(", totalYrs, ")</b>"),
                                 font = list(color = 'black', size = 16), showarrow = F)
      p
    }
    else if (slctYear == 'Trend') {
      nTimeYrs = nlevels(factor(dfJobFuncYrs$years))
      
      # number of rows for two plots in one row
      nPlotRws = ceiling(nTimeYrs / 2)
      rTraceHgt = round(1 / nPlotRws, digits = 2)
      
      # set layout domain list
      annoList = vector("list", nTimeYrs)
      
      p <- plot_ly(width = input$sldWidthGnr, height = input$sldHeightGnr, insidetextfont = list(color = '#FFFFFF'))
      for (i in 1:nTimeYrs) {
        iSlctYear = levels(factor(dfJobFuncYrs$years))[i]
        iData <- dfJobFuncYrs[dfJobFuncYrs$years == iSlctYear, ]
        
        x1 = 0 + ((i+1) %% 2)*0.55
        x2 = x1 + 0.45
        # trace layer number
        iTraceLyr = ceiling(i / 2) - 1
        y1 = 1 - iTraceLyr * rTraceHgt - 0.1
        y2 = y1 - rTraceHgt + 0.1
        # domain
        iDomain = list(x = c(x1, x2), y = c(y1, y2))
        
        p <- p %>% add_pie(data = iData, labels = ~Function, values = ~Count,
                           marker = list(colors = colorJFunc, line = list(color = '#FFFFFF', width = 1)),
                           domain = iDomain, hole = 0.6)
        iAnno = list(x=x1+0.25, y=y1-rTraceHgt/2, text = paste0("<b>Job Function<br>", "(", iSlctYear, ")</b>"),
                     font = list(size = 16), showarrow = F, xanchor="center")
        annoList[[i]] = iAnno
      }
      p <- p %>% layout(title = NULL,  showlegend = F,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% layout(annotations = annoList)
      p
    }
    else {
      jspData <- dfJobFuncYrs[dfJobFuncYrs$years == timeConvert(slctYear), ]
      p <- jspData %>% plot_ly(labels = ~Function, values = ~Count, insidetextfont = list(color = '#FFFFFF'),
                               marker = list(colors = colorJFunc, line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>%
        add_pie(hole = 0.6) %>%
        layout(title = NULL,  showlegend = F,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      p <- p %>% add_annotations(x= 0.5, y= 0.5, text = paste0("<b>Job Function<br>", "(", timeConvert(slctYear), ")</b>"),
                                 font = list(color = 'black', size = 16), showarrow = F)
      p
    }
  })
  
  output$jobFuncDonutTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    tblDAT = table(gnrData[,"job_function"])
    topJOB3 = names(head(sort(tblDAT, decreasing = TRUE), 3))

    HTML(paste0("<p><strong>Career Outcomes in Different Job Function</strong> In ", slctYear, ", the top three job functions are ", paste(topJOB3, collapse = ', '), ".</p><p>&nbsp;</p>"))
  })
  
  # bar plot job function data
  output$jobFuncBar <- renderPlotly({
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectGnrTp
    if (slctYear == 'Graduated in 2000-2014') {
      jspData = dfJobFuncAll
      jspData$Text = paste0(jspData$Function, ": ", jspData$Count, " (", round(jspData$Alumni*100,2), "%)")
      p <- plot_ly(jspData,x = "", y = ~Alumni*100, type = 'bar',color = ~job_function,colors = colorJFunc,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else if (slctYear == 'Trend') {
      jspData <- dfJobFuncYrs
      jspData$Text = paste0(jspData$Function,"|",jspData$years,":<br> ", jspData$Count, " (", round(jspData$Alumni*100,2), "%)")
      p <- plot_ly(jspData,x = ~years, y = ~Alumni*100, type = 'bar',color = ~job_function,colors = colorJFunc,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
    else {
      jspData <- dfJobFuncYrs[dfJobFuncYrs$years == timeConvert(slctYear), ]
      jspData$Text = paste0(jspData$Function, ": ", jspData$Count, " (", round(jspData$Alumni*100,2), "%)")
      p <- plot_ly(jspData,x = "", y = ~Alumni*100, type = 'bar',color = ~job_function,colors = colorJFunc,text=~Text,
                   hoverinfo ='text',marker = list(line = list(color = '#FFFFFF', width = 1)), width = input$sldWidthGnr, height = input$sldHeightGnr) %>% 
        layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack", showlegend = F) 
      p
    }
  })
  
  output$jobFuncBarTxt  <- renderText({
    slctYear <- timeConvert(input$selectGnrTp)
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      gnrData = dataGroup
    }
    else {
      gnrData = subset(dataGroup, years == slctYear)
    }
    
    tblDAT = table(gnrData[,"job_function"])
    topJOB3 = names(head(sort(tblDAT, decreasing = TRUE), 3))
    
    HTML(paste0("<p><strong>Career Outcomes in Different Job Function</strong> In ", slctYear, ", the top three job functions are ", paste(topJOB3, collapse = ', '), ".</p><p>&nbsp;</p>"))
  })
  # https://stackoverflow.com/questions/34315485/linking-to-a-tab-or-panel-of-a-shiny-app
  # https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard
  # https://stackoverflow.com/questions/33021757/externally-link-to-specific-tabpanel-in-shiny-app
  # https://stackoverflow.com/questions/28605185/create-link-to-the-other-part-of-the-shiny-app/28605517#28605517
  # https://stackoverflow.com/questions/27303526/r-shiny-build-links-between-tabs
  # https://github.com/rstudio/shiny/issues/772#issuecomment-112919149
  # https://groups.google.com/forum/#!msg/shiny-discuss/sJlasQf71fY/RW7Xc8F02IoJ
  
  # output job function table
  output$jobFuncTable <- renderTable({
    if (input$selectGnrTp == 'Graduated in 2000-2014') {
      jspData <- changeTableHeader(dfJobFuncAll,c('Job_Function','Alumni_Count','Alumni_Portion'))
    }
    else if (input$selectGnrTp == 'Trend') {
      jspData <- changeTableHeader(dfJobFuncYrs,c('Years','Job_Function','Alumni_Count','Alumni_Portion'))
    }
    else {
      jspData <- changeTableHeader(dfJobFuncYrs[dfJobFuncYrs$years == timeConvert(input$selectGnrTp), ],c('Years','Job_Function','Alumni_Count','Alumni_Portion'))
    }
    jspData
  })
  
  
  output$genrDynamicUI<-renderUI({
    # zoom job sector plot
    if (input$selectShowGnr == 2) {
      fluidRow(
        tabBox(
          title = "Employment Sector", width = 12,
          # The id lets us use input$tab_genJSector on the server to find the current tab
          id = "tab_genJSector",
          tabPanel("Enployment sector chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobSectDonutTxt"), plotlyOutput("jobSectDonut")),
          # tabPanel("Enployment sector bar chart", style = "overflow-x:scroll; overflow-y:scroll; max-height: 1200px", htmlOutput("jobSectBarTxt"), plotlyOutput("jobSectBar")),
          tabPanel("Enployment sector data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSectTable"))
        )
      )
    }
    # zoom career type plot
    else if (input$selectShowGnr == 3) {
      fluidRow(
        tabBox(
          title = "Caeer Type", width = 12,
          # The id lets us use input$tab_genJType on the server to find the current tab
          id = "tab_genJType",
          tabPanel("Career type chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobTypeDonutTxt"), plotlyOutput("jobTypeDonut")),
          # tabPanel("Career type bar chart", style = "overflow-x:scroll; overflow-y:scroll; max-height: 1200px", htmlOutput("jobTypeBarTxt"), plotlyOutput("jobTypeBar")),
          tabPanel("Career type data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobTypeTable"))
        )
      )
    }
    # zoom career type plot
    else if (input$selectShowGnr == 4) {
      fluidRow(
        tabBox(
          title = "Job Function", width = 12,
          # The id lets us use input$tab_genJFunc on the server to find the current tab
          id = "tab_genJFunc",
          tabPanel("Job function chart", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("jobFuncDonutTxt"), plotlyOutput("jobFuncDonut")),
          tabPanel("Job function data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobFuncTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
          tabBox(
            title = "Employment Sector",
            # The id lets us use input$tab_genJSectorSml on the server to find the current tab
            id = "tab_genJSectorSml",
            tabPanel("Sector chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobSectDonutTxt"), plotlyOutput("jobSectDonut")),
            tabPanel("Sector data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSectTable"))
          ),
          tabBox(
            title = "Caeer Type", width = 6,
            # The id lets us use input$tab_genJTypeSml on the server to find the current tab
            id = "tab_genJTypeSml",
            tabPanel("Type chart", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("jobTypeDonutTxt"), plotlyOutput("jobTypeDonut")),
            tabPanel("Type data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobTypeTable"))
          )
        ),
        column(width = 12,
          tabBox(
            title = "Job Function",
            # The id lets us use input$tab_genJFuncSml on the server to find the current tab
            id = "tab_genJFuncSml",
            tabPanel("Function chart", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("jobFuncDonutTxt"), plotlyOutput("jobFuncDonut")),
            tabPanel("Function data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobFuncTable"))
          )
        )
      )
    }
  })
  
  #<<< general career plot
  
  

  #############################################################################
  # Relationship >>>
  #############################################################################
  
  # Sankey plot
  output$jobSankeyPlot <- renderPlotly({
    sankeyData_gvisTOnetD3 <- function(gvisData, from="from", to="to", weight="weight") {
      # create nodes data frame
      vNode = unique(c(gvisData[,from], gvisData[,to]))
      nodesDF = data.frame(name=vNode)
      
      # add sequence number into nodes data frame extension
      nodes_ext = nodesDF
      nodes_ext$id = seq.int(nrow(nodes_ext))-1
      nodesDF$name <- as.character(nodesDF$name)
      
      # create links data frame by matching node name
      # http://stackoverflow.com/questions/11530184/match-values-in-data-frame-with-values-in-another-data-frame-and-replace-former
      linksDF = data.frame(source=nodes_ext[match(gvisData[,from], nodes_ext$name), 2],
                           target=nodes_ext[match(gvisData[,to], nodes_ext$name), 2], value=gvisData$weight)
      
      netD3Data = list(nodes=nodesDF, links=linksDF)
      
      return(netD3Data)
    }
    
    # choose data based on input$selectGnrTp from ui.R
    slctYear <- input$selectRlnTp
    if (slctYear == 'Graduated in 2000-2014') {
      skData = dataGroup
    }
    else if (slctYear == 'Trend') {
      # set trend data as total data
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    dataSecTyp <- skData %>% group_by(job_sector,career_type) %>% summarise (weight = n())
    dataSecTyp$job_sector <- as.character(dataSecTyp$job_sector)
    dataSecTyp$job_sector[dataSecTyp$job_sector == 'Unknown or Undecided'] <- "Unknown (sector)"
    dataSecTyp$job_sector <- factor(dataSecTyp$job_sector)
    dataSecTyp$career_type <- as.character(dataSecTyp$career_type)
    dataSecTyp$career_type[dataSecTyp$career_type == 'Unknown or Undecided'] <- "Unknown (type)"
    dataSecTyp$career_type <- factor(dataSecTyp$career_type)
    colnames(dataSecTyp)[colnames(dataSecTyp)=="job_sector"] <- "from"
    colnames(dataSecTyp)[colnames(dataSecTyp)=="career_type"] <- "to"
    
    dataTypSpe <- skData %>% group_by(career_type, job_function) %>% summarise (weight = n())
    dataTypSpe$career_type <- as.character(dataTypSpe$career_type)
    dataTypSpe$career_type[dataTypSpe$career_type == 'Unknown or Undecided'] <- "Unknown (type)"
    dataTypSpe$career_type <- factor(dataTypSpe$career_type)
    dataTypSpe$job_function <- as.character(dataTypSpe$job_function)
    dataTypSpe$job_function[dataTypSpe$job_function == 'Unknown or Undecided'] <- "Unknown (job_function)"
    dataTypSpe$job_function <- factor(dataTypSpe$job_function)
    colnames(dataTypSpe)[colnames(dataTypSpe)=="career_type"] <- "from"
    colnames(dataTypSpe)[colnames(dataTypSpe)=="job_function"] <- "to"
    
    dataSK <- bind_rows(as.data.frame(dataSecTyp), as.data.frame(dataTypSpe))
    netD3 <- sankeyData_gvisTOnetD3(dataSK)
    
    p <- plot_ly(
      type = "sankey",
      domain = c(
        x =  c(0,1),
        y =  c(0,1)
      ),
      orientation = "h",
      valueformat = ".0f",
      
      node = list(
        label = netD3$nodes$name,
        color = c("#8a2105","#f19898","#ef86ab","#99335c","#b44e11","#df9f4d",
                  "#295a29","#89f3d2","#04e0e8","#295a4c","#61d661","#617d0d","#91c844",
                  "#9bc2df","#97a9f5","#6c3a77","#b399c9","#8787fb","#483a77","#215c7a","#21327a","#21777a","#04e0e8","#e461c4","#892f73"),
        pad = 15,
        thickness = 15,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = netD3$links$source,
        target = netD3$links$target,
        value =  netD3$links$value,
        label =  netD3$links$job_cat
      ),
      width = input$sldWidthRln, height = input$sldHeightRln
    ) %>% 
      layout(
        title = "Connection Between Employment Sectors, Career Types, and Job Function",
        font = list(
          size = 12
        )
        # xaxis = list(showgrid = F, zeroline = F),
        # yaxis = list(showgrid = F, zeroline = F)
      )
    
    p
  })
  
  # output$jobSankeyTxt  <- renderText({HTML("<p><strong>Relationship Between Employment Sectors, Career Types and Job Function</strong> 
  #                                          The width of the lines is proportional to the relative quantity of scholars within each group. 
  #                                          <i>(Left & Middle) Division of job sectors by career type.</i> Focusing on the academic sector as an example, it can be seen that not all academic positions are tenure-track. The remainder of those in the academic sector are divided between professional, management, support, non-tenure-track, or trainee career types. 
  #                                          <i>(Middle & Right) Division of career types by job function.</i> Focusing on tenure-track positions as an example, it can be seen that the majority are conducting basic research in tenure-track positions. Most of the remaining individuals in tenure-track positions are either teaching, or conducting applied or clinical research.</p><p>&nbsp;</p>")})
  
  # output Sankey data table
  output$jobSankeyTable <- renderTable({
    slctYear <- input$selectRlnTp
    if (slctYear == 'Graduated in 2000-2014') {
      skData = dataGroup
    }
    else if (slctYear == 'Trend') {
      # set trend data as total data
      skData = dataGroup
    }
    else {
      skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
    }
    
    dataSecTyp <- skData %>% group_by(job_sector,career_type) %>% summarise (weight = n())
    dataSecTyp$job_sector <- as.character(dataSecTyp$job_sector)
    dataSecTyp$job_sector[dataSecTyp$job_sector == 'Unknown or Undecided'] <- "Unknown (sector)"
    dataSecTyp$career_type <- as.character(dataSecTyp$career_type)
    dataSecTyp$career_type[dataSecTyp$career_type == 'Unknown or Undecided'] <- "Unknown (type)"
    colnames(dataSecTyp)[colnames(dataSecTyp)=="job_sector"] <- "from"
    colnames(dataSecTyp)[colnames(dataSecTyp)=="career_type"] <- "to"
    
    dataTypSpe <- skData %>% group_by(career_type, job_function) %>% summarise (weight = n())
    dataTypSpe$career_type <- as.character(dataTypSpe$career_type)
    dataTypSpe$career_type[dataTypSpe$career_type == 'Unknown or Undecided'] <- "Unknown (type)"
    dataTypSpe$job_function <- as.character(dataTypSpe$job_function)
    dataTypSpe$job_function[dataTypSpe$job_function == 'Unknown or Undecided'] <- "Unknown (job_function)"
    colnames(dataTypSpe)[colnames(dataTypSpe)=="career_type"] <- "from"
    colnames(dataTypSpe)[colnames(dataTypSpe)=="job_function"] <- "to"
    
    dataSK <- bind_rows(as.data.frame(dataSecTyp), as.data.frame(dataTypSpe))
    colnames(dataSK) <- c('Job_Population','Job_Subpopulation','Alumni_Count')
    dataSK
  })
  
  output$rltnDynamicUI<-renderUI({
    fluidRow(
      tabBox(
        title = "Relationship between categories", width = 12,
        # The id lets us use input$tab_rlnSankey on the server to find the current tab
        id = "tab_rlnSankey",
        tabPanel("Relationship sankey plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", plotlyOutput("jobSankeyPlot")),
        tabPanel("Relationship data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("jobSankeyTable"))
      )
    )
  })
  
  
  #############################################################################
  # Job highlight >>>
  #############################################################################
  # Font size of legend - https://github.com/timelyportfolio/sunburstR/issues/35
  {
  # #>>> job distribution highlight
  # colorHL <- list(
  #   range = c("#C295C9","#867cb6","#b82e91","#97D6B4","#54b649","#2f9d49"),
  #   domain= c("Intn'l","Intn'l abroad","Intn'l in US","US","US abroad","US in US")
  # )
  # # Controlling colours across plots  --- https://github.com/timelyportfolio/sunburstR/issues/17
  # # colorHL <- c("#C295C9","#362586","#A22980","#97D6B4","#51B447","#27843C")
  # 
  # # tenure-track plot
  # output$hlTenurePlot <- renderSunburst({
  #   # choose data based on input$selectHlTp from ui.R
  #   slctYear <- input$selectHlTp
  #   if (slctYear == 'Graduated in 2000-2014') {
  #     skData = dataGroup
  #   }
  #   else {
  #     skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
  #   }
  #   
  #   # count tenure-track by international alumni abroad, international alumni in US, US alumni abroad, US alumni in US
  #   skData_tt = skData[skData$career_type == "Tenure track faculty", ]
  #   skData_tt$job_country = as.character(skData_tt$job_country)
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l abroad"
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US abroad"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l in US"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US in US"
  #   skData_tt$job_country = factor(skData_tt$job_country)
  #   skData_tt$citizenship = as.character(skData_tt$citizenship)
  #   skData_tt$citizenship[skData_tt$citizenship == 'International'] = "Intn'l"
  #   skData_tt$citizenship = factor(skData_tt$citizenship)
  #   skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
  #   skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
  #   
  #   p <- skData_tbl %>% mutate(path=paste(citizenship,job_country,sep='-')) %>% select(path,freq) %>% sunburst(colors = colorHL)
  #   p
  # })
  # 
  # output$hlTenureTxt  <- renderText({
  #   HTML("<p><strong>Closer Look at All Tenure-Track Positions</strong> 
  #        We examined the relative proportion of U.S. versus international scholars in tenure-track positions, and found that majority of tenure-track faculty positions are held by international scholars (center circle, light magenta). 
  #        However, most hold these positions abroad (outer circle, dark purple). 
  #        Of the U.S. scholars, most hold tenure-track positions in the United States (outer circle, dark green), with only small percentage of tenure-track positions being held by U.S. scholars abroad (outer circle, light green).</p><p>&nbsp;</p>")
  # })
  # 
  # # tenure-track table
  # output$hlTenureTable <- renderTable({
  #   # choose data based on input$selectHlTp from ui.R
  #   slctYear <- input$selectHlTp
  #   if (slctYear == 'Graduated in 2000-2014') {
  #     skData = dataGroup
  #   }
  #   else {
  #     skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
  #   }
  #   
  #   # count tenure-track by international alumni abroad,  international alumni in US, US alumni abroad, US alumni in US
  #   skData_tt = skData[skData$career_type == "Tenure track faculty", ]
  #   skData_tt$job_country = as.character(skData_tt$job_country)
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar abroad"
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar abroad"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar in US"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar in US"
  #   skData_tt$job_country = factor(skData_tt$job_country)
  #   skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
  #   skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
  #   changeTableHeader(skData_tbl,c('Origin_Country','Job_Country','Alumni_Count','Alumni_Portion'))
  # })
  # 
  # # additional training plot
  # output$hlTrainPlot <- renderSunburst({
  #   # choose data based on input$selectHlTp from ui.R
  #   slctYear <- input$selectHlTp
  #   if (slctYear == 'Graduated in 2000-2014') {
  #     skData = dataGroup
  #   }
  #   else {
  #     skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
  #   }
  #   
  #   # count tenure-track by international alumni abroad, international alumni in US, US alumni abroad, US alumni in US
  #   skData_tt = skData[skData$career_type == "Trainee", ]
  #   skData_tt$job_country = as.character(skData_tt$job_country)
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l abroad"
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US abroad"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l in US"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US in US"
  #   skData_tt$job_country = factor(skData_tt$job_country)
  #   skData_tt$citizenship = as.character(skData_tt$citizenship)
  #   skData_tt$citizenship[skData_tt$citizenship == 'International'] = "Intn'l"
  #   skData_tt$citizenship = factor(skData_tt$citizenship)
  #   skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
  #   skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
  #   
  #   p <- skData_tbl %>% mutate(path=paste(citizenship,job_country,sep='-')) %>% select(path,freq) %>% sunburst(colors = colorHL)
  #   p
  # })
  # 
  # output$hlTrainTxt  <- renderText({
  #   slctYear <- timeConvert(input$selectHlTp)
  #   if (slctYear %in% c('2000-2014','2010-2014')) {
  #     HTML("<p><strong>Closer Look at All Individuals Engaged in Additional Postdoctoral Training</strong> 
  #          We examined everyone that was engaged in additional postdoctoral training and found that the majority were international scholars (center circle, light magenta). 
  #          Most alumni conducted additional postdoctoral training within the United States, regardless of whether they were international or U.S. (outer circle, dark magenta & dark green, respectively).</p><p>&nbsp;</p>")
  #   }
  #   else if (slctYear == '2000-2004') {
  #     HTML("<p><strong>Closer Look at All Individuals Engaged in Additional Postdoctoral Training</strong> 
  #          We examined everyone that was engaged in additional postdoctoral training and found that the majority were international scholars (center circle, light magenta). 
  #          All alumni conducted additional postdoctoral training within the United States, regardless of whether they were international or U.S. (outer circle, dark magenta & dark green, respectively).</p><p>&nbsp;</p>")
  #   }
  #   else if (slctYear == '2005-2009') {
  #     HTML("<p><strong>Closer Look at All Individuals Engaged in Additional Postdoctoral Training</strong> 
  #          We examined everyone that was engaged in additional postdoctoral training and found that the majority were international scholars (center circle, light magenta). 
  #          All US alumni conducted additional postdoctoral training within the United States (outer circle, dark green). 
  #          Most international alumni conducted additional postdoctoral training within the United States (outer circle, dark magenta), 
  #          while a small portion of international alumni had additional postdoctoral training out side of the United States (outer circle, purple).</p><p>&nbsp;</p>")
  #   }
  # })
  # 
  # # additional training table
  # output$hlTrainTable <- renderTable({
  #   # choose data based on input$selectHlTp from ui.R
  #   slctYear <- input$selectHlTp
  #   if (slctYear == 'Graduated in 2000-2014') {
  #     skData = dataGroup
  #   }
  #   else {
  #     skData = dataGroup[dataGroup$years == timeConvert(slctYear), ]
  #   }
  #   
  #   # count tenure-track by international alumni abroad, international alumni in US, US alumni abroad, US alumni in US
  #   skData_tt = skData[skData$career_type == "Trainee", ]
  #   skData_tt$job_country = as.character(skData_tt$job_country)
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar abroad"
  #   skData_tt$job_country[(skData_tt$job_country != 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar abroad"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'International')] = "Intn'l Scholar in US"
  #   skData_tt$job_country[(skData_tt$job_country == 'United States') & (skData_tt$citizenship == 'US')] = "US Scholar in US"
  #   skData_tt$job_country = factor(skData_tt$job_country)
  #   skData_tbl <- skData_tt %>% group_by(citizenship, job_country) %>% summarise(cnt = n())
  #   skData_tbl <- data.frame(skData_tbl) %>% mutate(freq=cnt/sum(cnt))
  #   changeTableHeader(skData_tbl,c('Origin_Country','Job_Country','Alumni_Count','Alumni_Portion'))
  # })
  # 
  # output$hlDynamicUI<-renderUI({
  #   # zoom tenure track plot
  #   if (input$selectShowHl == 1) {
  #     fluidRow(
  #       tabBox(
  #         title = "Tenure track", width = 12,
  #         # The id lets us use input$tab_hlTT on the server to find the current tab
  #         id = "tab_hlTT",
  #         tabPanel("Tenure track plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTenureTxt"), sunburstOutput("hlTenurePlot", width = input$sldWidthHl, height = input$sldHeightHl)),
  #         tabPanel("Tenure track data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTenureTable"))
  #       )
  #     )
  #   }
  #   # zoom visiting plot
  #   else if (input$selectShowHl == 3) {
  #     fluidRow(
  #       tabBox(
  #         title = "Additional postdoc", width = 12,
  #         # The id lets us use input$tab_hlAT on the server to find the current tab
  #         id = "tab_hlAT",
  #         tabPanel("Addt'l postdoc plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTrainTxt"), sunburstOutput("hlTrainPlot", width = input$sldWidthHl, height = input$sldHeightHl)),
  #         tabPanel("Addt'l postdoc data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTrainTable"))
  #       )
  #     )
  #   }
  #   # default to show both
  #   else {
  #     fluidRow(
  #       tabBox(
  #         title = "Tenure track",
  #         # The id lets us use input$tab_hlTTSml on the server to find the current tab
  #         id = "tab_hlTTSml",
  #         tabPanel("Tenure track plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTenureTxt"), sunburstOutput("hlTenurePlot", width = input$sldWidthHl, height = input$sldHeightHl)),
  #         tabPanel("Tenure track data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTenureTable"))
  #       ),
  #       tabBox(
  #         title = "Additional postdoc",
  #         # The id lets us use input$tab_hlATSml on the server to find the current tab
  #         id = "tab_hlATSml",
  #         tabPanel("Addt'l postdoc plot", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("hlTrainTxt"), sunburstOutput("hlTrainPlot", width = input$sldWidthHl, height = input$sldHeightHl)),
  #         tabPanel("Addt'l postdoc data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("hlTrainTable"))
  #       )
  #     )
  #   }
  # })
  # #<<< job distribution highlight
  }
  


  #############################################################################
  # Job trend >>>
  #############################################################################
  {
    trndHeight <- function() {
      input$sldHeightTrnd
    }
    trndWidth <- function() {
      input$sldWidthTrnd
    }
    
    # job sector trend change plot
    output$trndSectPlot <- renderPlot({
      dfData <- dfJobSectYrs
      setDT(dfData)
      dfData <- dfData[, pos:=1 - cumsum(Alumni)+0.5*Alumni, by="years"]
      ptypLabel=paste0(round(subset(dfData, Alumni > 0.05)$Alumni,2) * 100,"%")
      p <- ggplot(dfData, aes(x=years, y=Alumni, fill=Sector)) + geom_bar(stat="identity")
      p <- p + geom_text(data=subset(dfData, Alumni > 0.05), aes(y = pos, label = ptypLabel))
      p <- p + scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5)) + scale_fill_manual(values=colorJSect)
      p <- p + coord_flip() +
        theme(aspect.ratio=1, axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
              axis.title=element_blank(), panel.grid=element_blank(), panel.background=element_blank())
      
      p <- p + coord_polar(theta="y")
      p
    },height=trndHeight,width=trndWidth)
    
    # job sector trend change txt
    output$trndSectPlotTxt  <- renderText({
      
      HTML(paste0("<p><strong>Career Trend in Different Employment Sectors</strong> ", 
                  ".</p><p>&nbsp;</p>"))
    })
    
    # output job sector trend table
    output$trndSectTable <- renderTable({
      jscData <- changeTableHeader(dfJobSectYrs,c('Years','Job_Sector','Alumni_Count','Alumni_Portion'))
      jscData
    })
    

    # career type trend change plot
    output$trndTypePlot <- renderPlot({
      dfData <- dfJobTypeYrs
      setDT(dfData)
      dfData <- dfData[, pos:=1 - cumsum(Alumni)+0.5*Alumni, by="years"]
      ptypLabel=paste0(round(subset(dfData, Alumni > 0.05)$Alumni,2) * 100,"%")
      p <- ggplot(dfData, aes(x=years, y=Alumni, fill=Type)) + geom_bar(stat="identity")
      p <- p + geom_text(data=subset(dfData, Alumni > 0.05), aes(y = pos, label = ptypLabel))
      p <- p + scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5)) + scale_fill_manual(values=colorJType)
      p <- p + coord_flip() +
        theme(aspect.ratio=1, axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
              axis.title=element_blank(), panel.grid=element_blank(), panel.background=element_blank())
      
      p <- p + coord_polar(theta="y")
      p
    },height=trndHeight,width=trndWidth)
    
    # job sector trend change txt
    output$trndTypePlotTxt  <- renderText({
      
      HTML(paste0("<p><strong>Career Trend in Different Career Types</strong> ",
                  ".</p><p>&nbsp;</p>"))
    })
    
    # output career type trend table
    output$trndTypeTable <- renderTable({
      jtpData <- changeTableHeader(dfJobTypeYrs,c('Years','Career_Type','Alumni_Count','Alumni_Portion'))
      jtpData
    })
    
    
    # job function trend change plot
    output$trndFuncPlot <- renderPlot({
      dfData <- dfJobFuncYrs
      setDT(dfData)
      dfData <- dfData[, pos:=1 - cumsum(Alumni)+0.5*Alumni, by="years"]
      ptypLabel=paste0(round(subset(dfData, Alumni > 0.05)$Alumni,2) * 100,"%")
      p <- ggplot(dfData, aes(x=years, y=Alumni, fill=Function)) + geom_bar(stat="identity")
      p <- p + geom_text(data=subset(dfData, Alumni > 0.05), aes(y = pos, label = ptypLabel))
      p <- p + scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5)) + scale_fill_manual(values=colorJFunc)
      p <- p + coord_flip() +
        theme(aspect.ratio=1, axis.text=element_blank(), axis.line=element_blank(), axis.ticks=element_blank(),
              axis.title=element_blank(), panel.grid=element_blank(), panel.background=element_blank())
      
      p <- p + coord_polar(theta="y")
      p
    },height=trndHeight,width=trndWidth)
    
    # job functtion trend change txt
    output$trndFuncPlotTxt  <- renderText({
      
      HTML(paste0("<p><strong>Career Trend in Different Job Functions</strong> ", 
                  ".</p><p>&nbsp;</p>"))
    })
    
    # output job function trend table
    output$trndFuncTable <- renderTable({
      jfcData <- changeTableHeader(dfJobFuncYrs,c('Years','Job_Function','Alumni_Count','Alumni_Portion'))
      jfcData
    })
    
    output$trndDynamicUI<-renderUI({
      # zoom job sector plot
      if (input$selectShowTrnd == 2) {
        fluidRow(
          tabBox(
            title = "Trend in Employment Sectors", width = 12,
            # The id lets us use input$tab_trndJSector on the server to find the current tab
            id = "tab_trndJSector",
            tabPanel("Enployment sector trend", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("trndSectPlotTxt"), plotOutput("trndSectPlot")),
            tabPanel("Enployment sector data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("trndSectTable"))
          )
        )
      }
      # zoom career type plot
      else if (input$selectShowTrnd == 3) {
        fluidRow(
          tabBox(
            title = "Trend in Caeer Types", width = 12,
            # The id lets us use input$tab_trndJType on the server to find the current tab
            id = "tab_trndJType",
            tabPanel("Career type trend", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("trndTypePlotTxt"), plotOutput("trndTypePlot")),
            tabPanel("Career type data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("trndTypeTable"))
          )
        )
      }
      # zoom job function plot
      else if (input$selectShowTrnd == 4) {
        fluidRow(
          tabBox(
            title = "Trend in Job Functions", width = 12,
            # The id lets us use input$tab_trndJFunc on the server to find the current tab
            id = "tab_trndJFunc",
            tabPanel("Job function trend", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("trndFuncPlotTxt"), plotOutput("trndFuncPlot")),
            tabPanel("Job function data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("trndFuncTable"))
          )
        )
      }
      # default to show all
      else {
        fluidRow(
          column(width = 12,
            tabBox(
              title = "Trend in Employment Sectors", width = 6,
              # The id lets us use input$tab_trndJSectorSml on the server to find the current tab
              id = "tab_trndJSectorSml",
              tabPanel("Enployment sector trend", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("trndSectPlotTxt"), plotOutput("trndSectPlot")),
              tabPanel("Enployment sector data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("trndSectTable"))
            ),
            tabBox(
              title = "Trend in Caeer Types", width = 6,
              # The id lets us use input$tab_trndJTypeSml on the server to find the current tab
              id = "tab_trndJTypeSml",
              tabPanel("Career type trend", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("trndTypePlotTxt"), plotOutput("trndTypePlot")),
              tabPanel("Career type data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("trndTypeTable"))
            )
          ),
          column(width = 12,
            tabBox(
              title = "Trend in Job Functions", width = 6,
              # The id lets us use input$tab_trndJFuncSml on the server to find the current tab
              id = "tab_trndJFuncSml",
              tabPanel("Job function trend", style = "overflow-x:scroll; overflow-y:scroll; height: 600px", htmlOutput("trndFuncPlotTxt"), plotOutput("trndFuncPlot")),
              tabPanel("Job function data", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("trndFuncTable"))
            )
          )
        )
      }
    })
  }
  
  
  #############################################################################
  # Training time >>>
  #############################################################################
  
  
  #>>> training time
  # training time job sector plot
  output$tmSectPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmSect <- dataGroup
      time.sct = aggregate(train_time ~ job_sector, tmSect, median)
      colnames(time.sct)[colnames(time.sct)=="train_time"] <- "Median_time"
      time.sct$sct_num = as.numeric(time.sct$job_sector)
      p <- ggplot(tmSect, aes(x = job_sector, y = train_time, fill = job_sector)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.sct, aes(label=round(Median_time,1), x=sct_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJSect)
    }
    else if (input$selectTmTp == 'Trend') {
      tmSect <- dataGroup
      time.sct = aggregate(train_time ~ years + job_sector, tmSect, median)
      colnames(time.sct)[colnames(time.sct)=="train_time"] <- "Median_time"
      time.sct$sct_num = as.numeric(time.sct$job_sector)
      p <- ggplot(tmSect, aes(x = years, y = train_time, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.sct, aes(label=round(Median_time,1), x=sct_num+0.4, y = Median_time))
      p <- p + facet_grid(job_sector ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmSect <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.sct = aggregate(train_time ~ job_sector, tmSect, median)
      colnames(time.sct)[colnames(time.sct)=="train_time"] <- "Median_time"
      time.sct$sct_num = as.numeric(time.sct$job_sector)
      p <- ggplot(tmSect, aes(x = job_sector, y = train_time, fill = job_sector)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.sct, aes(label=round(Median_time,1), x=sct_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJSect)
    }
    p <- p + ggtitle("Training Time (years)") + theme(legend.position="none") +
          labs(y='Years', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp # %>% layout(margin = list(l = 75))
  })
  
  output$tmSectTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmSCT = aggregate(train_time ~ job_sector, tmData, median)
    lgstSCT <- tolower(as.character(tmSCT$job_sector[tmSCT$train_time == max(tmSCT$train_time)]))
    
    HTML(paste0("<p><strong>Summary of Training Time Difference among Different Employment Sectors</strong> In ", slctYear, 
                ", alumni entering into a(n) <i>", lgstSCT, "</i> have the longest median training time (", max(tmSCT$train_time), " years).</p><p>&nbsp;</p>"))
   })
  
  # training time job sector table
  output$tmSectTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmSect <- changeTableHeader(dfTimeSectAll,c('Job_Sector','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmSect <- changeTableHeader(dfTimeSectYrs,c('Years','Job_Sector','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmSect <- changeTableHeader(dfTimeSectYrs[dfTimeSectYrs$years ==timeConvert( input$selectTmTp), ],c('Years','Job_Sector','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmSect
  })
  
  # training time career type plot
  output$tmTypePlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmType <- dataGroup
      time.typ = aggregate(train_time ~ career_type, tmType, median)
      colnames(time.typ)[colnames(time.typ)=="train_time"] <- "Median_time"
      time.typ$typ_num = as.numeric(time.typ$career_type)
      p <- ggplot(tmType, aes(x = career_type, y = train_time, fill = career_type)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.typ, aes(label=round(Median_time,1), x=typ_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJType)
    }
    else if (input$selectTmTp == 'Trend') {
      tmType <- dataGroup
      time.typ = aggregate(train_time ~ years + career_type, tmType, median)
      colnames(time.typ)[colnames(time.typ)=="train_time"] <- "Median_time"
      time.typ$typ_num = as.numeric(time.typ$career_type)
      p <- ggplot(tmType, aes(x = years, y = train_time, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.typ, aes(label=round(Median_time,1), x=typ_num+0.4, y = Median_time))
      p <- p + facet_grid(career_type ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmType <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.typ = aggregate(train_time ~ career_type, tmType, median)
      colnames(time.typ)[colnames(time.typ)=="train_time"] <- "Median_time"
      time.typ$typ_num = as.numeric(time.typ$career_type)
      p <- ggplot(tmType, aes(x = career_type, y = train_time, fill = career_type)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.typ, aes(label=round(Median_time,1), x=typ_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJType)
    }
    p <- p + ggtitle("Training Time (years)") + theme(legend.position="none") +
      labs(y='Years', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp # %>% layout(margin = list(l = 75))
  })
  
  output$tmTypeTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmTYP = aggregate(train_time ~ career_type, tmData, median)
    lgstTYP <- tolower(as.character(tmTYP$career_type[tmTYP$train_time == max(tmTYP$train_time)]))
    
    HTML(paste0("<p><strong>Summary of Training Time Difference among Different Career Types</strong> In ", slctYear, 
                ", alumni who enter into <i>", lgstTYP, "</i> positions have the longest median training time (", max(tmTYP$train_time), " years).</p><p>&nbsp;</p>"))
  })
  
  # training time career type table
  output$tmTypeTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmType <- changeTableHeader(dfTimeTypeAll,c('Career_Type','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmType <- changeTableHeader(dfTimeTypeYrs,c('Years','Career_Type','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmType <- changeTableHeader(dfTimeTypeYrs[dfTimeTypeYrs$years == timeConvert(input$selectTmTp), ],c('Years','Career_Type','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmType
  })
  
  # training time job function plot
  output$tmFuncPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmFunc <- dataGroup
      time.spc = aggregate(train_time ~ job_function, tmFunc, median)
      colnames(time.spc)[colnames(time.spc)=="train_time"] <- "Median_time"
      time.spc$spc_num = as.numeric(time.spc$job_function)
      p <- ggplot(tmFunc, aes(x = job_function, y = train_time, fill = job_function)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.spc, aes(label=round(Median_time,1), x=spc_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJFunc)
    }
    else if (input$selectTmTp == 'Trend') {
      tmFunc <- dataGroup
      time.spc = aggregate(train_time ~ years + job_function, tmFunc, median)
      colnames(time.spc)[colnames(time.spc)=="train_time"] <- "Median_time"
      time.spc$spc_num = as.numeric(time.spc$job_function)
      p <- ggplot(tmFunc, aes(x = years, y = train_time, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.spc, aes(label=round(Median_time,1), x=spc_num+0.4, y = Median_time))
      p <- p + facet_grid(job_function ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmFunc <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.spc = aggregate(train_time ~ job_function, tmFunc, median)
      colnames(time.spc)[colnames(time.spc)=="train_time"] <- "Median_time"
      time.spc$spc_num = as.numeric(time.spc$job_function)
      p <- ggplot(tmFunc, aes(x = job_function, y = train_time, fill = job_function)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.spc, aes(label=round(Median_time,1), x=spc_num+0.4, y = Median_time))
      p <- p + scale_fill_manual(values=colorJFunc)
    }
    p <- p + ggtitle("Training Time (years)") + theme(legend.position="none") +
      labs(y='Years', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp # %>% layout(margin = list(l = 75))
  })
  
  output$tmFuncTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmSPC = aggregate(train_time ~ job_function, tmData, median)
    lgstSPC <- tolower(as.character(tmSPC$job_function[tmSPC$train_time == max(tmSPC$train_time)]))
    
    HTML(paste0("<p><strong>Summary of Training Time Difference among Different Job Function</strong> In ", slctYear, 
        ", alumni who enter into <i>", lgstSPC, "</i> positions have the longest median training time (", max(tmSPC$train_time), " years).</p><p>&nbsp;</p>"))
  })
  
  # training time job function table
  output$tmFuncTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmFunc <- changeTableHeader(dfTimeFuncAll,c('Job_Function','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmFunc <- changeTableHeader(dfTimeFuncYrs,c('Years','Job_Function','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmFunc <- changeTableHeader(dfTimeFuncYrs[dfTimeFuncYrs$years == timeConvert(input$selectTmTp), ],c('Years','Job_Function','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    
    tmFunc
  })  
  
  # https://stackoverflow.com/questions/19440069/ggplot2-facet-wrap-strip-color-based-on-variable-in-data-set
  # https://stats.stackexchange.com/questions/8206/labeling-boxplots-in-r
  # https://stackoverflow.com/questions/32342616/ggplot-increase-distance-between-boxplots
  # training time gender plot
  output$tmGndrPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmGndr <- dataGroup
      time.gnd = aggregate(train_time ~ gender, tmGndr, median)
      colnames(time.gnd)[colnames(time.gnd)=="train_time"] <- "Median_time"
      time.gnd$g_num = as.numeric(time.gnd$gender)
      p <- ggplot(tmGndr, aes(x = gender, y = train_time, fill = gender)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.gnd, aes(label=round(Median_time,1), x=g_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=genderColors)
    }
    else if (input$selectTmTp == 'Trend') {
      tmGndr <- dataGroup
      time.gnd = aggregate(train_time ~ years + gender, tmGndr, median)
      colnames(time.gnd)[colnames(time.gnd)=="train_time"] <- "Median_time"
      time.gnd$g_num = as.numeric(time.gnd$gender)
      p <- ggplot(tmGndr, aes(x = years, y = train_time, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.gnd, aes(label=round(Median_time,1), x=g_num+0.3, y = Median_time))
      p <- p + facet_grid(gender ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmGndr <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.gnd = aggregate(train_time ~ gender, tmGndr, median)
      colnames(time.gnd)[colnames(time.gnd)=="train_time"] <- "Median_time"
      time.gnd$g_num = as.numeric(time.gnd$gender)
      p <- ggplot(tmGndr, aes(x = gender, y = train_time, fill = gender)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.gnd, aes(label=round(Median_time,1), x=g_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=genderColors)
    }
    p <- p + ggtitle("Training Time (years)") + theme(legend.position="none") +
      labs(y='Years', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp %>% layout(margin = list(l = 75))
  })
  
  output$tmGndrTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmMale = tmData$train_time[tmData$gender == 'Male']
    tmFemale = tmData$train_time[tmData$gender == 'Female']
    tTst = t.test(tmMale, tmFemale)
    
    if (tTst$p.value < 0.05){
      HTML(paste0("<p><strong>Summary of Training Time Difference between Males and Females</strong> In ", slctYear,
                  ", the difference in training time between male and female alumni is significant (p-value = ", format.pval(tTst$p.value,digits=5,eps = 0.00001,scientific = TRUE), ").</p><p>&nbsp;</p>"))
    }
    else {
      HTML(paste0("<p><strong>Summary of Training Time Difference between Males and Females</strong> In ", slctYear,
           ", the difference in training time between male and female alumni is not significant (p-value = ", format.pval(tTst$p.value,digits=5,eps = 0.00001,scientific = TRUE), ").</p><p>&nbsp;</p>"))
    }
  })
  
  # training time gender table
  output$tmGndrTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmGndr <- changeTableHeader(gendTimeAll,c('Gender','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmGndr <- changeTableHeader(gendTimeGrp,c('Years','Gender','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmGndr <- changeTableHeader(gendTimeGrp[gendTimeGrp$years == timeConvert(input$selectTmTp), ],c('Years','Gender','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmGndr
  })
  
  
  # training time citizen plot
  output$tmCtznPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmCtzn <- dataGroup
      time.ctz = aggregate(train_time ~ citizenship, tmCtzn, median)
      colnames(time.ctz)[colnames(time.ctz)=="train_time"] <- "Median_time"
      time.ctz$c_num = as.numeric(time.ctz$citizenship)
      p <- ggplot(tmCtzn, aes(x = citizenship, y = train_time, fill = citizenship)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.ctz, aes(label=round(Median_time,1), x=c_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=visitColors)
    }
    else if (input$selectTmTp == 'Trend') {
      tmCtzn <- dataGroup
      time.ctz = aggregate(train_time ~ years + citizenship, tmCtzn, median)
      colnames(time.ctz)[colnames(time.ctz)=="train_time"] <- "Median_time"
      time.ctz$c_num = as.numeric(time.ctz$citizenship)
      p <- ggplot(tmCtzn, aes(x = years, y = train_time, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.ctz, aes(label=round(Median_time,1), x=c_num+0.3, y = Median_time))
      p <- p + facet_grid(citizenship ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmCtzn <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.ctz = aggregate(train_time ~ citizenship, tmCtzn, median)
      colnames(time.ctz)[colnames(time.ctz)=="train_time"] <- "Median_time"
      time.ctz$c_num = as.numeric(time.ctz$citizenship)
      p <- ggplot(tmCtzn, aes(x = citizenship, y = train_time, fill = citizenship)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.ctz, aes(label=round(Median_time,1), x=c_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=visitColors)
    }
    p <- p + ggtitle("Training Time (years)") + theme(legend.position="none") +
      labs(y='Years', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp %>% layout(margin = list(l = 75))
  })
  
  output$tmCtznTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmIntl = tmData$train_time[tmData$citizenship == 'International']
    tmUS = tmData$train_time[tmData$citizenship == 'US']
    tTst = t.test(tmIntl, tmUS)
    
    if (tTst$p.value < 0.05){
      HTML(paste0("<p><strong>Summary of Training Time Difference between International and US Students</strong> In ", slctYear,
                  ", the difference in training time between international and US students is significant (p-value = ", format.pval(tTst$p.value,digits=5,eps = 0.00001,scientific = TRUE), ").</p><p>&nbsp;</p>"))
    }
    else {
      HTML(paste0("<p><strong>Summary of Training Time Difference between International and US Students</strong> In ", slctYear,
                  ", the difference in training time between international and US students is not significant (p-value = ", format.pval(tTst$p.value,digits=5,eps = 0.00001,scientific = TRUE), ").</p><p>&nbsp;</p>"))
    }
  })
  
  # training time citizen table
  output$tmCtznTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmCtzn <- changeTableHeader(citiTimeAll,c('Citizenship','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmCtzn <- changeTableHeader(citiTimeGrp,c('Years','Citizenship','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmCtzn <- changeTableHeader(citiTimeGrp[citiTimeGrp$years == timeConvert(input$selectTmTp), ],c('Years','Citizenship','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmCtzn
  })

  # training time funding plot
  output$tmFundPlot <- renderPlotly({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmFund <- dataGroup
      time.fnd = aggregate(train_time ~ funding, tmFund, median)
      colnames(time.fnd)[colnames(time.fnd)=="train_time"] <- "Median_time"
      time.fnd$c_num = as.numeric(time.fnd$funding)
      p <- ggplot(tmFund, aes(x = funding, y = train_time, fill = funding)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.fnd, aes(label=round(Median_time,1), x=c_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=visitColors)
    }
    else if (input$selectTmTp == 'Trend') {
      tmFund <- dataGroup
      time.fnd = aggregate(train_time ~ years + funding, tmFund, median)
      colnames(time.fnd)[colnames(time.fnd)=="train_time"] <- "Median_time"
      time.fnd$c_num = as.numeric(time.fnd$funding)
      p <- ggplot(tmFund, aes(x = years, y = train_time, fill = years)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.fnd, aes(label=round(Median_time,1), x=c_num+0.3, y = Median_time))
      p <- p + facet_grid(funding ~ .)
      p <- p + theme(strip.text.y = element_text(size=10, face="bold"))
    }
    else {
      tmFund <- dataGroup[dataGroup$years == timeConvert(input$selectTmTp), ]
      time.fnd = aggregate(train_time ~ funding, tmFund, median)
      colnames(time.fnd)[colnames(time.fnd)=="train_time"] <- "Median_time"
      time.fnd$c_num = as.numeric(time.fnd$funding)
      p <- ggplot(tmFund, aes(x = funding, y = train_time, fill = funding)) + geom_boxplot()
      p <- p + coord_flip() + geom_text(data=time.fnd, aes(label=round(Median_time,1), x=c_num+0.3, y = Median_time))
      p <- p + scale_fill_manual(values=visitColors)
    }
    p <- p + ggtitle("Training Time (years)") + theme(legend.position="none") +
      labs(y='Years', x='')
    gp <- ggplotly(p, width = input$sldWidthTm, height = input$sldHeightTm, tooltip=c("Median_time"))
    gp %>% layout(margin = list(l = 75))
  })
  
  output$tmFundTxt  <- renderText({
    slctYear <- timeConvert(input$selectTmTp)
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmData = dataGroup
    }
    else {
      tmData = subset(dataGroup, years == slctYear)
    }
    
    tmFund = tmData$train_time[tmData$funding == 'Yes']
    tmNfnd = tmData$train_time[tmData$funding == 'No']
    tTst = t.test(tmFund, tmNfnd)
    
    if (tTst$p.value < 0.05){
      HTML(paste0("<p><strong>Summary of Training Time Difference between Funded and Non-funded Students</strong> In ", slctYear,
                  ", the difference in training time between funded and non-funded students is significant (p-value = ", format.pval(tTst$p.value,digits=5,eps = 0.00001,scientific = TRUE), ").</p><p>&nbsp;</p>"))
    }
    else {
      HTML(paste0("<p><strong>Summary of Training Time Difference between Funded and Non-funded</strong> In ", slctYear,
                  ", the difference in training time between funded and non-funded students is not significant (p-value = ", format.pval(tTst$p.value,digits=5,eps = 0.00001,scientific = TRUE), ").</p><p>&nbsp;</p>"))
    }
  })
  
  # training time funding table
  output$tmFundTable <- renderTable({
    # choose data based on input$selectTmTp from ui.R
    if (input$selectTmTp == 'Graduated in 2000-2014') {
      tmFund <- changeTableHeader(citiTimeAll,c('Citizenship','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else if (input$selectTmTp == 'Trend') {
      tmFund <- changeTableHeader(citiTimeGrp,c('Years','Citizenship','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    else {
      tmFund <- changeTableHeader(citiTimeGrp[citiTimeGrp$years == timeConvert(input$selectTmTp), ],c('Years','Citizenship','Avg_Time','Min_Time','Max_Time','Alumni_Count'))
    }
    tmFund
  })
    
  # training time dynamic UI
  output$tmDynamicUI<-renderUI({
    # zoom job sector training time plot
    if (input$selectShowTm == 2) {
      fluidRow(
        tabBox(
          title = "Training time (Enployment sector)", width = 12,
          # The id lets us use input$tab_tmSect on the server to find the current tab
          id = "tab_tmSect",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmSectTxt"), plotlyOutput("tmSectPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmSectTable"))
        )
      )
    }
    # zoom career type training time plot
    else if (input$selectShowTm == 3) {
      fluidRow(
        tabBox(
          title = "Training time (Career type)", width = 12,
          # The id lets us use input$tab_tmType on the server to find the current tab
          id = "tab_tmType",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmTypeTxt"), plotlyOutput("tmTypePlot", height=input$sldHeightTm)),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmTypeTable"))
        )
      )
    }
    # zoom job function training time plot
    else if (input$selectShowTm == 4) {
      fluidRow(
        tabBox(
          title = "Training time (Job function)", width = 12,
          # The id lets us use input$tab_tmFunc on the server to find the current tab
          id = "tab_tmFunc",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmFuncTxt"), plotlyOutput("tmFuncPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmFuncTable"))
        )
      )
    }
    # zoom gender training time plot
    else if (input$selectShowTm == 5) {
      fluidRow(
        tabBox(
          title = "Training time (Gender differences)", width = 12,
          # The id lets us use input$tab_tmGndr on the server to find the current tab
          id = "tab_tmGndr",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmGndrTxt"), plotlyOutput("tmGndrPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmGndrTable"))
        )
      )
    }
    # zoom citizen training time plot
    else if (input$selectShowTm == 6) {
      fluidRow(
        tabBox(
          title = "Training time (Citizenship differences)", width = 12,
          # The id lets us use input$tab_tmCtzn on the server to find the current tab
          id = "tab_tmCtzn",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmCtznTxt"), plotlyOutput("tmCtznPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmCtznTable"))
        )
      )
    }
    # zoom funding training time plot
    else if (input$selectShowTm == 7) {
      fluidRow(
        tabBox(
          title = "Training time (Funding differences)", width = 12,
          # The id lets us use input$tab_tmFund on the server to find the current tab
          id = "tab_tmFund",
          tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmFundTxt"), plotlyOutput("tmFundPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmFundTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
          tabBox(
            title = "Training time (Enployment sector)", width = 6,
            # The id lets us use input$tab_tmSectSml on the server to find the current tab
            id = "tab_tmSectSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmSectTxt"), plotlyOutput("tmSectPlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmSectTable"))
          ),
          tabBox(
            title = "Training time (Career type)", width = 6,
            # The id lets us use input$tab_tmTypeSml on the server to find the current tab
            id = "tab_tmTypeSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmTypeTxt"), plotlyOutput("tmTypePlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmTypeTable"))
          )
        ),
        column(width = 12,
          tabBox(
            title = "Training time (Job function)", width = 6,
            # The id lets us use input$tab_tmFuncSml on the server to find the current tab
            id = "tab_tmFuncSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmFuncTxt"), plotlyOutput("tmFuncPlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmFuncTable"))
          ),
          tabBox(
            title = "Training time (Gender differences)", width = 6,
            # The id lets us use input$tab_tmGndrSml on the server to find the current tab
            id = "tab_tmGndrSml",
            tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmGndrTxt"), plotlyOutput("tmGndrPlot", height="400px")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmGndrTable"))
          )
        ),
        column(width = 12,
          tabBox(
            title = "Training time (Citizenship)", width = 6,
            # The id lets us use input$tab_tmCtznSml on the server to find the current tab
                 id = "tab_tmCtznSml",
                 tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmCtznTxt"), plotlyOutput("tmCtznPlot", height="400px")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmCtznTable"))
          ),
          tabBox(
                 title = "Training time (Funding differences)", width = 6,
                 # The id lets us use input$tab_tmFundSml on the server to find the current tab
                 id = "tab_tmFundSml",
                 tabPanel("Box plot", style = "overflow-x:scroll; overflow-y:scroll; height: 500px", htmlOutput("tmFundTxt"), plotlyOutput("tmFundPlot", height="400px")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("tmFundTable"))
          )
        )
      )
    }
  })
  
  # https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny
  # https://github.com/ropensci/plotly/issues/1036
  #<<< training time
  
  
  #############################################################################
  # Top countries >>>
  #############################################################################
  
  
  #>>> top 5 countries
  ctryData <- subset(dataGroup, country_origin %in% top5Ctry)

  ctHeight <- function() {
    input$sldHeightCt
  }
  ctWidth <- function() {
    input$sldWidthCt
  } 
  
  # job category plot for top countries
  output$cntrJobPlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    # topSect <- ccSctCnt$job_sector[ccSctCnt$cnt == max(ccSctCnt$cnt)]
    topSect <- setSect
    # count career_type
    ccTypCnt <- topData %>% group_by(career_type) %>% summarise (cnt = n())
    # topType <- ccTypCnt$career_type[ccTypCnt$cnt == max(ccTypCnt$cnt)]
    topType <- setType
    # count job_job_function
    ccSpcCnt <- topData %>% group_by(job_function) %>% summarise (cnt = n())
    # topFunc <- ccSpcCnt$job_function[ccSpcCnt$cnt == max(ccSpcCnt$cnt)]
    topFunc <- setFunc
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      ccSct1 <- ccSctAll
      psctLabel=paste0(round(ccSct1$percent,2) * 100,"%")
      ccSct1$lab <- psctLabel
      # set label of job sector other than top job sector to blank
      ccSct1[ccSct1$job_sector != topSect, ]$lab <- ""
      # add number info to country
      ccSct1$country_origin <- as.character(ccSct1$country_origin)
      ccSct1$country_origin <- paste(ccSct1$country_origin, paste0("N=",ccSct1$catcnt), sep="\n")
      ccSct1$country_origin <- factor(ccSct1$country_origin)
      # (7/13/18)
      labBase <- subset(ccSct1, job_sector == topSect)
      
      p1 <- ggplot(ccSct1,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,color=job_sector))+
        scale_size_area(name="Employment Sector\n(% within\nCountry)",max_size=20,labels=scales::percent,breaks = seq(0, 1, 0.2)) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p1 <- p1 + scale_color_manual(values=colorJSect, guide = FALSE)
      p1 <- p1 + geom_label(data=labBase, aes(x=country_origin,y=job_sector,label=lab))
      p1 <- p1 + labs(x="Country of Origin", y= "")
      p1 <- p1 + title("Employment Sector Distribution in Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      ccJtp1 <- ccJtpAll
      ptypLabel=paste0(round(ccJtp1$percent,2) * 100,"%")
      ccJtp1$lab <- ptypLabel
      # set label of career type other than top career type to blank
      ccJtp1[ccJtp1$career_type != topType, ]$lab <- ""
      # add number info to country
      ccJtp1$country_origin <- as.character(ccJtp1$country_origin)
      ccJtp1$country_origin <- paste(ccJtp1$country_origin, paste0("N=",ccJtp1$catcnt), sep="\n")
      ccJtp1$country_origin <- factor(ccJtp1$country_origin)
      # (7/13/18)
      labBase <- subset(ccJtp1, career_type == topType)
      
      p1 <- ggplot(ccJtp1,aes(x=country_origin,y=career_type)) +
        geom_point(aes(size=percent,color=career_type))+
        scale_size_area(name="Caeer Type\n(% within\nCountry)",max_size=20,labels=scales::percent,breaks = seq(0, 1, 0.1)) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p1 <- p1 + scale_color_manual(values=colorJType, guide = FALSE)
      p1 <- p1 + geom_label(data=labBase, aes(x=country_origin,y=career_type,label=lab))
      p1 <- p1 + labs(x="Country of Origin", y= "")
      p1 <- p1 + title("Caeer Type Distribution in Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      ccSpc1 <- ccSpcAll
      psctLabel=paste0(round(ccSpc1$percent,2) * 100,"%")
      ccSpc1$lab <- psctLabel
      # set label of job function other than top job function to blank
      ccSpc1[ccSpc1$job_function != topFunc, ]$lab <- ""
      # add number info to country
      ccSpc1$country_origin <- as.character(ccSpc1$country_origin)
      ccSpc1$country_origin <- paste(ccSpc1$country_origin, paste0("N=",ccSpc1$catcnt), sep="\n")
      ccSpc1$country_origin <- factor(ccSpc1$country_origin)
      # (7/13/18)
      labBase <- subset(ccSpc1, job_function == topFunc)
      
      p1 <- ggplot(ccSpc1,aes(x=country_origin,y=job_function)) +
        geom_point(aes(size=percent,color=job_function))+
        scale_size_area(name="Job Function\n(% within\nCountry)",max_size=20,labels=scales::percent,breaks = seq(0, 1, 0.1)) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p1 <- p1 + scale_color_manual(values=colorJFunc, guide = FALSE)
      p1 <- p1 + geom_label(data=labBase, aes(x=country_origin,y=job_function,label=lab))
      p1 <- p1 + labs(x="Country of Origin", y= "")
      p1 <- p1 + title("Job Function Distribution in Top 5 Countries")
    }
    # p1 <- p1 + theme(axis.text = element_text(size=12))
    p1
  },height=ctHeight,width=ctWidth)
  
  output$cntrJobTxt  <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Employment Sector</strong>
           The legend depicting the size of the circles applies to all panels because the job category data is repeated and overlaid with another dimension in the subsequent panels. 
           Percentages are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Caeer Type</strong>
           The legend depicting the size of the circles applies to all panels because the job category data is repeated and overlaid with another dimension in the subsequent panels. 
           Percentages are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the career types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting career types within China sums to 100% of Chinese alumni, etc.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Job Function</strong>
           The legend depicting the size of the circles applies to all panels because the job category data is repeated and overlaid with another dimension in the subsequent panels. 
           Percentages are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job function within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job function within China sums to 100% of Chinese alumni, etc.</p><p>&nbsp;</p>")
    }
  })
  
  # job category data table for top countries
  output$cntrJobTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      changeTableHeader(ccSctAll,c('Country_Origin','Job_Sector','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      changeTableHeader(ccJtpAll,c('Country_Origin','Career_Type','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      changeTableHeader(ccSpcAll,c('Country_Origin','Job_Function','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin'))
    }
  })  
  
  # training time plot for top countries
  output$cntrTimePlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    topSect <- setSect
    # count career_type
    ccTypCnt <- topData %>% group_by(career_type) %>% summarise (cnt = n())
    topType <- setType
    # count job_job_function
    ccSpcCnt <- topData %>% group_by(job_function) %>% summarise (cnt = n())
    topFunc <- setFunc
    
    # average training time
    avgTime <- as.integer(mean(topData$train_time))
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      # mean time
      mt2 <- aggregate(train_time~country_origin+job_sector,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("country_origin","job_sector"))
      
      # Label
      pmntLabel=paste0(round(ccMntAll$train_time,0),"yr")
      ccMntAll$lab <- pmntLabel
      # set label of job sector other than top job sector to blank
      ccMntAll[ccMntAll$job_sector != topSect, ]$lab <- ""
      
      # add number info to country
      ccMntAll$country_origin <- as.character(ccMntAll$country_origin)
      ccMntAll$country_origin <- paste(ccMntAll$country_origin, paste0("N=",ccMntAll$catcnt), sep="\n")
      ccMntAll$country_origin <- factor(ccMntAll$country_origin)
      # (3/27/18)
      labMnt <- subset(ccMntAll, job_sector == topSect)
      
      p2 <- ggplot(ccMntAll,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,fill=train_time),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Employment Sector\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p2 <- p2 + geom_label(data=labMnt, aes(x=country_origin,y=job_sector,label=lab))
      p2 <- p2 + scale_fill_gradient2(name="Mean Training\nTime (years)", low="green",mid="purple", high="orange", midpoint=avgTime)
      # p2 <- p2 + labs(x="Country of Origin", y= "Employment Sector")
      p2 <- p2 + labs(x="Country of Origin", y= "")
      p2 <- p2 + title("Mean Training Time in Different Employment Sectors\nacross Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      # mean time
      mt2 <- aggregate(train_time~country_origin+career_type,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccJtpAll,mt2,by=c("country_origin","career_type"))
      
      # Label
      pmntLabel=paste0(round(ccMntAll$train_time,0),"yr")
      ccMntAll$lab <- pmntLabel
      # set label of career type other than top career type to blank
      ccMntAll[ccMntAll$career_type != topType, ]$lab <- ""
      
      # add number info to country
      ccMntAll$country_origin <- as.character(ccMntAll$country_origin)
      ccMntAll$country_origin <- paste(ccMntAll$country_origin, paste0("N=",ccMntAll$catcnt), sep="\n")
      ccMntAll$country_origin <- factor(ccMntAll$country_origin)
      # (3/27/18)
      labMnt <- subset(ccMntAll, career_type == topType)
      
      p2 <- ggplot(ccMntAll,aes(x=country_origin,y=career_type)) +
        geom_point(aes(size=percent,fill=train_time),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Caeer Type\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p2 <- p2 + geom_label(data=labMnt, aes(x=country_origin,y=career_type,label=lab))
      p2 <- p2 + scale_fill_gradient2(name="Mean Training\nTime (years)", low="green",mid="purple", high="orange", midpoint=avgTime)
      # p2 <- p2 + labs(x="Country of Origin", y= "Caeer Type")
      p2 <- p2 + labs(x="Country of Origin", y= "")
      p2 <- p2 + title("Mean Training Time in Different Caeer Type\nacross Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      # mean time
      mt2 <- aggregate(train_time~country_origin+job_function,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSpcAll,mt2,by=c("country_origin","job_function"))
      
      # Label
      pmntLabel=paste0(round(ccMntAll$train_time,0),"yr")
      ccMntAll$lab <- pmntLabel
      # set label of job function other than top job function to blank
      ccMntAll[ccMntAll$job_function != topFunc, ]$lab <- ""
      
      # add number info to country
      ccMntAll$country_origin <- as.character(ccMntAll$country_origin)
      ccMntAll$country_origin <- paste(ccMntAll$country_origin, paste0("N=",ccMntAll$catcnt), sep="\n")
      ccMntAll$country_origin <- factor(ccMntAll$country_origin)
      # (3/27/18)
      labMnt <- subset(ccMntAll, job_function == topFunc)
      
      p2 <- ggplot(ccMntAll,aes(x=country_origin,y=job_function)) +
        geom_point(aes(size=percent,fill=train_time),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Function\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p2 <- p2 + geom_label(data=labMnt, aes(x=country_origin,y=job_function,label=lab))
      p2 <- p2 + scale_fill_gradient2(name="Mean Training\nTime (years)", low="green",mid="purple",high="orange", midpoint=avgTime)
      # p2 <- p2 + labs(x="Country of Origin", y= "Job Function")
      p2 <- p2 + labs(x="Country of Origin", y= "")
      p2 <- p2 + title("Mean Training Time in Different Job Function\nacross Top 5 Countries")
    }
    p2
  },height=ctHeight,width=ctWidth)
  
  output$cntrTimeTxt  <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Training Time</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because <u>the job category data is repeated and overlaid with the average training time</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc. 
           The mean training time (in years) is displayed in text for all alumni within the total selected job sector.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Training Time</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because <u>the job category data is repeated and overlaid with the average training time</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the career types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting career types within China sums to 100% of Chinese alumni, etc. 
           The mean training time (in years) is displayed in text for all alumni within the total selected career type.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Training Time</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because <u>the job category data is repeated and overlaid with the average training time</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job function within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job function within China sums to 100% of Chinese alumni, etc. 
           The mean training time (in years) is displayed in text for all alumni within the total selected job function.</p><p>&nbsp;</p>")
    }
  })
  
  # training time data table for top countries
  output$cntrTimeTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      # mean time
      mt2 <- aggregate(train_time~country_origin+job_sector,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("country_origin","job_sector"))
      ccMntAll <- changeTableHeader(ccMntAll,c('Country_Origin','Job_Sector','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Avg_Time(Years)'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      # mean time
      mt2 <- aggregate(train_time~country_origin+career_type,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccJtpAll,mt2,by=c("country_origin","career_type"))
      ccMntAll <- changeTableHeader(ccMntAll,c('Country_Origin','Career_Type','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Avg_Time(Years)'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      # mean time
      mt2 <- aggregate(train_time~country_origin+job_function,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSpcAll,mt2,by=c("country_origin","job_function"))
      ccMntAll <- changeTableHeader(ccMntAll,c('Country_Origin','Job_Function','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Avg_Time(Years)'))
    }
    ccMntAll
  })  

  # gender plot for top countries
  output$cntrGenderPlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    topSect <- setSect
    # count career_type
    ccTypCnt <- topData %>% group_by(career_type) %>% summarise (cnt = n())
    # topType <- ccTypCnt$career_type[ccTypCnt$cnt == max(ccTypCnt$cnt)]
    topType <- setType
    # count job_job_function
    ccSpcCnt <- topData %>% group_by(job_function) %>% summarise (cnt = n())
    topFunc <- setFunc
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_sector, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%M")
      ccGenAll$lab <- pgenLabel
      # set label of job sector other than top job sector to blank
      ccGenAll[ccGenAll$job_sector != topSect, ]$lab <- ""
      
      # add number info to country
      ccGenAll$country_origin <- as.character(ccGenAll$country_origin)
      ccGenAll$country_origin <- paste(ccGenAll$country_origin, paste0("N=",ccGenAll$catcnt), sep="\n")
      ccGenAll$country_origin <- factor(ccGenAll$country_origin)
      # (3/27/18)
      labGen <- subset(ccGenAll, job_sector == topSect)
      
      p3 <- ggplot(ccGenAll,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,fill=mpct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Employment Sector\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p3 <- p3 + geom_label(data=labGen, aes(x=country_origin,y=job_sector,label=lab))
      p3 <- p3 + scale_fill_gradient2(name="Gender\n(% Male)",low="deeppink2",mid="white", high="dodgerblue1", midpoint=0.5, labels=scales::percent)
      # p3 <- p3 + labs(x="Country of Origin", y= "Employment Sector")
      p3 <- p3 + labs(x="Country of Origin", y= "")
      p3 <- p3 + title("Gender Distribution in Different Employment Sectors\nacross Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, career_type, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccJtpAll,by=c("country_origin","career_type"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%M")
      ccGenAll$lab <- pgenLabel
      # set label of career type other than top career type to blank
      ccGenAll[ccGenAll$career_type != topType, ]$lab <- ""
      
      # add number info to country
      ccGenAll$country_origin <- as.character(ccGenAll$country_origin)
      ccGenAll$country_origin <- paste(ccGenAll$country_origin, paste0("N=",ccGenAll$catcnt), sep="\n")
      ccGenAll$country_origin <- factor(ccGenAll$country_origin)
      # (3/27/18)
      labGen <- subset(ccGenAll, career_type == topType)
      
      p3 <- ggplot(ccGenAll,aes(x=country_origin,y=career_type)) +
        geom_point(aes(size=percent,fill=mpct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Gender\n(% Male)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p3 <- p3 + geom_label(data=labGen, aes(x=country_origin,y=career_type,label=lab))
      p3 <- p3 + scale_fill_gradient2(name="Male \n(% within Country)",low="deeppink2",mid="white", high="dodgerblue1", midpoint=0.5, labels=scales::percent)
      # p3 <- p3 + labs(x="Country of Origin", y= "Caeer Type")
      p3 <- p3 + labs(x="Country of Origin", y= "")
      p3 <- p3 + title("Gender Distribution in Different Caeer Type\nacross Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_function, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSpcAll,by=c("country_origin","job_function"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%M")
      ccGenAll$lab <- pgenLabel
      # set label of job function other than top job function to blank
      ccGenAll[ccGenAll$job_function != topFunc, ]$lab <- ""
      
      # add number info to country
      ccGenAll$country_origin <- as.character(ccGenAll$country_origin)
      ccGenAll$country_origin <- paste(ccGenAll$country_origin, paste0("N=",ccGenAll$catcnt), sep="\n")
      ccGenAll$country_origin <- factor(ccGenAll$country_origin)
      # (3/27/18)
      labGen <- subset(ccGenAll, job_function == topFunc)
      
      p3 <- ggplot(ccGenAll,aes(x=country_origin,y=job_function)) +
        geom_point(aes(size=percent,fill=mpct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Function\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p3 <- p3 + geom_label(data=labGen, aes(x=country_origin,y=job_function,label=lab))
      p3 <- p3 + scale_fill_gradient2(name="Gender\n(% Male)",low="deeppink2",mid="white", high="dodgerblue1", midpoint=0.5, labels=scales::percent)
      # p3 <- p3 + labs(x="Country of Origin", y= "Job Function")
      p3 <- p3 + labs(x="Country of Origin", y= "")
      p3 <- p3 + title("Gender Distribution in Different Job Function\nacross Top 5 Countries")
    }
    p3
  },height=ctHeight,width=ctWidth)
  
  output$cntrGenderTxt <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Gender Ratio</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with gender composition (100% male to 0% male)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of males within the total selected job sector.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Gender Ratio</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with gender composition (100% male to 0% male)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the career types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting career types within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of males within the total selected career type.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Gender Ratio</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with gender composition (100% male to 0% male)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job function within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job function within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of males within the total selected job function.</p><p>&nbsp;</p>")
    }
  })
  
  # gender data table for top countries
  output$cntrGenderTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_sector, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%")
      ccGenAll$Male_Percent <- pgenLabel
      ccGenAll$mpct <- NULL
      ccGenAll <- changeTableHeader(ccGenAll,c('Country_Origin','Job_Sector','Gender','Gender_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_Male'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, career_type, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccJtpAll,by=c("country_origin","career_type"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%")
      ccGenAll$Male_Percent <- pgenLabel
      ccGenAll$mpct <- NULL
      ccGenAll <- changeTableHeader(ccGenAll,c('Country_Origin','Career_Type','Gender','Gender_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_Male'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      # gender count
      ccGenAll <- topData %>% group_by(country_origin, job_function, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSpcAll,by=c("country_origin","job_function"))
      
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # Label
      pgenLabel=paste0(round(ccGenAll$mpct,2)*100,"%")
      ccGenAll$Male_Percent <- pgenLabel
      ccGenAll$mpct <- NULL
      ccGenAll <- changeTableHeader(ccGenAll,c('Country_Origin','Job_Function','Gender','Gender_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_Male'))
    }
    ccGenAll
  })  

  # job location plot for top countries
  output$cntrLocationPlot <- renderPlot({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    # count job_sector
    ccSctCnt <- topData %>% group_by(job_sector) %>% summarise (cnt = n())
    topSect <- setSect
    # count career_type
    ccTypCnt <- topData %>% group_by(career_type) %>% summarise (cnt = n())
    # topType <- ccTypCnt$career_type[ccTypCnt$cnt == max(ccTypCnt$cnt)]
    topType <- setType
    # count job_job_function
    ccSpcCnt <- topData %>% group_by(job_function) %>% summarise (cnt = n())
    topFunc <- setFunc
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_sector, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%US")
      ccLocAll$lab <- plocLabel
      # set label of job sector other than top sector to blank
      ccLocAll[ccLocAll$job_sector != topSect, ]$lab <- ""
      
      # add number info to country
      ccLocAll$country_origin <- as.character(ccLocAll$country_origin)
      ccLocAll$country_origin <- paste(ccLocAll$country_origin, paste0("N=",ccLocAll$catcnt), sep="\n")
      ccLocAll$country_origin <- factor(ccLocAll$country_origin)
      # (3/27/18)
      labLoc <- subset(ccLocAll, job_sector == topSect)
      
      p4 <- ggplot(ccLocAll,aes(x=country_origin,y=job_sector)) +
        geom_point(aes(size=percent,fill=upct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Employment Sector\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p4 <- p4 + geom_label(data=labLoc, aes(x=country_origin,y=job_sector,label=lab))
      p4 <- p4 + scale_fill_gradient2(name="Job Location\n(% working\nin US)",low="purple4",mid="white", high="green4", midpoint=0.5, labels=scales::percent)
      # p4 <- p4 + labs(x="Country of Origin", y= "Employment Sector")
      p4 <- p4 + labs(x="Country of Origin", y= "")
      p4 <- p4 + title("Job Location in Different Employment Sectors\nacross Top 5 Countries")
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, career_type, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccJtpAll,by=c("country_origin","career_type"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%US")
      ccLocAll$lab <- plocLabel
      # set label of career type other than top career type to blank
      ccLocAll[ccLocAll$career_type != topType, ]$lab <- ""
      
      # add number info to country
      ccLocAll$country_origin <- as.character(ccLocAll$country_origin)
      ccLocAll$country_origin <- paste(ccLocAll$country_origin, paste0("N=",ccLocAll$catcnt), sep="\n")
      ccLocAll$country_origin <- factor(ccLocAll$country_origin)
      # (3/27/18)
      labLoc <- subset(ccLocAll, career_type == topType)
      
      p4 <- ggplot(ccLocAll,aes(x=country_origin,y=career_type)) +
        geom_point(aes(size=percent,fill=upct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Caeer Type\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p4 <- p4 + geom_label(data=labLoc, aes(x=country_origin,y=career_type,label=lab))
      p4 <- p4 + scale_fill_gradient2(name="Job Location\n(% working\nin US)",low="purple4",mid="white", high="green4", midpoint=0.5, labels=scales::percent)
      # p4 <- p4 + labs(x="Country of Origin", y= "Caeer Type")
      p4 <- p4 + labs(x="Country of Origin", y= "")
      p4 <- p4 + title("Job Location in Different Caeer Type\nacross Top 5 Countries")
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_function, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSpcAll,by=c("country_origin","job_function"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%US")
      ccLocAll$lab <- plocLabel
      # set label of job function other than top job function to blank
      ccLocAll[ccLocAll$job_function != topFunc, ]$lab <- ""
      
      # add number info to country
      ccLocAll$country_origin <- as.character(ccLocAll$country_origin)
      ccLocAll$country_origin <- paste(ccLocAll$country_origin, paste0("N=",ccLocAll$catcnt), sep="\n")
      ccLocAll$country_origin <- factor(ccLocAll$country_origin)
      # (3/27/18)
      labLoc <- subset(ccLocAll, job_function == topFunc)
      
      p4 <- ggplot(ccLocAll,aes(x=country_origin,y=job_function)) +
        geom_point(aes(size=percent,fill=upct),alpha=0.9,color="gray48",shape=21) +
        scale_size_area(name="Job Function\n(% within Country)",labels=scales::percent,max_size=20,guide = FALSE) +
        theme(panel.background = element_rect(fill = 'white', colour = 'black'), 
              panel.grid.major.x = element_line(colour = "gray", linetype="solid"),
              panel.grid.major.y = element_line(colour = "gray", linetype="dashed"),
              axis.text = element_text(size=11))
      p4 <- p4 + geom_label(data=labLoc, aes(x=country_origin,y=job_function,label=lab))
      p4 <- p4 + scale_fill_gradient2(name="Job Location\n(% working\nin US)",low="purple4",mid="white", high="green4", midpoint=0.5, labels=scales::percent)
      # p4 <- p4 + labs(x="Country of Origin", y= "Job Function")
      p4 <- p4 + labs(x="Country of Origin", y= "")
      p4 <- p4 + title("Job Location in Different Job Function\nacross Top 5 Countries")
    }
    p4
  },height=ctHeight,width=ctWidth)
  
  output$cntrLocationTxt <- renderText({
    if (input$selectCtPc == 1) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Work Location</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with work location (100% working in US to 0% working in the US)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job sectors within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job sectors within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of those working in the US within the total selected job sector.</p><p>&nbsp;</p>")
    }
    else if (input$selectCtPc == 2) {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Work Location</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with work location (100% working in US to 0% working in the US)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the career types within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting career types within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of those working in the US within the total selected career type.</p><p>&nbsp;</p>")
    }
    else {
      HTML("<p><strong>Variations Among the 'Top 5' Countries in Work Location</strong>
           The legend depicting the size of the circles (from the job category panel) applies to all panels because the <u>job category data is repeated and overlaid with work location (100% working in US to 0% working in the US)</u>. 
           Percentages represented by circle size are based on the alumni outcomes within each individual country. 
           For example, all circles on the solid gray line connecting the job function within the US sums to 100% of US alumni. 
           All circles on the solid gray line connecting job function within China sums to 100% of Chinese alumni, etc. 
           Percentages shown as text indicate the relative percentage of those working in the US within the total selected job function.</p><p>&nbsp;</p>")
    }
  })

  # job location data table for top countries
  output$cntrLocationTable <- renderTable({
    # choose data based on input$selectCtTp from ui.R
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = ctryData
    }
    else {
      topData = subset(ctryData, years == timeConvert(input$selectCtTp))
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(country_origin, job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSctAll)
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_sector, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("country_origin","job_sector"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%")
      ccLocAll$Percent_in_US <- plocLabel
      ccLocAll$upct <- NULL
      ccLocAll <- changeTableHeader(ccLocAll,c('Country_Origin','Job_Sector','Job_Location','Location_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_InUS'))
    }
    else if (input$selectCtPc == 2) {
      ccJtpAll <- topData %>% group_by(country_origin, career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccJtpAll)
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, career_type, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccJtpAll,by=c("country_origin","career_type"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%")
      ccLocAll$Percent_in_US <- plocLabel
      ccLocAll$upct <- NULL
      ccLocAll <- changeTableHeader(ccLocAll,c('Country_Origin','Career_Type','Job_Function','Location_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_InUS'))
    }
    else {
      ccSpcAll <- topData %>% group_by(country_origin, job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt)) %>% group_by(country_origin) %>% mutate(catcnt=sum(cnt))
      setDT(ccSpcAll)
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      
      # count location in US
      ccLocAll <- clocData %>% group_by(country_origin, job_function, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSpcAll,by=c("country_origin","job_function"))
      
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      # Label
      plocLabel=paste0(round(ccLocAll$upct,2)*100,"%")
      ccLocAll$Percent_in_US <- plocLabel
      ccLocAll$upct <- NULL
      ccLocAll <- changeTableHeader(ccLocAll,c('Country_Origin','Job_Function','Job_Location','Location_Count','Alumni_Count','Alumni_Portion','Total_FromCountryOrigin','Percent_InUS'))
    }
    ccLocAll
  })  

  # top countries dynamic UI
  output$cntrDynamicUI<-renderUI({
    # zoom job category plot
    if (input$selectShowCt == 2) {
      fluidRow(
        tabBox(
          title = "Job category", width = 12,
          # The id lets us use input$ctJob on the server to find the current tab
          id = "tab_ctJob",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrJobTxt"), plotOutput("cntrJobPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrJobTable"))
        )
      )
    }
    # zoom training time plot
    else if (input$selectShowCt == 3) {
      fluidRow(
        tabBox(
          title = "Training time", width = 12,
          # The id lets us use input$tab_ctTime on the server to find the current tab
          id = "tab_ctTime",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrTimeTxt"), plotOutput("cntrTimePlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrTimeTable"))
        )
      )
    }
    # zoom gender plot
    else if (input$selectShowCt == 4) {
      fluidRow(
        tabBox(
          title = "Gender", width = 12,
          # The id lets us use input$tab_ctGender on the server to find the current tab
          id = "tab_ctGender",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrGenderTxt"), plotOutput("cntrGenderPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrGenderTable"))
        )
      )
    }
    # zoom location bubble plot
    else if (input$selectShowCt == 5) {
      fluidRow(
        tabBox(
          title = "Location", width = 12,
          # The id lets us use input$tab_ctLocation on the server to find the current tab
          id = "tab_ctLocation",
          tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrLocationTxt"), plotOutput("cntrLocationPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrLocationTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
          tabBox(
            title = "Job category", width = 6,
            # The id lets us use input$ctJobSml on the server to find the current tab
            id = "tab_ctJobSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrJobTxt"), plotOutput("cntrJobPlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrJobTable"))
          ),
          tabBox(
            title = "Training time", width = 6,
            # The id lets us use input$ctTimeSml on the server to find the current tab
            id = "tab_ctTimeSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrTimeTxt"), plotOutput("cntrTimePlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrTimeTable"))
          )
        ),
        column(width = 12,
          tabBox(
            title = "Gender", width = 6,
            # The id lets us use input$tab_ctGenderSml on the server to find the current tab
            id = "tab_ctGenderSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrGenderTxt"), plotOutput("cntrGenderPlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrGenderTable"))
          ),
          tabBox(
            title = "Location", width = 6,
            # The id lets us use input$ctLocationSml on the server to find the current tab
            id = "tab_ctLocationSml",
            tabPanel("Bubble plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("cntrLocationTxt"), plotOutput("cntrLocationPlot")),
            tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("cntrLocationTable"))
          )
        )
      )
    }
  })
  
  # top countries information boxes
  output$cntrInfoBox<-renderUI({
    if (input$selectCtTp == 'Graduated in 2000-2014') {
      topData = dataGroup
    }
    else {
      topData = dataGroup[dataGroup$years == timeConvert(input$selectCtTp), ]
    }
    
    if (input$selectCtPc == 1) {
      ccSctAll <- topData %>% group_by(job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
      # topSect <- ccSctAll$job_sector[ccSctAll$cnt == max(ccSctAll$cnt)]
      topSect <- setSect

      # mean time
      mt2 <- aggregate(train_time~job_sector,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("job_sector"))

      # gender count
      ccGenAll <- topData %>% group_by(job_sector, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("job_sector"))
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      # count location in US
      ccLocAll <- clocData %>% group_by(job_sector, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("job_sector"))
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)
      
      boxData = list(paste0("Sector: '",topSect,"'"),
                     ccSctAll$percent[ccSctAll$job_sector==topSect],
                     ccMntAll$train_time[ccMntAll$job_sector==topSect],
                     ccGenAll$mpct[ccGenAll$job_sector==topSect],
                     ccLocAll$upct[ccLocAll$job_sector==topSect])
    }
    else if (input$selectCtPc == 2) {
      ccSctAll <- topData %>% group_by(career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
      # topType <- ccSctAll$career_type[ccSctAll$cnt == max(ccSctAll$cnt)]
      topType <- setType
      
      # mean time
      mt2 <- aggregate(train_time~career_type,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("career_type"))
      
      # gender count
      ccGenAll <- topData %>% group_by(career_type, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("career_type"))
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      # count location in US
      ccLocAll <- clocData %>% group_by(career_type, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("career_type"))
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)

      boxData = list(paste0("Type: '",topType,"'"),
                     ccSctAll$percent[ccSctAll$career_type==topType],
                     ccMntAll$train_time[ccMntAll$career_type==topType],
                     ccGenAll$mpct[ccGenAll$career_type==topType],
                     ccLocAll$upct[ccLocAll$career_type==topType])
    }
    else {
      ccSctAll <- topData %>% group_by(job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
      # topFunc <- ccSctAll$job_function[ccSctAll$cnt == max(ccSctAll$cnt)]
      topFunc <- setFunc
      
      # mean time
      mt2 <- aggregate(train_time~job_function,data=topData,FUN=mean)
      # merge data set 
      ccMntAll <- merge(ccSctAll,mt2,by=c("job_function"))
      
      # gender count
      ccGenAll <- topData %>% group_by(job_function, gender) %>% summarise (gcnt = n())
      # merge data set 
      ccGenAll <- merge(ccGenAll,ccSctAll,by=c("job_function"))
      # gender freq
      ccGenAll$mpct <- ccGenAll$gcnt/ccGenAll$cnt
      ccGenAll$mpct <- ifelse(ccGenAll$gender%in%"Male",ccGenAll$mpct,1-ccGenAll$mpct)
      
      # location in US or not
      clocData <- topData
      clocData$loc <- ifelse(clocData$job_country == "United States","US","NOT")
      # count location in US
      ccLocAll <- clocData %>% group_by(job_function, loc) %>% summarise (lcnt = n())
      # merge data set 
      ccLocAll <- merge(ccLocAll,ccSctAll,by=c("job_function"))
      # location freq
      ccLocAll$upct <- ccLocAll$lcnt/ccLocAll$cnt
      ccLocAll$upct <- ifelse(ccLocAll$loc == "US",ccLocAll$upct,1-ccLocAll$upct)

      boxData = list(paste0("Job Function: '", topFunc, "'"),
                     ccSctAll$percent[ccSctAll$job_function==topFunc],
                     ccMntAll$train_time[ccMntAll$job_function==topFunc],
                     ccGenAll$mpct[ccGenAll$job_function==topFunc],
                     ccLocAll$upct[ccLocAll$job_function==topFunc])
    }
    

    box(title = paste0("General Distribution of Selected ", boxData[[1]]),
      width = 12,
      fluidRow(
        valueBox(subtitle="Percent in this selected job", value=tags$p(paste0(round(boxData[[2]],3)*100,"%"),style="font-size:75%;"), icon = icon("th-large"), color = "maroon", width=3),
        valueBox(subtitle="Average training time (years)", value=tags$p(round(boxData[[3]],1),style="font-size:75%;"), icon = icon("hourglass-3"), color = "purple", width=3),
        valueBox(subtitle="Percent male", value=tags$p(paste0(round(boxData[[4]],3)*100,"%"),style="font-size:75%;"), icon = icon("male"), color = "blue", width=3),
        valueBox(subtitle="Percent working in US ", value=tags$p(paste0(round(boxData[[5]],3)*100,"%"),style="font-size:75%;"), icon = icon("home"), color = "green", width=3)
      )
    )
  })
  #<<< top countries
  
  
  #############################################################################
  # Degree >>>
  #############################################################################
  
  
  #>>> degree
  # https://moderndata.plot.ly/easy-error-bars-with-r-and-plotly/; http://rpubs.com/chelsea/68601
  # https://plot.ly/r/error-bars/
  # https://stackoverflow.com/questions/42905686/error-bars-in-plot-ly
  # https://plotly-book.cpsievert.me/scatter-traces.html
  # https://stackoverflow.com/questions/44638590/plotly-in-r-format-axis-tick-labels-to-percentage
  # https://plot.ly/r/bar-charts/; https://plot.ly/r/shiny-gallery/
  dgHeight <- function() {
    input$sldHeightDg
  }
  dgWidth <- function() {
    input$sldWidthDg
  }
  
  # job sector plot for study degree fields
  output$degrJSectPlot <- renderPlot({
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    topSect <- setSect
    
    dgcTypeAll <- degrData %>% group_by(major, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(major) %>% mutate(catcnt=sum(cnt))
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrType <- subset(degrType, catcnt >= 10)
    }
    else {
      degrType <- subset(degrType, catcnt >= 5)
    }
    degrType_TT <- subset(degrType, job_sector == topSect)
    # (1/8/18: change test from prop.test to binom.test due to error warning 'In prop.test(cnt, catcnt) : Chi-squared approximation may be incorrect')
    # degrType_TTci <- degrType_TT %>% mutate(low=prop.test(cnt,catcnt)$conf.int[1], upper=prop.test(cnt,catcnt)$conf.int[2])
    degrType_TTci <- degrType_TT %>% mutate(low=binom.test(cnt,catcnt)$conf.int[1], upper=binom.test(cnt,catcnt)$conf.int[2])
    
    # sort degree by percentage in tenure track
    # http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
    #   x$name <- factor(x$name, levels = x$name[order(x$val)])
    degrType_TTci$major <- as.character(degrType_TTci$major)
    degrType_TTci$major <- wrap_strings(degrType_TTci$major,30)
    srtDegrTTpct <- factor(degrType_TTci$major, levels=degrType_TTci$major[order(degrType_TTci$freq)])
    degrType_TTci$major <- srtDegrTTpct
    pctLabel=paste0(round(degrType_TTci$freq,2) * 100,"%")
    
    p <- ggplot(degrType_TTci,aes(major, y=freq, ymin=low, ymax=upper)) + 
      geom_errorbar(color="grey", width=0.7) + geom_point(stat="identity", size=3) + coord_flip() +
      scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5))
    # (1/8/18: fix the ggplot2 text label problem: "Error in eval(expr, envir, enclos) : object '.group' not found")
    # p <- p + geom_text(data=degrType_TTci, aes(y = freq+0.05, label = pctLabel)) + theme_classic() + 
    p <- p + geom_text(aes(label = pctLabel), hjust=-0.2,vjust=-0.2) + theme_classic() + 
      theme(axis.text=element_text(size=12, face = "bold"), axis.title=element_text(size=14,face="bold")) +
      # labs(x="Degree Study Field", y= paste0("Proportion in '", topSect, "'"),
      labs(x="", y= paste0("Proportion in '", topSect, "'"),
           panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    p
  },height=dgHeight,width=dgWidth)
  
  output$degrJSectTxt  <- renderText({HTML(paste0("<p><strong>Percentage of Alumni that Enter into ", setSect, " Positions Based on Their Doctoral Degree Field</strong>",
                                           " The relative percentage of alumni within each degree field entering into ", setSect, " positions is shown. The 95% confidence intervals for the binomial proportion are shown here.</p><p>&nbsp;</p>"))})
  
  # job sector data table for study degree fields
  output$degrJSectTable <- renderTable({
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    dgcTypeAll <- degrData %>% group_by(major, job_sector) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(major) %>% mutate(catcnt=sum(cnt))
    changeTableHeader(degrType,c('Degree_Field','Job_Sector','Alumni_Count','Alumni_Portion','TotalAlmumni_InDegree'))
  })  
  
  # career type plot for study degree fields
  output$degrJTypePlot <- renderPlot({
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    topType <- setType
    
    dgcTypeAll <- degrData %>% group_by(major, career_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(major) %>% mutate(catcnt=sum(cnt))
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrType <- subset(degrType, catcnt >= 10)
    }
    else {
      degrType <- subset(degrType, catcnt >= 5)
    }
    degrType_TT <- subset(degrType, career_type == topType)
    # (1/8/18: change test from prop.test to binom.test due to error warning 'In prop.test(cnt, catcnt) : Chi-squared approximation may be incorrect')
    # degrType_TTci <- degrType_TT %>% mutate(low=prop.test(cnt,catcnt)$conf.int[1], upper=prop.test(cnt,catcnt)$conf.int[2])
    degrType_TTci <- degrType_TT %>% mutate(low=binom.test(cnt,catcnt)$conf.int[1], upper=binom.test(cnt,catcnt)$conf.int[2])
    
    # sort degree by percentage in tenure track
    # http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
    #   x$name <- factor(x$name, levels = x$name[order(x$val)])
    degrType_TTci$major <- as.character(degrType_TTci$major)
    degrType_TTci$major <- wrap_strings(degrType_TTci$major,30)
    srtDegrTTpct <- factor(degrType_TTci$major, levels=degrType_TTci$major[order(degrType_TTci$freq)])
    degrType_TTci$major <- srtDegrTTpct
    pctLabel=paste0(round(degrType_TTci$freq,2) * 100,"%")
    
    p <- ggplot(degrType_TTci,aes(major, y=freq, ymin=low, ymax=upper)) + 
      geom_errorbar(color="grey", width=0.7) + geom_point(stat="identity", size=3) + coord_flip() +
      scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5))
    # (1/8/18: fix the ggplot2 text label problem: "Error in eval(expr, envir, enclos) : object '.group' not found")
    # p <- p + geom_text(data=degrType_TTci, aes(y = freq+0.05, label = pctLabel)) + theme_classic() + 
    p <- p + geom_text(aes(label = pctLabel), hjust=-0.2,vjust=-0.2) + theme_classic() + 
      theme(axis.text=element_text(size=12, face = "bold"), axis.title=element_text(size=14,face="bold")) +
      # labs(x="Degree Study Field", y= paste0("Proportion in '", topType, "'"),
      labs(x="", y= paste0("Proportion in '", topType, "'"),
           panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    p
  },height=dgHeight,width=dgWidth)

  output$degrJTypeTxt  <- renderText({HTML(paste0("<p><strong>Percentage of Alumni that Enter into ", setType, " Positions Based on Their Doctoral Degree Field</strong>",  
                                           " The relative percentage of alumni within each degree field entering into ", setType, " positions is shown. The 95% confidence intervals for the binomial proportion are shown here.</p><p>&nbsp;</p>"))})
  
  # career type data table for study degree fields
  output$degrJTypeTable <- renderTable({
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    dgcTypeAll <- degrData %>% group_by(major, career_type) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(major) %>% mutate(catcnt=sum(cnt))
    changeTableHeader(degrType,c('Degree_Field','Career_Type','Alumni_Count','Alumni_Portion','TotalAlmumni_InDegree'))
  })
  
  # job function plot for study degree fields
  output$degrJFuncPlot <- renderPlot({
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    topFunc <- setFunc
    
    dgcTypeAll <- degrData %>% group_by(major, job_function) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(major) %>% mutate(catcnt=sum(cnt))
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrType <- subset(degrType, catcnt >= 10)
    }
    else {
      degrType <- subset(degrType, catcnt >= 5)
    }
    degrType_TT <- subset(degrType, job_function == topFunc)
    # (1/8/18: change test from prop.test to binom.test due to error warning 'In prop.test(cnt, catcnt) : Chi-squared approximation may be incorrect')
    #   http://www.sthda.com/english/wiki/one-proportion-z-test-in-r
    # degrType_TTci <- degrType_TT %>% mutate(low=prop.test(cnt,catcnt)$conf.int[1], upper=prop.test(cnt,catcnt)$conf.int[2])
    degrType_TTci <- degrType_TT %>% mutate(low=binom.test(cnt,catcnt)$conf.int[1], upper=binom.test(cnt,catcnt)$conf.int[2])
    
    # sort degree by percentage in tenure track
    # http://rstudio-pubs-static.s3.amazonaws.com/7433_4537ea5073dc4162950abb715f513469.html
    #   x$name <- factor(x$name, levels = x$name[order(x$val)])
    degrType_TTci$major <- as.character(degrType_TTci$major)
    degrType_TTci$major <- wrap_strings(degrType_TTci$major,30)
    srtDegrTTpct <- factor(degrType_TTci$major, levels=degrType_TTci$major[order(degrType_TTci$freq)])
    degrType_TTci$major <- srtDegrTTpct
    pctLabel=paste0(round(degrType_TTci$freq,2) * 100,"%")
    
    p <- ggplot(degrType_TTci,aes(major, y=freq, ymin=low, ymax=upper)) + 
      geom_errorbar(color="grey", width=0.7) + geom_point(stat="identity", size=3) + coord_flip() +
      scale_y_continuous(labels=scales::percent, breaks=pretty_breaks(n=5))
    # (1/8/18: fix the ggplot2 text label problem: "Error in eval(expr, envir, enclos) : object '.group' not found")
    # p <- p + geom_text(data=degrType_TTci, aes(y = freq+0.05, label = pctLabel)) + theme_classic() + 
    p <- p + geom_text(aes(label = pctLabel), hjust=-0.2,vjust=-0.2) + theme_classic() + 
      theme(axis.text=element_text(size=12, face = "bold"), axis.title=element_text(size=14,face="bold")) +
      # labs(x="Degree Study Field", y= paste0("Proportion in '", topFunc, "'"),
      labs(x="", y= paste0("Proportion in '", topFunc, "'"),
           panel.border = element_blank(), panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    p
  },height=dgHeight,width=dgWidth)

  output$degrJFuncTxt  <- renderText({HTML(paste0("<p><strong>Percentage of Alumni that Enter into ", setFunc, " Positions Based on Their Doctoral Degree Field</strong>", 
                                           " The relative percentage of alumni within each degree field entering into ", setFunc, " positions is shown. The 95% confidence intervals for the binomial proportion are shown here.</p><p>&nbsp;</p>"))})
  
  # job function data table for study degree fields
  output$degrJFuncTable <- renderTable({
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      degrData = dataGroup
    }
    else {
      degrData = dataGroup[dataGroup$years == (input$selectDgTp), ]
    }
    
    dgcTypeAll <- degrData %>% group_by(major, job_function) %>% summarise (cnt = n()) %>% mutate(freq=cnt/sum(cnt))
    degrType <- dgcTypeAll %>% group_by(major) %>% mutate(catcnt=sum(cnt))
    changeTableHeader(degrType,c('Degree_Field','Job_Function','Alumni_Count','Alumni_Portion','TotalAlmumni_InDegree'))
  })  
  
  # degree study field dynamic UI
  output$degrDynamicUI<-renderUI({
    # zoom job category plot
    if (input$selectShowDg == 2) {
      fluidRow(
        tabBox(
          title = "Enployment sector", width = 12,
          # The id lets us use input$tab_dgSect on the server to find the current tab
          id = "tab_dgSect",
          tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJSectTxt"), plotOutput("degrJSectPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJSectTable"))
        )
      )
    }
    # zoom training time plot
    else if (input$selectShowDg == 3) {
      fluidRow(
        tabBox(
          title = "Career type", width = 12,
          # The id lets us use input$tab_dgType on the server to find the current tab
          id = "tab_dgType",
          tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJTypeTxt"), plotOutput("degrJTypePlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJTypeTable"))
        )
      )
    }
    # zoom gender plot
    else if (input$selectShowDg == 4) {
      fluidRow(
        tabBox(
          title = "Job function", width = 12,
          # The id lets us use input$tab_dgFunc on the server to find the current tab
          id = "tab_dgFunc",
          tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJFuncTxt"), plotOutput("degrJFuncPlot")),
          tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJFuncTable"))
        )
      )
    }
    # default to show all
    else {
      fluidRow(
        column(width = 12,
               tabBox(
                 title = "Enployment sector", width = 6,
                 # The id lets us use input$tab_dgSectSml on the server to find the current tab
                 id = "tab_dgSectSml",
                 tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJSectTxt"), plotOutput("degrJSectPlot")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJSectTable"))
               ),
               tabBox(
                 title = "Career type", width = 6,
                 # The id lets us use input$tab_dgTypeSml on the server to find the current tab
                 id = "tab_dgTypeSml",
                 tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJTypeTxt"), plotOutput("degrJTypePlot")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJTypeTable"))
               )
        ),
        column(width = 12,
               tabBox(
                 title = "Job function", width = 6,
                 # The id lets us use input$tab_dgFuncSml on the server to find the current tab
                 id = "tab_dgFuncSml",
                 tabPanel("Pointrange plot", style = "overflow-x:scroll; overflow-y:scroll; height: 700px", htmlOutput("degrJFuncTxt"), plotOutput("degrJFuncPlot")),
                 tabPanel("Data table", style = "overflow-x:scroll; overflow-y:scroll; max-height: 400px", tableOutput("degrJFuncTable"))
               )
        )
      )
    }
  })
  
  # degree study field information boxes
  output$degrInfoBox<-renderUI({
    if (input$selectDgTp == 'Graduated in 2000-2014') {
      dgrData = dataGroup
    }
    else {
      dgrData = dataGroup[dataGroup$years == timeConvert(input$selectDgTp), ]
    }
    
    ccSctAll <- dgrData %>% group_by(job_sector) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
    topSect <- setSect
    topSectPct <- ccSctAll$percent[ccSctAll$job_sector == topSect]
      
    ccTypAll <- dgrData %>% group_by(career_type) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
    topType <- setType
    topTypePct <- ccTypAll$percent[ccTypAll$career_type == topType]

    ccSpcAll <- dgrData %>% group_by(job_function) %>% summarise (cnt = n()) %>% mutate(percent=cnt/sum(cnt))
    topFunc <- setFunc
    topFuncPct <- ccSpcAll$percent[ccSpcAll$job_function == topFunc]

    box(title = "General distribution of selected job category",
        width = 12,
        fluidRow(
          valueBox(subtitle=paste0("Proportion in Sector: '",topSect,"'"), value=tags$p(paste0(round(topSectPct,3)*100,"%"),style="font-size:75%;"), icon = icon("th-large"), color = "maroon", width=4),
          valueBox(subtitle=paste0("Proportion in Type: '",topType,"'"), value=tags$p(paste0(round(topTypePct,3)*100,"%"),style="font-size:75%;"), icon = icon("th-list"), color = "blue", width=4),
          valueBox(subtitle=paste0("Proportion in Function: '",topFunc,"'"), value=tags$p(paste0(round(topFuncPct,3)*100,"%"),style="font-size:75%;"), icon = icon("th"), color = "green", width=4)
        )
    )
  })
  #<<< degree
  
  
  #############################################################################
  #############################################################################
  # Download >>>
  #############################################################################
  
  output$downloadCsv <- downloadHandler(
    filename = "rawData.csv",
    content = function(file) {
      write.csv(dataGroup, file)
    },
    contentType = "text/csv"
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(head(dataGroup, 5))
    options(orig)
  })
}



#### Use custom local image files as icons in a Shiny Dashboard value box 
#>>> https://gist.github.com/hrbrmstr/605e62c5bf6deadf304d80cf4b1f0239
#### R shiny dashboard how to use animated icons?
#>>> https://stackoverflow.com/questions/41454394/r-shiny-dashboard-how-to-use-animated-icons
#### Splunk shiny icons
#>>> https://www.splunk.com/blog/2015/06/25/shiny-icons.html
#### Likert plot by ggplot2
#>>> http://rnotr.com/likert/ggplot/barometer/likert-plots/
