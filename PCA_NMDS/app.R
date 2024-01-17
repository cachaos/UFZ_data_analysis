# load required packages -----
if (require(gplots) == F) {
  install.packages('gplots')
}
if (require(ggplot2) == F) {
  install.packages('ggplot2')
}
if (require(dplyr) == F) {
  install.packages('dplyr')
}
if (require(miscTools) == F) {
  install.packages('miscTools')
}
if (require(vegan) == F) {
  install.packages('vegan')
}
if (require(ggbiplot) == F) {
  if (require(devtools) == F) {
    install.packages('devtools')
  }
  require(devtools)
  install_github("vqv/ggbiplot")
}
if (require(grid) == F) {
  install.packages(grid)
}
if (require(devEMF) == F) {
  install.packages(devEMF)
}

require(gplots)
require(ggplot2)
library(plotly)
require(dplyr)
require(miscTools)
require(vegan)
require(ggbiplot)
require(grid)
require(devEMF)

library(shiny)
library(shinyjs)
library(data.table)
library(DT)


# Define UI ----
ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarLayout(
  ## File Inputs in Sidebar Panel ----
  sidebarPanel(
    h3("Load Data"),
    fileInput(
      "data",
      "Mandatory input: Choose Data CSV File",
      accept = c("text/csv",
                 "text/comma-separated-values,
                       .csv")
    ),
    fileInput(
      "colors",
      "Optional Input: List of sample colours csv",
      accept = c("text/csv",
                 "text/comma-separated-values,
                       .csv")
    ),
    
    shiny::actionButton(
      "ad",
      "ad"
    ),
    
    selectInput(
      "downloadlist",
      "downloadOptions",
      multiple = T,
      choices = c("Datensatz", "Bild1"),
      selected = NULL,
    ),
    
    #Widgets setting Parameters for downstream Analysis  ----
    ## Export name ----
    textInput("export_name", "Set export name",  value = "test"),
    ## select if input convert NA to 0 or delete whole column----
    checkboxInput("na", "Convert NA to 0", value = TRUE),
    h3("PCA & NMDS"),
    ## select  ifCenter data for PCA ----
    checkboxInput("PCA_center", "Center data for PCA", value = TRUE),
    ## select if Scale data for PCA ----
    checkboxInput("PCA_scale", "Scale data for PCA", value = TRUE),
    ## select if Autotransform data for NMDS ----
    checkboxInput("NMDS_autotransform", "Autotransform data for NMDS", value = TRUE),
    ## Return loadings for PCA axis ----
    checkboxInput("PCA_loadings", "return loadings for PCA axis", value = TRUE),
    ## How many PCA loadings colored ----
    sliderInput(
      "PCA_loadings_colored",
      "how many analyte loadings coloured in plot",
      min = 0,
      max = 100,
      value = 15
    ),
    # max should all analyte loadings: ncol(output$data) ---> dependencyy problem  string "all"),
    ## label loadings with either number of analyte corresponding to table ("number"), ----
    # or name of analyte ("name"), or no label ("none")
    selectInput(
      "PCA_loadings_label",
      "label loadings with",
      choices = list(
        "number of analyte" = 1,
        "name of analyte" = 2,
        "no label" = 3
      ),
      selected = 2
    ),
    ## Size of loading labels ----
    sliderInput(
      "PCA_label_size",
      "Size of loading labels",
      min = 1,
      max = 22,
      value = 2
    ),
    #what range shall this value be in? <--- isnt this already figure parameter???
    h3("Figure parameters"),
    ## Define Group colours for plot (Boolean: TRUE or FALSE) ----
    #checkboxInput("plot_group_colors", "Define group colors for plot", value = FALSE),
    ## label_samples_in_plot <- FALSE ----
    checkboxInput("plot_label_samples", "Label samples in plot", value = FALSE),
    ## label size (good keep as point-size) (numeric) ----
    sliderInput(
      "plot_label_size",
      "Size of labels in plot",
      min = 1,
      max = 22,
      value = 2
    ),
    #what range?
    ## Point size and shape ----
    sliderInput(
      "plot_point_size",
      "Size of points in plot",
      min = 1,
      max = 22,
      value = 2
    ),
    #what range?
    sliderInput(
      "plot_point_shape",
      "Shape of points in plot",
      min = 1,
      max = 22,
      value = 21
    ),
    #what range?
    ## Ellipse confidence plotting (Boolean: TRUE or FALSE) ----
    checkboxInput("plot_ellipses", "Plot ellipse confidence intervall", value = TRUE),
    ## confidence percentage (1-99) for stat ellipse (95 is good) ----
    sliderInput(
      "plot_confidence",
      "confidence percentage for stat ellipse",
      min = 0,
      max = 1,
      value = 0.95
    ),
    #weird name?
    ## type of calculation for ellipse "t" assumes a multivariate t-distribution, ----
    # and "norm" assumes a multivariate normal distribution. "euclid". ("t","norm" or "euclid")
    selectInput(
      "plot_ellipses_type",
      "type of calculation for ellipse",
      choices = c(
        "t distribution" = "t",
        "normal distribution" = "norm",
        "euclidian distribution" = "euclid"
      ),
      selected = "t distribution"
    ),
    ## calculate PCA ellipse areas (Boolean: TRUE or FALSE) ----
    checkboxInput("plot_ellipse_area", "calculate PCA ellipse areas", value = TRUE),
    ## plot centroid of ellipses (Boolean: TRUE or FALSE) ----
    checkboxInput("plot_centroid", "plot centroid of ellipses", value = TRUE),
    ## connect centroids to samples with lines (Boolean: TRUE or FALSE) ----
    checkboxInput(
      "plot_centroid_lines",
      "connect centroids to samples with lines",
      value = FALSE
    ),
    ## Centroid parameters (numeric) ----
    sliderInput(
      "plot_centroid_size",
      "Size of centroids in plot",
      min = 1,
      max = 22,
      value = 2
    ),
    #what range?
    sliderInput(
      "plot_centroid_shape",
      "shape of centroids in plot",
      min = 1,
      max = 22,
      value = 21
    ),
    ## Background colour for Plots (can use "ivory") (R colour name or R colour code) ----
    textInput(
      "plot_background",
      "Background color for plots (R colour names or R color code)",
      value = "whitesmoke"
    ),
    ###how to check if correct value?
    ## In PCA and NMDS plot show grid (Boolean: TRUE or FALSE)
    checkboxInput("plot_grid", "Show grid in PCA and NMDS plot", value = FALSE),
    ##  Width and height in cm of Boxplot export figures (numeric) ----
    ### (height is good as 10 width per sample approx 0.5 is good)
    ## e.g. Box_plot_width <- as.numeric(15)
    sliderInput(
      "plot_box_hight",
      "hight in cm of boxplot export figures",
      min = 0,
      max = 22,
      value = 10
    ),
    sliderInput(
      "plot_box_width",
      "width in cm of boxplot export figures",
      min = 0,
      max = 22,
      value = 0.5
    ),
    h3("Pairwise Analysis"),
    ## should loading be analyzed in pairwise comparisons -----
    checkboxInput(
      "pairwise_loadings",
      "loading analyzed in pairwise comparison",
      value = TRUE
    ),
    ## For pairwise posthoc test choose p-value adjustment for multi testing method ----
    # ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none")
    selectInput(
      "p_adjustment",
      "For pairwise posthoc test choose p-value adjustment for multi testing method",
      choices = list(
        "holm" = 1,
        "hochberg" = 2,
        "hommel" = 3,
        "bonferroni" = 4,
        "BH" = 5,
        "BY" = 6,
        "fdr" = 7,
        "none" = 8
      ),
      selected = 8
    ),
    ## Do pairwise pca plots (Boolean: TRUE or FALSE) ----
    checkboxInput("pairwise_pca_plots", "Do pairwise PCA plots ", value = TRUE),
    ## Do pairwise nmds plots ----
    checkboxInput("pairwise_nmds_plots", "Do pairwise NMDS plots ", value = TRUE),
    ),

  ## Display files as table in Main Panel ----
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("PCA", 
               plotOutput("PCA_plot"),
               downloadButton("download_PCA_pdf", "download PCA Plot as pdf"),
               plotOutput("PCA_diagnostics_plot"),
               downloadButton("download_PCA_diagnostics", "download PCA diagnostics Plot"),
               plotOutput("PCA_eli_area_barplot"),
               downloadButton("download_ellipse_bar", "download PCA ellipse areas barplot"),
               DT::dataTableOutput("PCA_eli_area_df"),
               downloadButton("download_ellipse_df","Download PCA ellipse areas Dataframe"),
               plotOutput("PC1_relative_loadings"),
               plotOutput("PCA_loadings"),
               plotOutput("PC2_relative_loadings"),
               DT::dataTableOutput("PCA_loadings_table"),
               downloadButton("download_PCA_loadings_df", "download PCA loadings DF"),
               plotOutput("pairwise_pca")
               ),
      tabPanel("NMDS", 
               plotOutput("NMDS"),
               plotOutput("NMDS_pairwise")
               ),
      tabPanel("Data and Stats", 
               DT::dataTableOutput("data"),
               downloadButton("downloadData", "download Dataset"),
               DT::dataTableOutput("stats_df"),
               DT::dataTableOutput("settings_df"),
      )
    )
  )
))
# Define server logic ----
server <- shinyServer(function(input, output) {
  shinyjs::useShinyjs()
  ## load data  ----
  df_raw <- reactive({
    req(input$data)
    read.csv(input$data$datapath, header = TRUE, sep = ",")
  })
  #should header be true or fals?
  
  ## checkbox if NA present: convert to 0, unchecked delete whole column ----
  #### Dataset needs to be: Sample, Group, Metabolite1, Metabolite2, Metabolite3
  df_NA <- reactive({
    df <- df_raw()
    colnames(df) [1:2] <- c("Sample","Group")
    if (input$na == TRUE) {
      df[is.na(df)] <- 0
    }
    if (input$na == FALSE) {
      df <- df[, colSums(is.na(df)) == 0]
    }
    return(df)
  })
  
  ### render data ----
  output$data <- DT::renderDataTable(df_NA())
  
  ###  download data ----
  observe({
    shinyjs::toggle("downloadData", !is.null(input$data))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data", Sys.Date(), ".csv")
    },
    content = function(file){
      write.csv(df_NA(), file)
    }
  )
  
  ## analytes  ----
  df_analytes <- reactive ({
    df <- df_NA()
    Analyte_names <- as.vector(colnames(df)[3:length(df)])
    Analyte_mapping <- as.data.frame(cbind(c(1:(ncol(df)-2)),Analyte_names))
    rownames(Analyte_mapping) <- Analyte_mapping[,1]
    colnames(Analyte_mapping) <- c("R_script","Analyte")
    print(Analyte_mapping)
    return(Analyte_mapping)
  })
  
  ## groups in data ----
  df_groups <- reactive({
    df <- df_NA()
    unique_groups <- unique(df[, 2])
    Number_of_Groups <- as.numeric(length (unique_groups))
    groupdata <- as.data.frame(df[, 2])
    Distinct_group_names <- distinct(groupdata)
    return(Distinct_group_names)
  })
  
  ###render df group names ----
  output$group_names <- renderPrint(df_groups())
  
  
  ## load colors ----
  colors_upload <- reactive({
    req(input$colors)
    incolors <- input$colors
    colors <- read.csv(incolors$datapath, header = TRUE, sep = ",")
    colnames(colors) <- c("Group", "Color")
    Group_colors <- as.vector(colors$Color)
    names(Group_colors) <- colors$Group
    return(Group_colors)
  })
  
  ### render colors df ----
  output$colors <- renderPrint(colors_upload())
  
  ## point_colour_black ----
  point_colour_black <- reactive({
    if (input$plot_point_shape > 20) {
      point_colour_black <- TRUE
    }
    else{
      point_colour_black <- FALSE
    }
    return(point_colour_black)
  })
  
  ## PCA-diagnostics plot ----
  
  PCA_diagnostics <- reactive({   
    data <- df_NA()
    data <- data[, colSums(data != 0) != 0]
    sample.data <- data[, 3:ncol(data)]
    sample.name <- data[, 1]
    sample.group <- c(as.character(data [, 2]))
    sample.pca <-
      prcomp(sample.data,
             center = input$PCA_center,
             scale. = input$PCA_scale)
    
    # plot pricipal component variances
    PCA_diagnostics_plot <-
      plot(sample.pca, main = "Principal component variances", type = "l")
  
    dev.copy2pdf(file = "PCA_diagnostics_plot.pdf")
  return(PCA_diagnostics_plot)
  })
  
  ### render PCA_diagnostics plot ----
  output$PCA_diagnostics_plot <- renderPlot(PCA_diagnostics())
  
  ### download PCA_diagnostics plot ----
  observe({
    shinyjs::toggle("download_PCA_diagnostics", !is.null(input$data))
  })
  output$download_PCA_diagnostics <- downloadHandler(
    filename = function() {
      paste("PCA_diagnostics", Sys.Date(), ".pdf")
    },
    content = function(file){
      file.copy("PCA_diagnostics_plot.pdf", file)
    }
  )
  
  ##PCA ----
  PCA <- reactive({    
      data <- df_NA()
      data <- data[, colSums(data != 0) != 0]
      sample.data <- data[, 3:ncol(data)]
      sample.name <- data[, 1]
      sample.group <- c(as.character(data [, 2]))
      sample.pca <-
        prcomp(sample.data,
               center = input$PCA_center,
               scale. = input$PCA_scale)
      PCA_data <- list(sample.pca = sample.pca, sample.data=sample.data, sample.group = sample.group, sample.name = sample.name)  
  return(PCA_data)
  })
  
  
  ## PCA plot ----
  PCA_plot <- reactive({
    data <- df_NA()
    data <- data[, colSums(data != 0) != 0]
    PCA_data <- PCA()
    Export_name <- input$export_name
    sample.pca <- PCA_data$sample.pca
    sample.data <- PCA_data$sample.data
    sample.group <- PCA_data$sample.group
    sample.name <- PCA_data$sample.name
    # Variances
    variances_pca <-
      as.vector((sample.pca$sdev) ^ 2 / sum(sample.pca$sdev ^ 2))
    
    #Calculate significance CHANGE Column start CHANGE Grouping (raw_data$...)
    if (min(sample.data) < 0) {
      Diss_index_method = "euclidean"
    } else{
      Diss_index_method = "bray"
    }
    col_nr <- as.numeric(ncol(data))
    Significance <-
      adonis(formula = data[, 3:col_nr] ~ data$Group,
             data = data,
             method = Diss_index_method)
    stat_all <- Significance$aov.tab
    stat_p <- as.numeric(stat_all[1, 6])
    
    posthoc_permanova <-
      pairwise.adonis(data[, 3:ncol(data)], data$Group)
    
    ## Data-Frame from PCA results
    #make data frame from PCA results
    df_out <- as.data.frame(sample.pca$x)
    df_out$Group <- sample.group
    df_out$Sample <- sample.name
    
    number_of_groups <- as.numeric(length(unique(sample.group)))
    # calculte centroids of groups
    centroids <- aggregate(cbind(PC1, PC2) ~ Group, df_out, mean)
    df_connect <-
      merge(df_out, aggregate(cbind(
        mean.PC1 = PC1, mean.PC2 = PC2
      ) ~ Group, df_out, mean), by = "Group")
    
    if (point_colour_black() == TRUE) {
      g <- ggplot(df_connect, aes(
        x = PC1,
        y = PC2,
        color = Group,
        fill = Group
      )) +
        geom_point(
          aes(fill = Group, colour = Group),
          size = input$plot_point_size,
          shape = input$plot_point_shape,
          colour = "Black"
        )
    } else{
      g <- ggplot(df_connect, aes(
        x = PC1,
        y = PC2,
        color = Group,
        fill = Group
      )) +
        geom_point(
          aes(fill = Group, colour = Group),
          size = input$plot_point_size,
          shape = input$plot_point_shape
        )
      
    }
    
    # if centroids to be plotted and if connencting lines between centroids and data points
    if (input$plot_centroid == TRUE) {
      if (input$plot_centroid_lines == TRUE) {
        g <- g +
          geom_point(
            aes(x = mean.PC1, y = mean.PC2),
            size = input$plot_centroid_size,
            shape = input$plot_centroid_shape
          ) +
          geom_segment(aes(
            x = mean.PC1,
            y = mean.PC2,
            xend = PC1,
            yend = PC2
          ))
      } else{
        #Plot only centroids without connecting lines
        g <-
          g + geom_point(
            data = centroids,
            size = input$plot_centroid_size,
            shape = input$plot_centroid_shape
          )
      }
    }
    # if ellipse with user defined type and confidence level should be drawn
    if (input$plot_ellipses == TRUE) {
      g <- g + stat_ellipse(
        geom = "polygon",
        alpha = 0.3,
        segments = 201,
        level = input$plot_confidence,
        type = input$plot_ellipses_type
      )
    }
    
    # If colours user defined
    if(!is.null(input$colors)) {
      incolors <- colors_upload()
      g <-
        g + scale_fill_manual(values = incolors) + scale_colour_manual(values = incolors)
    }
    
    # if labels (sample names) should be plotted
    if (input$plot_label_samples == TRUE) {
      sample.name <- as.vector(df_connect$Sample)
      g <-
        g + geom_text(
          aes(label = sample.name),
          hjust = 0.5,
          vjust = 1.3,
          size = input$plot_label_size
        )
    }
    
    g <-
      g + theme(legend.direction = 'horizontal', legend.position = 'top')
    g <- g + theme_bw()
    g <-
      g + ggtitle(paste("PCA of",Export_name,"\n (Permanova (adonis) P-value =", stat_p, ")")) +
      xlab(paste(
        "PC 1 (Var. explained ",
        round(100 * variances_pca[1], 2),
        "%)",
        sep = ""
      )) +
      ylab(paste(
        "PC 2 (Var. explained ",
        round(100 * variances_pca[2], 2),
        "%)",
        sep = ""
      )) +
      theme(
        axis.text.x = element_text(
          colour = "Black",
          size = 8,
          angle = 0,
          hjust = 0.5,
          vjust = 0,
          face = "bold"
        ),
        axis.text.y = element_text(
          colour = "black",
          size = 8,
          angle = 0,
          hjust = 1,
          vjust = 0.5,
          face = "bold"
        ),
        axis.title.x = element_text(
          colour = "Black",
          size = 8,
          angle = 0,
          hjust = 0.5,
          vjust = -1,
          face = "bold"
        ),
        axis.title.y = element_text(
          colour = "Black",
          size = 8,
          angle = 90,
          hjust = 0.5,
          vjust = 1,
          face = "bold"
        ),
        plot.title = element_text(
          colour = "black",
          size = 10,
          angle = 0,
          hjust = 0.5,
          vjust = 2,
          face = "bold"
        ),
        legend.title = element_text(
          colour = "black",
          size = 8,
          angle = 0,
          hjust = 0,
          vjust = 2,
          face = "bold"
        ),
        legend.text = element_text(
          colour = "black",
          size = 8,
          angle = 0,
          hjust = 0,
          vjust = 0.5,
          face = "bold"
        ),
        panel.background = element_rect(fill = input$plot_background),
        legend.background = element_rect(fill = input$plot_background)
      )
    
    ## if grid should be shown in plot
    if (input$plot_grid == FALSE) {
      g <-
        g + theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank())
    }
    
    g <-
      g +  annotate(
        "text",
        label = paste("Permanova (adonis) P-value =", stat_p),
        size = 2
      )
    g <- g + theme(aspect.ratio = 1)
    g <-
      g + guides(
        fill = guide_legend(title = "Sample Group"),
        colour = guide_legend(title = "Sample Group")
      )
    return(g)
  })
  
  ### Post-hoc pairwise PERMANOVA (adonis)analysis ####need to do sim-method  = Diss_index_method reactive ----
  pairwise.adonis <-
    function(x,
             factors,
             sim.function = 'vegdist',
             sim.method = "euclidean",
             p.adjust.m = input$p_adjust)
    {
      library(vegan)
      
      co = combn(unique(as.character(factors)), 2)
      pairs = c()
      F.Model = c()
      R2 = c()
      p.value = c()
      
      
      for (elem in 1:ncol(co)) {
        if (sim.function == 'daisy') {
          library(cluster)
          x1 = daisy(x[factors %in% c(co[1, elem], co[2, elem]),], metric = sim.method)
        } else{
          x1 = vegdist(x[factors %in% c(co[1, elem], co[2, elem]),], method = sim.method)
        }
        
        ad = adonis(x1 ~ factors[factors %in% c(co[1, elem], co[2, elem])])
        
        pairs = c(pairs, paste(co[1, elem], 'vs', co[2, elem]))
        
        F.Model = c(F.Model, ad$aov.tab[1, 4])
        
        R2 = c(R2, ad$aov.tab[1, 5])
        
        p.value = c(p.value, ad$aov.tab[1, 6])
      }
      p.adjusted = p.adjust(p.value, method = p.adjust.m)
      sig = c(rep('', length(p.adjusted)))
      sig[p.adjusted <= 1] <- 'ns'
      sig[p.adjusted <= 0.1] <- '.'
      sig[p.adjusted <= 0.05] <- '*'
      sig[p.adjusted <= 0.01] <- '**'
      sig[p.adjusted <= 0.001] <- '***'
      sig[p.adjusted <= 0.0001] <- '****'
      
      pairw.res = data.frame(pairs, F.Model, R2, p.value, p.adjusted, sig)
      print("Signif. codes:  0 '****' 0.0001 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 'ns' 1")
      return(pairw.res)
    }
  
  ### render PCA Plot ----
  output$PCA_plot <- renderPlot({
    ggsave("PCA.pdf", PCA_plot())
    PCA_plot()
    })
  
  ### download PCA Plot ----
  observe({
    shinyjs::toggle("download_PCA_pdf", !is.null(input$data))
  })
      output$download_PCA_pdf <- downloadHandler(
        filename = function() {
          "PCA.pdf"
        },
        content = function(file) {
          file.copy("PCA.pdf", file, overwrite = TRUE)
        }
      )

  ## PCA-ellipse-areas ----
  PCA_ellipse_areas <- reactive({
    Export_name <- input$export_name
    # Get ellipse coordinates from plot and add group names
    pb <- ggplot_build(PCA_plot())
    g <- PCA_plot()
    ellipse_data <- pb$data[[3]][c("fill","x","y")]
    ellipse_sample_groups <- as.data.frame(cbind(c(unique(ellipse_data$fill)),c(unique((g[["data"]][["Group"]])))), stringsAsFactors = FALSE)
    colnames(ellipse_sample_groups) <- c("fill","group")
    ellipse_data_2 <- merge(ellipse_sample_groups,ellipse_data, by ="fill", all.y= TRUE, all.x= FALSE)
    
    ### Loop through each group to get area of each ellipse
    # get groups
    el_group_fills <-  c(unique(ellipse_data_2$fill))
    #make results file
    results_el <- data.frame(Group=character(), 
                             Area=numeric(), 
                             stringsAsFactors=FALSE)
    
    #### Loop 
    for( i in 1: length(el_group_fills)){
      group_fill_i <- el_group_fills[i]
      ellipse_data_i <- subset(ellipse_data_2, fill == group_fill_i)
      group_el_i <-  ellipse_data_i[1,2]
      ellipse_data_i <-ellipse_data_i[,3:4]
      
      # Center of ellipse
      #browser()
      ctr <- MASS::cov.trob(ellipse_data_i)$center  # Per @Roland's comment
      
      # Calculate distance to center from each point on the ellipse
      dist2center <- sqrt(rowSums((t(t(ellipse_data_i)-ctr))^2))
      
      # Calculate area of ellipse from semi-major and semi-minor axes. 
      # These are, respectively, the largest and smallest values of dist2center. 
      area_i <- pi*min(dist2center)*max(dist2center)
      
      results_el[i,] <- c(group_el_i, area_i)
    }
    ### plot areas in barchart
    plot_data_el_area <- results_el
    plot_data_el_area$Area <- as.numeric(plot_data_el_area$Area)
    a <- ggplot(aes(x= Group, y = Area, fill= Group), data= plot_data_el_area)+
      geom_bar(stat="identity", colour="Black", width=0.7)
    
    # if defined colours are present
    if(!is.null(input$colors)) {
      a <- a + scale_fill_manual(values = Group_colours)
    }
    a <- a + theme_bw()
    a <- a + ggtitle(paste("Ellipse areas of PCA -",Export_name)) +
      xlab(paste(""))+
      ylab("Ellipse area")+ 
      theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
            axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
            axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
            axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
            plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
            legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
            legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
            panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))
    
    ## if grid should be shown in plot
    if(input$plot_grid == FALSE){
      a <- a + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())
    } 

    ellipse_areas <- list(a = a, results_el = results_el)
    return (ellipse_areas)
 })
      ### render Elipse Areas Bar-Plot ----
      output$PCA_eli_area_barplot <- renderPlot({
        ggsave("PCA_ellipse_areas_barchart.pdf", PCA_ellipse_areas()$a)
        PCA_ellipse_areas()$a
      })
      
      ### download Elipse Areas Bar-Plot ----
      observe({
        shinyjs::toggle("PCA_ellipse_areas_barchart.pdf", !is.null(input$data))
      })
      output$download_ellipse_bar <- downloadHandler(
        filename = function() {
          "PCA_ellipse_areas_barchart.pdf"
        },
        content = function(file) {
          file.copy("PCA_ellipse_areas_barchart.pdf", file, overwrite = TRUE)
        }
      )  
      ### render Elipse Areas df ----
      output$PCA_eli_area_df <- DT::renderDataTable({
        PCA_ellipse_areas()$results_el
      })
      
      ### download PCA ellipse areas df ----
      observe({
        shinyjs::toggle("download_PCA_elli_areas_df", !is.null(input$data))
      })
      output$download_ellipse_df <- downloadHandler(
        filename = function() {
          paste("ellipse_areas", Sys.Date(), ".csv")
        },
        content = function(file){
          write.csv(PCA_ellipse_areas$results_el, file)
        }
      )

  ##PCA loadings ----
  PCA_loadings <- reactive ({
    
  if(input$PCA_loadings == TRUE){
    ### PCA-axis loadings ----
    PCA_data <- PCA()
    sample.data <-  PCA_data$sample.data
    sample.pca <- PCA_data$sample.pca

    ### Calculate loadings
    Loading_analytes <- df_analytes()
    Loading_analytes$R_script <- as.numeric(Loading_analytes$R_script)
    sample.pca.loadings <- merge(Loading_analytes, sample.pca$rotation[,1:2], by.x ="R_script", by.y = 0, all.y= TRUE)
    
    # get relative (%) contributions to loadings
    PC1_abs_loadings_summed <- sum(abs(sample.pca.loadings$PC1))
    PC2_abs_loadings_summed <- sum(abs(sample.pca.loadings$PC2))
    sample.pca.loadings$PC1_rel_loading <- abs(sample.pca.loadings$PC1) *100 /PC1_abs_loadings_summed 
    sample.pca.loadings$PC2_rel_loading <- abs(sample.pca.loadings$PC2) *100 /PC2_abs_loadings_summed 
    sample.pca.loadings$PC_summed_rel_loading <- sqrt((sample.pca.loadings$PC1_rel_loading)^2 + (sample.pca.loadings$PC2_rel_loading)^2)
    
    
    ## Format data for plotting
    plot_loadings <- sample.pca.loadings[order(sample.pca.loadings$PC_summed_rel_loading, decreasing = TRUE), ]
    rownames(plot_loadings) <- c(1:nrow(plot_loadings))
    
    ## how many loadings colored in plot
    highlight_nr <- input$PCA_loadings_colored
    Legend_label_top <- paste("Top ",highlight_nr, sep="")
    
    # correction if top_loadings value greater than 
    # the number of Analytes
    if(highlight_nr > nrow(plot_loadings)){
      highlight_nr <- nrow(plot_loadings)
      Legend_label_top <- "All"
    }
    
    plot_loadings$highlight <- c(rep("highlight", times = highlight_nr), rep("no",times = (nrow(plot_loadings) - highlight_nr)))
    
    if(input$PCA_loadings_label == "number"){
      plot_loadings$load_labels <- c(plot_loadings[1:highlight_nr,1],rep("",times = (nrow(plot_loadings) - highlight_nr)))
    }
    if(input$PCA_loadings_label == "name"){
      plot_loadings$load_labels <- c(plot_loadings[1:highlight_nr,2],rep("",times = (nrow(plot_loadings) - highlight_nr)))
    }
    
    ### get limits for plot
    x_limit <- 1.2 * max(abs(plot_loadings$PC1))
    y_limit <- 1.2 * max(abs(plot_loadings$PC2))
    
    #### Plot loadings
    
    l1 <- ggplot(plot_loadings, aes(x=PC1, y=PC2, fill= highlight)) + # , color=Group, fill=Group))+
      geom_point(size=input$plot_point_size, shape = input$plot_point_shape) 
    
    
    ## Add labels
    if(input$PCA_loadings_label == "number"){
      l1 <- l1 + geom_text(aes(label= load_labels), vjust = 1, size = input$PCA_label_size)
    }
    if(input$PCA_loadings_label == "name"){
      l1 <- l1 + geom_text(aes(label= load_labels), vjust = 1, size = input$PCA_label_size)
    }
    ####### set export name as input option
    Export_name <- input$export_name
    
    l1 <- l1 + theme(legend.direction = 'horizontal', legend.position = 'top') 
    l1 <- l1 + theme_bw()
    l1 <- l1 + ggtitle(paste("PCA Loadings of ",Export_name, sep = "")) +
      xlab("Loadings of PC 1")+
      ylab("Loadings of PC 2")+ 
      theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
            axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
            axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
            axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
            plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
            legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
            legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
            panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))
    
    ## if grid should be shown in plot
    l1 <- l1 + geom_hline(yintercept= 0) + geom_vline(xintercept= 0) + theme(aspect.ratio=1)+
      xlim(-x_limit, x_limit) + ylim(-y_limit, y_limit) + 
      scale_fill_discrete(name = "Contributors \nto loadings", labels = c(Legend_label_top , "Other"))
    
    ## relative loadings PC1 ----
    ## forrmat data
    plot_rel_loadings_PC1 <- sample.pca.loadings[order(sample.pca.loadings$PC1_rel_loading, decreasing = TRUE), ]
    rownames(plot_rel_loadings_PC1) <- c(1:nrow(plot_rel_loadings_PC1))
    

    highlight_nr <-  input$PCA_loadings_colored
    
    # correction if top_loadings value greater than 
    # the number of Analytes
    if(highlight_nr > nrow(plot_rel_loadings_PC1)){
      highlight_nr <- nrow(plot_rel_loadings_PC1)
    }
    
    
    plot_rel_loadings_PC1 <- plot_rel_loadings_PC1[1:highlight_nr,]
    plot_rel_loadings_PC1$idu <- row.names(plot_rel_loadings_PC1)
    plot_rel_loadings_PC1$idu <-as.numeric(plot_rel_loadings_PC1$idu)
    
    ## plot
    l2 <- ggplot(plot_rel_loadings_PC1, aes(x=idu, y=PC1_rel_loading)) +
      geom_bar(stat="identity", fill = "steelblue", colour = "black", width = 0.5) 
    
    
    l2 <- l2 + theme(legend.direction = 'horizontal', legend.position = 'top') 
    l2 <- l2 + theme_bw()
    l2 <- l2 + ggtitle(paste("Relative loadings of PC1 of ",Export_name, sep = "")) +
      ylab("Relative loadings of PC 1 (%)")+
      xlab("Analyte") + 
      theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
            axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
            axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
            axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
            plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
            legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
            legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
            panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))#print figure
    
    l2 <- l2 + coord_flip() + scale_x_reverse(breaks= c(1:highlight_nr), labels = plot_rel_loadings_PC1$Analyte)
    

    ##relative loadings PC2 ----
    
    ## format data
    plot_rel_loadings_PC2 <- sample.pca.loadings[order(sample.pca.loadings$PC2_rel_loading, decreasing = TRUE), ]
    rownames(plot_rel_loadings_PC2) <- c(1:nrow(plot_rel_loadings_PC2))
    

    highlight_nr <- input$PCA_loadings_colored
    
    # correction if top_loadings value greater than 
    # the number of Analytes
    if(highlight_nr > nrow(plot_rel_loadings_PC2)){
      highlight_nr <- nrow(plot_rel_loadings_PC2)
    }
    
    
    plot_rel_loadings_PC2 <- plot_rel_loadings_PC2[1:highlight_nr,]
    plot_rel_loadings_PC2$idu <- row.names(plot_rel_loadings_PC2)
    plot_rel_loadings_PC2$idu <-as.numeric(plot_rel_loadings_PC2$idu)
    
    ## plot
    l3 <- ggplot(plot_rel_loadings_PC2, aes(x= idu, y=PC2_rel_loading)) +
      geom_bar(stat="identity", fill = "steelblue", colour = "black", width = 0.5) 
    
    
    l3 <- l3 + theme(legend.direction = 'horizontal', legend.position = 'top') 
    l3 <- l3 + theme_bw()
    l3 <- l3 + ggtitle(paste("Relative loadings of PC2 of ",Export_name, sep = "")) +
      ylab("Relative loadings of PC 2 (%)")+
      xlab("Analyte") + 
      theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
            axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
            axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
            axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
            plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
            legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
            legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
            panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))#print figure
    
    l3 <- l3 + coord_flip() + scale_x_reverse(breaks= c(1:highlight_nr), labels = plot_rel_loadings_PC2$Analyte)

    loadings_plots <- list(l1 =l1, l2 = l2, l3 = l3, sample.pca.loadings = sample.pca.loadings)
    return (loadings_plots)
 }
  })
  
  ###render loadings plot and data ----
  output$PCA_loadings <- renderPlot(PCA_loadings()$l1)
  output$PC1_relative_loadings <- renderPlot(PCA_loadings()$l2)
  output$PC2_relative_loadings <- renderPlot(PCA_loadings()$l3)
  output$PCA_loadings_table  <- DT::renderDataTable(PCA_loadings()$sample.pca.loadings)

  ###download loadings plot and data ----
  observe({
    shinyjs::toggle("download_PCA_loadings_df", !is.null(input$data))
  })
  
  output$download_PCA_loadings_df <- downloadHandler(
    filename = "PCA_loadings.csv",
    content = function(file) {
      write.csv(PCA_loadings()$sample.pca.loadings)
    }
  )
  

  ## pairwise PCA  ----
  PCA_pairwise <- reactive({
  if(input$pairwise_pca_plots == TRUE){
    
    data <- df_NA()
    data <- data[, colSums(data != 0) != 0]

    posthoc_permanova <-
      pairwise.adonis(data[, 3:ncol(data)], data$Group)

    
    # Seperate groups
    library(stringr)
    posthoc_group_data <- cbind(str_split_fixed(posthoc_permanova$pairs, " vs ", 2),posthoc_permanova)
    
    posthoc_group_data[,1] <- as.character(posthoc_group_data[,1])
    posthoc_group_data[,2] <- as.character(posthoc_group_data[,2])
    
    
    #Start loop for pairwise analysis, figures and export
    for(i in 1:nrow(posthoc_group_data))
    {
      Group_1 <- posthoc_group_data[i,1]
      Group_2 <- posthoc_group_data[i,2]
      #subset for the groups in pairwise comparison
      posthoc_data <- subset(data, Group %in% c(Group_1,Group_2))
      #remove zero columns
      posthoc_data[is.na(posthoc_data)] <- 0 
      posthoc_data <- posthoc_data[ , colSums(posthoc_data != 0) != 0]
      
      
      sample.data <- posthoc_data[, 3:ncol(posthoc_data)]
      sample.name <- posthoc_data[, 1]
      sample.group <- c(as.character(posthoc_data [, 2]))
      sample.pca <-
        prcomp(sample.data,
               center = input$PCA_center,
               scale. = input$PCA_scale)
      # print method. The prcomp function returns an object of class prcomp, which have some methods available. The print method returns the standard deviation of each of the four PCs, and their rotation (or loadings), which are the coefficients of the linear combinations of the continuous variables."
      #print(sample.pca)
      
      # plot pricipal component variances
      plot(sample.pca, type = "l")
     # pdf(file.path(results_folder_pca,paste("PCA_diagnostic-pairwise_",Group_1,"_vs_",Group_2,".pdf",sep="")))
      plot(sample.pca, type = "l")
      dev.off()
    #  emf(file.path(results_folder_pca,paste("PCA_diagnostic-pairwise_",Group_1,"_vs_",Group_2,".emf",sep="")))
      plot(sample.pca, type = "l")
      dev.off()
      
      # summary method
      summary(sample.pca)
      
      # Variances
      variances_pca <- as.vector((sample.pca$sdev)^2 / sum(sample.pca$sdev^2))
      
      #Calculate significance CHANGE Column start CHANGE Grouping (raw_data$...)
      stat_p <- round(as.numeric(posthoc_permanova[i,5]), digits = 5)
      
      
      #### plot PCA
      #make data frame from PCA results
      df_out <- as.data.frame(sample.pca$x)
      df_out$Group <-sample.group
      df_out$Sample <- sample.name
      
      # calculte centroids of groups
      centroids <- aggregate(cbind(PC1,PC2)~Group,df_out,mean)
      df_connect <- merge(df_out,aggregate(cbind(mean.PC1=PC1,mean.PC2=PC2)~Group,df_out,mean),by="Group")
      
      # PLOT DATA: If fill colour for shape possible add black edge to points
      if(point_colour_black() == TRUE){
        g <- ggplot(df_connect, aes(x=PC1, y=PC2, color=Group, fill=Group))+
          geom_point(aes(fill=Group),size=input$plot_point_size, shape = input$plot_point_shape, colour="Black")
      }else{
        g <- ggplot(df_connect, aes(x=PC1, y=PC2, color=Group, fill=Group))+
          geom_point(aes(fill=Group, colour=Group),size=input$plot_point_size, shape = input$plot_point_shape)
      }
      
      # if centroids to be plotted and if connencting lines between centroids and data points
      if(input$plot_centroid == TRUE){
        if(input$plot_centroid_lines == TRUE){
          g <- g +
            geom_point(aes(x=mean.PC1,y=mean.PC2),size=input$plot_centroid_size, shape = input$plot_centroid_shape)+
            geom_segment(aes(x=mean.PC1, y=mean.PC2, xend=PC1, yend=PC2)) 
        }else{ #Plot only centroids without connecting lines 
          g <- g + 
            geom_point(data=centroids, size = input$plot_centroid_size, shape = input$plot_centroid_shape )
        }
      }
      # if ellipse with user defined type and confidence level should be drawn
      if(input$plot_ellipses == TRUE){
        g <- g + stat_ellipse(geom = "polygon", alpha = 0.3, level = input$plot_confidence, type = input$plot_ellipses_type)
      }
      
      # If colours user defined
      if(!is.null(input$colors)) {
        incolors <- colors_upload()
        g <-
          g + scale_fill_manual(values = incolors) + scale_colour_manual(values = incolors)
      }
      
      # if labels (sample names) should be plotted
      if(input$plot_label_samples == TRUE){
        sample.name <- as.vector(df_connect$Sample)
        g <- g + geom_text(aes(label=sample.name),hjust=0.5, vjust=1.3, size=input$plot_label_size) 
      }
      
      g <- g + theme(legend.direction = 'horizontal', legend.position = 'top') 
      g <- g + theme_bw()
      g <- g + ggtitle(paste("PCA of \n (Permanova (adonis) P-value =",stat_p,")")) +
        xlab(paste("PC 1 (Var. explained ",round(100*variances_pca[1],2),"%)", sep=""))+
        ylab(paste("PC 2 (Var. explained ",round(100*variances_pca[2],2),"%)", sep=""))+ 
        theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
              axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
              axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
              axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
              plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
              legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
              legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
              panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))
      
      ## if grid should be shown in plot
      if(input$plot_grid == FALSE){
        g <- g + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())
      } 
      
      g <- g +  annotate("text", label = paste("Permanova (adonis) P-value =",stat_p),size = 2) 
      g <- g + theme(aspect.ratio=1) 
      g <- g + guides(fill=guide_legend(title="Sample Group"), colour=guide_legend(title="Sample Group"))

      
      #export figures
     # ggsave(file=file.path(paste(results_folder_pca,"/PCA-pairwise_",Group_1,"_vs_",Group_2,".emf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
     #ggsave(file=file.path(paste(results_folder_pca,"/PCA-pairwise_",Group_1,"_vs_",Group_2,".pdf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
      
      ### ellipse areas ----
      if(input$plot_ellipse_area == TRUE){
        # Get ellipse coordinates from plot and add group names
        pb <- ggplot_build(g)
        ellipse_data <- pb$data[[3]][c("fill","x","y")]
        ellipse_sample_groups <- as.data.frame(cbind(c(unique(ellipse_data$fill)),c(unique((g[["data"]][["Group"]])))), stringsAsFactors = FALSE)
        colnames(ellipse_sample_groups) <- c("fill","group")
        ellipse_data_2 <- merge(ellipse_sample_groups,ellipse_data, by ="fill", all.y= TRUE, all.x= FALSE)
          
        ### Loop through each group to get area of each ellipse
        # get groups
        el_group_fills <-  c(unique(ellipse_data_2$fill))
        #make results file
        results_el <- data.frame(Group=character(), 
                                 Area=numeric(), 
                                 stringsAsFactors=FALSE)
        
        #### Loop 
        for( i in 1: length(el_group_fills)){
          group_fill_i <- el_group_fills[i]
          ellipse_data_i <- subset(ellipse_data_2, fill == group_fill_i)
          group_el_i <-  ellipse_data_i[1,2]
          ellipse_data_i <-ellipse_data_i[,3:4]
          
          # Center of ellipse
          
          ctr <- MASS::cov.trob(ellipse_data_i)$center  # Per @Roland's comment
          
          # Calculate distance to center from each point on the ellipse
          dist2center <- sqrt(rowSums((t(t(ellipse_data_i)-ctr))^2))
          
          # Calculate area of ellipse from semi-major and semi-minor axes. 
          # These are, respectively, the largest and smallest values of dist2center. 
          area_i <- pi*min(dist2center)*max(dist2center)
          
          results_el[i,] <- c(group_el_i, area_i)
        }
        ### barchart 
        plot_data_el_area <- results_el
        plot_data_el_area$Area <- as.numeric(plot_data_el_area$Area)
        a <- ggplot(aes(x= Group, y = Area, fill= Group), data= plot_data_el_area)+
          geom_bar(stat="identity", colour="Black", width=0.7)
        
        # If colours user defined
        if(!is.null(input$colors)) {}
          a <- a + scale_fill_manual(values = input$colors)
        }
        a <- a + theme_bw()
        a <- a + ggtitle(paste("Ellipse areas of PCA -")) +
          xlab(paste(""))+
          ylab("Ellipse area")+ 
          theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
                axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
                axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
                axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
                plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
                legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
                legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
                panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))
        
        ## if grid should be shown in plot
        if(input$plot_grid == FALSE){
          a <- a + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())
        } 
        return(a)
        ### Export bar plots
       # ggsave(file=file.path(paste(results_folder_pca,"/PCA-ellipse_areas-",Group_1,"_vs_",Group_2,".emf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
      #  ggsave(file=file.path(paste(results_folder_pca,"/PCA-ellipse_areas-",Group_1,"_vs_",Group_2,".pdf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
        
        #write results table
       # write.csv(results_el,file=file.path(paste(results_folder_pca,"/PCA-ellipse_areas-",Group_1,"_vs_",Group_2,".csv",sep="")),row.names=FALSE)
      }
      
      ### analyse PCA-axis loadings ----
      if(input$pairwise_loadings == TRUE){
        
        # Create results directory
       # dir.create(path=paste(results_folder_pca,"/_PCA_loadings", sep=""))
      #  loading_folder_group <- paste(results_folder_pca,"/_PCA_loadings", sep="")
        
        ### Calculate loadings
        Loading_analytes <- df_analytes()
        Loading_analytes$R_script <- as.numeric(Loading_analytes$R_script)
        sample.pca.loadings <- merge(Loading_analytes, sample.pca$rotation[,1:2], by.x ="R_script", by.y = 0, all.y= TRUE)
        
        # get relative (%) contributions to loadings
        PC1_abs_loadings_summed <- sum(abs(sample.pca.loadings$PC1))
        PC2_abs_loadings_summed <- sum(abs(sample.pca.loadings$PC2))
        sample.pca.loadings$PC1_rel_loading <- abs(sample.pca.loadings$PC1) *100 /PC1_abs_loadings_summed 
        sample.pca.loadings$PC2_rel_loading <- abs(sample.pca.loadings$PC2) *100 /PC2_abs_loadings_summed 
        sample.pca.loadings$PC_summed_rel_loading <- sqrt((sample.pca.loadings$PC1_rel_loading)^2 + (sample.pca.loadings$PC2_rel_loading)^2)
        
        # write export taqble
        #write.csv(sample.pca.loadings,file=file.path(paste(loading_folder,"/PCA_loadings-table-",Group_1,"_vs_",Group_2,".csv",sep="")),row.names=TRUE)
        
        
        ## Format data for plotting
        plot_loadings <- sample.pca.loadings[order(sample.pca.loadings$PC_summed_rel_loading, decreasing = TRUE), ]
        rownames(plot_loadings) <- c(1:nrow(plot_loadings))
        

        highlight_nr <- input$PCA_loadings_colored
        Legend_label_top <- paste("Top ",highlight_nr, sep="")
      
        # correction if top_loadings value greater than 
        # the number of Analytes
        if(highlight_nr > nrow(plot_loadings)){
          highlight_nr <- nrow(plot_loadings)
          Legend_label_top <- "All"
        }
        
        plot_loadings$highlight <- c(rep("highlight", times = highlight_nr), rep("no",times = (nrow(plot_loadings) - highlight_nr)))
        
        if(input$PCA_loadings_label == "number"){
          plot_loadings$load_labels <- c(plot_loadings[1:highlight_nr,1],rep("",times = (nrow(plot_loadings) - highlight_nr)))
        }
        if(input$PCA_loadings_label == "name"){
          plot_loadings$load_labels <- c(plot_loadings[1:highlight_nr,2],rep("",times = (nrow(plot_loadings) - highlight_nr)))
        }
        
        ### get limits for plot
        x_limit <- 1.2 * max(abs(plot_loadings$PC1))
        y_limit <- 1.2 * max(abs(plot_loadings$PC2))
        
        #### Plot loadings
        
        l1 <- ggplot(plot_loadings, aes(x=PC1, y=PC2, fill= highlight)) + # , color=Group, fill=Group))+
          geom_point(size=input$plot_point_size, shape = input$plot_point_shape) 
        
        
        ## Add labels
        if(input$PCA_loadings_label == "number"){
          l1 <- l1 + geom_text(aes(label= load_labels), vjust = 1, size = input$PCA_label_size)
        }
        if(input$PCA_loadings_label == "name"){
          l1 <- l1 + geom_text(aes(label= load_labels), vjust = 1, size = input$PCA_label_size)
        }
        
        
        l1 <- l1 + theme(legend.direction = 'horizontal', legend.position = 'top') 
        l1 <- l1 + theme_bw()
        l1 <- l1 + ggtitle(paste("PCA Loadings of ",Export_name, sep = "")) +
          xlab("Loadings of PC 1")+
          ylab("Loadings of PC 2")+ 
          theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
                axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
                axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
                axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
                plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
                legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
                legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
                panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))
        
        ## add lines and limits
        l1 <- l1 + geom_hline(yintercept= 0) + geom_vline(xintercept= 0) + theme(aspect.ratio=1) +
          xlim(-x_limit, x_limit) + ylim(-y_limit, y_limit)+
          scale_fill_discrete(name = "Contributors \nto loadings", labels = c(Legend_label_top , "Other"))
        
        
        #print figure
        #print(l1)
        
        #export figures
        #ggsave(file=file.path(paste(loading_folder_group,"/PCA_loadings-",Group_1,"_vs_",Group_2,".emf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
        #ggsave(file=file.path(paste(loading_folder_group,"/PCA_loadings-",Group_1,"_vs_",Group_2,".pdf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
        
        #### relative loadings PC1 ----
        
        ## forrmat data
        plot_rel_loadings_PC1 <- sample.pca.loadings[order(sample.pca.loadings$PC1_rel_loading, decreasing = TRUE), ]
        rownames(plot_rel_loadings_PC1) <- c(1:nrow(plot_rel_loadings_PC1))
        

        highlight_nr <- input$input$PCA_loadings_colored

        # correction if top_loadings value greater than 
        # the number of Analytes
        if(highlight_nr > nrow(plot_rel_loadings_PC1)){
          highlight_nr <- nrow(plot_rel_loadings_PC1)
        }
        
        
        plot_rel_loadings_PC1 <- plot_rel_loadings_PC1[1:highlight_nr,]
        plot_rel_loadings_PC1$idu <- row.names(plot_rel_loadings_PC1)
        plot_rel_loadings_PC1$idu <-as.numeric(plot_rel_loadings_PC1$idu)
        
        ## plot
        l2 <- ggplot(plot_rel_loadings_PC1, aes(x=idu, y=PC1_rel_loading)) +
          geom_bar(stat="identity", fill = "steelblue", colour = "black", width = 0.5) 
        
        
        l2 <- l2 + theme(legend.direction = 'horizontal', legend.position = 'top') 
        l2 <- l2 + theme_bw()
        l2 <- l2 + ggtitle(paste("Relative loadings of PC1 of ",Export_name, sep = "")) +
          ylab("Relative loadings of PC 1 (%)")+
          xlab("Analyte") + 
          theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
                axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
                axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
                axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
                plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
                legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
                legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
                panel.background = element_rect(fill = Background_colour), legend.background = element_rect(fill = input$plot_background))#print figure
        
        l2 <- l2 + coord_flip() + scale_x_reverse(breaks= c(1:highlight_nr), labels = plot_rel_loadings_PC1$Analyte)
        
        print(l2)
        
        #export figures
       # ggsave(file=file.path(paste(loading_folder_group,"/PC1_relative_loadings-",Group_1,"_vs_",Group_2,".emf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
      #  ggsave(file=file.path(paste(loading_folder_group,"/PC1_relative_loadings-",Group_1,"_vs_",Group_2,".pdf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
        
        ### relative loadings PC2 ----
        
        ## forrmat data
        plot_rel_loadings_PC2 <- sample.pca.loadings[order(sample.pca.loadings$PC2_rel_loading, decreasing = TRUE), ]
        rownames(plot_rel_loadings_PC2) <- c(1:nrow(plot_rel_loadings_PC2))
        

        # correction if top_loadings value greater than 
        # the number of Analytes
        if(highlight_nr > nrow(plot_rel_loadings_PC2)){
          highlight_nr <- nrow(plot_rel_loadings_PC2)
        }
        
        
        plot_rel_loadings_PC2 <- plot_rel_loadings_PC2[1:highlight_nr,]
        plot_rel_loadings_PC2$idu <- row.names(plot_rel_loadings_PC2)
        plot_rel_loadings_PC2$idu <-as.numeric(plot_rel_loadings_PC2$idu)
        
        ## plot
        l3 <- ggplot(plot_rel_loadings_PC2, aes(x= idu, y=PC2_rel_loading)) +
          geom_bar(stat="identity", fill = "steelblue", colour = "black", width = 0.5) 
        
        
        l3 <- l3 + theme(legend.direction = 'horizontal', legend.position = 'top') 
        l3 <- l3 + theme_bw()
        l3 <- l3 + ggtitle(paste("Relative loadings of PC2 of ",Export_name, sep = "")) +
          ylab("Relative loadings of PC 2 (%)")+
          xlab("Analyte") + 
          theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
                axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
                axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
                axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
                plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
                legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
                legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
                panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))#print figure
        
        l3 <- l3 + coord_flip() + scale_x_reverse(breaks= c(1:highlight_nr), labels = plot_rel_loadings_PC2$Analyte)
        
        print(l3)
        
        #export figures
       # ggsave(file=file.path(paste(loading_folder_group,"/PC2_relative_loadings-",Group_1,"_vs_",Group_2,".emf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
      #  ggsave(file=file.path(paste(loading_folder_group,"/PC2_relative_loadings-",Group_1,"_vs_",Group_2,".pdf",sep="")), width = plot_width, height = plot_height, units = c("cm"))
        
      } ### END OF PAIRWISE LOADINGS ANALYSIS
      
    } # end loop for pairwise permanova analysis
    
    # end of if statement if pairwise pca plots should be done
  return(l3)
    #### render pariwise PCA ----
  })

  output$pairwise_pca <- renderPlot(PCA_pairwise())
  
  ## NMDS ----
  NMDS <- reactive({
    data <- df_NA()
    #Grouping data change number of columns as applicable
    meta_data <- data[,1:2]
    sample <- c(data[,1])
    all_data <- data
    # data for nmds change star column as applicable
    nmds_data <- data[,3:ncol(data)]

    
    rownames(all_data) <- data[,1] 
    rownames(data)<- data[,1]                                
    rownames(meta_data)<-data[,1]
    
    
    col_nr<- as.numeric(ncol(all_data))
    
    if(min(nmds_data)< 0) {
      Diss_index_method = "euclidean"
    }else{
      Diss_index_method = "bray"
    }
    #NMDS calculation 
    dats.mds<-metaMDS(nmds_data,  distance= Diss_index_method, k=2, autotransform=T, zerodist="add")
    
    #Calculate significance CHANGE Column start CHANGE Grouping (raw_data$...)
    Significance <- adonis(formula = all_data[, 3:col_nr] ~ all_data$Group, data = nmds_data, method = "bray")
    stat_all <- Significance$aov.tab
    stat_p <- as.numeric(stat_all[1,6])
    
    
    #Preparing data for plotting
    
    #######data.scores[[1]][["sites"]] not working::: subscript out of bounds error 
    ######also not working in original script
    data.scores <- list(scores(dats.mds))#Using the scores function from vegan to extract the site scores and convert to a data.frame
    data.scores <- as.data.frame(data.scores[[1]])
    plot_data <- cbind(data.scores,meta_data)
    
    grp <- plot_data$Group  #### CHANGE to required grouping plot_data$.....
    data_plot <- cbind(plot_data,grp)
    data_plot <- data_plot[with(data_plot, order(grp,NMDS1,NMDS2)), ]
    
    centroids <- aggregate(cbind(NMDS1,NMDS2)~Group,data_plot,mean)
    df_connect_p <- merge(data_plot,aggregate(cbind(mean.NMDS1=NMDS1,mean.NMDS2=NMDS2)~Group,data_plot,mean),by="Group")
    
    
    #Plot
    # PLOT DATA: If fill colour for shape possible add black edge to points
    if(point_colour_black() == TRUE){
      p <- ggplot(df_connect_p, aes(x=NMDS1, y=NMDS2, color= Group, fill= Group))+
        geom_point(aes(fill=Group),size=input$plot_point_size, shape = input$plot_point_shape, colour="Black")
    }else{
      p <- ggplot(df_connect_p, aes(x=NMDS1, y=NMDS2, color=Group, fill=Group))+
        geom_point(aes(fill=Group, colour=Group),size=input$plot_point_size, shape = input$plot_point_shape)
    }
    
    # if centroids to be plotted and if connencting lines between centroids and data points
    if(input$plot_centroid == TRUE){
      if(input$plot_centroid_lines == TRUE){
        p <- p +
          geom_point(aes(x=mean.NMDS1,y=mean.NMDS2),size=input$plot_centroid_size, shape = input$plot_centroid_shape)+
          geom_segment(aes(x=mean.NMDS1, y=mean.NMDS2, xend=NMDS1, yend=NMDS2)) 
      }else{ #Plot only centroids without connecting lines 
        p <- p + 
          geom_point(data=centroids, size = input$plot_centroid_size, shape = input$plot_centroid_shape )
      }
    }
    
    # if ellipse with user defined type and confidence level should be drawn
    if(input$plot_ellipses == TRUE){
      p <- p + stat_ellipse(geom = "polygon", alpha = 0.3, level = input$plot_confidence, type = input$plot_ellipses_type)
    }
    
    p <- p + coord_equal() + 
      theme_bw() + 
      theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
            axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
            axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
            axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
            plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
            legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
            legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
            panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))
    ## if grid should be shown in plot
    if(input$plot_grid == FALSE){
      p <- p + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())
    } 
    p <- p + guides(fill=guide_legend(title="Sample Group"), colour=guide_legend(title="Sample Group"))
    p <- p +  ggtitle(paste("NMDS of ",input$export_name,"\n ( P-value =",stat_p, "  Stress",round(dats.mds$stress,3),")"))
    p <- p + theme(aspect.ratio=1) 
    #if group colours defined
    if(!is.null(input$colors)) {
      p <- p + scale_fill_manual(values= Group_colours) + scale_colour_manual(values= Group_colours)
    }
    
    #if sample labels wanted
    if(input$plot_label_samples == TRUE){
      p <- p + geom_text(data=df_connect_p,aes(x=NMDS1,y=NMDS2,label=Sample), size = input$plot_label_size,hjust=0.5, vjust=1.3)
    }
    print(class(p))
    
    # Stress values >0.2 are generally poor and potentially uninterpretable, whereas values <0.1 are good and <0.05 are excellent
   # ggsave(file=file.path(paste(results_folder,"/NMDS.emf",sep="")), width = plot_width, height = plot_width, units = c("cm"))
  #  ggsave(file=file.path(paste(results_folder,"/NMDS.pdf",sep="")), width = plot_width, height = plot_width, units = c("cm"))
    return(p)
  })
  ### render NMDS plot ----  
    output$NMDS <- renderPlot(NMDS())
# 
#   ## pairwise NMDS -----
#   NMDS_pairwise <- reactive({
#     
#     # Do you wish pairwise nmds plots
#     if(input$pairwise_nmds_plots == TRUE){
#       
#       # make seperate results folder
#      # dir.create(path=paste(getwd(),"/_Results_PCA_data_separation-",Export_name,"/pairwise_nmds",sep=""))
#     #  results_folder_nmds <- paste(getwd(),"/_Results_PCA_data_separation-",Export_name,"/pairwise_nmds",sep="")
#       data <- df_NA()
#       data <- data[, colSums(data != 0) != 0]
#       
#       posthoc_permanova <-
#         pairwise.adonis(data[, 3:ncol(data)], data$Group)
#       
#       
#       # Seperate groups
#       library(stringr)
#       posthoc_group_data <- cbind(str_split_fixed(posthoc_permanova$pairs, " vs ", 2),posthoc_permanova)
#       
#       posthoc_group_data[,1] <- as.character(posthoc_group_data[,1])
#       posthoc_group_data[,2] <- as.character(posthoc_group_data[,2])
#       
#       #Start loop for pairwise analysis, figures and export
#       for(i in 1:nrow(posthoc_group_data))
#       {
#         Group_1 <- posthoc_group_data[i,1]
#         Group_2 <- posthoc_group_data[i,2]
#         #subset for the groups in pairwise comparison
#         posthoc_data <- subset(calculation_data, Group %in% c(Group_1,Group_2))
#         #remove zerio columns
#         posthoc_data[is.na(posthoc_data)] <- 0 
#         posthoc_data <- posthoc_data[ , colSums(posthoc_data != 0) != 0]
#         rownames(posthoc_data) <- 1:nrow(posthoc_data)
#         
#         #Grouping data change number of columns as applicable
#         meta_data <- posthoc_data[,1:2]
#         sample <- c(posthoc_data[,1])
#         all_data <- posthoc_data
#         # data for nmds change star column as applicable
#         value_data <- posthoc_data[,3:ncol(posthoc_data)]
#         
#         # number of columns
#         col_nr<- as.numeric(ncol(all_data))
#         
#         #NMDS calculation 
#         dats.mds<-metaMDS(value_data,  distance= Diss_index_method, k=2, autotransform=T, zerodist="add")
#         
#         #Calculate significance CHANGE Column start CHANGE Grouping (raw_data$...)
#         stat_p <- round(as.numeric(posthoc_permanova[i,5]), digits = 5)
#         
#         #Preparing data for plotting
#         data.scores <- list(scores(dats.mds))#Using the scores function from vegan to extract the site scores and convert to a data.frame
#         data.scores <- as.data.frame(data.scores[[1]])
#         plot_data <- cbind(data.scores,meta_data)
#         
#         grp <- plot_data$Group  #### CHANGE to required grouping plot_data$.....
#         data_plot <- cbind(plot_data,grp)
#         data_plot <- data_plot[with(data_plot, order(grp,NMDS1,NMDS2)), ]
#         
#         centroids <- aggregate(cbind(NMDS1,NMDS2)~Group,data_plot,mean)
#         df_connect_p <- merge(data_plot,aggregate(cbind(mean.NMDS1=NMDS1,mean.NMDS2=NMDS2)~Group,data_plot,mean),by="Group")
#         
#         
#         #Plot# PLOT DATA: If fill colour for shape possible add black edge to points
#         if(point_colour_black() == TRUE){
#           p <- ggplot(df_connect_p, aes(x=NMDS1, y=NMDS2, color= Group, fill= Group))+
#             geom_point(aes(fill=Group),size=input$plot_point_size, shape = input$plot_point_shape, colour="Black")
#         }else{
#           p <- ggplot(df_connect_p, aes(x=NMDS1, y=NMDS2, color=Group, fill=Group))+
#             geom_point(aes(fill=Group, colour=Group),size=input$plot_point_size, shape =input$plot_point_shape)
#         }
#         
#         # if centroids to be plotted and if connencting lines between centroids and data points
#         if(input$plot_centroid == TRUE){
#           if(input$plot_centroid_lines == TRUE){
#             p <- p +
#               geom_point(aes(x=mean.NMDS1,y=mean.NMDS2),size=centroid_size, shape = centroid_shape)+
#               geom_segment(aes(x=mean.NMDS1, y=mean.NMDS2, xend=NMDS1, yend=NMDS2)) 
#           }else{ #Plot only centroids without connecting lines 
#             p <- p + 
#               geom_point(data=centroids, size = input$plot_centroid_size, shape = input$plot_centroid_shape )
#           }
#         }
#         
#         # if ellipse with user defined type and confidence level should be drawn
#         if(input$plot_ellipses == TRUE){
#           p <- p + stat_ellipse(geom = "polygon", alpha = 0.3,level = input$plot_confidence, type = input$ellipses_type)
#         }
#         
#         p <- p + coord_equal() + 
#           theme_bw() + 
#           
#           theme(axis.text.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=0,face="bold"),
#                 axis.text.y = element_text(colour="black",size=8,angle=0,hjust=1,vjust=0.5,face="bold"),  
#                 axis.title.x = element_text(colour="Black",size=8,angle=0,hjust=0.5,vjust=-1,face="bold"),
#                 axis.title.y = element_text(colour="Black",size=8,angle=90,hjust=0.5,vjust=1,face="bold"),
#                 plot.title = element_text(colour="black",size=10,angle=0,hjust=0.5,vjust=2,face="bold"),
#                 legend.title = element_text(colour="black",size=8,angle=0,hjust=0,vjust=2,face="bold"),
#                 legend.text = element_text(colour="black",size=8,angle=0,hjust=0,vjust=0.5,face="bold"),
#                 panel.background = element_rect(fill = input$plot_background), legend.background = element_rect(fill = input$plot_background))
#         ## if grid should be shown in plot
#         if(input$plot_grid == FALSE){
#           p <- p + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank())
#         } 
#         p <- p + guides(fill=guide_legend(title="Sample Group"), colour=guide_legend(title="Sample Group"))
#         p <- p +  ggtitle(paste("NMDS of ",Export_name,"\n ( P-value =",stat_p, "  Stress",round(dats.mds$stress,3),")"))
#         p <- p + theme(aspect.ratio=1) 
#         
#         #if group colours defined
#         if(point_colour_black() == TRUE){
#           if(!is.null(input$colors)) {
#             p <- p + scale_fill_manual(values= Group_colours) + scale_colour_manual(values= Group_colours)
#           }
#         }else{
#           if(!is.null(input$colors)) {
#             p <- p + scale_fill_manual(values= Group_colours) + scale_colour_manual(values= Group_colours)
#           }
#         }
#         
#         
#         #if sample labels wanted
#         if(input$plot_label_samples == TRUE){
#           p <- p + geom_text(data=df_connect_p,aes(x=NMDS1,y=NMDS2,label=Sample), size = label_size,hjust=0.5, vjust=1.3)
#         }
#         
#         # Stress values >0.2 are generally poor and potentially uninterpretable, whereas values <0.1 are good and <0.05 are excellent
#         
#        # print(p)
#        # ggsave(file=file.path(paste(results_folder_nmds,"/NMDS_pairwise_",Group_1,"_vs_",Group_2,".emf",sep="")), width = plot_width, height = plot_width, units = c("cm"))
#       #  ggsave(file=file.path(paste(results_folder_nmds,"/NMDS_pairwise_",Group_1,"_vs_",Group_2,".pdf",sep="")), width = plot_width, height = plot_width, units = c("cm"))
#         
#         #end of For loop
#       }
#       
#       # end of if statement if pairwise nmds plots should be done
#     }
#     return(p)
#   })
#   ### render NMDS pairwise plot ----
#   output$NMDS_pairwise <- renderPlot(NMDS_pairwise())
#   
#   
#Statistics Dataframe ----
  stats <- reactive({
  data <- df_NA()

  # Get sample group names
  Group_names <- unique(as.vector(data$Group))
  
  # Number of sample groups
  Number_of_Groups <- as.numeric(length (Group_names))
  
  for(i in 1:Number_of_Groups)
  {
    Group_name <- Group_names[i] 
    data_sub <- data[data[,2]== Group_name,]
    
    
    # SD of samples 
    index_sd <- 3
    SD <- sd(na.omit(c(data_sub[,3])))
    
    repeat
    { 
      index_sd <- 1 + index_sd
      SD_2 <- sd(na.omit(c(data_sub[,(index_sd)])))
      SD <- cbind(SD,SD_2)
      if (index_sd == ncol(data_sub))
        break;
    }

    # Mean of samples
    index_mean <- 3
    Mean_value <- mean(na.omit(c(data_sub[,3])))
    
    repeat
    { 
      index_mean <- 1 + index_mean
      Mean_value_2 <- mean(na.omit(c(data_sub[,(index_mean)])))
      Mean_value <- cbind(Mean_value,Mean_value_2)
      if (index_mean == ncol(data_sub))
        break;
    }
    
    
    # Counting samples including zero values but omitting NA
    index_count <- 3
    Count_value <- sum(as.numeric(na.omit(c(data_sub[,3]))>=0))
    repeat
    { 
      index_count <- 1 + index_count
      Count_value_2 <- sum(as.numeric(na.omit(c(data_sub[,index_count])))>=0)
      Count_value <- cbind(Count_value,Count_value_2)
      if (index_count == ncol(data_sub))
        break;
    }
    
    # adding SD, mean and count as well as Standard error of mean (SEM) and CV together with data
    
    CV <-as.data.frame((t(SD)/t(Mean_value)))
    SEM <- as.data.frame(t(SD)/sqrt(t(Count_value)))
    SD <- as.data.frame(SD)
    Mean_value<- as.data.frame(Mean_value)
    Count_value <- as.data.frame(Count_value)
    
    print(length(CV))
    print(length(SEM))
    print(length(Mean_value))
    sample_names <- as.character(data_sub[,1])
    
    data_sub_results <- cbind(t(Mean_value),t(SD),SEM,CV,t(Count_value),t(data_sub[3:ncol(data_sub)]))
    colnames(data_sub_results) <- c(paste("Mean",Group_name),paste("SD",Group_name),paste("SEM",Group_name),paste("CV",Group_name),paste("ID in number of ",Group_name),sample_names)
    rownames(data_sub_results) <- colnames(data_sub)[3:ncol(data_sub)]
    
    #Merge data with analyte names 
    #Analyte_mapping <- df_analytes()
    rownames(Analyte_mapping) <- Analyte_mapping[,2]
    print(length(Analyte_mapping))
    data_sub_results_2 <- merge(Analyte_mapping,data_sub_results, by="row.names")
    data_sub_results_final <- data_sub_results_2[,3:ncol(data_sub_results_2)]
    
    # Export results data for each sample group
    #write.csv(data_sub_results_final,file=file.path(paste(results_folder,sep="",paste("/",Export_name,"_Sample_group-",Group_name,".csv",sep=""))),row.names=FALSE)
    
  }
  
  ## Export settings used
  
  # make settings data frame
  settings_parameters <- c("Zero values converted to NA:",
                           "For pairwise statistics p-value adjustment method used:",
                           "PCA data centred:",
                           "PCA data scaled:",
                           "Plot ellipses:",
                           "Ellipse confidence level:",
                           "Ellipse type:",
                           "NMDS data autotransformed:")
  
  
  
  settings <-c(input$na, 
               input$p_adjustment, 
               input$PCA_center, 
               input$PCA_scale, 
               input$plot_ellipses, 
               input$plot_confidence, 
               input$plot_ellipses_type, 
               input$NMDS_autotransform)
  
  settings_data <- as.data.frame(cbind(settings_parameters, settings))
  colnames(settings_data) <- c("Parameters", "Settings")
  
  # export settings
  stats_df <- list(settings_data = settings_data, data_sub_results_final = data_sub_results_final, data_sub_results_2 = data_sub_results_2)
  
  return(stats_df)
  })
  
  ###render statistics dataframe
  output$stats_df <- DT::renderDataTable(stats()$data_sub_results_final)
  output$settings_df <- DT::renderDataTable(stats()$settings_data)
})

# Run App ----
shinyApp(ui = ui, server = server)
