
library(shiny)
library(shinydashboard)
library(datasets)
library(DT)
library(tidyr)
library(dplyr)
library(shinyjs)
library(ggrepel)
library(ggbiplot)
library(stringr)
library(readr)
#runExample("test.csv")

shinyServer(function(input, output, session) {
  values <- reactiveValues(
    plots = list(),
    maindata = NULL     # the data frame used throughout the app
  ) 
   
  

  
  # Load sample dataset
  observeEvent(input$sample_data_btn, {
    file <- "wt.csv"
    values$maindata <- read.csv(file)
    
  })
  
  
  
   data <- reactive({ 
    if (is.null(values$maindata)){
      req(input$file1) ## ?req #  require that the input is available
      
      
      inFile <- input$file1
      
      df <- read.csv(inFile$datapath, sep = ",", stringsAsFactors = FALSE)
      #df[is.na(df)] <- 0
      #df[is.na(df)] <- 0
      
      
    } else {
      df <- values$maindata
     
    }
     # df[is.na(df)] <- 0
     # return(df)
     # 
  })
    
    filtereddata <- eventReactive({
      #validate(need(input$dataset != "","Please select a data set in csv format only!!!"))#
      input$update
      data()
    },  {
      req(data())
      if(is.null(input$select) || input$select == "")
        data() 
      else if (is.null(input$exp_con) || input$exp_con == "")
          data()[, colnames(data()) %in% input$select]
      else {
        data()[, colnames(data()) %in%  exp_con_sel()]
      }
    })
  
  exp_con_sel <- reactive({
    names_col <- c("class", "length", "DB")
    gs <- c("early", "mid", "late", "stat")
    t <- c("13","20","30","4")
    ifelse(input$exp_con == "Growth Stage", names_col <-append(names_col, grep(paste(gs,collapse="|"), colnames(data()), value=TRUE)),
           ifelse(input$exp_con == "Temperature",names_col <-append(names_col,grep(paste(t,collapse="|"),colnames(data()), value=TRUE)),
                  ifelse(input$exp_con == "Salt",names_col <-append(names_col,grep("Na",colnames(data()), value=TRUE)),
                         ifelse(input$exp_con == "Methanol",names_col <-append(names_col,grep("Met",colnames(data()), value=TRUE)),
                                ifelse(input$exp_con == "Triton",names_col <-append(names_col,grep("TX",colnames(data()), value=TRUE)),
                                       colnames(data()))))))
    return(names_col)
  })

  observeEvent(
    data(), {
      updateSelectInput(session, "select", choices=colnames(data()), selected = colnames(data()))
      
    })
  observeEvent(
    input$update, {
      updateSelectInput(session,"select", choices=colnames(filtereddata()), selected = colnames(filtereddata()))
      
    })

  # eventReactive(input$sample_data_btn, {
  #   inFile <- "data/test.csv"
  #   
  # })
  
  
  
  Detail <- eventReactive({
  filtereddata()
    },  {
  if (input$feat == "Double Bonds") {
    df<-filtered_data()
    df <- select(df, -c(length))
    df_gat <- df %>% gather(rep, num, -one_of("DB","class"))
    
    df_gat_sum <- df_gat %>%
      group_by(rep,class) %>%                     
      dplyr::mutate(sumper=num/sum(num))%>%
      group_by(rep,class,DB)%>%
      dplyr::summarise(Per=sum(sumper)*100) %>% spread(rep, Per) %>% ungroup()
  } else if(input$feat == "Length"){
    df<-filtered_data()
    df <- select(df,  -DB)
    df_gat <- df %>% gather(rep, num, -one_of("length","class"))
    
    df_gat_sum <- df_gat %>%
      group_by(rep,class) %>%                     
      dplyr::mutate(sumper=num/sum(num))%>%
      group_by(rep,class,length)%>%
      dplyr::summarise(Per=sum(sumper)*100) %>% spread(rep, Per) %>% ungroup()
  } else if(input$feat == "Head Group"){
    df<-filtered_data()
    df<-df%>%separate(class, into = c("class", "bal"),sep =  " ")
    df <- df %>% select(-bal, - length, -DB)
    df$class <-  sub("^m", "", df$class )
    df$class <-  sub("DIP", "DIPs", df$class )
    df_gat <- df %>% gather(rep, num, -one_of("class"))
    #df_gat$rep<-sub("\\d$","",df_gat$rep)
    #df_gat$rep<-sub("\\.$","",df_gat$rep)
    
    #wt_DB_gat_sum <-wt_DB_gat %>% group_by(rep, DB) %>% summarise_all(funs(sum(.)))
    df_gat_sum <-
      df_gat%>%
      group_by(rep) %>%                     
      dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,class)%>%
      dplyr::summarise(Per =sum(sumper)*100) %>% spread(rep, Per) %>% ungroup()
  } else if(input$feat =="Species"){
    filtereddata()
  }
  })
  filter_class <- reactive({
    
    filtereddata() %>%
      mutate(class = str_replace(class, "m","")) %>%
      mutate(class = str_replace(class, "\\<DIP\\>", "DIPs")) %>%
      separate(class, into = c("class", "bal"),sep =  " ") %>% 
      select(-bal)
    
  }) 
  
  filtered_data <- eventReactive({
    input$update
  },  {
    
    if(is.null(input$feat_gh)  && is.null(input$feat_db) && is.null(input$feat_len))
    { filtereddata()
    }else {
      filter_class() %>% filter(class %in% input$feat_gh,DB %in% input$feat_db, length %in% input$feat_len)
    }
    #https://stackoverflow.com/questions/30001211/filter-data-frame-for-use-in-multiple-renderplot-functions 
  })
  # observeEvent(input$feat, {
  #   updateSelectInput(session, "feat_gh", choices=unique(filter_class()$class), selected = input$feat_gh)
  #   updateSelectInput(session, "feat_db", choices=unique(filtered_data()$DB), selected = input$feat_db )
  #   updateSelectInput(session, "feat_len", choices=unique(filtered_data()$length), selected = input$feat_len)
  #   
  # })
  observeEvent(data(), {
    updateSelectInput(session, "species", choices=unique(data()$class), selected = unique(data()$class))
    updateSelectInput(session, "feat_gh", choices=unique(filter_class()$class), selected = unique(filter_class()$class))
    updateSelectInput(session, "feat_db", choices=unique(filtereddata()$DB), selected =unique(filtereddata()$DB)  )
    updateSelectInput(session, "feat_len", choices=unique(filtereddata()$length), selected =unique(filtereddata()$length) )
     })
  

  output$contents <- DT::renderDataTable(datatable(Detail()) ) 
  
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file) {
      write.csv(Detail(), file)
    },
    contentType = "text/csv"
  )
  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]
    
    if(length(add)!=0) data[add] <- NA
    data
  }
                                                  ################ PLOT ####################
  Plot <- eventReactive({ 
    input$update_plot
  },{
    
    df <- fncols(Detail(), c("class","length","DB"))
    #df <-df %>% select(-X)
                                                                  ### LIPID ABUNDANCE ####
    if(input$plot_type == "Lipid Abundance"){
      #gat <- gather(df, rep, num, -one_of("class", "length", "DB") )
      df[is.na(df)] <- 0
      
      if (input$feat == "Double Bonds") {
        bx_data <- df %>%
          select( -c(length)) %>% separate(class, into = c("class", "bal"),sep =  " ") %>% 
          select(-bal)
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("DB", "class"))
        input_name <- "DB"
        bx_data_gat <- bx_data_gat %>%
          group_by(exp_condition,class) %>%                     
          dplyr::mutate(sumper=abundance/sum(abundance))%>%
          group_by_("exp_condition","class",input_name)%>%
          dplyr::summarise(abundance=sum(sumper)*100)
        bx_data_gat$DB <- factor(bx_data_gat$DB, levels=(unique(bx_data_gat$DB)))
      } else if(input$feat == "Length") {
        bx_data <- df %>%
          select( -c(DB))%>%
          separate(class, into = c("class", "bal"),sep =  " ") %>% 
          select(-bal)
        input_name <- "length"
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("length","class"))
        bx_data_gat <- bx_data_gat %>%
          group_by(exp_condition,class) %>%                     
          dplyr::mutate(sumper=abundance/sum(abundance))%>%
          group_by_("exp_condition","class",input_name)%>%
          dplyr::summarise(abundance=sum(sumper)*100)
        bx_data_gat$length <- factor(bx_data_gat$length, levels=(unique(bx_data_gat$length)))
        
      } else if(input$feat == "Head Group"){
        bx_data <- df %>% select( -c(DB,length)) 
        input_name <- "class"
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("class"))
        bx_data_gat <- bx_data_gat %>%
          group_by(exp_condition) %>%                     
          dplyr::mutate(sumper=abundance/sum(abundance))%>%
          group_by_("exp_condition",input_name)%>%
          dplyr::summarise(abundance=sum(sumper)*100)
        bx_data_gat$class <- factor(bx_data_gat$class, levels=(unique(bx_data_gat$class)))
        
      } else {
        bx_data <- df
        colnames(bx_data)[colnames(bx_data)=="class"] <- "lipid"
        bx_data <- bx_data %>% select( -c(DB,length)) %>%
          separate(lipid, into = c("class", "bal"),sep =  " ",remove = FALSE) %>% 
          select(-bal)
        bx_data$class <-  sub("^m", "", bx_data$class )
        bx_data$class <-  sub("DIP", "DIPs", bx_data$class )
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("class", "lipid"))
        bx_data_gat$lipid <- factor(bx_data_gat$lipid, levels=(unique(bx_data_gat$lipid)))
        input_name <- "lipid"
      }
      
      
      
      
      if(!input$hg_lipabu){
      ggplot(bx_data_gat, aes_string(x = input_name, y= "abundance", col = "input$Legend" )) + 
        geom_point(size = 0.4, position = position_dodge(width = 0.3)) +
        stat_summary(fun.y= "mean", aes( group=1, colour = "Mean"), geom="point",size = 0.5, show.legend = TRUE )+ 
        theme_bw() +
        theme(legend.position=input$legendposition, 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x=element_text(angle=90,hjust=1),
              text = element_text(size=10),
              axis.ticks.length=unit(0.5,"cm"),
              legend.text=element_text(size=10)) +
        labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ")+
        scale_color_manual(labels = c(input$Legend, "Mean"), values = c("red", "black"))}
      else{
        ggplot(bx_data_gat, aes_string(x = input_name, y= "abundance", col = "input$Legend" )) + 
          geom_point(size = 0.4, position = position_dodge(width = 0.3)) +
          stat_summary(fun.y= "mean", aes( group=1, colour = "Mean"), geom="point",size = 0.5, show.legend = TRUE )+ 
          theme_bw() +
          theme(legend.position=input$legendposition, 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.x=element_text(angle=90,hjust=1),
                text = element_text(size=10),
                axis.ticks.length=unit(0.5,"cm"),
                legend.text=element_text(size=10)) +
          labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ")+
          scale_color_manual(labels = c(input$Legend, "Mean"), values = c("red", "black")) + facet_grid(. ~ class,scales =  "free_x")
      }
      
     
    }
                                                              #### PCA  #####
    else if(input$plot_type == "PCA"){
      gat_pca <- gather(df,  replicate, membr_perc, -one_of("class", "length", "DB") )
      gat_pca[is.na(gat_pca)] <- 0
       if(input$feat == "Double Bonds"){
        gat_pca <- select(gat_pca,-c(length, class))
        
        gat_pca <- gat_pca %>%
          group_by(replicate) %>%                     
          dplyr::mutate(sumper=membr_perc/sum(membr_perc)) %>%
          group_by(replicate, DB) %>%
          dplyr::summarise(Per=sum(sumper)*100)
        
        wt_spread <- spread(gat_pca, DB, Per)
      } else if(input$feat == "Length")
      {gat_pca <- select(gat_pca,-c(DB, class))
      
      gat_pca <- gat_pca %>%
        group_by(replicate) %>%                     
        dplyr::mutate(sumper=membr_perc/sum(membr_perc)) %>%
        group_by(replicate, length) %>%
        dplyr::summarise(Per=sum(sumper)*100)
      
      wt_spread <- spread(gat_pca, length, Per)}
      
      else if(input$feat == "Head Group")
      {gat_pca <- select(gat_pca,-c(DB, length))
      
      gat_pca <- gat_pca %>%
        group_by(replicate) %>%                     
        dplyr::mutate(sumper=membr_perc/sum(membr_perc)) %>%
        group_by(replicate, class) %>%
        dplyr::summarise(Per=sum(sumper)*100)
      
      wt_spread <- spread(gat_pca, class, Per)}
      else{
        gat_pca <- select(gat_pca,-c(length, DB))
        wt_spread <- spread(gat_pca, class, membr_perc)
      }
      wt_spread <- as.data.frame(wt_spread)
      wt_spread$replicate <- make.names(wt_spread$replicate, unique=TRUE)
      rownames(wt_spread) <- wt_spread$replicate
      wt_spread <- select(wt_spread,-replicate)
      
      wt.rep <- rownames(wt_spread)
      wt.rep<-sub("^X","",wt.rep)
      wt.rep<-sub("\\d{1}$","",wt.rep)
      wt.rep<-sub("\\.$","",wt.rep) #remomve dot at the end
      wt.rep<- factor(wt.rep)
      
      prin_comp_wttwmp <- prcomp(na.omit(wt_spread))
      if(!input$pca_names){
        ggbiplot(prin_comp_wttwmp, 
                 obs.scale = 1, 
                 var.scale = 1,
                 groups = wt.rep ,
                 var.axes = FALSE,
                 ellipse = FALSE) +
          #label = wt.rep)
          #geom_point(aes( colour=wt.rep), size = 1) +
          geom_text_repel(label = wt.rep,
                          size = 2,
                          segment.color = 'grey50')+
          theme_bw()+  
          theme( legend.position = "none") +
          labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ")
      } else{
        ggbiplot(prin_comp_wttwmp, 
                 obs.scale = 1, 
                 var.scale = 1,
                 groups = wt.rep,
                 var.axes = FALSE) +
          #label = wt.rep)
          theme_bw()+  
          theme( legend.position =input$legendposition) +
          labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ")
      }
      
    } 
                                                                    #### BOX PLOT #####
    else if(input$plot_type == "Box Plot"){

      if (input$feat == "Double Bonds") {
        bx_data <- df %>%
          select( -c(length)) %>% separate(class, into = c("class", "bal"),sep =  " ") %>% 
          select(-bal)
        bx_data_gat <- bx_data %>% gather(rep, num, -one_of("DB", "class"))
        input_name <- "DB"
        bx_data_gat <- bx_data_gat %>%
          group_by(rep,class) %>%                     
          dplyr::mutate(sumper=num/sum(num))%>%
          group_by_("rep","class",input_name)%>%
          dplyr::summarise(num=sum(sumper)*100)
        bx_data_gat$DB <- factor(bx_data_gat$DB, levels=(unique(bx_data_gat$DB)))
      } else if(input$feat == "Length") {
        bx_data <- df %>%
          select( -c(DB))%>%
          separate(class, into = c("class", "bal"),sep =  " ") %>% 
          select(-bal)
        input_name <- "length"
        bx_data_gat <- bx_data %>% gather(rep, num, -one_of("length","class"))
        bx_data_gat <- bx_data_gat %>%
          group_by(rep,class) %>%                     
          dplyr::mutate(sumper=num/sum(num))%>%
          group_by_("rep","class",input_name)%>%
          dplyr::summarise(num=sum(sumper)*100)
        bx_data_gat$length <- factor(bx_data_gat$length, levels=(unique(bx_data_gat$length)))
        
      } else if(input$feat == "Head Group"){
        bx_data <- df %>% select( -c(DB,length)) %>%
          separate(class, into = c("class", "bal"),sep =  " ") %>% 
          select(-bal)
        input_name <- "class"
        bx_data_gat <- bx_data %>% gather(rep, num, -one_of("class"))
        bx_data_gat <- bx_data_gat %>%
          group_by(rep) %>%                     
          dplyr::mutate(sumper=num/sum(num))%>%
          group_by_("rep",input_name)%>%
          dplyr::summarise(num=sum(sumper)*100)
        bx_data_gat$class <- factor(bx_data_gat$class, levels=(unique(bx_data_gat$class)))
        
      } else {
        bx_data <- df
        colnames(bx_data)[colnames(bx_data)=="class"] <- "lipid"
        bx_data <- bx_data %>% select( -c(DB,length)) %>%
          separate(lipid, into = c("class", "bal"),sep =  " ",remove = FALSE) %>% 
          select(-bal)
        bx_data$class <-  sub("^m", "", bx_data$class )
        bx_data$class <-  sub("DIP", "DIPs", bx_data$class )
        bx_data_gat <- bx_data %>% gather(rep, num, -one_of("class", "lipid"))
        bx_data_gat$lipid <- factor(bx_data_gat$lipid, levels=(unique(bx_data_gat$lipid)))
        input_name <- "lipid"
      }
      
      
      
      
     
  
      #wt_class_gat_sum$rep<-sub("X","",wt_class_gat_sum$rep)
      bx_data_gat$rep<-sub("\\d$","",bx_data_gat$rep)
      bx_data_gat$rep<-sub("\\.$","",bx_data_gat$rep)
      
      gs <- c("early", "mid", "late", "stat")
      bx_data_gat$groups <- ifelse(grepl("TX",bx_data_gat$rep), "TX", 
                                       ifelse(grepl("4",bx_data_gat$rep),"Temperature", 
                                              ifelse(grepl("13",bx_data_gat$rep),"Temperature", 
                                                     ifelse(grepl("30",bx_data_gat$rep),"Temperature",
                                                            ifelse(grepl("20",bx_data_gat$rep),"Temperature",
                                                                   ifelse(grepl(paste(gs, collapse = "|"), bx_data_gat$rep),"Growth stage",
                                                                          ifelse(grepl("Na",bx_data_gat$rep), "NaCl",
                                                                                 ifelse(grepl("Met",bx_data_gat$rep), "MetOH",
                                                                                        "Other"))))))))
      # if(input$feat== 'Double Bonds'){
      #   bx_data_gat$DB <- factor(bx_data_gat$DB, levels = c(unique(bx_data_gat$DB)))
      # } else if(input$feat == 'Length'){                                                                                
      #   bx_data_gat$length <- factor(bx_data_gat$length, levels = c(unique(bx_data_gat$length)))
      # } else if(input$feat == 'Head Group'){
      #   bx_data_gat$class <- factor(bx_data_gat$class, levels = c(unique(bx_data_gat$class)))
      # } else {
      #   bx_data_gat$lipid <- factor(bx_data_gat$lipid, levels = c(unique(bx_data_gat$lipid)))
      # }
      # 
      if(!input$hg_bx){
        bx_data_gat$num <- as.numeric(bx_data_gat$num)
      ggplot(bx_data_gat, aes_string(x =input_name, y= "num", col = "groups")) +
        stat_boxplot(geom ='errorbar') +
        geom_boxplot(colour = "grey", fill= "white")+
        geom_jitter(size = 0.4, position = position_dodge(width = 0.5))+
        theme_bw()  + 
        theme(axis.text.x=element_text(angle=90,hjust=1)) + 
        coord_fixed(ratio = 1/10) +
        labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ")+
        ylim(0,100)
      } else{
        bx_data_gat$num <- as.numeric(bx_data_gat$num)
        ggplot(bx_data_gat, aes_string(x =input_name, y= "num", col = "groups")) +
          stat_boxplot(geom ='errorbar') +
          geom_boxplot(colour = "grey", fill= "white")+
          geom_jitter(size = 0.4, position = position_dodge(width = 0.5))+
          theme_bw()  + 
          theme(axis.text.x=element_text(angle=90,hjust=1)) + 
          coord_fixed(ratio = 1/10) +
          labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ")+
          ylim(0,100) + facet_grid(. ~ class,scales =  "free_x")
      }
      
    }
                                                                      #### Standard Deviation #####
    else if(input$plot_type == "Standard Deviation"){
      
      sd <- df
      sd[is.na(sd)] <- 0
      #names(filtereddata()) <- sub("^X","",names(filtereddata()))
      #SD for total
      
      if(input$feat == "Species"){
        sd_all <- sd %>% select(-one_of("DB","length"))
        sd_lipid <- sd_all %>% gather(rep, num, -one_of("class")) %>% tbl_df()
        name_feat = "class"
      } else if(input$feat == "Head Group"){
        sd_all <- sd %>% select(-one_of("DB","length"))
        sd_lipid <- sd_all %>% gather(rep, num, -one_of("class")) %>% tbl_df()
        name_feat = "class"
      } else if(input$feat == "Length"){
        sd_all <- sd %>% select(-one_of("DB","class"))
        sd_lipid <- sd_all %>% gather(rep, num, -one_of("length")) %>% tbl_df()
        name_feat = "length"
      }else{
        sd_all <- sd %>% select(-one_of("class","length"))
        sd_lipid <- sd_all %>% gather(rep, num, -one_of("DB")) %>% tbl_df()
        name_feat = "DB"
      }
      
      sd_lipid <- sd_lipid %>%
        group_by(rep) %>%                     
        dplyr::mutate(sumper=num/sum(num)) %>%
        group_by_(.dots = c("rep", name_feat)) %>%
        dplyr::summarise(Per=sum(sumper)*100) %>% 
        spread(rep, Per) %>% 
        ungroup()
      
      
      
      sd_lipid <- sd_lipid %>% 
        select(-one_of(name_feat))%>%
        rowwise() %>% 
        do(data.frame( sd_total = sd(unlist(.)))) %>% 
        bind_cols(sd_lipid, .)
      
      #SD for TX
      sd_lipid <- sd_lipid %>% 
        select(starts_with("TX")) %>%
        rowwise() %>% 
        do(data.frame(sd_TX = sd(unlist(.)))) %>% 
        bind_cols(sd_lipid, .)
      
      #SD for gs
      sd_lipid <- sd_lipid %>% 
        select(starts_with("early"),starts_with("mid"),starts_with("late"),starts_with("stat")) %>%
        rowwise() %>% 
        do(data.frame(sd_gs = sd(unlist(.)))) %>% 
        bind_cols(sd_lipid, .)
      
      #SD for temp
      sd_lipid <- sd_lipid %>% 
        select(contains("13"),contains("20"), contains("early"),contains("4"))%>%
        rowwise() %>% 
        do(data.frame(sd_temp = sd(unlist(.))))%>% 
        bind_cols(sd_lipid, .)
      
      #SD for NaCl
      sd_lipid <- sd_lipid %>% 
        select(starts_with("Na")) %>%
        rowwise() %>% 
        do(data.frame(sd_NaCl = sd(unlist(.)))) %>% 
        bind_cols(sd_lipid, .)
      
      #SD for MetOh
      sd_lipid <- sd_lipid %>% 
        select(starts_with("Met")) %>%
        rowwise() %>% 
        do(data.frame(sd_Met = sd(unlist(.)))) %>% 
        bind_cols(sd_lipid, .)
      
      
      sd_all <- sd_lipid %>% 
        select_('starts_with("sd")', name_feat)
      
      
      sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
        mutate(sd_tot = mean(sd_total)) %>% ungroup()
      #order variables base on number
      
      
      sd_all <- sd_all[with(sd_all, order(sd_total)), ]
      #name_feat <- noquote(name_feat)
      
      sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
        mutate(sd_tot = mean(sd_total)) %>% ungroup()
      #order variables base on number
      sd_all <- sd_all[with(sd_all, order(sd_total)), ]
    
      if(is.null(input$exp_con) || input$exp_con == ""){
        sd_all <- sd_all %>%group_by_(.dots = name_feat) %>%
          mutate(sd_tot = mean(sd_total))
        #order variables base on number
        sd_all <- sd_all[with(sd_all, order(sd_total)), ]
        sd_all <- sd_all%>% ungroup()
        sd_all <- as.data.frame(sd_all)
        ggplot(sd_all, aes(x =reorder(sd_all[, name_feat], sd_total), y= sd_total)) + 
          geom_point(aes(colour = cut(sd_total, c(-Inf, sd_tot[1], Inf))),
                     size = 1) +
          coord_flip() +
          geom_hline(aes(yintercept = sd_tot,linetype = "Mean")) +
          scale_linetype_manual(name = " ", values = 1 )  +
          
          theme_bw()+
          theme(axis.text.x = element_text(angle = 0, hjust = 1)
          )+
          labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ") +
          scale_color_manual(name = " ",
                             values = c( "black",
                                         "red"),
                             labels = c("below mean", "over mean"))
      } else { 
        if(input$exp_con == 'Growth Stage') {
          sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
            mutate(mean_gs = mean(sd_gs))
          sd_all <- sd_all%>% ungroup()
          sd_all <- as.data.frame(sd_all)
          
          
          ggplot(sd_all, aes(x =reorder(sd_all[, name_feat], sd_gs), y= sd_gs)) + 
            geom_point(aes(colour = cut(sd_gs, c(-Inf, mean_gs[1], Inf))),
                       size = 1) +
            coord_flip() +
            geom_hline(aes(yintercept = mean_gs,linetype = "Mean")) +
            scale_linetype_manual(name = " ", values = 1 )  +
            
            theme_bw()+
            theme(axis.text.x = element_text(angle = 0, hjust = 1)
            )+
            labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ") +
            scale_color_manual(name = " ",
                               values = c( "black",
                                           "red"),
                               labels = c("below mean", "over mean"))
        } else if(input$exp_con == "Temperature"){
          sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
            mutate(mean_temp = mean(sd_temp))
          sd_all <- sd_all%>% ungroup()
          sd_all <- as.data.frame(sd_all)
          ggplot(sd_all, aes(x =reorder(sd_all[, name_feat], sd_temp), y= sd_temp)) + 
            geom_point(aes(colour = cut(sd_temp, c(-Inf, mean_temp[1], Inf))),
                       size = 1) +
            coord_flip() +
            geom_hline(aes(yintercept = mean_temp,linetype = "Mean")) +
            scale_linetype_manual(name = " ", values = 1 )  +
            
            theme_bw()+
            theme(axis.text.x = element_text(angle = 0, hjust = 1)
            )+
            labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ") +
            scale_color_manual(name = " ",
                               values = c( "black",
                                           "red"),
                               labels = c("below mean", "over mean"))
        }else if(input$exp_con == "Salt"){
          sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
            mutate(mean_NaCl = mean(sd_NaCl))
          sd_all <- sd_all%>% ungroup()
          sd_all <- as.data.frame(sd_all)
          ggplot(sd_all, aes(x =reorder(sd_all[, name_feat], sd_NaCl), y= sd_NaCl)) + 
            geom_point(aes(colour = cut(sd_NaCl, c(-Inf, mean_NaCl[1], Inf))),
                       size = 1) +
            coord_flip() +
            geom_hline(aes(yintercept = mean_NaCl,linetype = "Mean")) +
            scale_linetype_manual(name = " ", values = 1 )  +
            
            theme_bw()+
            theme(axis.text.x = element_text(angle = 0, hjust = 1)
            )+
            labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ") +
            scale_color_manual(name = " ",
                               values = c( "black",
                                           "red"),
                               labels = c("below mean", "over mean"))
        }else if(input$exp_con == "Methanol"){
          sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
            mutate(mean_Met = mean(sd_Met))
          sd_all <- sd_all%>% ungroup()
          sd_all <- as.data.frame(sd_all)
          ggplot(sd_all, aes(x =reorder(sd_all[, name_feat], sd_Met), y= sd_Met)) + 
            geom_point(aes(colour = cut(sd_Met, c(-Inf, mean_Met[1], Inf))),
                       size = 1)+
            coord_flip() +
            geom_hline(aes(yintercept = mean_Met,linetype = "Mean")) +
            scale_linetype_manual(name = " ", values = 1 )  +
            
            theme_bw()+
            theme(axis.text.x = element_text(angle = 0, hjust = 1)
            )+
            labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ") +
            scale_color_manual(name = " ",
                               values = c( "black",
                                           "red"),
                               labels = c("below mean", "over mean"))
        }else if(input$exp_con == "Triton"){
          sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
            mutate(mean_TX = mean(sd_TX))
          sd_all <- sd_all%>% ungroup()
          sd_all <- as.data.frame(sd_all)
          ggplot(sd_all, aes(x = reorder(sd_all[, name_feat], sd_TX), y= sd_TX)) + 
            geom_point(aes(colour = cut(sd_TX, c(-Inf, mean_TX[1], Inf))),
                       size = 1) +
            coord_flip() +
            geom_hline(aes(yintercept = mean_TX,linetype = "Mean")) +
            scale_linetype_manual(name = " ", values = 1 )  +
            
            theme_bw()+
            theme(axis.text.x = element_text(angle = 0, hjust = 1)
            )+
            labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ") +
            scale_color_manual(name = " ",
                               values = c( "black",
                                           "red"),
                               labels = c("below mean", "over mean"))
        }
        
        
      }
                                                           ############## LINE PLOT ##################
    } else if(input$plot_type == "Line Plot"){
      df[is.na(df)] <- 0
      if (input$feat == "Double Bonds") {
        temp_DB<-df
        if(!input$hg){
          temp_DB <- select(temp_DB, -c(length, class))
          temp_DB_gat <- temp_DB %>% gather(rep, num, -one_of("DB"))
          temp_DB_gat$rep<-sub("\\d$","",temp_DB_gat$rep)
          temp_DB_gat$rep<-sub("\\.$","",temp_DB_gat$rep)
          
          temp_DB_gat <- temp_DB_gat %>% filter()
          
          temp_DB_gat_sum <-
            temp_DB_gat%>%
            group_by(rep) %>%                     
            dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,DB)%>%
            dplyr::summarise(Per =sum(sumper)*100, sd = sd(sumper))
          
          temp_DB_gat_sum$DB <- factor(temp_DB_gat_sum$DB, levels = c( unique(temp_DB_gat_sum$DB) ) )
          
          ggplot(temp_DB_gat_sum, aes(x = rep, y = Per)) + 
            geom_point(aes(colour = DB), size = 5) +
            geom_line(aes(colour = DB, group = DB)) +
            geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
            theme_bw()+
            #theme(legend.position = "none")+
            coord_equal(ratio =1/20)+
            ylim(0,100) +
            labs(title = input$plotTitle5, x = input$xlab5, y = input$ylab5, color = " ")
        }
        else {
          temp_DB <- select(temp_DB, -c(length))
          temp_DB_gat <- temp_DB %>% gather(rep, num, -one_of("DB","class"))
          temp_DB_gat$rep<-sub("\\d$","",temp_DB_gat$rep)
          temp_DB_gat$rep<-sub("\\.$","",temp_DB_gat$rep)
          
          temp_byclass_gat_sum <- temp_DB_gat %>%
            group_by(rep,class) %>%                     
            dplyr::mutate(sumper=num/sum(num))%>%
            group_by(rep,class,DB)%>%
            dplyr::summarise(Per=sum(sumper)*100, sd=sd(sumper))
          
          temp_byclass_gat_sum$DB <- factor(temp_byclass_gat_sum$DB, levels = c( unique(temp_byclass_gat_sum$DB) ) )
          temp_byclass_gat_sum$class <- factor(temp_byclass_gat_sum$class, levels = c( unique(temp_byclass_gat_sum$class) ) )
          
          ggplot(temp_byclass_gat_sum, aes(x = rep, y = Per), fill = "gray") + 
            geom_point(aes(colour = DB), size = 5) +
            geom_line(aes(colour = DB, group = DB)) +
            facet_grid(. ~ class) +
            geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
            theme_bw() + 
            coord_equal(ratio =1/20) +
            ylim(0,100) +
            labs(title = input$plotTitle5, x = input$xlab5, y = input$ylab5, color = " ")
        }
      }else if(input$feat == "Length") {
        
        temp_LEN<-df
        if(!input$hg){
          temp_LEN <- select(temp_LEN, -class, -DB)
          temp_len_gat <- temp_LEN %>% gather(rep, num, -one_of("length"))
          temp_len_gat$rep<-sub("\\d$","",temp_len_gat$rep)
          temp_len_gat$rep<-sub("\\.$","",temp_len_gat$rep)
          temp_len_gat <- temp_len_gat %>% filter()
          #wt_DB_gat_sum <-wt_DB_gat %>% group_by(rep, DB) %>% summarise_all(funs(sum(.)))
          temp_len_gat_sum <-
            temp_len_gat%>%
            group_by(rep) %>%                     
            dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,length)%>%
            dplyr::summarise(Per =sum(sumper)*100, sd = sd(sumper))
          
          temp_len_gat_sum$length <- factor(temp_len_gat_sum$length, levels = c(unique(temp_len_gat_sum$length)) )
          
          ggplot(temp_len_gat_sum, aes(x = rep, y = Per)) + 
            geom_point(aes(color = length), size = 5) +
            geom_line(aes(colour = length, group = length)) +
            #geom_dl(aes(label=length),method="last.points")
            #geom_text_repel(data = last_text, aes(x=rep, y = Per, label = length), hjust = 0, vjust = 0.35)+
            geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
            theme_bw()+
            #theme(legend.position = "none")+
            coord_equal(ratio =1/20)+
            labs(title = "LENGTH - only temp") +
            ylim(0,100)+
            labs(title = input$plotTitle5, x = input$xlab5, y = input$ylab5, color = " ")
        }
        else {
          temp_LEN <- select(temp_LEN,  -DB)
          temp_len_gat <- temp_LEN %>% gather(rep, num, -one_of("length","class"))
          temp_len_gat$rep<-sub("\\d$","",temp_len_gat$rep)
          temp_len_gat$rep<-sub("\\.$","",temp_len_gat$rep)
          
          LEN_byclass_gat_sum <- temp_len_gat %>%
            group_by(rep,class) %>%                     
            dplyr::mutate(sumper=num/sum(num))%>%
            group_by(rep,class,length)%>%
            dplyr::summarise(Per=sum(sumper)*100, sd=sd(sumper))
          
          LEN_byclass_gat_sum$length <- factor(LEN_byclass_gat_sum$length, levels = c(unique(LEN_byclass_gat_sum$length)) )
          LEN_byclass_gat_sum$class <- factor(LEN_byclass_gat_sum$class, levels = c(unique(LEN_byclass_gat_sum$class)) )
          
          ggplot(LEN_byclass_gat_sum, aes(x = rep, y = Per)) + 
            geom_point(aes(colour = length), size = 5) +
            geom_line(aes(colour = length, group = length)) +
            facet_grid(. ~ class) +
            geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
            theme_bw() + 
            coord_equal(ratio =1/20) +
            ylim(0,100)+
            labs(title = input$plotTitle5, x = input$xlab5, y = input$ylab5, color = " ")
        }
      } else if(input$feat == "Species"){
        

        temp_byclass<-df
        colnames(temp_byclass)[colnames(temp_byclass)=="class"] <- "lipid"
        temp_byclass<-temp_byclass%>%separate(lipid, into = c("class", "bal"),sep =  " ",remove = FALSE)
        temp_byclass <- temp_byclass %>% select(-bal, - length, -DB)
        temp_byclass$class <-  sub("^m", "", temp_byclass$class )
        temp_byclass$class <-  sub("DIP$", "DIPs", temp_byclass$class )
        temp_byclass_gat <- temp_byclass %>% gather(rep, num, -one_of("lipid","class"))
        temp_byclass_gat$rep<-sub("\\d$","",temp_byclass_gat$rep)
        temp_byclass_gat$rep<-sub("\\.$","",temp_byclass_gat$rep)
        temp_byclass_gat <- temp_byclass_gat %>% filter()
        #wt_DB_gat_sum <-wt_DB_gat %>% group_by(rep, DB) %>% summarise_all(funs(sum(.)))
        temp_byclass_gat_sum <-
          temp_byclass_gat%>%
          group_by(rep,class) %>%                     
          dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,class,lipid)%>%
          dplyr::summarise(Per =sum(sumper)*100, sd = sd(sumper))
        
       # temp_byclass_gat_sum$lipid <- factor(temp_byclass_gat_sum$lipid, levels = c(unique(temp_byclass_gat_sum$lipid)) )
        
        
        p<- ggplot(temp_byclass_gat_sum, aes(x = rep, y = Per)) + 
          geom_point(aes(color = lipid), size = 5) +
          geom_line(aes(colour = lipid, group = lipid)) +
          geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
          theme_bw()+
          #theme(legend.position = "none")+
          coord_equal(ratio =1/20)+
          ylim(0,100)+
          labs(title = input$plotTitle5, x = input$xlab5, y = input$ylab5, color = " ")
        if(input$hg){
          p <- p + facet_grid(. ~ class)
        }
        
        p
        
        
      } else {
        
        temp_byclass<-df
        temp_byclass<-temp_byclass%>%separate(class, into = c("class", "bal"),sep =  " ")
        temp_byclass <- temp_byclass %>% select(-bal, - length, -DB)
        temp_byclass$class <-  sub("^m", "", temp_byclass$class )
        temp_byclass$class <-  sub("DIP", "DIPs", temp_byclass$class )
        temp_byclass_gat <- temp_byclass %>% gather(rep, num, -one_of("class"))
        temp_byclass_gat$rep<-sub("\\d$","",temp_byclass_gat$rep)
        temp_byclass_gat$rep<-sub("\\.$","",temp_byclass_gat$rep)
        temp_byclass_gat <- temp_byclass_gat %>% filter()
        #wt_DB_gat_sum <-wt_DB_gat %>% group_by(rep, DB) %>% summarise_all(funs(sum(.)))
        temp_byclass_gat_sum <-
          temp_byclass_gat%>%
          group_by(rep) %>%                     
          dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,class)%>%
          dplyr::summarise(Per =sum(sumper)*100, sd = sd(sumper))
        
        temp_byclass_gat_sum$class <- factor(temp_byclass_gat_sum$class, levels = c(unique(temp_byclass_gat_sum$class)) )
        
        
        p<- ggplot(temp_byclass_gat_sum, aes(x = rep, y = Per)) + 
          geom_point(aes(color = class), size = 5) +
          geom_line(aes(colour = class, group = class)) +
          geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
          theme_bw()+
          #theme(legend.position = "none")+
          coord_equal(ratio =1/20)+
          ylim(0,100)+
          labs(title = input$plotTitle5, x = input$xlab5, y = input$ylab5, color = " ")
        if(input$hg){
          p <- p + facet_grid(. ~ class)
        }
        
        p
      }
      
    }
      
      
      
      
      
      
    
    })
  
  
  
  
  
  
  
  
  output$plot <- renderPlot(Plot() ) 

  
  
  output$selected_var <- renderText({ 
    if(is.null(input$exp_con)){
      paste("Experimental conditions:", "All" )
    }else{
      #con <- list(input$exp_con)
      sprintf("Experimental conditions: %s", paste(input$exp_con ,  collapse=', '))
    }
    
  })
  output$selected_feat <- renderText({ 
    paste("Lipids feature:", input$feat )
  })
  
  # output$info <- renderPrint({
  #   # With base graphics, need to tell it what the x and y variables are.
  #   brushedPoints(bx_data_gat, input$plot_brush)
  # })
  
                                                  ################ RESET INPUT ##############
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::reset("feat")
    
  })
  
  
                                                      ########## BRUSHED POINTS ##############
  output$plot_brushed_points<-renderDataTable({
    df <- fncols(Detail(), c("class","length","DB"))
      df[is.na(df)] <- 0
      if (input$feat == "Double Bonds") {
        bx_data <- df %>%
          select( -c(length)) %>% separate(class, into = c("class", "bal"),sep =  " ") %>% 
          select(-bal)
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("DB", "class"))
        input_name <- "DB"
        bx_data_gat <- bx_data_gat %>%
          group_by(exp_condition,class) %>%                     
          dplyr::mutate(sumper=abundance/sum(abundance))%>%
          group_by_("exp_condition","class",input_name)%>%
          dplyr::summarise(abundance=sum(sumper)*100)
        bx_data_gat$DB <- factor(bx_data_gat$DB, levels=(unique(bx_data_gat$DB)))
      } else if(input$feat == "Length") {
        bx_data <- df %>%
          select( -c(DB))%>%
          separate(class, into = c("class", "bal"),sep =  " ") %>% 
          select(-bal)
        input_name <- "length"
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("length","class"))
        bx_data_gat <- bx_data_gat %>%
          group_by(exp_condition,class) %>%                     
          dplyr::mutate(sumper=abundance/sum(abundance))%>%
          group_by_("exp_condition","class",input_name)%>%
          dplyr::summarise(abundance=sum(sumper)*100)
        bx_data_gat$length <- factor(bx_data_gat$length, levels=(unique(bx_data_gat$length)))
        
      } else if(input$feat == "Head Group"){
        bx_data <- df %>% select( -c(DB,length)) 
        input_name <- "class"
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("class"))
        bx_data_gat <- bx_data_gat %>%
          group_by(exp_condition) %>%                     
          dplyr::mutate(sumper=abundance/sum(abundance))%>%
          group_by_("exp_condition",input_name)%>%
          dplyr::summarise(abundance=sum(sumper)*100)
        bx_data_gat$class <- factor(bx_data_gat$class, levels=(unique(bx_data_gat$class)))
        
      } else {
        bx_data <- df
        colnames(bx_data)[colnames(bx_data)=="class"] <- "lipid"
        bx_data <- bx_data %>% select( -c(DB,length)) %>%
          separate(lipid, into = c("class", "bal"),sep =  " ",remove = FALSE) %>% 
          select(-bal)
        bx_data$class <-  sub("^m", "", bx_data$class )
        bx_data$class <-  sub("DIP", "DIPs", bx_data$class )
        bx_data_gat <- bx_data %>% gather(exp_condition, abundance, -one_of("class", "lipid"))
        bx_data_gat$lipid <- factor(bx_data_gat$lipid, levels=(unique(bx_data_gat$lipid)))
        input_name <- "lipid"
      }
      #https://stackoverflow.com/questions/39609830/r-shiny-ggplot-brush
    res<- brushedPoints(bx_data_gat, input$plot_brush)
    #subset_res<-subset(res,select=c(HUC12,Ecological_Health,Ecological_Stress,Comprehensive_Score))
    print (res)
  })
  
  # output$brush_info<-renderPrint({
  #   
  #   #cat("input$plot_brush:\n")
  #   input$plot_brush
  # })
  # 
  # When the save button is clicked, add the plot to a list and clear the input
  observeEvent(input$save_plot_btn, {
    plot_name <- trimws(input$save_plot_name)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot()
    }
  })
  
  observeEvent(input$save_plot_duplicate_confirm, {
    save_plot()
    removeModal()
  })
  save_plot <- function() {
    shinyjs::show("save_plot_checkmark")
    values$plots[[trimws(input$save_plot_name)]] <- Plot() 
    updateTextInput(session, "save_plot_name", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark", anim = TRUE, animType = "fade")
    )
  }
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn",
                         condition = nzchar(trimws(input$save_plot_name)))
    
  })
  # ----------- Export tab -----------
  
  # Create a server variable that we can use in the UI for a conditionalPanel
  output$saved_plots_exist <- reactive({
    length(values$plots) > 0
  })
  outputOptions(output, 'saved_plots_exist', suspendWhenHidden = FALSE)
  
  countNoun <- function(num, noun) {
    if (num == 1) paste0(num, " ", noun)
    else paste0(num, " ", noun, "s")
  }
  
  output$export_btn_ui <- renderUI({
    btn_text <- paste0("Download ",
                       countNoun(length(input$plots_order), "plot"),
                       " (", countNoun(export_num_pages(), "page"), ")")
    downloadButton("export_btn", btn_text)
  })
  
  # Select the plots and the order of the plots to export
  output$plots_order_ui <- renderUI({
    selectizeInput("plots_order", "Plots to export (drag to reorder)",
                   choices = names(values$plots), selected = names(values$plots),
                   multiple = TRUE, options = list(
                     plugins = list('drag_drop','remove_button')))
  })
  
  # If no plots are chosen to export, don't show all the export options
  observe({
    shinyjs::toggle(selector = "#exporting_plots_options, #preview_plots_options",
                    condition = length(input$plots_order) > 0)
  })
  
  # Show a dropdown to select which page to show
  output$plots_select_page_ui <- renderUI({
    num_pages <- export_num_pages()
    
    # Try to remain on the same page even when the dropdown changes
    isolate({
      if (!is.null(input$plots_select_page) &&
          as.numeric(input$plots_select_page) <= num_pages) {
        selected <- input$plots_select_page
      } else {
        selected <- 1
      }
    })
    selectInput("plots_select_page", "Page to preview",
                choices = seq(num_pages), selected = selected)
  })
  
  # Calculate the number of pages to export
  export_num_pages <- reactive({
    if (input$export_multiple) {
      plots_per_page <- input$export_nrow * input$export_ncol
      pages <- ceiling(length(input$plots_order) / plots_per_page)
    } else {
      pages <- length(input$plots_order)
    }
    pages
  })
  
  # print a specific page of plots (either 1 plot/page or multiple rows/cols)
  export_print_page <- function(page) {
    page <- as.numeric(page)
    if (!input$export_multiple) {
      plot_name <- input$plots_order[page]
      values$plots[plot_name]
    } else {
      plots_per_page <- input$export_nrow * input$export_ncol
      idx_start <- (page - 1) * plots_per_page + 1
      idx_end <- min(length(input$plots_order), page * plots_per_page)
      if (idx_start > idx_end) {
        return()
      }
      plot_names <- input$plots_order[idx_start:idx_end]
      plots <- values$plots[plot_names]
      
      gridExtra::grid.arrange(
        grobs = plots,
        nrow = input$export_nrow,
        ncol = input$export_ncol,
        as.table = (input$export_arrangement == "byrow")
      )
    }
  }
  
  # Show a dropdown to select a plot to remove in the Export tab
  output$plots_remove_ui <- renderUI({
    selectInput("plots_remove", NULL, names(values$plots))
  })
  
  # Preview a plot in the Export tab
  output$plot_preview <- renderPlot({
    if (is.null(input$plots_select_page)) {
      return()
    }
    export_print_page(input$plots_select_page)
  },
  width = function() { plot_preview_width() },
  height = function() { plot_preview_height() })
  
  # Return the dimensions of the PDF page selected in the Export tab
  pdf_page_dim <- reactive({
    if (input$export_pdf_orientation == "landscape") {
      width <- 11
      height <- 8.5
    } else if (input$export_pdf_orientation == "portrait") {
      width <- 8.5
      height <- 11
    } else {
      width <- input$export_pdf_width
      height <- input$export_pdf_height
    }
    list(width = width, height = height)
  })
  
  # Calculate the dimensions of the plot preview
  plot_preview_dim <- reactive({
    # If it's PDF, the units are inches and default resolution is 72 px/inch
    if (input$export_file_type == "pdf") {
      width <- pdf_page_dim()$width * 72
      height <- pdf_page_dim()$height * 72
    } else {
      width <- input$export_file_width
      height <- input$export_file_height
    }
    
    # Keep the aspect ratio, but make the max dimensions 500
    ratio <- height/width
    if (ratio > 1) {
      height <- 500
      width <- height/ratio
    } else {
      width <- 500
      height <- ratio*width
    }
    
    list(width = width, height = height)
  })
  plot_preview_width <- reactive({
    plot_preview_dim()$width
  })
  plot_preview_height<- reactive({
    plot_preview_dim()$height
  })
  
  # Remove the currently selected plot from the saved plots list
  observeEvent(input$remove_plot_btn, {
    values$plots[[input$plots_remove]] <- NULL
  })
  
  # Determine the file name of the exported plots file.
  # If there's only one plot or using PDF, export it in its raw format.
  # Multiple plots in non-PDF format are zipped together.
  export_file_name <- reactive({
    if (export_num_pages() == 1 || input$export_file_type == "pdf") {
      paste0("export_plots", ".", input$export_file_type)
    } else {
      paste0("export_plots", ".zip")
    }
  })
  
  # Download the saved plots
  output$export_btn <- downloadHandler(
    filename = function() {
      export_file_name()
    },
    content = function(file) {
      tryCatch({
        file_type <- input$export_file_type
        
        # If saving as PDF, save all pages in one file
        if (file_type == "pdf") {
          width <- pdf_page_dim()$width
          height <- pdf_page_dim()$height
          
          file_names <- "export_plots.pdf"
          grDevices::pdf(file = file_names, width = width, height = height,
                         title = file_names, onefile = TRUE)
          
          if (!input$export_multiple) {
            plots <- values$plots[input$plots_order]
            invisible <- lapply(plots, print)
          } else {
            num_pages <- export_num_pages()
            for (page in seq(num_pages)) {
              suppressMessages(print(export_print_page(page)))
            }
          }
          
          grDevices::dev.off()
        }
        # If saving as raw images, save each page as a separate file
        else {
          num_pages <- export_num_pages()
          for (page in seq(num_pages)) {
            export_print_page(page)
          }
          file_names <- lapply(seq(num_pages), function(page) { 
            file_name <- paste0("export_plots_p", page, ".", file_type)
            export_params <- list(file_name,
                                  width = input$export_file_width,
                                  height = input$export_file_height)
            do.call(file_type, export_params)
            print(export_print_page(page))
            grDevices::dev.off()
            file_name
          })
          file_names <- unlist(file_names)
        }
        
        # If there's a single file, download the file. If multiple files, zip
        if (length(file_names) == 1) {
          file.copy(file_names, file, overwrite = TRUE)
        } else {
          zip(file, file_names)
        }
        
        # Remove the generated files so that we don't run out of disk space :)
        
        file.remove(file_names)
      },
      error = function(err) {
        stop(err$message)
      })
    }
  )
  
  
})
