sd_lipid <- df
df[is.na(df)] <- 0
#names(filtereddata()) <- sub("^X","",names(filtereddata()))
#SD for total

if(input$feat == "Species"){
  sd_all <- sd_all %>% select(-one_of("DB","length"))
  sd_lipid <- sd_all %>% gather(rep, num, -one_of("length","DB")) %>% tbl_df()
  name_feat = "class"
} else if(input$feat == "Head Group"){
  sd_all <- sd_all %>% select(-one_of("DB","length"))
  sd_lipid <- sd_all %>% gather(rep, num, -one_of("length","DB")) %>% tbl_df()
  name_feat = "class"
} else if(input$feat == "Length"){
  sd_all <- sd_all %>% select(-one_of("DB","class"))
  sd_lipid <- sd_all %>% gather(rep, num, -one_of("DB","class")) %>% tbl_df()
  name_feat = "length"
}else if(input$feat == "Double Bonds"){
  sd_all <- sd_all %>% select(-one_of("class","length"))
  sd_lipid <- sd_all %>% gather(rep, num, -one_of("length","class")) %>% tbl_df()
  name_feat = "DB"
}

sd_lipid <- sd_lipid %>%
  group_by(rep) %>%                     
  dplyr::mutate(sumper=num/sum(num)) %>%
  group_by(rep, name_feat) %>%
  dplyr::summarise(Per=sum(sumper)*100) %>% 
  spread(rep, Per) %>% 
  ungroup()



sd_lipid <- sd_lipid %>% 
  select_(.dots= (name_feat)) %>%
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
  select(contains("13"),contains("20"), contains("early"),contains("4") )%>%
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
  select(starts_with("sd"), name_feat)


sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
  mutate(sd_tot = mean(sd_total))
#order variables base on number


sd_all <- sd_all[with(sd_all, order(sd_total)), ]
name_feat <- noquote(name_feat)

sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
  mutate(sd_tot = mean(sd_total))
#order variables base on number
sd_all <- sd_all[with(sd_all, order(sd_total)), ]



if(is.null(input$exp_con) || input$exp_con == ""){
  sd_all <- sd_all %>%group_by_(.dots = name_feat) %>%
    mutate(sd_tot = mean(sd_total))
  #order variables base on number
  sd_all <- sd_all[with(sd_all, order(sd_total)), ]
  
  ggplot(sd_all, aes(x =reorder(name_feat, sd_total), y= sd_total)) + 
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
  if(input$exp_con == "gs") {
    sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
      mutate(mean_gs = mean(sd_gs))
    
    
    
    ggplot(sd_all, aes(x =reorder(name_feat, sd_gs), y= sd_gs)) + 
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
  } else if(input$exp_con == "temp"){
    sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
      mutate(mean_temp = mean(sd_temp))
    
    ggplot(sd_all, aes(x =reorder(name_feat, sd_temp), y= sd_temp)) + 
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
  }else if(input$exp_con == "salt"){
    sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
      mutate(mean_NaCl = mean(sd_NaCl))
    
    ggplot(sd_all, aes(x =reorder(name_feat, sd_NaCl), y= sd_NaCl)) + 
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
  }else if(input$exp_con == "met"){
    sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
      mutate(mean_Met = mean(sd_Met))
    
    ggplot(sd_all, aes(x =reorder(name_feat, sd_Met), y= sd_Met)) + 
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
  }else if(input$exp_con == "tri"){
    sd_all <- sd_all %>% group_by_(.dots = name_feat) %>%
      mutate(mean_TX = mean(sd_TX))
    
    ggplot(sd_all, aes(x = reorder(name_feat, sd_TX), y= sd_TX)) + 
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

}