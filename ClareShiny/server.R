server <- function(input, output) {
  
  dateRangeInput<-reactive({
    dataset <- subset(Raw_Data, reported >= input$daterange[1] & reported <= input$daterange[2] &
                        patient_age_in_years >= 15 & patient_age_in_years <=49)
    
  })
  
  output$Eligible_Women <- renderValueBox({
    valueBox(summarise(dateRangeInput(),  Eligible_Women = n_distinct(patient_id)),
             "Women Eligible for FP (15-49) years)", icon = icon("list"),
             color = "purple"
    )
  })
  
  dateRangeInput2<-reactive({
    dataset <- subset(Raw_Data, reported >= input$daterange[1] & reported <= input$daterange[2] &
                        patient_age_in_years >= 15 & patient_age_in_years <=49 &
                        in_the_household == TRUE & is_pregnant == FALSE)
  })
  
  output$FP_Clients <- renderValueBox({
    valueBox(summarise(dateRangeInput2(),  FP_Clients = n_distinct(patient_id)),
             "Eligible women registered for FP (not pregnant & in HH)", icon = icon("list"),
             color = "purple"
    )
  })
  
  dateRangeInput3<-reactive({
    dataset <- subset(Raw_Data, reported >= input$daterange[1] & reported <= input$daterange[2] &
                        patient_age_in_years >= 15 & patient_age_in_years <=49 &
                        in_the_household == TRUE & is_pregnant == FALSE & using_modern_fp == FALSE) 
  })
  
  output$Not_Using_Modern_FP <- renderValueBox({
    valueBox(summarise(dateRangeInput3(),  Not_Using_Modern_FP = n_distinct(patient_id)),
             "Women who have used modern FP but not currently using", icon = icon("list"),
             color = "purple"
    )
  })
  
  dateRangeInput4<-reactive({
    dataset <- subset(Raw_Data, reported >= input$daterange[1] & reported <= input$daterange[2] &
                        patient_age_in_years >= 15 & patient_age_in_years <=49 &
                        in_the_household == TRUE & is_pregnant == FALSE & used_modern_fp == FALSE) 
  })
  
  output$NeverUsed_Modern_FP <- renderValueBox({
    valueBox(summarise(dateRangeInput4(),  Not_Using_Modern_FP = n_distinct(patient_id)),
             "Women who have never used modern FP", icon = icon("list"),
             color = "purple"
    )
  })
  
  dateRangeInput5<-reactive({
    dataset <- subset(Raw_Data, reported >= input$daterange[1] & reported <= input$daterange[2] &
                        patient_age_in_years >= 15 & patient_age_in_years <=49 &
                        in_the_household == TRUE & is_pregnant == FALSE & using_modern_fp == FALSE & 
                        take_on_switch == TRUE) 
  })
  output$Unmet_needFP1 <- renderValueBox({
    valueBox(summarise(dateRangeInput5(), Unmet_needFP = n_distinct(patient_id)),
             "Women who would like to take up modern FP who have previously been in use", icon = icon("list"),
             color = "purple"
    )
  })
  
  
  dateRangeInput6<-reactive({
    dataset <- subset(Raw_Data, reported >= input$daterange[1] & reported <= input$daterange[2] &
                        patient_age_in_years >= 15 & patient_age_in_years <=49 &
                        in_the_household == TRUE & is_pregnant == FALSE & used_modern_fp == FALSE & 
                        take_on_switch == TRUE) 
  })
  output$Unmet_needFP2 <- renderValueBox({
    valueBox(summarise(dateRangeInput6(), Unmet_needFP = n_distinct(patient_id)),
             "Women who would like to take up modern FP who have never used any before", icon = icon("list"),
             color = "purple"
    )
  })
  
  
  output$Unmet_needFP<- renderValueBox({
    valueBox(
      paste0(250), "Total women with unmet need for FP (26%) at registration (Mar-Nov '18)", icon = icon("list"),
      color = "purple"
    )
  })
  
  # dateRangeInput5<-reactive({
  #   dataset <- subset(Raw_Data, reported >= input$daterange[1] & reported <= input$daterange[2] &
  #                       patient_age_in_years >= 15 & patient_age_in_years <=49 &
  #                       in_the_household == TRUE & is_referral_case == TRUE) 
  # })
  # 
  # output$Referrals <- renderValueBox({
  #   valueBox(summarise(dateRangeInput5(), Referrals = n_distinct(patient_id)),
  #     "Referrals to HFs to discuss suitable FP for women with no risk factors", icon = icon("list"),
  #     color = "purple"
  #   )
  # })
  # 
  #   
  dateRangeInput7<-reactive({
    dataset <- subset(Preg_Reg, reported1 >= input$daterange1[1] & reported1 <= input$daterange1[2] &
                        pregnant == TRUE)
    
  })
  
  output$Expectant_Women <- renderValueBox({
    valueBox(summarise(dateRangeInput7(),  Expectant_Women = n_distinct(patient_id)),
             "Expectant women", icon = icon("list"),
             color = "purple"
    )
  })
  
  output$Newborns <- renderValueBox({
    valueBox(
      paste0(985), "# of Newborns", icon = icon("list"),
      color = "purple"
    )
  })
  
  #   output$FirstPNC_Visit <- renderValueBox({
  #     valueBox(
  #       paste0(803), "1st PNC Visits", icon = icon("list"),
  #       color = "purple"
  #     )
  #   })
  #   
  #   output$Newborns_U1Week <- renderValueBox({
  #     valueBox(
  #       paste0(114), "Newborns less than 1 week", icon = icon("list"),
  #       color = "purple"
  #     )
  #   })
  #   
  #   output$TMonths <- renderValueBox({
  #     valueBox(
  #       paste0(58), "3 months olds", icon = icon("list"),
  #       color = "purple"
  #     )
  #   })
  #   
  #   output$SMonths <- renderValueBox({
  #     valueBox(
  #       paste0(49), "6 months olds", icon = icon("list"),
  #       color = "purple"
  #     )
  #   })
  #   
  #   output$NMonths <- renderValueBox({
  #     valueBox(
  #       paste0(152), "9 months olds", icon = icon("list"),
  #       color = "purple"
  #     )
  #   })
  #   
  #   output$EMonths <- renderValueBox({
  #     valueBox(
  #       paste0(103), "18 months olds", icon = icon("list"),
  #       color = "purple"
  #     )
  #   })
  #   
  #   #output$Short_Term_Users <- renderValueBox({
  #   #valueBox(
  #   # paste0(Short_Term1_No), "Follow ups for women who took a short term method", icon = icon("list"),
  #   #color = "purple"
  #   # )
  #   # })
  #   #creating the plotOutput content
  #   
  output$Preg <- renderPlot({
    ggplot(Preg_Status, aes(fill=is_pregnant, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer("Pregnant") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
  })
  
  output$Modern_FP<- renderPlot({
    ggplot(Modern_FP, aes(fill=using_modern_fp, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer("Using Modern FP") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
  })
  #   
  #   output$Take_Switch<- renderPlot({
  #     ggplot(Unmet_Need, aes(fill=take_on_switch, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Take/Switch") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=16, face="bold")) +
  #       theme(legend.text = element_text(size = 14, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #   })
  #   
  #   
  output$Methods<- renderPlot({
    ggplot(data =Methods_Used, aes(x = current_method, y = value, fill = current_method,label = value)) +
      geom_col(position = "dodge") +
      geom_text(position = position_dodge(width = 1.0))+
      scale_fill_brewer(type = 'div') +
      theme_minimal() +
      labs(x = "Method", y = "Prevalence of Usage") +
      theme(legend.position="none")
    
    
  })
  
  output$Methods2<- renderPlot({
    ggplot(data =Methods_Administered , aes(x = fp_method_administered, y = value, fill = fp_method_administered,label = value)) +
      geom_col(position = "dodge") +
      geom_text(position = position_dodge(width = 1.0))+
      scale_fill_brewer(type = 'div') +
      theme_minimal() +
      labs(x = "Method", y = "Prevalence of Usage") +
      theme(legend.position="none") +
      coord_flip()
  })  
  
  
  
  
  #   output$Reasons<- renderPlot({
  #     ggplot(data =Reasons, aes(x = "", y = value, fill = g_reason_not_switch,label = value)) + 
  #       geom_col(position = "dodge") +
  #       geom_text(position = position_dodge(width = 0.9))+
  #       theme_minimal() + 
  #       labs( y = "Prevalence") 
  #     
  #   })
  #   
  output$Side_Effects_Presence<- renderPlot({
    ggplot(Short_Users, aes(fill=has_side_effects, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer("Has side effects") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
  })
  
  output$Side_Effects<- renderPlot({
    ggplot(data =Short_Term1, aes(x = side_effects, y = value, fill = side_effects,label = value)) +
      geom_col(position = "dodge") +
      geom_text(position = position_dodge(width = 1.0))+
      scale_fill_brewer(type = 'div') +
      theme_minimal() +
      labs(x = "Side effect", y = "Prevalence") +
      theme(legend.position="none")+
      coord_flip()
  })
  
  output$Prospective_Take<- renderPlot({
    ggplot(Prospective_Users, aes(fill=take_on_switch, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer("Take/Switch") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
  })
  
  output$New_FP<- renderPlot({
    ggplot(data =Method1, aes(x = new_fp_method, y = value, fill = new_fp_method,label = value)) +
      geom_col(position = "dodge") +
      geom_text(position = position_dodge(width = 1.0))+
      scale_fill_brewer(type = 'div') +
      theme_minimal() +
      labs(x = "New FP method", y = "Prevalence") +
      theme(legend.position="none")
    
  })
  
  output$llin<- renderPlot({
    ggplot(LLIN_Users, aes(fill=llin, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer("LLIN") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
    
  })
  
  output$ANC<- renderPlot({
    ggplot(Gone_ANC, aes(fill=gone_for_anc, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer("Gone for ANC") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  })
  
  output$TT<- renderPlot({
    ggplot(TT, aes(fill=tt_immunization, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer("TT Immunization") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
  })
  
  output$Delivery<- renderPlot({
    ggplot(data=Del_Outcome)+
      geom_bar(aes(x="", y=per, fill=display_delivery_outcome), stat="identity", width = 1)+
      coord_polar("y", start=2)+
      theme_void()+
      geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))
    
  })
  
  output$HF<- renderPlot({
    ggplot(HF_Delivery1, aes(fill= health_facility_delivery, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer(" HF Delivery") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
  })
  
  output$ANC_Visits<- renderPlot({
    ggplot(data =ANC_Visit, aes(x = anc_visits_count, y = value, fill = anc_visits_count,label = value)) +
      geom_col(position = "dodge") +
      geom_text(position = position_dodge(width = 1.0))+
      scale_fill_brewer(type = 'seq') +
      theme_minimal() +
      labs(x = "HF ANC visits attended", y = "Number") +
      theme(legend.position="none")
  })
  
  output$Breast_fed<- renderPlot({
    ggplot(HF_Delivery1, aes(fill= health_facility_delivery, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
      geom_rect(color='Black') +
      coord_polar(theta="y") +
      xlim(c(1, 4)) +
      scale_fill_brewer(" HF Delivery") + blank_theme +
      theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
      theme(panel.grid=element_blank()) +
      theme(axis.text=element_blank()) +
      theme(axis.ticks=element_blank()) +
      theme(legend.title = element_text(size=16, face="bold")) +
      theme(legend.text = element_text(size = 14, face = "bold")) +
      geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
    
  })
  #   
  #   output$Vacc1<- renderPlot({
  #     ggplot(Received_Vaccine, aes(fill= g_received_vaccines, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Received vaccine") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=16, face="bold")) +
  #       theme(legend.text = element_text(size = 14, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #   })
  #   
  #   output$Immun1<- renderPlot({
  #     ggplot(Immun_Updated1, aes(fill= g_immunization_updated, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Immunization Upto_date") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=10, face="bold")) +
  #       theme(legend.text = element_text(size = 10, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   output$Vacc3<- renderPlot({
  #     ggplot(Received_Vaccine3, aes(fill= g_received_vaccines, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Received vaccine") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=16, face="bold")) +
  #       theme(legend.text = element_text(size = 14, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   output$Immun3<- renderPlot({
  #     ggplot(Immun_Updated3, aes(fill= g_immunization_updated, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Immunization Upto_date") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=10, face="bold")) +
  #       theme(legend.text = element_text(size = 10, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   output$Vacc6<- renderPlot({
  #     ggplot(Received_Vaccine6, aes(fill= g_received_vaccines, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Received vaccine") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=16, face="bold")) +
  #       theme(legend.text = element_text(size = 14, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   
  #   output$Immun6<- renderPlot({
  #     ggplot(Immun_Updated6, aes(fill= g_immunization_updated, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Immunization Upto_date") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=10, face="bold")) +
  #       theme(legend.text = element_text(size = 10, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   output$Vacc9<- renderPlot({
  #     ggplot(Received_Vaccine9, aes(fill= g_received_vaccines, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Received vaccine") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=16, face="bold")) +
  #       theme(legend.text = element_text(size = 14, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   output$Immun9<- renderPlot({
  #     ggplot(Immun_Updated9, aes(fill= g_immunization_updated, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Immunization Upto_date") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=10, face="bold")) +
  #       theme(legend.text = element_text(size = 10, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   output$Vacc18<- renderPlot({
  #     ggplot(Received_Vaccine18, aes(fill= g_received_vaccines, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Received vaccine") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=16, face="bold")) +
  #       theme(legend.text = element_text(size = 14, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   output$Immun18<- renderPlot({
  #     ggplot(Immun_Updated18, aes(fill= g_immunization_updated, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  #       geom_rect(color='Black') +
  #       coord_polar(theta="y") +
  #       xlim(c(1, 4)) +
  #       scale_fill_brewer("Immunization Upto_date") + blank_theme +
  #       theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("") +
  #       theme(panel.grid=element_blank()) +
  #       theme(axis.text=element_blank()) +
  #       theme(axis.ticks=element_blank()) +
  #       theme(legend.title = element_text(size=10, face="bold")) +
  #       theme(legend.text = element_text(size = 10, face = "bold")) +
  #       geom_label(aes(label=paste(round(value*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)
  #     
  #   })
  #   
  #   
}