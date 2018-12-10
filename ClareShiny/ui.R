ui<-dashboardPage(
  dashboardHeader(title = "FP_ANC_PNC"),
  dashboardSidebar(
    sidebarMenu( 
      menuItem("Family Planning",tabName = "Family_Planning",icon = icon("bar-chart-o")),
      menuItem("Antenatal Care",tabName = "ANC_Care",icon = icon("bar-chart-o")),
      menuItem("Postnatal Care",tabName = "PNC_Care",icon = icon("bar-chart-o"))
      # menuItem("Immunization",tabName = "Immun",icon = icon("bar-chart-o"))
      # 
    )),
  dashboardBody(
    tabItems(
      tabItem(
        dateRangeInput("daterange", "Choose Date Range",
                       start = min(Raw_Data$reported),
                       end = max(Raw_Data$reported),
                       min = min(Raw_Data$reported),
                       max = max(Raw_Data$reported),
                       separator = " - ", format = "dd/mm/yy"),
        tabName = "Family_Planning",
        h1("FP"),
        fluidRow(
          (valueBoxOutput("Eligible_Women")),
          (valueBoxOutput("FP_Clients")),
          (valueBoxOutput("Not_Using_Modern_FP")),
          (valueBoxOutput("NeverUsed_Modern_FP")),
          (valueBoxOutput("Unmet_needFP1")),
          (valueBoxOutput("Unmet_needFP2")),
          (valueBoxOutput("Unmet_needFP")),
          # (valueBoxOutput("Referrals")),
          #(valueBoxOutput("Short_Term_Users"))
          
          # box(title="Pregnancy status of all eligible women (n=2527)",
          #     (plotOutput("Preg",height = 300,width = 500))),
          # box(title="Eligible women Using/Not Using Modern FP (n=2108)",
          #     (plotOutput("Modern_FP",height = 300,width = 500))),
          # box(title="Take/Switch Modern FP (n=711)-remaining 36 would either like to get pregnant or have reached menopause",
          #     (plotOutput("Take_Switch",height = 300,width = 500))),
          box(title="Method in use at time of registation & Prevalence",
              (plotOutput("Methods",height = 300,width = 500))),
          box(title="New method administered & Prevalence for women who would like to switch/take up moderm FP",
              (plotOutput("Methods2",height = 300,width = 500))),
          # box(title="Reasons for not taking/switching to Modern FP",
          #     (plotOutput("Reasons",height = 300,width = 500))),
          box(title="Presence of Side Effects-Women using short-term contraceptives (n=228)",
              (plotOutput("Side_Effects_Presence",height = 300,width = 500))),
          box(title="Side Effects-Women using short-term contraceptives (n=26)",
              (plotOutput("Side_Effects",height = 300,width = 500))),
          box(title="Take/Switch Modern FP-Prospective Users (n=1266)",
              (plotOutput("Prospective_Take",height = 300,width = 500))),
          box(title="New FP method taken by Prospective Users",
              (plotOutput("New_FP",height = 300,width = 500)))
        )),
      
      # tabItem(tabName = "Immun",
      #         h1("Immunization by age cohort as at Sept 2018"),
      #         fluidRow(
      #           (valueBoxOutput("Newborns")),
      #           (valueBoxOutput("FirstPNC_Visit")),
      #           (valueBoxOutput("Newborns_U1Week")),
      #           (valueBoxOutput("TMonths")),
      #           (valueBoxOutput("SMonths")),
      #           (valueBoxOutput("NMonths")),
      #           (valueBoxOutput("EMonths")),
      #           box(title="Delivery outcome (n=867)",
      #               (plotOutput("Delivery",height = 300,width = 500))),
      #           box(title="HF Delivery (n=781)",
      #               (plotOutput("HF",height = 300,width = 500))),
      #           box(title="HF ANC visits attended",
      #               (plotOutput("ANC_Visits",height = 300,width = 500))),
      #           box(title="Newborn breast-fed after delivery (n=795 live births)",
      #               (plotOutput("Breast_fed",height = 300,width = 500))),
      #           box(title="Newborns <1 week who have/haven't received a vaccination",
      #               (plotOutput("Vacc1",height = 300,width = 500))),
      #           box(title="Newborns <1 week whose Immunization status is/isn't Upto_date",
      #               (plotOutput("Immun1",height = 300,width = 500))),
      #           box(title="3 Months olds who have/haven't received a vaccination",
      #               (plotOutput("Vacc3",height = 300,width = 500))),
      #           box(title="3 Months olds whose Immunization status is/isn't Upto_date",
      #               (plotOutput("Immun3",height = 300,width = 500))),
      #           box(title="6 Months olds who have/haven't received a vaccination",
      #               (plotOutput("Vacc6",height = 300,width = 500))),
      #           box(title="6 Months olds whose Immunization status is/isn't Upto_date",
      #               (plotOutput("Immun6",height = 300,width = 500))),
      #           box(title="9 Months olds who have/haven't received a vaccination",
      #               (plotOutput("Vacc9",height = 300,width = 500))),
      #           box(title="9 Months olds whose Immunization status is/isn't Upto_date",
      #               (plotOutput("Immun9",height = 300,width = 500))),
      #           box(title="18 Months olds who have/haven't received a vaccination",
      #               (plotOutput("Vacc18",height = 300,width = 500))),
      #           box(title="18 Months olds whose Immunization status is/isn't Upto_date",
      #               (plotOutput("Immun18",height = 300,width = 500)))
      #         )
      # ),
      
      tabItem(  
        dateRangeInput("daterange1", "Choose Date Range",
                       start = min(Preg_Reg$reported1),
                       end = max(Preg_Reg$reported1),
                       min = min(Preg_Reg$reported1),
                       max = max(Preg_Reg$reported1),
                       separator = " - ", format = "dd/mm/yy"),
        tabName = "ANC_Care",
        h1("ANC"),
        fluidRow(
          (valueBoxOutput("Expectant_Women")),
          box(title="Expectant women using LLIN",
              (plotOutput("llin",height = 300,width = 500))),
          box(title="Expectant women who have/haven't gone for facility ANC",
              (plotOutput("ANC",height = 300,width = 500))),
          box(title="Expectant women who have/haven't received TT Immunization",
              (plotOutput("TT",height = 300,width = 500)))
        )),
      
      
      
      
      tabItem(tabName = "PNC_Care",
              h1("PNC"),
              fluidRow(
                (valueBoxOutput("Newborns")),
                #(valueBoxOutput("FirstPNC_Visit")),
                box(title="Delivery outcome (n=1167)",
                    (plotOutput("Delivery",height = 300,width = 500))),
                box(title="HF Delivery (n=1051)",
                    (plotOutput("HF",height = 300,width = 500))),
                box(title="HF ANC visits attended",
                    (plotOutput("ANC_Visits",height = 300,width = 500))),
                box(title="Newborn breast-fed after delivery (n=985 live births)",
                    (plotOutput("Breast_fed",height = 300,width = 500)))
              )
      )
      
    )
  )
)
