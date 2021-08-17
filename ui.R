## ui.R ##
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(pool)
library(dplyr)
library(DT)
library(RMySQL)
library(writexl)
library(formattable)
library(pracma)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PreSiBO Home", tabName = "home"),
      menuItem("About PreSiBO", tabName = "about"),
      menuItem("Target Search", tabName = "target"),
      menuItem("Network Search", tabName = "network"),
      menuItem("Drug Search", tabName = "drug"),
      menuItem("AI/ML Search", tabName = "aiml")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(type="text/css", "label{ display: table-cell; text-align: right; vertical-align: middle;} .form-group { display: table-row;}"),
      tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle;} .form-group { display: table-row;}")
    ),
    tags$head(tags$style("#tbl_selected{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#sgn_output1{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#sgn_output2{color: orange; font-size: 17px;}")),
    tags$head(tags$style("#ngd_output2{color: orange; font-size: 17px;}")),
    tabItems(
      # PreSiBO Home content
      tabItem(tabName = "home",
              br(),
              h1(strong("PreSiBO")),
              p("AI/ML enabled precision"), 
              p("medicine tool using"), 
              p(strong("Pre"),"dictor,", strong("Si"),"gnature,"), 
              p(strong("B"),"iomarker, and", strong("O"),"utcome"), 
              p("profiles for Alzheimer Disease")
      ),
      
      # About PreSiBO content
      tabItem(tabName = "about",
              br(), 
              strong(p("About PresiBo", align = "left", style = "color:green; font-size:25px")),
              br(),
              h4("The PreSiBO is a database system on Amazon cloud for precision medicine in Alzheimer’s disease by profiling predictor, signature, biomarker, and outcome (PreSiBO) features in genome guided patient subgroups and targeted drug discovery/repurposing for Alzheimer’s disease. The PreSiBO implements a hierarchical model of target, network, and drug tables with the PreSiBO features for fast evaluation of targets and existing drugs. It uses Amazon relational database management system (RDBMS) for database and storage and an Amazon Web Services (AWS) tool for big data processing and analysis. The PreSiBO features are profiled as component (tables) in a hierarchical model of network, target, variant, and drug layers. These features is integrated into the internal (hidden) layer for AI/ML applications for patient classification, target prioritization, and drug repurposing in AD. The current PreSiBO version equips with query and retrieval capability for target, network, and drug", align = "left"),
              br(), br(),
              img(src = "overall.png", width = 600, height = 300, align = "center"),
              h4("Overall flow of PreSiBO", align = "center"),
              br(),br(),
              br(),br(), align = "center",
              strong(p("Features", align = "left", style = "color:green; font-size:25px")),
              h4("1. Predictor (P): Variant (Var); Gene (Gen); Polygenic risk score (Prs)", align = "left"),
              h4("2. Signature: Blood or Brain - Tissue-Level or Cell-Level Transcriptome; Methylome; Proteome", align = "left"),		
              h4("3. Biomarker (B): Fluid or Imaging", align = "left"),
              h4("4. Fluid: CSF; Plasma", align = "left"),
              h4("5. Imaging: MRI; PET", align = "left"),
              h4("6. Outcome (O): Clinical (Cli) or Neuropathological (Neu)", align = "left"),
              h4("7. Clinical (Cli): Diagnosis (Dx); Cognitive Functions", align = "left"),
              h4("8. Neuropathological (Neu): Diagnosis (Dx); Tangles; Plaque", align = "left"),
              br(), br(),
              strong(p("Data Source", align = "left", style = "color:green; font-size:25px")),
              strong(p("Predictor-Outcome", align = "left", style = "font-size:18px")),
              h4("1. P.Var-O.Cli: Kunkle et al. Nat Genet. 2019; 51:414–30. PMID: 30820047.", align = "left"),
              h4("2. P.Var-O.Neu: Beecham et al. PLoS Genet. 2014;10:e1004606. PMID: 25188341. ", align = "left"),		
              h4("3. Predictor-Biomarker", align = "left"),
              h4("4. P.Var-B.Fluid.CSF.Ab42/pTau/tTau: Deming et al.", align = "left"),
              h4("5. P.Var-B.Imaging.MRI: Provided by Kwangsik Nho (Indiana U)", align = "left"),
              h4("6. P.Var-B.Imaging.PET: Provided by Kwangsik Nho (Indiana U)", align = "left"),
              h4("7. Predictor-Signature", align = "left"),
              h4("8. P.Var-S.Brain.Tissue.Transcriptome: Ng et al. Nat Neurosci. 2017;20:1418. PMID: 28869584", align = "left"),
              br(),
              strong(p("Signature-Outcome", align = "left", style = "font-size:18px")),
              h4("1. S.Brain.Tissue.Transcriptome-O.Neu.Dx: Panitch et al. Mol Psychiat. 2021. In press.", align = "left"),
              h4("2. S.Brain.Cell.Transcriptome-O.Neu.Dx: Panitch et al. Mol Psychiat. 2021. In press.", align = "left"),
              h4("3. S.Brain.Tissue.Transcriptome-O.Neu.Tangle: Panitch et al. Mol Psychiat. 2021. In press.", align = "left"),
              h4("4. S.Brain.Tissue.Transcriptome-O.Neu.Plaque: Panitch et al. Mol Psychiat. 2021. In press.", align = "left"),
              h4("5. S.Brain.Tissue.Transcriptome-O.Neu.pTau181: Panitch et al. Mol Psychiat. 2021. In press.", align = "left"),
              h4("6. S.Brain.Tissue.Transcriptome-O.Neu.pTau231: Panitch et al. Mol Psychiat. 2021. In press.", align = "left"),
              br(),
              strong(p("Network", align = "left", style = "font-size:18px")),
              h4("1. S.Brain.Tissue.Transcriptome: Chung et al", align = "left"),
              h4("2. S.Brain.Cell.Transcriptome: Sahelijo et al.", align = "left"),
              h4("3. S.Blood.Tissue.Transcriptome: Hu et al.", align = "left"),
              h4("4. S.Brain.Proteome: Hu et al.", align = "left"),
              br(), br(),
              br(), br(),
              img(src = "relationship.png", width = 600, height = 300, align = "center"),
              h4("Relationship between PreSiBO Features", align = "center")
      ),
      
      # Target Search content
      tabItem(tabName = "target",
              ui <- fluidPage(
                searchInput(inputId = "targetSearch", 
                            label = "Gene name or chr:bp", 
                            placeholder = "APOE or 19:45000-55000",
                            btnSearch = "Search",
                            width = "400px"),
                br(),br(),
                tabsetPanel(
                  # Target Overview
                  tabPanel("Overview",
                           ui <- fluidRow(
                             column(3,
                                    br(),
                                    strong(p("Predictor-Outcome", align = "left", style = "color:green; font-size:15px")),
                                    h5("Outcome: ", tags$a("Clinical Diagnosis", href="#"), align = "left"),
                                    h5("1. ", tags$a("100 SNPs", href="#"), " with P<0.05", align = "left"),
                                    h5("2. Best SNP: rs123 with P=5.2x10^(-200)", align = "left"),
                                    h5("Outcome: ", tags$a("Tangle", align = "left"),
                                       h5("1. ", tags$a("100 SNPs", href="#"), " with P<0.05", href="#"), align = "left"),
                                    h5("2. Best SNP: rs123 with P=0.004", align = "left"),
                                    h5("Outcome: ", tags$a("Plague", align = "left"),
                                       h5("1. ", tags$a("100 SNPs", href="#"), " with P<0.05", href="#"), align = "left"),
                                    h5("2. Best SNP: rs123 with P=0.004", align = "left"),
                                    br(),
                                    strong(p("Predictor-Biomarker", align = "left", style = "color:green; font-size:15px")),
                                    h5("Biomarker: ", tags$a("CSF.Abeta", href="#"), align = "left"),
                                    h5("1. ", tags$a("45 SNPs", href="#"), " with P<0.05", align = "left"),
                                    h5("2. Best SNP: rs123 with P=5.2x10^(-8)", align = "left"),
                                    h5("Biomarker: ", tags$a("CSF.pTau", href="#"), align = "left"),
                                    h5("1. ", tags$a("30 SNPs", href="#"), " with P<0.05", align = "left"),
                                    h5("2. Best SNP: rs123 with P=5.2x10^(-4)", align = "left"),
                                    h5("Biomarker: ", tags$a("PET", href="#"), align = "left"),
                                    h5("1. ", tags$a("25 SNPs", href="#"), " with P<0.05", align = "left"),
                                    h5("2. Best SNP: rs123 with P=0.003", align = "left"),
                                    h5("Biomarker: ", tags$a("MRI.HPV", href="#"), align = "left"),
                                    h5("1. ", tags$a("56 SNPs", href="#"), " with P<0.05", align = "left"),
                                    h5("2. Best SNP: rs123 with P=0.002", align = "left"),
                                    br(),
                                    strong(p("Predictor-Signature", align = "left", style = "color:green; font-size:15px")),
                                    h5("Signature: ", tags$a("eQTL", href="#"), align = "left"),
                                    h5("1. ", tags$a("20 SNPs", href="#"), " with P<0.05", align = "left"),
                                    h5("2. Best SNP: rs123 with P=5.2x10^(-6)", align = "left"),
                                    h5("Signature: ", tags$a("mQTL", href="#"), align = "left"),
                                    h5("1. ", tags$a("50 SNPs", href="#"), " with P<0.05", align = "left"),
                                    h5("2. Best SNP: rs123 with P=5.2x10^(-6)", align = "left")
                             ),
                             column(3,
                                    br(),
                                    img(src = "preout.png", width = 200, height = 150, align = "center"),
                                    img(src = "prebio.png", width = 200, height = 150, align = "center"),
                                    img(src = "presig.png", width = 200, height = 150, align = "center")
                             ),
                             column(5, 
                                    br(),
                                    h2("All Regional Plots"),
                                    br()
                                    #img(src = "predchart.png", width = 470, height = 470, align = "center")
                             )
                           )
                  ),
                  # Target Predictor
                  tabPanel("Predictor",
                           ui <- fluidPage(
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               selectInput("var",
                                           label = "SBO Selection:",
                                           choices = c("Outcome-Clinical Diagnosis", 
                                                       "Outcome-Tangle",
                                                       "Outcome-Plague", 
                                                       "Biomarker-CSF.Abeta", 
                                                       "Biomarker-CSF.pTau", 
                                                       "Biomarker-PET", 
                                                       "Biomarker-MRI.HPV", 
                                                       "Signature-eQTL", 
                                                       "Signature-mQTL"),
                                           selected = "Outcome-Clinical Diagnosis")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "predFilter", label = "Filter: P < ",
                                 placeholder = "0.05",
                                 width = "150px"
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               numericInput(inputId = "predShow",
                                            label = "Show",
                                            value = 10)
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "predSearch", label = "Search ",
                                 placeholder = "ex) rs78286437",
                                 width = "150px"
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               downloadButton("downloadPred", "Download Table")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                             div(
                               style="overflow-x: auto;",
                               br(),
                               br(),
                               textOutput("tbl_selected"),
                               br(),
                               dataTableOutput("tbl"),
                               br()
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 90px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               img(src = "regionalplot.png", width = 250, height = 250, align = "center")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 90px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               img(src = "heatmapplot.png", width = 250, height = 250, align = "center")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 90px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               img(src = "snpplot.png", width = 250, height = 250, align = "center")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 155px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               actionButton("regionalplot", "Download Plot")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 250px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               actionButton("heatmapplot", "Download Plot")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 220px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               actionButton("snpplot", "Download Plot")
                             )
                           )
                  ),
                  tabPanel("Signature",
                           ui <- fluidPage(
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               p("Analysis: ", strong("AD vs. Control", style = "color:green; font-size:17px"), style = "font-size:17px")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "downSig", label = "Filter: P < ",
                                 placeholder = "0.05",
                                 width = "450px"
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               numericInput(inputId = "sigShow",
                                            label = "Show",
                                            value = 10)
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "sigSearch", label = "Search ",
                                 placeholder = "ex) Brain, Tissue",
                                 width = "150px"
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               downloadButton("downloadSig", "Download Table")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                             div(
                               style="overflow-x: auto;",
                               br(),
                               dataTableOutput("view"),
                               br(), br(), br(), br()
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 0px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               p("Analysis: ", strong("QTL", style = "color:green; font-size:17px"), style = "font-size:17px")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 120px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "downSig2", label = "Filter: P < ",
                                 placeholder = "0.05",
                                 width = "450px",
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               numericInput(inputId = "sigShow2",
                                            label = "Show",
                                            value = 10)
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               searchInput(
                                 inputId = "sigSearch2", label = "Search ",
                                 placeholder = "ex) Brain, Tissue",
                                 width = "150px"
                               )
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 40px;",HTML("<br>")),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               downloadButton("downloadSig2", "Download Table")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
                             div(
                               style="overflow-x: auto;",
                               br(),
                               dataTableOutput("view2"),
                               br()
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 50px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               img(src = "genesplot.png", width = 500, height = 250, align = "center")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 50px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               br(),
                               img(src = "apoeplot.png", width = 300, height = 250, align = "center")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 260px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               actionButton("genesplot", "Download Plot")
                             ),
                             div(style="display: inline-block;vertical-align:top; width: 330px;"),
                             div(
                               style="display: inline-block;vertical-align:top;",
                               actionButton("apoeplot", "Download Plot")
                             )
                           )
                  )
                )
              )
      ),
      
      # Network Search content
      tabItem(tabName = "network",
              searchInput(inputId = "netSearch", 
                          label = "Gene name", 
                          placeholder = "APOE",
                          btnSearch = "Search"),
              br(),br(),
              tabsetPanel(
                tabPanel("Overview",
                         ui <- fluidRow(
                           column(6,
                                  br(),
                                  strong(p("Tissue-Level Transcriptome Network using Bulk RNA-Seq Data", align = "left", style = "color:orange; font-size:15px")),
                                  strong(p("Brain-Brain Preserved Networks in AD Cases", align = "left", style = "font-size:15px")),
                                  h5("1. Identified ", span("83", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                  h5("2. Preserved ", span("29", style = "color:orange;") ," of 83 modules in validation brains", align = "left"),
                                  h5("3. Enriched ", span("14", style = "color:orange;") ," of 29 modules for AD related outcomes", align = "left"),
                                  h5("4. Generated ", span("14", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-5) for tangles and plaques", align = "left"),
                                  strong(p("Brain-Blood Preserved Networks in AD Cases", align = "left", style = "font-size:15px")),
                                  h5("1. Identified ", span("15", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                  h5("2. Preserved ", span("4", style = "color:orange;") ," of 15 modules in validation brains", align = "left"),
                                  h5("3. Enriched ", span("4", style = "color:orange;") ," of 4 modules for AD related outcomes", align = "left"),
                                  h5("4. Generated ", span("4", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-5) for AD risk", align = "left"),
                                  h5("5. Significantly correlated ", span("2", style = "color:orange;") ," of 4 modules with cognitively defined subgroups", align = "left"),
                                  h5("6. Significantly correlated ", span("3", style = "color:orange;") ," of 4 modules with MRI defined subgroups", align = "left"),
                                  br(),
                                  strong(p("Cell-Level Transcriptome Network using Single Nuclei RNA-Seq Data", align = "left", style = "color:orange; font-size:15px")),
                                  strong(p("Cell-Cell Preserved Networks in AD Cases and Controls", align = "left", style = "font-size:15px")),
                                  h5("1. Identified ", span("60", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                  h5("2. Preserved ", span("16", style = "color:orange;") ," of 60 modules in validation brains", align = "left"),
                                  h5("3. Enriched ", span("12", style = "color:orange;") ," of 16 modules for AD related outcomes", align = "left"),
                                  h5("4. Generated ", span("12", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-8) for AD risk", align = "left"),
                                  h5("5. Significantly correlated ", span("0", style = "color:orange;") ," of 12 modules with cognitively defined subgroups", align = "left"),
                                  h5("6. Significantly correlated ", span("3", style = "color:orange;") ," of 12 modules with MRI defined subgroups", align = "left"),
                                  br(),
                                  strong(p("Tissue-Level Proteome Network", align = "left", style = "color:orange; font-size:15px")),
                                  strong(p("Brain-Brain Preserved Networks in AD Cases", align = "left", style = "font-size:15px")),
                                  h5("1. Identified ", span("15", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                  h5("2. Preserved ", span("4", style = "color:orange;") ," of 15 modules in validation brains", align = "left"),
                                  h5("3. Enriched ", span("4", style = "color:orange;") ," of 4 modules for AD related outcomes", align = "left"),
                                  h5("4. Generated ", span("12", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-8) for AD risk", align = "left"),
                                  h5("5. Significantly correlated ", span("0", style = "color:orange;") ," of 12 modules with cognitively defined subgroups", align = "left"),
                                  h5("6. Significantly correlated ", span("3", style = "color:orange;") ," of 12 modules with MRI defined subgroups", align = "left"),
                                  br()
                           ),
                           column(3,
                                  br(),
                                  img(src = "analysisflow.png", width = 430, height = 800, align = "center")
                           )
                         )
                ),
                tabPanel("Signature Guided Networks",
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           textOutput("sgn_output1")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           searchInput(
                             inputId = "sigNet", label = "Filter: Z-Summary < ",
                             placeholder = "0.05",
                             width = "450px",
                           )
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           numericInput(inputId = "sigNetShow",
                                        label = "Show",
                                        value = 10)
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           searchInput(
                             inputId = "sigNetSearch", label = "Search ",
                             placeholder = "ex) APOE, M17",
                             width = "150px"
                           )
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           downloadButton("downloadSigNet", "Download Table")
                         ),
                         div(
                           style="overflow-x: auto;",
                           br(),
                           dataTableOutput("viewtbl0"),
                           br(), br(), br(), br()
                         ),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           textOutput("sgn_output2")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 75px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           searchInput(
                             inputId = "sigNet2", label = "Filter: P < ",
                             placeholder = "0.05",
                             width = "450px"
                           )
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           numericInput(inputId = "sigNetShow2",
                                        label = "Show",
                                        value = 10)
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           searchInput(
                             inputId = "sigNetSearch2", label = "Search ",
                             placeholder = "ex) DPM1, NIPAL3",
                             width = "150px"
                           )
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           downloadButton("downloadSigNet2", "Download Table")
                         ),
                         div(
                           style="overflow-x: auto;",
                           br(),
                           dataTableOutput("viewtbl12_1")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           img(src = "networkplot.png", width = 200, height = 200, align = "center")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 50px;"),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           br(),
                           img(src = "heatsigplot.png", width = 200, height = 200, align = "center")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 100px;"),
                         div(),
                         div(style="display: inline-block;vertical-align:top; width: 100px;"),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           actionButton("networkplot", "Download Plot")
                         ),
                         div(style="display: inline-block;vertical-align:top; width: 140px;"),
                         div(
                           style="display: inline-block;vertical-align:top;",
                           actionButton("heatsigplot", "Download Plot")
                         )
                ),
                tabPanel("Network Guided Genetic Profiles",
                         ui <- fluidPage(
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             selectInput(inputId = "source1",
                                         label = "Source of Network:",
                                         choices = c("Brain-Brain Tissue-Level (Prefrontal Cortex) Transcriptome",
                                                     "Brain-Brain Cell-Level (Astrocyte) Transcriptome",
                                                     "Brain-Brain Cell-Level (Excitatory Neuron) Transcriptome",
                                                     "Brain-Brain Cell-Level (Inhibitory Neuron) Transcriptome",
                                                     "Brain-Brain Cell-Level (Microglia) Transcriptome",
                                                     "Brain-Brain Cell-Level (Oligodendrocyte) Transcriptome",
                                                     "Brain-Brain Cell-Level (OPC) Transcriptome",
                                                     "Brain-Blood Transcriptome",
                                                     "Brain-Brain Proteome"),
                                         selected = "Brain-Brain Tissue-Level (Prefrontal Cortex) Transcriptome"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 120px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             selectInput("source2",
                                         label = "Selection of SNPs for Polygenic Risk Scores (PRSs):",
                                         choices = c("P<0.05",
                                                     "P<0.001",
                                                     "P<5E-08"),
                                         selected = "P<0.05"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 50px;"),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             p("Brain-Blood Tissue-Level (Prefrontal Cortex) Transcriptome Networks ", style = "color:orange; font-size:17px")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 20px;"),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             searchInput(
                               inputId = "netGene", label = "Filter: Z-Summary < ",
                               placeholder = "0.05",
                               width = "450px"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             searchInput(
                               inputId = "sigGeneSearch", label = "Search ",
                               placeholder = "ex) APOE, M17",
                               width = "150px"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 20px;"),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             downloadButton("downloadNetGene", "Download Table")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 155px;",HTML("<br>")),
                           div(
                             style="overflow-x: auto; overflow-y: auto;",
                             br(),
                             dataTableOutput("viewtbl"),
                             br(), br(), br(), br()
                           ),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             #textOutput("ngs_output2")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 330px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             searchInput(
                               inputId = "netGene2", label = "Filter: P < ",
                               placeholder = "0.05",
                               width = "450px"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             searchInput(
                               inputId = "sigGeneSearch2", label = "Search ",
                               placeholder = "ex) DPM1, NIPAL3",
                               width = "150px"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 10px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             downloadButton("downloadNetGene2", "Download Table")
                           ),
                           div(
                             style="overflow-x: auto; overflow-y: auto; display: none",
                             br(),
                             dataTableOutput("viewtbl2_1"),
                             br()
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 50px;"),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             img(src = "m2prsplot.png", width = 500, height = 200, align = "center")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 100px;"),
                           div(),
                           div(style="display: inline-block;vertical-align:top; width: 250px;"),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             actionButton("m2prsplot", "Download Plot")
                           )
                         )
                ),
                tabPanel("Network Guided Drugs",
                         ui <- fluidPage(
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             selectInput("source",
                                         label = "Source of Network:",
                                         choices = c("Brain-Brain Tissue-Level (Prefrontal Cortex) Transcriptome",
                                                     "Brain-Brain Cell-Level (Astrocyte) Transcriptome",
                                                     "Brain-Brain Cell-Level (Excitatory Neuron) Transcriptome",
                                                     "Brain-Brain Cell-Level (Inhibitory Neuron) Transcriptome",
                                                     "Brain-Brain Cell-Level (Microglia) Transcriptome",
                                                     "Brain-Brain Cell-Level (Oligodendrocyte) Transcriptome",
                                                     "Brain-Brain Cell-Level (OPC) Transcriptome",
                                                     "Brain-Blood Transcriptome",
                                                     "Brain-Brain Proteome"),
                                         selected = "Brain-Brain Tissue-Level (Prefrontal Cortex) Transcriptome"
                             ) 
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 350px;"),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             p("Brain-Blood Tissue-Level (Prefrontal Cortex) Transcriptome Networks ", style = "color:orange; font-size:17px")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 70px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             searchInput(
                               inputId = "sigDrugSearch", label = "Search ",
                               placeholder = "ex) APOE, M17",
                               width = "150px"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 70px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             downloadButton("downloadNetDrug", "Download Table")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 250px;",HTML("<br>")),
                           div(
                             style="overflow-x: auto;",
                             br(),
                             dataTableOutput("viewtable1"),
                             br(), br(), br(), br()
                           ),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             textOutput("ngd_output2")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 300px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             searchInput(
                               inputId = "sigDrugSearch2", label = "Search ",
                               placeholder = "ex) actarit, diarrhea",
                               width = "150px"
                             )
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 60px;",HTML("<br>")),
                           div(
                             style="display: inline-block;vertical-align:top;",
                             br(),
                             downloadButton("downloadNetDrug2", "Download Table")
                           ),
                           div(style="display: inline-block;vertical-align:top; width: 150px;",HTML("<br>")),
                           div(
                             style="overflow-x: auto;",
                             br(),
                             dataTableOutput("viewtable12_1")
                           )
                         )
                )
              )
      ),
      
      # Drug Search content
      tabItem(tabName = "drug",
              searchInput(inputId = "drugSearch", 
                          label = "Gene name", 
                          placeholder = "APOE",
                          btnSearch = "Search"),
              br(),br(),
              div(
                style="display: inline-block;vertical-align:top;",
                br(),
                searchInput(inputId = "searchInd", 
                            label = "Search ", 
                            placeholder = "ex) actarit, diarrhea",
                            btnSearch = "Search")
              ),
              div(style="display: inline-block;vertical-align:top; width: 100px;"),
              div(
                style="display: inline-block;vertical-align:top;",
                br(),
                numericInput(inputId = "showInd", 
                            label = "Show", 
                            value = 10)
              ),
              div(style="display: inline-block;vertical-align:top; width: 100px;"),
              div(
                style="display: inline-block;vertical-align:top;",
                br(),
                downloadButton("downloadIndic", "Download Table")
              ),
              div(style="display: inline-block;vertical-align:top; width: 200px;",HTML("<br>")),
              div(
                style="overflow-x: auto;",
                br(),
                dataTableOutput("viewtable2"),
                br()
              )
          ),
      
      # AI/ML Search content
      tabItem(tabName = "aiml"
      ) 
    )
  )
)
