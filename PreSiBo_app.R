library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DBI)
library(pool)
library(dplyr)
library(DT)
library(RMySQL)

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
          tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle;} .form-group { display: table-row;}"),
          tags$style(HTML(".selectize-input { width: 135%; }"))
        ),
        tabItems(
          # PreSiBO Home content
          tabItem(tabName = "home",
                    br(),
                    h1(strong("PreSiBO")),
                    p("AI/ML enabled precision"), 
                    p("medicine tool using"), 
                    p(strong("P"),"redictor,", strong("S"),"ignature,"), 
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
                  h4("Relationship between PreSiBO Features", align = "center"),
                  align = "center"
          ),
          
          # Target Search content
          tabItem(tabName = "target",
                  ui <- fluidPage(
                    textInput(inputId = "targetSearch", 
                              label = "Gene name or chr:bp", 
                              value = "APOE"),
                    actionButton(inputId = "targetSearch", "Search"),
                    br(),br(),
                    tabsetPanel(
                      tabPanel("Overview",
                                ui <- fluidRow(
                                  column(3,
                                           br(),
                                           strong(p("Predictor-Outcome", align = "left", style = "color:green; font-size:15px")),
                                           h6("Outcome: ", tags$a("Clinical Diagnosis", href="#"), align = "left"),
                                           h6("1. ", tags$a("100 SNPs", href="#"), " with P<0.05", align = "left"),
                                           h6("2. Best SNP: rs123 with P=5.2x10^(-200)", align = "left"),
                                           h6("Outcome: ", tags$a("Tangle", align = "left"),
                                           h6("1. ", tags$a("100 SNPs", href="#"), " with P<0.05", href="#"), align = "left"),
                                           h6("2. Best SNP: rs123 with P=0.004", align = "left"),
                                           h6("Outcome: ", tags$a("Plague", align = "left"),
                                           h6("1. ", tags$a("100 SNPs", href="#"), " with P<0.05", href="#"), align = "left"),
                                           h6("2. Best SNP: rs123 with P=0.004", align = "left"),
                                           br(),
                                           strong(p("Predictor-Biomarker", align = "left", style = "color:green; font-size:15px")),
                                           h6("Biomarker: ", tags$a("CSF.Abeta", href="#"), align = "left"),
                                           h6("1. ", tags$a("45 SNPs", href="#"), " with P<0.05", align = "left"),
                                           h6("2. Best SNP: rs123 with P=5.2x10^(-8)", align = "left"),
                                           h6("Biomarker: ", tags$a("CSF.pTau", href="#"), align = "left"),
                                           h6("1. ", tags$a("30 SNPs", href="#"), " with P<0.05", align = "left"),
                                           h6("2. Best SNP: rs123 with P=5.2x10^(-4)", align = "left"),
                                           h6("Biomarker: ", tags$a("PET", href="#"), align = "left"),
                                           h6("1. ", tags$a("25 SNPs", href="#"), " with P<0.05", align = "left"),
                                           h6("2. Best SNP: rs123 with P=0.003", align = "left"),
                                           h6("Biomarker: ", tags$a("MRI.HPV", href="#"), align = "left"),
                                           h6("1. ", tags$a("56 SNPs", href="#"), " with P<0.05", align = "left"),
                                           h6("2. Best SNP: rs123 with P=0.002", align = "left"),
                                           br(),
                                           strong(p("Predictor-Signature", align = "left", style = "color:green; font-size:15px")),
                                           h6("Signature: ", tags$a("eQTL", href="#"), align = "left"),
                                           h6("1. ", tags$a("20 SNPs", href="#"), " with P<0.05", align = "left"),
                                           h6("2. Best SNP: rs123 with P=5.2x10^(-6)", align = "left"),
                                           h6("Signature: ", tags$a("mQTL", href="#"), align = "left"),
                                           h6("1. ", tags$a("50 SNPs", href="#"), " with P<0.05", align = "left"),
                                           h6("2. Best SNP: rs123 with P=5.2x10^(-6)", align = "left")
                                         ),
                                  column(3,
                                           br(),
                                           img(src = "preout.png", width = 200, height = 150, align = "center"),
                                           img(src = "prebio.png", width = 200, height = 150, align = "center"),
                                           img(src = "presig.png", width = 200, height = 150, align = "center")
                                         ),
                                  column(5, 
                                           br(),
                                           img(src = "predchart.png", width = 470, height = 470, align = "center")
                                         )
                                )
                               ),
                      tabPanel("Predictor",
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
                                           selected = "Outcome-Clinical Diagnosis"),
                               numericInput(inputId = "numInp", label = "Show", value = 10),
                               searchInput(
                                 inputId = "predSearch", label = "Filter: P <",
                                 placeholder = "0.05",
                                 width = "450px"
                               ),
                               downloadButton("downloadData", "Download"),
                               tableOutput("tbl")
                      ),
                      tabPanel("Signature",
                                 ui <- fluidPage(
                                     br(),
                                     textInput(inputId = "sigSearch", 
                                               label = "Gene name", 
                                               value = "APOE"),
                                     actionButton(inputId = "sigSearch", "Search"),
                                     br(), br(),
                                     numericInput(inputId = "in_obs",
                                                  label = "Show",
                                                  value = 10),
                                     searchInput(
                                       inputId = "download", label = "Filter: P <",
                                       placeholder = "0.05",
                                       width = "450px"
                                     ),
                                     downloadButton("downloadData", "Download"),
                                     br(),
                                     tableOutput("view"),
                                     br()
                                 )
                      )
                    )
                  )
          ),
          
          # Network Search content
          tabItem(tabName = "network",
                  textInput(inputId = "netSearch", 
                            label = "Gene name", 
                            value = "APOE"),
                  actionButton(inputId = "netSearch", "Search"),
                  br(),br(),
                  tabsetPanel(
                    tabPanel("Overview",
                               ui <- fluidRow(
                                 column(6,
                                          br(),
                                          strong(p("Tissue-Level Transcriptome Network using Bulk RNA-Seq Data", align = "left", style = "color:orange; font-size:15px")),
                                          strong(p("Brain-Brain Preserved Networks in AD Cases", align = "left", style = "font-size:15px")),
                                          h6("1. Identified ", span("83", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                          h6("2. Preserved ", span("29", style = "color:orange;") ," of 83 modules in validation brains", align = "left"),
                                          h6("3. Enriched ", span("14", style = "color:orange;") ," of 29 modules for AD related outcomes", align = "left"),
                                          h6("4. Generated ", span("14", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-5) for tangles and plaques", align = "left"),
                                          strong(p("Brain-Blood Preserved Networks in AD Cases", align = "left", style = "font-size:15px")),
                                          h6("1. Identified ", span("15", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                          h6("2. Preserved ", span("4", style = "color:orange;") ," of 15 modules in validation brains", align = "left"),
                                          h6("3. Enriched ", span("4", style = "color:orange;") ," of 4 modules for AD related outcomes", align = "left"),
                                          h6("4. Generated ", span("4", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-5) for AD risk", align = "left"),
                                          h6("5. Significantly correlated ", span("2", style = "color:orange;") ," of 4 modules with cognitively defined subgroups", align = "left"),
                                          h6("6. Significantly correlated ", span("3", style = "color:orange;") ," of 4 modules with MRI defined subgroups", align = "left"),
                                          br(),
                                          strong(p("Cell-Level Transcriptome Network using Single Nuclei RNA-Seq Data", align = "left", style = "color:orange; font-size:15px")),
                                          strong(p("Cell-Cell Preserved Networks in AD Cases and Controls", align = "left", style = "font-size:15px")),
                                          h6("1. Identified ", span("60", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                          h6("2. Preserved ", span("16", style = "color:orange;") ," of 60 modules in validation brains", align = "left"),
                                          h6("3. Enriched ", span("12", style = "color:orange;") ," of 16 modules for AD related outcomes", align = "left"),
                                          h6("4. Generated ", span("12", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-8) for AD risk", align = "left"),
                                          h6("5. Significantly correlated ", span("0", style = "color:orange;") ," of 12 modules with cognitively defined subgroups", align = "left"),
                                          h6("6. Significantly correlated ", span("3", style = "color:orange;") ," of 12 modules with MRI defined subgroups", align = "left"),
                                          br(),
                                          strong(p("Tissue-Level Proteome Network", align = "left", style = "color:orange; font-size:15px")),
                                          strong(p("Brain-Brain Preserved Networks in AD Cases", align = "left", style = "font-size:15px")),
                                          h6("1. Identified ", span("15", style = "color:orange;") ," modules in discovery brains", align = "left"),
                                          h6("2. Preserved ", span("4", style = "color:orange;") ," of 15 modules in validation brains", align = "left"),
                                          h6("3. Enriched ", span("4", style = "color:orange;") ," of 4 modules for AD related outcomes", align = "left"),
                                          h6("4. Generated ", span("12", style = "color:orange;") ," module based polygenic risk scores (mbPRSs) with GWAS SNPs<10^(-8) for AD risk", align = "left"),
                                          h6("5. Significantly correlated ", span("0", style = "color:orange;") ," of 12 modules with cognitively defined subgroups", align = "left"),
                                          h6("6. Significantly correlated ", span("3", style = "color:orange;") ," of 12 modules with MRI defined subgroups", align = "left"),
                                          br()
                                      ),
                                 column(3,
                                          br(),
                                          img(src = "analysisflow.png", width = 430, height = 800, align = "center")
                                        )
                               )
                             ),
                    tabPanel("Signature Guided Networks",
                               br(),
                               numericInput(inputId = "in_obs",
                                            label = "Show",
                                            value = 10),
                               searchInput(
                                 inputId = "download", label = "Filter: P <",
                                 placeholder = "0.05",
                                 width = "450px"
                               ),
                               downloadButton("downloadData", "Download"),
                               br(),
                               tableOutput("viewtbl0"),
                               br()
                             ),
                    tabPanel("Network Guided Genetic Profiles",
                               ui <- fluidPage(
                                br(),
                                selectInput(inputId = "source1", width = 385,
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
                                ),
                                br(),
                                selectInput(inputId = "source2", width = 350,
                                            label = "Selection of SNPs for Polygenic Risk Scores (PRSs):",
                                            choices = c("P < 0.05",
                                                        "P < 0.001",
                                                        "P < 5E-08"),
                                            selected = "P < 0.05"
                                ),
                                br(),
                                numericInput(inputId = "in_obs",
                                             label = "Show",
                                             value = 10),
                                searchInput(
                                  inputId = "download", label = "Filter: P <",
                                  placeholder = "0.05",
                                  width = "450px"
                                ),
                                downloadButton("downloadData", "Download"),
                                br(),
                                tableOutput("viewtbl"),
                                br()
                               )
                            ),
                    tabPanel("Network Guided Drugs",
                             ui <- fluidPage(
                               br(),
                               selectInput("source", width = 385,
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
                               ),
                               br(),
                               numericInput(inputId = "in_obs",
                                            label = "Show",
                                            value = 10),
                               searchInput(
                                 inputId = "download", label = "Filter: P <",
                                 placeholder = "0.05",
                                 width = "450px"
                               ),
                               downloadButton("downloadData", "Download"),
                               br(),
                               tableOutput("viewtable1"),
                               br()
                             )
                        )
                  )
          ),
          
          # Drug Search content
          tabItem(tabName = "drug",
                  textInput(inputId = "drugSearch", 
                            label = "Gene name", 
                            value = "APOE"),
                  actionButton(inputId = "search", "Search"),
                  br(),br(),
                  tabsetPanel(
                    tabPanel("Indication Based Search",
                                 br(),
                                 textInput(inputId = "drugIDSearch", 
                                           label = "Indication: ", 
                                           value = "Alzheimer’s disease"),
                                 actionButton(inputId = "search", "Search"),
                                 br(),br(),
                                 numericInput(inputId = "in_obs",
                                              label = "Show",
                                              value = 10),
                                 searchInput(
                                   inputId = "drugDownload", label = "Filter: P <",
                                   placeholder = "0.05",
                                   width = "450px"
                                 ),
                                 downloadButton("downloadData", "Download"),
                                 br(),
                                 tableOutput("viewtable2"),
                                 br()
                             ),
                    tabPanel("MOA Based Search",
                                 br(),
                                 textInput(inputId = "MOAsearch", 
                                           label = "Mechanism of Action (MOA): ", 
                                           value = "Inhibitor"),
                                 actionButton(inputId = "search", "Search"),
                                 br(),br(),
                                 numericInput(inputId = "in_obs",
                                              label = "Show",
                                              value = 10),
                                 searchInput(
                                   inputId = "MOAdownload", label = "Filter: P <",
                                   placeholder = "0.05",
                                   width = "450px"
                                 ),
                                 downloadButton("downloadData", "Download"),
                                 br(),
                                 tableOutput("viewtable3"),
                                 br()
                             )
                  )
          ),
          
          # AI/ML Search content
          tabItem(tabName = "aiml"
          ) 
        )
      )
)

server <- function(input, output, session) {
  output$view <- renderTable({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    query <- paste0("SELECT 
                    	go.snp_id, 
                      go.a1,
                      go.a2,
                      go.freq1,
                      go.beta,
                      go.se,
                      go.p
                     FROM presibo.gwasvariantreferencetable gv 
                     INNER JOIN presibo.gwasoutcome go ON gv.snp_id = go.snp_id 
                     WHERE gv.gene_id = '", input$sigSearch, "';")
    dbGetQuery(conn, query)
  })
}

shinyApp(ui, server)