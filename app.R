library(shiny)
library(shinythemes)
library(shinydashboard)

ui <- 
  navbarPage("",
             tabPanel("PreSiBO Home",
                      fluidPage(
                        br(), br(),
                        h4("The PreSiBO is a database system on Amazon cloud for precision medicine in Alzheimer’s disease by profiling predictor, signature, biomarker, and outcome (PreSiBO) features in genome guided patient subgroups and targeted drug discovery/repurposing for Alzheimer’s disease. 
                           The PreSiBO implements a hierarchical model of target, network, and drug tables with the PreSiBO features for fast evaluation of targets and existing drugs. 
                           It uses Amazon relational database management system (RDBMS) for database and storage and an Amazon Web Services (AWS) tool for big data processing and analysis. 
                           The PreSiBO features contains previously published data after clean, curate, and format. The current PreSiBO version equips with query and retrieval capability for target, network, and drug. We plan to implement applications of machine learning algorithms on these searches.", align = "left"),
                        br(), br(),
                        img(src = "overall.png", width = 700, height = 400),
                        h4("Overall flow of PreSiBO"),
                        br(),br(),
                        h4("The PreSiBO features are profiled as component (tables) in a hierarchical model of network, target, variant, and drug layers. These features is integrated into the internal (hidden) layer for AI/ML applications for patient classification, target prioritization, and drug repurposing in AD.", align = "left"),
                        br(),br(),
                        img(src = "presiboml.png", width = 700, height = 400),
                        h4("Machine learning application on the PreSiBO System"),
                        br(),br(),
                        align = "center")
                      ),
             tabPanel("About PreSiBO",
                      fluidPage(
                        br(), br(),
                        img(src = "relationship.png", width = 700, height = 400),
                        h4("Relationship between PreSiBO Features"),
                        br(), br(),
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
                        align = "center")
             ),
             tabPanel("Gene Search", 
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Target Search",
                                   fluidPage(
                                     ui <- dashboardPage(
                                       dashboardHeader(),
                                       dashboardSidebar(
                                         sidebarMenu(
                                           menuItem("Overview", tabName = "overview"),
                                           menuItem("Predictor", tabName = "predictor"),
                                           menuItem("Signature", tabName = "signature")
                                         )
                                       ),
                                       dashboardBody(
                                         tabItems(
                                           # Overview tab content
                                           tabItem(tabName = "overview",
                                                   shinydashboard::sidebarSearchForm(textId = "txtSearch", buttonId = "btnSearch",
                                                                                     label = "Search ...")
                                           ),
                                           
                                           # Predictor tab content
                                           tabItem(tabName = "predictor",
                                                   tabsetPanel(
                                                     tabPanel("Predictor-Outcome"),
                                                     tabPanel("Predictor-Biomarker"),
                                                     tabPanel("Predictor-Signature")
                                                   )
                                           ),
                                           
                                           # Signature tab content
                                           tabItem(tabName = "signature",
                                                   tabsetPanel(
                                                     tabPanel("Signature-Outcome"),
                                                     tabPanel("Signature-Biomarker")
                                                   )
                                           ) 
                                         )
                                       )
                                     )
                                   )
                                   ),
                          tabPanel("Network Search"),
                          tabPanel("Drug Search")
                        )))
            )

server <- function(input, output) { }

shinyApp(ui, server)