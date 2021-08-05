## server.R ##
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

server <- function(input, output, session) {
  
  ##############Target Search##############
  dfPred <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    #######When type the gene name############
    if (strsplit(input$targetSearch, ":") == input$targetSearch) {
      
      if (input$var == "Outcome-Clinical Diagnosis" || input$var == "Outcome-Tangle" || input$var == "Outcome-Plague") {
        query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "';")
        if(input$predFilter != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND gp.`-Log10(Pval)` < '", input$predFilter, "';")
        }
        if (input$predSearch != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)`) LIKE '%", input$predSearch, "%';")
        }
      }
      
      if (input$var == "Biomarker-CSF.Abeta" || input$var == "Biomarker-CSF.pTau" || input$var == "Biomarker-PET" || input$var == "Biomarker-MRI.HPV") {
        query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "';")
        if(input$predFilter != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND gp.`-Log10(Pval)` < '", input$predFilter, "';")
        }
        if (input$predSearch != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)`) LIKE '%", input$predSearch, "%';")
        }
      }
      
      if (input$var == "Signature-eQTL" || input$var == "Signature-mQTL") {
        query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "';")
        if(input$predFilter != '') {
          query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND gp.`-Log10(Pval)` < '", input$predFilter, "';")
        }
        if (input$predSearch != '') {
          query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)`) LIKE '%", input$predSearch, "%';")
        }
      }
    }
    
    #######When type the only one positon############
    else  {
      split_list_var = strsplit(input$targetSearch, ":")
      print(split_list_var[[1]][1])
      print(split_list_var[[1]][2])
      
      #######When type the range of positons############
      if (strsplit(split_list_var[[1]][2], "-") != split_list_var[[1]][2]) {
        new_split_list_var = strsplit(split_list_var[[1]][2], "-")
        
        print(new_split_list_var[[1]][1])
        print(new_split_list_var[[1]][2])
        
        new_var1 = paste0(split_list_var[[1]][1], "-", new_split_list_var[[1]][1])
        new_var2 = paste0(split_list_var[[1]][1], "-", new_split_list_var[[1]][2])
        print(new_var1)
        print(new_var2)
        
        if (input$var == "Outcome-Clinical Diagnosis" || input$var == "Outcome-Tangle" || input$var == "Outcome-Plague") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.`-Log10(Pval)` < '", input$predFilter, "' AND gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)`) LIKE '%", input$predSearch, "%';")
          }
        }
        
        if (input$var == "Biomarker-CSF.Abeta" || input$var == "Biomarker-CSF.pTau" || input$var == "Biomarker-PET" || input$var == "Biomarker-MRI.HPV") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.`-Log10(Pval)` < '", input$predFilter, "' AND gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)`) LIKE '%", input$predSearch, "%';")
          }
        }
        
        if (input$var == "Signature-eQTL" || input$var == "Signature-mQTL") {
          query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.`-Log10(Pval)` < '", input$predFilter, "' AND gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)`) LIKE '%", input$predSearch, "%';")
          }
        } 
      }
      
      #######When type the only one positon############
      else {
        new_var1 = paste0(split_list_var[[1]][1], "-", split_list_var[[1]][2])
        print(new_var1)
        if (input$var == "Outcome-Clinical Diagnosis" || input$var == "Outcome-Tangle" || input$var == "Outcome-Plague") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` = '", new_var1, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.`-Log10(Pval)` < '", input$predFilter, "' AND gv.`#variant_id` = '", new_var1, "';")
          }
        }
        
        if (input$var == "Biomarker-CSF.Abeta" || input$var == "Biomarker-CSF.pTau" || input$var == "Biomarker-PET" || input$var == "Biomarker-MRI.HPV") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` = '", new_var1, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.`-Log10(Pval)` < '", input$predFilter, "' AND gv.`#variant_id` = '", new_var1, "';")
          }
        }
        
        if (input$var == "Signature-eQTL" || input$var == "Signature-mQTL") {
          query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` = '", new_var1, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.feature_name, gp.feature_ch, gp.feature_start, gp.spearman_rho, gp.ensgid, gp.`-Log10(Pval)` 
                         FROM presibo.gwasvariantreferencetable gv
                         INNER JOIN presibo.`hwas-pred-signature` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.`-Log10(Pval)` < '", input$predFilter, "' AND gv.`#variant_id` = '", new_var1, "';")
          }
        } 
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$tbl <- renderDataTable({
    datatable(head(dfPred(), n = input$predShow), options = list(dom = 't'))
  })
  
  output$downloadPred <- downloadHandler(
    filename = function() { paste("Predictor", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfPred(), path = file)}
  )
  
  ##############Signature Search##############
  dfSig <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if(input$downSig != '')
    {
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `-Log10(Pval)` < '", input$downSig, "';")
      if (input$sigSearch != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `-Log10(Pval)` < '", input$downSig, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)`) LIKE '%", input$sigSearch, "%';")
      }
    }
    else
    {
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "';")
      if (input$sigSearch != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)`) LIKE '%", input$sigSearch, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$view <- renderDataTable({
    datatable(head(dfSig(), n = input$sigShow), options = list(dom = 't'))
  })
  
  output$downloadSig <- downloadHandler(
    filename = function() { paste("Signature", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSig(), path = file)}
  )
  
  
  ##############Signature Search2##############
  dfSig2 <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if(input$downSig2 != '')
    {
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `-Log10(Pval)` < '", input$downSig2, "';")
      if (input$sigSearch2 != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `-Log10(Pval)` < '", input$downSig2, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)`) LIKE '%", input$sigSearch2, "%';")
      }
    }
    else
    {
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "';")
      if (input$sigSearch2 != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)` 
                       FROM presibo.`target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `-Log10(Pval)`) LIKE '%", input$sigSearch2, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$view2 <- renderDataTable({
    datatable(head(dfSig2(), n = input$sigShow2), options = list(dom = 't'))
  })
  
  output$downloadSig2 <- downloadHandler(
    filename = function() { paste("Signature", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSig2(), path = file)}
  )
  
  ##############Signature Network Search##############
  dfSigNet <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if (input$sigNet != "") {
      query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND `Z-Summary` < '", input$sigNet, "';")
      if (input$sigNetSearch != "") {
        query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND `Z-Summary` < '", input$sigNet, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`) 
                        LIKE '%", input$sigNetSearch, "%';")
      }
    }
      
    else {
      query <- paste0("SELECT
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "';")
      if (input$sigNetSearch != "") {
        query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`) 
                        LIKE '%", input$sigNetSearch, "%';")
      }
    }
      
    dbGetQuery(conn, query)
  })
  
  output$viewtbl0 <- renderDataTable({
    datatable(head(dfSigNet(), n = input$sigNetShow), options = list(dom = 't'))
  })
  
  # row selection
  #output$viewtbl0 = renderDataTable(dfSigNet(), server = FALSE, selection = 'single')
  #output$viewtbl2 = renderPrint(input$viewtbl0_rows_selected)
  
  output$downloadSigNet <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSigNet(), path = file)}
  )
  
  ##############Network Genetic Search##############  
  dfNetGene <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if (input$sigNet != "") {
      query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND `Z-Summary` < '", input$netGene, "';")
      if (input$sigGeneSearch != "") {
        query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND `Z-Summary` < '", input$netGene, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`) 
                        LIKE '%", input$sigGeneSearch, "%';")
      }
    }
    
    else {
      query <- paste0("SELECT
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "';")
      if (input$sigGeneSearch != "") {
        query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`) 
                        LIKE '%", input$sigGeneSearch, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$viewtbl <- renderDataTable({
    datatable(head(dfNetGene()), options = list(dom = 't'))
  })
  
  #output$viewtbl = renderDataTable(dfNetGene(), server = FALSE, selection = 'single')
  #output$viewtbl2 = renderPrint(input$viewtbl_rows_selected)
  
  output$downloadNetGene <- downloadHandler(
    filename = function() { paste("Network_Genetic", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetGene(), path = file)}
  )
  
  ##############Network Drug Search##############  
  dfNetDrug <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    query <- paste0("SELECT
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "';")
    if (input$sigDrugSearch != "") {
      query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`
                      FROM presibo.networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`, 
                          `Braak GWAS Pval`, 
                          `NP GWAS Pval`) 
                        LIKE '%", input$sigDrugSearch, "%';")
    }
    dbGetQuery(conn, query)
  })
  
  output$viewtable1 <- renderDataTable({
    datatable(head(dfNetDrug()), options = list(dom = 't'))
  })
  
  #output$viewtable1 = renderDataTable(dfNetDrug(), server = FALSE, selection = 'single')
  #output$viewtable12 = renderPrint(input$viewtable1_rows_selected)
  
  output$downloadNetDrug <- downloadHandler(
    filename = function() { paste("Network_Drug", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetDrug(), path = file)}
  )
  
  ##############Indication Drug Search##############
  dfInd <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if(input$drugSearch != '') {
      query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id FROM presibo.drugdb WHERE gene_id = '", input$drugSearch, "';")
      if(input$IndicSearch != '') {
        query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id FROM presibo.drugdb WHERE gene_id = '", input$drugSearch, "' AND indication = ", '"', input$IndicSearch, '"', ";")
      }
      if (input$searchInd != '') {
        query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id 
                       FROM presibo.drugdb 
                       WHERE gene_id = '", input$drugSearch, "' AND CONCAT(gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id) LIKE '%", input$searchInd, "%';")
      }
      dbGetQuery(conn, query)
    }
  })
  
  output$viewtable2 <- renderDataTable({
    datatable(head(dfInd()), options = list(dom = 't'))
  })
  
  output$downloadIndic <- downloadHandler(
    filename = function() { paste("Indication_Drug", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfInd(), path = file)}
  )
  
  ##############MOA Drug Search##############
  dfMOA <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if(input$drugSearch != '') {
      query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id FROM presibo.drugdb WHERE gene_id = '", input$drugSearch, "';")
      if(input$MOAsearch != '') {
        query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id FROM presibo.drugdb WHERE gene_id = '", input$drugSearch, "' AND moa = '", input$MOAsearch, "';")
      }
      if (input$searchMOA != '') {
        query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id 
                       FROM presibo.drugdb 
                       WHERE gene_id = '", input$drugSearch, "' AND CONCAT(gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id) LIKE '%", input$searchMOA, "%';")
      }
      dbGetQuery(conn, query)
    }
  })
  
  output$viewtable3 <- renderDataTable({
    datatable(head(dfMOA()), options = list(dom = 't'))
  })
  
  output$downloadMOA <- downloadHandler(
    filename = function() { paste("MOA_Drug", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfMOA(), path = file)}
  )
  
}