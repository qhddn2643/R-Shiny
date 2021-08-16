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
        query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "';")
        if(input$predFilter != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND gp.p < '", input$predFilter, "';")
        }
        if (input$predSearch != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p) LIKE '%", input$predSearch, "%';")
        }
      }
      
      if (input$var == "Biomarker-CSF.Abeta" || input$var == "Biomarker-CSF.pTau" || input$var == "Biomarker-PET" || input$var == "Biomarker-MRI.HPV") {
        query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "';")
        if(input$predFilter != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND gp.p < '", input$predFilter, "';")
        }
        if (input$predSearch != '') {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.gene_id = '", input$targetSearch, "' AND CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p) LIKE '%", input$predSearch, "%';")
        }
      }
      
      if (input$var == "Signature-eQTL" || input$var == "Signature-mQTL") {
        query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE gene_id = '", input$targetSearch, "';")
        if(input$predFilter != '') {
          query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE gene_id = '", input$targetSearch, "' AND p < '", input$predFilter, "';")
        }
        if (input$predSearch != '') {
          query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE gene_id = '", input$targetSearch, "' AND CONCAT(snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p) LIKE '%", input$predSearch, "%';")
        }
      }
    }
    
    #######When type the only one positon############
    else  {
      split_list_var = strsplit(input$targetSearch, ":")
      #print(split_list_var[[1]][1])
      #print(split_list_var[[1]][2])
      
      #######When type the range of positons############
      if (strsplit(split_list_var[[1]][2], "-") != split_list_var[[1]][2]) {
        new_split_list_var = strsplit(split_list_var[[1]][2], "-")
        
        #print(new_split_list_var[[1]][1])
        #print(new_split_list_var[[1]][2])
        
        new_var1 = paste0(split_list_var[[1]][1], "-", new_split_list_var[[1]][1])
        new_var2 = paste0(split_list_var[[1]][1], "-", new_split_list_var[[1]][2])
        #print(new_var1)
        #print(new_var2)
        
        if (input$var == "Outcome-Clinical Diagnosis" || input$var == "Outcome-Tangle" || input$var == "Outcome-Plague") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.p < '", input$predFilter, "' AND gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p) LIKE '%", input$predSearch, "%';")
          }
        }
        
        if (input$var == "Biomarker-CSF.Abeta" || input$var == "Biomarker-CSF.pTau" || input$var == "Biomarker-PET" || input$var == "Biomarker-MRI.HPV") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.p < '", input$predFilter, "' AND gv.`#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p) LIKE '%", input$predSearch, "%';")
          }
        }
        
        if (input$var == "Signature-eQTL" || input$var == "Signature-mQTL") {
          query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE `#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE p < '", input$predFilter, "' AND `#variant_id` BETWEEN '", new_var1, "' AND '", new_var2, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE CONCAT(snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p) LIKE '%", input$predSearch, "%';")
          }
        } 
      }
      
      #######When type the only one positon############
      else {
        new_var1 = paste0(split_list_var[[1]][1], "-", split_list_var[[1]][2])
        print(new_var1)
        if (input$var == "Outcome-Clinical Diagnosis" || input$var == "Outcome-Tangle" || input$var == "Outcome-Plague") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` = '", new_var1, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.p < '", input$predFilter, "' AND gv.`#variant_id` = '", new_var1, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-outcome1` gp ON gv.snp_id = gp.snp_id
                         WHERE CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p) LIKE '%", input$predSearch, "%';")
          }
        }
        
        if (input$var == "Biomarker-CSF.Abeta" || input$var == "Biomarker-CSF.pTau" || input$var == "Biomarker-PET" || input$var == "Biomarker-MRI.HPV") {
          query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gv.`#variant_id` = '", new_var1, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE gp.p < '", input$predFilter, "' AND gv.`#variant_id` = '", new_var1, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p 
                         FROM gwasvariantreferencetable gv
                         INNER JOIN `gwas-pred-biomarker` gp ON gv.snp_id = gp.snp_id
                         WHERE CONCAT(gp.snp_id, gp.a1, gp.a2, gp.freq1, gp.beta, gp.se, gp.p) LIKE '%", input$predSearch, "%';")
          }
        }
        
        if (input$var == "Signature-eQTL" || input$var == "Signature-mQTL") {
          query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE `#variant_id` = '", new_var1, "';")
          if(input$predFilter != '') {
            query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE p < '", input$predFilter, "' AND `#variant_id` = '", new_var1, "';")
          }
          if (input$predSearch != '') {
            query <- paste0("SELECT snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p 
                         FROM `hwas-pred-signature`
                         WHERE CONCAT(snp_id, feature_name, feature_ch, feature_start, spearman_rho, ensgid, p) LIKE '%", input$predSearch, "%';")
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
  
  output$tbl_selected <- renderText({ 
    if (input$var != '') {
      paste("Table: ", input$var)
    }
  })
  
  ##############Signature Search - Top table##############
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
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `P-value` < '", input$downSig, "';")
      if (input$sigSearch != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `P-value` < '", input$downSig, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value`) LIKE '%", input$sigSearch, "%';")
      }
    }
    else
    {
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "';")
      if (input$sigSearch != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value`) LIKE '%", input$sigSearch, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$view <- renderDataTable({
    datatable(head(dfSig(), n = input$sigShow), options = list(dom = 't', pageLength = input$sigShow))
  })
  
  output$downloadSig <- downloadHandler(
    filename = function() { paste("Signature", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSig(), path = file)}
  )
  
  ##############Signature Search2 - Bottom table##############
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
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `P-value` < '", input$downSig2, "';")
      if (input$sigSearch2 != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND `P-value` < '", input$downSig2, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value`) LIKE '%", input$sigSearch2, "%';")
      }
    }
    else
    {
      query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "';")
      if (input$sigSearch2 != '') {
        query <- paste0("SELECT `Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value` 
                       FROM `target-signature` 
                       WHERE `Gene ID` = '", input$targetSearch, "' AND CONCAT(`Gene ID`, Source, Type, `Cell Type`, `Z-Score or LogFC`, `P-value`) LIKE '%", input$sigSearch2, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$view2 <- renderDataTable({
    datatable(head(dfSig2(), n = input$sigShow2), options = list(dom = 't', pageLength = input$sigShow2))
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
                          `AD GWAS Pval`
                      FROM networksearch 
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
                          `AD GWAS Pval`
                      FROM networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND `Z-Summary` < '", input$sigNet, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`) 
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
                          `AD GWAS Pval`
                      FROM networksearch 
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
                          `AD GWAS Pval`
                      FROM networksearch 
                      WHERE `Gene ID` = '", input$netSearch, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`) 
                        LIKE '%", input$sigNetSearch, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$viewtbl0 <- renderDataTable({
    datatable(head(dfSigNet(), n = input$sigNetShow), options = list(dom = 't', pageLength = input$sigNetShow), selection = 'single')
  })
  
  output$downloadSigNet <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSigNet(), path = file)}
  )
  
  output$sgn_output1 <- renderText({ 
    if (input$netSearch != '') {
      paste("Signature based networks contain ", input$netSearch)
    }
  })
  
  # row selection
  #output$viewtbl0 = renderDataTable(dfSigNet(), server = FALSE, selection = 'single')
  #output$viewtbl12 = renderPrint(input$viewtbl0_rows_selected)
  
  output$sgn_output2 <- renderText({ 
    if (input$viewtbl0_rows_selected == 1) {
      paste("Signature profiles of genes in M17")
    }
    else if (input$viewtbl0_rows_selected == 2) {
      paste("Signature profiles of genes in M2")
    }
    else if (input$viewtbl0_rows_selected == 3) {
      paste("Signature profiles of genes in M45")
    }
  })
  
  dfSigNet2 <- reactive({
    renderPrint(input$viewtbl0_rows_selected)
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if (input$viewtbl0_rows_selected == 1) {
      query <- paste0("SELECT `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P` FROM module_genes_m17;")
      if (input$sigNetSearch2 != "") {
        query <- paste0("SELECT `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P` FROM module_genes_m17 
                        WHERE CONCAT(`Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
                        LIKE '%", input$sigNetSearch2, "%';")
      }
      dbGetQuery(conn, query)
    }
    else if (input$viewtbl0_rows_selected == 2) {
      query <- paste0("SELECT `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P` FROM module_genes_m2;")
      if (input$sigNetSearch2 != "") {
        query <- paste0("SELECT `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P` FROM module_genes_m2 
                        WHERE CONCAT(`Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
                        LIKE '%", input$sigNetSearch2, "%';")
      }
      dbGetQuery(conn, query)
    }
    else if (input$viewtbl0_rows_selected == 3) {
      query <- paste0("SELECT `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P` FROM module_genes_m45;")
      if (input$sigNetSearch2 != "") {
        query <- paste0("SELECT `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P` FROM module_genes_m45 
                        WHERE CONCAT(`Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
                        LIKE '%", input$sigNetSearch2, "%';")
      }
      dbGetQuery(conn, query)
    }
  })
  
  output$viewtbl12_1 <- renderDataTable({
    datatable(head(dfSigNet2(), n = input$sigNetShow2), options = list(dom = 't', pageLength = input$sigNetShow2))
  })
  
  output$downloadSigNet2 <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfSigNet2(), path = file)}
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
                          `AD GWAS Pval`
                      FROM junming_ampad_blood_network_transcriptome_dx_all 
                      WHERE `Gene ID` = '", input$netSearch, "' AND `Z-Summary` < '", input$netGene, "';")
      if (input$sigNetSearch != "") {
        query <- paste0("SELECT 
                          `Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`
                      FROM junming_ampad_blood_network_transcriptome_dx_all 
                      WHERE `Gene ID` = '", input$netSearch, "' AND `Z-Summary` < '", input$netGene, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`) 
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
                          `AD GWAS Pval`
                      FROM junming_ampad_blood_network_transcriptome_dx_all 
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
                          `AD GWAS Pval`
                      FROM junming_ampad_blood_network_transcriptome_dx_all 
                      WHERE `Gene ID` = '", input$netSearch, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`) 
                        LIKE '%", input$sigGeneSearch, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$viewtbl <- renderDataTable({
    datatable(dfNetGene(), options = list(dom = 't'), selection = 'single')
  })
  
  output$downloadNetGene <- downloadHandler(
    filename = function() { paste("Network_Genetic", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetGene(), path = file)}
  )
  
  output$ngs_output2 <- renderText({ 
    if (input$viewtbl_rows_selected == 1) {
      paste("Signature profiles of genes in M17")
    }
    else if (input$viewtbl_rows_selected == 2) {
      paste("Signature profiles of genes in M2")
    }
    else if (input$viewtbl_rows_selected == 3) {
      paste("Signature profiles of genes in M45")
    }
  })
  
  dfNetGene2 <- reactive({
    renderPrint(input$viewtbl_rows_selected)
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    if (input$viewtbl_rows_selected == 1) {
      query <- paste0("SELECT * FROM module_genes_m17;")
      if (input$sigGeneSearch2 != "") {
        query <- paste0("SELECT * FROM module_genes_m17 
                        WHERE CONCAT(ENSGID, `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
                        LIKE '%", input$sigGeneSearch2, "%';")
      }
      dbGetQuery(conn, query)
    }
    else if (input$viewtbl_rows_selected == 2) {
      query <- paste0("SELECT * FROM module_genes_m2;")
      if (input$sigGeneSearch2 != "") {
        query <- paste0("SELECT * FROM module_genes_m2 
                        WHERE CONCAT(ENSGID, `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
                        LIKE '%", input$sigGeneSearch2, "%';")
      }
      dbGetQuery(conn, query)
    }
    else if (input$viewtbl_rows_selected == 3) {
      query <- paste0("SELECT * FROM module_genes_m45;")
      if (input$sigGeneSearch2 != "") {
        query <- paste0("SELECT * FROM module_genes_m45 
                        WHERE CONCAT(ENSGID, `Gene Name`, `Tissue P`, `Ast P`, `End P`, `Exn P`, `Inn P`, `Mic P`, `Oli P`, `Opc P`) 
                        LIKE '%", input$sigGeneSearch2, "%';")
      }
      dbGetQuery(conn, query)
    }
  })
  
  output$viewtbl2_1 <- renderDataTable({
    datatable(dfNetGene2(), options = list(dom = 't'))
  })
  
  output$downloadNetGene2 <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetGene2(), path = file)}
  )
  
  ##############Network Drug Search- Top table##############  
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
                          `AD GWAS Pval`
                      FROM junming_ampad_brain_network_proteome_dx_all 
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
                          `AD GWAS Pval`
                      FROM junming_ampad_brain_network_proteome_dx_all 
                      WHERE `Gene ID` = '", input$netSearch, "' AND 
                        CONCAT(`Gene ID`, 
                          Type, 
                          `Module ID`, 
                          `No Genes`, 
                          `Z-Summary`, 
                          `Omics Source`, 
                          Source, `AD Pval`, 
                          `AD GWAS Pval`) 
                        LIKE '%", input$sigDrugSearch, "%';")
    }
    dbGetQuery(conn, query)
  })
  
  output$viewtable1 <- renderDataTable({
    datatable(dfNetDrug(), options = list(dom = 't'), selection = 'single')
  })
  
  #output$viewtable1 = renderDataTable(dfNetDrug(), server = FALSE, selection = 'single')
  #output$viewtable12 = renderPrint(input$viewtable1_rows_selected)
  
  output$ngd_output2 <- renderText({ 
    if (input$viewtable1_rows_selected == 1) {
      paste("Approved drugs targeting genes in M17")
    }
    else if (input$viewtable1_rows_selected == 2) {
      paste("Approved drugs targeting genes in M2")
    }
    else if (input$viewtable1_rows_selected == 3) {
      paste("Approved drugs targeting genes in M45")
    }
  })
  
  output$downloadNetDrug <- downloadHandler(
    filename = function() { paste("Network_Drug", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetDrug(), path = file)}
  )
  
  dfNetDrug2 <- reactive({
    conn <- dbConnect(
      MySQL(),
      dbname = "presibo",
      host = "localhost",
      username = "root",
      password = "newrootpassword"
    )
    on.exit(dbDisconnect(conn), add = TRUE)
    
    if (input$viewtable1_rows_selected == 1) {
      query <- paste0("SELECT dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id
                       FROM drugdb dr
                       INNER JOIN module_genes_m17 ms ON dr.gene_id = ms.`Gene Name`;")
      if (input$sigDrugSearch2 != "") {
        query <- paste0("SELECT dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id
                         FROM presibo.drugdb dr
                         INNER JOIN presibo.module_genes_m17 ms ON dr.gene_id = ms.`Gene Name`
                         WHERE CONCAT(dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id) 
                         LIKE '%", input$sigDrugSearch2, "%';")
      }
    }
    else if (input$viewtable1_rows_selected == 2) {
      query <- paste0("SELECT dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id
                       FROM drugdb dr
                       INNER JOIN module_genes_m2 ms ON dr.gene_id = ms.`Gene Name`;")
      if (input$sigDrugSearch2 != "") {
        query <- paste0("SELECT dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id
                         FROM presibo.drugdb dr
                         INNER JOIN presibo.module_genes_m2 ms ON dr.gene_id = ms.`Gene Name`
                         WHERE CONCAT(dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id) 
                         LIKE '%", input$sigDrugSearch2, "%';")
      }
    }
    else if (input$viewtable1_rows_selected == 3) {
      query <- paste0("SELECT dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id
                       FROM drugdb dr
                       INNER JOIN module_genes_m45 ms ON dr.gene_id = ms.`Gene Name`;")
      if (input$sigDrugSearch2 != "") {
        query <- paste0("SELECT dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id
                         FROM presibo.drugdb dr
                         INNER JOIN presibo.module_genes_m45 ms ON dr.gene_id = ms.`Gene Name`
                         WHERE CONCAT(dr.gene_id, dr.drug_name, dr.indication, dr.moa, dr.presibo_db_id, dr.drugbank_id) 
                         LIKE '%", input$sigDrugSearch2, "%';")
      }
    }
    dbGetQuery(conn, query)
  })
  
  output$viewtable12_1 <- renderDataTable({
    datatable(dfNetDrug2(), options = list(dom = 't'))
  })
  
  output$downloadNetDrug2 <- downloadHandler(
    filename = function() { paste("Signature_Network", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfNetDrug2(), path = file)}
  )
  
  ##############Drug Search##############
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
      query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id FROM drugdb WHERE gene_id = '", input$drugSearch, "';")
      if (input$searchInd != '') {
        query <- paste0("SELECT gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id 
                       FROM drugdb 
                       WHERE gene_id = '", input$drugSearch, "' AND CONCAT(gene_id, drug_name, indication, moa, presibo_db_id, drugbank_id) LIKE '%", input$searchInd, "%';")
      }
      dbGetQuery(conn, query)
    }
  })
  
  output$viewtable2 <- renderDataTable({
    datatable(head(dfInd(), n = input$showInd), options = list(dom = 't', pageLength = input$showInd))
  })
  
  output$downloadIndic <- downloadHandler(
    filename = function() { paste("Drug", "xlsx", sep = ".")},
    content = function(file) {write_xlsx(dfInd(), path = file)}
  )
  
}