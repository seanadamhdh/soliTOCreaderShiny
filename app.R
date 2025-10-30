

if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}
if(!require(shinyjs)){
  install.packages("shinyjs")
  reqiure(shinyjs)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
}
if(!require(simplerspec)){
  install_github("https://github.com/philipp-baumann/simplerspec.git")
  require(simplerspec)}
if(!require(prospectr)){
  #install_packages("prospectr")
  install_github("https://github.com/l-ramirez-lopez/prospectr.git")
  require(prospectr)
}
# 
# if(!require(TUBAFsoilFunctions)){
#   install_github("https://github.com/seanadamhdh/TUBAFsoilFunctions.git",ref="dev")
#   require(TUBAFsoilFunctions)}
fix_soliTOC_colnames=function (soliTOC_raw_excel, save = F, save_location = "keep", 
          return = T, Date_Time_colname_position = 6, time.col_name = "Zeit") 
{
  if (is.character(soliTOC_raw_excel)) {
    soliTOC_file = read_excel(soliTOC_raw_excel)
  }
  else if (is.data.frame(soliTOC_raw_excel)) {
    soliTOC_file = soliTOC_raw_excel
  }
  else {
    print("ERROR: Neither path to soliTOC Excel file or data.frame with soliTOC data provided.")
    return(NULL)
  }
  colnames = names(soliTOC_file)
  if (str_detect(colnames[Date_Time_colname_position], time.col_name)) {
    colnames_new = c(colnames[1:(Date_Time_colname_position - 
                                   1)], str_split_fixed(colnames[6], pattern = " ", 
                                                        n = 2)[1, 1] %>% str_remove_all(pattern = " "), str_split_fixed(colnames[6], 
                                                                                                                        pattern = " ", n = 2)[1, 2] %>% str_remove_all(pattern = " "), 
                     colnames[(Date_Time_colname_position + 1):(length(colnames) - 
                                                                  1)])
    print(colnames_new)
    names(soliTOC_file) <- colnames_new
    print(names(soliTOC_file))
  }
  else {
    print("Warning: No changes made. Column names seem to be in order. Check manually.")
  }
  if (save == T) {
    if (save_location == "keep" & is.character(soliTOC_raw_excel)) {
      write.csv(soliTOC_file, paste0(dirname(soliTOC_raw_excel), 
                                     "/", str_remove(basename(soliTOC_raw_excel), 
                                                     ".xlsx")[1], "_fixed.xlsx"))
    }
    else if (!save_location == "keep") {
      write.csv(soliTOC_file, save_location)
    }
    else {
      print("Warning: No save location provided. No file saved.")
    }
  }
  if (return == T) {
    return(soliTOC_file)
  }
}


get_dayfactors=function (dataset, ID_col = "Name", std_id = "caco3", value_col = "TC  [%]", 
          actual = 12, keep_batch = F) 
{
  dataset <- dummy_df <- mutate(dataset, factor = if_else(.data[[ID_col]] == 
                                                            std_id, actual/.data[[value_col]], NA))
  if (is.na(dataset$factor[1])) {
    dataset$factor[1] <- 1
  }
  {
    b <- 0
    batch <- c(b)
    for (i in c(2:nrow(dataset))) {
      if (dataset[[ID_col]][i] == std_id & dataset[[ID_col]][i - 
                                                             1] != std_id) {
        b <- b + 1
      }
      batch <- c(batch, b)
    }
  }
  dataset <- tibble(dataset, batch)
  factor_list <- dataset %>% group_by(batch) %>% summarise(mean(factor, 
                                                                na.rm = T))
  factor_ <- left_join(data.frame(batch), factor_list, by = "batch")
  names(factor_) <- c("batch", "factor")
  factor_ <- mutate(factor_, final_factor = if_else(dummy_df$factor %>% 
                                                      is.na, factor, dummy_df$factor))
  dataset$factor <- factor_$final_factor
  if (!keep_batch) {
    dataset <- select(dataset, -c("batch"))
  }
  return(dataset)
}




pull_set_soliTOC=function (soliTOC_file, fix_cols = T, ID_col_set = "Name", set_id = c("LA", 
                                                                      "LQ", "LR", "LP", "LG"), ID_col_std = "Name", std_id = "caco3", 
          Memo_col_set = "Memo", set_memo = c("GT300"), std_value_col = "TC  [%]", 
          actual = 12, keep_batch = F, omit_check_cols = F) 
{
  if (is.character(soliTOC_file)) {
    print("read excel")
    soliTOC_raw = read_excel(soliTOC_file)
  }
  else if (is.data.frame(soliTOC_file)) {
    soliTOC_raw = soliTOC_file
  }
  else {
    print("ERROR: Neither path to soliTOC Excel file or data.frame with soliTOC data provided.")
    return(NULL)
  }
  if (fix_cols) {
    soliTOC_raw = fix_soliTOC_colnames(soliTOC_raw)
  }
  print(soliTOC_raw)
  if ((!omit_check_cols) & (names(soliTOC_raw) %>% last %>% 
                            str_remove("...") %>% str_detect("[:digit:]"))) {
    print("ERROR: Colnames do not match data. Check raw Excel format. Probably Date/Time separation issue.")
    return(NULL)
  }
  else {
    soliTOC_all = get_dayfactors(soliTOC_raw, ID_col = ID_col_std, 
                                 std_id = std_id, value_col = std_value_col, actual = actual, 
                                 keep_batch = keep_batch)
    soliTOC_all = mutate(soliTOC_all, TOC400 = `TOC400  [%]` * 
                           factor, ROC = `ROC  [%]` * factor, TIC900 = `TIC900  [%]` * 
                           factor, TOC = `TOC  [%]` * factor, TC = `TC  [%]` * 
                           factor)
    if (!any(is.na(set_memo))) {
      soliTOC_set = soliTOC_all %>% filter(str_detect(.data[[ID_col_set]], 
                                                      paste(set_id, collapse = "|")) & str_detect(.data[[Memo_col_set]], 
                                                                                                  paste(set_memo, collapse = "|")))
    }
    else {
      soliTOC_set = soliTOC_all %>% filter(str_detect(.data[[ID_col_set]], 
                                                      paste(set_id, collapse = "|")))
    }
    return(soliTOC_set)
  }
}

soliTOC_remove_duplicates=function (dataset, measurement_method = "DIN19539", reference = T, 
          method = "best_MAE", measurement_method.col_name = "Methode", 
          date.col_name = "Datum", time.col_name = "Zeit", name.col_name = "Name", 
          reference.col_name = "CORG", measured.col_name = "TOC") 
{
  duplicates <- filter(dataset, .data[[measurement_method.col_name]] == 
                         measurement_method) %>% filter(.data[[name.col_name]] %in% 
                                                          {
                                                            dataset %>% filter(.data[[measurement_method.col_name]] == 
                                                                                 measurement_method) %>% filter(duplicated({
                                                                                   dataset %>% filter(.data[[measurement_method.col_name]] == 
                                                                                                        measurement_method) %>% pull(name.col_name)
                                                                                 })) %>% pull(name.col_name)
                                                          })
  print("duplicates found")
  print(nrow(duplicates))
  if (nrow(duplicates > 0)) {
    if (reference == T & method == "best_MAE") {
      best_measurements <- duplicates %>% mutate(MAE = abs(.data[[measured.col_name]] - 
                                                             .data[[reference.col_name]])) %>% group_by(.data[[name.col_name]]) %>% 
        filter(MAE == min(MAE))
      dataset_new <- dataset %>% filter(!(paste(.data[[date.col_name]], 
                                                .data[[time.col_name]]) %in% paste(duplicates[[date.col_name]], 
                                                                                   duplicates[[time.col_name]])) | paste(.data[[date.col_name]], 
                                                                                                                         .data[[time.col_name]]) %in% paste(best_measurements[[date.col_name]], 
                                                                                                                                                            best_measurements[[time.col_name]]))
    }
    else if (method == "mean") {
      best_measurements <- duplicates %>% group_by(.data[[name.col_name]]) %>% 
        summarise(across(where(is.numeric), ~mean(.x, 
                                                  na.rm = T)), across(where(is.integer), ~mean(.x, 
                                                                                               na.rm = T)), across(where(is.double), ~mean(.x, 
                                                                                                                                           na.rm = T)), across(where(is.character), ~first(.x, 
                                                                                                                                                                                           na_rm = T)), across(where(is.logical), ~first(.x, 
                                                                                                                                                                                                                                         na_rm = T)))
      dataset_new <- dataset %>% filter(!(paste(.data[[date.col_name]], 
                                                .data[[time.col_name]]) %in% paste(duplicates[[date.col_name]], 
                                                                                   duplicates[[time.col_name]])) | paste(.data[[date.col_name]], 
                                                                                                                         .data[[time.col_name]]) %in% paste(best_measurements[[date.col_name]], 
                                                                                                                                                            best_measurements[[time.col_name]]))
    }
    else if (method == "latest") {
      best_measurements <- duplicates %>% group_by(.data[[name.col_name]]) %>% 
        summarise(across(everything(), ~last(.x, na_rm = T)))
      dataset_new <- dataset %>% filter(!(paste(.data[[date.col_name]], 
                                                .data[[time.col_name]]) %in% paste(duplicates[[date.col_name]], 
                                                                                   duplicates[[time.col_name]])) | paste(.data[[date.col_name]], 
                                                                                                                         .data[[time.col_name]]) %in% paste(best_measurements[[date.col_name]], 
                                                                                                                                                            best_measurements[[time.col_name]]))
    }
    print("duplicates removed")
    print(nrow(dataset) - nrow(dataset_new))
    return(dataset_new)
  }
  else {
    return(dataset)
  }
}


process_soliTOC <- function(
    soliTOC_file, 
    fix_cols = TRUE, 
    ID_col_set = "Name", 
    set_id = c("LA", "LQ", "LR", "LP", "LG"), 
    ID_col_std = "Name", 
    std_id = "caco3", 
    Memo_col_set = "Memo", 
    set_memo = c("GT300"), 
    std_value_col = "TC  [%]", 
    actual = 12, 
    keep_batch = FALSE, 
    omit_check_cols = FALSE,
    remove_duplicates=TRUE,
    measurement_method = "DIN19539",
    reference = TRUE,
    method = "best_MAE",
    measurement_method.col_name = "Methode",
    date.col_name = "Datum",
    time.col_name = "Zeit",
    name.col_name = "Name",
    reference.col_name = "CORG",
    measured.col_name = "TOC"
) {
  # Step 1: Pull and preprocess the dataset
  soliTOC_set <- tryCatch({
    pull_set_soliTOC(
      soliTOC_file = soliTOC_file,
      fix_cols = fix_cols,
      ID_col_set = ID_col_set,
      set_id = set_id,
      ID_col_std = ID_col_std,
      std_id = std_id,
      Memo_col_set = Memo_col_set,
      set_memo = set_memo,
      std_value_col = std_value_col,
      actual = actual,
      keep_batch = keep_batch,
      omit_check_cols = omit_check_cols
    )
  }, error = function(e) {
    message("Error in pull_set_soliTOC: ", e$message)
    return(NULL)
  })
  
  if (is.null(soliTOC_set)) return(NULL)
  
  # Step 2: Remove duplicates (optional)
  if (remove_duplicates) {
    soliTOC_clean <- tryCatch({
      soliTOC_remove_duplicates(
        dataset = soliTOC_set,
        measurement_method = measurement_method,
        reference = reference,
        method = method,
        measurement_method.col_name = measurement_method.col_name,
        date.col_name = date.col_name,
        time.col_name = time.col_name,
        name.col_name = name.col_name,
        reference.col_name = reference.col_name,
        measured.col_name = measured.col_name
      )
    }, error = function(e) {
      message("Error in soliTOC_remove_duplicates: ", e$message)
      return(NULL)
    })
    
    if (is.null(soliTOC_clean)) return(NULL)
  } else {
    soliTOC_clean <- soliTOC_set
  }
  
  # Step 3: Rename columns
  OUTPUT <- soliTOC_clean %>% 
    rename(
      Pos = `Pos.`,
      Gewicht_mg = `Gewicht  [mg]`,
      TOC400area = `TOC400  Fläche`,
      ROCarea = `ROC  Fläche`,
      TIC900area = `TIC900  Fläche`,
      TOC400raw = `TOC400  [%]`,
      ROCraw = `ROC  [%]`,
      TIC900raw = `TIC900  [%]`,
      TOC400coef = `TOC400  Faktor`,
      ROCcoef = `ROC  Faktor`,
      TIC900coef = `TIC900  Faktor`,
      DillutionFaktor = `Verd.  Faktor`,
      TOC400blank = `TOC400  Blind`,
      ROCblank = `ROC  Blind`,
      TIC900blank = `TIC900  Blind`,
      TOCraw = `TOC  [%]`,
      TCraw = `TC  [%]`
    )
  
  return(OUTPUT)
}










ui <- fluidPage(
  
  # Application title
  titlePanel("Load and process soliTOC csv exports"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      selectInput(inputId = "ID_col",
                  label="ID column",
                  choices=c("Name",
                            "Memo",
                            "Pos.",
                            "Datum",
                            "Methode",
                            "Zeit",
                            "Info"),
                  selected = "Name"
      ),
      
      
      print("Set . in text filters to select all"),
      textInput(inputId = "set_id",
                label="Common batch ID",
                value="."
      ),
      
      selectInput(inputId = "memo_col",
                  label="Second ID column",
                  choices=c("Name",
                            "Memo",
                            "Pos.",
                            "Datum",
                            "Methode",
                            "Zeit",
                            "Info"),
                  selected = "Name"
      ),
      textInput(inputId = "set_memo",
                label="Second common batch ID",
                value="."
      ),
      
      
      selectInput(inputId = "std_col",
                  label="Standard ID column",
                  choices=c("Name",
                            "Memo",
                            "Pos.",
                            "Datum",
                            "Methode",
                            "Zeit",
                            "Info"),
                  selected = "Name"
      ),
      textInput(inputId = "std_id",
                label="Standard ID",
                value="caco3",
      ),
      
      numericInput(inputId = "actual",
                   label="Standard theroretical value",
                   value = 12),
      
      
      checkboxInput(inputId = "omit_check_cols",label = "Should column check be omitted?",value = FALSE),
      checkboxInput(inputId = "fix_cols",label = "Attempt to fix columnn names",value = TRUE),
      checkboxInput(inputId = "keep_batch",label = "Keep batch information (dayfactor grouping)?",value=TRUE),
      
      checkboxInput(inputId = "remove_duplicates",label = "Remove or average replicates?",value=FALSE),
      
      textInput(inputId = "meas_method",
                label="Methodenname",
                value="DIN19539"),
      
      selectInput(inputId = "fix_method",
                  label="Standard ID column",
                  choices=c("mean",
                            "latest"),
                  selected = "mean"),
      
      
      
      fileInput(inputId = "soliTOCpath",
                label = "Select soliTOC csv measuurement file: ",
                multiple=F
      ),
      
      
      withBusyIndicatorUI(
        actionButton("go","Run",
                     style="color: #fff; background-color: #22e",
                     class="btn-primary")
      ),
      
      
      uiOutput("download"),
      
      position="left"
    ), 
    
    
    mainPanel(
      
      textOutput("action"),
    
      tableOutput("table")
        
    )
  )
  
)

# SERVER ####
server <- function(input, output) {
  
  soliTOCfile <- eventReactive(input$go, {
    
    cat("fix_cols ", input$fix_cols,"\n",
    "ID_col_set ", input$ID_col,"\n",
    "set_id ",input$set_id,"\n",
    "ID_col_std " , input$std_col,"\n",
    "std_id" ,input$std_id,"\n",
    "Memo_col_set  ",input$memo_col,"\n",
    "set_memo ",input$set_memo,"\n",
    "std_value_col ", input$std_id,"\n",
    "actual ", input$actual,"\n",
   " keep_batch", input$keep_batch,"\n",
    "omit_check_cols", input$omit_check_cols,"\n"
    )
    process_soliTOC(
      soliTOC_file=input$soliTOCpath$datapath, 
      fix_cols = input$fix_cols, 
      ID_col_set = input$ID_col, 
      set_id = input$set_id, 
      ID_col_std = input$std_col, 
      std_id = input$std_id, 
      Memo_col_set = input$memo_col, 
      set_memo = input$set_memo, 
      std_value_col = "TC  [%]", 
      actual = input$actual, 
      keep_batch = input$keep_batch, 
      omit_check_cols = input$omit_check_cols,
      remove_duplicates=input$remove_duplicates,
      measurement_method = input$meas_method,
      reference = F,
      method = input$fix_method,
      measurement_method.col_name = "Methode",
      date.col_name = "Datum",
      time.col_name = "Zeit",
      name.col_name = input$ID_col,
      reference.col_name = "CORG",
      measured.col_name = "TOC"
    )->x0
    return(list(x0 = as.data.frame(x0)))
  })
  
  OUT <- eventReactive(input$go, {
    withBusyIndicatorServer("go", {
      x1 <- soliTOCfile()
      
      if (is.null(x1)) {
        return(list(x1 = tibble()))
      }
      
      # DEBUG
      cat("[DEBUG] OUT() x1 structure:\n")
      print(str(x1$x0, max.level = 1))
      
      list(x1 = as_tibble(x1$x0))
    })
  })
  
  table <- reactive({
    req(OUT())
    OUT()$x1
  })

  output$table<-renderTable({
    
    table()
    
    
  })  
  
  
  output$downloadData <- downloadHandler(
    filename = function() paste0(Sys.time(), ".csv"),
    content = function(file) write.csv(table(), file, row.names = FALSE)
  )
  
  output$download <- renderUI({
    req(input$go, table())
    downloadButton("downloadData", "Download as csv")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)



