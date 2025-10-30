

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

if(!require(TUBAFsoilFunctions)){
  install_github("https://github.com/seanadamhdh/TUBAFsoilFunctions.git",ref="dev")
  require(TUBAFsoilFunctions)}




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
    TUBAFsoilFunctions::pull_set_soliTOC(
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
      TUBAFsoilFunctions::soliTOC_remove_duplicates(
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
                  selected = "Memo"
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
      
      
      checkboxInput(inputId = "omit_check_cols",label = "Should column check be omitted?"),
      checkboxInput(inputId = "fix_cols",label = "Attempt to fix columnn names"),
      checkboxInput(inputId = "keep_batch",label = "Keep batch information (dayfactor grouping)?"),
      
      checkboxInput(inputId = "remove_duplicates",label = "Remove or average replicates?"),
      
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
      #
      textOutput("action")
      
      
      
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



