library(RSelenium)
library(dplyr)
library(tidyr)
library(rvest)
library(webdriver)

rD <- RSelenium::rsDriver(port = 4158L, browser = "firefox", chromever = NULL)
remDr<-rD[["client"]]
remDr$navigate("https://mnregaweb4.nic.in/netnrega/SocialAuditFindings/SA-Frequency-Of-Reported-Issues-Report.aspx") 

#Find the state dropdown menu and get its options
state_dropdown <- remDr$findElement(using = "xpath", value = "//select[@name='ctl00$ContentPlaceHolder1$ddlstate']")
state_options <- state_dropdown$getElementText() %>% unlist()
state_text <- gsub("Select State", " ", state_options)
state_text <- unlist(strsplit(state_text, "\n"))
state_text <- state_text[-1]
state_text <- state_text[24] #Remove this if you want the loop to work with all states

#Create a data frame to store the errors
error_combinations_table <- data.frame(State = character(), District = character(), Block = character(), Panchayat = character(), Year = character(), stringsAsFactors = FALSE) #Captures errors in show 100 entries

error_combinations_entries <- data.frame(State = character(), District = character(), Block = character(), Panchayat = character(), Year = character(), stringsAsFactors = FALSE) #Captures errors in show 100 entries

file_error_combinations <- data.frame(State = character(), District = character(), Block = character(), Panchayat = character(), Year = character(), stringsAsFactors = FALSE) #Captures errors in file renaming

scraped_combinations <- data.frame(State = character(), District = character(), Block = character(), Panchayat = character(), Year = character(), stringsAsFactors = FALSE) #Captures all scraped combinations

#Loop through each state, district, and block combination
for (state in state_text) {
  #Select the state dropdown menu and choose the current state
  state_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddlstate']")
  state_dropdown$sendKeysToElement(list(state, key = "enter"))
  state_text <- gsub("Select State", " ", state_options)
  state_text <- unlist(strsplit(state_text, "\n"))
  state_text <- state_text[-1]
  
  #Wait for the district dropdown menu to load
  Sys.sleep(15)
  
  #Find the district dropdown menu and get its options
  district_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddldistrict']")
  district_options <- district_dropdown$getElementText() %>% unlist()
  district_text <- gsub("Select State", " ", district_options)
  district_text <- unlist(strsplit(district_text, "\n"))
  district_text <- district_text[-1]
  
  for (district in district_text) {
    #Select the district dropdown menu and choose the current district
    district_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddldistrict']")
    district_dropdown$sendKeysToElement(list(district, key = "enter"))
    district_text <- gsub("Select District", " ", district_options)
    district_text <- unlist(strsplit(district_text, "\n"))
    district_text <- district_text[-1]
    
    #Wait for the block dropdown menu to load
    Sys.sleep(15)
    
    #Find the block dropdown menu and get its options
    block_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddlBlock']")
    block_options <- block_dropdown$getElementText() %>% unlist()
    block_text <- gsub("Select Block", " ", block_options)
    block_text <- unlist(strsplit(block_text, "\n"))
    block_text <- block_text[-1]
    
    for (block in block_text) {
      #Select the block dropdown menu and choose the current block
      block_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddlBlock']")
      block_dropdown$sendKeysToElement(list(block, key = "enter"))
      block_text <- gsub("Select Block", " ", block_options)
      block_text <- unlist(strsplit(block_text, "\n"))
      block_text <- block_text[-1]
      
      #Wait for the block dropdown menu to load
      Sys.sleep(15)
      
      #Find the Panchayat dropdown menu and get its options
      panchayat_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddlPanchayat']")
      panchayat_options <- panchayat_dropdown$getElementText() %>% unlist()
      panchayat_text <- gsub("Select Panchayat", " ", panchayat_options)
      panchayat_text <- unlist(strsplit(panchayat_text, "\n"))
      panchayat_text <- panchayat_text[-1]
      
      for (panchayat in panchayat_text) {
        #Select the panchayt dropdown menu and choose the current panchayat
        panchayat_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddlPanchayat']")
        panchayat_dropdown$sendKeysToElement(list(panchayat, key = "enter"))
        panchayat_text <- gsub("Select Panchayat", " ", panchayat_options)
        panchayat_text <- unlist(strsplit(panchayat_text, "\n"))
        panchayat_text <- panchayat_text[-1]
        
        #Wait for the panchayat dropdown menu to load
        Sys.sleep(2)
        
        #Find the year dropdown menu and get its options
        year_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddlAuditYear']")
        year_options <- year_dropdown$getElementText() %>% unlist()
        year_text <- gsub("Select Year", " ", year_options)
        year_text <- unlist(strsplit(year_text, "\n"))
        year_text <- year_text[-1]
        
        for (year in year_text) {
          #Select the block dropdown menu and choose the current block
          year_dropdown <- remDr$findElement(using = "xpath", "//select[@name='ctl00$ContentPlaceHolder1$ddlAuditYear']")
          year_dropdown$sendKeysToElement(list(year, key = "enter"))
          year_text <- gsub("Select Year", " ", year_options)
          year_text <- unlist(strsplit(year_text, "\n"))
          year_text <- year_text[-1]
          
          #Wait for the year dropdown menu to load
          Sys.sleep(10)
          
          submit_button <- remDr$findElement(using = "xpath", value = '//*[@id="ContentPlaceHolder1_btnSubmit"]')
          retry_count <- 3
          for (i in seq_len(retry_count)) {
            element_loaded <- FALSE
            tryCatch({
              submit_button$clickElement()
              Sys.sleep(20)
              wait_for_element <- remDr$findElement(using = "xpath", "//div[@id='dtBasicExample_wrapper']")
              element_loaded <- TRUE
            }, error = function(e) {
              message(paste0("Attempt ", i, " failed, retrying in 20 seconds..."))
              Sys.sleep(20) #Wait for 20 seconds before retrying
            })
            
            if (element_loaded) {
              break #Exit the loop if successful
            } else if (i == retry_count) {
              error_combinations_table <- rbind(error_combinations_table, data.frame(State = state, District = district, Block = block, Panchayat = panchayat, Year = year, stringsAsFactors = FALSE))
              message(paste0("Combination (", state, ", ", district, ", ", block, ", ", panchayat, ", ", year, ") threw an error while loading the table. Moving on to the next combination."))
            }
          }
          
          #Changing how many options are displayed from 25 to 100
          #Using retrying since no such element errors are frequent
          retry_count <- 3
          for (j in seq_len(retry_count)) {
            success <- FALSE
            tryCatch({
              showentries_button <- remDr$findElement(using = 'xpath', "//select[@name = 'dtBasicExample_length']/option[@value='100']")
              showentries_button$clickElement()
              success <- TRUE
            }, error = function(e) {
              message(paste0("Attempt ", j, " failed, retrying in 20 seconds..."))
              Sys.sleep(20) #Wait for 20 seconds before retrying
            })
            
            if (success) {
              break #Exit the loop if successful
            } else if (j == retry_count) {
              #If all retries failed, store the combination that threw the error in the error_combinations data frame
              error_combinations_entries <- rbind(error_combinations_entries, data.frame(State = state, District = district, Block = block, Panchayat = panchayat, Year = year, stringsAsFactors = FALSE))
              message(paste0("Combination (", state, ", ", district, ", ", block, ", ", panchayat, ", ", year, ") threw an error in loading 100 entries. Moving on to the next combination."))
            }
          }
          
          Sys.sleep(20)
          
          #Download the excel
          download_button <- remDr$findElement(using = "xpath", value = '//*[@id="btnExcel"]')
          download_button$clickElement()
          
          #Wait for download to finish
          Sys.sleep(20)
          
          #Create a dataframe for the dropdown menu options used to download the file 
          result <- data.frame(State = state, District = district, Block = block, Panchayat = panchayat, Year = year, stringsAsFactors = FALSE)
          
          #Set folder paths
          source_folder <- "/Users/ganeshgorti/Downloads/mgnrega_sikkim"
          destination_folder <- "/Users/ganeshgorti/Downloads/mgnrega_sikkim_new"
          
          #Set the new file name
          new_name <- paste0(result$State,"_",result$District,"_",result$Block,"_",result$Panchayat,"_",result$Year,".xls")
          
          #Copy the latest file to the destination folder
          files <- list.files(source_folder, full.names = TRUE)
          if (length(files) > 0) {
            files_info <- file.info(files)
            files_sorted <- files[order(files_info$mtime, decreasing = TRUE)]
            latest_file <- files_sorted[1]
            file.copy(latest_file, destination_folder)
            
            #Check if the copied file already has the new name
            new_file_path <- file.path(destination_folder, new_name)
            if (file.exists(new_file_path)) {
              file_error_combinations <- rbind(file_error_combinations, data.frame(State = state, District = district, Block = block, Panchayat = panchayat, Year = year, stringsAsFactors = FALSE))
              message("The file already exists. Combination (", state, "_", district, "_", block, "_", panchayat, "_", year, ") was not created.")
            } else {
              file.rename(file.path(destination_folder, basename(latest_file)), new_file_path)
              message("File successfully renamed.")
            }
          } else {
            message("No files found in the source folder.")
          }
          #Creating dataframe for capturing scraped combination 
          scraped_combinations <- rbind(scraped_combinations, data.frame(State = state, District = district, Block = block, Panchayat = panchayat, Year = year, stringsAsFactors = FALSE))
          
          #Writing dataframes (scraped combinations and errors in table and 100 entries to csv)
          write.csv(scraped_combinations, "/Users/ganeshgorti/Downloads/mgnrega_sikkim_new/scraped_combinations_sikkim_mangan.csv", row.names=FALSE)
          write.csv(error_combinations_table, "/Users/ganeshgorti/Downloads/mgnrega_sikkim_new/error_combinations_table_sikkim_mangan.csv", row.names=FALSE)
          write.csv(error_combinations_entries, "/Users/ganeshgorti/Downloads/mgnrega_sikkim_new/error_combinations_entries_sikkim_mangan.csv", row.names=FALSE)
          write.csv(files_info, "/Users/ganeshgorti/Downloads/mgnrega_sikkim_new/files_info__sikkim.csv", row.names=FALSE)
          write.csv(file_error_combinations, "/Users/ganeshgorti/Downloads/mgnrega_sikkim_new/files_error_combinations_sikkim.csv", row.names=FALSE)
        }
      }
    }
  }
}

#Quit the browser and close the Selenium server
remDr$close()
rD$server$stop()


#####################################
#***********************************#
#####################################


###############
#OLD CODE FOR SUBMIT BUTTON -> This is not working since errors are not being captured in the dataframe 
###############

#Submit the options 
#Using retrying since the division does not seem to be loading often
# submit_button <- remDr$findElement(using = "xpath", value = '//*[@id="ContentPlaceHolder1_btnSubmit"]')
# element_loaded <- FALSE
# retry_count <- 3
# for (i in seq_len(retry_count)) {
#   tryCatch({
#     submit_button$clickElement()
#     Sys.sleep(20)
#     wait_for_element <- remDr$findElement(using = "xpath", "//div[@id='dtBasicExample_wrapper']")
#     element_loaded <- TRUE
#   }, error = function(e) {
#     if (i == retry_count) {
#       #If all retries failed, record the combination that threw the error
#       error_combinations_table <- rbind(error_combinations_table, data.frame(State = state, District = district, Block = block, Panchayat = panchayat, Year = year, stringsAsFactors = FALSE))
#       message(paste0("Combination (", state, ", ", district, ", ", block, ", ", panchayat, ", ", year, ") threw an error while loading the table. Moving on to the next combination."))
#     } else {
#       message(paste0("Attempt ", i, " failed, retrying in 20 seconds..."))
#       Sys.sleep(20) #Wait for 20 seconds before retrying
#     }
#   })
#   if (element_loaded) {
#     break #Exit the loop if element is loaded
#   }
# }

######
#OLD CODE FOR ERROR HANDLING IN NUMBER OF ENTRIES, does not work since break is withing tryCatch
#####
# 
# retry_count <- 3
# for (i in seq_len(retry_count)) {
#   tryCatch({
#     webElem_showentries <- remDr$findElement(using = 'xpath', "//select[@name = 'dtBasicExample_length']/option[@value='100']")
#     webElem_showentries$clickElement()
#     break # exit the loop if successful
#   }, error = function(e) {
#     if (i == retry_count) {
#       stop(e) # re-throw the exception if all retries failed
#     } else {
#       message(paste0("Attempt ", i, " failed, retrying in 20 seconds..."))
#       Sys.sleep(20) # wait for 20 seconds before retrying
#     }
#   })
# }
