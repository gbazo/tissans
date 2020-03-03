fetch_tiss_ans <- function(year_start, year_end, uf="all", system_op, vars=NULL){

  # Database who will access
  tp_data <- c("A", "H", "D", "DP")
  if(!(system_op %in% tp_data)) stop("Database unknown.")

  # Check dates
  if(year_start > year_end) stop("Start date must be before end date.")

  if(system_op == "A" | system_op == "H"){
    dates <- as.character(year_start:year_end)
  }

  # Check UF
  ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
  if(!all((uf %in% c("all",ufs)))) stop("UF unknow.")

  if(uf != ufs){
    ufs <- uf

    # Create files list for download
    if(system_op == "A") {
      url <- "ftp://ftp.dadosabertos.ans.gov.br/FTP/PDA/TISS/AMBULATORIAL/"
      for (j in ufs) {
        for(i in dates){
          files_list <- paste0(url, i, "/", j, ".zip")

          temp <- tempfile()
          temp2 <- tempfile()

          tryCatch({
            download.file(files_list, temp, mode = "wb")
            partial <- unzip(temp, exdir = temp2)
          },
          error=function(cond) {
            message(paste("Something went wrong with this URL:", file))
            message("This can be a problem with the Internet or the file does not exist yet.")
          })

          # Merge files
          files  <- list.files(temp2, pattern = '\\_DET.csv', full.names = T)
          files2  <- list.files(temp2, pattern = '\\_CONS.csv', full.names = T)

          tables <- lapply(files, read.csv, header = TRUE)
          tables2 <- lapply(files2, read.csv, header = TRUE)

          data_det <- do.call(rbind, tables)
          data_cons <- do.call(rbind, tables2)

          write.csv(data_det, file = paste0("./", j, "_DET", ".csv"), row.names=F)
          write.csv(data_cons, file = paste0("./", j, "_CONS", ".csv"), row.names=F)
        }
      }
    } else if (system_op == "H") {
      url <- "ftp://ftp.dadosabertos.ans.gov.br/FTP/PDA/TISS/HOSPITALAR/"
      for (j in uf) {
        for(i in dates){
          files_list <- paste0(url, i, "/", j, ".zip")

          temp <- tempfile()
          temp2 <- tempfile()

          tryCatch({
            download.file(files_list, temp, mode = "wb")
            #partial <- unzip(temp)
            partial <- unzip(temp, exdir = temp2)
          },
          error=function(cond) {
            message(paste("Something went wrong with this URL:", file))
            message("This can be a problem with the Internet or the file does not exist yet.")
          })

          # Merge files
          files  <- list.files(temp2, pattern = '\\_DET.csv', full.names = T)
          files2  <- list.files(temp2, pattern = '\\_CONS.csv', full.names = T)

          tables <- lapply(files, read.csv, header = TRUE)
          tables2 <- lapply(files2, read.csv, header = TRUE)

          data_det <- do.call(rbind, tables)
          data_cons <- do.call(rbind, tables2)

          write.csv(data_det, file = paste0("./home/", j, "_DET", ".csv"), row.names=F)
          write.csv(data_cons, file = paste0("./home/", j, "_CONS", ".csv"), row.names=F)
        }
      }
    } else if (system_op == "D") {
      url <- "ftp://ftp.dadosabertos.ans.gov.br/FTP/PDA/TISS/DICIONARIO/Dicionario_de_variaveis.ods"
      files_list <- url

      temp <- tempfile()

      tryCatch({
        download.file(files_list, temp, mode = "wb")
        data_ods <- readODS::read_ods(temp)
        write.csv(data_ods, file = "DIC_VARS.csv", row.names=F)
      })

    } else if (system_op == "DP") {
      url <- "ftp://ftp.dadosabertos.ans.gov.br/FTP/PDA/TISS/DADOS_DE_PLANOS/DADOS_PLANOS_SAUDE.csv"
      files_list <- url

      temp <- tempfile()

      tryCatch({
        download.file(files_list, temp, mode = "wb")
        write.csv(data_cons, file = "DADOS_PLANO_SAUDE.csv", row.names=F)
      })
    }
  }

# Return
return("Ok. Verify your personal folder.")
}
