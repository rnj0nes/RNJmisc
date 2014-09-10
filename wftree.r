# create directory tree for workflow
# Rich Jones (rich_jones@brown.edu)
# June 3, 2009 (ado)
# September 10, 2014 (r)
#
#   USAGE:
#
#    # creates a workflow directory (folder) tree
#    > wftree("foo")    
#
#    # creates a workflow directory (folder) tree with individual private folders
#    > wftree("foo" , private = c("RNJ", "DT", "CK", "TGT", "ALG" ))
#
#  Ends you up in the ../posted/analysis subfolder
#
#  If you run it and the folders already exist, no error message
#  but existing folders will not be overwritten.
#
#  This means if the project folder (mnemonic) already exists but
#  not all of the specific subfolders have been created, this function
#  will fill in the missing folders and insert the (mostly) null
#  files that identify the project.
#
#
wftree <- function(x, private = NULL) {
        # force the value provided, the mnemonic for the
        # workflow folder, t be uppercase and
        # to be a string
        m <- toupper(toString(x)) # the project mnemonic
        # This is the list of standard working folders
        # Following the IFAR/QSP interpretation of JS Long's 
        # workflow
        folders <- c("- HOLD THEN DELETE",
                "- TO FILE",
                "ADMIN", 
                "ADMIN/BUDGET" , 
                "ADMIN/CORRESPONDENCE" ,
                "ADMIN/IRB", 
                "ADMIN/PROPOSAL", 
                "DOCUMENTATION", 
                "DOCUMENTATION/CODEBOOK", 
                "MAILBOX", 
                "POSTED", 
                "POSTED/ANALYSIS",
                "POSTED/DATA",
                "POSTED/DATA/DERIVED", 
                "POSTED/DATA/SOURCE",
                "POSTED/DESCSTATS",
                "POSTED/FIGURES", 
                "POSTED/TEXT", 
                "PRIVATE")
        # pwd holds the current working directory, the
        # directory from which the wftree function was
        # called. The user might want to 
        # > setwd("c:/work")
        # or something like it before calling wftree
        pwd <- getwd()
        # dir.create will not stop if the folder already exists
        # and will not overwrite if the folder already exists
        dir.create(file.path(pwd, m), showWarnings = FALSE)
        setwd(file.path(pwd, m))
        # Save a file that has the name "- MNEMONIC STUDY"
        # in the main (and each subfolder, later)
        m <- "FOO"
        file.name <- paste("-", m , "STUDY")
        write.table(file.name , file = file.name , row.names = FALSE, col.names = FALSE, sep = "")
        pwdm <- getwd()
        l <- length(folders)
        for (i in 1:l) {
                setwd(file.path(pwd, m))
                subf <- folders[i]
                dir.create(file.path(pwdm, subf), showWarnings = FALSE)
                setwd(file.path(pwdm, subf))
                write.table(file.name , file = file.name , row.names = FALSE, col.names = FALSE, sep = "")
        }
        # Handle private folders
        setwd(file.path(pwdm,"PRIVATE"))
        pwdmp <- getwd()
        if (!is.null(private)) {
                p <- length(private)
                for (i in 1:p) {
                        setwd(file.path(pwdmp))
                        subf <- private[i]
                        dir.create(file.path(pwdmp, subf), showWarnings = FALSE)
                        setwd(file.path(pwdmp, subf))
                        write.table(file.name , file = file.name , row.names = FALSE, col.names = FALSE, sep = "")
                }
        }
        # End by leaving user in posted/analysis
        setwd(file.path(pwdm,"POSTED/ANALYSIS"))
        pwd <- getwd()
        message(paste("Current working folder is", pwd))
}

