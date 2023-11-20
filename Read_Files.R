paths_content <- readLines("File/file_paths.txt")
Asset_line <- grep("^Asset", paths_content)
Asset_value <- sub("^Asset=\\s*", "", paths_content[Asset_line])
bcc_Raw <- read_excel(Asset_value, sheet = "ASSET")
