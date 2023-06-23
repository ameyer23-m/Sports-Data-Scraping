library(rvest)
library(tidyverse)
library(readr)
library(dplyr)
```
## Goalies Data

### Overall Statistics

# Function to extract goalie table from a URL and add the school name
extract_goalie_table <- function(url, school_name) {
  webpage <- read_html(url)
  goalie_table <- webpage %>% 
    html_nodes("table") %>% 
    .[[5]] %>% 
    html_table(fill = TRUE)
  goalie_table$Name <- gsub("\n", " ", goalie_table$Name)
  goalie_table$Name <- gsub("\\s+", " ", goalie_table$Name)
  goalie_table$Name[goalie_table$Name == "ThÃ©o Weidinger"] <- "Théo Weidinger"
  goalie_table <- goalie_table[complete.cases(goalie_table$No.), ]
  goalie_table$School <- school_name  # Add the school name column
  goalie_table <- subset(goalie_table, select = -c(`Yr`, `Pos`, `tie`))
  return(goalie_table)
}

# URLs and school names
urls <- c(
  "https://www.woosterathletics.com/sports/mlax/2022-23/teams/wooster?view=lineup&r=0&pos=",
  "https://www.wittenbergtigers.com/sports/mlax/2022-23/teams/wittenberg?view=lineup&r=0&pos="
)
school_names <- c("Wooster", "Wittenberg")

# Extract goalie tables and add the school name
goalie_tables <- Map(extract_goalie_table, urls, school_names)

# Combine all goalie tables
ncac_goalie_table1 <- bind_rows(goalie_tables)

# second HTML type
extract_goalie_table2 <- function(url, school_name) {
  webpage <- read_html(url)
  goalie_table <- webpage %>% 
    html_nodes("table") %>% 
    .[[3]] %>% 
    html_table(fill = TRUE)
  
  split_names <- strsplit(goalie_table$Player, ", ")
  fixed_names <- sapply(split_names, function(names) names[2])
  goalie_table$Player <- fixed_names
  
  
  goalie_table$Name <- gsub("\n", " ", goalie_table$Player)
  goalie_table$Name <- gsub("\\s+", " ", goalie_table$Player)
  
  goalie_table <- goalie_table[complete.cases(goalie_table$"#"), ]
  
  goalie_table <- subset(goalie_table, select = -c(`Bio Link`, `Player`, `SF`))
  
  colnames(goalie_table) <- tolower(colnames(goalie_table))
  names(goalie_table)[names(goalie_table) == "#"] <- "No."
  names(goalie_table)[names(goalie_table) == "name"] <- "Name"
  names(goalie_table)[names(goalie_table) == "w"] <- "win"
  names(goalie_table)[names(goalie_table) == "l"] <- "loss"
  
  goalie_table <- goalie_table %>%
    separate(`gp-gs`, into = c("gp", "gs"), sep = "-")
  
  goalie_table$gp <- as.numeric(goalie_table$gp)
  goalie_table$gs <- as.numeric(goalie_table$gs)
  
  goalie_table$`win %` <- (as.numeric(goalie_table$win) / (as.numeric(goalie_table$win) + as.numeric(goalie_table$loss))) * 100
  goalie_table$`win %` <- sprintf("%.1f", goalie_table$`win %`)
  
  goalie_table$`sv%` <- goalie_table$`sv%` * 100
  
  goalie_table$School <- school_name 
  return(goalie_table)
}


urls2 <- c(
  "https://denisonbigred.com/sports/mens-lacrosse/stats",
  "https://sports.wabash.edu/sports/mens-lacrosse/stats/2023",
  "https://hiramterriers.com/sports/mlax/stats#individual",
  "https://athletics.kenyon.edu/sports/mens-lacrosse/stats/2023#individual",
  "https://goyeo.com/sports/mens-lacrosse/stats/2023#individual",
  "https://depauwtigers.com/sports/mens-lacrosse/stats",
  "https://battlingbishops.com/sports/mens-lacrosse/stats/2023#individual"
)
school_names2 <- c("Denison", "Wabash", "Hiram", "Kenyon", "Oberlin", "Depauw", "Ohio Wesleyan")

# Extract goalie tables and add the school name
goalie_tables <- Map(extract_goalie_table2, urls2, school_names2)

# Combine all goalie tables
ncac_goalie_table2 <- bind_rows(goalie_tables)

ncac_goalie_table_overall <- merge(ncac_goalie_table2, ncac_goalie_table1, all = TRUE)

file_path <- "your diectory/ncac_goalie_table_overall.csv"

write.csv(ncac_goalie_table_overall, file = file_path, row.names = FALSE)


### Conference Stats


# Function to extract goalie table from a URL and add the school name
extract_goalie_table <- function(url, school_name) {
  webpage <- read_html(url)
  goalie_table <- webpage %>% 
    html_nodes("table") %>% 
    .[[7]] %>% 
    html_table(fill = TRUE)
  goalie_table$Name <- gsub("\n", " ", goalie_table$Name)
  goalie_table$Name <- gsub("\\s+", " ", goalie_table$Name)
  goalie_table$Name[goalie_table$Name == "ThÃ©o Weidinger"] <- "Théo Weidinger"
  goalie_table <- goalie_table[complete.cases(goalie_table$No.), ]
  goalie_table$School <- school_name  # Add the school name column
  goalie_table <- subset(goalie_table, select = -c(`Yr`, `Pos`, `tie`))
  return(goalie_table)
}

# URLs and school names
urls <- c(
  "https://www.woosterathletics.com/sports/mlax/2022-23/teams/wooster?view=lineup&r=0&pos=",
  "https://www.wittenbergtigers.com/sports/mlax/2022-23/teams/wittenberg?view=lineup&r=0&pos="
)
school_names <- c("Wooster", "Wittenberg")

# Extract goalie tables and add the school name
goalie_tables <- Map(extract_goalie_table, urls, school_names)

# Combine all goalie tables
ncac_goalie_table1 <- bind_rows(goalie_tables)

extract_goalie_table2 <- function(url, school_name) {
  webpage <- read_html(url)
  goalie_table <- webpage %>% 
    html_nodes("table") %>% 
    .[[5]] %>% 
    html_table(fill = TRUE)
  
  split_names <- strsplit(goalie_table$Player, ", ")
  fixed_names <- sapply(split_names, function(names) names[2])
  goalie_table$Player <- fixed_names
  
  
  goalie_table$Name <- gsub("\n", " ", goalie_table$Player)
  goalie_table$Name <- gsub("\\s+", " ", goalie_table$Player)
  
  goalie_table <- goalie_table[complete.cases(goalie_table$"#"), ]
  
  goalie_table <- subset(goalie_table, select = -c(`Bio Link`, `Player`, `SF`))
  
  colnames(goalie_table) <- tolower(colnames(goalie_table))
  names(goalie_table)[names(goalie_table) == "#"] <- "No."
  names(goalie_table)[names(goalie_table) == "name"] <- "Name"
  names(goalie_table)[names(goalie_table) == "w"] <- "win"
  names(goalie_table)[names(goalie_table) == "l"] <- "loss"
  
  goalie_table <- goalie_table %>%
    separate(`gp-gs`, into = c("gp", "gs"), sep = "-")
  
  goalie_table$gp <- as.numeric(goalie_table$gp)
  goalie_table$gs <- as.numeric(goalie_table$gs)
  
  goalie_table$`win %` <- (as.numeric(goalie_table$win) / (as.numeric(goalie_table$win) + as.numeric(goalie_table$loss))) * 100
  goalie_table$`win %` <- sprintf("%.1f", goalie_table$`win %`)
  
  goalie_table$`sv%` <- goalie_table$`sv%` * 100
  
  goalie_table$School <- school_name 
  return(goalie_table)
}


urls2 <- c(
  "https://denisonbigred.com/sports/mens-lacrosse/stats",
  "https://sports.wabash.edu/sports/mens-lacrosse/stats/2023",
  "https://hiramterriers.com/sports/mlax/stats#individual",
  "https://athletics.kenyon.edu/sports/mens-lacrosse/stats/2023#individual",
  "https://goyeo.com/sports/mens-lacrosse/stats/2023#individual",
  "https://depauwtigers.com/sports/mens-lacrosse/stats",
  "https://battlingbishops.com/sports/mens-lacrosse/stats/2023#individual"
)
school_names2 <- c("Denison", "Wabash", "Hiram", "Kenyon", "Oberlin", "Depauw", "Ohio Wesleyan")

# Extract goalie tables and add the school name
goalie_tables <- Map(extract_goalie_table2, urls2, school_names2)

# Combine all goalie tables
ncac_goalie_table2 <- bind_rows(goalie_tables)

ncac_goalie_table_conference <- merge(ncac_goalie_table2, ncac_goalie_table1, all = TRUE)

file_path <- "your diectory/ncac_goalie_table_conference.csv"

write.csv(ncac_goalie_table_conference, file = file_path, row.names = FALSE)




## field Players Data

### Overall Stats

extract_field_players_table <- function(url, school_name) {
  webpage <- read_html(url)
  field_players_table <- webpage %>%
    html_nodes("table") %>%
    .[[4]] %>%
    html_table(fill = TRUE)
  field_players_table$Name <- gsub("\n", " ", field_players_table$Name)
  field_players_table$Name <- gsub("\\s+", " ", field_players_table$Name)
  field_players_table$Name[field_players_table$Name == "ThÃ©o Weidinger"] <- "Théo Weidinger"
  field_players_table <- field_players_table[complete.cases(field_players_table$No.), ]
  
  field_players_table$fo <- as.character(field_players_table$fo)
  
  field_players_table <- subset(field_players_table, select = -c(`Yr`, `Pos`))
  
  fo_ratio <- strsplit(field_players_table$fo, "-")
  numerator <- as.numeric(sapply(fo_ratio, "[[", 1))
  denominator <- as.numeric(sapply(fo_ratio, "[[", 2))
  
  field_players_table$`fo%` <- numerator / denominator
  
  field_players_table$`fo%` <- round(field_players_table$`fo%`, 3)
  
  
  field_players_table$School <- school_name  # Add the school name column
  return(field_players_table)
}

urls <- c(
  "https://www.woosterathletics.com/sports/mlax/2022-23/teams/wooster?view=lineup&r=0&pos=",
  "https://www.wittenbergtigers.com/sports/mlax/2022-23/teams/wittenberg?view=lineup&r=0&pos="
)
school_names <- c("Wooster", "Wittenberg")

field_players_tables <- Map(extract_field_players_table, urls, school_names)

ncac_field_players_table1 <- bind_rows(field_players_tables)

# Second HTML Type

extract_field_players_table2 <- function(url, school_name) {
  webpage <- read_html(url)
  field_players_table <- webpage %>% 
    html_nodes("table") %>% 
    .[[2]] %>% 
    html_table(fill = TRUE)
  
  split_names <- strsplit(field_players_table$Player, ", ")
  fixed_names <- sapply(split_names, function(names) names[2])
  field_players_table$Player <- fixed_names
  
  
  field_players_table$Name <- gsub("\n", " ", field_players_table$Player)
  field_players_table$Name <- gsub("\\s+", " ", field_players_table$Player)
  
  field_players_table <- field_players_table[complete.cases(field_players_table$"#"), ]
  
  field_players_table <- subset(field_players_table, select = -c(`Bio Link`, `Player`, `GWG`, `PN-PIM`))
  
  colnames(field_players_table) <- tolower(colnames(field_players_table))
  names(field_players_table)[names(field_players_table) == "#"] <- "No."
  names(field_players_table)[names(field_players_table) == "dwn"] <- "dn"
  names(field_players_table)[names(field_players_table) == "name"] <- "Name"
  
  
  field_players_table <- field_players_table %>%
    separate(`gp-gs`, into = c("gp", "gs"), sep = "-")
  
  field_players_table$gp <- as.numeric(field_players_table$gp)
  field_players_table$gs <- as.numeric(field_players_table$gs)
  
  field_players_table$fo <- as.character(field_players_table$fo)
  
  fo_ratio <- strsplit(field_players_table$fo, "-")
  numerator <- as.numeric(sapply(fo_ratio, "[[", 1))
  denominator <- as.numeric(sapply(fo_ratio, "[[", 2))
  
  field_players_table$`fo%` <- numerator / denominator
  
  field_players_table$`fo%` <- round(field_players_table$`fo%`, 3)
  
  field_players_table <- field_players_table[!is.na(field_players_table$Name), ]
  
  
  field_players_table$School <- school_name 
  return(field_players_table)
}


urls2 <- c(
  "https://denisonbigred.com/sports/mens-lacrosse/stats",
  "https://sports.wabash.edu/sports/mens-lacrosse/stats/2023",
  "https://hiramterriers.com/sports/mlax/stats#individual",
  "https://athletics.kenyon.edu/sports/mens-lacrosse/stats/2023#individual",
  "https://goyeo.com/sports/mens-lacrosse/stats/2023#individual",
  "https://depauwtigers.com/sports/mens-lacrosse/stats",
  "https://battlingbishops.com/sports/mens-lacrosse/stats/2023#individual"
)
school_names2 <- c("Denison", "Wabash", "Hiram", "Kenyon", "Oberlin", "Depauw", "Ohio Wesleyan")

# Extract goalie tables and add the school name
field_players_tables <- Map(extract_field_players_table2, urls2, school_names2)

# Combine all goalie tables
ncac_field_players_table2 <- bind_rows(field_players_tables)

ncac_field_players_table_overall <- merge(ncac_field_players_table2, ncac_field_players_table1, all = TRUE)

file_path <- "your diectory/ncac_field_players_table_overall.csv"

write.csv(ncac_field_players_table_overall, file = file_path, row.names = FALSE)


### Conference Stats

extract_field_players_table <- function(url, school_name) {
  webpage <- read_html(url)
  field_players_table <- webpage %>%
    html_nodes("table") %>%
    .[[6]] %>%
    html_table(fill = TRUE)
  field_players_table$Name <- gsub("\n", " ", field_players_table$Name)
  field_players_table$Name <- gsub("\\s+", " ", field_players_table$Name)
  field_players_table$Name[field_players_table$Name == "ThÃ©o Weidinger"] <- "Théo Weidinger"
  field_players_table <- field_players_table[complete.cases(field_players_table$No.), ]
  
  field_players_table$fo <- as.character(field_players_table$fo)
  field_players_table$gp <- as.integer(field_players_table$gp)
  field_players_table$g <- as.integer(field_players_table$g)
  field_players_table$a <- as.integer(field_players_table$a)
  field_players_table$up <- as.integer(field_players_table$up)
  field_players_table$dn <- as.integer(field_players_table$dn)
  field_players_table$gb <- as.integer(field_players_table$gb)
  field_players_table$to <- as.integer(field_players_table$to)
  field_players_table$ct <- as.integer(field_players_table$ct)
  field_players_table$sh <- as.integer(field_players_table$sh)
  field_players_table$sog <- as.integer(field_players_table$sog)
  
  
  field_players_table <- subset(field_players_table, select = -c(`Yr`, `Pos`))
  
  fo_ratio <- strsplit(field_players_table$fo, "-")
  numerator <- as.numeric(sapply(fo_ratio, "[[", 1))
  denominator <- as.numeric(sapply(fo_ratio, "[[", 2))
  
  field_players_table$`fo%` <- numerator / denominator
  
  field_players_table$`fo%` <- round(field_players_table$`fo%`, 3)
  
  
  field_players_table$School <- school_name  # Add the school name column
  return(field_players_table)
}

urls <- c(
  "https://www.woosterathletics.com/sports/mlax/2022-23/teams/wooster?view=lineup&r=0&pos=",
  "https://www.wittenbergtigers.com/sports/mlax/2022-23/teams/wittenberg?view=lineup&r=0&pos="
)
school_names <- c("Wooster", "Wittenberg")

field_players_tables <- Map(extract_field_players_table, urls, school_names)

ncac_field_players_table1 <- bind_rows(field_players_tables)

extract_field_players_table2 <- function(url, school_name) {
  webpage <- read_html(url)
  field_players_table <- webpage %>% 
    html_nodes("table") %>% 
    .[[4]] %>% 
    html_table(fill = TRUE)
  
  split_names <- strsplit(field_players_table$Player, ", ")
  fixed_names <- sapply(split_names, function(names) names[2])
  field_players_table$Player <- fixed_names
  
  
  field_players_table$Name <- gsub("\n", " ", field_players_table$Player)
  field_players_table$Name <- gsub("\\s+", " ", field_players_table$Player)
  
  field_players_table <- field_players_table[complete.cases(field_players_table$"#"), ]
  
  field_players_table <- subset(field_players_table, select = -c(`Bio Link`, `Player`, `GWG`, `PN-PIM`))
  
  colnames(field_players_table) <- tolower(colnames(field_players_table))
  names(field_players_table)[names(field_players_table) == "#"] <- "No."
  names(field_players_table)[names(field_players_table) == "dwn"] <- "dn"
  names(field_players_table)[names(field_players_table) == "name"] <- "Name"
  
  
  field_players_table <- field_players_table %>%
    separate(`gp-gs`, into = c("gp", "gs"), sep = "-")
  
  field_players_table$gp <- as.numeric(field_players_table$gp)
  field_players_table$gs <- as.numeric(field_players_table$gs)
  
  field_players_table$fo <- as.character(field_players_table$fo)
  
  fo_ratio <- strsplit(field_players_table$fo, "-")
  numerator <- as.numeric(sapply(fo_ratio, "[[", 1))
  denominator <- as.numeric(sapply(fo_ratio, "[[", 2))
  
  field_players_table$`fo%` <- numerator / denominator
  
  field_players_table$`fo%` <- round(field_players_table$`fo%`, 3)
  
  field_players_table <- field_players_table[!is.na(field_players_table$Name), ]
  
  
  field_players_table$School <- school_name 
  return(field_players_table)
}


urls2 <- c(
  "https://denisonbigred.com/sports/mens-lacrosse/stats",
  "https://sports.wabash.edu/sports/mens-lacrosse/stats/2023",
  "https://hiramterriers.com/sports/mlax/stats#individual",
  "https://athletics.kenyon.edu/sports/mens-lacrosse/stats/2023#individual",
  "https://goyeo.com/sports/mens-lacrosse/stats/2023#individual",
  "https://depauwtigers.com/sports/mens-lacrosse/stats",
  "https://battlingbishops.com/sports/mens-lacrosse/stats/2023#individual"
)
school_names2 <- c("Denison", "Wabash", "Hiram", "Kenyon", "Oberlin", "Depauw", "Ohio Wesleyan")

# Extract goalie tables and add the school name
field_players_tables <- Map(extract_field_players_table2, urls2, school_names2)

# Combine all goalie tables
ncac_field_players_table2 <- bind_rows(field_players_tables)

ncac_field_players_table_conference <- merge(ncac_field_players_table2, ncac_field_players_table1, all = TRUE)

file_path <- "your diectory/ncac_field_players_table_conference.csv"

write.csv(ncac_field_players_table_conference, file = file_path, row.names = FALSE)
