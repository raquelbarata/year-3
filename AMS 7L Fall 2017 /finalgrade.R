# get the course_grade csv file from ecommons
coursegrade <- read.csv('course_grade-Gradebook.csv', stringsAsFactors = FALSE)
colnames(coursegrade)
dim(coursegrade)

name <- coursegrade[, 1]
name <- gsub(", ", ",", name, fixed = TRUE)
# change classnumber 
# classnumber <- c("22602", "22603", "22604", "24503", "24504")

classnumber <- c("70466")

# change section
# Section <- paste(c('AMS7L04', 'AMS7L05', 'AMS7L06', 'AMS7L09', 'AMS7L10'), 
#                  ".csv", sep = "") ## download from MyUCSC Grade roster
Section <- paste(c('AMS7L'), ".csv", sep = "")  

for (s in 1:length(Section)) {
    section <- read.csv(Section[s], stringsAsFactors = FALSE)
    section <- section[, c(1, 2, 5, 6, 7)]
    colnames(section) <- c("id", "Name", "Grade", "GradeBasis", "Level")
    if("W" %in% section[, "Grade"]) {
        section <- section[c(section[, "Grade"] != "W"), ]
    }
    for (i in 1:length(name)) {
        if (sum(grepl(name[i], section[, "Name"])) == 1) {
            idx = which(grepl(name[i], section[, "Name"]) == TRUE)
            section[, "Grade"][idx] = coursegrade[, "Course.Grade"][i]
        }
    }
    # pnp <- section[section$GradeBasis == "PNP", ]
    section[section$GradeBasis == "PNP", ]$Grade <- 
        ifelse(pnp$Grade %in% c("A", "B", "C"), "P", "NP") 
    # section[, "Name"] <- gsub(" ", "", section[, "Name"], fixed = TRUE)
    write.csv(section, paste(classnumber[s], Section[s], sep=""), 
              row.names = FALSE, quote = TRUE, na = "")
}

