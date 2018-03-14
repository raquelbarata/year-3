dat <- read.csv('gradebook-Gradebook.csv')
dim(dat)

# Lab 1 to 9
lab_out <- "Lab.10" # Keep this untouched
colnm <- colnames(dat)
for (i in 1:9) {
    lab_in <- paste0("Lab.", i)
    idx <- (1:length(colnm))[sapply(colnm, FUN = function(x) 
        !is.na(grep(lab_in, x)[1]) & is.na(grep(lab_out, x)[1]))]
    comp0 <- rowMeans(!is.na(dat[, idx[-c(1:2)]]))
    comp <- (comp0 >= 0.5) * 30 + (comp0 == 1) * 30
    dat[, idx[1]] <- comp
    dat[, idx[2]] <- rowSums(dat[, idx[-2]], na.rm = TRUE)
}

### Lab 10
idx <- (1:length(colnm))[sapply(colnm, FUN = function(x) !is.na(grep("Lab.10", x)[1]))]
comp0 <- rowMeans(!is.na(dat[, idx[-c(1:2)]]))
comp <- (comp0 >= 0.5) * 30 + (comp0 == 1) * 30
dat[, idx[1]] <- comp
dat[, idx[2]] <- rowSums(dat[, idx[-2]], na.rm = TRUE)

dat$Student.Name = sapply(dat$Student.Name, FUN = function(x) paste('"', x, '"', sep = ""))

dat[is.na(dat)] <- 0
dat.total <- dat[, grep("Total", colnm)]
dat.min <- apply(dat.total, 1, min)
bonus = 1
dat[, grep("Final", colnm)] <- (rowSums(dat.total) - dat.min) / 9 + bonus
write.csv(dat, 'gradebook_complete.csv', row.names = FALSE, quote = FALSE, 
          na = "")
