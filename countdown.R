#!/usr/bin/Rscript
due <- as.Date("2014-02-17")
if ((due-Sys.Date()) > 1) {
  cat(sprintf("%d days until book is due.\n", due-Sys.Date()))
} else {
  cat("You *are* done, right?\n")
}
