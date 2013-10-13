#!/usr/bin/Rscript
due <- as.Date("2013-11-12")
if ((due-Sys.Date()) > 1) {
  cat(sprintf("%d days until book is due.\n", due-Sys.Date()))
} else {
  cat("You *are* done, right?\n")
}