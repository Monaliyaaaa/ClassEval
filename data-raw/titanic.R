titanic <- as.data.frame(Titanic)

titanic_expanded <- tidyr::uncount(titanic, Freq)
titanic_expanded$Survived <- ifelse(titanic_expanded$Survived == "Yes", 1, 0)

usethis::use_data(titanic_expanded, overwrite = TRUE)
