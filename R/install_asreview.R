#' install_asreview
#'
#' installs asreview

install_asreview <- function(){
  test<-(system("pip --version", intern = TRUE))
  if(!grepl("python", test)){
    stop("please install pip on your system")
  }
  system("pip install asreview")
}

#' update_asreview
#'
#' update asreview

update_asreview <- function(){
  test<-(system("pip --version", intern = TRUE))
  if(!grepl("python", test)){
    stop("please install pip on your system")
  }
  system("pip install --upgrade asreview")
}
