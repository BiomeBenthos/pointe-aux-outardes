#' Fonction pour remplir les NAs
#' 
#' @export

mat_fill_na <- function(mat) {

  mat[lower.tri(mat) & is.na(mat)] <- 100
  
  return(mat)
  
}


files <- list.files("output/2023_v2")

for(i in files) {

  tmp <- readxl::read_xlsx(sprintf("output/2023_v2/%s",i)) |> as.data.frame()

  row.names(tmp) <- tmp[,1]
  tmp <- tmp[,-1]

  tmp <- mat_fill_na(tmp)

  nm_csv <- gsub("\\.xlsx", "\\.csv", i)

  write.csv(tmp, sprintf("output/2023_v2/%s", nm_csv))

}
