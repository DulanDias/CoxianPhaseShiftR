#' @export
loadModelFromFile <- function(filepath) {
  tryCatch({
    model <- readRDS(file = filepath)
    cat("Model successfully loaded from", filepath, "\n")
    return(model)
  }, error = function(e) {
    cat("Error loading model:", e$message, "\n")
    return(NULL) # Return NULL if there was an error
  })
}
