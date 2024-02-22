saveModelToFile <- function(model, filepath) {
  tryCatch({
    saveRDS(model, file = filepath)
    cat("Model successfully saved to", filepath, "\n")
  }, error = function(e) {
    cat("Error saving model:", e$message, "\n")
  })
}
