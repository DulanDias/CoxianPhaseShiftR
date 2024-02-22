#' Load Model from File
#'
#' This function loads a model object from a specified file path. It is designed to work with models
#' that have been saved using `saveRDS()`. This is useful for scenarios where you need to persist
#' model objects on disk for later use, such as for prediction or further analysis.
#'
#' @param filepath A string specifying the path to the file where the model object is saved. The file
#' should have been created using `saveRDS()`.
#'
#' @return The model object loaded from the specified file. If an error occurs during loading, `NULL`
#' is returned and an error message is printed to the console.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have previously saved a model to "model.rds"
#'   model_path <- "path/to/your_saved_model.rds"
#'   loaded_model <- loadModelFromFile(model_path)
#'   # Now `loaded_model` can be used for predictions or further analysis
#' }
#'
#' @export
saveModelToFile <- function(model, filepath) {
  tryCatch({
    saveRDS(model, file = filepath)
    cat("Model successfully saved to", filepath, "\n")
  }, error = function(e) {
    cat("Error saving model:", e$message, "\n")
  })
}
