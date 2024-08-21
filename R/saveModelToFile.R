#' Save Model to File
#'
#' This function saves a model object to a specified file path using `saveRDS()`. This is useful for
#' scenarios where you need to persist model objects on disk for later use, such as for prediction or further analysis.
#'
#' @param model The model object to be saved.
#' @param filepath A string specifying the path to the file where the model object should be saved. The file
#' will be created or overwritten using `saveRDS()`.
#'
#' @return NULL. The function is called for its side effects (saving the model to disk). If an error occurs during saving, an error message is printed to the console.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have a model object `my_model`
#'   model_path <- "path/to/your_saved_model.rds"
#'   saveModelToFile(my_model, model_path)
#'   # The model is now saved to the specified file path
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
