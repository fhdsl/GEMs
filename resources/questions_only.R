library(stringr)

files <- list.files("module/")
module_files <- files[str_detect(files, "student_guide.qmd")]

make_questions_file <- function(qmdfile){
  # Establish file names
  input_file <- paste0(here::here(), "/module/", qmdfile)
  output_file <- paste0(here::here(), "/module/", str_replace(qmdfile, "student_guide", "questions_only"))

  # Get lines
  qmd_lines <- readLines(input_file, warn = FALSE)
  
  # -----------------------------
  # Get yaml
  # -----------------------------
  
  yaml_lines <- which(qmd_lines == "---")
  if (length(yaml_lines) == 2) {
    yaml_chunk <- c(qmd_lines[yaml_lines[1]:yaml_lines[2]], "")
  } else
    (yaml_chunk <- "")
  
  # -----------------------------
  # Get question chunks
  # -----------------------------
  
  chunks <- c("")
  chunk_start_lines <- which(qmd_lines == "::: {.callout-note}" | qmd_lines == "::: callout-note" )
  for(chunk_start in chunk_start_lines){
    print(chunk_start)
    sub_qmd <- qmd_lines[chunk_start:length(qmd_lines)]
    chunk_size <- which(sub_qmd == ":::")[1]
    chunks <- c(chunks, sub_qmd[1:chunk_size], "")
  }
  
  # -----------------------------
  # Write the cleaned content
  # -----------------------------
  
  # Combine yaml chunk and question chunks
  writeLines(c(yaml_chunk, chunks), output_file)
  message("Questions only file written to: ", output_file)
}

sapply(module_files, make_questions_file)


