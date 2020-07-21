eq_assignment_lint = function(source_file, assignment_token, message) {
  lapply(lintr:::ids_with_token(source_file, assignment_token), function(id) {
    parsed = lintr:::with_id(source_file, id)

    lintr::Lint(filename = source_file$filename, line_number = parsed$line1,
                column_number = parsed$col1, type = "style", message = message,
                line = source_file$lines[as.character(parsed$line1)],
                linter = "assignment_linter")
  })
}

new_assignment_linter = function(source_file) {
  left_assign_lints = eq_assignment_lint(source_file, "LEFT_ASSIGN", "Use =, not <-, for assignment.")
  right_assign_lints = eq_assignment_lint(source_file, "RIGHT_ASSIGN", "Use =, not ->, for assignment.")

  return(c(left_assign_lints, right_assign_lints))
}