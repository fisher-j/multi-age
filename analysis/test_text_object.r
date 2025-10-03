# Given a glmmTMB model and one of "cond", "disp", or "zi", print model formula
# optionally including labels and link functions
format_model_formula <- function(mod, component, label = FALSE) {
  comp_label <- switch(
    component,
    cond = c("Conditional:", ""),
    disp = c("Dispersion:", "(log)"),
    zi = c("Hurdle:", "(logit)")
  )
  form <- format(formula(mod, component = component)) |>
    paste0(collapse = "") |>
    str_replace("\\s{2,}", " ")
  if (label) {
    return(paste(comp_label[1], form, comp_label[2]))
  }
  form
}
