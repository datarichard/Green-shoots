`%notin%` <- Negate(`%in%`)

summarize <- function(.data, ..., .groups = "drop") {
  dplyr::summarise(.data, ..., .groups = .groups)
}

prettyDigits <- function(.x) {
  #' Prints numbers to a fixed digit length, e.g., 009
  sprintf("%03d", .x)
}

se <- function(x, na.rm=T) {
  sd(x, na.rm) / sqrt(sum(!is.na(x))-1)
}
  
  
Mode <- function(x, na.rm=F) {
  if(na.rm) {x <- na.omit(x)}
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
  
extract_numeric <- function(x) {
  #' Extract numeric component of variable.
  #'
  #' This uses a regular expression to strip all non-numeric character from
  #' a string and then coerces the result to a number. This is useful for
  #' strings that are numbers with extra formatting (e.g. $1,200.34).
  #'
  #' @param x A character vector (or a factor).
  #' @export
  #' @examples
  #' extract_numeric("$1,200.34")
  #' extract_numeric("-2%")
  #'
  #' # The heuristic is not perfect - it won't fail for things that
  #' # clearly aren't numbers
  #' extract_numeric("-2-2")
  #' extract_numeric("12abc34")
  as.numeric(gsub("[^0-9.-]+", "", as.character(x)))
}

roundf <- function(.x, digits = 2) {
  #' Rounds a value to a fixed number of decimal places
  #' (Useful) for table
  #' e.g., sprintf("%.2f", .x)
  format(round(.x, digits = digits), nsmall = digits)
  
}

percent <- function(data, ..., wt = NULL) {
  #' My cute function to print percentages instead of counts
  #' This uses a grouping variable and then applies `count` 
  #'
  #' @param data A data frame (possibly grouped).
  #' @export
  #' @examples
  #' 
  
  grouping_vars_expr <- quos(...)
  grouping_var <- NULL
  
  if (length(grouping_vars_expr) > 1) {
    grouping_var <- grouping_vars_expr[-length(grouping_vars_expr)]
  }
  
  data %>%
    count(!!! grouping_vars_expr, wt = {{wt}}) %>%
    group_by(!!! grouping_var) %>%
    mutate(proportion = round((n) / sum(n), 3)) 
}

critical_r <- function(n, p = .05) {
  
  # e.g., critical_r(n = 50, p = .05)
  
  critical_t <- qt(p = p/2, df = n-2, lower.tail=F)
  
  t2r <- function(t, df) {
    sqrt(t^2 / (t^2 + df))
  }
  
  return(t2r(critical_t, n-2))
}

# summarise mean and ci
prop_ci <- function(.x, .n = NULL, rowwise=F) {
  
  if (rowwise) {
    
    prop.test(x = .x, n = .n) |> 
      broom::tidy() |> 
      select(y = estimate, ymin = conf.low, ymax = conf.high)
    
  } else {
  # must pass a numeric vector!
  
  .x <- stats::na.omit(.x)
  x = sum(.x)
  n = sum(!is.na(.x))
  
  prop.test(x = x, n = n) |> 
    broom::tidy() |> 
    select(y = estimate, ymin = conf.low, ymax = conf.high) |> 
    # mutate(across(everything(), ~. *100)) %>%
    mutate(n = n)
  }
}
# e.g., yvar must be a numeric vector of 0s and 1s
# ggplot(df, (aes(x = xvar, y = yvar))) +
#   stat_summary(fun.data = prop_ci, geom = "pointrange")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", formula = "y ~ x", 
              method.args = list(family = "binomial"), ...)
}
# To fit a logistic regression, you need to coerce the values to
# a numeric vector lying between 0 and 1.
# ggplot(rpart::kyphosis, aes(Age, Kyphosis)) +
#   geom_jitter(height = 0.05) +
#   binomial_smooth()

pvalue_format <- function(x){
  z <- cut(x, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), 
           labels = c("***", "**", "*", ""))
  as.character(z)
}
# Needed to add a column of stars to a results table, e.g.,
# mutate(` ` = pvalue_format(`p.value`)) %>% 
# flextable() |> 
#   ...
#   align(j = " ", align = "left") %>% 
#   padding(padding.right = 0, j = "p value", part  = "all") %>% 
#   bold(j = " ", bold = TRUE) %>% 
#   padding(padding.left = 0, j = " ", part  = "all") 







