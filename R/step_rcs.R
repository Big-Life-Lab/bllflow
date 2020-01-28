#' @export
step_rcs <- function(recipe,
                     ...,
                     role = 'predictor',
                     trained = FALSE,
                     skip = FALSE,
                     id = recipes::rand_id("RCS"),
                     number_of_knots = 3,
                     knots = NULL,
                     new_names = list()) {
  terms <- recipes::ellipse_check(...)
  recipes::add_step(
    recipe,
    step_rcs_new(
      terms = terms,
      trained = trained,
      role = role,
      append = append,
      id = id,
      skip = skip,
      number_of_knots = number_of_knots,
      knots = knots,
      new_names = new_names
    )
  )
}

step_rcs_new <-
  function(terms,
           role,
           trained,
           id,
           skip,
           number_of_knots,
           knots,
           new_names) {
    step(
      subclass = "rcs",
      terms = terms,
      role = role,
      trained = trained,
      id = id,
      skip = skip,
      number_of_knots = number_of_knots,
      knots = knots,
      new_names = new_names
    )
  }
#' @export
prep.step_rcs <- function(x, training, info = NULL, ...) {
  require(Hmisc)
  for (variable_name in recipes::terms_select(x$terms, info = info)) {
    # Verify the training data variable
    if (is.null(training[[variable_name]])) {
      stop(paste(variable_name, 'is missing from the training data'))
    }
    if (!is.numeric(training[[variable_name]])) {
      stop(paste(
        variable_name,
        'is not numeric therefore rcs cannot
        be calculated'
      ))
    }
    x$knots[[variable_name]] <- Hmisc::rcspline.eval(training[[variable_name]], nk = x$number_of_knots, knots.only = TRUE)
    }
  
  return(
    step_rcs_new(
      terms = x$terms,
      trained = TRUE,
      role = x$role,
      skip = x$skip,
      id = x$id,
      number_of_knots = x$number_of_knots,
      knots = x$knots,
      new_names = x$new_names
    )
  )
}

#' @export
bake.step_rcs <- function(object, new_data, ...) {
  require(tibble)
  for (variable_name in names(object$knots)) {
    # TESTING
    norm <- 2
    type <- 'ordinary'
    # TESTING
    knot1 <- object$knots[[variable_name]][1]
    knotnk <- object$knots[[variable_name]][object$number_of_knots[[variable_name]]]
    knotnk1 <- object$knots[[variable_name]][object$number_of_knots[[variable_name]] - 1]
    
    kd <- if (norm == 0)
      1
    else if (norm == 1)
      knotnk - knotnk1
    else
      (knotnk - knot1)^(2 / 3)
    
    power <- if (type == "integral")
      4
    else
      3
    
    for(knot_index in 1:(object$number_of_knots[[variable_name]]) - 2){
      
      knot_name <- object$new_names[[variable_name]][[knot_index]]
      new_data[, knot_name] <-
        pmax((new_data[[variable_name]] - object$knots[[variable_name]][knot_index]) / kd, 0)^power +
        ((knotnk1 - object$knots[[variable_name]][knot_index]) *
           pmax((new_data[[variable_name]] - knotnk) / kd, 0)^power -
           (knotnk - object$knots[[variable_name]][knot_index]) * (pmax((
             new_data[[variable_name]] - knotnk1
           ) / kd, 0)^power)) / (knotnk - knotnk1)
    }
  }
  
  return(as_tibble(new_data))
}

print.step_rcs <-
  function(x, width = max(20, options()$width - 30), ...) {
    cat("z score for ", sep = "")
    recipes:::printer(names(x$means), x$terms, x$trained, width = width)
    invisible(x)
  }

# prep_slice_rcs_eval <-
#   function(original_vector,
#            knots = NULL,
#            number_of_knots = 5,
#            include_original_column = FALSE,
#            knots.only = FALSE,
#            type = "ordinary",
#            norm = 2,
#            rpm = NULL,
#            pc = FALSE,
#            fractied = 0.05)
#   {
#     if (!length(knots)) {
#       xx <- original_vector[!is.na(original_vector)]
#       n <- length(xx)
#       if (n < 6)
#         stop("knots not specified, and < 6 non-missing observations")
#       if (number_of_knots < 3)
#         stop("number_of_knots must be >= 3")
#       xu <- sort(unique(xx))
#       nxu <- length(xu)
#       if ((nxu - 2) <= number_of_knots) {
#         warning(
#           sprintf(
#             "%s knots requested with %s unique values of original_vector.  knots set to %s interior values.",
#             number_of_knots,
#             nxu,
#             nxu - 2
#           )
#         )
#         knots <- xu[-c(1, length(xu))]
#       }
#       else {
#         outer <- if (number_of_knots > 3)
#           0.05
#         else
#           0.1
#         if (number_of_knots > 6)
#           outer <- 0.025
#         knots <- numeric(number_of_knots)
#         overrideFirst <- overrideLast <- FALSE
#         nke <- number_of_knots
#         firstknot <- lastknot <- numeric(0)
#         if (fractied > 0 && fractied < 1) {
#           f <- table(xx) / n
#           if (max(f[-c(1, length(f))]) < fractied) {
#             if (f[1] >= fractied) {
#               firstknot <- min(xx[xx > min(xx)])
#               xx <- xx[xx > firstknot]
#               nke <- nke - 1
#               overrideFirst <- TRUE
#             }
#             if (f[length(f)] >= fractied) {
#               lastknot <- max(xx[xx < max(xx)])
#               xx <- xx[xx < lastknot]
#               nke <- nke - 1
#               overrideLast <- TRUE
#             }
#           }
#         }
#         if (nke == 1)
#           knots <- median(xx)
#         else {
#           if (nxu <= nke)
#             knots <- xu
#           else {
#             p <- if (nke == 2)
#               seq(0.5, 1 - outer, length = nke)
#             else
#               seq(outer, 1 - outer, length = nke)
#             knots <- quantile(xx, p)
#             if (length(unique(knots)) < min(nke, 3)) {
#               knots <- quantile(xx, seq(outer, 1 - outer,
#                                         length = 2 * nke))
#               if (length(firstknot) && length(unique(knots)) <
#                   3) {
#                 midval <- if (length(firstknot) && length(lastknot))
#                   (firstknot + lastknot) / 2
#                 else
#                   median(xx)
#                 knots <-
#                   sort(c(
#                     firstknot,
#                     midval,
#                     if (length(lastknot))
#                       lastknot
#                     else
#                       quantile(xx,
#                                1 - outer)
#                   ))
#               }
#               if ((nu <- length(unique(knots))) < 3) {
#                 cat("Fewer than 3 unique knots.  Frequency table of variable:\n")
#                 print(table(original_vector))
#                 stop()
#               }
#               warning(
#                 paste(
#                   "could not obtain",
#                   nke,
#                   "interior knots with default algorithm.\n",
#                   "Used alternate algorithm to obtain",
#                   nu,
#                   "knots"
#                 )
#               )
#             }
#           }
#           if (length(xx) < 100) {
#             xx <- sort(xx)
#             if (!overrideFirst)
#               knots[1] <- xx[5]
#             if (!overrideLast)
#               knots[nke] <- xx[length(xx) - 4]
#           }
#         }
#         knots <- c(firstknot, knots, lastknot)
#       }
#     }
#     knots <- sort(unique(knots))
#     number_of_knots <- length(knots)
#     if (number_of_knots < 3) {
#       cat("fewer than 3 unique knots.  Frequency table of variable:\n")
#       print(table(original_vector))
#       stop()
#     }
#     if (knots.only)
#       return(knots)
#     if (length(rpm))
#       original_vector[is.na(original_vector)] <- rpm
#     xx <- matrix(1.1, length(original_vector), number_of_knots - 2)
#     knot1 <- knots[1]
#     knotnk <- knots[number_of_knots]
#     knotnk1 <- knots[number_of_knots - 1]
#     kd <- if (norm == 0)
#       1
#     else if (norm == 1)
#       knotnk - knotnk1
#     else
#       (knotnk - knot1)
#     ^ (2 / 3)
#     power <- if (type == "integral")
#       4
#     else
#       3
#   }
# 
# bake_sclice_rcs_eval <- function(working_data,) {
#   # Calculate number_of_knots
#   
#   #
#   
#   for (j in 1:(number_of_knots - 2)) {
#     xx[, j] <- pmax((original_vector - knots[j]) / kd, 0) ^ power +
#       ((knotnk1 - knots[j]) *
#          pmax((original_vector - knotnk) / kd, 0) ^ power -
#          (knotnk - knots[j]) *
#          (pmax((
#            original_vector - knotnk1
#          ) / kd, 0) ^ power)) / (knotnk - knotnk1)
#   }
#   if (power == 4)
#     xx <- cbind(original_vector, original_vector * original_vector / 2, xx * kd / 4)
#   else if (include_original_column)
#     xx <- cbind(original_vector, xx)
#   if (pc) {
#     p <- prcomp(xx, scale = TRUE, center = TRUE)
#     pcparms <- p[c("center", "scale", "rotation")]
#     xx <- p$original_vector
#     attr(xx, "pcparms") <- pcparms
#   }
#   attr(xx, "knots") <- knots
#   xx
#   
# }