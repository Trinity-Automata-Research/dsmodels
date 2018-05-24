#' Create a new dsproto object
#'
#' dsproto draws from the prototype implementation in ggplot2, which in turn is inspired by the
#' ggplot2's implementation, with improvements in inheritance and performance.
#' The ggplot2 implementation is copyright by Hadley Wickham and Winston Chang, and released under
#' GPLv2.
#'
#' @section Calling dsproto methods:
#'
#' dsproto methods can take an optional \code{self} argument: if it is present,
#' it is a regular method; if it's absent, it's a "static" method.
#'
#' @section Calling methods in a parent:
#'
#' To explicitly call a methods in a parent, use
#' \code{dsproto_parent(Parent, self)}.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. If \code{NULL} (the default), no class name will
#'   be added to the object.
#' @param _inherit dsproto object to inherit from. If \code{NULL}, don't inherit
#'   from any object.
#' @param parent,self Access parent class \code{parent} of object \code{self}.
#' @param ... A list of members in the dsproto object.
#' @examples
#' #Create a class called "dog"
#' doggy <- dsproto(`_class` = "dog", `_inherit` = NULL)
#' #Add children to the parent class "dog". Then add members.
#' GS <- dsproto(`_class` = "German Shephard", `_inherit` = doggy,
#'         name = "Pedro",
#'         color = "Brown",
#'         personality = "Friendly",
#'         age = 1,
#'         parents = list("Spot", "Alfred"),
#'         birthday = function(self) self$age = self$age + 1)
#' #Call those members
#' GS$color
#' GS$age
#' #Call a function member
#' GS$birthday()
#' #Happy Birthday to Pedro! Your dog is now 2! Let's see what the age returns
#' GS$age
#' @export
dsproto <- function(`_class` = NULL, `_inherit` = NULL, ...) {
  e <- new.env(parent = emptyenv())

  members <- list(...)
    # Check to make sure all of your parameters are of the correct class
    # Check to make sure all of your parameters are defined (should not be NULL)

  if (length(members) != sum(nzchar(names(members)))) {
    stop("All members of a dsproto object must be named.")
  }

  # R <3.1.2 will error when list2env() is given an empty list, so we need to
  # check length. https://github.com/hadley/dsplot2/issues/1444
  if (length(members) > 0) {
    list2env(members, envir = e)
  }

  if (!is.null(`_inherit`)) {
    if (!is.dsproto(`_inherit`)) {
      stop("`_inherit` must be a dsproto object.")
    }
    e$super <- `_inherit`
    class(e) <- c(`_class`, class(`_inherit`))

  } else {
    class(e) <- c(`_class`, "dsproto")
  }
  e
}

#' Tests if an object is a ds object.
# @rdname dsproto
#' @param x An object to test.
#' @keywords internal
#' @export
is.dsproto <- function(x) inherits(x, "dsproto")

# Finds name in x
# a <- dsproto(Yay = TRUE)
# fetch_dsproto(a, "Yay")
fetch_dsproto <- function(x, name) {
  res <- NULL

  val <- .subset2(x, name)
  # The is.null check is an optimization for a common case; exists() also
  # catches the case where the value exists but has a NULL value.
  if (!is.null(val) || exists(name, envir = x, inherits = FALSE)) {
    res <- val
  } else {
    # If not found here, recurse into super environments
    super <- .subset2(x, "super")
    if (is.dsproto(super))
      res <- fetch_dsproto(super, name)
  }

  res
}

#' @export
#' @rdname dsproto
dsproto_parent <- function(parent, self) {
  structure(list(parent = parent, self = self), class = "dsproto_parent")
}

#' Used to call members of a dsproto
#' @export
#' @usage NULL
#' @keywords internal
# @rdname dsproto
`$.dsproto` <- function(x, name) {
  res <- fetch_dsproto(x, name)
  if (!is.function(res)) {
    return(res)
  }

  make_proto_method(x, res)
}

#' @export
#' @usage NULL
#' @rdname dsproto
`$.dsproto_parent` <- function(x, name) {
  res <- fetch_dsproto(.subset2(x, "parent"), name)
  if (!is.function(res)) {
    return(res)
  }

  make_proto_method(.subset2(x, "self"), res)
}

make_proto_method <- function(self, f) {
  args <- formals(f)
  # is.null is a fast path for a common case; the %in% check is slower but also
  # catches the case where there's a `self = NULL` argument.
  has_self  <- !is.null(args[["self"]]) || "self"  %in% names(args)

  if (has_self) {
    fun <- function(...) f(..., self = self)
  } else {
    fun <- function(...) f(...)
  }

  class(fun) <- "dsproto_method"
  fun
}


#' @export
#' @usage NULL
#' @rdname dsproto
`[[.dsproto` <- `$.dsproto`

#' Convert a dsproto object to a list
#
#' This will not include the object's \code{super} member.
#
#' @param x A dsproto object to convert to a list.
#' @param inherit If \code{TRUE} (the default), flatten all inherited items into
#   the returned list. If \code{FALSE}, do not include any inherited items.
#' @param ... Further arguments to pass to \code{as.list.environment}.
#' @rdname dsproto
#' @usage NULL
#' @keywords internal
#' @export
as.list.dsproto <- function(x, inherit = TRUE, ...) {
  res <- list()

  if (inherit) {
    if (!is.null(x$super)) {
      res <- as.list(x$super)
    }
  }

  current <- as.list.environment(x, ...)
  res[names(current)] <- current
  res$super <- NULL
  res
}

# Print a dsproto object
#
# If a dsproto object has a \code{$print} method, this will call that method.
# Otherwise, it will print out the members of the object, and optionally, the
# members of the inherited objects.
#
# @param x A dsproto object to print.
# @param flat If \code{TRUE} (the default), show a flattened list of all local
#   and inherited members. If \code{FALSE}, show the inheritance hierarchy.
# @param ... If the dsproto object has a \code{print} method, further arguments
#   will be passed to it. Otherwise, these arguments are unused.
#' @rdname dsproto
#' @export
print.dsproto <- function(x, ..., flat = TRUE) {
  if (is.function(x$print)) {
    x$print(...)

  } else {
    cat(format(x, flat = flat), "\n", sep = "")
    invisible(x)
  }
}


#' Format a dsproto object
#'
#' @inheritParams print.dsproto
#' @rdname dsproto
#' @export
format.dsproto <-  function(x, ..., flat = TRUE) {
  classes_str <- function(obj) {
    classes <- setdiff(class(obj), "dsproto")
    if (length(classes) == 0)
      return("")
    paste0(": Class ", paste(classes, collapse = ', '))
  }

  # Get a flat list if requested
  if (flat) {
    objs <- as.list(x, inherit = TRUE)
  } else {
    objs <- x
  }

  str <- paste0(
    "<dsproto object", classes_str(x), ">\n",
    indent(object_summaries(objs, flat = flat), 4)
  )

  if (flat && !is.null(x$super)) {
    str <- paste0(
      str, "\n",
      indent(
        paste0("super: ", " <dsproto object", classes_str(x$super), ">"),
        4
      )
    )
  }

  str
}

# Return a summary string of the items of a list or environment
# x must be a list or environment
object_summaries <- function(x, exclude = NULL, flat = TRUE) {
  if (length(x) == 0)
    return(NULL)

  if (is.list(x))
    obj_names <- sort(names(x))
  else if (is.environment(x))
    obj_names <- ls(x, all.names = TRUE)

  obj_names <- setdiff(obj_names, exclude)

  values <- vapply(obj_names, function(name) {
    obj <- x[[name]]
    if (is.function(obj)) "function"
    else if (is.dsproto(obj)) format(obj, flat = flat)
    else if (is.environment(obj)) "environment"
    else if (is.null(obj)) "NULL"
    else if (is.atomic(obj)) trim(paste(as.character(obj), collapse = " "))
    else paste(class(obj), collapse = ", ")
  }, FUN.VALUE = character(1))

  paste0(obj_names, ": ", values, sep = "", collapse = "\n")
}

# Given a string, indent every line by some number of spaces.
# The exception is to not add spaces after a trailing \n.
indent <- function(str, indent = 0) {
  gsub("(\\n|^)(?!$)",
       paste0("\\1", paste(rep(" ", indent), collapse = "")),
       str,
       perl = TRUE
  )
}

# Trim a string to n characters; if it's longer than n, add " ..." to the end
trim <- function(str, n = 60) {
  if (nchar(str) > n) paste(substr(str, 1, 56), "...")
  else str
}

#' @export
#' @rdname dsproto
print.dsproto_method <- function(x, ...) {
  cat(format(x), sep = "")
}

#' @export
#' @rdname dsproto
format.dsproto_method <- function(x, ...) {

  # Given a function, return a string from srcref if present. If not present,
  # paste the deparsed lines of code together.
  format_fun <- function(fn) {
    srcref <- attr(fn, "srcref", exact = TRUE)
    if (is.null(srcref))
      return(paste(format(fn), collapse = "\n"))

    paste(as.character(srcref), collapse = "\n")
  }

  x <- unclass(x)
  paste0(
    "<dsproto method>",
    "\n  <Wrapper function>\n    ", format_fun(x),
    "\n\n  <Inner function (f)>\n    ", format_fun(environment(x)$f)
  )
}

dsproto_formals <- function(x) formals(environment(x)$f)

#' Facade
#'
# Acts as a parent object for Ranges, Features, Backgrounds, and Visualizations
#'
#' @keywords internal
# @rdname dsproto
# @format NULL
# @usage NULL
#' @export
facade <- dsproto("facade", NULL,
  bound = FALSE,
  recalculate = function(self, model) {self$on.bind(model)},
  render = function(self, model) {
    if(!self$bound)
      stop("Critical error: attempting to render dsmodels object before it is bound. Please notify developers.", call. = FALSE)
  },
  on.bind = function(self, model) {self$bound = TRUE}
  )

#' Background
#'
# Class describing bottom level items, such as dsregion.
#' @keywords internal
# @rdname dsproto
# @format NULL
# @usage NULL
#' @export
background <- dsproto("background", facade, behind=TRUE)

#' Visualization
#'
# Class describing middle level items, such as dsdots and dsarrows.
#' @keywords internal
#@rdname dsproto
# @format NULL
# @usage NULL
#' @export
visualization <- dsproto("visualization", facade, behind=TRUE)

#' Feature
#'
# Class describing top level items, such as dspoint and dscurve.
#' @keywords internal
# @rdname dsproto
# @format NULL
# @usage NULL
#' @export
feature <- dsproto("feature", facade)


#' Checks if object is a facade
# @rdname dsproto
#' @param x An object to test
#' @keywords internal
#' @export
is.facade <- function(x) inherits(x, "facade")


#' Checks if object is a background
# @rdname dsproto
#' @param x An object to test.
#' @keywords internal
#' @export
is.background <- function(x) inherits(x, "background")


#' Checks if object is a visualization
# @rdname dsproto
#' @param x An object to test
#' @keywords internal
#' @export
is.visualization <- function(x) inherits(x, "visualization")


#' Checks if object is a feature
# @rdname dsproto
#' @param x An object to test
#' @keywords internal
#' @export
is.feature <- function(x) inherits(x, "feature")
