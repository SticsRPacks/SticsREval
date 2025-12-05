setGeneric("usms", function(object) standardGeneric("usms"))
setGeneric("rotations", function(object) standardGeneric("rotations"))

setClass(
  "DataSource",
  slots = list(
    usms = "character",
    rotations = "list"
  )
)
setMethod(
  "usms",
  "DataSource",
  function(object) {
    object@usms
  }
)
setMethod(
  "rotations",
  "DataSource",
  function(object) {
    object@rotations
  }
)
