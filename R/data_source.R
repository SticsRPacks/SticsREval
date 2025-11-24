setGeneric("usms", function(object) standardGeneric("usms"))
setClass(
  "DataSource",
  slots = list(
    usms = "character"
  )
)
setMethod(
  "usms",
  "DataSource",
  function(object) {
    object@usms
  }
)
