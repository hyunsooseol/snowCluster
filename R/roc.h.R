
# This file is automatically generated, you probably don't want to edit this

rocOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rocOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            dep = NULL,
            covs = NULL,
            plot1 = FALSE,
            width1 = 500,
            height1 = 500,
            plot2 = FALSE,
            width2 = 500,
            height2 = 500,
            plot3 = FALSE,
            width3 = 500,
            height3 = 500, ...) {

            super$initialize(
                package="snowCluster",
                name="roc",
                requiresData=TRUE,
                ...)

            private$..dep <- jmvcore::OptionVariable$new(
                "dep",
                dep,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..covs <- jmvcore::OptionVariables$new(
                "covs",
                covs)
            private$..plot1 <- jmvcore::OptionBool$new(
                "plot1",
                plot1,
                default=FALSE)
            private$..width1 <- jmvcore::OptionInteger$new(
                "width1",
                width1,
                default=500)
            private$..height1 <- jmvcore::OptionInteger$new(
                "height1",
                height1,
                default=500)
            private$..plot2 <- jmvcore::OptionBool$new(
                "plot2",
                plot2,
                default=FALSE)
            private$..width2 <- jmvcore::OptionInteger$new(
                "width2",
                width2,
                default=500)
            private$..height2 <- jmvcore::OptionInteger$new(
                "height2",
                height2,
                default=500)
            private$..plot3 <- jmvcore::OptionBool$new(
                "plot3",
                plot3,
                default=FALSE)
            private$..width3 <- jmvcore::OptionInteger$new(
                "width3",
                width3,
                default=500)
            private$..height3 <- jmvcore::OptionInteger$new(
                "height3",
                height3,
                default=500)

            self$.addOption(private$..dep)
            self$.addOption(private$..covs)
            self$.addOption(private$..plot1)
            self$.addOption(private$..width1)
            self$.addOption(private$..height1)
            self$.addOption(private$..plot2)
            self$.addOption(private$..width2)
            self$.addOption(private$..height2)
            self$.addOption(private$..plot3)
            self$.addOption(private$..width3)
            self$.addOption(private$..height3)
        }),
    active = list(
        dep = function() private$..dep$value,
        covs = function() private$..covs$value,
        plot1 = function() private$..plot1$value,
        width1 = function() private$..width1$value,
        height1 = function() private$..height1$value,
        plot2 = function() private$..plot2$value,
        width2 = function() private$..width2$value,
        height2 = function() private$..height2$value,
        plot3 = function() private$..plot3$value,
        width3 = function() private$..width3$value,
        height3 = function() private$..height3$value),
    private = list(
        ..dep = NA,
        ..covs = NA,
        ..plot1 = NA,
        ..width1 = NA,
        ..height1 = NA,
        ..plot2 = NA,
        ..width2 = NA,
        ..height2 = NA,
        ..plot3 = NA,
        ..width3 = NA,
        ..height3 = NA)
)

rocResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rocResults",
    inherit = jmvcore::Group,
    active = list(
        instructions = function() private$.items[["instructions"]],
        text = function() private$.items[["text"]],
        plot1 = function() private$.items[["plot1"]],
        plot2 = function() private$.items[["plot2"]],
        plot3 = function() private$.items[["plot3"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="ROC Analysis",
                refs="snowCluster")
            self$add(jmvcore::Html$new(
                options=options,
                name="instructions",
                title="Instructions",
                visible=TRUE))
            self$add(jmvcore::Preformatted$new(
                options=options,
                name="text",
                title="ROC Analysis"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot1",
                title="ROC Curve",
                requiresData=TRUE,
                visible="(plot1)",
                renderFun=".plot1",
                refs="multipleROC",
                clearWith=list(
                    "covs",
                    "dep",
                    "width1",
                    "height1")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot2",
                title="Multiple ROC curves",
                requiresData=TRUE,
                visible="(plot2)",
                renderFun=".plot2",
                refs="multipleROC",
                clearWith=list(
                    "covs",
                    "dep",
                    "width2",
                    "height2")))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot3",
                title="Faceted ROC plots",
                requiresData=TRUE,
                visible="(plot3)",
                renderFun=".plot3",
                refs="multipleROC",
                clearWith=list(
                    "covs",
                    "dep",
                    "width3",
                    "height3")))}))

rocBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "rocBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "snowCluster",
                name = "roc",
                version = c(1,0,0),
                options = options,
                results = rocResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE,
                weightsSupport = 'auto')
        }))

#' ROC Analysis
#'
#' 
#' @param data .
#' @param dep .
#' @param covs .
#' @param plot1 .
#' @param width1 .
#' @param height1 .
#' @param plot2 .
#' @param width2 .
#' @param height2 .
#' @param plot3 .
#' @param width3 .
#' @param height3 .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$instructions} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$text} \tab \tab \tab \tab \tab a preformatted \cr
#'   \code{results$plot1} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' @export
roc <- function(
    data,
    dep,
    covs,
    plot1 = FALSE,
    width1 = 500,
    height1 = 500,
    plot2 = FALSE,
    width2 = 500,
    height2 = 500,
    plot3 = FALSE,
    width3 = 500,
    height3 = 500) {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("roc requires jmvcore to be installed (restart may be required)")

    if ( ! missing(dep)) dep <- jmvcore::resolveQuo(jmvcore::enquo(dep))
    if ( ! missing(covs)) covs <- jmvcore::resolveQuo(jmvcore::enquo(covs))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(dep), dep, NULL),
            `if`( ! missing(covs), covs, NULL))

    for (v in dep) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- rocOptions$new(
        dep = dep,
        covs = covs,
        plot1 = plot1,
        width1 = width1,
        height1 = height1,
        plot2 = plot2,
        width2 = width2,
        height2 = height2,
        plot3 = plot3,
        width3 = width3,
        height3 = height3)

    analysis <- rocClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}
