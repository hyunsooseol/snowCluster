
# This file is automatically generated, you probably don't want to edit this

mcaOptions <- if (requireNamespace('jmvcore')) R6::R6Class(
    "mcaOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            facs = NULL,
            vars = NULL,
            plot1 = TRUE,
            plot2 = FALSE,
            plot3 = FALSE,
            plot4 = FALSE, ...) {

            super$initialize(
                package='snowCluster',
                name='mca',
                requiresData=TRUE,
                ...)

            private$..facs <- jmvcore::OptionVariable$new(
                "facs",
                facs,
                suggested=list(
                    "nominal"),
                permitted=list(
                    "factor"))
            private$..vars <- jmvcore::OptionVariables$new(
                "vars",
                vars,
                suggested=list(
                    "nominal",
                    "ordinal"),
                permitted=list(
                    "factor"))
            private$..plot1 <- jmvcore::OptionBool$new(
                "plot1",
                plot1,
                default=TRUE)
            private$..plot2 <- jmvcore::OptionBool$new(
                "plot2",
                plot2,
                default=FALSE)
            private$..plot3 <- jmvcore::OptionBool$new(
                "plot3",
                plot3,
                default=FALSE)
            private$..plot4 <- jmvcore::OptionBool$new(
                "plot4",
                plot4,
                default=FALSE)

            self$.addOption(private$..facs)
            self$.addOption(private$..vars)
            self$.addOption(private$..plot1)
            self$.addOption(private$..plot2)
            self$.addOption(private$..plot3)
            self$.addOption(private$..plot4)
        }),
    active = list(
        facs = function() private$..facs$value,
        vars = function() private$..vars$value,
        plot1 = function() private$..plot1$value,
        plot2 = function() private$..plot2$value,
        plot3 = function() private$..plot3$value,
        plot4 = function() private$..plot4$value),
    private = list(
        ..facs = NA,
        ..vars = NA,
        ..plot1 = NA,
        ..plot2 = NA,
        ..plot3 = NA,
        ..plot4 = NA)
)

mcaResults <- if (requireNamespace('jmvcore')) R6::R6Class(
    inherit = jmvcore::Group,
    active = list(
        plot1 = function() private$.items[["plot1"]],
        plot2 = function() private$.items[["plot2"]],
        plot3 = function() private$.items[["plot3"]],
        plot4 = function() private$.items[["plot4"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="MCA Plot")
            self$add(jmvcore::Image$new(
                options=options,
                name="plot1",
                title="Correlation between variables",
                requiresData=TRUE,
                refs="factoextra",
                visible="(plot1)",
                width=500,
                height=500,
                renderFun=".plot1"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot2",
                title="Coordinates of variable categories",
                requiresData=TRUE,
                refs="factoextra",
                visible="(plot2)",
                width=500,
                height=500,
                renderFun=".plot2"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot3",
                title="Plot of individuals",
                requiresData=TRUE,
                refs="factoextra",
                visible="(plot3)",
                width=500,
                height=500,
                renderFun=".plot3"))
            self$add(jmvcore::Image$new(
                options=options,
                name="plot4",
                title="individuals by groups",
                requiresData=TRUE,
                refs="factoextra",
                visible="(plot4)",
                width=500,
                height=500,
                renderFun=".plot4"))}))

mcaBase <- if (requireNamespace('jmvcore')) R6::R6Class(
    "mcaBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = 'snowCluster',
                name = 'mca',
                version = c(1,0,0),
                options = options,
                results = mcaResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' MCA Plot
#'
#' 
#' @param data The data as a data frame.
#' @param facs .
#' @param vars .
#' @param plot1 .
#' @param plot2 .
#' @param plot3 .
#' @param plot4 .
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$plot1} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot2} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot3} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plot4} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' @export
mca <- function(
    data,
    facs,
    vars,
    plot1 = TRUE,
    plot2 = FALSE,
    plot3 = FALSE,
    plot4 = FALSE) {

    if ( ! requireNamespace('jmvcore'))
        stop('mca requires jmvcore to be installed (restart may be required)')

    if ( ! missing(facs)) facs <- jmvcore::resolveQuo(jmvcore::enquo(facs))
    if ( ! missing(vars)) vars <- jmvcore::resolveQuo(jmvcore::enquo(vars))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(facs), facs, NULL),
            `if`( ! missing(vars), vars, NULL))

    for (v in facs) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    for (v in vars) if (v %in% names(data)) data[[v]] <- as.factor(data[[v]])

    options <- mcaOptions$new(
        facs = facs,
        vars = vars,
        plot1 = plot1,
        plot2 = plot2,
        plot3 = plot3,
        plot4 = plot4)

    analysis <- mcaClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}