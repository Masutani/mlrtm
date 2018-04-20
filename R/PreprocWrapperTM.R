#' topic model (LDA) wrapper for mlr preprocessing

#' preprocessor on training
#' 
#' @import topicmodels
#' @import tm
#' @import tidyverse
#' @import tidytext
#' @export

prepTrainLDA = function(data, target, args = list(k, exclude)) {
    labels <- data[, target]
    # exclude features decomposed
    if (!is.null(args$exclude) && length(args$exclude) > 0) {
        directs <- data[, args$exclude]
        data <- data[, - args$exclude]
    }
    # tidyr features
    tdata <- data %>%
        mutate(lineid = row_number()) %>%
        tidyr::gather(key = feature, value = occ, - target, - lineid)
    # convert to document term matrix in tm library
    dtm = tdata %>%
            tidytext::cast_dtm(lineid, feature, occ)
    # make topic model with LDA
    model <- topicmodels::LDA(dtm, args$k);
    # build latent vector
    x.topics <- topics(model)
    x.terms <- terms(model, args$k)
    latent <- as.data.frame(model@gamma)
    names(latent) <- paste0("latent", c(1:args$k))
    # reconsturct training dataset
    rdata <- cbind(labels, latent)
    if (!is.null(args$exclude) && length(args$exclude) > 0) {
        rdata <- cbind(rdata, direct)
    }
    return(list(data = rdata, control = list(k=args$k, exclude=args$explude,model=model) ))
}
#' preprocessor on prediction
#' 
#' @import topicmodels
#' @import tm
#' @import tidyverse
#' @import tidytext
#' @export
prepPredictionLDA = function(data, target, args, control = list(k, exclude, model)) {
    # exclude features decomposed
    if (!is.null(control$exclude) && length(control$exclude) > 0) {
        directs <- data[, control$exclude]
        data <- data[, - control$exclude]
    }
    # tidyr features
    tdata <- data %>%
        mutate(lineid = row_number()) %>%
        tidyr::gather(key = feature, value = occ, - lineid)
    # convert to document term matrix in tm library
    dtm = tdata %>%
        cast_dtm(lineid, feature, occ)
    # decompose to latent vector using trained topic model
    test.topics <- posterior(control$model, dtm)
    # build latent vector
    latent <- as.data.frame(test.topics$topics)
    names(latent) <- paste0("latent", c(1:control$k))
    rdata <- latent
    # reconsturct prediction dataset
    if (!is.null(control$exclude) && length(control$exclude) > 0) {
        rdata <- cbind(rdata, direct)
    }
    return(rdata)
}