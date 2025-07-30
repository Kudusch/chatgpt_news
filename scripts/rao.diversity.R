# Implementation from https://github.com/vanderleidebastiani/SYNCSA/tree/master
# Vanderlei J. Debastiani, Valério D. Pillar; SYNCSA-R tool for analysis of metacommunities based on functional traits and phylogeny of the community components, Bioinformatics, Volume 28, Issue 15, 1 August 2012, Pages 2067–2068, https://doi.org/10.1093/bioinformatics/bts325
rao.diversity <- function(comm, traits = NULL, phylodist = NULL, checkdata = TRUE, ord = "metric",
                          put.together = NULL, standardize = TRUE, transformation = "standardized",
                          spp.weights = NULL, ...)
{
    
    
    matrix.w.transformation <- function(comm, transformation = "standardized", spp.weights = NULL,
                                        reference = NULL, type = 0, include = TRUE, notification = TRUE)
    {
        matrix.w <- as.matrix(comm)
        TRANS <- c("none", "standardized", "weights", "beals")
        trans <- pmatch(transformation, TRANS)
        if (length(trans) > 1) {
            stop("\n Only one argument is accepted in transformation \n")
        }
        if (is.na(trans)) {
            stop("\n Invalid transformation \n")
        }
        w.NA <- apply(matrix.w, 2, is.na)
        if(notification){
            if(any(w.NA)){
                warning("Warning: NA in community data", call.= FALSE)
            }
        }
        if(trans == 2){
            matrix.w <- sweep(matrix.w, 1, rowSums(matrix.w, na.rm = TRUE), "/")
        }
        if(trans == 3){
            is.bin.weights <- all(spp.weights %in% c(0, 1))
            if(!is.bin.weights | is.null(spp.weights)){
                stop("\n spp.weights must be 0 or 1\n")
            }
            if((ncol(matrix.w) != length(spp.weights))){
                stop("\n spp.weights must be the same length of number of species \n")
            }
            matrix.w <- sweep(matrix.w, 1, rowSums(matrix.w, na.rm = TRUE), "/")
            spp.weights <- sapply(spp.weights, function(x, n) rep(x, n), n = nrow(matrix.w))
            matrix.w <- matrix.w*spp.weights
        }
        if(trans == 4){
            if(is.null(reference)){
                reference <- matrix.w
            }
            if((ncol(matrix.w) != ncol(reference)) | (nrow(matrix.w) != nrow(reference))){
                stop("\n comm and reference data must be the same dimensions \n")
            }
            matrix.w <- vegan::beals(matrix.w, reference = reference, type = type, include = include)
        }
        w.NA <- apply(matrix.w, 2, is.na)
        matrix.w[w.NA] <- 0
        return(matrix.w)
    }
    var.type <- function(data)
    {
        if(!inherits(data, c("data.frame", "matrix"))){
            stop("data must be a matrix or a data.frame")
        }
        colnames(data) <- colnames(data, do.NULL = FALSE, prefix = "var")
        is.bin <- function(k) all(k[!is.na(k)] %in% c(0, 1))
        nc <- ncol(data)
        if (is.data.frame(data)) {
            type <- sapply(data, data.class)
            type2 <- type
            bin.var <- rep(NA, nc)
            for (i in 1:nc) {
                bin.var[i] <- is.bin(data[, i])
            }
            type[type %in% c("numeric", "integer")] <- "c"
            type[type == "ordered"] <- "o"
            type[type == "character"] <- "n"
            type[type == "factor"] <- "f"
            type[bin.var] <- "b"
            type[type2 == "character"] <- "n"
            type[type2 == "factor"] <- "f"
            names(type) <- NULL
        }
        else {
            if(any(sapply(data, data.class) == "character")){
                stop("\n If data is a matrix class it must be entirely numeric \n")
            }
            type <- rep("c", nc)
        }
        return(type)
    }
    organize.syncsa <- function (comm, traits = NULL, phylodist = NULL, envir = NULL,
                                 strata = NULL, spp.weights = NULL,
                                 check.comm = TRUE, convert.traits = FALSE, ranks = TRUE)
    {
        res <- list(call = match.call())
        res.temp <- res
        res.temp$stop <- FALSE
        if (missing(comm)){
            stop("\n comm not fount\n")
        }
        if(!inherits(comm, c("data.frame", "matrix"))){
            stop("comm must be a matrix or a data.frame")
        }
        if (is.null(colnames(comm))){
            stop("\n Column names of comm are null\n")
        }
        if (is.null(rownames(comm))){
            stop("\n Row names of comm are null\n")
        }
        commvartype <- var.type(comm)
        if(any(commvartype == "n") | any(commvartype == "f") | any(commvartype == "o")){
            stop("\n comm must contain only numeric or binary variables \n")
        }
        list.warning <- list()
        put.together <- NULL
        if(check.comm){
            col.rm <- colnames(comm)[!colSums(comm, na.rm = TRUE)>0]
            row.rm <- rownames(comm)[!rowSums(comm, na.rm = TRUE)>0]
            if(length(col.rm)>0){
                warning("Species removed from community data - Check list of warning in list.warning", call. = FALSE)
                list.warning$comm$spp <- data.frame(species.removed = col.rm)
            }
            if(length(row.rm)>0){
                warning("Communities removed from community data - Check list of warning in list.warning", call. = FALSE)
                list.warning$comm$comm <- data.frame(communities.removed = row.rm)
            }
            comm <- comm[, colSums(comm, na.rm = TRUE)>0, drop = FALSE]
            comm <- comm[rowSums(comm,na.rm=TRUE)>0, , drop = FALSE]
        }
        if(any(is.na(comm))){
            warning("Warning: NA in community data", call. = FALSE)
        }
        if (!is.null(traits)) {
            if(!inherits(traits, c("data.frame", "matrix"))){
                stop("traits must be a matrix or a data.frame")
            }
            if (is.null(colnames(traits))){
                stop("\n Column names of traits are null\n")
            }
            if (is.null(rownames(traits))){
                stop("\n Row names of traits are null\n")
            }
            traitsvartype <- var.type(traits)
            if(any(traitsvartype == "n")){
                stop("\n trait must contain only numeric, binary, factor or ordinal variables \n")
            }
            match.names <- match(colnames(comm), rownames(traits))
            if(sum(is.na(match.names))>0){
                list.warning$traits$spp <- data.frame(species.not.on.traits = setdiff(colnames(comm), rownames(traits)))
                warning("ERROR - Check list of warning in list.warning$traits", call. = FALSE)
                res.temp$stop <- TRUE
                res.temp$list.warning <- list.warning
                return(res.temp)
            }
            traits <- as.data.frame(traits[match.names, , drop = FALSE])
            if(convert.traits){
                if(any(traitsvartype == "f")){
                    warning("Factor variables expanded in dummy variables", call. = FALSE)
                }
                traits.dummy.temp <- var.dummy(traits)
                traits <- traits.dummy.temp$data
                put.together <- traits.dummy.temp$together
                traitsvartype <- var.type(traits)
                if(any(traitsvartype == "o")){
                    warning("Ordinal variables transformed in continual variables", call. = FALSE)
                }
                traits <- data.matrix(traits)
                for (i in 1:length(traitsvartype)){
                    if (traitsvartype[i] == "o"){
                        if (ranks){
                            traits[,i] <- rank(traits[,i], na.last = "keep")
                        } else {
                            traits[,i] <- as.numeric(traits[, i])
                        }
                    }
                }
                traitsvartype <- var.type(traits)
            }
            if(any(is.na(traits))){
                warning("Warning: NA in traits matrix", call. = FALSE)
            }
        }
        if (!is.null(phylodist)) {
            if(!inherits(phylodist, c("data.frame", "matrix"))){
                stop("phylodist must be a matrix or a data.frame")
            }
            if (is.null(colnames(phylodist))){
                stop("\n Column names of phylodist are null\n")
            }
            if (is.null(rownames(phylodist))){
                stop("\n Row names of phylodist are null\n")
            }
            phylodistvartype <- var.type(phylodist)
            if(any(phylodistvartype == "n") | any(phylodistvartype == "f") | any(phylodistvartype == "o")){
                stop("\n phylodist must contain only numeric or binary variables \n")
            }
            match.names <- match(colnames(comm), colnames(phylodist))
            if(sum(is.na(match.names))>0){
                list.warning$phylodist$spp <- data.frame(species.not.on.phylodist = setdiff(colnames(comm), colnames(phylodist)))
                warning("ERROR - Check list of warning in list.warning$phylodist", call. = FALSE)
                res.temp$stop <- TRUE
                res.temp$list.warning <- list.warning
                return(res.temp)
            }
            phylodist <- phylodist[match.names, match.names, drop = FALSE]
            if(any(is.na(phylodist))){
                warning("Warning: NA in phylogenetic distance matrix",call.=FALSE)
            }
        }
        if (!is.null(envir)) {
            if(!inherits(envir, c("data.frame", "matrix"))){
                stop("envir must be a matrix or a data.frame")
            }
            if (is.null(colnames(envir))){
                stop("\n Column names of envir are null\n")
            }
            if (is.null(rownames(envir))){
                stop("\n Row names of envir are null\n")
            }
            envirvartype <- var.type(envir)
            if(any(envirvartype == "n") | any(envirvartype == "f") | any(envirvartype == "o")){
                stop("\n envir must contain only numeric or binary variables \n")
            }
            match.names <- match(rownames(comm), rownames(envir))
            if(sum(is.na(match.names))>0){
                list.warning$envir$comm <- data.frame(comm.not.on.envir = setdiff(rownames(comm), rownames(envir)))
                warning("ERROR - Check list of warning in list.warning$envir", call. = FALSE)
                res.temp$stop <- TRUE
                res.temp$list.warning <- list.warning
                return(res.temp)
            }
            envir <- envir[match.names,,drop=FALSE]
            if(any(is.na(envir))){
                warning("Warning: NA in environmental data", call. = FALSE)
            }
        }
        if (!is.null(strata)) {
            if (is.null(names(strata))){
                stop("\n Names of strata factor are null\n")
            }
            match.names <- match(colnames(comm), names(strata))
            if(sum(is.na(match.names))>0){
                list.warning$strata <- data.frame(species.not.on.strata = setdiff(colnames(comm), names(strata)))
                warning("ERROR - Check list of warning in list.warning$strata", call. = FALSE)
                res.temp$stop <- TRUE
                res.temp$list.warning <- list.warning
                return(res.temp)
            }
            strata <- strata[match.names]
        }
        if (!is.null(spp.weights)) {
            if (is.null(names(spp.weights))){
                stop("\n Names of spp.weights are null\n")
            }
            match.names <- match(colnames(comm), names(spp.weights))
            if(sum(is.na(match.names))>0){
                list.warning$spp.weights <- data.frame(species.not.on.spp.weights = setdiff(colnames(comm), names(spp.weights)))
                warning("ERROR - Check list of warning in list.warning$spp.weights", call. = FALSE)
                res.temp$stop <- TRUE
                res.temp$list.warning <- list.warning
                return(res.temp)
            }
            spp.weights <- spp.weights[match.names]
        }
        if (!is.null(traits)) {
            spp.all.trait.na <- apply(is.na(traits), 1, sum)==ncol(traits)
            if(any(spp.all.trait.na)){
                if (is.null(strata)) {
                    strata <- rep(1, nrow(traits))
                    names(strata) <- colnames(comm)
                }
                strata[spp.all.trait.na] <- max(strata)+seq_len(sum(spp.all.trait.na))
                warning("The strata vector was generated or modified", call. = FALSE)
            }
        }
        if (is.null(traits)){
            traits <- NULL
            traitsvartype <- NULL
        }
        if (is.null(phylodist)){
            phylodist <- NULL
            phylodistvartype <- NULL
        }
        if (is.null(envir)){
            envir <- NULL
            envirvartype <- NULL
        }
        if (is.null(strata)){
            strata <- NULL
        }
        if (is.null(spp.weights)){
            spp.weights <- NULL
        }
        if(length(list.warning)>0){
            res$list.warning <- list.warning
        }
        res$community <- comm
        res$traits <- traits
        res$phylodist <- phylodist
        res$environmental <- envir
        res$community.var.type <- commvartype
        res$traits.var.type <- traitsvartype
        res$phylodist.var.type <- phylodistvartype
        res$environmental.var.type <- envirvartype
        res$strata <- strata
        res$put.together <- put.together
        res$spp.weights <- spp.weights
        class(res) <- c("list", "metacommunity.data")
        return(res)
    }
    
    
    diver.internal <- function(community, distance){
        if(any(is.na(distance))){
            distance.na <- ifelse(is.na(distance), 0, 1)
            inter.na <- community%*%distance.na
            adjustment <- rowSums(sweep(community, 1, inter.na, "*", check.margin = FALSE))
            distance[is.na(distance)] <- 0
            inter <- community%*%distance
            res <- rowSums(sweep(community, 1, inter, "*", check.margin = FALSE))
            res <- ifelse(adjustment>0, res/adjustment, res)
        } else{
            inter <- community%*%distance
            res <- rowSums(sweep(community, 1, inter, "*", check.margin = FALSE))
        }
        return(res)
    }
    res <- list(call = match.call())
    if (inherits(comm, "metacommunity.data")) {
        if (!is.null(traits) | !is.null(phylodist) | !is.null(put.together) | !is.null(spp.weights)) {
            stop("\n When you use an object of class metacommunity.data the arguments traits, phylodist, spp.weights and put.together must be null. \n")
        }
        traits <- comm$traits
        phylodist <- comm$phylodist
        put.together <- comm$put.together
        spp.weights <- comm$spp.weights
        comm <- comm$community
    }
    list.warning <- list()
    if(checkdata){
        organize.temp <- organize.syncsa(comm, traits = traits, phylodist = phylodist, spp.weights = spp.weights, check.comm = TRUE)
        if(!is.null(organize.temp$stop)){
            organize.temp$call <- match.call()
            return(organize.temp)
        }
        list.warning <- organize.temp$list.warning
        comm <- organize.temp$community
        traits <- organize.temp$traits
        phylodist <- organize.temp$phylodist
        spp.weights <- organize.temp$spp.weights
    }
    if(length(list.warning)>0){
        res$list.warning <- list.warning
    }
    if(any(is.na(comm))){
        stop("\n community data with NA\n")
    }
    TRANS <- c("none", "standardized", "weights", "beals", "max.weights")
    trans <- pmatch(transformation, TRANS)
    if (length(trans) > 1) {
        stop("\n Only one argument is accepted in transformation \n")
    }
    if (is.na(trans) | trans == 4) {
        stop("\n Invalid transformation \n")
    }
    if(trans == 5){
        is.bin.weights <- all(spp.weights %in% c(0, 1))
        if(!is.bin.weights | is.null(spp.weights)){
            stop("\n spp.weights must be 0 or 1\n")
        }
        comm <- matrix.w.transformation(comm, transformation = "standardized", notification = FALSE)
        VecMax <- Vectorize(function(x, y) max(x,y))
        dist.weights <- outer(spp.weights, spp.weights, FUN = VecMax)
    } else{
        comm <- matrix.w.transformation(comm, transformation = transformation, spp.weights = spp.weights, notification = FALSE)
    }
    S <- ncol(comm)
    dist.1 <- 1 - diag(x = rep(1, S))
    if (!is.null(traits)) {
        traits <- as.data.frame(traits)
        m <- ncol(traits)
        weights <- rep(1, m)
        make.names <- is.null(colnames(traits))
        colnames(traits) <- colnames(traits, do.NULL = FALSE, prefix = "T")
        names(weights) <- colnames(traits)
        if(!is.null(put.together)){
            if(!inherits(put.together, "list")){
                stop("\n put.together must be a object of class list\n")
            }
            if(make.names){
                for(k in 1:length(put.together)){
                    put.together[[k]] <- paste("T", put.together[[k]], sep = "")
                }
            }
            if(max(table(unlist(put.together)))>1){
                stop("\n The same trait appears more than once in put.together\n")
            }
            if(length(setdiff(unlist(put.together), colnames(traits)))>0){
                stop("\n Check traits names in put.together\n")
            }
            for(k in 1:length(put.together)){
                weights[put.together[[k]]] <- 1/length(put.together[[k]])
            }
        }
        dist.functional <- sqrt(as.matrix(FD::gowdis(x=traits, asym.bin = NULL, ord = ord, w = weights, ...)))
        if (checkdata) {
            if(any(is.na(dist.functional))){
                # stop("\n traits with too much NA \n")
                warning("Warning: NA in distance between species", call. = FALSE)
            }
        }
    }
    if (!is.null(phylodist)) {
        dist.phylogenetic <- as.matrix(phylodist)
        if (checkdata) {
            if(any(is.na(dist.phylogenetic))){
                # stop("\n phylodist with NA \n")
                warning("Warning: NA in phylodist", call. = FALSE)
            }
        }
        if(standardize){
            dist.phylogenetic <- dist.phylogenetic/max(dist.phylogenetic, na.rm = TRUE)
        }
    }
    if(trans == 5){
        dist.1 <- dist.1*dist.weights
    }
    SD <- diver.internal(comm, dist.1)
    res$Simpson <- SD
    if (!is.null(traits)){
        if(trans == 5){
            dist.functional <- dist.functional*dist.weights
        }
        FD <- diver.internal(comm, dist.functional)
        res$FunRao <- FD
        res$FunRedundancy <- SD-FD
    }
    if (!is.null(phylodist)){
        if(trans == 5){
            dist.phylogenetic <- dist.phylogenetic*dist.weights
        }
        PD <- diver.internal(comm, dist.phylogenetic)
        res$PhyRao <- PD
        res$PhyRedundancy <- SD-PD
    }
    return(res)
}
#' @title Check the type of variables
#'
#' @description Function to check the type of variables in a data.frame or matrix. This function was extracted
#' and slightly modified of the function \code{\link{gowdis}}.
#'
#' @encoding UTF-8
#' @param data A data.frame or matrix.
#' @return A vector with the variable types, where 'c' is continuous/numeric, 'o' is
#' ordinal, 'b' is binary, 'n' is nominal and 'f' is factor.
#' @author Vanderlei Julio Debastiani <vanderleidebastiani@@yahoo.com.br>
#' @seealso  \code{\link{syncsa}}, \code{\link{organize.syncsa}}, \code{\link{var.dummy}}
#' @keywords Auxiliary
#' @export
var.type <- function(data)
{
    if(!inherits(data, c("data.frame", "matrix"))){
        stop("data must be a matrix or a data.frame")
    }
    colnames(data) <- colnames(data, do.NULL = FALSE, prefix = "var")
    is.bin <- function(k) all(k[!is.na(k)] %in% c(0, 1))
    nc <- ncol(data)
    if (is.data.frame(data)) {
        type <- sapply(data, data.class)
        type2 <- type
        bin.var <- rep(NA, nc)
        for (i in 1:nc) {
            bin.var[i] <- is.bin(data[, i])
        }
        type[type %in% c("numeric", "integer")] <- "c"
        type[type == "ordered"] <- "o"
        type[type == "character"] <- "n"
        type[type == "factor"] <- "f"
        type[bin.var] <- "b"
        type[type2 == "character"] <- "n"
        type[type2 == "factor"] <- "f"
        names(type) <- NULL
    }
    else {
        if(any(sapply(data, data.class) == "character")){
            stop("\n If data is a matrix class it must be entirely numeric \n")
        }
        type <- rep("c", nc)
    }
    return(type)
}