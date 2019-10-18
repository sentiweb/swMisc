#' Parse command line arguments with the form var=value or var (no value=TRUE)
#'
#' Parse command line arguments.
#' If arguments definition is provided validate them
#'
#' @param defs arguments definition list of arguments (names = name of argument)
#' @param args argument list (used for test)
#'
#' @details
#' \describe{
#'  \item{type}{"logical", "boolean","integer", "choices", can be abbreviated}
#'  \item{required}{TRUE if argument is required}
#'  \item{default}{Default value}
#'  \item{description}{Description of option}
#'  \item{choices}{acceptable choices, if type =="choices"}
#'  \item{multiple}{if TRUE accept multiple values of the same type, comma separated}
#'  \item{range}{if TRUE accept range, with the syntax from-to, can be comma separated }
#'  \item{unique}{ensure uniqueness of values}
#' }
parseArgs = function(defs=NULL, args=NULL) {

    opts = options("cli.args")$cli.args

    if( !is.null(opts) ) {
        message("Using defaut cli.args")
        return(opts)
    }

    if( is.null(args) ) {
        args <- commandArgs(TRUE)
    }

    args = strsplit(args, split='=', fixed=T)

    opts = list()
    if(length(args) > 0) {
        for(i in 1:length(args)) {
            o = args[[i]]
            if(length(o) > 1) {
                value = o[2]
            } else {
                value = T
            }
            opts[[ o[1] ]] = value
        }
    }

    # No validation, simply return arguments
    if( is.null(defs) ) {
        return(opts)
    }

    print_usage = function() {
        h = Map(function(name, def) {
            paste("   - ", name," :", def$type, def$description, ifelse(isTRUE(def$required), "[Required]","[Optional]"))
        }, names(defs), defs)

        cat("Arguments : \n")
        cat(paste(h, collapse = "\n"))
        cat("\n")
    }

    defined.opts = names(defs)

    provided.opts = names(opts)

    if(length(provided.opts) > 0) {
        if( any(is.na(match(provided.opts, defined.opts))) ) {
            provided.opts = provided.opts[ !provided.opts %in% defined.opts]
            print_usage()
            stop(paste("Unknown parameters ", paste(provided.opts, collapse=',')))
        }
    }

    types = c("logical", "bool", 'integer', "choices")

    yes = c("1","T","TRUE","YES","Y")
    no = c("0","F","FALSE","NO","N")

    errors = c()

    add_error = function(err) {
        cat("Adding error", err,"\n")
        errors <<- c(errors, err)
    }

    for(option in defined.opts) {
        def = defs[[option]]
        default.value = def$default
        if(!option %in% provided.opts) {
            if( isTRUE(def$required) ) {
                add_error(paste("Missing required arg", option))
                next()
            }
            if( is.null(default.value) ) {
                next() # No default value for missing parameter, then skip parsing,
            }
            v = default.value
        } else {
            v = opts[[option]]
        }

        if( !is.null(def$type) ) {
            if( is.null(v)  ) {
                # Null value not acceptable
                # if type is defined, null=T/F is needed to accept or not null value
                if( !isTRUE(def$null) ) {
                    add_error(paste("Unexpected null value for arg", option))
                    next()
                }
            } else {

                type = match.arg(def$type, types)

                if( isTRUE(def$multiple) ) {
                    v = unlist(strsplit(v, split=',', fixed=T))
                }

                if(type %in% c("logical", "bool")) {
                    v = toupper(v)
                    i = !v %in% c(yes, no)
                    if( any(i) ) {
                        e =
                        add_error(paste0("Unexpected value ",paste(sQuote(v[i]), collapse=',')," for a logical for arg ",option))
                        next()
                    }
                    v = ifelse(v  %in% yes, TRUE, FALSE)
                }

                if( type == "integer" ) {
                    if( isTRUE(def$range) ) {
                        v = unlist(lapply(strsplit(v, split='-', fixed=T), function(r) {
                            rr = as.integer(r)
                            if(length(rr) == 1) {
                                return(rr)
                            }

                            if( any(is.na(rr)) ) {
                                stop(paste("Expected integer value in range", paste(r, collapse = '-')))
                            }
                            by = 1L
                            if(length(rr) > 2) {
                             by = rr[3]
                            }
                            seq.int(from=rr[1], to=rr[2], by=by)
                        }))
                    }

                    vv = as.integer(v)
                    i = is.na(vv)
                    if( any(i) ) {
                        add_error(paste0("Unexpected value ",paste(sQuote(v[i]), collapse=',')," for a integer for arg ",option))
                        next()
                    }
                    v = vv
                }

                if(type == "choices") {
                    if( is.null(def$choices) ) {
                        stop(paste0("Choices not defined for arg ", option))
                    }
                    i = !(v %in% def$choices)
                    if(any(i)) {
                        add_error(paste0("Unexpected value ",paste(sQuote(v[i]), collapse=',')," among choices: [", paste(def$choices, collapse=','),"] for arg ",option))
                        next()
                    }
                }
            }
            if( isTRUE(def$unique) ) {
                v = unique(v)
            }
            opts[[option]] = v
        }
    }
    if(length(errors) > 0) {
        print_usage()
        stop(paste(errors, collapse = "\n"))
    }
    opts
}

#' Parse string to integer list accepting, comma separated values and range ('min-max') and 'all')
#' @param v string to parse
#' @param acceptable.values list of acceptable values
#' @param use.all accept "all" special keyword, add values in acceptable.values
#' @param unique make values unique in list
parse_int_list = function(v, acceptable.values, use.all=TRUE, unique=TRUE) {
  from = substitute(v)
  v = unlist(strsplit(v, split=',', fixed=T))
  v = unlist(lapply(strsplit(v, split='-', fixed=T), function(r) {
    rr = as.integer(r)
    if(length(rr) == 1) {
      return(rr)
    }

    if( any(is.na(rr)) ) {
      stop(paste("Expected integer value in range", paste(r, collapse = '-')))
    }
    by = 1L
    if(length(rr) > 2) {
      by = rr[3]
    }
    seq.int(from=rr[1], to=rr[2], by=by)
  }))
  if(use.all && "all" %in% v) {
    v = v[v != "all"]
    v = c(v, acceptable.values)
  }
  v = as.integer(v)
  if( any(is.na(v)) ) {
    stop(paste("Unexpected non integer value in ", from))
  }
  i = !v %in% acceptable.values
  if(any(i)) {
    stop(paste("Unexpected value for int :", paste(v[i], collapse = ",")))
  }
  if(unique) {
    v = unique(v)
  }
  v
}

