
print.tricontext.data.table <- function (x,...) {
    cat ("Formal context:\n")
    cat ("Objects: ")
    print(attr(x,"objects"),...)
    cat ("Attributes: ")
    print(attr(x,"attributes"),...)
    cat("Conditions: ")
    print(attr(x,"conditions"),...)
                                        #    print.data.frame(x,...)
    invisible(x)
}

format.tricontext.data.table <- function (x,...) {
    paste("Formal context:\n",
          "Objects: ",
          format(attr(x,"objects")),
          "Attributes: ",
          format(attr(x,"attributes")),
          "Conditions: ",
          attr(x,"conditions"),
          sep="")
#    format.data.frame(x,...)
}

is.tricontext.data.table <- function(x) {
    inherits(x, "tricontext.data.table")
}

null.tricontext.data.table <- function() {
    x = structure(null.data.table(),
        objects = NULL,
        attributes = NULL,
        compare = function(x,y) all (x<y)
                  )
}

# get the extent context
collapse_objects <- function(x) {
    columns = names(x);
    columns <- columns[columns != attr(x,"objectcol")]
    x[,c(lapply(.SD,min),list(list(.objects))),.SDcols=columsns]
}

# get the intent context
collapse_attributes <- function(x) {
    columns = names(x);
    columns <- columns[columns != attr(x,"objectcol")]
    x[,.(conditions=base::min(.SD)),
      by=attr(x,"objectcol"),
      .SDcols=columns]
}

# hedge
collapse_conditions <- function(x) {
    columns = names(x);
    columns <- columns[columns != attr(x,"objectcol")]
    x[,.SD>0,by=attr(x,"objectcol"),.SDcols=columns]
}

"_tricontext.data.table" <- function(x,
                                     objects,
                                     attributes,
                                     conditions,
                                     objectname,
                                     attributename,
                                     conditionname,
                                     collapse) {
    setattr(x,"class",c("tricontext.data.table", attr(x,"class")))
    setattr(x,"objects",objects)
    setattr(x,"attributes",attributes)
    setattr(x,"conditions",conditions)
    setattr(x,"objectname",objectname)
    setattr(x,"attributename",attributename)
    setattr(x,"conditionname",conditionname)
    setattr(x,"collapse_objects",    collapse$objects)
    setattr(x,"collapse_attributes", collapse$attributes)
    setattr(x,"collapse_conditions",     collapse$conditions)
    setattr(x,"type", "adjacency")
    if (is.null(objects))
        stop ("This type of tricontext is currently not supported")
        objects = rownames(x)
    if (is.null(objects))
        objects = 1:nrow(x)
    if (is.character(objects) && !is.list(objects) && length(objects) == 1) {
        stop ("This type of tricontext is currently not supported")
        if (!(objects %in% names(x)))
            stop("objects must be either NULL, or a list, or a column name",
                 call = TRUE)
        setattr(x,"objectcol",objects)
        setattr(x,"objects", x[,c(objects), with = FALSE])
    } else {
        if (attr(x,"type")!= "adjacency") {
            stop ("This type of tricontext is currently not supported")
            x[,".objects":=objects, with=FALSE]
            setattr(x,"objectcol",".objects")
        }
    }
    #if (!is.null(objectname)) x[,objects:=objectname]
    return(x)
}

#' Convert a data.table into a triadic context
#'
#' This function adds the necessary information in order to
#' convert a data table into a triadic context
#'
#' @param x A valid description of a data.table object.
#'          It shuold contain at three columns, which are
#'          considered as objects, attributes and conditions,
#'          respectively.
#'
#'          See @ref data.table for further reference.
#' @param objects either NULL or a list of objects. This list
#'                must be a superset of the objects that are
#'                actually used in the objects column.
#'
#'               If NULL, the values from the object column ar used as a guess.
#' @param attributes either NULL or a list of attributes.
#'                   This list must be a superset of the attributes
#'                   that are contained in the attribute column.
#'
#'                   If NULL the values from the attibute column are used.
#' @param conditions either NULL or al list of conditions. This list must
#'                   be a superset of the conditions in the condition column.
#'
#'                   If NULL the values from the condition column are used.
#' @param objectname Name of the object column
#' @param attributename Name of the attribute column
#' @param conditionname Name of the condition column
#' @param collapse_obj  currently unused
#'                      This parameter will be used to avoid scaling of
#'                      many-valueed triadic contexts
#' @param collapse_attr currently unused
#'                      This parameter will be used to avoid scaling of
#'                      many-valueed triadic contexts
#' @param collapse_cond currently unused
#'                      This parameter will be used to avoid scaling of
#'                      many-valueed triadic contexts
#' @return a triadic formal context
#' @export
tricontext.data.table <- function(x=NULL,
                                  ...,
                                  objects = NULL,
                                  attributes = NULL,
                                  conditions = NULL,
                                  objectname = NULL,
                                  attributename = NULL,
                                  conditionname = NULL,
                                  collapse_obj  = collapse_objects,
                                  collapse_attr = collapse_attributes,
                                  collapse_cond  = collapse_conditions) {
    if (is.tricontext.data.table(x)) return (x)
    if (is.null(x)) {
        x <- data.table(...,
                        keep.rownames=TRUE,
                        check.names = TRUE)
    } else {
        x <- data.table(x,
                        ...,
                        keep.rownames=TRUE,
                        check.names = TRUE)
    }
    return(`_tricontext.data.table`(x,
                                    objects,
                                    attributes,
                                    conditions,
                                    objectname,
                                    attributename,
                                    conditionname,
                                    list(
                                        objects    = collapse_obj,
                                        attributes = collapse_attr,
                                        conditions     = collapse_cond)
                                    )
           )
}

as.tricontext.data.table <- function(x=NULL,
                                     ...,
                                     objects = NULL,
                                     attributes = NULL,
                                     conditions = NULL,
                                     collapse_obj  = collapse_objects,
                                     collapse_attr = collapse_attributes,
                                     collapes_cond  = collapse_conditions,
                                     do_copy = FALSE) {
    if (is.tricontext.data.table(x)) return (x)
    if (is.null(x)) {
        x = as.data.table(...,keep.rownames=TRUE, check.names = TRUE)
    } else {
        x = as.data.table(x,...,keep.rownames=TRUE, check.names = TRUE)
    }
    if (do_copy) x = copy(x)
    return(`_tricontext.data.table`(x,
                                    objects,
                                    attributes,
                                    conditions,
                                    list(
                                        objects    = collapse_obj,
                                        attributes = collapse_attr,
                                        conditions     = collapes_cond)
                                    )
           )
}

get.objects <- function(x) {
    y = attr(x,"objects")
    return (x[,attr(x,"objectcol")])
}

is.triextent.data.table <- function(x) inherits (x,"triextent.data.table")
is.triintent.data.table <- function(x) inherits (x,"triintent.data.table")

#' Create an extent from some object.
#' @param x description of the object
#' A see extent.tricontext.data.table for information about creating an
#' extent from a list of objects.
#' @return a formal triadic extent
#' @export
extent <- function (x,...) {
    UseMethod("extent")
}

#' Create an intent from some object.
#' @param x description of the object
#' A see intent.tricontext.data.table for information about creating an
#' intent from a list of objects.
#' @return a formal triadic intent
#' @export
intent <- function (x,...) {
    UseMethod("intent")
}

#' Create an modus from some object.
#' @param x description of the object
#' A see modus.tricontext.data.table for information about creating an
#' modus from a list of objects.
#' @return a formal triadic modus
#' @export
modus <- function (x,...) {
    UseMethod("modus")
}

#' Get a set of condition/object pairs depending on a givon R object
#' @param x description of the object
#' A see modextent.triintent.data.table for information about creating an
#' a partial triadic derivation of an intent.
#' @return a an object representing condition/object pairs
#' @export
modextent <- function (x,...) {
    UseMethod("modextent")
}

#' Get a set of condition/attribute pairs depending on a givon R object
#' @param x description of the object
#' A see modintent.triintent.data.table for information about creating an
#' a partial triadic derivation of an extent.
#' @return a an object representing condition/attribute pairs
#' @export
modintent <- function (x,...) {
    UseMethod("modintent")
}

#' Get a set of object/attribute pairs depending on a givon R object
#' @param x description of the object
#' A see extintent.trimodus.data.table for information about creating an
#' a partial triadic derivation of an modus.
#' @return a an object representing object/attribute pairs
#' @export
extintent <- function (x,...) {
    UseMethod("extintent")
}

#' Create an extent from some object.
#' @param x description of the object
#' A see extent.tricontext.data.table for information about creating an
#' extent from a list of objects.
#' @return a formal triadic extent
#' @export
concept <- function (x,...) {
    UseMethod("concept")
}

#' @export
extent.triextent.data.table <- function(x) x
#' @export
intent.triintent.data.table <- function(x) x
#' @export
modus.trimodus.data.table <- function(x) x
#' @export
modextent.trimodextent.data.table <- function(x) x
#' @export
modintent.trimodintent.data.table <- function(x) x
#' @export
extintent.triextintent.data.table <- function(x) x
#' @export
concept.triconcept.data.table <- function (x) x

#' Convert a list of object names into a possible formal extent of a
#' triadic context
#'
#' In fact this is a lazy definiton of an extent. The object set is
#' not necessarily the extent of a formal triconcept. This function
#' creates the link between a set of object names and a formal
#' tricontext.
#' @param context formal tricontext, the possible extent should be
#'     associated with
#' @param objects a subset of objects of the formal tricontext context
#' @return a formal triextent
#' @export
extent.tricontext.data.table <- function (context, objects) {
    cxt_objects = attr(context,"objects")
    if (length(setdiff(objects,cxt_objects))) {
        stop ("The following objects have not been found:",
              setdiff(objects,cxt_objects),
              call.=TRUE)
    }
    objects <- as.data.table(objects)
    setnames(objects,attr(context,"objectname"))
    structure(NA,
              context = context,
              objects = objects,
              class = "triextent.data.table")
}

#' @export
print.triextent.data.table <- function(x,...) {
    cat("Triadic extent: ")
    print(attr(x,"objects"),...)
    invisible(x)
}

#' @export
format.triextent.data.table <- function(x,...) {
    paste("Triadic extent: ",
          format(attr(x,"objects"),...),
          sep="")
}

#' Convert a list of attribute names into a possible formal intent of a
#' triadic context
#'
#' In fact this is a lazy definiton of an intent. The attribute set is
#' not necessarily the intent of a formal triconcept. This function
#' creates the link between a set of attribute names and a formal
#' tricontext.
#' @param context formal tricontext, the possible intent should be
#'     associated with
#' @param attributes a subset of attributes of the formal tricontext context
#' @return a formal triintent
#' @export
intent.tricontext.data.table <- function (context, attributes) {
    cxt_attributes = attr(context,"attributes")
    if (length(setdiff(attributes,cxt_attributes))) {
        stop ("The following objects have not been found:",
              setdiff(attributes,cxt_attributes),
              call.=TRUE)
    }
    attributes <- as.data.table(attributes)
    setnames(attributes,attr(context,"attributename"))
    structure(NA,
              context = context,
              attributes = attributes,
              class = "triintent.data.table")
}

#' @export
print.triintent.data.table <- function(x,...) {
    cat("Triadic intent: ")
    print(attr(x,"attributes"),...)
    invisible(x)
}

#' @export
format.triintent.data.table <- function(x,...) {
    paste("Triadic intent: ",
          format(attr(x,"attributes"),...),
          sep="")
}


#' Convert a list of condition names into a possible formal modus of a
#' triadic context
#'
#' In fact this is a lazy definiton of an modus. The condition set is
#' not necessarily the modus of a formal triconcept. This function
#' creates the link between a set of condition names and a formal
#' tricontext.
#' @param context formal tricontext, the possible modus should be
#'     associated with
#' @param conditions a subset of conditions of the formal tricontext context
#' @return a formal trimodus
#' @export
modus.tricontext.data.table <- function (context, conditions) {
    cxt_conditions = attr(context,"conditions")
    if (length(setdiff(conditions,cxt_conditions))) {
        stop ("The following objects have not been found:",
              setdiff(conditions,cxt_conditions),
              call.=TRUE)
    }
    conditions <- as.data.table(conditions)
    setnames(conditions,attr(context,"conditionname"))
    structure(NA,
              context = context,
              conditions = conditions,
              class = "trimodus.data.table")
}

#' @export
print.trimodus.data.table <- function(x,...) {
    cat("Triadic modus: ")
    print(attr(x,"conditions"),...)
    invisible(x)
}

#' @export
format.trimodus.data.table <- function(x,...) {
    paste("Triadic modus: ",
          format(attr(x,"conditions"),...),
          sep="")
}

#'@export
"_derive_1D_2D" <- function(cxt, given, names) {
    mergenames <- names[2:3]
    names(mergenames) <- mergenames
    reduced <- Reduce(function(x,y) {
        x[[2]] <- as.data.table(x[[1]][eval(as.name(names[1]))==y,mergenames,with=FALSE])
        x[[1]] <- as.data.table(x[[1]][eval(as.name(names[1]))!=y][x[[2]],,on=mergenames])
        return(x)
    },given,list(cxt,NULL))
    reduced[[2]][,mergenames,with=FALSE]
}

#'@export
"_derive_1D_2D2" <- function(cxt, given, names) {
    mergenames <- names[2:3]
    names(mergenames) <- mergenames
    querynames <- c(names[1])
    names(querynames) <- querynames
    given <- data.table(given)
    setnames(given,querynames)
    fullcount = nrow(given)
    (cxt[given,on=querynames]
        [,ifelse(.N==fullcount,.SD[1,],.SD[FALSE,]),
         by=mergenames]
        [,mergenames,with=FALSE])
}

#'@export
"_derive_2D_1D" <- function(cxt, given, names) {
    reduced <- Reduce(function(x,y) {
        x[[2]] <- x[[1]][y,names[3],with=FALSE, on = names[1:2]]
        x[[1]] <-
            x[[1]][x[[2]],,on=names[3]][(eval(as.name(names[1]))!= y[,names[1]]) |
                       (eval(as.name(names[2]))!=y[,names[2]])
                                 ]
        return(x)
    },
    apply(given,1,function(x) as.data.table(as.list(x))),
                      list(setkeyv(as.data.table(copy(cxt)),names[1:2]),NULL))
    reduced[[2]]
}

#'@export
"_derive_2D_1D2" <- function(cxt, given, names) {
    tmp <- apply(given,1,function(x) as.data.table(cxt[as.data.table(as.list(x)),names[3],with=FALSE]))
    intersect <- function(x,y) {
        x[y,nomatch=0,on=names[3]]
    }
    Reduce(intersect,tmp,tmp[[1]])
}

#'@export
"_derive_2D_1D3" <- function(cxt, given, names) {
    if (!nrow(given)) {
        return(as.data.table(cxt[,FALSE,by=c(names[3])][,names[3],with=FALSE]))
    }
    intersect2 <- function(x,y) {
        intersect(x,y[[1]])
    }
    n = names[1:2]
    names(n)=names[1:2]
    setnames(as.data.table(cxt[given,.(list(list(eval(as.name(names[3]))))),by=.EACHI,on=names(n)]
     [,Reduce(intersect2,V1,V1[[1]][[1]])]),c(names[3]))
}

#'@export
"_derive_1D_2D3" <- function(cxt, given, names) {
    mergenames <- names[2:3]
    names(mergenames) <- mergenames
    intersect <- function(x,y) {
        x[y,nomatch=0,on=mergenames]
    }
    tmp <- lapply(unlist(given),function(x) {
        as.data.table(cxt[eval(as.name(names[1]))==x,mergenames,with=FALSE])
    }
    )

    Reduce(intersect,tmp,tmp[[1]])
}

#'@export
"_derive_1D_2D4" <- function(cxt, given, names) {
    if (!is.data.table(given)) {
        given <- as.data.table(given)
        setnames(given,c(names[1]))
    }
    mergenames <- names[2:3]
    names(mergenames) <- mergenames
    intersect2 <- function(x,y) {
        x[y,nomatch=0,on=mergenames]
    }
                                        #    print(list(list(list(eval(lapply(mergenames,as.name))))))
                                        #    tmp <- lapply(given,function(x) as.data.table(cxt[eval(as.name(names[1]))==x,mergenames,with=FALSE]))
                                        #    Reduce(intersect2,tmp,tmp[[1]])
    inlist <- function(x) list(list(x))
    if (FALSE) {
        as.data.table(cxt[given,
                          .(set=inlist(.SD)),
                          keyby=.EACHI,
                          on=names[1],
                          .SDcols=mergenames]
                      [,Reduce(intersect2,set,set[[1]][[1]])]
                      )
    } else {
        as.data.table(cxt[given,
                          .(set=inlist(.SD)),
                          keyby=.EACHI,
                          on=names[1]]
                      [,Reduce(intersect2,set,set[[1]][[1]])]
                      [,mergenames,with=FALSE]
                      )
    }
}

#'@export
"_derive_1D_1D_1D" <- function(cxt, one, two, names) {
    if (!is.data.table(one)) {
        one <- as.data.table(one)
        setnames(one,c(names[1]))
    }
    if (!is.data.table(two)) {
        two <- as.data.table(two)
        setnames(two,c(names[2]))
    }
    mergenames <- names[3]
    names(mergenames) <- mergenames
    intersect2 <- function(x,y) {
        if (!is.list(x) || !is.list(y)) return(NA)
        if (!length(x) || !length(y)) return (NA)
        x[y,nomatch=0,on=mergenames]
    }
    myreduce <- function(x) {
        len <- length(x)
        if(len == 0L) return(NA)
        if(len == 1L) return(x[[1]])
        f <- match.fun(intersect2)
        res <- x[[1]]
        for (i in seq_along(x)) {
            res <- f(res,x[[i]])
            if (!is.list(res)) break
            if (!nrow(res)) break
        }
        return(res)
    }
                                        #    print(list(list(list(eval(lapply(mergenames,as.name))))))
                                        #    tmp <- lapply(given,function(x) as.data.table(cxt[eval(as.name(names[1]))==x,mergenames,with=FALSE]))
                                        #    Reduce(intersect2,tmp,tmp[[1]])
    inlist <- function(x) list(x)
    if (FALSE) {
        as.data.table(cxt[given,
                          .(set=inlist(.SD)),
                          keyby=.EACHI,
                          on=names[1],
                          .SDcols=mergenames]
                      [,Reduce(intersect2,set,set[[1]][[1]])]
                      )
    } else {
        (tmp <- cxt[one,
                    .SD,
                    keyby=.EACHI,
                    on=names[1]][two,
             .(setlist=inlist(.SD)),
             keyby=eval(names[1:2]),
             on=names[2],
             .SDcols=names[3]]
            [,myreduce(setlist)])
#        (tmp <- tmp
#            [,Reduce(intersect2,setlist[[1]],setlist[[1]][[1]])])
        if (!is.list(tmp))
            return(setnames(data.table(x=list()),c(names[3])))
        as.data.table(tmp,names=c(names[3]))
    }
}

#' Get the condition/object pairs from a formal triadic context that are
#' shared by a set of given attributes.
#'
#' The result is a triadic equivalent to the binary derivation
#' operation in the (binary) formal context of attributes and the
#' product of the condition and object sets that is derived from the
#' triadic context.
#' @param x a triadic intent.
#' @return a an object representing the condition/object pairs that
#' are shared by all attributes of the triadic intent x
#' @export
modextent.triintent.data.table <- function(x) {
    cxt <- attr(x,"context")
    names <- c(attr(cxt,"attributename"),
               attr(cxt,"conditionname"),
               attr(cxt,"objectname")
               )
    reduced <- `_derive_1D_2D4`(cxt,
                                attr(x,"attributes"),
                                names
                                )
    structure(NA,
              context = cxt,
              conditions = as.data.table(unique(reduced[,names[2],with=FALSE])),
              objects = as.data.table(unique(reduced[,names[3],with=FALSE])),
              pairs = as.data.table(unique(reduced[,
                  names[2:3],with=FALSE])),
              class = "trimodextent.data.table")
}


#' Get the condition/attribute pairs from a formal triadic context that are
#' shared by a set of given attributes.
#'
#' The result is a triadic equivalent to the binary derivation
#' operation in the (binary) formal context of attributes and the
#' product of the condition and attribute sets that is derived from the
#' triadic context.
#' @param x a triadic extent.
#' @return a an object representing the condition/attribute pairs that
#' are shared by all attributes of the triadic extent x
#' @export
modintent.triextent.data.table <- function(x) {
    cxt <- attr(x,"context")
    names <- c(attr(cxt,"objectname"),
               attr(cxt,"attributename"),
               attr(cxt,"conditionname"))
    reduced <- `_derive_1D_2D4`(cxt,
                                attr(x,"objects"),
                                names)
    structure(NA,
              context = cxt,
              attributes = as.data.table(unique(reduced[,names[2],with=FALSE])),
              conditions = as.data.table(unique(reduced[,names[3],with=FALSE])),
              pairs = as.data.table(unique(reduced[,
                  names[2:3],with=FALSE])),
              class = "trimodintent.data.table")
}

#' Get the object/attribute pairs from a formal triadic context that are
#' shared by a set of given attributes.
#'
#' The result is a triadic equivalent to the binary derivation
#' operation in the (binary) formal context of attributes and the
#' product of the object and attribute sets that is derived from the
#' triadic context.
#' @param x a triadic modus.
#' @return a an object representing the object/attribute pairs that
#' are shared by all attributes of the triadic modus x
#' @export
extintent.trimodus.data.table <- function(x) {
    cxt <- attr(x,"context")
    names <- c(attr(cxt,"conditionname"),
               attr(cxt,"objectname"),
               attr(cxt,"attributename")
               )
    reduced <- `_derive_1D_2D4`(cxt,
                               attr(x,"conditions"),
                               names
                               )
    structure(NA,
              context = cxt,
              attributes = as.data.table(unique(reduced[,names[3],with=FALSE])),
              objects = as.data.table(unique(reduced[,names[2],with=FALSE])),
              pairs = as.data.table(unique(reduced[,names[2:3],with=FALSE])),
              class = "triextintent.data.table")
}


#' Get the attributes from a formal triadic context that are
#' shared by a set of given condition/object pairs.
#'
#' The result is a triadic equivalent to the binary derivation
#' operation in the (binary) formal context of attributes and the
#' product of the condition and object sets that is derived from the
#' triadic context.
#' @param x a trimodextent as generated by modextent.
#' @return a triadic intent representing the attributes that
#' are shared by all condition/object pairs of the triadic modextent x
#' @export
intent.trimodextent.data.table <- function(x) {
    cxt <- attr(x,"context")
    names <- c(attr(cxt,"conditionname"),
               attr(cxt,"objectname"),
               attr(cxt,"attributename"))
    reduced <- `_derive_2D_1D3`(cxt,
                               attr(x,"pairs"),
                               names
                               )
    structure(NA,
              context = cxt,
              attributes = reduced,
              class = "triintent.data.table")
}

#' @describeIn intent
#' Get the attributes from a formal triadic context that are shared by
#' all elemets of the set theoretical product of a modus with an
#' extent.
#'
#' The result is the set of all attributes that are connected to all
#' given objects under all given conditions via the incidence relation.
#' @param modus a triadic modus
#' @param extent a triadic extent
#' @return a triadic intent.
#' @export
intent.trimodus.data.table <- function(modus,extent) {
    cxt <- attr(modus,"context")
    if (!is.triextent.data.table(extent))
        stop ("The secound argument must be a triadic extent")
    if (!identical(cxt,attr(extent,"context")))
        stop ("Trying to infer a common intent from a modus and an extent of different contexts")
    reduced <- `_derive_1D_1D_1D`(cxt,
                                  attr(modus,"conditions"),
                                  attr(extent,"objects"),
                                  c(attr(cxt,"conditionname"),
                                    attr(cxt,"objectname"),
                                    attr(cxt,"attributename")))
    structure(NA,
              context = cxt,
              attributes = reduced,
              class = "triintent.data.table")
}

#' @describeIn intent
#' Get the attributes from a formal triadic context that are shared by
#' all elemets of the set theoretical product of a modus with an
#' extent.
#'
#' The result is the set of all attributes that are connected to all
#' given objects under all given conditions via the incidence relation.
#' @param extent a triadic extent
#' @param modus a triadic modus
#' @return a triadic intent.
#' @export
intent.triextent.data.table <- function(extent,modus) intent.trimodus.data.table(modus,extent)

#' @describeIn extent
#' Get the attributes from a formal triadic context that are shared by
#' all elemets of the set theoretical product of a modus with an
#' intent.
#'
#' The result is the set of all attributes that are connected to all
#' given objects under all given conditions via the incidence relation.
#' @param modus a triadic modus
#' @param intent a triadic intent
#' @return a triadic intent.
#' @export
extent.trimodus.data.table <- function(modus,intent) {
    cxt <- attr(modus,"context")
    if (!is.triintent.data.table(intent))
        stop ("The secound argument must be a triadic intent")
    if (!identical(cxt,attr(intent,"context")))
        stop ("Trying to infer a common intent from a modus and an intent of different contexts")
    reduced <- `_derive_1D_1D_1D`(cxt,
                                  attr(modus,"conditions"),
                                  attr(intent,"attributes"),
                                  c(attr(cxt,"conditionname"),
                                    attr(cxt,"attributename"),
                                    attr(cxt,"objectname")))
    structure(NA,
              context = cxt,
              attributes = reduced,
              class = "triextent.data.table")
}

#' @describeIn extent
#' Get the attributes from a formal triadic context that are shared by
#' all elemets of the set theoretical product of a modus with an
#' intent.
#'
#' The result is the set of all attributes that are connected to all
#' given objects under all given conditions via the incidence relation.
#' @param intent a triadic intent
#' @param modus a triadic modus
#' @return a triadic intent.
#' @export
extent.triintent.data.table <- function(intent,modus) intent.trimodus.data.table(modus,intent)

#' @describeIn modus
#' Get the attributes from a formal triadic context that are shared by
#' all elemets of the set theoretical product of a extent with an
#' intent.
#'
#' The result is the set of all attributes that are connected to all
#' given objects under all given conditions via the incidence relation.
#' @param extent a triadic extent
#' @param intent a triadic intent
#' @return a triadic intent.
#' @export
modus.triextent.data.table <- function(extent,intent) {
    cxt <- attr(extent,"context")
    if (!is.triintent.data.table(intent))
        stop ("The secound argument must be a triadic intent")
    if (!identical(cxt,attr(intent,"context")))
        stop ("Trying to infer a common intent from a extent and an intent of different contexts")
    reduced <- `_derive_1D_1D_1D`(cxt,
                                  attr(extent,"objects"),
                                  attr(intent,"attributes"),
                                  c(attr(cxt,"objectname"),
                                    attr(cxt,"attributename"),
                                    attr(cxt,"conditionname")))
    structure(NA,
              context = cxt,
              attributes = reduced,
              class = "trimodus.data.table")
}

#' @describeIn modus
#' Get the attributes from a formal triadic context that are shared by
#' all elemets of the set theoretical product of a extent with an
#' intent.
#'
#' The result is the set of all attributes that are connected to all
#' given objects under all given conditions via the incidence relation.
#' @param intent a triadic intent
#' @param extent a triadic extent
#' @return a triadic intent.
#' @export
modus.triintent.data.table <- function(intent,extent) intent.triextent.data.table(extent,intent)



#' Get the objects from a formal triadic context that are
#' shared by a set of given condition/attribute pairs.
#'
#' The result is a triadic equivalent to the binary derivation
#' operation in the (binary) formal context of objects and the
#' product of the condition and attribute sets that is derived from the
#' triadic context.
#' @param x a trimodintent as generated by modintent.
#' @return a triadic extent representing the objects that
#' are shared by all condition/attribute pairs of the triadic modintent x
#' @export
extent.trimodintent.data.table <- function(x) {
    cxt <- attr(x,"context")
    names <- c(attr(cxt,"attributename"),
               attr(cxt,"conditionname"),
               attr(cxt,"objectname")
               )
    reduced <- `_derive_2D_1D3`(cxt,
                                attr(x,"pairs"),
                                names)
    structure(NA,
              context = cxt,
              objects = reduced,
              class = "triextent.data.table")
}

#' Get the conditions from a formal triadic context that are
#' shared by a set of given object/attribute pairs.
#'
#' The result is a triadic equivalent to the binary derivation
#' operation in the (binary) formal context of attributes and the
#' product of the object and attribute sets that is derived from the
#' triadic context.
#' @param x a triextintent as generated by extintent.
#' @return a modus representing the conditions that
#' are shared by all object/attribute pairs of the triadic extintent x
#' @export
modus.triextintent.data.table <- function(x) {
    cxt <- attr(x,"context")
    names <- c(attr(cxt,"objectname"),
               attr(cxt,"attributename"),
               attr(cxt,"conditionname")
               )
    reduced <- `_derive_2D_1D3`(cxt,
                               attr(x,"pairs"),
                               names
                               )
    structure(NA,
              context = cxt,
              conditions = reduced,
              class = "trimodus.data.table")
}

#' @export
print.trimodextent.data.table <- function(x,...) {
    cat("Triadic modus×extent: ")
    print(attr(x,"conditions"),...)
    print(attr(x,"objects"),...)
    invisible(x)
}

#' @export
format.trimodextent.data.table <- function(x,...) {
    paste("Triadic modus×extent: ",
          format(attr(x,"conditions"),...),
          format(attr(x,"objects"),...),
          sep="")
}

#' @export
print.trimodintent.data.table <- function(x,...) {
    cat("Triadic modus×intent: ")
    print(attr(x,"conditions"),...)
    print(attr(x,"attributes"),...)
    invisible(x)
}

#' @export
format.trimodintent.data.table <- function(x,...) {
    paste("Triadic modus×intent: ",
          format(attr(x,"conditions"),...),
          format(attr(x,"attributes"),...),
          sep="")
}

#' @export
print.triextintent.data.table <- function(x,...) {
    cat("Triadic extent×intent: ")
    print(attr(x,"objects"),...)
    print(attr(x,"attributes"),...)
    invisible(x)
}

#' @export
format.triextintent.data.table <- function(x,...) {
    paste("Triadic extent×intent: ",
          format(attr(x,"objects"),...),
          format(attr(x,"attributes"),...),
          sep="")
}

concept <- function (x,...) {
    UseMethod("concept")
}

subcontext <- function (x,...) {
    UseMethod("subcontext")
}

subcontext.intent.data.table <- function (x) {
    cxt <- attr(x,"context")
    y <- structure(as.tricontext.data.table(cxt[,
                                             c(attr(x,"attributes"),
                                               attr(cxt,"objectcol")),
                                             with=FALSE],
                                         objects=attr(cxt,"objectcol"),
                                         attributes=attr(cxt,"attributes"),
                                         ),
                   context = cxt)
    setattr(y,"class",c("subtricontext.data.table", class(cxt)))
}

subcontext.extent.data.table <- function (x) {
    cxt <- attr(x,"context")
    y <- structure(as.tricontext.data.table(cxt[.objects %in% attr(x,"objects")],
                                         objects=attr(attr(x,"context"),
                                             "objects"),
                                         attributes=attr(attr(x,"context"),
                                             "attributes"),
),
                   context = cxt)
    setattr(y,"class",c("subtricontext.data.table", class(cxt)))
}

format.subtricontext.data.table <- function(x,...) {
    paste("Subcontext:\n",
          format.tricontext.data.table(x,...),
          sep="")
}

print.subtricontext.data.table <- function(x,...) {
    cat("Subcontext:\n")
    print.tricontext.data.table(x,...)
    invisible(x)
}
