make_BlockExoMatrix <- function (infoVar,numFor) {
    output <- matrix(0,infoVar$nvar,infoVar$nvar)
    insert <- matrix(1,numFor,infoVar$nvar-numFor)
    rows <- c(((infoVar$nvar-numFor)+1):infoVar$nvar)
    cols <- c(1:(infoVar$nvar-numFor))
    output[rows,cols] <- insert;
    return(output)
}

create_SZ <- function (impact,shocks) {
    impacts <- impact %>% 
        do.call(cbind,.) %>% 
        t(.)
    sign_condition <- c(1,-1)
    SS <- apply(impacts,2,function(x) column_to_matrix(x,sign_condition)) 
    names(SS) <- shocks
    zero_condition <- c(0)
    ZZ <- apply(impacts,2,function(x) column_to_matrix(x,zero_condition)) 
    names(ZZ) <- shocks
    output <- list(SS,ZZ)
    names(output) <- c("sign","zero")
    return(output)
}

column_to_matrix <- function (x,condition) {
    # values: vector containing all sign restrictions (1 or -1) for a given vector (shock)
    values <- x[x %in% condition]
    size <- length(values)
    # col_positions: vector containing column positions to fill in matrix with corresponding values
    col_positions <- which(x %in% condition)
    # row_positions: vector containing row positions to fill in matrix with corresponding values 
    row_positions <- c(1:size)
    output_matrix <- matrix(0,size,length(x))
    if (identical(condition,c(1,-1))) {
        if (size>1) {
            diag(output_matrix[row_positions,col_positions]) <- values 
        }
        if (size==1) {
            output_matrix[row_positions,col_positions] <- values 
        }
    }
    
    if (identical(condition,c(0))) {
        if (size>1) {
            diag(output_matrix[row_positions,col_positions]) <- replicate(size,1) 
        }
        if (size==1) {
            output_matrix[row_positions,col_positions] <- replicate(size,1) 
        }
    };
    return(output_matrix)
}
