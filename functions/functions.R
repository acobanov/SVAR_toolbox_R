get_SZ <- function(Y){



##when we want to impose sign and zero restrictions at several horizons 
##we stack the IRFs into single matrix : impact pattern matrix (Y)
##each coloumn of impact pattern matrix represents restrictions for
##given structural shock (for all horizons that restriczions are
##                         %imposed)
##number of rows and columns of impact pattern matrix
number_of_rows=dim(Y)[1];
number_of_columns=dim(Y)[2];

#------------------------------------------------------------------------------------------------------------------------------
# SIGN RESTRICTIONS:
#-----------------------------------------------------------------------------------------------------------------------------
# The sign restrictions are represented as list SS.
# Each list element (S) is a selection matrix for a given structural shock.
# If there are no sign restrictions, S matrix doesn't exist.
# The dimension of S matrix is c x (n*r), where:
#     c is number of sign restrictions for a given structural shock, 
#     n is total number of endogenous variables and
#     r is total number of types of imposed restrictions 
# For an example if we set the zero and/or sign restrictions on the IRFs at the impact and 1st horizon, the r is equal to 2,
# since the restrictions are imposed on 2 horizons.
#-----------------------------------------------------------------------------------------------------------------------------
    SS=list();
    
    for (j in c(1:number_of_columns))  ##for each structural shock
        ##number of sign restrictions for j-th structural shock
        check_sign=(Y(:,j)==1 | Y(:,j)==-1);
        c=sum(check_sign); 
        if c~=0 ##if there is at least one sign restriction for given shock
            ##initialize selection matrix to zero matrix
            S=zeros(c,number_of_rows);
            d=1;
            for i=1: number_of_rows
                if (Y(i,j)==1) | (Y(i,j)==-1)
                    S(d,i)=Y(i,j); 
                    ##skip to next row (exactly one non-zero entry in each
                    ##row)
                    d=d+1;
                end
            end    
            SS{j,1}=S;
            clear S;
        end
        if c==0
            SS{j,1}=zeros(0,number_of_rows);
        end
            
    end 
    
    ##ZERO RESTRICTIONS:
    %representation of zero restrictions: cell array ZZ
    %each element of cell array ZZ is selection matrix for given structural
    %shock
    ZZ=cell(number_of_columns,1);
    for j=1:number_of_columns % for each structural shock
        %number of zero restrictions for j-th structural shock
        c=sum(Y(:,j)==0);
        if c~=0 %if there is at least one zero restriction for given shock
            Z=zeros(c,number_of_rows);
            d=1;
            for i=1:number_of_rows
                if (Y(i,j)==0)
                    Z(d,i)=1;
                    d=d+1;
                end
            end
            ZZ{j,1}=Z;
            clear Z;
        end
        if c==0
            ZZ{j,1}=zeros(0,number_of_rows);
        end
    end
    
}