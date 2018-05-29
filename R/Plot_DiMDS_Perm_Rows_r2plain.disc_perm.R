#Plot_DiMDS_Perm_Rows_r2plain.disc_perm.R

#'Plot results from test of Null Hypothesis on Rows
#'
#'
#'@param res_DiMDS Output from DiMDS_Perm_Rows
#'@param Title Name the figure
#'@param dev_new Flag to create a new device
#'@return Histogram of the r2 under the null
#'@export


Plot_DiMDS_Perm_Rows_r2plain.disc_perm <- function(res_DiMDS, Title = "r2plain.disc_perm", dev_new = T){

  Plot_Hist(Distribution = res_DiMDS$Perm_Rows$r2plain.disc_perm,
            alpha = .05,
            Stat = res_DiMDS$res_Disc_Full$proj2Bary$r2plain.disc,
            main = Title,
            dev_new = dev_new)
}

