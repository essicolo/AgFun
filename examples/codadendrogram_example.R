###################
# Load librairies #
###################
# compositions for CoDaDendrogram2 dependencies and ilr computations
library(compositions)

# ilrDefinitio.R to have pretty names for balances
source('https://raw.githubusercontent.com/essicolo/AgFun/master/ilrDefinition.R')

# CoDaDendrogram2, a fork of CoDaDendrogram, to plot ranges, densities and boxplots on horizontal axes
source('https://raw.githubusercontent.com/essicolo/AgFun/master/codadend2.R')


#############
# Load data #
#############
# Fictive ionomic data
ionomes = read.table('https://raw.githubusercontent.com/essicolo/AgFun/master/examples/plant-ionomes.csv', sep=',', header=TRUE)


########
# CoDa #
########
parts = ionomes[, -c(1, 2)] # identify parts
if (!'Fv' %in% colnames(parts)) parts$Fv = 100 - rowSums(parts) # compute the filling value if not already computed
comp = acomp(parts) # close the simplex and provides acomp data class

# The sequential binary partition matrix
sbp = matrix(c(1, 1, 1, 1, 1,-1,
               1, 1,-1,-1,-1, 0,
               1,-1, 0, 0, 0, 0,
               0, 0, 1,-1,-1, 0,
               0, 0, 0, 1,-1, 0),
             ncol=ncol(parts), byrow=TRUE)
colnames(sbp) = colnames(parts)

# compute balances
bal = ilr(comp, V = gsi.buildilrBase(t(sbp)))
ilr_def = ilrDefinition(sbp = sbp, side='-+') # pretty names
colnames(bal) = ilr_def # pretty names as column names

# plot the dendrogram
CoDaDendrogram2(comp=comp, V=gsi.buildilrBase(t(sbp)), 
                equal.height = TRUE, # vertical bars not scaled to relative variance
                range='autoconf', show.range = TRUE, n_digits=2, # range of horizontal bars scaled to confidence intervals
                type='conf', conf.method = 't', conf.level = 0.95, # how to compute confidence intervals
                group=ionomes$Culture) # the vector for grouping

CoDaDendrogram2(comp=comp, V=gsi.buildilrBase(t(sbp)), 
                equal.height = TRUE, # vertical bars not scaled to relative variance
                range='auto', show.range = TRUE, n_digits=2, # range of horizontal bars scaled to data
                type='boxplot',
                group=ionomes$Culture) # the vector for grouping

CoDaDendrogram2(comp=comp, V=gsi.buildilrBase(t(sbp)), 
                equal.height = TRUE, # vertical bars not scaled to relative variance
                range='auto', show.range = TRUE, n_digits=2, # range of horizontal bars scaled to data
                type='density',
                group=ionomes$Culture) # the vector for grouping

