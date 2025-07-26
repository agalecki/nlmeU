##  NOTE: This code pertains to panels R15.5 - R15.7
##  To execute code in this file package lme4 has to be used. 


###################################################
### code chunk Chap15init
###################################################
options(width=65, digits=5, show.signif.stars = FALSE) 
date()
packageVersion("lme4")
packageVersion("Matrix")
sessionInfo()
SeedValue <- 17761
set.seed(SeedValue)

###################################################
### code chunk: R15.1
###################################################
n1 <- 2                 # Number of levels for the factor g1
n2 <- 3                 # Number of levels for the factor g2
i <- gl(n1, n2)         # i 
j <- gl(n2, 1, n1*n2)   # j
b1x <- rnorm(n1, 0, 1)  # b_i
b2x <- rnorm(n2, 0, 2)  # b_j
dt0 <- data.frame(i, j)
(dtc <- 
   within(dt0,
          {             # g1 and g2 are crossed
           eps <- rnorm(nrow(dt0), 0, 0.2)
           b1 <- b1x[i]
           b2 <- b2x[j]
           y <- 10 + b1 + b2 + eps
           g2 <- factor(j, labels = letters[1:n2])
           g1 <- factor(LETTERS[i])
           }))

###################################################
### code chunk: R15.5
###################################################
require(lme4)
fmc <- lmer(y ~ 1 + (1|g1) + (1|g2), data = dtc)
summary(fmc)
print(VarCorr(fmc))
gf <- getME(fmc, "flist")     # Grouping factors
xtabs(~g1 + g2, gf)           # g1 and g2 fuly crossed 
(Zt <- getME(fmc, "Zt"))      # Z'



###################################################
### code chunk: R15.6
###################################################
(S <- getME(fmc, "Lambdat"))                      # Diagonal scale-matrix 
(T <- Diagonal(n = nrow(getME(fmc, "Lambdat"))))  # Unit diagonal matrix T      
# T <- Cholesky(S, super = FALSE, perm = FALSE)   # General syntax will work for non-diagonal matrix S
perm <- Ls@perm
# Convert to 1-based indices
perm_1based <- perm + 1
# Create permutation matrix P
n <- length(perm)
P <- Matrix(0, n, n)
P[cbind(1:n, perm_1based)] <- 1
P <- as(P, "pMatrix")                             # Permutation matrix
print(P)


###################################################
### code chunk: R15.7
###################################################
TS <- T %*% S                      # Product of T and S
(sig <- sigma(fmc))                # sigma 
(D <- sig * sig * tcrossprod(TS))  # D = sigma^2 TSST'13.9), (13.33)
A  <- getME(fmc, "A")              # A matrix (related to Z * Lambda)
ZTS <- t(Zt) %*% TS                # Z*T*S
(max(abs(t(A) - ZTS )))            # verify A' = Z*T*S : (13.34)
Ac <- tcrossprod(A)                # A*A'
(AcI <- Ac + diag(1, nrow(A)))     # A*A' + I
Ls <- getME(fmc, "L")              # Cholesky factor L_Z (13.38)
as(Ls, "sparseMatrix")
PP <- P %*% AcI %*% t(P)           # P*(A*A' + I)*P'
L <- as(Ls, "sparseMatrix")
print(L)
max(abs(tcrossprod(L) - PP))       # L_Z*L_Z' = P*(A*A' + I)*P': (13.38)  

### sessionInfo
sessionInfo()                      # Before detaching packages
detach(package:lme4.0) 

