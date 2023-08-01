# Make R package netmeta available
#
library(netmeta)


# Make Linde2016 dataset available
# (use R command 'help(Linde2016)' for more information on dataset)
#
data(Linde2016)


# Define order of treatments in printouts and forest plots
#
trts <- c("TCA", "SSRI", "SNRI", "NRI", "Low-dose SARI", "NaSSa", "rMAO-A",
          "Ind drug", "Hypericum", "Face-to-face CBT", "Face-to-face PST",
          "Face-to-face interpsy", "Face-to-face psychodyn",
          "Other face-to-face", "Remote CBT", "Self-help CBT",
          "No contact CBT", "Face-to-face CBT + SSRI",
          "Face-to-face interpsy + SSRI", "Face-to-face PST + SSRI",
          "UC", "Placebo")


# Standard random effects NMA model (with placebo as reference treatment)
#
net1 <- netmeta(lnOR, selnOR, treat1, treat2, id,
                data = Linde2016, reference.group = "placebo",
                sm = "OR", comb.fixed = FALSE, comb.random = TRUE,
                seq = trts, nchar.trts = 8)
#
net1


# Additive CNMA model with
# - placebo as inactive component and reference
# - automatically generated C matrix
#
nc1 <- netcomb(net1, inactive = "placebo")
# nc1$C.matrix
#
nc1


# Interaction CNMA model with
# - interaction 'Face-to-face PST * SSRI'
# - placebo as inactive component and reference
#
# C matrix with definition of interaction(s) has to be provided:
# - add column to C matrix with 1 in the row 'Face-to-face PST + SSRI'
#   and zeros otherwise
#
C1.int <- cbind(nc1$C.matrix, F2fPST.SSRI = 0)
C1.int["Face-to-face PST + SSRI", "F2fPST.SSRI"] <- 1
# C1.int
#
nc1.int <- netcomb(net1, C.matrix = C1.int, inactive = "placebo")
#
nc1.int


# Combine results of standard NMA and CNMAs
# (with placebo as reference)
#
nb1 <- netbind(nc1, nc1.int, net1,
               name = c("Additive CNMA", "Interaction CNMA",
                        "Standard NMA"),
               col.study = c("red", "blue", "black"),
               col.square = c("red", "blue", "black"))


# Create Figure 2 showing
# - treatment comparisons with Placebo (argument 'reference.group')
# - using Placebo as baseline treatment (default)
#
pdf("forest-placebo.pdf", width = 6, height = 13.5)
#
forest(nb1, xlim = c(0.45, 10), at = c(0.5, 1, 2, 5, 10),
       col.by = "black", addrow.subgroups = FALSE,
       fontsize = 10, spacing = 0.7, squaresize = 0.9,
       label.left = "Favours Placebo",
       label.right = "Favours other")
#
dev.off()
