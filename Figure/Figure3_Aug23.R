library(patchwork)

source("./Figure3A_Aug23.R")

source("./Figure3B_Aug23.R")
source("./Figure3C_Aug23.R")
source("./Figure3D_Aug23.R")


fig.3a/fig.3b/((fig.3c+fig.3d)+plot_layout(guides = "collect") &
                 theme(legend.position = "bottom"))