library(here)
library(randomForestSRC)
library(ggRandomForests)

laggyAggy <- read.csv(here("data","laggyAggy.csv"))

yemenRfsrc <- rfsrc(logFlow ~ flowLag1+dist+Origin+Destination+DestConflict+NumMonths+OrigConflict+
                      DestFood+OrigFood+DestFuel+OrigFuel+OrigConflict3+DestConflict3,data=laggyAggy)

gg_v_yemen <- gg_variable(yemenRfsrc)
gg_md_yemen <- gg_minimal_depth(yemenRfsrc)
xvar_yemen <- gg_md_yemen$topvars

#minimal depth plot

plot(gg_md_yemen) + theme_bw() + theme(text = element_text(size=18))


##interaction plots
#interactionYemen <- find.interaction(yemenRfsrc)
#write.csv(interactionYemen,here("data","interactionYemen.csv"))
interactionYemen <- read.csv(here("data","interactionYemen.csv"))
plot(gg_interaction(interactionYemen), xvar=gg_md_yemen$topvars, panel=TRUE) +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))


