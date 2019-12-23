library("FactoMineR")
library("factoextra")
library("ggplot2")

load("data_biplot.rda")

# define categories
cats <- apply(df, 2, function(x) nlevels(as.factor(x)))

df_mca <- MCA(df, graph = F,ncp = 3)

df_mca$eig

summary.MCA(df_mca)

# take how much dim1 and dim2 explains
eig <- df_mca$eig
dim1 <- round(eig[1,3], 1)
dim2 <- round(eig[2,3] - eig[1,3], 1)

# get variables
mca_var_df <- data.frame(df_mca$var$coord, 
                         Variable=rep(names(cats), cats))

# data frame with observation coordinates
mca_obs_df <- data.frame(df_mca$ind$coord)

plot(df_mca)
fviz_screeplot(df_mca)
fviz_mca_biplot(df_mca)

#get MCA var 
var <- get_mca_var(df_mca)

var$coord

fviz_mca_var(df_mca)

mca_var_df$Change <- ifelse(grepl("Increased", row.names(mca_var_df)), "Increased",
                            ifelse(grepl("Decreased", row.names(mca_var_df)), "Decreased",
                                   row.names(mca_var_df)))

mca_var_df$Variable <- ifelse(mca_var_df$Variable=="Perception","Farmers' perception",
                              as.character(mca_var_df$Variable))

# MCA plot of observations and categories
ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  ylim(-2,2.5) +
  xlim(-1.7, 1.7) +
  geom_text(data = mca_var_df, 
            mapping = aes(x = Dim.1, y = Dim.2, 
                          label = Change, colour = Variable),
            size=4, vjust=0, hjust=1, show.legend = FALSE) +
  geom_point(data=mca_var_df, aes(x=Dim.1,y=Dim.2,shape=factor(Variable), 
                                  fill=factor(Variable)), size=2) +
  scale_shape_manual(values=c(21,22,23,24)) +
  scale_fill_manual(values=c("#E74C3C","#1E8449","#3498DB","#424949")) +
  scale_colour_manual(values=c("#E74C3C","#1E8449","#3498DB","#424949")) +
  theme_bw() +
  theme(legend.title= element_blank(),
        legend.position = "bottom",
        legend.text= element_text(size=14, colour="black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size = 12, angle = 0, 
                                   hjust = 1, vjust = 1, face = "plain"),
        axis.text.y = element_text(size = 12, angle = 0, 
                                   hjust = 1, vjust = 1, face = "plain")) +
  labs(x = paste0("Dim 1 (", dim1,"%)"), 
       y = paste0("Dim 2 (", dim2,"%)"))


ggsave("biplot.png",
       plot = bp, 
       dpi = 400, 
       width = 20, 
       height = 20 ,
       units = "cm")