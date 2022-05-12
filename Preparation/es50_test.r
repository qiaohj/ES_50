library(entropart)
data(Paracou618)
# Ns is the total number of trees per species
Ns <- as.AbdVector(Paracou618.MC$Ns)
# Species probabilities
Ps <- as.ProbaVector(Paracou618.MC$Ns)
# Whittaker plot
plot(Ns)
Hurlbert(Ns, 2)

k<-2
N <- sum(Ns)
S <- length(Ns)
# Use lchoose and differences to avoid Inf
Ns<-c(5, rep(0, 4))
N <- sum(Ns)
S <- length(Ns)
lcNk <- lchoose(N, 5)
index <- S - sum(exp(lchoose(N-Ns, 5)-lcNk))
index

df<-data.frame(sp_id=paste("sp", c(1:5)), n_observation=Ns)
p1<-ggplot(df, aes(x="", y=n_observation, fill=sp_id)) +
  geom_bar(stat="identity", width=1) +coord_polar("y", start=0)+
  ggtitle(sprintf("ES5=%.2f", index))+
  scale_fill_discrete(labels=sprintf("%s=%d", df$sp_id, df$n_observation))
p1

Ns<-c(5, rep(1, 4))
N <- sum(Ns)
S <- length(Ns)
lcNk <- lchoose(N, 5)
index <- S - sum(exp(lchoose(N-Ns, 5)-lcNk))
index

df<-data.frame(sp_id=paste("sp", c(1:5)), n_observation=Ns)
p5<-ggplot(df, aes(x="", y=n_observation, fill=sp_id)) +
  geom_bar(stat="identity", width=1) +coord_polar("y", start=0)+
  ggtitle(sprintf("ES5=%.2f", index))+
  scale_fill_discrete(labels=sprintf("%s=%d", df$sp_id, df$n_observation))
p5

Ns<-c(5, rep(5, 4))
N <- sum(Ns)
S <- length(Ns)
lcNk <- lchoose(N, 5)
index <- S - sum(exp(lchoose(N-Ns, 5)-lcNk))
index

df<-data.frame(sp_id=paste("sp", c(1:5)), n_observation=Ns)
p2<-ggplot(df, aes(x="", y=n_observation, fill=sp_id)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ggtitle(sprintf("ES5=%.2f", index))+
  scale_fill_discrete(labels=sprintf("%s=%d", df$sp_id, df$n_observation))
p2

Ns<-c(1, rep(1, 4))
N <- sum(Ns)
S <- length(Ns)
lcNk <- lchoose(N, 5)
index <- S - sum(exp(lchoose(N-Ns, 5)-lcNk))
index

df<-data.frame(sp_id=paste("sp", c(1:5)), n_observation=Ns)
p3<-ggplot(df, aes(x="", y=n_observation, fill=sp_id)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ggtitle(sprintf("ES5=%.2f", index))+
  scale_fill_discrete(labels=sprintf("%s=%d", df$sp_id, df$n_observation))
p3

Ns<-c(100, rep(100, 4))
N <- sum(Ns)
S <- length(Ns)
lcNk <- lchoose(N, 5)
index <- S - sum(exp(lchoose(N-Ns, 5)-lcNk))
index

df<-data.frame(sp_id=paste("sp", c(1:5)), n_observation=Ns)
p4<-ggplot(df, aes(x="", y=n_observation, fill=sp_id)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ggtitle(sprintf("ES5=%.2f", index))+
  scale_fill_discrete(labels=sprintf("%s=%d", df$sp_id, df$n_observation))
p4

Ns<-c(100, rep(1, 4))
N <- sum(Ns)
S <- length(Ns)
lcNk <- lchoose(N, 5)
index <- S - sum(exp(lchoose(N-Ns, 5)-lcNk))
index

df<-data.frame(sp_id=paste("sp", c(1:5)), n_observation=Ns)
p6<-ggplot(df, aes(x="", y=n_observation, fill=sp_id)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+ggtitle(sprintf("ES5=%.2f", index))+
  scale_fill_discrete(labels=sprintf("%s=%d", df$sp_id, df$n_observation))
p6

pp<-ggpubr::ggarrange(plotlist = list(p1, p5, p2, p3, p4, p6), nrow=2, ncol=3)
pp
