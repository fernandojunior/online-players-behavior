df <- read.table("training_full_v2.csv", head=F, sep= ",")

head(df[df$V1==1, 5])

win.kills = df[df$V1==1, 5]

loss.kills = df[df$V1==0, 5]

kills = df[, 5]

winlose = df[, 1]


hist(head(win.kills, 5000))
hist(head(loss.kills, 5000))

# um jogador responsavel por todas as kills
pro = df[df$V5>0.9999 & df$V1==1,]



win.kills[win.kills$]



t = head(df[df$V1==1,], 5000)

length(t[t$V5<0.6, 5])

t = head(df[df$V1==0,], 5000)
length(t[t$V5<0.6, 5])

df[df$V1==1 & df$V5<0.6, 5]


plot(density(loss.kills))

lines(density(win.kills), col = "red")
