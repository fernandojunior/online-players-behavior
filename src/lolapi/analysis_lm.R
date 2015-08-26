classified_data = ldata2[,5:19]

linear_fit = lm(Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt
+ MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions +
TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
LargestMultiKill + LargestKillingSpree + LargestCritStrike, classified_data)

residual <- resid(linear_fit)

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3307,	Adjusted R-squared:  0.3306 

summary(lm(formula = Cluster ~ Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3255,	Adjusted R-squared:  0.3254 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3305,	Adjusted R-squared:  0.3304 

summary(lm(formula = Cluster ~ Kills + Deaths + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3272,	Adjusted R-squared:  0.3271

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3296,	Adjusted R-squared:  0.3295 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3262,	Adjusted R-squared:  0.3261 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3268,	Adjusted R-squared:  0.3267 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:   0.31,	Adjusted R-squared:  0.3098 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3287,	Adjusted R-squared:  0.3286 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3241,	Adjusted R-squared:  0.324 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.2069,	Adjusted R-squared:  0.2068 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3267,	Adjusted R-squared:  0.3266 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3291,	Adjusted R-squared:  0.329 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.2993,	Adjusted R-squared:  0.2992 

summary(lm(formula = Cluster ~ Kills + Deaths + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree, 
    data = classified_data))

# Multiple R-squared:  0.3277,	Adjusted R-squared:  0.3276 

## step 2

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3305,	Adjusted R-squared:  0.3304 

summary(lm(formula = Cluster ~  GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3248,	Adjusted R-squared:  0.3247 

summary(lm(formula = Cluster ~ Kills + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3268,	Adjusted R-squared:  0.3267 

summary(lm(formula = Cluster ~ Kills + GoldEarned + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3294,	Adjusted R-squared:  0.3293 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3259,	Adjusted R-squared:  0.3258 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3265,	Adjusted R-squared:  0.3264 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3085,	Adjusted R-squared:  0.3084 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3285,	Adjusted R-squared:  0.3284 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.324,	Adjusted R-squared:  0.3239 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + TowerKills + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.2069,	Adjusted R-squared:  0.2068 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + 
    LargestMultiKill + LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.3263,	Adjusted R-squared:  0.3262 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestKillingSpree + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.329,	Adjusted R-squared:  0.3289 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestCritStrike, 
    data = classified_data))

# Multiple R-squared:  0.2918,	Adjusted R-squared:  0.2917 

summary(lm(formula = Cluster ~ Kills + GoldEarned + TotalDamageDealt + 
    MagicDamageDealt + PhysicalDamageDealt + TotalDamageDealtToChampions + 
    TotalDamageTaken + MinionsKilled + WardsPlaced + TowerKills + 
    LargestMultiKill + LargestKillingSpree, 
    data = classified_data))

# Multiple R-squared:  0.3273,	Adjusted R-squared:  0.3272 