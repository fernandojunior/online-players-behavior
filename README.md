# Online players' behavior

This repository is part of a in progress master's project. Despite the importance of competitive video game in the world, such as the multiplayer online battle arena (MOBA) games, there is still a lack of tools and researches based on behavior analysis to help players improve their skills. Threfore, the project aims to reduce this gap and focuses on finding patterns from the players' behavior to characterize them in relation to performance using machine learning, more specifically, clustering algorithms.

## Data

The game being analyzed is the [League of Legends](http://br.leagueoflegends.com/), a popular MOBA, and the data set (historical matches) we use was provided by Riot Games from its application public interface ([API](https://developer.riotgames.com)). A match history contains data such as game mode type and identification number. It also contains statistics information of each participant in the match. All participant statistics can be found in the [API Documentation](https://developer.riotgames.com/api/methods#!/1064). In the project, we use only 22 participant statistics.

## Source

Two programming languages are used in the project: Python and R. Python to raw and parse the data and R to analyze it.

To raw your own JSON matches:

1. Configure the Riot API Key in **src/python/config.py** to access League of Legends data
2. Access **src/python** directory
3. Run **pip install -r requeriments.txt**
2. Run **python raw.py**
3. All rawed matches can be found in **src/dump** directory

To parse all matches present in *src/dump* direcotry to a CSV file:

1. Access **src/python** directory
2. Run **python parse.py**
3. The CSV file created can be found in **src/data** directory

To analyze the parsed data:

1. Open **src/R/analysis.R** file
2. Change the line **'../data/data.csv'** to read the CSV file created previously
3. Acess **src/R** direcotry
4. Run **Rscript analysis.R**
5. All plots and results can be found in **src/output**

To configure other settings, as participant statistics to parse, edit **src/python/config.py** file.

## Author

Fernando Felix do Nascimento Junior

## Professor adviser

Leandro Balby Marinho

## Keywords

League of Legends, Machine Learning, Players' Behavior, Python, R.

## License

Released under [the MIT license](https://github.com/dndlab/dndlab.github.io/blob/master/LICENSE)

## References

C. C. Aggarwal, C. K. Reddy. "Data Clustering - Algorithms and Applications", CRC Press, 2014

A. Drachen, C. Thurau, J. Togelius, G. Yannakakis, C. Bauckage, "Skill-based differences in spatio-temporal team behaviour in Defence of The Ancients 2 (DotA 2)", Games Media Entertainment (GEM), IEEE, pp. 1-8, 2014.

M. S. El-Nasr, A. Drachen, A. Canossa. "Game Analytics - Maximizing the Value of Player Data", Springer-Verlag, 2013.

W. H. Kruskal and W. A. Wallis, "Use of ranks in one-criterion variance analysis", Journal of the American Statistical Association 47. 1952.

R. Lowry, "Concepts & applications of inferential statistics", 2012. [Online]. http://vassarstats.net/textbook/ch11a.html

H. Y. Ong, S. Deolalikar, M. V. Peng, "Player behavior and optimal team composition in online multiplayer games", ArXiv preprint arXiv:1503.02230v1 [cs.SI], 2015. http://arxiv.org/abs/1503.02230

F. Rioult, J. Mtivieita, B. Helleub, N. Scellesc, C. Durandb, "Mining tracks of competitive video games", AASRI Procedia, Vol. 8, pp. 82-87, 2014.

S. Salvador and P. CHAN, "Determining the number of clusters/segments in hierarchical clustering/segmentation algorithms", Proceedings of the 16th IEEE International Conference on Tools with Artificial Intelligence, pp. 576-584, 2004.

Superdata Research. "eSports market brief 2015". http://www.superdataresearch.com

P. Yang, B. Harrison, D. L. Roberts, "Identifying patterns in combat that are predictive of success in MOBA games", Proceedings of Foundations of Digital Games, 2014.

M. J. Zaki and W. Meira Junior, "Data mining and analysis: Fundamental concepts and algorithms", Cambridge University Press, 2014.
