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
