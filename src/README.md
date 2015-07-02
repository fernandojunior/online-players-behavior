# Web project skeleton

A repository that contains a skeleton web project with python and flask. 
The structure of the project and some files are based on a yeoman generator (generator-angular-flask).

## How to

Some basics steps to reproduce this project:

```sh

# Install pip
sudo apt-get install python-pip

# Install virtualenv
pip install virtualenv

# Create virtualenv for project
virtualenv python

# Install Flask
python/bin/pip install Flask

# Install SQLAlchemy
python/bin/pip install SQLAlchemy

#Install SQLAlchemy plugin for flask
python/bin/pip install flask-sqlalchemy

```

## TODO

Things to see later:

* sqlalchemy-migrate
* flask-whooshalchemy
* http://learnpythonthehardway.org/book/ex46.html
* Geradores de API como o Python­EVE ou o Flask­API
* Autenticação e autorização com Flask­Login ou Flask­Security
* definir um endpoint dinâmico para os arquivos estáticos e desta forma hospedar
em um CDN ou em serviços externos como Amazon S3, Akamai ou Dropbox
* ipython

## References

* http://flask-sqlalchemy.pocoo.org/2.0/
* https://github.com/rayokota/generator-angular-flask

## License

Released under [the MIT license](https://github.com/dndlab/dndlab.github.io/blob/master/LICENSE)
