from flask import Flask
from flask.ext.sqlalchemy import SQLAlchemy

# static_url_path: url to access the static folder.
# The default name of static folder is 'static'.
# Use static_folder arg to change it.
app = Flask(__name__, static_url_path='')
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:////tmp/test.db'

# used to enc session cookies
app.config['SECRET_KEY'] = "A0Zr98j/3yX R~XHH!jmN]LWX/,?RT"

db = SQLAlchemy(app)

# routes
from app import routes