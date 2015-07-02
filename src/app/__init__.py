from flask import Flask
from flask.ext.sqlalchemy import SQLAlchemy

# static_url_path: url to access the static folder.
# The default name of static folder is 'static'.
# Use static_folder arg to change it.
app = Flask(__name__, static_folder='static', static_url_path='')

# db
app.config['SQLALCHEMY_DATABASE_URI'] = 'sqlite:////tmp/test.db'
# used to enc session cookies
app.config['SECRET_KEY'] = "A0Zr98j/3yX R~XHH!jmN]LWX/,?RT"
# port
# debug

db = SQLAlchemy(app)

from app import views

# blueprints for routes
app.register_blueprint(views.basics)
app.register_blueprint(views.tests)
app.register_blueprint(views.users)
