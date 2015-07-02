# basic imports
import datetime
import json

# flask imports
from flask import abort, Blueprint, jsonify, request

# app imports
from app.db import db
from app.models import User

users = Blueprint('users', __name__, static_folder='../static', static_url_path='')

@users.route('/myapp/Users', methods = ['GET'])
def get_all_Users():
    entities = User.query.all()
    return json.dumps([entity.to_dict() for entity in entities])

@users.route('/myapp/Users/<int:id>', methods = ['GET'])
def get_User(id):
    entity = User.query.get(id)
    if not entity:
        abort(404)
    return jsonify(entity.to_dict())

@users.route('/myapp/Users', methods = ['POST'])
def create_User():
    entity = User(
        name = request.json['name']
    )
    db.session.add(entity)
    db.session.commit()
    return jsonify(entity.to_dict()), 201

@users.route('/myapp/Users/<int:id>', methods = ['PUT'])
def update_User(id):
    entity = User.query.get(id)
    if not entity:
        abort(404)
    entity = User(
        name = request.json['name'],
        id = id
    )
    db.session.merge(entity)
    db.session.commit()
    return jsonify(entity.to_dict()), 200

@users.route('/myapp/Users/<int:id>', methods = ['DELETE'])
def delete_User(id):
    entity = User.query.get(id)
    if not entity:
        abort(404)
    db.session.delete(entity)
    db.session.commit()
    return '', 204
