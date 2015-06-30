from app import app, db
from app.models import Test
from flask import abort, jsonify, request
import datetime
import json

@app.route('/myapp/Tests', methods = ['GET'])
def get_all_Tests():
    entities = Test.query.all()
    return json.dumps([entity.to_dict() for entity in entities])

@app.route('/myapp/Tests/<int:id>', methods = ['GET'])
def get_Test(id):
    entity = Test.query.get(id)
    if not entity:
        abort(404)
    return jsonify(entity.to_dict())

@app.route('/myapp/Tests', methods = ['POST'])
def create_Test():
    entity = Test(
        name = request.json['name']
    )
    db.session.add(entity)
    db.session.commit()
    return jsonify(entity.to_dict()), 201

@app.route('/myapp/Tests/<int:id>', methods = ['PUT'])
def update_Test(id):
    entity = Test.query.get(id)
    if not entity:
        abort(404)
    entity = Test(
        name = request.json['name'],
        id = id
    )
    db.session.merge(entity)
    db.session.commit()
    return jsonify(entity.to_dict()), 200

@app.route('/myapp/Tests/<int:id>', methods = ['DELETE'])
def delete_Test(id):
    entity = Test.query.get(id)
    if not entity:
        abort(404)
    db.session.delete(entity)
    db.session.commit()
    return '', 204
