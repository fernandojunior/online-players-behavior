from flask import Blueprint, make_response, jsonify

basics = Blueprint('basics', __name__, static_folder='../static', static_url_path='')

@basics.route('/')
def index():
    return basics.send_static_file('index.html')

@basics.errorhandler(400)
def bad_request(error):
    return make_response(jsonify( { 'error': 'Bad request' } ), 400)

# error handlers based on status code

@basics.errorhandler(404)
def not_found(error):
    return make_response(jsonify( { 'error': 'Not found' } ), 404)

