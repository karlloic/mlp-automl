from flask import Flask, jsonify
from flask_restful import Api
from .resources import DataSet


# Init a Flask app instance
app = Flask(__name__)


@app.route('/index')
def index():
    return jsonify(
        hello='AutoML'
    )


# Add REST resources to the micro service
api = Api(app)
api.add_resource(DataSet, '/dataset/eda')


if __name__ == '__main__':
    app.run(debug=True)
