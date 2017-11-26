from flask import Flask, jsonify
from flask_restful import Api
from .resources import Job, UPLOAD_FOLDER


# Init a Flask app instance
app = Flask(__name__)

# Config
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER


@app.route('/index', methods=['GET', 'POST'])
def index():
    return jsonify(
        hello='AutoML'
    )


# Add REST resources to the micro service
api = Api(app)
api.add_resource(Job, '/job/', '/job/<int:jobid>')


if __name__ == '__main__':
    app.run(debug=True)
