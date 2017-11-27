from flask import Flask, jsonify
from flask_restful import Api
from .resources import Job
from .tasks import celery
from .config import config, configure_app


# Init a Flask app instance
app = Flask(__name__)

# Load flask config
# app.config.from_pyfile('config.py')
app = configure_app(app)


# Initialize Celery app
# celery = Celery(app.name, broker=app.config['CELERY_BROKER_URL'])
celery.conf.update(app.config)  # Takes in additional config from Flask instance


@app.route('/index', methods=['GET', 'POST'])
def index():
    return jsonify(
        hello='AutoML'
    )


# Add REST resources to the micro service
api = Api(app)
api.add_resource(Job, '/job/', '/job/<string:jobid>')


if __name__ == '__main__':
    app.run(debug=True)
