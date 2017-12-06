from flask import Flask, jsonify
from flask_mail import Mail, Message
from flask_restful import Api
from .resources import Job
from .tasks import celery, mail
from .config import configure_app


# Init a Flask app instance
app = Flask(__name__)

# Load flask config
configure_app(app)

# Bind to mail
mail.init_app(app)
app.app_context()

# Initialize Celery app with config from Flask instance
celery.conf.update(app.config)


@app.route('/', methods=['GET', 'POST'])
def index():
    return jsonify(
        hello='AutoML'
    )


# Add REST resources to the micro service
api = Api(app)
api.add_resource(Job, '/job/', '/job/<string:jobid>')


if __name__ == '__main__':
    app.run(debug=True)
