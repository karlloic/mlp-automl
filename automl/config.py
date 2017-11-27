import os


ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '../'))
DATA_DIR = ROOT_DIR + '/data/'

config = {
    'production': 'automl.config.ProductionConfig',
    'testing': 'automl.config.TestingConfig',
    'dev': 'automl.config.DevelopmentConfig',
    'base': 'automl.config.BaseConfig'
}


class BaseConfig:
    # Debugging
    TESTING = False
    DEBUG = False

    # Database
    SECRET_KEY = 'eyJhbGciOiJIUzI1NiIsImV4cCI6MTM4NTY2OTY1NSwiaWF0IjoxMzg1NjY5MDU1fQ'
    SQLALCHEMY_DATABASE_URI = 'sqlite:///{}/drugapi.sqlite'.format(DATA_DIR)
    SQLALCHEMY_COMMIT_ON_TEARDOWN = True
    SQLALCHEMY_TRACK_MODIFICATIONS = False  # Adds significant overhead. Will be deprecated in the future

    # Flask-Security.
    # We're using PBKDF2 with salt.
    SECURITY_PASSWORD_HASH = 'pbkdf2_sha512'
    SECURITY_PASSWORD_SALT = 'XbOEFJkhjHJ5uRINh2JA1BPzXjSohKYDRT472wGOvjc'
    SECURITY_TOKEN_MAX_AGE = 24 * 60 * 60
    WTF_CSRF_ENABLED = False

    # Set mail-related config values
    SECURITY_EMAIL_SENDER = 'yongn.kamdem@gmail.com'
    MAIL_SERVER = 'email-smtp.us-west-2.amazonaws.com'
    MAIL_PORT = 465
    MAIL_USE_SSL = True
    MAIL_USERNAME = os.environ.get('MAIL_USERNAME')
    MAIL_PASSWORD = os.environ.get('MAIL_PASSWORD')

    # Celery & Redis config
    CELERY_BROKER_URL = 'redis://localhost:6379/0'
    CELERY_RESULT_BACKEND = 'redis://localhost:6379/0'

    @staticmethod
    def init_app(app):
        pass


class DevelopmentConfig(BaseConfig):
    TESTING = True
    DEBUG = True
    SQLALCHEMY_DATABASE_URI = os.environ.get('TEST_FLASK_DATABASE_URL') or 'sqlite:///{}/drugapi.sqlite'.format(DATA_DIR)


class TestingConfig(BaseConfig):
    TESTING = True
    SQLALCHEMY_DATABASE_URI = os.environ.get('TEST_FLASK_DATABASE_URL')


class ProductionConfig(BaseConfig):
    SQLALCHEMY_DATABASE_URI = os.environ.get('FLASK_DATABASE_URL')
    # SQLALCHEMY_DATABASE_URI = \
    #     'postgresql://drugapi:password@drugapidb.c4uqpmwcoexk.us-east-1.rds.amazonaws.com:5432/drugapidb'

    @classmethod
    def init_app(cls, app):
        BaseConfig.init_app()

        # Add additional configs


def configure_app(app):
    config_name = os.getenv('FLASK_CONFIGURATION_MODE', 'dev')
    app.config.from_object(config[config_name])  # object-based default configuration
    app.config.from_pyfile('config.cfg', silent=True)  # instance-folders configuration

    return app


