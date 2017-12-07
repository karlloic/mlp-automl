from celery import Celery, states
from flask.config import Config
from flask_mail import Mail, Message
import pandas as pd
from .config import configure_app, DATA_DIR, BaseConfig
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt


# Initialize a config
# config = Config('../' + DATA_DIR)
# configure_app(config=config)
mail = Mail()
# mail.init_mail(config=config)


# Initialize unbound Celery app
# TODO With this config, only one broker URL can be used for all environments
celery = Celery(__name__, broker=BaseConfig.CELERY_BROKER_URL)


@celery.task(bind=True)
def eda(self, data_path):
    result = {}
    self.update_state(state=states.PENDING)

    X = pd.read_csv(data_path, header=None)

    # Plot and save
    pd.plotting.scatter_matrix(X, figsize=(15, 15), marker='o',
                               hist_kwds={'bins': 20}, s=60, alpha=.8)

    name = data_path.split('/')[-1].split('.')[0]
    plot_path = DATA_DIR + name + '_pairplot.png'
    plt.savefig(plot_path, format='png')

    result['pairplot'] = plot_path

    # Send email notification
    send_email_notification('Task Complete')

    return result


def send_email_notification(message):
    msg = Message(
        'AutoMLK - Task Complete',
        sender='yongn.kamdem@gmail.com',
        recipients=['karlloic@gmail.com']
    )
    msg.body = message
    from automl import app
    app.app_context()
    mail.init_app(app)

    mail.send(msg)

