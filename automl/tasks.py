from celery import Celery, states
import pandas as pd
from .config import DATA_DIR, BaseConfig
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt


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

    return result

