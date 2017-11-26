import os
import flask_restful
import pandas as pd
from flask import request, send_from_directory, jsonify
from werkzeug.utils import secure_filename
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '../'))
UPLOAD_FOLDER = ROOT_DIR + '/data/'

# Will be used to store jobs and generate job ids
jobs = {}
job_count = 0


class Job(flask_restful.Resource):
    """
    This class represents an eda job. Simply a numpy ndarray
    """

    def __init__(self):
        global job_count

        # Only create new job if POST
        if request.method == 'POST':
            job_count += 1
            self.jobid = job_count
            self.dataset = {
                'data': None,
                'target': None,
                'data_filename': None,
                'target_filename': None
            }
            self.output_path = None

    def get(self, jobid):
        global jobs

        try:
            output_filename = jobs[jobid].split('/')[-1]
        except KeyError:
            print('No job with jobid {} found. Create a job first'.format(jobid))
            pass

        return send_from_directory(UPLOAD_FOLDER, output_filename, mimetype='image/png', as_attachment=True)

    def post(self):
        datafile_path, targetfile_path = self._loadfiles()

        self.dataset['data'] = pd.read_csv(datafile_path, header=None)
        self.dataset['target'] = pd.read_csv(targetfile_path, header=None)

        # Plot
        self._pairplot()

        response = {
            'jobid': self.jobid,
            'status': 202,
            'message': datafile_path.split('/')[-1] + ' successfully uploaded and job started.'
        }

        return jsonify(response)

    def _describe(self):
        pass

    def _pairplot(self):
        global jobs

        pd.plotting.scatter_matrix(self.dataset['data'], figsize=(15, 15), marker='o',
                                   hist_kwds={'bins': 20}, s=60, alpha=.8)

        self.output_path = UPLOAD_FOLDER + self.dataset['data_filename'].split('.')[0] + '_pairplot.png'
        plt.savefig(self.output_path, format='png')

        # Register job in global dict
        jobs[self.jobid] = self.output_path

    def _loadfiles(self):
        if 'data' and 'target' not in request.files:
            return 'No data or target file path'

        datafile = request.files['data']
        targetfile = request.files['target']

        self.dataset['data_filename'] = data_filename = secure_filename(datafile.filename)
        self.dataset['target_filename'] = target_filename = secure_filename(targetfile.filename)

        if data_filename == '' or target_filename == '':
            return 'No data or target selected'

        datafile_path = os.path.join(UPLOAD_FOLDER, data_filename)
        targetfile_path = os.path.join(UPLOAD_FOLDER, target_filename)

        if datafile and targetfile and data_filename and target_filename:
            if not os.path.exists(UPLOAD_FOLDER):
                os.mkdir(UPLOAD_FOLDER)
            datafile.save(datafile_path)
            targetfile.save(targetfile_path)

        return datafile_path, targetfile_path

