import os
import flask_restful
from flask import request, send_from_directory, jsonify
from werkzeug.utils import secure_filename
from celery import states
from .config import DATA_DIR
from .tasks import eda


# Will be used to store jobs and generate job ids. AKA our database
jobs = {}

response = {
    'jobid': -1,
    'status': 202,
    'message': ' successfully uploaded and job started.'
}


class Job(flask_restful.Resource):
    """
    This class represents an eda job. Simply a numpy ndarray
    """

    def __init__(self):

        # Only create new job if POST
        if request.method == 'POST':
            self.jobid = None
            self.data_path = None
            self.target_path = None
            self.result = {}

    def get(self, jobid):
        global jobs, response

        response['status'] = 404
        response['message'] = 'No job with jobid {} found. Create a job first'.format(jobid)

        # Check status of celery task
        task = eda.AsyncResult(jobid)
        state = task.state

        if state == states.SUCCESS:
            self.result = task.get()
            jobs[jobid] = self.result
        elif state == states.PENDING:
            response['status'] = 202
            response['message'] = 'Job not complete.'
            return jsonify(response)
        elif state == states.FAILURE:
            try:
                response['status'] = 400
                response['message'] = task.info.get('error')
            except:
                response['message'] = 'Job failed. Unknown error occurred.'
                return jsonify(response)

        # TODO Returns only pairplot for now. TO generalize
        # If job was successful,
        try:
            output_filename = jobs[jobid]['pairplot'].split('/')[-1]
        except KeyError:
            return jsonify(response)
            pass

        return send_from_directory(DATA_DIR, output_filename, mimetype='image/png', as_attachment=True)

    def post(self):
        global response

        # Load data files
        try:
            self.data_path, self.target_path = self._uploadfiles()
            response['message'] = self.data_path.split('/')[-1] + response['message']
        except ValueError:  # None returned for unsuccessful file load
            response['status'] = 400
            response['message'] = 'Failed to upload data file. Job canceled.'
            return jsonify(response)

        # Send EDA task to celery if files succefully uploaded
        task = eda.apply_async(args=[self.data_path], countdown=5)
        response['jobid'] = task.task_id

        return jsonify(response)

    def _uploadfiles(self):
        if 'data' and 'target' not in request.files:
            return 'No data or target file path'

        datafile = request.files['data']
        targetfile = request.files['target']

        data_filename = secure_filename(datafile.filename)
        target_filename = secure_filename(targetfile.filename)

        if data_filename == '' or target_filename == '':
            return 'No data or target selected'

        datafile_path = os.path.join(DATA_DIR, data_filename)
        targetfile_path = os.path.join(DATA_DIR, target_filename)

        if datafile and targetfile and data_filename and target_filename:
            if not os.path.exists(DATA_DIR):
                os.mkdir(DATA_DIR)

            # Save dataset locally
            try:
                datafile.save(datafile_path)
                targetfile.save(targetfile_path)
            except IOError as e:
                print('Unable to save dataset.')
                print(e)
                return None

        return datafile_path, targetfile_path
