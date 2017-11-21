import os
import flask_restful
from flask import request
from werkzeug.utils import secure_filename


ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), '../'))
UPLOAD_FOLDER = ROOT_DIR + '/data'


class DataSet(flask_restful.Resource):
    """
    This class represents a dataset object. Simply a numpy ndarray
    """
    def __init__(self):
        pass

    def get(self):
        pass

    def post(self):
        if 'file' not in request.files:
            return 'No file path'

        file = request.files['file']
        filename = secure_filename(file.filename)
        if filename == '':
            return 'No file selected'

        if file and filename:
            if not os.path.exists(UPLOAD_FOLDER):
                os.mkdir(UPLOAD_FOLDER)
            file.save(os.path.join(UPLOAD_FOLDER, filename))
        return file.filename + ' successfully uploaded.'

    def describe(self):
        pass

    def pairplot(self):
        pass
