# mlp-automl
AutoML project for ML Products class

## Python version
For best results use Python 3.x

## Dependencies
### Virtual environment
It is best to run this project with on a venv

Install virtualenv

`pip install virtualenv`

Create a virtual environment for project in a separate dir

`virtualenv -p python3 mlp-automl-env`

Activate virtualenv. Navigate to venv dir and run

`source <path-to-venv>/bin/activate`

Later, to deactivate venv run

`deactivate`

### Package dependencies

From project root dir, install dependencies with 

`pip install -r requirements.txt`

## Run web service
- Make sure the package `gunicorn' is installed
- Navigate to root directory
- Start the webservice with `gunicorn automl:app`
- Test that the service works by navigting to `localhost:8000/index`
- Start a new job with `curl -XPOST -F 'data=@<path-to-data-file>/iris.data.txt' -F 'target=@<path-to-dataset-dependent-variable-file>/iris.target.txt' localhost:8000/job/`
- Download job results from browser with `localhost:8000/job/<jobid>`
