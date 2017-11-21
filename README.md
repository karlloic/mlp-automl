# mlp-automl
AutoML project for ML Products class

## Dependencies
Install dependencies with `pip install -r requirements.txt`

## Run web service
- Make sure the package `gunicorn' is installed
- Navigate to root directory
- Start the webservice with `gunicorn automl:app`
- Test at `localhost:8000/index`
- To test file upload navigate `localhost:8000/dataset/eda`
