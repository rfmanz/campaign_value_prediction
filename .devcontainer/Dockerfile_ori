FROM quay.io/jupyter/minimal-notebook

USER root
ARG GRANT_SUDO="yes"

ARG env_name=py3.11
ARG py_ver=3.11

# Copy the requirements.txt file into the image
COPY requirements.txt /tmp/

# Use mamba to create the environment and install the required packages
RUN mamba create --yes -n "${env_name}" \
    python=${py_ver} \
    'ipykernel' \
    'jupyterlab' \
    'mamba' \
    'pip' \
    && mamba clean --all -f -y

# Activate the environment and install the Python packages specified in requirements.txt
RUN /bin/bash -c "source activate ${env_name} && pip install -r /tmp/requirements.txt"

# Create Python kernel and link it to jupyter
RUN "${CONDA_DIR}/envs/${env_name}/bin/python" -m ipykernel install --user --name="${env_name}" && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"

# This changes the custom Python kernel so that the custom environment will
# be activated for the respective Jupyter Notebook and Jupyter Console
RUN /opt/setup-scripts/activate_notebook_custom_env.py "${env_name}"