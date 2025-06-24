# ────────────────────────────────────────────────────────────────
# Base image – still Jupyter minimal-notebook
# ────────────────────────────────────────────────────────────────
ARG REGISTRY=quay.io
ARG OWNER=jupyter
ARG BASE_CONTAINER=$REGISTRY/$OWNER/minimal-notebook
FROM $BASE_CONTAINER

ARG GRANT_SUDO="yes"
USER root

# ────────────────────────────────────────────────────────────────
# 1️⃣  System dependencies (single layer)
# ────────────────────────────────────────────────────────────────
RUN set -eux; \
    apt-get update -y && \
    apt-get install -y --no-install-recommends \
        fonts-dejavu \
        unixodbc unixodbc-dev \
        imagemagick && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# Give notebook user optional sudo (unchanged)
RUN if [ "$GRANT_SUDO" = "1" ] || [ "$GRANT_SUDO" = "yes" ]; then \
        echo "${NB_USER} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/added-by-start-script; \
    fi

# ────────────────────────────────────────────────────────────────
# 2️⃣  Conda environment – Python + R + pip (one layer)
# ────────────────────────────────────────────────────────────────
COPY --chown=${NB_UID}:${NB_GID} .devcontainer/environment.yml /tmp/environment.yml
RUN set -eux; \
    mamba env update -n base -f /tmp/environment.yml && \
    mamba clean --all -f -y && \
    fix-permissions "$CONDA_DIR" && \
    fix-permissions "/home/${NB_USER}"

# ────────────────────────────────────────────────────────────────
# 3️⃣  Final tweaks (radian) + permissions
# ────────────────────────────────────────────────────────────────
RUN pip install --no-cache-dir radian && \
    fix-permissions "$CONDA_DIR" && \
    fix-permissions "/home/${NB_USER}"

USER ${NB_UID}
