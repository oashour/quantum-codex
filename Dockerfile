FROM python:3.11.3-slim-bullseye as build
# Install gcc (for building pymatgen)
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
	build-essential gcc

# Create venv and add it to path
WORKDIR /quantum-codex
RUN python -m venv /quantum-codex/venv
ENV PATH="/quantum-codex/venv/bin:$PATH"

# Install requirements
COPY requirements.txt .
RUN pip install -r requirements.txt

# Use digest for deterministic builds
FROM python:3.11.3-slim-bullseye@sha256:eaee5f73efa9ae962d2077756292bc4878c04fcbc13dc168bb00cc365f35647e

# Install tcl/xslt for helpdoc
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
	tcl tcllib xsltproc git

# Add python user and group with UID/GID 999 so we don't run stuff as root
RUN groupadd -g 999 python && \
    useradd -r -u 999 -g python python

# Make the WORKDIR and set owner
RUN mkdir /quantum-codex && chown python:python /quantum-codex
WORKDIR /quantum-codex

# Copy 
COPY --chown=python:python --from=build /quantum-codex/venv ./venv
COPY --chown=python:python . .


USER 999

ENV PATH="/quantum-codex/venv/bin:$PATH"
RUN pip install '.[app,database]'

CMD [ "gunicorn", "--preload", "-w", "4", "codex.app:create_app()", "-c", "config.py", "--bind", "0.0.0.0:5000" ]
