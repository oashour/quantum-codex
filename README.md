# DFT-CODEX

The Density Functional Theory Comprehensive Documentation Explorer (DFT-CODEX or just CODEX) is a WebApp that helps you browse and explore documentation of popular DFT packages by uploading input files for your code of choice. Currently, CODEX fully supports Quantum ESPRESSO (every documented executable like `pw.x`, `ph.x` but even `matdyn.x` and `q2r.x`) and VASP.

CODEX is structured in a modular way so that support for new codes can easily be added by users and developers without knowing anything about how the front or backends of the WebApp works, by providing a JSON file with a specific schema.

This package is currently in the Pre-Alpha Stage.

# Usage
CODEX is currently still in development and there is no publicly accessible URL. You will have to install it locally.

The repository is currently private so you need to have your GitHub SSH keys setup properly.

Note that CODEX is built and tested with Python 3.11.3, I have no idea how well it will work with older versions, but anything after 3.8 will probably work?
## Development Installation
```
git clone https://github.com/oashour/dft-codex.git
cd dft-codex/
# Activate a venv, make sure python points to the right version
python -m venv /path/to/your/venvs/codex-dev
source /path/to/your/venvs/codex-dev/bin/activate
pip install --editable .
```

Using the `--editable` flag keeps the source code on your path so you don't have to reinstall every time you change something.
## Testing Installation
```
# Activate a venv, make sure python points to the right version
python -m venv /path/to/your/venvs/codex
source /path/to/your/venvs/codex/bin/activate
pip install git+ssh://git@github.com/oashour/dft-codex.git
```

## Running CODEX
CODEX's backend is written in Flask. From the root directory and with your virtual environment active, simply run `flask run` and go to `127.0.0.1:5000` in your browser. (Note: use the development installation for now.)

In the future, there will be a command line interface that generates a permalink to the website from local files, so you don't have to download your files from the cluster and go through the pain of uploading them to the website.
# Roadmap
This is only sitting here temporarily :)
## Barebones Alpha
- [x] Full QE database
    - [x] Automatic executable detection solely from the structure of the input file
- [x] Full VASP database
    - [x] Incremental database generation code without pulling everything
- [x] Modular/abstract classes and uniform database format
    - [x] easy to add support for other codes with minimal modification
- [x] Generating a codex
    - [x] CLI --> *deprecated*
    - [x] Python API
- [x] Interacting with and rebuilding the database
    - [x] CLI --> *deprecated*
    - [x] Python API
- [x] Demo front end
    - [x] Usable but sorta ugly interface
    - [x] Responsive, dynamically generated
    - [x] Click on a tag to go to the full documentation

## More fully featured Alpha
- [ ] Backend
    - [x] Flask for the routing etc
    - [ ] General improvements to back-end file handling etc --> **See below, good first issues**
    - [ ] Database (TinyDB or MongoDB?)
    - [ ] REST API
    - [ ] Lightweight Python wrapper/CLI for REST API
    - [ ] Containerization and deployment (NERSC spin long term, Heroku short term)
- [ ] Front end
    - [ ] Full facelift for the UI and UX --> **See below, good first issues**
    - [ ] Crystal structure visualizer (can use `crystal-toolkit` react component)
    - [ ] Info about k-grid (number of points in IBZ, packing fraction, etc.)
    - [ ] Explanation of "cards" (e.g., QE card options, certain tags in POSCAR, etc.)
    - [ ] Robo crystallographer integration --> **Good first issue**
    - [ ] BZ visualizer --> long-term, probably hard*
    - [ ] POTCAR analysis (is it recommended on VASP wiki or not, what do `_sv` mean, etc.) --> **Good first issue** *long-term*
    - [ ] UPF analysis (just file name) for PSLib and ONCVPSP --> **Good first issue**, *longterm*
    - [ ] Prettier XSL template for QE --> *long-term*
- [ ] Documentation and user guide
- [ ] Refactor
    - [ ] Abstract Codex into a Superclass
    - [ ] Fully refactor database generation code 
    - [ ] Restructure package, split flask into modules, etc.
    - [ ] Clean up deps
- [ ] AI/LLM
    - [ ] Cleaner comments/descriptions, summarized from the DB using LLM (pre-generated)
    - [ ] Explanation of what a DFT calculation is doing (dynamically generated)
### Front-end good first issues
- [ ] Proper layout
- [ ] CSS improvements
- [ ] Drag and drop upload and reorder (might take a fair bit of JS)
- [ ] Light/dark theme.
- [ ] Better code copy button, find a better solution than the current one (rolled my own with jQuery). Bootstrap docs look nice.
- [ ] Add an adjustable divider between doc preview/code block, so you can focus one or the other on smaller screens
- [ ] Better looking and closable alerts.
- [ ] Hints, tooltips, short explanations of what toggles do, etc.
### Back-end good first issues
- [ ] Accept only uploads with `.pwi` and `.in` extensions and `INCAR*`, `KPOINTS*` etc. filenames.
- [ ] Maybe: figure out whether something is QE/VASP based on file name? Might not be sustainable since a lot of codes use `.in` for input files.
- [ ] Lots of error handling
# Contributing

If you find any bugs or would like to request any features, please use the issue tracker.

If you'd like to help develop CODEX, clone the repo as in option 1 above but use `pip install --editable .` so that changes to the source code are reflected in your environment immediately without needing to reinstall.

We prefer a [fork and pull](https://guides.github.com/activities/forking/) model for contributions. Make sure your code complies with `PEP8`.

# Database

**Note:** will be overhauled soon.

CODEX provides utilities for generating new databases for Quantum ESPRESSO and VASP. 

The databases are located with the package files, in the directory `codex/database/{code}-{version}`, where {code} is either `qe` or `vasp`, and `version` is either a QE version (e.g., 7.2) or a date (YYYYMMDD) of when the VASP database was created.

## Quantum ESPRESSO

Building the QE database for a version not included by default or rebuilding it from scratch requires some external utilities (namely, `tcl`, `tcllib` and `xsltproc`). You should not normally have to do this, but a user interface is available for conveience. 

You can install the dependencies as follows:

```bash
$ apt-get install tcl tcllib xsltproc # Debian
$ yum install tcl tcllib xsltproc # Red Hat
$ brew install tcl-tk libxslt # Mac with [Homebrew](https://brew.sh)
```

You also need `git` version 2.37.1 or later (which was released in 2020 and many systems have ancient default versions of `git`, make sure to check). 

These packages are required because of how QE's documentation is built using [helpdoc](https://gitlab.com/QEF/q-e/-/blob/03a7911496c4e7a01918acde8af2d65527e720ee/dev-tools/README.helpdoc), Quantum ESPRESSO's homebrew documentation generator, written in tcl, which DFT-CODEX combines with a custom XSL template to modernize its look. First, a minimal clone of the QE repo (around 2 MB) is built, with the bare minimum necessary files to build the documentation for the intended version. Then `helpdoc` is run on all the `INPUT_*.def` files which have the documentation for each `*.x` executable. This generates XML and HTML files, and the database is built from them.

You can generate the entire database from scratch for some version (e.g., 6.4.1) as:
```python
from codex.database.espresso import run_helpdoc, generate_database
v = '6.4.1'
run_helpdoc(v)
generate_database(v)
```

Calling `run_helpdoc` is only necessary the first time when you don't have the HTML and XML files already there.

## VASP
VASP does not have versioned documentation, so the database is built from the wiki. The distribution also includes the most recent mirror of the VASP wiki. 
