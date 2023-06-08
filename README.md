# DFT-CODEX

The Density Functional Theory Comprehensive Documentation Explorer (DFT-CODEX or just CODEX) is a python package that helps you browse and explore documentation of popular DFT packages by providing an input file to your code of choice. Currently, CODEX fully supports Quantum ESPRESSO (every documented executable like `pw.x`, `ph.x` but even `matdyn.x` and `q2r.x`), and VASP support is a work in progress.

This package is currently in the Pre-Alpha Stage.

# Installation
## Option 1:
```
git clone https://github.com/oashour/dft-codex.git
cd def-codex/
# RECOMMENDED: activate virtual env or conda env
pip install .
```

## Option 2:
If you have your GitHub SSH keys setup properly (since this repo is currently private):
```
pip install git+ssh://git@github.com/oashour/dft-codex.git
```

# Usage
See `codex --help` for more information.

From a directory that has some Quantum ESPRESSO or VASP input files, just run:
```
codex
```

and your web browser will open to `http://localhost:1240` showing the codex. DFT-CODEX uses port 1240 by default (hc = 1240 eV.nm), but you can pick a custom port via the `--port` flag.

By default, it will find all files with the extensions `*.in` and `*.pwi` and treat them as Quantum ESPRESSO input files, and all files whose names start with `INCAR*`, `KPOINTS*`, `POSCAR*` and `POTCAR*` are treated as VASP input files. You can also specify specific files via the `-f` or `--filenames` flag.

DFT-CODEX can detect which QE package an input file is for (e.g., `pw.x`, `ph.x`, `projwfc.x`, etc.) and the database contains entries for every documented QE package.

The VASP database is based on VASP wiki and is a work in progress. Full documentation is being written at the moment.

## Using `codex` on a cluster
If you'd like to use this on a cluster or remote host, you'll have to use port forwarding (to port 1240 by default)
```
ssh -L 1240:localhost:1240 <username>@<host>
```

or make that a permanent setting in your `.ssh/config` file. For example, my entry for NERSC looks like this:

```
Host perlmutter*.nersc.gov saul*.nersc.gov cori*.nersc.gov dtn*.nersc.gov
    User <username>
    LocalForward 1240 127.0.0.1:1240
```

You'll need to go to the link manually from your local machine's browser (again. `http://localhost:1240` by default.) Some terminals (e.g., iTerm2 on Mac) allow you to click (or cmd+ or ctrl+ click) links to open them in your browser.

DFT-CODEX can detect when it can't find a browser and will tell you how to port forward and give you the link.

# Roadmap
## Alpha (v0.0.1)
- [x] Full QE database
    - [x] Automatic executable detection solely from the structure of the input file
- [ ] Full VASP database
    - [x] Incremental database generation code without pulling everything
- [x] Generating a codex
    - [x] CLI
    - [x] Python API
- [ ] Interacting with and rebuilding the database
    - [ ] CLI
    - [x] Python API
- [x] Frictionless workflow by starting an HTTP server and opening the browser
    - [x] Responsive, 100% local
    - [x] Click on a tag to go to the full documentation
    - [x] Navigation within the documentation panes
- [x] Functional yet ugly codex interface
- [x] Light-weight package with no `pymatgen` dependency
- [ ] Improved and compressed database format (`.json` isn't great)
    - [x] Small database size
### Other alpha features
- [ ] Explanation of the crystal structure with `robocrystallographer`
- [ ] Explanation of the k-points grid or bands path
- [ ] Better CSS, less ugly user interface
    - [ ] Navigation within the documentation panes
## Beta
- [ ] Better comments on the file, pre-generated using a fine-tuned LLM then included in the database

## Stretch
- [ ] Generating a natural-language description of a DFT calculation (a la methods section) locally using a fine-tuned LLM
# Contributing

If you find any bugs or would like to request any features, please use the issue tracker.

If you'd like to help develop CODEX, clone the repo as in option 1 above but use `pip install --editable .` so that changes to the source code are reflected in your environment immediately without needing to reinstall.

We prefer a [fork and pull](https://guides.github.com/activities/forking/) model for contributions. Make sure your code complies with `PEP8`.

# Database
By default, DFT-CODEX ships with databases for the most recent versions of Quantum ESPRESSO (currently, versions 6.7, 6.8, 7.0, 7.1 and 7.2). New databases will be generated and added to the package's datafiles as QE is updated. 

VASP does not have versioned documentation, so the database is built from the wiki. The distribution also includes the most recent mirror of the (relevant portions of) VASP wiki. You can run `codex database vasp refresh` to refresh what you have (i.e., only updated or new wiki pages will be fetched), or `codex database vasp rebuild` to rebuild it from scratch (takes about 10-20 seconds). The code will also periodically check the database version and alert you if it's old.

The databases are located with the package files, in the directory `codex/database/{code}-{version}`, where {code} is either `qe` or `vasp`, and `version` is either a QE version (e.g., 7.2) or a date (YYYYMMDD) of when the VASP database was created.

## Quantum ESPRESSO's database

Building the QE database for a version not included by default or rebuilding it from scratch requires some external utilities (namely, `tcl`, `tcllib` and `xsltproc`). You should not normally have to do this, but a user interface is available for conveience. 

You can install the dependencies as follows:

```bash
$ apt-get install tcl tcllib xsltproc # Debian
$ yum install tcl tcllib xsltproc # Red Hat
$ brew install tcl-tk libxslt # Mac with [Homebrew](https://brew.sh)
```

You also need `git` version 2.37.1 or later (which was released in 2020 and many systems have ancient default versions of `git`, make sure to check). 

These packages are required because of how QE's documentation is built using [helpdoc](https://gitlab.com/QEF/q-e/-/blob/03a7911496c4e7a01918acde8af2d65527e720ee/dev-tools/README.helpdoc), Qauntum ESPRESSO's homebrew documentation generator, written in tcl, which DFT-CODEX combines with a custom XSL template to modernize its look. First, a minimal clone of the QE repo (around 2 MB) is built, with the bare minimum necessary files to build the documentation for the intended version. Then helpdoc is run on all the `INPUT_*.def` files which have the documentation for each `*.x` executable. This generates XML and HTML files, and the database is built from them.

You can generate the entire database from scratch for some version (e.g., 6.4.1) as:
```bash
codex database espresso rebuild --version 6.4.1
```

This goes through both steps: running `helpdoc` and generating the database. If you already have the XML, you can run
```bash
codex database espresso refresh --version 6.4.1
```

