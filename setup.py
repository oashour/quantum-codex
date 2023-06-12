# Copyright (c) Omar A. Ashour
# Distributed under the terms of the MIT License.

from setuptools import setup, find_packages

import os

SETUP_PTH = os.path.dirname(os.path.abspath(__file__))

with open(os.path.join(SETUP_PTH, "README.md")) as f:
    desc = f.read()

setup(
    name="dft-codex",
    packages=find_packages(),
    package_data={"": ["database/qe-*/*.html", "database/qe-*/*.json", "database/*.js", "database/*.css"]},
    version="0.0.0",
    python_requires=">=3.10",
    install_requires=[
        "f90nml",
        "requests",
        "lxml",
        "bs4",
        "wikitextparser",
        "mwparserfromhell",
        "mwcomposerfromhell",
        "flask-reuploaded",
        "flask",
    ],
    extras_require={},
    author="Omar A. Ashour",
    author_email="ashour@berkeley.edu",
    maintainer="Omar A. Ashour",
    maintainer_email="ashour@berkeley.edu",
    url="https://github.com/oashour/dft-codex.git",
    license="MIT",
    description="A python package to help you understand your DFT input files.",
    long_description=desc,
    keywords=["vasp", "quantum-espresso", "DFT", "quantum-chemistry", "materials-science"],
    entry_points={
    "console_scripts": [
        "codex = codex.cli:main",
    ]},
    classifiers=[
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Development Status :: 1 - Planning",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License" "Operating System :: OS Independent",
        "Topic :: Scientific/Engineering :: Physics",
        "Topic :: Scientific/Engineering :: Chemistry",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
)
