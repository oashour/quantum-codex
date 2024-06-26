"""
Copyright (c) Omar A. Ashour
Distributed under the terms of the MIT License.
"""

import os
import pathlib
from setuptools import setup, find_packages


SETUP_PTH = os.path.dirname(os.path.abspath(__file__))

desc = pathlib.Path(os.path.join(SETUP_PTH, "README.md")).read_text()

setup(
    name="quantum-codex",
    packages=find_packages(),
    package_data={
        "database": [
            "database/vasp-cache/*json",
            "database/json/*.json",
            "database/espresso-helpdoc/*",
        ]
    },
    version="0.0.0",
    python_requires=">=3.10",
    install_requires=["requests"],
    extras_require={
        "database": ["lxml", "mwparserfromhell"],
        "app": [
            "flask",
            "flask-reuploaded",
            "flask-pymongo",
            "flask-smorest",
            "gunicorn",
        ],
        "core": ["pymatgen", "tabulate", "lxml", "f90nml", "nanoid"],
    },
    author="Omar A. Ashour",
    author_email="ashour@berkeley.edu",
    maintainer="Omar A. Ashour",
    maintainer_email="ashour@berkeley.edu",
    url="https://github.com/oashour/quantum-codex.git",
    license="MIT",
    description="A python package to help you understand your DFT input files.",
    long_description=desc,
    keywords=["vasp", "quantum-espresso", "DFT", "quantum-chemistry", "materials-science"],
    entry_points={
        "console_scripts": [
            "codex = codex.cli.cli:main",
        ]
    },
    classifiers=[
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Development Status :: 2 - Pre-Alpha",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License" "Operating System :: OS Independent",
        "Topic :: Scientific/Engineering :: Physics",
        "Topic :: Scientific/Engineering :: Chemistry",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
)
