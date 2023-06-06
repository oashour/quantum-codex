# Copyright (c) Omar A. Ashour
# Distributed under the terms of the MIT License.

from setuptools import setup, find_namespace_packages

import os

SETUP_PTH = os.path.dirname(os.path.abspath(__file__))

with open(os.path.join(SETUP_PTH, "README.md")) as f:
    desc = f.read()


setup(
    name="pymatgen-io-espresso",
    packages=find_namespace_packages(include=["pymatgen.io.*"]),
    version="0.0.0",
    install_requires=[
        "f90nml",
    ],
    extras_require={},
    package_data={},
    author="Omar A. Ashour",
    author_email="ashour@berkeley.edu",
    maintainer="Omar A. Ashour",
    url="https://github.com/oashour/dft-tutor.git",
    license="MIT",
    description="A python package to help you understand your DFT input files.",
    long_description=desc,
    keywords=["vasp", "quantum-espresso", "DFT", "quantum-chemistry", "materials-science"],
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Development Status :: 2 - Pre-Alpha",
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License"
        "Operating System :: OS Independent",
        "Topic :: Scientific/Engineering :: Physics",
        "Topic :: Scientific/Engineering :: Chemistry",
        "Topic :: Software Development :: Libraries :: Python Modules",
    ],
)