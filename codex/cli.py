"""
Command line interface for DFT-CODEX
"""
import argparse
from argparse import RawTextHelpFormatter
from importlib.metadata import metadata
from importlib import resources
import logging
import sys
import os
from inspect import cleandoc
import datetime

from codex import Codex


# Metadata
metadata = metadata("dft-codex")
__author__ = metadata["Author"]
__version__ = metadata["Version"]
__email__ = metadata["Author-email"]
__maintainer__ = metadata["Maintainer"]
__maintainer_email__ = metadata["Maintainer-email"]
__summary__ = metadata["Summary"]
__URL__ = metadata["Home-page"]


def _get_parser():
    parser = argparse.ArgumentParser(
        description=f"""{__summary__}""",
        epilog=cleandoc(
            f"""
    Author: {__author__} ({__email__}), 
    Version: {__version__}, 
    Maintainer: {__maintainer__} ({__email__}), 
    URL: {__URL__}"""
        ),
        formatter_class=RawTextHelpFormatter,
    )

    parser.add_argument(
        "work_dir",
        metavar="D",
        type=str,
        nargs="?",  # One or zero directories
        default=None,
        help="Directory containing the input files. (default: current directory)",
    )

    parser.add_argument(
        "-f",
        "--filenames",
        default=None,
        nargs="+",
        metavar="F",
        help="One or more QE or VASP input files to parse.",
    )

    parser.add_argument(
        "--dbversion",
        default="latest",
        metavar="V",
        help="Database version to use (default: latest)",
    )

    return parser


def main():
    args = _get_parser().parse_args()
    logging.basicConfig(
        filename="codex.log",
        level=logging.INFO,
        filemode="w",
        format="%(message)s",
    )

    console = logging.StreamHandler()
    logging.info(" ".join(sys.argv[:]))
    logging.getLogger("").addHandler(console)

    if args.filenames is not None and args.work_dir is not None:
        sys.exit("Cannot specify filenames and directory at the same time.")

    if args.work_dir is None:
        logging.info("No directory specified, using current directory.")
        work_dir = os.getcwd()

    if args.filenames is None:
        filenames = os.listdir(work_dir)
        files_qe = [f for f in filenames if f.endswith(".in") or f.endswith(".pwi")]
        files_vasp = [
            f
            for f in filenames
            if f.startswith("INCAR")
            or f.startswith("KPOINTS")
            or f.startswith("POSCAR")
            or f.startswith("POTCAR")
        ]
        filenames = files_qe + files_vasp
        if not filenames:
            sys.exit("No input files found in directory.") 
        else:
            logging.info(f"Detected {len(filenames)} files in {work_dir}.")
    else:
        filenames = args.filenames
        files_qe = [f for f in filenames if f.endswith(".in") or f.endswith(".pwi")]
        files_vasp = [
            f
            for f in filenames
            if f.startswith("INCAR")
            or f.startswith("KPOINTS")
            or f.startswith("POSCAR")
            or f.startswith("POTCAR")
        ]
    # Whether to use QE and/or VASP or not
    use_qe = bool(files_qe)
    use_vasp = bool(files_vasp)

    logging.info(f"Working on files: " + ", ".join(filenames))

    database_dir = resources.files("codex.database")

    # If database version is "latest", check which one it is
    db_files = resources.contents("codex.database")
    if args.dbversion == "latest":
        qe_dbs= [float(f.split("-")[1]) for f in db_files if f.startswith("qe-")]
        vasp_dbs = [float(f.split("-")[1]) for f in db_files if f.startswith("vasp-")]
        qe_version = None
        vasp_version = None
        if qe_dbs:
            qe_version = str(max(qe_dbs))
            logging.info(f"Using QE database for version {qe_version} (latest)")
        if vasp_dbs:
            vasp_version = str(max(vasp_dbs))
            # TODO: convert to human readable date
            vasp_date = datetime.datetime.strptime(vasp_version, "%Y%m%d")
            logging.info(f"Using VASP database cached on {vasp_date} (latest)")
    else:
        qe_version = args.dbversion
        vasp_version = args.dbversion
        # Check if they exist
        if f"qe-{qe_version}" not in db_files:
            sys.exit(f"QE database version {qe_version} not found.")
        if f"vasp-{vasp_version}" not in db_files:
            sys.exit(f"VASP database version {vasp_version} not found.")

    filenames = filenames[0]  # TODO: add option for multiple files...
    if use_qe:
        Codex(filenames, database_dir, qe_version)
    if use_vasp:
        sys.exit("VASP not implemented yet.")

if __name__ == "__main__":
    main()
