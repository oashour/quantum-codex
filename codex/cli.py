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
from http.server import HTTPServer, SimpleHTTPRequestHandler
import webbrowser
from threading import Timer
import shutil
import re
from packaging import version

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

def _open_browser(port):
    try:
        webbrowser.get()
    except webbrowser.Error:
        logging.warning("Could not open browser. Are you on a cluster through SSH?")
        logging.warning("If that's the case, you can set up port forwarding with:")
        logging.warning(f"ssh -L {port}:localhost:{port} <username>@<hostname>")
        logging.warning(f"and then open your local machine's browser to http://localhost:{port}/")
        return
    webbrowser.open(f"http://localhost:{port}/")

def _split_qe_and_vasp_files(filenames):
    files_qe = [f for f in filenames if f.endswith(".in") or f.endswith(".pwi")]
    files_vasp = [
        f
        for f in filenames
        if f.startswith("INCAR")
        or f.startswith("KPOINTS")
        or f.startswith("POSCAR")
        or f.startswith("POTCAR")
    ]

    return files_qe, files_vasp


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

    parser.add_argument(
        "--version",
        action="store_true",
        help="Return the version of DFT-CODEX",
    )

    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Enable verbose output",
    )

    parser.add_argument(
        "--keep-scratch",
        action="store_true",
        help="Don't clean up temporary/scratch directories after exit (e.g., .codex)",
    )

    parser.add_argument(
        "--port",
        "-p",
        default="42069",
        metavar="P",
        help="Port to start HTTP server on (default: 42069)",
    )

    return parser


def main():
    """
    The main CLI for codex
    """
    args = _get_parser().parse_args()
    if args.verbose:
        level = logging.DEBUG
    else:
        level = logging.INFO
    logging.basicConfig(
        filename="codex.log",
        level=level,
        filemode="w",
        format="%(message)s",
    )

    logging.basicConfig(level=level)
    console = logging.StreamHandler()
    logging.info(" ".join(sys.argv[:]))
    logging.getLogger("").addHandler(console)

    if args.version:
        sys.exit(f"DFT-CODEX version {__version__}")
    if args.filenames is not None and args.work_dir is not None:
        sys.exit("Cannot specify filenames and directory at the same time.")

    if args.work_dir is None:
        logging.info("No directory specified, using current directory.")
        work_dir = os.getcwd()

    if args.filenames is None:
        files_in_dir = os.listdir(work_dir)
        files_qe, files_vasp = _split_qe_and_vasp_files(files_in_dir)
        filenames = files_qe + files_vasp
        if not filenames:
            sys.exit("No input files found in directory.")
        else:
            logging.info(f"Detected {len(filenames)} files in {work_dir}.")
    else:
        filenames = args.filenames
        files_qe, files_vasp = _split_qe_and_vasp_files(filenames)

    # Whether to use QE and/or VASP
    use_qe = bool(files_qe)
    use_vasp = bool(files_vasp)

    logging.info(f"Working on files: " + ", ".join(filenames))

    database_dir = resources.files("codex.database")

    # If database version is "latest", check which one it is
    db_contents = resources.contents("codex.database")
    if args.dbversion == "latest":
        # Find all the QE databases available
        pattern = r"qe-[0-9]+\.[0-9]+"
        qe_dbs = [re.match(pattern, f).group(0).split("-")[1] for f in db_contents if re.match(pattern, f)]
        # Find all the VASP databases available, of the form vasp-YYYYMMDD
        pattern = r"vasp-[0-9]+"
        vasp_dbs = [re.match(pattern, f).group(0).split("-")[1] for f in db_contents if re.match(pattern, f)]

        qe_version = None
        vasp_version = None
        if qe_dbs:
            qe_version = version.parse(qe_dbs[0])
            for v in qe_dbs[1:]:
                if version.parse(v) > qe_version:
                    qe_version = version.parse(v)
        if vasp_dbs:
            vasp_dbs = [int(v) for v in vasp_dbs]
            vasp_version = str(max(vasp_dbs))
            # TODO: convert to human readable date
            vasp_date = datetime.datetime.strptime(vasp_version, "%Y%m%d")
            logging.info(f"Using VASP database cached on {vasp_date} (latest)")
    else:
        qe_version = args.dbversion
        vasp_version = args.dbversion
        # TODO: this will break any time the code automatically finds
        # VASP files in cwd when you want to use QE and vice versa
        if use_qe and f"qe-{qe_version}" not in db_contents:
            sys.exit(f"QE database version {qe_version} not found.")
        if use_vasp and f"vasp-{vasp_version}" not in db_contents:
            sys.exit(f"VASP database version {vasp_version} not found.")

    filenames = filenames[0]  # TODO: add option for multiple files...
    codex = None
    if use_qe:
        codex = Codex(files_qe, qe_version)
        html_filename = "index.html" if not use_vasp else "index_qe.html"
        codex.build(html_filename)
    if use_vasp:
        sys.exit("VASP not implemented yet.")
        codex = Codex(files_vasp, database_dir, vasp_version)
        html_filename = "index.html" if not use_qe else "index_vasp.html"
        codex.build(html_filename)
    if codex is None:
        # TODO: better error handling
        sys.exit("No files generated for some reason, something went wrong...")
    if use_qe and use_vasp:
        # TODO: generate index file with links to both QE and VASP codexes
        sys.exit("QE+VASP not implemented yet.")

    # TODO: directory handling needs to be better
    # More consistency with Codex.build()
    os.chdir(os.path.join(work_dir, '.codex'))
    port = int(args.port)
    httpd = HTTPServer(("localhost", port), SimpleHTTPRequestHandler)
    logging.info(f"Serving HTTP on localhost port {port} (http://localhost:{port}/)...")
    Timer(1, _open_browser, [port]).start()

    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        logging.info("Keyboard interrupt received, exiting.")
        scratch_dir = os.path.join(work_dir, ".codex")
        if not args.keep_scratch:
            logging.info(f"Cleaning up scratch directory... {scratch_dir}")
            shutil.rmtree(scratch_dir)
        sys.exit(0)

if __name__ == "__main__":
    main()
