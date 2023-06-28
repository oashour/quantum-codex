"""
Bluepprints for the main pages/front end of the app
"""
from datetime import datetime
from codex import PRETTY_CODE_MAP
from flask import Blueprint

bp = Blueprint("main", __name__)


@bp.app_template_filter("isinstance")
def is_instance(value, t):
    """isinstance filter for jinja2 templates"""
    return isinstance(value, eval(t))


@bp.app_template_filter("format_version")
def format_version(version, code):
    """Format the database version for display"""
    if code == "vasp":
        date = datetime.fromtimestamp(int(version))
        return date.strftime("Archived from VASP wiki on %Y-%m-%d at %H:%M:%S UTC")
    return version


@bp.app_template_filter("format_code")
def format_code(code, dbversion):
    """Convert code to pretty/human-friendly version for display"""
    version = f" (v{dbversion})" if code != "vasp" else ""
    return PRETTY_CODE_MAP.get(code, code) + version


@bp.app_template_filter("format_filetype")
def format_filetype(filetype, code):
    """Convert file type to pretty/human-friendly version for display"""
    if code == "espresso":
        filetype += ".x"
    return f"<tt>{filetype}</tt>"


from codex.main import routes
