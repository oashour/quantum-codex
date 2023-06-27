from flask import Blueprint

bp = Blueprint("main", __name__)


@bp.app_template_filter("isinstance")
def is_instance(value, t):
    """isinstance filter for jinja2 templates"""
    return isinstance(value, eval(t))


from codex.main import routes
