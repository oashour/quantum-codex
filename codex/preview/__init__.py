from flask import Blueprint

bp = Blueprint('preview', __name__)

@bp.app_template_filter("isinstance")
def is_instance(value, t): 
    return isinstance(value, eval(t))

from codex.preview import routes