from __future__ import annotations

import dash
from dash import html
import os
from codex import Codex

# standard Dash imports for callbacks (interactivity)
from dash.dependencies import Input, Output
from pymatgen.core import Lattice, Structure

import crystal_toolkit.components as ctc

#from pymatgen.io.espresso.pwin import Pwin

# For caching
#from flask_caching import Cache

# don't run callbacks on page load
app = dash.Dash(__name__, prevent_initial_callbacks=True)

# Cache stuff
#cache = Cache(
#    app.server,
#    config={
#        "CACHE_TYPE": "RedisCache",
#        "CACHE_REDIS_URL": os.environ.get("REDIS_URL", "localhost:6379"),
#    },
#)

# now we give a list of structures to pick from
structures = [
    Structure(Lattice.hexagonal(5, 3), ["Na", "Cl"], [[0, 0, 0], [0.5, 0.5, 0.5]]),
    Structure(Lattice.cubic(5), ["K", "Cl"], [[0, 0, 0], [0.5, 0.5, 0.5]]),
]

# we show the first structure by default
structure_component = ctc.StructureMoleculeComponent(structures[0], id="hello_structure")

# and we create a button for user interaction
my_button = html.Button("Swap Structure", id="change_structure_button")

# now we have two entries in our app layout,
# the structure component's layout and the button
tag = "test"
link = "https://www.google.com"
file_id = "0"
tag_link = dash.html.A(
    id=tag, href=link, className="tag-link", children=tag, **{"data-fileid": file_id}
)
#tag_value = dash.html.Span(className="tag-value", children=" = 123.456")
#tag_comment = dash.html.Span(className="comment", children=" ! I do this thing")
#tag = dash.html.Div(className="tag", children=[tag_link, tag_value, tag_comment])
##my_layout = html.Div([structure_component.layout(), my_button])

codex = Codex(['example/sr3pbo_bands.in'], '7.2')
div = codex.html[0]

my_layout = html.Div([div])

#ctc.register_crystal_toolkit(app=app, layout=my_layout, cache=cache)
ctc.register_crystal_toolkit(app=app, layout=my_layout)


# for the interactivity, we use a standard Dash callback
@app.callback(
    Output(structure_component.id(), "data"),
    Input("change_structure_button", "n_clicks"),
)
def update_structure(n_clicks):
    """Toggle between hexagonal and cubic structures on button click."""
    return structures[n_clicks % 2]


if __name__ == "__main__":
    app.run(debug=True, port=8050)
