"""
Utility functions for the models.
"""
from lxml import etree

def remove_html_tags(text):
    """Remove html tags from a string"""
    if text:
        parser = etree.HTMLParser()
        tree = etree.fromstring(text, parser)
        string = etree.tostring(tree, encoding="unicode", method="text")
        return string.strip()
    return text
