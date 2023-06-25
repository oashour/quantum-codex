"""
Module for the Codex class, which is used to generate a codex from a DFT input
"""

import os
import random
import re
from abc import ABC, abstractmethod
import uuid

from tabulate import tabulate

from codex.utils import range_dict_get, remove_html_tags
from codex.database.utils import get_database


class AbstractCodex(ABC):
    """
    A class to store the parsed information from a DFT input file
    """

    def __init__(self, input_filename, dbversion, client):
        self.filename = os.path.basename(input_filename)
        self.uuid = uuid.uuid4().hex
        db, self.dbversion = get_database(client, self.code, dbversion)
        self.filetype = self._get_filetype(input_filename, db)
        self.tags, self.cards = self._get_tags_cards(input_filename, db[self.filetype])

    @property
    @abstractmethod
    def code(self):
        """
        The code that the codex is for (e.g., vasp, qe, etc.)
        Short name, all lowercase. See code_pretty.
        """
        pass

    @property
    @abstractmethod
    def indent(self):
        """
        The indent to use for each line in the input file
        """
        pass

    @property
    @abstractmethod
    def comment_token(self):
        """
        The token to use for comments (e.g., ! or #)
        If you'd like to have a space after the token, include it like comment_token = '! '
        """
        pass

    @property
    @abstractmethod
    def section_start_token(self):
        """
        The token to use for the start of a section
        (e.g., & for F90 namelists like in QE)
        """
        pass

    @property
    @abstractmethod
    def section_end_token(self):
        """
        The token to use for the end of a section
        (e.g., / for F90 namelists like in QE)
        """
        pass

    @property
    @abstractmethod
    def code_pretty(self):
        """
        A "pretty" name for the code to be shown in the Codex
        (e.g., Quantum ESPRESSO instead of qe)
        """
        pass

    @abstractmethod
    def _get_filetype(self, filename):
        """
        Figures out what type of input file is being read
        (for codes that have multiple packages or read several files per run)
        """
        pass

    @abstractmethod
    def _get_tags_cards(self):
        """
        Gets the tags and cards from the input file
        returns:
            tags: a list of dictionaries, where each dictionary has the form:
            {
                "name": the name of the tag, formatted for display
                "value": the value of the tag, formatted for display
                "comment": the comment associated with the tag, formatted for display
                "tag_pad": the pad (spaces or whatever) between the tag and the '=' (or other) sign
                "value_pad": the pad between the '=' (or other) sign and the value
                "comment_pad": the pad between the value and the comment
                "id": used for the html tag-id. Should be unique to the tag,
                                         and unformatted (no spaces)
                "href": link to go to when the tag is clicked
            }
            cards: currently just a string appended at the end of the file,
                   will be implemented later
        """
        pass

    @abstractmethod
    def _format_value(self, tag, value):
        """
        Formats the value of a tag.
        For example, in VASP and QE, True and False are formatted as .TRUE. and .FALSE.
        In VASP, MAGMOM gets special formatting.
        In QE, strings are formatted with single quotes.
        """
        pass

    @staticmethod
    @abstractmethod
    def _file_to_str(filename):
        """
        Given a "file object", it reads the file and converts it to a formatted "file string".

        A "file object" is basically whatever the _get_tags_cards() implementation
        passes to the _get_tags method of the AbstractCodex. Generally this is some
        dict-like object.
        """
        pass

    @abstractmethod
    def _get_href(self, tag, db):
        """
        Gets the href for a tag
        """
        pass

    def _get_tags(self, input_file, db):
        """
        Converts the tags in the input file into the format needed for the codex
        Takes a dict-like input_file object that looks like this:
        {section_name: {tag_name: tag_value, ...}, ...}
        """
        pads_and_formats = self._get_pad_and_format(self._file_to_str(input_file))
        tags_dict = {}
        for section, tags in input_file.items():
            tags_dict[section] = []
            for t, val in tags.items():
                comment = self._get_comment(t, val, db)
                href = self._get_href(t, db)
                # TODO: doesn't work with lists with multiple elements (QE)
                tags_dict[section].append(
                    {
                        "name": pads_and_formats[t]["formatted_tag"],
                        "value": pads_and_formats[t]["formatted_value"],
                        "comment": comment,
                        "tag_pad": pads_and_formats[t]["tag_pad"],
                        "value_pad": pads_and_formats[t]["value_pad"],
                        "comment_pad": pads_and_formats[t]["comment_pad"],
                        "id": t,
                        "href": href,
                    }
                )
        return tags_dict

    # TODO: this needs a lot of improvement
    def _get_comment(self, tag, val, db):
        """
        Gets the comment for the tag and value from the database
        """
        if options := db.find_one({"name": tag})["options"]:
            if comment := range_dict_get(str(val), options):
                return remove_html_tags(comment)
        if summary := db.find_one({"name": tag})["summary"]:
            comment = remove_html_tags(summary)
            if comment.startswith(tag):
                # TODO: this is vasp specific and temporary
                comment = comment.strip(tag).strip(", ").strip()
            return comment
        return "No Comment Available"

    def _get_pad_and_format(self, file_string):
        """
        Takes a file that has already been converted to a string (using, e.g., file_to_str)
        and formats the tags and values and computes the padding between them
        """
        lines = []
        for l in file_string.split("\n"):
            if l:
                tag = l.split("=")[0].strip()
                val = l.split("=")[1].strip()
                val = self._format_value(tag, val)
                comment = "! Place holder"
                lines.append([tag, "=", val, comment])
        table = tabulate(lines, tablefmt="plain")
        tags = {}
        for l in table.split("\n"):
            tag = l.split("=")[0]
            val = l.split("=")[1].split("!")[0]
            # The -1 accounts for the extra spacing around the = sign
            # that's being added by tabulate
            key = re.sub(r"\(.*\)", "", tag.strip())
            tags[key] = {
                "tag_pad": (len(tag) - len(tag.rstrip()) - 1) * " ",
                "value_pad": (len(val) - len(val.lstrip()) - 1) * " ",
                "comment_pad": (len(val) - len(val.rstrip()) - 1) * " ",
                "formatted_value": val.strip(),
                "formatted_tag": tag.strip(),
            }
        return tags
